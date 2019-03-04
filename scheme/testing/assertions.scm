(library (testing assertions)

  (export assert-eq assert-eqv assert-equal assert-not
          assert-predicate assert-compare
          assert-raises assert-no-raise assert-all)

  (import (rnrs (6))
          (format))

  (define-syntax define-assertion
    (lambda (x)
      (syntax-case x ()
        ((define-assertion (<name> <formals> ...) <body> ...)
         #'(define (<name> <formals> ...)
             (define (exception-description)
              (apply string-append
                "assertion violation in `~s` with arguments:\n"
                (map (lambda (arg)
                       (format "  ‣ ~s: ~~s\n" arg)) '(<formals> ...))))

             (unless (begin <body> ...)
               (raise (assertion-violation
                        '<name>
                        (exception-description)
                        '<name> <formals> ...))))))))

  (define-assertion (assert-eq a b)
    (eq? a b))

  (define-assertion (assert-eqv a b)
    (eqv? a b))

  (define-assertion (assert-equal a b)
    (equal? a b))

  (define-assertion (assert-not a)
    (not a))

  (define-assertion (assert-predicate predicate? a)
    (predicate? a))

  (define-assertion (assert-compare comp? a b)
    (comp? a b))

  (define (all f x)
    (not (find (lambda (x) (not (f x))) x)))

  (define-assertion (assert-all predicate? lst)
    (all predicate? lst))

  (define-syntax define-assertion-syntax
    (lambda (x)
      (syntax-case x ()
        ((define-assertion-syntax (<name> <formals> ...) <body> ...)
         (let ((<formal-syms> (map symbol->string (syntax->datum #'(<formals> ...)))))
           #`(define-syntax <name>
               (lambda (x)
                 (syntax-case x ()
                   ((<name> <formals> ...)
                    #'(let ((passing? (begin <body> ...)))
                        (define (exception-description)
                          (apply string-append
                            "assertion violation in `~s` with arguments:\n"
                            (map (lambda (arg)
                              (format #f "  ‣ ~a: ~~s\n" arg)) '#,<formal-syms>)))

                        (unless passing?
                          (raise (assertion-violation
                                   '<name>
                                   (exception-description)
                                   '<name>
                                   '<formals> ...)))))))))))))

  (define-assertion-syntax (assert-raises <predicate> <expression>)
    (call/cc
      (lambda (return)
        (with-exception-handler
          (lambda (y) (return (<predicate> y)))
          (lambda () <expression> #f)))))

  (define-assertion-syntax (assert-no-raise <expression>)
    (call/cc
      (lambda (return)
        (with-exception-handler
          (lambda (y) (return #f))
          (lambda () <expression> #t)))))
)
