(library (testing suite)
  (export define-test-suite define-test-case test-case run-suite make-suite suite?)

  (import (rnrs (6))
          (format)

          (monads)
	  (parsing)

          (testing reports)
          (testing tests)
          (utility cut)
          (testing private colours))

  (define-record-type failure)
  (define *failure* (make-failure))

  (define (string-tokenize str c)
    (parse-string
     (sep-by (many-char (char!= c)) (char= c))
     str))

  (define (string-join lst sep)
    (if (null? lst)
      ""
      (let loop ((result (car lst))
                 (rest (cdr lst)))
        (if (null? rest)
          result
          (loop (string-append result sep (car rest))
                (cdr rest))))))

  (define (text-indent pre text)
    (string-join
      (map (cut string-append pre <>)
           (string-tokenize text #\newline))
      (string #\newline)))

  #| Syntax ================================================================ |#
  (define-record-type suite
    (fields name (mutable register) description))

  (define (suite-list s)
    (reverse (map car (suite-register s))))

  (define (suite-get-test s t)
    (cond
      ((assq t (suite-register s)) => cdr)
      (else *failure*)))

  (define (suite-add-test! s t f)
    (suite-register-set!
      s
      (cons (cons t f) (suite-register s))))

  (define (remove-last-newline str)
    (cond
      ((string=? str "") "")
      ((eq? (string-ref str (- (string-length str) 1)) #\newline)
       (substring str 0 (- (string-length str) 1)))
      (else str)))

  (define (run-suite s)
    (println "Running test suite {:s}" (suite-name s))
    (println "{:}══════════════════════════════════════════════════════════════════{:}"
             (set-colour-blue) (reset-colour))
    (fold-left
      (lambda (report test)
        (print "{:} ... "
                (string-append
                  (symbol->string test)
                  (make-string (- 25 (string-length (symbol->string test))) #\space)))
        (let ((result (run-test (suite-get-test s test))))
          (if (failed? result)
            (println "{:}😣  failed\n{:}{:}\n{:}{:}\n{:}{:}{:}"
              (set-colour-red) (set-colour-dark-red)
              "  ╭────────────────────────────────────────────────────────────"
              (reset-colour)
              (text-indent (format "  {:}│{:} " (set-colour-dark-red) (reset-colour))
                           (remove-last-newline (result-message result)))
              (set-colour-dark-red)
              "  ╰────────────────────────────────────────────────────────────"
              (reset-colour))
            (println "{:}🙂  passed{:}"
                    (set-colour-green)
                    (reset-colour)))
          (report-add-result report result)))
      (make-report 0 0 '())
      (suite-list s)))

  (define-syntax define-test-suite
    (lambda (x)
      (syntax-case x ()
        ((define-test-suite <name> <description>)
         #'(define <name>
             (make-suite '<name> '() <description>))))))

  (define-syntax define-test-case
    (lambda (x)
      (syntax-case x ()
        ((define-test-case <suite> <name> <body>)
         #'(suite-add-test! <suite> '<name>
                           (make-test '<name>
                                      (lambda () <body>) '())))

        ((define-test-case <suite> <name> (<fixtures> ...) <body> ...)
         #'(suite-add-test! <suite> '<name>
                           (make-test '<name>
                                      (lambda (<fixtures> ...) <body> ...)
                                      '(<fixtures> ...)))))))


  (define-syntax test-case
    (lambda (x)
      (syntax-case x ()
        ((test-case <name> (<fixtures> ...) <body> ...)
         #'(apply-test (make-test '<name>
                                  (lambda (<fixtures> ...) <body> ...)
                                  '(<fixtures> ...))))

        ((test-case <name> <body>)
         #'(apply-test (make-test '<name>
                                  (lambda () <body>) '()))))))
)
