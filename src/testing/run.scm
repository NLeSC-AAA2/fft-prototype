(library (testing run)

  (export file->suites)

  (import (rnrs (6))
          (rnrs eval (6))

          (utility algorithms)

          (match)

          (testing tests)
          (testing suite)
          (testing reports)

          (testing private contexts)
          (testing private generic))

  #|  Parsing program structure
   | ====================================================================== |#

  #| Tests whether an expression is an import statement.
   |#
  (define (import-clause? expr)
    (and (pair? expr) (eq? (car expr) 'import)))

  #| Tests whether an expression is a definition.
   |#
  (define (define-clause? expr)
    (and (pair? expr) (eq? (car expr) 'define)))

  #| Tests wether an expression defines a unit test. Unit tests are functions
   | whose name starts with `test`.
   |#
  (define (test-clause? expr)
    (and (define-clause? expr)
         (let ((name (symbol->string (get-definition-name expr))))
           (and (< 4 (string-length name))
                (string-starts-with? "test" name)))))

  #| Obtains all import clauses from a program.
   |#
  (define (get-imports program)
    (append-map cdr (filter import-clause? program)))

  #| Extract the name symbol from a function definition.
   |#
  (define (get-definition-name expr)
    (match expr
      ((define (,name . ,args) ,body ...)
       name)
      ((define ,name ,rest ...)
       name)
      (,default #f)))

  (define (get-definition-args expr)
    (match expr
      ((define (,name . ,args) ,body ...)
       args)
      ((define ,name (lambda ,args ,body ...))
       args)
      (,default '())))

  #| Transform a function definition to a valid `letrec` clause.
   |#
  (define (define->letrec-clause expr)
    (match expr
      ((define (,name . ,args) ,body ...)
       `(,name (lambda ,args . ,body)))
      ((define ,name (lambda ,args ,body ...))
       `(,name (lambda ,args . ,body)))))

  #| Extract all named test definitions from a program.
   |#
  (define (get-defined-tests program)
    (map (lambda (x)
           (cons (get-definition-name x)
                 (get-definition-args x)))
         (filter test-clause? program)))

  #| Get all definitions from a program.
   |#
  (define (get-definitions program)
    (filter define-clause? program))

  (define (get-program-body program)
    (filter (lambda (x) (not (import-clause? x))) program))

  (define (library? program)
    (eq? (caar program) 'library))

  (define (library-spec program)
    (match (car program)
      ((library ,spec (export ,exports ...) ,body ...)
       spec)))

  (define (library-exports program)
    (match (car program)
      ((library ,spec (export ,exports ...) ,body ...)
       exports)))

  (define (library->suites program)
    (eval 
      `(filter suite? (list ,@(library-exports program)))
      (environment '(rnrs (6))
                   '(testing suite)
                   (library-spec program))))

  (define (file->suites path)
    (let* ((program (read-all (open-input-file path))))
      (cond
        ((library? program) (library->suites program))
        (else (list (monkey-program->suite path program))))))

  (define (monkey-program->suite path program)
    (let* ((env     (apply environment (get-imports program)))
           (body    (get-program-body program))
           (tests   (get-defined-tests program)))
      (make-suite
        path
        (map (lambda (test)
               (cons (car test)
                     (make-test (car test)
                                (eval `(let () ,@body ,(car test)) env)
                                (cdr test))))
             tests)
        "")))
)
