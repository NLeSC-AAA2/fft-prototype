#| Defines the `test` record type, and the `run-test` function.
 |#
(library (testing tests)

  (export test? make-test test-name test-fixtures test-function apply-test
          result? make-result *passed* passed? failed? result-message
          result-exception
          run-test)

  (import (rnrs (6))
	  (rename (ice-9 format) (format ice-9-format))
          (format))

  (define-record-type test
    (fields name function fixtures))

  (define-record-type result
    (fields exception))

  (define *passed* (make-result '()))

  #| Returns `#t` if the test passed.
   |#
  (define (passed? result) (eq? result *passed*))

  #| Returns `#t` if the argument is a `result` record and the test failed.
   |#
  (define (failed? result) (not (passed? result)))

  (define (result-message result)
    (let ((exn (result-exception result)))
      (if (string? (condition-message exn))	  
	  (apply ice-9-format #f
		 (condition-message exn)
		 (condition-irritants exn))
	  (format "Illegal condition: {}, {}\n"
		  (condition-message exn)
		  (condition-irritants exn)))))

  #| Apply the `function` member of a `test` record.
   |#
  (define (apply-test t)
    ((test-function t)))

  #| Runs a test using an exception-handler. If an exception is raised during
   | the test, it is returned as a new `result` record.  If the test throws no
   | exception, it passes, and the value `*passed*` is returned.
   |#
  (define (run-test t)
    (call/cc
      (lambda (return)
        (with-exception-handler
	    (lambda (x)
	      (return (make-result x)))
          (lambda ()
            (apply-test t)
            *passed*)))))
)
