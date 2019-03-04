(library (testing private generic)
  (export identity string-starts-with? string-ends-with? read-all append-map)
  (import (rnrs (6))
          (utility algorithms))

  #|  Generic utility functions
   | ====================================================================== |#

  #| Identity function.
   |#
  (define (identity x) x)

  #| Tests whether a string starts with a certain prefix.
   |#
  (define (string-starts-with? pre str)
    (string=? (substring str 0 (string-length pre))
              pre))

  #| Tests whether a string ends with a certain postfix.
   |#
  (define (string-ends-with? post str)
    (let ((n (string-length str))
          (m (string-length post)))
      (string=? (substring str (- n m) n) post)))

  #| Read all expressions from a port.
   |#
  (define (read-all port)
    (do ((result '() (cons (read port) result)))
	((port-eof? port) (if (null? result)
			      '()
			      (reverse (cdr result))))))
;    (unfold port-eof? read identity port))
)
