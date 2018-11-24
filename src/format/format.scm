(library (format format)
  (export format print println formatter)
  (import (rnrs (6))
          (parsing)
          (monads))

  (define-record-type spec
    (fields fill align sign sharp zero width precision type))

  (define identifier
    (seq <parsing>
         (satisfies item char-alphabetic?)
         (many-char* (satisfies item 
                                (lambda (c)
      				  (or (char-alphabetic? c)
      				      (char-numeric? c)
      				      ((char-in "_-") c)))))
         (flush)))

  (define integer
    (some-char (satisfies item char-numeric?)
      	 string->number))

  (define argument
    (choice integer identifier))

  (define parameter
    (seq <parsing>
         (x <- argument)
         (literal "$")
         (parsing-return x)))

  (define count
    (choice parameter integer))

  (define spec-parser
    (seq <parsing>
         (fill      <- (optional (look-ahead
      			    (one item)
      			    (one (char= #\< #\^ #\>)))))
         (align     <- (optional (one (char= #\< #\^ #\>))))
         (sign      <- (optional (one (char= #\+ #\-))))
         (sharp     <- (optional (one (char= #\#))))
         (zero      <- (optional (one (char= #\0))))
         (width     <- (optional (choice count (one (char= #\*)))))
         (precision <- (optional (seq <parsing> (one (char= #\.)) count)))
         (type      <- (optional identifier))
         (parsing-return
           (make-spec fill align sign sharp zero width precision type))))

  (define escape
    (choice (seq <parsing>
      	   (literal "{{")
      	   (parsing-return "{"))
            (seq <parsing>
      	   (literal "}}")
      	   (parsing-return "}"))))

  (define clause
    (seq <parsing>
         (literal "{")
         (arg <- (optional argument))
         (fmt <- (optional (seq <parsing>
      			  (literal ":")
      			  spec-parser)))
         (literal "}")
         (parsing-return (cons arg fmt))))

  (define text
    (some-char (char!= #\{ #\})))

  (define format-string-parser
    (many (choice text escape clause)))

  (define (parse-format-string str)
    (parse-string
     format-string-parser
     str))

  (define (format-value spec value port)
    ;;(display "formating: ") (write value) (display ", type: ")
    ;;(write (if (nothing? spec) "<nothing>" (spec-type spec))) (newline)
    (cond
     ((nothing? spec)            (display value port))
     ((equal? (spec-type spec) "s") (write value port))
     (else (display value port))))

  (define (format-arguments fmt-lst args port)
    (let loop ((iter-args args)
               (items     fmt-lst))
      (cond
       ((null? items)         #f)

       ((string? (car items))
        (display (car items) port)
        (loop iter-args (cdr items)))

       ((pair?   (car items))
        (cond
         ((nothing? (caar items))
          (when (null? iter-args)
            (raise "format: not enough arguments"))
          (format-value (cdar items)
      		  (car iter-args)
      		  port)
          (loop (cdr iter-args)
      	  (cdr items)))

         ((number? (caar items))
          (when (>= (caar items) (length args))
            (raise "format: not enough arguments"))
          (format-value (cdar items)
      		  (list-ref args (caar items))
      		  port)
          (loop iter-args
      	  (cdr items)))

         ((string? (caar items))
          (raise "format: named arguments not supported"))

         (else (raise "format: sanity failure"))))

       (else (raise "format: sanity failure")))))

  (define (formatter str)
    (let ((fmt-lst (parse-format-string str)))
      (lambda args
        (let-values (((output g) (open-string-output-port)))
          (format-arguments fmt-lst args output)
          (g)))))

  (define (format str . args)
    (apply (formatter str) args))

  (define (print str . args)
    (display (apply (formatter str) args)))

  (define (println str . args)
    (apply print str args) (newline))
)
