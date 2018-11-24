(library (parsing)
  (export
   ;; <parsing> monad
   <parsing> parsing-bind parsing-return

   ;; elementary parsers
   item fail choice optional
   pop push cons-top update-top
   one many many* some many-char many-char* some-char
   literal sep-by satisfies
   flush ignore

   ;; characters
   char= char!= word space space? char-in

   ;; practical parsers
   tokenize many-end-with* enclosed look-ahead

   ;; parser record
   parser? parser-name parser-call make-parser

   ;; utility
   parse-string)

  (import (rnrs (6))
	  
	  (utility receive)
	  (utility algorithms)
	  
	  (monads)
	  (text-cursors))

  (define-record-type failure
    (fields msg stack))

  (define-record-type parser
    (fields name* precedence function))

  (define (parser-call p c a)
    (if (procedure? p)
	(p c a)
	((parser-function p) c a)))

  (define (parser-name p)
    (if (procedure? p)
	"<?>"
	(parser-name* p)))

  (define (parsing-bind parser f)
    (lambda (cursor aux)
      (receive (result cursor aux)
	  (parser-call parser cursor aux)
	(if (failure? result)
	    (values result cursor aux)
	    (parser-call (f result) cursor aux)))))

  (define (parsing-return value)
    (lambda (cursor aux)
      (values value cursor aux)))

  (define <parsing> (make-monad parsing-bind parsing-return))

  (define (parse-string parser string)
    (let ((cursor (string->text-cursor string)))
      (receive (result cursor aux)
	  (parser-call parser cursor '())
	result)))

  (define item
    (lambda (cursor aux)
      (if (text-cursor-null? cursor)
	  (values (make-failure "end of text" (list item))
		  cursor
		  aux)
	  (values (text-cursor-ref cursor)
		  (text-cursor-next cursor)
		  aux))))

  (define (choice parser . rest)
    (define (choice2 parser1 parser2)
      (lambda (cursor1 aux1)
	(receive (result cursor2 aux2)
	    (parser-call parser1 cursor1 aux1)
	  (if (failure? result)
	      (parser-call parser2 cursor1 aux1)
	      (values result cursor2 aux2)))))

    (fold-left choice2 parser rest))

  (define (fail msg stack)
    (lambda (cursor aux)
      (values (make-failure msg stack) cursor aux)))

  (define optional
    (case-lambda
      ((parser)         (optional parser *nothing*))
      ((parser default) (choice parser (parsing-return default)))))

  (define pop
    (case-lambda
      (() (pop reverse))
      ((transfer)
       (lambda (cursor aux)
	 (values (transfer (car aux)) cursor (cdr aux))))))

  (define (push-cursor)
    (lambda (cursor aux)
      (values *nothing* cursor (cons cursor aux))))

  (define (pop-cursor)
    (lambda (cursor aux)
      (values *nothing* (car aux) (cdr aux))))

  (define (push value)
    (lambda (cursor aux)
      (values *nothing* cursor (cons value aux))))

  (define (update-top transfer)
    (lambda (cursor aux)
      (values *nothing* cursor (cons (transfer (car aux)) (cdr aux)))))

  (define (cons-top value)
    (update-top (lambda (aux) (cons value aux))))

  (define (set-aux new-aux)
    (lambda (cursor aux)
      (values *nothing* cursor new-aux)))

  (define (get-aux)
    (lambda (cursor aux)
      (values aux cursor aux)))

  (define (ignore parser)
    (seq <parsing>
	 (x <- (get-aux))
	 parser
	 (set-aux x)))

  (define flush
    (case-lambda
      (() (flush values))
      ((transfer)
       (lambda (cursor aux)
	 (values (transfer (text-cursor-select cursor))
		 (text-cursor-flush cursor)
		 aux)))))

  (define (many* parser)
    (optional
     (seq <parsing>
	  (x <- parser)
	  (if (not (nothing? x))
	      (cons-top x) (parsing-return *nothing*))
	  (many* parser))))

  (define many
    (case-lambda
      ((parser)          (many parser reverse))
      ((parser transfer) (seq <parsing>
			      (push '())
			      (many* parser)
			      (pop transfer)))))

  (define some
    (case-lambda
      ((parser)          (some parser reverse))
      ((parser transfer) (seq <parsing>
			      (push '())
			      (x <- parser)
			      (cons-top x)
			      (many* parser)
			      (pop transfer)))))

  (define (many-char* parser)
    (optional
     (seq <parsing>
	  parser
	  (many-char* parser))))

  (define many-char
    (case-lambda
      ((parser)          (many-char parser values))
      ((parser transfer) (seq <parsing>
			      (flush)
			      (many-char* parser)
			      (flush transfer)))))

  (define (one parser)
    (seq <parsing>
	 (x <- parser)
	 (flush)
	 (parsing-return x)))

  (define some-char
    (case-lambda
      ((parser) (some-char parser values))
      ((parser transfer) (seq <parsing>
			      (flush)
			      parser
			      (many-char* parser)
			      (flush transfer)))))

  (define (literal string)
    (let ((l (string-length string)))
      (lambda (cursor aux)
	(let ((text (text-cursor-peek cursor l)))
	  (if (and text (string=? text string))
	      (values string (text-cursor-flush
			      (text-cursor-forward cursor l)) aux)
	      (values (make-failure text
				    (list `(literal ,string)))
		      cursor aux))))))

  (define (sep-by parser sep)
    (optional
     (seq <parsing>
	  (a  <- parser)
	  (as <- (many (seq <parsing> sep parser)))
	  (parsing-return (cons a as)))
     '()))

  (define (satisfies parser predicate)
    (seq <parsing>
	 (result <- parser)
	 (if (predicate result)
	     (parsing-return result)
	     (fail "" '()))))

  (define (char-in lst)
    (let ((lst (if (string? lst) (string->list lst) lst)))
      (lambda (char)
	(memq char lst))))

  (define (char= . cs)
    (satisfies item (char-in cs)))

  (define (char!= . cs)
    (satisfies item (lambda (c) (not ((char-in cs) c)))))

  (define space?
    (many-char (satisfies item char-whitespace?)))

  (define space
    (some-char (satisfies item char-whitespace?)))

  (define word
    (some-char (satisfies item char-alphabetic?)))

  (define (tokenize parser)
    (seq <parsing>
	 space?
	 (x <- parser)
	 space?
	 (parsing-return x)))

  (define (many-end-with* parser str)
    (choice
     (literal str)
     (seq <parsing>
	  parser
	  (many-end-with* parser str))))

  (define (look-ahead parser ahead)
    (seq <parsing>
	 (x <- parser)
	 (push-cursor)
	 ahead
	 (pop-cursor)
	 (parsing-return x)))

  (define (many-end-with-exc* parser end transfer)
    (choice
     (seq <parsing> (x <- (flush transfer)) end (parsing-return x))
     (seq <parsing> parser (many-end-with-exc* parser end transfer))))

  (define (enclosed start parser end transfer)
    (seq <parsing>
	 start
	 (flush)
	 (many-end-with-exc* parser end transfer)))
)
