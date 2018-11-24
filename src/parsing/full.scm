#|!
 | Parsing
 | =======
 |
 | A parser takes a text-cursor and an auxiliary state and returns three
 | values, being the result, a new state, and the updated text-cursor.
 |
 | The text-cursor selects a block of text with a start and end position.
 | Each time the cursor is advanced, the block of text grows by one
 | character. This way, we don't need to manually copy each character
 | as it passes the parser. For instance, reading an integer can be done
 | by passing numeric characters with out memorizing them, only to flush
 | the text-cursor once we're convinced no more characters need to be read.
 |
 | On a higher level, the auxiliary variable may be used as a stack to
 | build up a list of objects, or if so desired, other data structures.
 | This is used most obviously in the (many p) function, which would not
 | be tail-recursive without the auxiliary variable.
 |#

(library (parsing full)
  (export parse-string seq-parsing parse-bind parse-return
          item fail choice optional *nothing*
          pop push cons-top update-top
          many many* some
          many-char many-char* some-char
          literal sep-by satisfies
          flush ignore

          many-end-with*

          char= char!= word space space?
          tokenize

          parser? parser-name parser-call make-parser)

  (import (rnrs (6))
          (monads)
          (monads receive)
          (parsing predicates)
          (parsing private cut)
          (parsing private text-cursors))

  (define (string-join lst sep)
    (if (null? lst)
      ""
      (let loop ((result (car lst))
                 (rest (cdr lst)))
        (if (null? rest)
          result
          (loop (string-append result sep (car rest))
                (cdr rest))))))

  (define-record-type nothing)
  (define *nothing* (make-nothing))

  (define-record-type parser
    (fields (immutable name parser-name*)
            (immutable precedence parser-precedence*)
            function)
    (protocol
      (lambda (new)
        (case-lambda
          ((n p) (make-parser n -1 p))
          ((n prec p)
           (new n prec
                (if (parser? p) (parser-function p) p)))))))

  (define (parser-precedence p)
    (if (parser? p)
      (parser-precedence* p)
      -1))

  (define (name-unary-operator precedence symb parser)
    (string-append
      (if (< precedence (parser-precedence parser))
        (string-append "(" (parser-name parser) ")")
        (parser-name parser))
      symb))

  (define (name-many-operator precedence symb . parsers)
    (string-join
      (map (lambda (parser)
             (if (< precedence (parser-precedence parser))
               (string-append "(" (parser-name parser) ")")
               (parser-name parser))) parsers)
      symb))

  (define (parser-call p c a)
    (if (procedure? p)
      (p c a)
      ((parser-function p) c a)))

  (define (parser-name p)
    (if (procedure? p)
      "<?>"
      (parser-name* p)))

  (define (parse-bind parse f)
    #|! Bind operator in parsing monad.
     |     λ (parse: [m a] f: λ (a) -> [m b]) -> [m b]
     |#
    (lambda (cursor aux)
      (receive (result cursor aux)
               (parser-call parse cursor aux)
        (if (failure? result)
          (values result cursor aux)
          (parser-call (f result) cursor aux)))))

  (define (parse-return value)
    #|! Return operator in parsing monad.
     |     λ (value: a) -> [m a]
     |#
    (make-parser
      "<>"
      (lambda (cursor aux)
        (values value cursor aux))))

  (define-monad parsing parse-bind parse-return)

  (define (parse-string p s)
    (let ((c (string->text-cursor s)))
      (receive (result c a) (parser-call p c '())
        result)))

  (define item
    #|! Get a single character from a stream.
     |#
    (make-parser "<char>"
      (lambda (cursor aux)
        (if (text-cursor-end? cursor)
          (values (make-failure (eof-object) '(item) aux)
                  cursor
                  aux)
          (values (text-cursor-ref cursor)
                  (text-cursor-next cursor)
                  aux)))))

  (define (choice parse . ps)
    #|! Parses with `parse`. If that fails, parses with
     | (car ps), and so on.
     |#
    (define (choice2 p1 p2)
      (lambda (c1 a1)
        (receive (v c2 a2) (parser-call p1 c1 a1)
          (if (failure? v)
            (parser-call p2 c1 a1)
            (values v c2 a2)))))

    (make-parser
      (apply name-many-operator 10 " | " parse ps) 10
      (fold-left choice2 parse ps)))

  (define optional
    (case-lambda
      ((p)   (optional p *nothing*))
      ((p v) (make-parser
               (name-unary-operator 0 "?" p) 0
               (choice p (parse-return v))))))

  (define fail
    #|! (fail) always fails returning *failure*
     |  (fail exc trc) always fails returning failure
     |    with specified exception and trace.
     |#
    (case-lambda
      (()
       (lambda (c a)
         (values *failure* c a)))
      ((exc trc)
       (lambda (c a)
         (values (make-failure exc trc a) c a)))))

  (define pop
    #|! Pop a value from the auxiliary state.
     |  By default this also reverses the value, which is then
     |  assumed to be a list; optionally a different mapping
     |  may be specified.
     |#
    (case-lambda
      (()  (pop reverse))
      ((f) (lambda (c a)
             (values (f (car a)) c (cdr a))))))

  (define (push value)
    #|! Push a value to the auxiliary state. Returns *nothing*.
     |#
    (lambda (c a)
      (values *nothing* c (cons value a))))

  (define (update-top f)
    #|! Modify the top value in the auxiliary state stack.
     |#
    (lambda (c a)
      (values *nothing* c (cons (f (car a)) (cdr a)))))

  (define (cons-top value)
    #|! Cons a value on to the top list in the aux stack.
     |#
    (update-top (cut cons value <>)))

  (define (set-aux new-a)
    (lambda (c a)
      (values *nothing* c new-a)))

  (define (get-aux)
    (lambda (c a)
      (values a c a)))

  (define (ignore parse)
    (seq-parsing
      (x <- (get-aux))
      parse
      (set-aux x)))

  (define flush
    #|! Return the selected sub-string and flush the text-cursor
     |  Optionally call with a function argument by which the
     |  sub-string should be mapped.
     |#
    (case-lambda
      (()  (flush values))
      ((f) (lambda (c a)
             (values (f (text-cursor-select c))
                     (text-cursor-flush c)
                     a)))))

  (define (many* parse)
    #|! Repeat a parser, consing return values to aux top,
     |  until the parser fails.
     |#
    (optional
      (seq-parsing
        (x <- parse)
        (if (not (nothing? x))
          (cons-top x) (return *nothing*))
        (many* parse))))

  (define many
    #|! Parse zero or more times.
     |    `(many p)` many parses with `p`, returns list
     |    `(many p f)` many parses with `p`, pops from auxiliary
     |      stack passing result through `f`.
     |  As `p` repeats, values are pushed to the aux top in
     |  reverse order, so the default value for `f` is `reverse`.
     |#
    (case-lambda
      ((p)   (many p reverse))
      ((p f) (make-parser
               (name-unary-operator 0 "*" p) 0
               (seq-parsing
                 (push '())
                 (many* p)
                 (pop f))))))

  (define some
    #|! Parse one or more times.
     |    see also: many
     |#
    (case-lambda
      ((p)   (some p reverse))
      ((p f) (make-parser
               (name-unary-operator 0 "+" p) 0
               (seq-parsing
                 (push '())
                 (x <- p)
                 (cons-top x)
                 (many* p)
                 (pop f))))))

  (define (many-char* parse)
    #|! Repeat a parser until it fails, the only way the actions
     |  by this parser are stored, is through the text-cursor.
     |#
    (optional
      (seq-parsing
        parse
        (many-char* parse))))

  (define many-char
    #|! Repeat a parser until it fails, passing the resulting string
     |  through function `f` upon return. This will flush the text-cursor
     |  before and after.
     |#
    (case-lambda
      ((p)   (many-char p values))
      ((p f) (make-parser
               (string-append (parser-name p) "*")
               (seq-parsing
                 (flush)
                 (many-char* p)
                 (flush f))))))

  (define some-char
    #|! Repeat a parser one ore more times, until it fails. Returns a string.
     |#
    (case-lambda
      ((p)   (some-char p values))
      ((p f) (make-parser
               (string-append (parser-name p) "+")
               (seq-parsing
                 (flush)
                 p
                 (many-char* p)
                 (flush f))))))

  (define (literal str)
    (make-parser
      (string-append "\"" str "\"")
      (let ((l (string-length str)))
        (lambda (c a)
          (let ((text (text-cursor-select-forward c l)))
            (if (and text (string=? text str))
              (values str (text-cursor-forward c l) a)
              (values *failure* c a)))))))

  (define (sep-by p sep)
    (make-parser
      (string-append (parser-name p) " [" (parser-name sep) " ...]") 100
      (optional
        (seq-parsing
          (a  <- p)
          (as <- (many (seq-parsing sep p)))
          (return (cons a as)))
        '())))

  (define (satisfies parse predicate)
    (seq-parsing
      (result <- parse)
      (if (predicate result)
        (return result)
        (fail))))

  (define (char-in lst)
    (lambda (char)
      (memq char lst)))

  (define (char= . cs)
    (satisfies item (char-in cs)))

  (define (char!= . cs)
    (satisfies item (*not (char-in cs))))

  (define space?
    (many-char (satisfies item char-whitespace?)))

  (define space
    (some-char (satisfies item char-whitespace?)))

  (define word
    (some-char (satisfies item char-alphabetic?)))

  (define (tokenize parse)
    (seq-parsing
      space? (x <- parse) space?
      (return x)))

  (define (many-end-with* parse str)
    (choice
      (literal str)
      (seq-parsing
        parse
        (many-end-with* parse str))))

  (define (many-end-with-exc* parse end f)
    (choice
      (seq-parsing (x <- (flush f)) end (return x))
      (seq-parsing parse (many-end-with-exc* parse end f))))

  (define (enclosed start parse end f)
    (seq-parsing
      start
      (flush)
      (many-end-with-exc* parse end f)))
)
