(import (rnrs (6))
        (monads)
        (utility receive)
        (parsing)
        (utility algorithms)

        (testing assertions))

(define (test-sep-by)
  (assert-equal
   (list "hello" "there" "how" "is" "this" "parsed")
   (parse-string
    (sep-by (tokenize word) (literal ","))
    "hello, there,   how  ,  is , this, parsed")))

(define (test-literal)
  (assert-equal
   (parse-string
    (literal "hello")
    "hello, world")
   "hello")

  (assert-equal
   (parse-string
    (seq <parsing>
         (x <- (literal "hello"))
         (y <- (literal ", world"))
         (parsing-return (cons x y)))
    "hello, world!")
   (cons "hello" ", world")))

(define (test-fold)
  (assert-equal
   (fold-left (lambda (x y) (cons y x))
              '() '(1 2 3 4 5))
   '(5 4 3 2 1)))


(define (test-choice)
  (assert-equal
   (parse-string
    (choice (literal "hello") (literal "world"))
    "world, hello")
   "world"))

(define (test-many)
  (assert-equal
   (parse-string
    (many-char (choice (char= #\a) (char= #\b)))
    "abbabababc")
   "abbababab"))
