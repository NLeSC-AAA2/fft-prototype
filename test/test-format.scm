(import (rnrs (6))
	(format)
	(testing assertions))

(define (test-format)
  (assert-equal
   (format "{}, {}!" "Hello" "World")
   "Hello, World!")
  (assert-equal
   (format "{0:s} - {0}!" "hello")
   "\"hello\" - hello!")
  (assert-equal
   (format "{0} {2} {1}" 1 2 3)
   "1 3 2")
  (assert-equal
   (format "{:s}" '(a b c))
   "(a b c)"))

(define (test-format-text)
  (assert-equal
    (format "Hello, World!")
    "Hello, World!"))

(define (test-format-curlies)
  (assert-equal
    (format "digraph callgraph {{\n{}\n{}\n}}" "a" "b")
    "digraph callgraph {\na\nb\n}")
  (assert-equal
    (format "hello {{}} blah") "hello {} blah")
  (assert-equal
    (format "dingo }}{{") "dingo }{"))

