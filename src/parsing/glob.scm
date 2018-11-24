(library (parsing glob)
  (export sep-by is-string sat parse-glob-pattern glob glob-element)

  (import (rnrs (6))

          (srfi :48)
          (monads)

          (parsing cut)
          (parsing predicates)
          (parsing simple)

          (monads receive)
          (parsing text-streams))

  (define (parse-string p s)
    (let ((c (make-cursor (open-string-input-port s))))
      (receive (value c*) (p c)
        value)))

  (define item
    (lambda (c)
      (let ((char (cursor-char c)))
        (if (eof-object? char)
          (values (make-failure "end-of-file" 'item c) c)
          (values char (cursor-advance c))))))

  (define (sat pred)
    (seq-parser
      (c <- item)
      (if (pred c)
        (return c)
        parser-failure)))

  (define (is-char c)
    (sat (cut char=? c <>)))

  (define (sep-by p sep)
    (define sep-by*
      (seq-parser
        (a  <- p)
        (as <- (many (seq-parser sep p)))
        (return (cons a as))))

    (choice sep-by* parser-failure))

  #| Read a sequence of characters identical to those in `lst` |#
  (define (is-list lst)
    (if (null? lst)
      (seq-parser (return '()))
      (seq-parser
        (is-char (car lst))
        (is-list (cdr lst))
        (return lst))))

  #| Read a string identical to `s` |#
  (define (is-string s)
    (seq-parser
      (u <- (is-list (string->list s)))
      (return (list->string u))))

  (define (one-of . x)
    (cut memq <> x))

  (define glob-element
    (some (choice
      (seq-parser (x <- (is-string "**")) (return 'double-glob))
      (seq-parser (x <- (is-string "*"))  (return 'glob))
      (seq-parser (x <- (some (sat (*not (one-of #\* #\/))))) (return (list->string x))))))

  (define glob
    (sep-by glob-element (is-string "/")))

  (define (parse-glob-pattern pattern)
    (parse-string glob pattern))
)
