(library (standard-algorithms)
  (export append-reverse string-join)
  (import (rnrs (6)))

  (define (append-reverse rev-head tail)
    (if (null? rev-head)
        tail
        (append-reverse
         (cdr rev-head)
         (cons (car rev-head) tail))))

  (define (string-join lst sep)
    (do ((result ""   (string-append result sep (car lst)))
         (lst    lst  (cdr lst)))
        ((null? lst) result)))
)
