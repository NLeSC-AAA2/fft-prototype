(import (rnrs (6))
        (monads))

(define (div x y)
  (if (= 0 y)
    *nothing*
    (/ x y)))

(define (test-with-maybe)
  (define (inc x) (+ 1 x))
  (assert (nothing? (maybe-bind (div 1 0) inc)))
  (assert (= 3/2    (maybe-bind (div 1 2) inc)))
  (assert (= 42     (maybe-return 42))))

(define (test-seq-maybe)
  (define (f x)
    (seq <maybe>
      (a <- (div 1 x))
      (b <- (+ a 1))
      (maybe-return b)))

  (assert (nothing? (f 0)))
  (assert (= 4/3 (f 3))))
