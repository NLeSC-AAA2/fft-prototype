(library (utility algorithms)
  (export append-reverse append-map string-join unfold range)
  (import (rnrs (6)))

  (define (append-reverse rev-head tail)
    (if (null? rev-head)
	tail
	(append-reverse
	 (cdr rev-head)
	 (cons (car rev-head) tail))))

  (define (append-map f . args)
    (apply append (apply map f args)))

  (define (range n)
    (let loop ((n n)
               (result '()))
      (if (zero? n)
          result
          (loop (- n 1) (cons (- n 1) result)))))
  #|
  (define (fold-left f acc . lsts)
    (if (null? (car lsts))
	acc
	(apply
	 fold-left
	 f
	 (apply f acc (map car lsts))
	 (map cdr lsts))))

  (define (fold-right f acc . lsts)
    (reverse
     (fold-left
      (lambda (acc . args)
	(apply f (append-reverse args (list acc))))
      acc
      (map reverse lsts))))
  |#

  (define unfold
    (case-lambda
      ((p f g seed) (unfold p f g seed (lambda (x) '())))
      ((p f g seed tail-gen)
       (do ((x seed (g x))
	    (result '() (cons (f x) result)))
	   ((p x) (cons (tail-gen x) (reverse result)))))))
  
  (define (string-join lst sep)
    (do ((result ""   (string-append result sep (car lst)))
	 (lst    lst  (cdr lst)))
	((null? lst) result)))
)
