(library (arrays)
  (export array? make-array array-data array-size array-stride array-offset
          array-slice array-copy! array-ref array-set!)

  (import (rnrs (6)))

  (define-record-type array
    (fields data size stride offset))
  
  (define (array-slice a start step end)
    (make-array
      (array-data a)
      (ceiling (/ (- end start) step))
      (* (array-stride a) step)
      (+ (array-offset a) (* start (array-stride a)))))
  
  (define (array-copy! a b)
    (do ((a-i (array-offset a) (+ a-i (array-stride a)))
         (b-i (array-offset b) (+ b-i (array-stride b)))
         (n (array-size a) (- n 1)))
        ((zero? n) '())
      (vector-set! 
        (array-data b) b-i
        (vector-ref (array-data a) a-i))))
  
  (define (array-ref a i)
    (vector-ref 
      (array-data a)
      (+ (array-offset a) (* (array-stride a) i))))
  
  (define (array-set! a i v)
    (vector-set!
      (array-data a)
      (+ (array-offset a) (* (array-stride a) i))
      v))
)
