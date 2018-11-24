#| Code generator for Cooley-Tukey FFT algorithm
 |#

(import (rnrs (6))
        (format)
        (utility cut)
        (utility algorithms)
        (pfds sequences)
        (call-graphs))

(define pi 3.14159265359)

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

(define (:* . args) (cons '* args))
(define (:+ . args) (cons '+ args))
(define (:- . args) (cons '- args))

(define (fft! in out)
  (let* ((n (array-size in))
         (m (/ n 2)))
    (assert (= n (array-size out)))
    (if (= 1 n)
      (array-copy! in out)
      (begin
        (fft! (array-slice in 0 2 n)
              (array-slice out 0 1 m))
        (fft! (array-slice in 1 2 n)
              (array-slice out m 1 n))
        (do ((k 0 (+ k 1)))
            ((= k m) '())
          (let* ((t (array-ref out k))
                 (u (array-ref out (+ k m)))
                 (w (:* (exp (* -2 pi +i (/ k n))) u)))
            (array-set! out k       (:+ t w))
            (array-set! out (+ k m) (:- t w))))))))

(define (fft x)
  (let* ((n   (vector-length x))
         (in  (make-array x n 1 0))
         (out (make-array (make-vector n) n 1 0)))
    (fft! in out)
    (array-data out)))

(define (vector-range n)
  (list->vector (range n)))

(display (vector-range 16)) (newline)
(display (fft (vector-range 16))) (newline)

(define (call-graph->dot g)
  (let ((dot-nodes (map (cut format "a{} [label=\"{:s}\"]" <> <>)
                        (range (sequence-size (graph-nodes g)))
                        (sequence->list (graph-nodes g))))
        (dot-edges (append-map (lambda (e)
                                 (let ((to (car e))
                                       (froms (cdr e)))
                                   (map (lambda (from) (format "a{} -> a{}" from to)) froms)))
                               (graph-edges g))))
    (format "digraph callgraph {{\n{}\n{}\n}}\n"
            (string-join dot-nodes "\n  ")
            (string-join dot-edges "\n  "))))

(display (call-graph->dot (expression->call-graph (fft (vector-range 16)))))
