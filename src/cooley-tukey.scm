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

(define (format-symbol fmt . args)
  (string->symbol (apply format fmt args)))

(define (:* . args) (cons '× args))
(define (:+ . args) (cons '+ args))
(define (:- . args) (cons '- args))
(define (:wave-number k n) (format-symbol "w{}" k))
(define (wave-number k n) (exp (* -2i pi (/ k n))))

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
                 (w (if (zero? k)
                        u
                        (:* (:wave-number k n) u))))
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

(define (symbol-vector-range n fmt)
  (list->vector (map (cut format-symbol fmt <>) (range n))))

(define (symbol-range n fmt)
  (map (cut format-symbol fmt <>) (range n)))

;(display (vector-range 16)) (newline)
;(display (fft (vector-range 16))) (newline)

(define (function-node-html-table name n-args)
  (let* ((args (append-map
                 (lambda (port-name) 
                   `((td (port . ,port-name) 
                         (border . 1) (bgcolor . "#cccccc")
                         (height . "5pt") (width . "20pt"))
                     (/td)))
                 (symbol-range n-args "p{}"))))
  `((table (border . 0) (cellspacing . 0) (style . "ROUNDED"))
      (tr) ,@args (/tr)
      (tr) (td (border . 1) (colspan . ,n-args)) ,name (/td) (/tr)
    (/table))))

(define (format-html-list expr-list)
  (apply string-append (map format-html expr-list)))

(define (format-html expr)
  (cond
    ((string? expr) expr)
    ((symbol? expr) (format "{:s}" expr))
    ((and (pair? expr) (null? (cdr expr)))
     (format "<{:s}>" (car expr))) 
    ((pair? expr)
     (format "<{} {}>" (car expr) (format-html-attrs (cdr expr))))
    (else (raise (format "illegal html expr: {:s}" expr)))))

(define (format-html-attrs attrs)
  (string-join (map (lambda (kv-pair)
                      (format "{:s}=\"{}\"" (car kv-pair) (cdr kv-pair)))
                    attrs)
               " "))

(define (dot-function-node idx name n-args)
  (let* ((html-table (format-html-list (function-node-html-table name n-args))))
    (format "a{0} [shape=none label=<{1}>]" idx html-table)))

(define (dot-node idx n)
  (case (car n)
    ((call) (dot-function-node idx (cadr n) (caddr n)))
    ((data) (format "a{0} [shape=\"ellipse\" label=\"{1:s}\"]" idx (cadr n)))))

(define (call-graph->dot g)
  (let ((dot-nodes (map dot-node 
                        (range (sequence-size (graph-nodes g)))
                        (sequence->list (graph-nodes g))))
        (dot-edges (append-map (lambda (e)
                                 (let ((to (car e))
                                       (froms (cdr e)))
                                   (map (lambda (from p)
                                          (format "a{} -> a{}:p{}" from to p))
                                        froms (range (length froms)))))
                               (graph-edges g))))
    (format "digraph callgraph {{\n{}\n{}\n}}\n"
            (string-join dot-nodes "\n  ")
            (string-join dot-edges "\n  "))))

(display (call-graph->dot 
           (expression->call-graph 
             (cons 'output (vector->list (fft (symbol-vector-range 8 "x{}")))))))
             ; (cons 'list (vector->list (fft (vector-range 8)))))))
