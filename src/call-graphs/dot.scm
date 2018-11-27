(library (call-graphs dot)
  (export call-graph->dot)
  (import (rnrs (6))
          (format)
          (pfds sequences)
          (utility algorithms)
          (utility cut)
          (call-graphs call-graphs))

  (define (format-symbol fmt . args)
    (string->symbol (apply format fmt args)))

  (define (symbol-range n fmt)
    (map (cut format-symbol fmt <>) (range n)))

  (define (node-html-table name n-in n-out)
    (let* ((td-attrs '((border . 1) (bgcolor . "#cccccc")
                       (height . "5pt") (width . "20pt")))
           (in-args  (append-map
                       (lambda (port-name) 
                         `((td (port . ,port-name) ,@td-attrs) (/td)))
                       (symbol-range n-in "inp{}")))
           (out-args (append-map
                       (lambda (port-name)
                         `((td (port . ,port-name) ,@td-attrs) (/td)))
                       (symbol-range n-out "out{}"))))
    `((table (border . 0) (cellspacing . 0) (style . "ROUNDED"))
        ,@(if (not (zero? n-in)) `((tr) ,@in-args (/tr)) '())
        (tr) (td (border . 1) (colspan . ,(max n-in n-out))) ,name (/td) (/tr)
        ,@(if (not (zero? n-out)) `((tr) ,@out-args (/tr)) '())
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
  
  (define (dot-table-node idx name n-in n-out)
    (let* ((html-table (format-html-list (node-html-table name n-in n-out))))
      (format "a{0} [shape=none label=<{1}>]" idx html-table)))
  
  (define (dot-node idx n)
    (dot-table-node idx (node-name n) (node-n-in n) (node-n-out n)))
  
  (define (call-graph->dot g)
    (let ((dot-nodes (map dot-node 
                          (range (sequence-size (graph-nodes g)))
                          (sequence->list (graph-nodes g))))
          (dot-edges (map (lambda (e)
                            (let ((from-idx  (caar e))
                                  (from-port (cdar e))
                                  (to-idx    (cadr e))
                                  (to-port   (cddr e)))
                              (format "a{}:out{} -> a{}:inp{}" from-idx from-port to-idx to-port)))
                          (graph-edges g))))
      (format "digraph callgraph {{\n{}\n{}\n}}\n"
              (string-join dot-nodes "\n  ")
              (string-join dot-edges "\n  "))))
)
