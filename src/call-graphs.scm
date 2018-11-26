(library (call-graphs)
  (export graph? make-graph graph-nodes graph-edges
          expression->call-graph
          
          node? node-name node-kind node-n-in node-n-out)

  (import (rnrs (6))
          (pfds sequences)
          (utility cut)
          (utility contexts)
          (utility receive)
          (utility pmatch)
          (utility algorithms)
          (format)
          (monads))

  (define-record-type node
    (fields name kind n-in n-out))

  #| A call graph is a collection of nodes and directed edges.
   | Each node has a name, kind, number of input arguments and
   | number of output arguments. The `kind` is one of `data`, 
   | `call`.
   | 
   |
   | Each edge is a pair of two pairs:
   | 
   |    ((from-idx . from-port) . (to-idx . to-port))
   |#
  (define-record-type graph
    (fields nodes edges))

  (define-context builder
    (fields nodes edges table))

  (define (get-or-fail-node key)
    (seq <state>
      (table <- (get-state builder-table))
      (item  :: (assoc key table))
      (if item
        (state-return (cdr item))
        (raise (format "graph builder: key not found in table: {}" key)))))

  (define (get-or-create-node key name kind n-in n-out)
    (seq <state>
      (table <- (get-state builder-table))
      (item  :: (assoc key table))
      (if item
        (state-return (cdr item))
        (create-node key name kind n-in n-out))))

  (define (create-node key name kind n-in n-out)
    (seq <state>
      (n <- (get-state (lambda (b) (with-builder b (sequence-size nodes)))))
      (update-state (lambda (b)
                      (update-builder b
                        (table (cons (cons key n) table))
                        (nodes (sequence-snoc nodes (make-node name kind n-in n-out))))))
      (state-return n)))

  (define (find-node key)
    (seq <state>
      (table <- (get-state builder-table))
      (state-return (assoc key table))))

  (define (get-expr-id expr)
    (pmatch expr
      ((output . ,args)
       (seq <state>
         (args <- (seq-map <state> get-expr-id args))
         (n    <- (create-node '(output output) 'output 'output (length args) 0))
         (add-links n args)
         (state-return (cons n 0))))

      ((input ,name ,index)
       (seq <state>
         (n <- (get-or-fail-node `(input ,name)))
         (state-return (cons n index))))

      ((const ,name)
       (seq <state>
         (n <- (create-node `(const ,name) name 'const 0 1))
         (state-return (cons n 0))))

      ((call ,f . ,args)
       (seq <state>
         (n    <- (find-node expr))
         (if n
           (state-return (cons (cdr n) 0))
           (seq <state>
             (args <- (seq-map <state> get-expr-id args))
             (n    <- (create-node expr f 'call (length args) 1))
             (add-links n args)
             (state-return (cons n 0))))))))

  (define (add-links n args)
    (seq <state>
      (new-edges :: (map (lambda (p arg) (cons arg (cons n p)))
                         (range (length args)) args))
      (update-state (lambda (b)
                      (update-builder b
                        (edges (append edges new-edges)))))))

  (define (expression->call-graph* expr)
    (seq <state>
      (get-expr-id expr)
      (nodes <- (get-state builder-nodes))
      (edges <- (get-state builder-edges))
      (state-return (make-graph nodes edges))))

  (define (expression->call-graph input expr)
    (let* ((nodes (list->sequence (map (lambda (x)
                                         (pmatch x
                                           ((,name ,n-out) (make-node name 'input 0 n-out))))
                                       input)))
           (table (map (lambda (i x)
                         (pmatch x
                           ((,name ,n-out) `((input ,name) . ,i))))
                       (range (length input))
                       input))
           (builder (make-builder nodes '() table)))
      (receive (graph _) ((expression->call-graph* expr) builder)
        graph)))
)
