(library (call-graphs)
  (export graph? make-graph graph-nodes graph-edges
          expression->call-graph)

  (import (rnrs (6))
          (pfds sequences)
          (pfds hamts)
          (cut)
          (receive)
          (monads))

  (define-record-type graph
    (fields nodes edges))

  (define-record-type :state
    (fields nodes edges table))

  (define (:state-set-nodes s nodes)
    (let ((edges (:state-edges s))
          (table (:state-table s)))
      (make-:state nodes edges table)))

  (define (:state-set-edges s edges)
    (let ((nodes (:state-nodes s))
          (table (:state-table s)))
      (make-:state nodes edges table)))

  (define (:state-set-table s table)
    (let ((nodes (:state-nodes s))
          (edges (:state-edges s)))
      (make-:state nodes edges table)))

  (define (get-expr-id expr)
    (seq <state>
      (table <- (get-state :state-table))
      (if (hamt-contains? table expr)
          (state-return (hamt-ref table expr #f))
          (seq <state>
            (n <- (add-new-expr expr))
            (table <- (get-state :state-table))
            (update-state (cut :state-set-table <>
                               (hamt-set table expr n)))
            (state-return n)))))

  (define (seq-map M f . args)
    (if (null? (car args))
      ((monad-return M) '())
      (seq M
        (first <- (apply f (map car args)))
        (rest  <- (apply seq-map M f (map cdr args)))
        ((monad-return M) (cons first rest)))))

  (define (add-new-expr expr)
    (cond
      ((list? expr)
       (seq <state>
         (args  <- (seq-map <state> get-expr-id (cdr expr)))
         (n     <- (add-node (car expr)))
         (add-links n args)
         (state-return n)))

      (else (add-node expr))))

  (define (add-node expr)
    (seq <state>
      (nodes <- (get-state :state-nodes))
      (n     :: (sequence-size nodes))
      (update-state (cut :state-set-nodes <>
                         (sequence-snoc nodes expr)))
      (state-return n)))

  (define (add-links n args)
    (seq <state>
      (edges <- (get-state :state-edges))
      (update-state (cut :state-set-edges <>
                         (cons (cons n args) edges)))))

  (define (expression->call-graph* expr)
    (seq <state>
      (get-expr-id expr)
      (nodes <- (get-state :state-nodes))
      (edges <- (get-state :state-edges))
      (state-return (make-graph nodes edges))))

  (define (expression->call-graph expr)
    (let ((state (make-:state (make-sequence) '() (make-hamt equal-hash eq?))))
      (receive (graph _) ((expression->call-graph* expr) state)
        graph)))
)
