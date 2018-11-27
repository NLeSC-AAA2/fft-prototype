(library (call-graphs)
  (export graph? make-graph graph-nodes graph-edges
          expression->call-graph
          
          node? node-name node-kind node-n-in node-n-out
          
          call-graph->dot)

  (import (rnrs (6))
          (call-graphs call-graphs)
          (call-graphs dot))
)
