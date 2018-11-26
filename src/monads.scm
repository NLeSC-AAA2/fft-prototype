(library (monads)
  (export seq make-monad monad? monad-return monad-bind <- ::
          <maybe> maybe-bind maybe-return *nothing* nothing?
          <state> state-bind state-return get-state set-state
          update-state
          
          seq-map)

  (import (rnrs (6))
          (monads monads)
          (monads maybe)
          (monads state)
          (monads support))
)
