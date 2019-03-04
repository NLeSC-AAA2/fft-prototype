(library (utility boxes)
  (export box unbox box? box-set!)
  (import (rnrs (6)))

  (define-record-type (box-record box box?)
    (fields (mutable data unbox box-set!)))
)
