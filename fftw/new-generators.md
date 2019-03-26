# Adapting GenFFT

## Merging real and imaginary arguments

For creating an OpenCL kernel it is easier to pass the array arguments as a single pointer in stead of real/imaginary pair of pointers.

To translate from C arrays to indices in the code generator, GenFFT creates an array of symbolic values that reference indices into the C array somehow. The function that does this is called `locative_array_c`. It creates an array of *locatives*, which is a term used to identify values with a locational property, as being a member of a sequence.

In its original form `locative_array_c` takes separate arguments for the real and imaginary parts:

``` {.ocaml}
let locative_array_c n rarr iarr loc vs = 
  array n (fun i -> 
    let klass = Unique.make () in
    let (rloc, iloc) = loc i in
    (Variable.make_locative rloc klass rarr i vs,
     Variable.make_locative iloc klass iarr i vs))
```

I changed this to take only the one complex array argument and compute the real and imaginary indices.

``` {.ocaml}
let locative_array_z n carr loc vs =
  array n (fun i ->
    let klass = Unique.make () in
    let (rloc, iloc) = loc i in
    (Variable.make_locative rloc klass carr (2*i) vs,
     Variable.make_locative iloc klass carr (2*i + 1) vs))
```

Then, in the generator code, where we had

``` {.ocaml}
let input = 
  locative_array_c n 
    (C.array_subscript riarray vistride)
    (C.array_subscript iiarray vistride)
    locations sivs
```

We can write

``` {.ocaml}
let input =
  locative_array_z n
    (C.array_subscript ciarray vistride)
    locations sivs
```

I did the same for the output arrays and changed the definition of the for-loop to match. This changes the `notw` code from dual pointer to single complex pointer interface, although still passed as pointer to float.

That was all very nice, but it only works if the stride equals one. The better solution is to just add a little code that computes the imaginary pointer from the real one. The following code block replaces the `Asch annot` that was the body in the for loop before.

``` {.ocaml}
Block (
  [ Decl (C.realtypep, iioarray) ],
  [ Stmt_assign (CVar iioarray, CPlus [CVar rioarray; byvl (Integer 1)]);
    Asch annot ])
```

This feels a bit more like a hack than the previous non-solution, but it has the advantage that it actually works.
