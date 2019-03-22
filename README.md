# FFT generator prototype

A series of experiments for code generation of FFT codes.

* Prototype written in R6RS scheme, generates graphs

![Graph of FFT](figures/cooley-tukey.svg)

* Adaptation of FFTW3

## Dependencies

* Scheme prototype

    - R6RS Scheme, either Guile 2 or Chez Scheme.
    - Graphviz

* GenFFT

    - OCAML
    - OCAML build (`apt install ocamlbuild`)

* Codelet runner

    - enTangleD (https://jhidding.github.io/enTangleD)
    - Python, NumPy
    - indent utility (`apt install indent`)

## Running

For unit tests, run `make test`.

