.PHONY: test
.SILENT: figures

test:
	guile --debug -L src -L . scripts/run-tests.scm ./test

figures:
	mkdir figures

figures/cooley-tukey.svg: src/cooley-tukey.scm figures
	guile -L src src/cooley-tukey.scm | dot -Tsvg > figures/cooley-tukey.svg

