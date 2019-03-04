.PHONY: test
.SILENT: figures

test:
	guile --debug -L scheme -L . scripts/run-tests.scm ./test

figures:
	mkdir figures

figures/cooley-tukey.svg: scripts/cooley-tukey.scm figures
	guile -L src scripts/cooley-tukey.scm | dot -Tsvg > figures/cooley-tukey.svg

