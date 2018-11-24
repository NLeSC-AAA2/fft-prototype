.PHONY: test

test:
	guile --debug -L src -L . scripts/run-tests.scm ./test
