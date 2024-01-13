CL            := ros
QUICKLOAD     := --load cl-polyhedral.asd --eval '(progn (load "cl-polyhedral.asd") (ql:quickload :cl-polyhedral))'

.DEFAULT_GOAL := help

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

.PHONY: test
test: ## Running the test harness
	$(CL) $(QUICKLOAD) --eval '(print (asdf:test-system :cl-polyhedral/test))' dynamic-space-size=4096
