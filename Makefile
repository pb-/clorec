test:
	clojure -X:test
.PHONY: test

develop:
	clojure -M:nrepl
.PHONY: develop
