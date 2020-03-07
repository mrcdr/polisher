(push #p"./" asdf:*central-registry*)
(ql:quickload :polisher.test)
(asdf:test-system :polisher)
;; If the test failed, error wouldn't be handled
;; and the process would exit with non-zero error code.
