(push #p"./" asdf:*central-registry*)
(ql:quickload :polisher.test)
(uiop:quit (if (asdf:test-system :polisher) 0 1))
