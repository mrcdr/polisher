(push #p"./" asdf:*central-registry*)
(ql:quickload :polisher.test)
(parachute::test-toplevel :polisher.test)
