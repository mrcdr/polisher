(asdf:defsystem :polisher.test
  :description "Test suite for Polisher"
  :author "mrcdr"
  :depends-on (:polisher :1am)
  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "test"))))
  :perform (test-op (o c)
             (symbol-call :polisher.test :run-test)))
