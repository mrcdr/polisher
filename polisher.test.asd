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
             (let ((*package* (find-package :polisher.test)))
               (symbol-call :1am :run))))
