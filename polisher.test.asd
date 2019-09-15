(asdf:defsystem :polisher.test
  :description "Test suite for polisher"
  :author "mrcdr"
  :depends-on (:polisher :1am)
  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "test")))))
