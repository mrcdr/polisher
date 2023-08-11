(asdf:defsystem :polisher.test
  :description "Test suite for Polisher"
  :author "mrcdr"
  :license "MIT"
  :homepage "https://github.com/mrcdr/polisher"
  :depends-on (:polisher :parachute)
  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "test"))))
  :perform (test-op (o c)
             (symbol-call :polisher.test :run-test)))
