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
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :parachute :test :polisher.test)))
