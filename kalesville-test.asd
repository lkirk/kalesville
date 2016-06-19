(in-package :cl-user)
(defpackage kalesville-test-asd
  (:use :cl :asdf))
(in-package :kalesville-test-asd)

(defsystem kalesville-test
  :author ""
  :license ""
  :depends-on (:kalesville
               :prove)
  :components ((:module "t"
                :components
                ((:file "kalesville"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
