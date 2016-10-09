(in-package :cl-user)
(defpackage kalesville-asd
  (:use :cl :asdf))
(in-package :kalesville-asd)

(defsystem kalesville
  :version "0.2"
  :author "lkirk"
  :license "GPLv3"
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql
	       :uuid)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "migrations" :depends-on ("config" "db"))
                 (:file "config"))))
  :description "kalesville gardening/misc. enthusiasts!"
  :in-order-to ((test-op (load-op kalesville-test))))
