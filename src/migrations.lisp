(in-package :cl-user)
(defpackage kalesville.migrations
  (:use :cl)
  (:import-from :kalesville.config
                :config)
  (:import-from :datafly
                :*connection*
                :connect-cached)
  (:export :create-table))
(in-package :kalesville.migrations)


;; Add code for migrating objects.
