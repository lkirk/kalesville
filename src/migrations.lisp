(in-package :cl-user)
(defpackage kalesville.migrations
  (:use :cl
        :kalesville.db
	:kalesville.config
        :datafly
	:sxql)
  (:export :km-create-table
	   :km-migration-complete?))
(in-package :kalesville.migrations)

(defun mark-migration-complete (migration-id)
  (format t "marking migration ~3,'0d complete~%" migration-id)
  (with-connection (db)
    (execute
     (insert-into :migration
       (set= :id migration-id)))))

(defun km-migration-complete? (migration-id)
  (with-connection (db)
    (if
     (handler-case
	 (retrieve-one
	  (select :id
	    (from :migration)
	    (where
	     (:= :id migration-id))))
       ;; if migration table does not exist...
       ;; TODO: be more specific? revisit if problems
       (dbi.error:<dbi-programming-error> () nil))
     t)))

(defun km-create-table (migration-id sql)
  (let ((dbi (config :databases))
	(env (appenv)))
    (if
     (km-migration-complete? migration-id)
     (format t "migration ~3,'0d: has already been run~%" migration-id)
     (progn
       (assert (not (null env)) (env)
	       "APP_ENV not set. Please specify a valid APP_ENV")

       (format t "migration ~3,'0d: begin~%" migration-id)
       (format t "db connection: ~A~%" dbi)
       (format t "env: ~A~%" env)
       (format t "creating table with the following sql cmd:~%~A~%" sql)

       (with-connection (db)
	 (execute sql))

       (mark-migration-complete migration-id))))

  (format t "migration ~3,'0d: completed successfully, migration-status ~A~%"
	  migration-id (km-migration-complete? migration-id)))
