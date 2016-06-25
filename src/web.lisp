(in-package :cl-user)
(defpackage kalesville.web
  (:use :cl
        :caveman2
        :kalesville.config
        :kalesville.view
        :kalesville.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :kalesville.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

@route GET "/"
(defun index ()
  (render #P"index.html"))

@route GET "/api/comments"
(defun comments ()
  (render-json
   (with-connection (db)
     (retrieve-all
      (yield
       (select :*
	 (from :user_comments)))))))



@route POST "/api/post-comment"
(defun insert-comment (&key (|user|) (|comment|))
  ;; (format nil "the user ~A has just posted ~A~%" |user| |comment|)
  (with-connection (db)
    (execute
     (insert-into :user_comments
       (set= :id (uuid:make-v4-uuid)
	     :user |user|
	     :comment |comment|)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
