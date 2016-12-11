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

;; return html
@route GET "/"
(defun home ()
  (render #P"home.html"))

@route GET "/comments"
(defun comments ()
  (render #P"comments.html"))

@route GET "/recipes"
(defun recipes ()
  (render #P"recipes.html"))

@route GET "/recipe/:recipe-id"
(defun recipe ()
  (render #P"recipe.html"))

@route GET "/about"
(defun about ()
  (render #P"about.html"))

;; return json
@route GET "/api/comments"
(defun comments-api ()
  (render-json
   (with-connection (db)
     (retrieve-all
      (yield
       (select :*
	       (from :user_comments)))))))

@route GET "/api/recipes"
(defun recipes-api ()
  (render-json
   (with-connection (db)
     (retrieve-all
      (yield
       (select :*
	       (from :recipes)))))))

@route GET "/api/recipe/:recipe-id"
(defun recipe-api (&key recipe-id)
  (render-json
   (with-connection (db)
     (retrieve-one
      (select :*
	      (from :recipes)
	      (where (:= :id recipe-id))
	      )
      ))))

@route POST "/api/post-comment"
(defun insert-comment (&key (|author|) (|text|))
  ;; (format nil "the author ~A has just posted ~A~%" |author| |comment|)
  (let ((uid (format nil "~A" (uuid:make-v4-uuid))) ; coerces uuid to string
	(row (make-hash-table)))

    (setf (gethash 'id row) uid)
    (setf (gethash 'author row) |author|)
    (setf (gethash 'text row) |text|)

    (with-connection (db)
      (execute
       (insert-into :user_comments
	 (set= :id uid
	       :author |author|
	       :text |text|))))
    (render-json row)))

@route POST "/api/post-recipe"
(defun insert-recipe (&key (|title|) (|ingredients|) (|procedures|))
  (let ((uid (format nil "~A" (uuid:make-v4-uuid))) ; coerces uuid to string
	(row (make-hash-table)))

    (setf (gethash 'id row) uid)
    (setf (gethash 'title row) |title|)
    (setf (gethash 'ingredients row) |ingredients|)
    (setf (gethash 'procedures row) |procedures|)

    (with-connection (db)
      (execute
       (insert-into :recipes
	 (set= :id uid
	       :title |title|
	       :ingredients |ingredients|
	       :procedures |procedures|))))
    (render-json row)))


;;
;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
