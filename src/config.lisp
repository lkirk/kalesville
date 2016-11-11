(in-package :cl-user)
(defpackage kalesville.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :kalesville.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :kalesville))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defconfig :common ;; source of all common config
    `(:application-root ,(asdf:component-pathname (asdf:find-system :kalesville))))

(defconfig |production|
    '(:debug nil))

(defconfig |development| ;; useful for web dev, mock datasets
    '(:debug T
      :databases ((:maindb :sqlite3 :database-name #P"./dev-db.sqlite"))))

(defconfig |docker|
    '(:debug T
      :databases ((:maindb
		   :mysql
		   :database-name "kalesville-web"
		   :username "mysql"
		   :password "mysql"
		   :host "mysql"
		   :port 3306
		   ))))


(defconfig |test| ;; for more low level tests, blank db
    '(:debug T
      :databases ((:maindb :sqlite3 :database-name #P"./test-db.sqlite"))))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun dockerp ()
  (string= (appenv) "docker"))

(defun productionp ()
  (string= (appenv) "production"))
