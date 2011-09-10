(defpackage "BRKDB"
  (:use "COMMON-LISP"))
(in-package "BRKDB")
(use-package "EXT")
(use-package "ALIEN")
(use-package "C-CALL")

(def-alien-routine ("brkdb_open" %brkdb_open) (* t)
  (filename c-string))

(def-alien-routine ("brkdb_put" %brkdb_put) int
  (db (* t)) (keystr c-string) (valstr c-string))

(def-alien-routine ("brkdb_get" %brkdb_get) c-string
  (db (* t)) (keystr c-string))

(def-alien-routine ("brkdb_close" %brkdb_close) int
  (db (* t)))

(defun db-open (filename)
  "Open the Berkeley database named by FILENAME."
  (let ((ret (%brkdb_open filename)))
    (when (zerop (sys:sap-int (alien:alien-sap ret)))
      (error "Error opening database file ~A" filename))
    ret))
(declaim (inline btree-open))
(export 'db-open)

(defun db-put (db key val)
  "Associate KEY with VAL in Berkeley database DB."
  (%brkdb_put db key val))
(declaim (inline btree-put))
(export 'db-put)

(defun db-get (db str)
  "Return the value of STR in Berkeley database DB."
  (%brkdb_get db str))
(declaim (inline btree-get))
(export 'db-get)

(defun db-close (db)
  (%brkdb_close db))
(declaim (inline btree-close))
(export 'db-close)

(defmacro with-open-database ((dbvar filename) &body body)
  `(let ((,dbvar (db-open ,filename)))
     (unwind-protect
	 (progn ,@body)
       (db-close ,dbvar))))
(export 'with-open-database)