(in-package :cl-user)
(defpackage "ELISPDB"
  (:use "PCRE" "BRKDB" "COMMON-LISP" "EXT")
  (:export get-word-hash get-elae-entry make-elae-database make-word-database
	   archive-entry-entries archive-entry-auto-generated archive-entry-package
	   archive-entry-filename make-databases))

   
  