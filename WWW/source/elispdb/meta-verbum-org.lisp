(in-package :elispdb)

;;; see variables.lisp

;;; araneida is served from the checkout area in my home directory,
;;; because I am a bear of little brain and can't keep the contents
;;; of more than two checkout areas (here and laptop) straight without
;;; expending far too much mental effort

(setf *araneida-site-name* "meta.verbum.org")
(setf *araneida-source-directory* #p"/var/www/")

;;; static content comes from /usr/local/doc which always reflects the
;;; current cvs repository state (commitinfo hook)

(setf *static-site-name* "meta.verbum.org")
(setf *static-pages-directory* #p"/var/www/")

