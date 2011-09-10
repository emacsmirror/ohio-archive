;;;Date: 3 Oct 88 22:23:30 GMT
;;;From: "Randal L. Schwartz @ Stonehenge" <oliveb!intelca!mipos3!merlyn@sun.com>
;;;Subject: a new version of define-cxxr-function
;;;Reply-To: "Randal L. Schwartz @ Stonehenge" <merlyn@intelob.intel.com>
;;;Organization: Stonehenge; netaccess via BiiN, Hillsboro, Oregon, USA

;;;Here's something for you hackers that want cXXr functions that run
;;;fast.  Enjoy.

;;; inspired by jrose@sun.com (John Rose)
;;; Make sure Common Lisp c[ad]+r functions are defined,
;;; and not as macros!
;;; LastEditDate="Mon Oct  3 13:09:13 1988" by merlyn

(provide 'cl-cadr)
(require 'byte-compile "bytecomp")

(defun define-cxxr-function (xx)
  "Given string XX, create standard function cXXr."
  (and (not (string-match "^[ad][ad]+$" xx))
       (error "define-cxxr-function: invalid argument"))
  (let ((f (intern (format "c%sr" xx)))
	(form 'x))
    (while (> (length xx) 0)
      (setq form (list (intern (format "c%sr" (substring xx -1))) form)
	    xx (substring xx 0 -1)))
    (fset f (byte-compile-lambda (list 'lambda '(x)
				       "Created by define-cxxr-function."
				       form)))))

(mapcar 'define-cxxr-function
	'("aa" "ad" "da" "dd"
	  "aaa" "aad" "ada" "add" "daa" "dad" "dda" "ddd"
	  "aaaa" "aaad" "aada" "aadd" "adaa" "adad" "adda" "addd"
	  "daaa" "daad" "dada" "dadd" "ddaa" "ddad" "ddda" "dddd"))
