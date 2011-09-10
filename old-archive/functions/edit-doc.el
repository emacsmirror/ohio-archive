;From utkcs2!emory!samsung!sdd.hp.com!decwrl!ads.com!saturn!ccm Thu Jul 12 13:36:20 EDT 1990
;Article 3181 of gnu.emacs:
;Path: utkcs2!emory!samsung!sdd.hp.com!decwrl!ads.com!saturn!ccm
;>From: ccm@warhol.ads.com (Chris McConnell)
;Newsgroups: gnu.emacs
;Subject: How to set function documentation
;Message-ID: <CCM.90Jul11150057@warhol.ads.com>
;Date: 11 Jul 90 19:00:57 GMT
;Sender: usenet@ads.com (USENET News)
;Distribution: gnu
;Organization: Advanced Decision Systems, Mountain View, CA 94043, +1 (415)
;	960-7300
;Lines: 23
;
;These functions will allow changes to the documentation string of a function.

;;;
(defun get-doc (function)
  "Return the raw documentation string of the symbol FUNCTION.  If
there is documentation, this will be either a string or a number, if
not it will probably be a list."
  (car (cdr (cdr (symbol-function function)))))

;;;
(defun set-doc (function string)
  "Set the documentation of the symbol FUNCTION to STRING."
  (let* ((old-function (symbol-function function))
	 (old-doc (cdr (cdr old-function)))
	 (doc (car old-doc)))
    ;; I did not use rplacd so that I can replace read-only objects
    (fset function
	  (nconc (list (car old-function)
		       (car (cdr old-function))
		       string)
		 (if(or (stringp doc) (numberp doc))
		    (cdr old-doc)
		    old-doc)))))


