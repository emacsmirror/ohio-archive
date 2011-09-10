;; -*- emacs-lisp -*-
;;
;; name: mailto.el 
;; version: 0.8
;; description: some support for rfc 2368 (mailto: urls)
;; creation date: 1998-11-07
;; author: "Sen Nagata" <sen@eccosys.com>
;; warning: not optimized at all

;; required:
;;
;;   -browse-url.el (i think this comes w/ emacs)
;;   -string.el (from elib) -- this should not be necessary any more

;; installation:
;;
;;   -place this file in a place where emacs can find it (emacs loadpath)

;; notes and todo:
;;
;;   -try to remove dependencies on other packages (because the code
;;    reuse is small, etc.)
;;   -i've used this code w/ emacs versions >= 20.3

;; from rfc 2368:
;;
;;      mailtoURL  =  "mailto:" [ to ] [ headers ]
;;      to         =  #mailbox
;;      headers    =  "?" header *( "&" header )
;;      header     =  hname "=" hvalue
;;      hname      =  *urlc
;;      hvalue     =  *urlc
;;
;;    "#mailbox" is as specified in RFC 822 [RFC822]. This means that it
;;    consists of zero or more comma-separated mail addresses, possibly
;;    including "phrase" and "comment" components. Note that all URL
;;    reserved characters in "to" must be encoded: in particular,
;;    parentheses, commas, and the percent sign ("%"), which commonly occur
;;    in the "mailbox" syntax.
;;
;;    "hname" and "hvalue" are encodings of an RFC 822 header name and
;;    value, respectively. As with "to", all URL reserved characters must
;;    be encoded.
;;
;;    The special hname "body" indicates that the associated hvalue is the
;;    body of the message. The "body" hname should contain the content for
;;    the first text/plain body part of the message. The mailto URL is
;;    primarily intended for generation of short text messages that are
;;    actually the content of automatic processing (such as "subscribe"
;;    messages for mailing lists), not general MIME bodies.
;;
;;    Within mailto URLs, the characters "?", "=", "&" are reserved.
;;
;;    Because the "&" (ampersand) character is reserved in HTML, any mailto
;;    URL which contains an ampersand must be spelled differently in HTML
;;    than in other contexts.  A mailto URL which appears in an HTML
;;    document must use "&amp;" instead of "&".
;;
;;    Also note that it is legal to specify both "to" and an "hname" whose
;;    value is "to". That is,
;;
;;      mailto:addr1%2C%20addr2
;;
;;      is equivalent to
;;
;;      mailto:?to=addr1%2C%20addr2
;;
;;      is equivalent to
;;
;;      mailto:addr1?to=addr2
;;
;;    8-bit characters in mailto URLs are forbidden. MIME encoded words (as
;;    defined in [RFC2047]) are permitted in header values, but not for any
;;    part of a "body" hname.

(defconst mailto-version "mailto.el 0.8")

;; it would be nice to have a function which could do the equivalent of
;; perl's /e...so i wrote a limited version.  see 
;; 'string-replace-match-using-function'

;; not using string-replace-match from string.el from elib
;(require 'string)

;; we could use 'url-unhex-string' from url.el in w3, but i don't want
;; to force people to get w3 to use this...
(fset 'mailto-unhexify-string 'my-unhexify-string)

;; uses 'browse-url', but this is loaded autmatically?
;; i need to perform this 'require' explicity, because if i don't,
;; 'browse-url-url-at-point' doesn't seem to get defined if emacs is
;; running in terminal mode
(require 'browse-url)

;; removing explicit dependencies on browse-url in code that uses mailto.el
(fset 'mailto-url-at-point 'browse-url-url-at-point)

;; only an approximation...
;; see rfc 1738
(defconst mailto-url-regexp
;  "^\\([-a-zA-Z0-9+.]+:\\)\\([^?]*\\)\\?\\(.*\\)")
  ; isn't there a better way to achieve case insensitivity?
  ; need a better regexp...could we set 'case-fold-search' in
  ; functions that use this regular expression?
;  "^\\([mM][aA][iI][lL][tT][oO]:\\)\\([^?]*\\)\\(\\?\\(.*\\)\\)?")
;  "\\([mM][aA][iI][lL][tT][oO]:\\)\\([^?]*\\)\\(\\?\\(.*\\)\\)?"
  "\\([mM][aA][iI][lL][tT][oO]:\\)\\([^?]+\\)\\(\\?\\(.*\\)\\)*"
  )
;  "\\([mM][aA][iI][lL][tT][oO]:\\)\\([^?]*\\)\\(\\?\\(.*\\)\\)*[^>]*.*$")

;; describes 'mailto:'
(defconst mailto-url-scheme-index 1)
;; describes the portion of the url between 'mailto:' and '?'
;; i'm going to call this part the 'prequery'
(defconst mailto-url-prequery-index 2)
;; describes the portion of the url after '?'
;; i'm going to call this part the 'query'
(defconst mailto-url-query-index 4)

;; replace all occurences of "&amp;" w/ "&"
;; warning: haven't tested this lately
(defun mailto-html-unescape-url (mailto-url)
;  (string-replace-match "&amp;" mailto-url "&" t t))
  ;; much slower but independent of string.el
  (string-replace-match-using-function 
   "&amp;" 
   mailto-url 
   (lambda (x) ("&"))
  t))

;; parse a mailto: url into parts and return an alist containing these
;; components.  returned alist looks something like:
;;
;;  (("To:" . "mailinglist@domain.com") 
;;   ("Subject:" . "this is a subject")
;;   ("Body:" . "this is a body"))
;;
;; WARNING: the mailto: url is assumed NOT to have been taken from html --
;; so it should have '&' and not '&amp;'...
(defun mailto-parse-url (mailto-url)
  (let (scheme prequery query mailto-alist)
    (string-match mailto-url-regexp mailto-url)

    ;; possibly replace all occurences of "&amp;" w/ "&"...
    ;(setq mailto-url (mailto-html-unescape-url mailto-url))

    ;; unnecessary
    (setq scheme
	  (match-string mailto-url-scheme-index mailto-url))

    ;; necessary :-)
    ;; do we need to unhexify 'prequery'?
    (setq prequery 
	  (match-string mailto-url-prequery-index mailto-url))
    (setq query 
	  (match-string mailto-url-query-index mailto-url))

    ;; build our mailto-alist if 'query' is non-empty
    (if (not (null query))
	(setq mailto-alist
	      (mapcar
	       (lambda (x)
                 ;; there's a better way...
		 (let* ((temp-list (split-string x "="))
			(header-name (car temp-list))
			(header-value (cadr temp-list)))
                   ;; each element of our result looks something like:
                   ;;
                   ;;   ("From:" . "god@heaven.org")
		   (cons 
                    ;; capitalize and append colon to an unhexified header-name
		    (concat
		     (capitalize (mailto-unhexify-string header-name))
		     ":")
		    ;; unhexify header-value
		    (mailto-unhexify-string header-value))))
	       (split-string query "&"))))

    ;; if there is a 'prequery' portion, convert this part into the second
    ;; form described on page 2 of rfc 2368:
    ;;
    ;;   mailto:?to=addr1%2C%20addr2
    ;;
    ;; isn't it legal for this to be:
    ;;
    ;;   mailto:?to=addr1,%20addr2
    ;;
    ;; ?
    ;;
    ;; actually, don't bother converting...but modify mailto-alist
    ;; if necessary w.r.t. the "To:" cons cell

    ;; we need to do more if the 'prequery' portion is not empty
    (if (not (string-equal "" prequery))
	(progn
	  ;; if there's already a "To:" cons cell, modify the value
	  (if (assoc "To:" mailto-alist)
	      (let* ((our-cons-cell
		      (assoc "To:" mailto-alist))
		     (our-car ; unnecessary
		      (car our-cons-cell))
		     (our-cdr
		      (cdr our-cons-cell)))
		(setcdr 
		 our-cons-cell
		 (concat our-cdr ", " prequery)))
            ;; there isn't a "To:" cons cell, so make one and add it
	    (setq mailto-alist
		  (cons
		   (cons "To:" prequery)
		   mailto-alist)))))

    ;; the value we return...
    mailto-alist))

(defun mailto-extract-mailto-urls-from-string (string)
  "Extract mailto: urls from a string, STRING.

STRING may contain multiple mailto: urls.  The results are returned
as a list.  If there are no mailto: urls, nil is returned."
  (string-match-global "mailto:[^>]+" string t)
  )

;; what i want is something like m//g in perl...
(defun string-match-global (regexp string &optional case-insensitive)
  "Extract strings in STRING which match REGEXP.

The result is returned as a list of the matched strings.  If there were
no matches, nil is returned.

Optional arg CASE-INSENSTIVE toggles whether the search is case-insensitive.
By default, the search is case-sensitive."
  (let ((case-fold-search case-insensitive)
	(string-length (length string))
	(current-position 0)
	(strings-list nil))
    (while (< current-position string-length)
      (if (setq current-position
		(string-match regexp string current-position))
	  (progn
	    ;; keep track of each match
	    (setq strings-list
		  (cons (substring string
				   (match-beginning 0)
				   (match-end 0))
			strings-list))
	    ;; prepare to search rest of string
	    (setq current-position (match-end 0)))
	;; there were no more matches, so make the loop end
	;; by making things so that the loop condition fails
	(setq current-position string-length)))
    ;; our result
    strings-list
    ))

;; i'll make a better version some time...
;; want to be able to hand my-func multiple arguments, may be...
;; perhaps we can use '&rest args'...
(defun string-replace-match-using-function 
  (regexp string func &optional global)
  "Replace match of REGEXP in STRING with result of evaluating FUNC.
If not match is found, string should be returned as is.

Optional arg GLOBAL means to replace all matches instead of only the first."
  (let ((result-string "")
	(case-fold-search t))

    ;; look for every occurence?
    (if global
	(let ((last-position 0)
	      (current-position 0)
	      (string-length (length string)))

          ;; ugly?  may be...
	  (while (< current-position string-length)

	    (if (setq current-position
		      (string-match regexp string current-position))

                ;; there was a match, so append pre-match portion and 
                ;; transformed match portion
		(progn
                  ;; work on building result-string
		  (setq result-string 
			(concat result-string 
			        ;; what didn't match which came before the 
                                ;; match and after the last match (if any)
				(substring string 
					   last-position 
					   current-position)
			        ;; transform what matched...
				(funcall func 
					 (substring string
						    (match-beginning 0) 
						    (match-end 0)))))
			       
	          ;; update where we are looking in the string
		  (setq current-position (match-end 0))
		  (setq last-position current-position))

	      ;; there was no match, so append the rest of the string to 
              ;; result-string
	      (progn
	        ;; finish building result-string
		(setq result-string
		      (concat result-string
			      (substring string
					 last-position)))
	        ;; do this to fall out of the loop -- should be a better way
		(setq current-position string-length))
	      )))

      ;; not global -- only do things once
      (if (not (string-match regexp string 0))
	  (setq result-string string)
	(setq result-string
	      (concat (substring string 0 (match-beginning 0))
		      (funcall func
			       (substring string
					  (match-beginning 0)
					  (match-end 0)))
		      (substring string (match-end 0))))))

    ;; our result :-)
    result-string)
  )

;; warning: no uppercase letters -- perhaps i should do something about that...
(defun hex-to-dec (hex-digit)
  "Convert a hex digit [0-9a-f] to decimal."
  (if (>= hex-digit ?a)
      (+ (- hex-digit ?a) 10)
    (- hex-digit ?0)))

;; convert from a hex byte string (e.g. '20') to a string of one character
;; (e.g. '20' -> ' ')
(defun hex-byte-string-to-char-string (hex-byte-string)
  ;; convert to a string of one character
  (char-to-string
   ;; add the converted byte values together
   (+ 
    ;; convert the right/low byte
    (hex-to-dec
       (string-to-char (substring hex-byte-string 1 2)))
      ;; convert the left/high byte
      (* 16 
	 (hex-to-dec 
	  (string-to-char (substring hex-byte-string 0 1)))))))

;; takes a string like %20 and returns the corresponding ascii character
;; should just place a lambda version of this in my-unhexify-string...
(defun our-func (string)
  (hex-byte-string-to-char-string
   (substring string 1 3)))

;; just to remove the w3 dependency...
(defun my-unhexify-string (hex-string)
  (string-replace-match-using-function
   "%\\([0-9a-f][0-9a-f]\\)" hex-string 'our-func t))

(provide 'mailto)

; test
;(string-replace-match-using-function "%\\([0-9a-f][0-9a-f]\\)" 
;				     "%20hithere%20" 
;				     'our-func
;				     nil
;				     nil)

; elib candidate
; (defun string-replace-match-using-function (regexp string func &optional global)
;   "Replace match of REGEXP in STRING using the result of evaluating FUNC on the matched string.
; If no match is found, nil is returned instead of the new string.

; Optional arg GLOBAL means to replace all matches instead of only the first."

;   (let ((data (match-data)))
;     (unwind-protect

; 	(if global
; 	    (let ((result "") 
; 		  (start 0)
; 		  matchbeginning
; 		  matchend)
; 	      (while (string-match regexp string start)
; 		(setq matchbeginning (match-beginning 0)
; 		      matchend (match-end 0)
; 		      result (concat result
; 				     (substring string start matchbeginning)
; 				     (funcall func
; 					      (substring string
; 							 (match-beginning 0)
; 							 (match-end 0))))
; 		      start matchend))

; 	      (if matchbeginning	; matched at least once
; 		  (concat result (substring string start))
; 		nil))

; 	  ;; not GLOBAL
; 	  (if (not (string-match regexp string 0))
; 	      nil
; 	    (concat (substring string 0 (match-beginning 0))
; 		    (funcall func
; 			     (substring string
; 					(match-beginning 0)
; 					(match-end 0)))
; 		    (substring string (match-end 0)))))
;       (store-match-data data))))

; test
;(string-replace-match-using-function 
; "%\\([0-9a-f][0-9a-f]\\)" 
; "mailto:majordomo@huis-clos.mit.edu?body=unsubscribe%20scwm-discuss"
; 'our-func
; t)

; (defun string-replace-match-using-function 
;   (regexp string func &optional global args)
;   "Replace match of REGEXP in STRING using the result of evaluating FUNC on the matched string.
; If no match is found, nil is returned instead of the new string.

; Optional arg GLOBAL means to replace all matches instead of only the first.

; Optional arg ARGS is a list of indeces of subexpressions of REGEXP -- the
; intended use is to construct a list of arguments (the matching substrings)
; to pass to FUNC."

;   (let ((data (match-data)))
;     ; if 'args' was not specified, set it to a list containing 0 --
;     ; this means use the whole matched string as an argument to 'func'
;     (if (not args)
; 	(setq args (list 0)))

;     (unwind-protect

; 	;; find all matches
; 	(if global
; 	    (let ((result "") 
; 		  (start 0)
; 		  matchbeginning
; 		  matchend
; 		  func-args)
; 	      ; while there are matches...
; 	      (while (string-match regexp string start)
; 		(progn
; 		  (setq matchbeginning (match-beginning 0)
; 			matchend (match-end 0)
; 			; compute arguments to 'func'
; 			func-args (mapcar
; 				   (lambda (index)
; 				     (substring string
; 						(match-beginning index)
; 						(match-end index)))
; 				   args)
;                         ; compute the resulting string
; 			result (concat 
; 				result
; 				(substring string start matchbeginning)
; 				; compute the replacement text
; 				(apply func func-args))
; 		       start matchend)))

; 	      (if matchbeginning	; matched at least once
; 		  (concat result (substring string start))
; 		nil))

; 	  ;; not GLOBAL
; 	  (if (not (string-match regexp string 0))
; 	      nil
; 	    (let (func-args)
; 	      ; compute arguments to 'func'
; 	      (setq func-args (mapcar
; 			       (lambda (index)
; 				 (substring string
; 					    (match-beginning index)
; 					    (match-end index)))
; 			       args))
; 	      ; compute the resulting string
; 	      (concat (substring string 0 (match-beginning 0))
; 		      ; compute the replacement text
; 		      (apply func func-args)
; 		      (substring string (match-end 0)))))
;       (store-match-data data)))))

; tests
;  (string-replace-match-using-function
;   "%\\([0-9a-f][0-9a-f]\\)" 
;   "mailto:majordomo@huis-clos.mit.edu?body=unsubscribe%20scwm-discuss" 
;   'our-func 
;   t
;   '(0))

;  (string-replace-match-using-function
;   "%\\([0-9a-f][0-9a-f]\\)" 
;   "mailto:majordomo@huis-clos.mit.edu?body=unsubscribe%20scwm-discuss" 
;   'hex-byte-string-to-char-string
;   t
;   '(1))
