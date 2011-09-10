;;; mew-refile-misc.el -- more mew-refile-guess-* functions

;;; Written by: Sen Nagata <sen@eccosys.com>
;;;

;; Keywords: refile, mew
;; Version: 0.2

;;; Commentary:
;;
;; installation:
;;
;;   -put this file in an appropriate directory so emacs can find it
;;
;;   -put:
;;
;;     (add-hook 'mew-init-hook (lambda () (require 'mew-refile-misc)))
;;
;;    in .emacs (or wherever you place your mew settings)
;;
;;   -modify the value of mew-refile-guess-control to use any combination of:
;;
;;    `mew-refile-guess-by-ml-headers'
;;    `mew-refile-guess-by-x-ml-name'
;;    `mew-refile-guess-by-mailing-list'
;;    `mew-refile-guess-by-x-mailing-list'
;;
;; questions:
;;
;;   -does mew provide any functions to extract addresses from header
;;    values?  there is `mew-header-parse-address-list2', but it drops
;;    'anonymous' addresses...looks like `mew-header-parse-address'
;;    may be what i am looking for :-)
;;
;; notes:
;;
;;   -'C-uo' is great!

;;; History:
;;
;; 0.2:
;;
;;  wrote `mew-refile-guess-by-ml-headers'
;;
;; 0.1:
;;
;;  initial implementation

;;; Code:
(defconst mew-refile-misc-version "mew-refile-misc.el 0.2")

;; actually need:
;;
;;   mew-func.el (for `mew-assoc-case-equal')
;;   mew-header.el (for `mew-header-get-value')
;;
;; will this work?
(eval-when-compile
  (require 'mew))

;;
;; by ml-headers returns: guess1 or nil
;;
(defun mew-refile-guess-by-ml-headers ()
  ;; yes, this is complicated -- i thought it was better to have something
  ;; than nothing even though i don't really like this way of doing things
  (let (
	list-id mailing-list x-mailing-list x-ml-name
	headers-list ent ret
	)

    ;; 'List-Id' is used in mailman
    ;; List-Id: Mailman mailing list management users <mailman-users.python.org>
    (if (setq list-id (mew-header-get-value "List-Id:"))
	(progn
	  (string-match "<\\([^>.]+\\)\\.[^>]+>$" list-id)
	  (setq headers-list
		(cons (match-string 1 list-id) 
		      headers-list))))

    ;; 'Mailing-List' is used by ezmlm    
    ;; Mailing-List: contact freshmeat-news-help@freshmeat.net; run by ezmlm
    (if (setq mailing-list (mew-header-get-value "Mailing-List:"))
	(progn
	  (string-match "\\([^ ]+\\)-help\\(@[^;]+\\);" mailing-list)
	  (setq headers-list
		(cons (match-string 1 mailing-list)
		      headers-list))))

    ;; 'X-Mailing-List' is used by smartlist
    ;; X-Mailing-List: <debian-devel@lists.debian.org> archive/latest/42880
    (if (setq x-mailing-list (mew-header-get-value "X-Mailing-List:"))
	(progn
	  (string-match "^<\\([^@]+\\)@[^>]+>" x-mailing-list)
	  ;;(string-match "^ *<\\([^@]+\\)@[^>]+>" x-mailing-list)
	  (setq headers-list
		(cons (match-string 1 x-mailing-list)
		      headers-list))))

    ;; 'X-ML-Name' is used by fml -- too bad this isn't helpful all the time
    ;; X-ML-Name: Mew-dist
    ;; X-ML-Name: Wanderlust
    (if (setq x-ml-name (mew-header-get-value "X-ML-Name:"))
	(progn
	  (string-match "^\\([a-zA-Z0-9_-]+\\)$" x-ml-name)
	  (setq headers-list
		(cons (match-string 1 x-ml-name)
		      headers-list))))

    ;; for the moment, use only the first guess if any
    (if headers-list
	(progn
	  (setq ent 
		(mew-assoc-case-equal (car headers-list) mew-folder-alist 1))
	  (if ent (setq ret (cons (nth 0 ent) ret)))))
    ret))

;;
;; by x-ml-name returns: guess1 or nil
;;
;; based on `mew-refile-guess-by-folder'
(defun mew-refile-guess-by-x-ml-name ()
  ;; typical examples:
  ;; X-ML-Name: Mew-dist
  ;; X-ML-Name: tm(ja) / tm ML ...
  ;; X-ML-Name: Wanderlust
  ;;
  ;; perhaps an alist of X-ML-Name: values to folder names would be useful?
  (let ((x-ml-name (mew-header-get-value "X-ML-Name:"))
	ent ret)
    (if x-ml-name
	(progn
	  (setq ent (mew-assoc-case-equal x-ml-name mew-folder-alist 1))
	  (if ent (setq ret (cons (nth 0 ent) ret)))))
    ret))

;;
;; by mailing-list returns: guess1 or nil
;;
;; based on `mew-refile-guess-by-folder'
(defun mew-refile-guess-by-mailing-list ()
  ;; typical example:
  ;; Mailing-List: contact freshmeat-news-help@freshmeat.net; run by ezmlm
  ;;
  ;; should try to extract address and then guess an address from the result
  (let ((mailing-list (mew-header-get-value "Mailing-List:"))
	ent ret ml-name)
    (if mailing-list
	(progn
	  (string-match "\\([^ ]+\\)-help\\(@[^;]+\\);" mailing-list)
	  (setq ml-name (mew-addrstr-extract-user
			 (concat (match-string 1 mailing-list)
				 (match-string 2 mailing-list))))
	  (setq ent (mew-assoc-case-equal ml-name mew-folder-alist 1))
	  (if ent (setq ret (cons (nth 0 ent) ret)))))
    ret))

;;
;; by x-mailing-list returns: (guess1 guess2 ...) or nil
;;
(defun mew-refile-guess-by-x-mailing-list ()
  ;; typical example:
  ;; X-Mailing-List: <debian-devel@lists.debian.org> archive/latest/42880
  ;;
  ;; what a hack...
  (let ((temp-list mew-refile-guess-key-list)
	results)
    (setq mew-refile-guess-key-list '("X-Mailing-List:"))
    (setq results (mew-refile-guess-by-folder))
    (setq mew-refile-guess-key-list temp-list)
    results))

(provide 'mew-refile-misc)

;;; mew-refile-misc.el ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; testing

; (setq mew-refile-guess-control
;       '(
;         mew-refile-guess-by-ml-headers
;         mew-refile-ctrl-throw
;         mew-refile-guess-by-x-ml-name
;         mew-refile-guess-by-mailing-list
; ; the following one can be done by adding "X-Mailing-List:" to 
; ; mew-refile-guess-key-list -- but, it may be useful to do this on its 
; ; own...
;         mew-refile-guess-by-x-mailing-list
;         mew-refile-ctrl-throw
; 	mew-refile-guess-by-alist
; 	mew-refile-guess-by-newsgroups
; 	mew-refile-guess-by-folder
; 	mew-refile-ctrl-throw
; 	mew-refile-ctrl-auto-boundary
; 	;; deprecated as of 1.94
; 	;mew-refile-guess-by-msgid
; 	;; new from 1.94
; 	mew-refile-guess-by-thread
; 	;; new from 1.94
; 	mew-refile-guess-by-from-folder
; 	mew-refile-guess-by-from
; 	mew-refile-guess-by-default
; 	))
