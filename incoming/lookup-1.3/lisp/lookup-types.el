;;; lookup-types.el --- Lookup various data types
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-types.el,v 1.4 1999/07/27 13:06:48 tsuchiya Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'evi)
(require 'lookup-utils)
(require 'lookup-vars)

(put 'lookup-defstruct 'lisp-indent-function 2)
(defmacro lookup-defstruct (name slots &rest args)
  ;; Lookup $B$GMQ$$$k%G!<%?9=B$$rDj5A$9$k$?$a$N%^%/%m!#(B
  ;; NAME $B$O%G!<%?$NL>A0!#(BSLOTS $B$O%G!<%?$,J];}$9$k>pJs$N%j%9%H!#(B
  ;; ARGS $B$ODI2C$N@_Dj!#2<5-$N<BNc$r;2>H!#(B
  ;; $B$3$l$K$h$jDj5A$5$l$?%G!<%?%?%$%W$O!"0J2<$N4X?t$r;}$D$3$H$K$J$k!#(B
  ;;
  ;; lookup-make-NAME     - $B%G!<%?$r@8@.$9$k(B
  ;; lookup-NAME-p        - $B%G!<%?%?%$%W$r%A%'%C%/$9$k(B
  ;; lookup-NAME-SLOT     - SLOT $B$N%G!<%?$r;2>H$9$k(B
  ;; lookup-NAME-set-SLOT - SLOT $B$K%G!<%?$r%;%C%H$9$k(B
  ;;
  ;; ARGS $B$K(B :with-properties $B%*%W%7%g%s$rIU$1$?>l9g!"99$K<!$N4X?t$,:n$i$l$k!#(B
  ;;
  ;; lookup-NAME-get-property - $B%*%V%8%'%/%H$NB0@-$rF@$k(B
  ;; lookup-NAME-put-property - $B%*%V%8%'%/%H$KB0@-$r@_Dj$9$k(B
  ;; lookup-NAME-PROP         - PROP $B$N%G!<%?$r;2>H$9$k(B
  ;; lookup-NAME-set-PROP     - PROP $B$K%G!<%?$r%;%C%H$9$k(B
  ;;
  (let* ((str (symbol-name name)) (n 0)
	 (prefix (concat "lookup-" str "-"))
	 (tag (list 'quote (intern (concat ":" str))))
	 (with-properties (memq ':with-properties args))
	 (properties (eval (plist-get args ':with-properties)))
	 ;; function names
	 (f-make (intern (concat "lookup-make-" str)))
	 (f-p (intern (concat prefix "p")))
	 (f-plist (intern (concat prefix "plist")))
	 (f-get-prop (intern (concat prefix "get-property")))
	 (f-put-prop (intern (concat prefix "put-property"))))
    (nconc (list 'progn
		 ;; Constructor:
		 ;;
		 ;; (defsubst lookup-make-NAME (SLOT...)
		 ;;   (vector :NAME SLOT... nil))
		 (let ((plist (if lookup-debug-mode '(lookup-new-plist))))
		   (list 'defsubst f-make slots
			 (cons 'vector
			       (cons tag (if with-properties
					     (append slots (list plist))
					   slots)))))
		 ;; Predicate:
		 ;;
		 ;; (defun lookup-NAME-p (NAME)
		 ;;   (and (vectorp NAME) (eq (aref NAME 0) :NAME)))
		 (list 'defun f-p (list name)
		       (list 'and (list 'vectorp name)
			     (list 'eq (list 'aref name 0) tag))))
	   ;; Accessors:
	   ;;
	   (apply 'nconc
		  (mapcar (lambda (slot)
			    (let* ((str (symbol-name slot))
				   (f-ref (intern (concat prefix str)))
				   (f-set (intern (concat prefix "set-" str))))
			      (list
			       ;; (defsubst lookup-NAME-SLOT (NAME)
			       ;;   (aref NAME n))
			       (list 'defsubst f-ref (list name)
				     (list 'aref name (setq n (1+ n))))
			       ;; (defsubst lookup-NAME-set-SLOT (NAME SLOT)
			       ;;   (aset NAME n SLOT))
			       (list 'defsubst f-set (list name slot)
				     (list 'aset name n slot)))))
			  slots))
	   ;; Properties:
	   ;;
	   (when with-properties
	     (setq n (1+ n))
	     (list
	      ;; (defmacro lookup-NAME-plist (NAME &optional plist)
	      ;;   (if plist (list 'aset NAME n plist) (list 'aref NAME n)))
	      (list 'defmacro f-plist (list name '&optional 'plist)
		    (list 'if 'plist
			  (if lookup-debug-mode
			      (` (list 'set (list 'aref (, name) (, n)) plist))
			    (list 'list ''aset name n 'plist))
			  (if lookup-debug-mode
			      (` (list 'symbol-value
				       (list 'aref (, name) (, n))))
			    (list 'list ''aref name n))))
	      ;; (defun lookup-NAME-get-property (NAME prop)
	      ;;   (plist-get (lookup-NAME-plist NAME) prop))
	      (list 'defun f-get-prop (list name 'prop)
		    (list 'plist-get (list f-plist name) 'prop))
	      ;; (defun lookup-NAME-put-property (NAME prop value)
	      ;;   (lookup-NAME-plist NAME
	      ;;     (plist-put (lookup-NAME-plist NAME) prop value)))
	      (list 'defun f-put-prop (list name 'prop 'value)
		    (list f-plist name
			  (list 'plist-put (list f-plist name)
				'prop 'value)))))
	   (when with-properties
	     (apply 'nconc
		    (mapcar (lambda (prop)
			      (let* ((str (symbol-name prop))
				     (ref (intern (concat prefix str)))
				     (set (intern (concat prefix "set-" str))))
				(list
				 ;; (defsubst lookup-NAME-PROP (NAME)
				 ;;   (lookup-NAME-get-property NAME 'PROP))
				 (list 'defsubst ref (list name)
				       (list f-get-prop name
					     (list 'quote prop)))
				 ;;(defsubst lookup-NAME-set-PROP (NAME PROP)
				 ;; (lookup-NAME-put-property NAME 'PROP PROP))
				 (list 'defsubst set (list name prop)
				       (list f-put-prop name
					     (list 'quote prop) prop)))))
			    properties))))))

(defun lookup-new-plist ()
  ;; plist $B$rJ];}$9$k$?$a$N%7%s%\%k$r@8@.$9$k!#(B
  ;; plist $B$r$=$N$^$^%G!<%?9=B$$KAH$_9~$`$H!"%G%P%C%0$N:]$K=PNO$,Bg$-$/(B
  ;; $B$J$j2a$.$F8+$E$i$/$J$k!#$=$N$?$a!"JQ?t(B `lookup-debug-mode' $B$,(B non-nil
  ;; $B$N>l9g$K$O!"%*%V%8%'%/%H$K$O$3$N%7%s%\%k$rJ|$j9~$s$G$*$-!"$=$NCM$H$7$F(B
  ;; plist $B$r=hM}$9$k!#(B
  (let ((plist (make-symbol "plist")))
    (set plist nil)
    plist))


;;;;;;;;;;;;;;;;;;;;
;: Search Method
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B3F<o8!:wJ}<0!#$=$l$>$l$N8!:wJ}<0$OC10l$N%7%s%\%k$K$h$C$FI=8=$5$l$k!#(B
;; $B8=:_!"<!$N$b$N$,M-8z!#(B
;;
;; exact     - $B40A40lCW8!:w(B
;; prefix    - $BA0J}0lCW8!:w(B
;; suffix    - $B8eJ}0lCW8!:w(B
;; substring - $BItJ,0lCW8!:w(B
;; regexp    - $B@55,I=8=8!:w(B
;; keyword   - $B%-!<%o!<%I8!:w(B
;; text      - $BA4J88!:w(B
;; reference - $B%j%U%!%l%s%9(B
;;
;; $BM?$($i$l$?8!:wJ}<0$+$i<B:]$K$I$N$h$&$J8!:w$r9T$J$&$+$O3F(B agent $B$K(B
;; $BG$$5$l$F$*$j!"87L)$J5,Dj$O$J$$!#$=$l$>$l$N8!:wJ}<0$K$D$$$F$N0lHLE*$J(B
;; $BDj5A$O!"%^%K%e%"%k$N(B "Search Methods" $B$r;2>H!#(B

(defconst lookup-word-search-methods
  '(exact prefix suffix substring regexp keyword stemming expansion text))

(defconst lookup-search-methods
  (cons 'default (cons 'reference lookup-word-search-methods)))

(defconst lookup-method-key-alist
  '((exact . "=") (prefix . "<") (suffix . ">") (substring . "-")
    (regexp . "r") (keyword . "@") (stemming . "^") (text . "/")
    (default . "*") (reference . "%") (expansion . "#")))

(defun lookup-method-key (method)
  (or (lookup-assq-ref lookup-method-key-alist method)
      (error "Unknown search method: %s" method)))


;;;;;;;;;;;;;;;;;;;;
;: Search Query
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B8!:w$9$k%Q%?!<%s$r3JG<$9$k$?$a$N%G!<%?%?%$%W!#(BLookup $B$O%f!<%6$NF~NO$r(B
;; $B2r@O$7!"8!:wJ}<0$K1~$8$F$3$N%?%$%W$N%*%V%8%'%/%H$r@8@.$9$k!#@8@.$5$l$?(B
;; $B%G!<%?$O:G=*E*$K8!:w%(!<%8%'%s%H$KEO$5$l!"%(!<%8%'%s%HKh$K$=$NFbMF$r(B
;; $BH=CG$7$F8!:w$,<B9T$5$l$k!#(B

;; Structure:
;;
;;   [:query METHOD STRING]
;;
;; METHOD $B$O8!:wJ}<0!#JQ?t(B `lookup-search-methods' $B$K$"$k%7%s%\%k$,M-8z!#(B
;; STRING $B$O8!:wJ8;zNs!#(BMETHOD $B$HBP$K$7$F!"8!:w$9$k%Q%?!<%s$,7hDj$5$l$k!#(B
;;   $B$3$NJ8;zNs$O$"$i$+$8$a!"ITMW$JJ8;z$r<h$j=|$/Ey!"$"$kDxEY$N@55,2=$r(B
;;   $B$7$F$*$/$3$H$,K>$^$7$$!#(B

(lookup-defstruct query (method string))

;; Functions:
;;
;; lookup-parse-pattern   - $B0z?t$NJ8;zNs$r2r@O$7!"(Bquery $B$r@8@.$9$k!#(B
;; lookup-query-to-regexp - query $B$r(B regexp $B%Q%?!<%s$KJQ49$9$k!#(B

(defun lookup-parse-pattern (pattern)
  ;; $BF~NOJ8;zNs(B PATTERN $B$r2r@O$7!"(Bquery $B$r@8@.$7$FJV$9!#(B
  (let (method)
    (cond
     ;; 'word' -> match exactly
     ((string-match "^'\\(.*\\)'$" pattern)
      (setq method 'exact pattern (match-string 1 pattern)))
     ;; /word/ -> match regexp
     ((string-match "^/\\(.*\\)/$" pattern)
      (setq method 'regexp pattern (match-string 1 pattern)))
     ;; /word  -> search text
     ((string-match "^/" pattern)
      (setq method 'text pattern (substring pattern 1)))
     ;; @word  -> match keyword
     ((string-match "^@" pattern)
      (setq method 'keyword pattern (substring pattern 1)))
     ;; *word* -> match substring
     ((string-match "^\\*\\([^*?]*\\)\\*$" pattern)
      (setq method 'substring pattern (match-string 1 pattern)))
     ;; word*  -> match prefixes
     ((string-match "^\\([^*?]*\\)\\*$" pattern)
      (setq method 'prefix pattern (match-string 1 pattern)))
     ;; *word  -> match suffixes
     ((string-match "^\\*\\([^*?]*\\)$" pattern)
      (setq method 'suffix pattern (substring pattern 1)))
     ;; w*o?d  -> "w.*o.d"
     ((string-match "[*?]" pattern)
      (setq pattern (if (string-match "^\\*" pattern)
			(substring pattern 1)
		      (concat "^" pattern)))
      (setq pattern (if (string-match "\\*$" pattern)
			(substring pattern 0 (match-beginning 0))
		      (concat pattern "$")))
      (let ((start 0))
	(while (string-match "*" pattern start)
	  (setq pattern (replace-match ".*" t t pattern)
		start (+ (match-end 0) 1))))
      (while (string-match "?" pattern)
	(setq pattern (replace-match "." t t pattern)))
      (setq method 'regexp pattern pattern))
     ;; default
     (t (setq method 'default)))
    (lookup-make-query method pattern)))

(defun lookup-query-to-regexp (query)
  ;; QUERY $B$r(B regexp $B%Q%?!<%s$KJQ49$9$k!#(B
  (let* ((method (lookup-query-method query))
	 (string (lookup-query-string query))
	 (quote (regexp-quote string)))
    (cond ((eq method 'keyword) (concat "\\<" quote "\\>"))
	  ((eq method 'prefix) (concat "^" quote))
	  ((eq method 'suffix) (concat quote "$"))
	  ((eq method 'exact) (concat "^" quote "$"))
	  ((eq method 'substring) quote)
	  ((eq method 'text) string)
	  ((eq method 'regexp) string)
	  (t (error "Illegal search method for regexp: %s" method)))))


;;;;;;;;;;;;;;;;;;;;
;: Search Module
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B8!:w%b%8%e!<%k$rI=$o$9%G!<%?%?%$%W!#3F%b%8%e!<%k$O$3$N%?%$%W$N(B
;; $B%*%V%8%'%/%H$H$7$F@8@.$5$l$k!#(B
;;
;; $B8!:w%b%8%e!<%k$H$O!"0l2s$N8!:w$NC10L$H$J$k<-=q$N$^$H$^$j$N$3$H!#(B
;; $BJ#?t$N%b%8%e!<%k$rMQ0U$9$k$3$H$G!"BP>]$N0[$J$kJ#?t$N8!:w%3%^%s%I$r(B
;; $BDj5A$9$k$3$H$,=PMh$k!#2<5-$N(B `Setup' $B$N9`$r;2>H!#(B

;; Structure:
;;
;;   [:module NAME DICTIONARIES PLIST]
;;
;; NAME $B$O!"%b%8%e!<%k$NL>>N!#(B
;; DICTIONARIES $B$O!"%b%8%e!<%k$K4^$^$l$k<-=q$N%j%9%H!#(B
;; PLIST $B$O!"B0@-%j%9%H$NJ];}$K;H$o$l$k!#(B

;; Properties:
;;
;; history     - $B%b%8%e!<%k$NMzNr!#(B
;; agents      - $B%b%8%e!<%k$N<-=q$,B0$9$k%(!<%8%'%s%H$N%j%9%H!#(B
;; id-list     - $B%b%8%e!<%k$NDj5A$K4^$^$l$k<-=q(BID $B$N%j%9%H!#(B
;; initialized - $B%b%8%e!<%k$,=i4|2=$5$l$F$$$l$P(B `t'$B!#(B

(lookup-defstruct module (name dictionaries)
  :with-properties '(history))

;; Initialize:
;;
;; $B%b%8%e!<%k$N=i4|2=$OFs$D$NCJ3,$G9T$J$o$l$k!#(B
;;
;; 1) $B%*%V%8%'%/%H$N@8@.(B
;;    $B%b%8%e!<%k!&%*%V%8%'%/%H$r@8@.$9$k!#$3$l$O%G!<%?$r@8@.$9$k$@$1$G!"(B
;;    $B0l@Z$N=i4|2=$O9T$J$o$J$$!#$3$l$r9T$J$&4X?t$O(B `lookup-new-module'$B!#(B
;; 2) $B%(!<%8%'%s%H$N=i4|2=(B
;;    $B@_Dj$K=>$C$F8!:w%(!<%8%'%s%H$r@8@.$7!"=i4|2=$r9T$J$&!#$3$l$O(B
;;    $B4X?t(B `lookup-module-init' $B$G9T$J$o$l$k!#(B
;;
;; $B$3$l$i$rJ,$1$k$N$O!"%f!<%6$N;H$$>!<j$r$h$/$9$k$?$a$G$"$k!#%b%8%e!<%k$N(B
;; $B@8@.$O(B Lookup $B$N5/F08e$9$0$K9T$J$o$l$k$,!"(BM-x lookup-pattern $B$J$I$G(B
;; Lookup $B$r5/F0$7$?$H$-!"8!:w8l$rF~NO$9$kA0$K%(!<%8%'%s%H$N%;%C%H%"%C%W(B
;; $B$^$G9T$J$o$l$?$N$G$OF~NO$^$G;~4V$,3]$+$j!"$d$d%9%H%l%9$H$J$k!#$=$3$G(B
;; $B=i4|2=$r8e2s$7$K$9$k$3$H$G>/$7$G$b5/F0$rAa$a!"F~NO$,9T$J$o$l$F$+$i(B
;; $B=i4|2=$r;O$a$k$h$&$K$7$F$$$k!#(B

(defun lookup-new-module (spec)
  (let ((name (car spec)) (id-list (cdr spec)) module agents)
    ;; get agent list
    (lookup-foreach (lambda (id)
		      ;; get the list of agents matched with ID
		      (let ((match (concat "^" (regexp-quote id)))
			    (list (lookup-agent-list)) (start agents))
			(while list
			  (if (string-match match (lookup-agent-id (car list)))
			      (setq agents (cons (car list) agents)))
			  (setq list (cdr list)))
			(if (eq start agents)
			    (error "No match agent: %s" id))))
		    ;; get a list of agent-IDs
		    (lookup-nunique (mapcar (lambda (id)
					      (string-match "^[^:]*" id)
					      (substring id 0 (match-end 0)))
					    id-list)))
    (setq agents (nreverse (lookup-nunique agents 'eq)))
    ;; construct module
    (setq module (lookup-make-module name nil))
    (lookup-module-put-property module 'agents agents)
    (lookup-module-put-property module 'id-list id-list)
    (lookup-module-init module)))

(defun lookup-module-init (module)
  (lookup-module-set-history module (lookup-new-history))
  module)

(defun lookup-module-setup (module)
  (unless (lookup-module-get-property module 'setup)
    (let ((agents (lookup-module-get-property module 'agents))
	  dicts dictionary-list)
      ;; setup agents
      (lookup-foreach 'lookup-agent-setup agents)
      ;; get dictionary list
      (setq dictionary-list
	    (apply 'append (mapcar 'lookup-agent-dictionaries agents)))
      (lookup-foreach (lambda (id)
			(let ((match (concat "^" (regexp-quote id)))
			      (list dictionary-list) (start dicts))
			  (while list
			    (if (string-match match (lookup-dictionary-id (car list)))
				(setq dicts (cons (car list) dicts)))
			    (setq list (cdr list)))
			  (if (eq start agents)
			      (error "No match dictionary: %s" id))))
		      (lookup-module-get-property module 'id-list))
      (lookup-module-set-dictionaries module (nreverse dicts))
      (lookup-module-put-property module 'setup t))))

(defun lookup-module-clear (module)
  (lookup-module-set-dictionaries module nil)
  (lookup-module-set-history module nil)
  (lookup-module-put-property module 'setup nil))


;;;;;;;;;;;;;;;;;;;;
;: Search Agent
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B8!:w%(!<%8%'%s%H$rI=$o$9%G!<%?%?%$%W!#3F<o%(!<%8%'%s%H$O$3$N%?%$%W$N(B
;; $B%*%V%8%'%/%H$H$7$F@8@.$5$l$k!#(B

;; Structure:
;;
;;   [:agent CLASS LOCATION OPTIONS PLIST]
;;
;; CLASS $B$O%(!<%8%'%s%H$N%/%i%9!#(BLOCATION $B$O<-=q$N=j:_!#(B
;; OPTIONS $B$O%*%W%7%g%s$N%j%9%H!#JQ?t(B `lookup-search-agents' $B$r;2>H!#(B
;; PLIST $B$O!"B0@-%j%9%H$NJ];}$K;H$o$l$k!#(B

;; Properties:
;;
;; id           - $B%(!<%8%'%s%H(BID$B!#(B("CLASS+LOCATION")
;; title        - $B%(!<%8%'%s%H$N%?%$%H%k!#(B
;; dictionaries - $B%(!<%8%'%s%H$K4^$^$l$k<-=q$rJ];}$9$k!#(B
;; defaults     - $B%(!<%8%'%s%H$N%G%U%)%k%H$N@_Dj$rJ];}$9$k!#(B

(lookup-defstruct agent (class location options)
  :with-properties '(id title dictionaries defaults))

;; Definitions:
;;
;; $B3F%(!<%8%'%s%H$OF1L>$N(B elisp $B%U%!%$%k$K$h$C$FDj5A$5$l!"(Brequire $B$K$h$j(B
;; $BFI$_9~$^$l$k!#$D$^$j3F%(!<%8%'%s%H$O(B provide $B$5$l$J$1$l$P$J$i$J$$!#(B
;;
;; $B%(!<%8%'%s%H$O8!:w<B9T$N$?$a$K$$$/$D$+$N%3%^%s%I$r;}$D!#3F%3%^%s%I$O(B
;; $BDj5A%U%!%$%k$NCf$G(B (put AGENT COMMAND FUNCTION) $B$N$h$&$KDj5A$5$l$k!#(B
;; $B8=:_!"<!$N%3%^%s%I$,0UL#$r;}$D!#(B($BBgJ8;z$N$O0z?t(B)
;;
;; setup AGENT
;;   $B%(!<%8%'%s%H$N%;%C%H%"%C%W!&%3%^%s%I!#$3$l$O%(!<%8%'%s%H$N=i4|2=$r(B
;;   $B9T$J$$!"MxMQ2DG=$J<-=q%*%V%8%'%/%H$N%j%9%H$rJV$9!#(B
;; clear AGENT
;;   $B%(!<%8%'%s%H$N=*N;%3%^%s%I!#$3$l$O%(!<%8%'%s%H$N8eJRIU$1$r9T$J$$!"(B
;;   Lookup $B$N=*N;$KHw$($k!#(B
;;
;; $B%(!<%8%'%s%H$O$$$/$D$+$N8GM-$NI8=`@_Dj$r;}$D!#$3$l$ODj5A%U%!%$%k$NCf$G(B
;; (put AGENT KEY VALUE) $B$N$h$&$KDj5A$5$l!"%f!<%6!&%*%W%7%g%s$K$h$j>e=q$-(B
;; $B2DG=$H$J$C$F$$$k!#<!$N(B `Options' $B$N9`$r;2>H!#(B

(defun lookup-agent-require (agent)
  ;; $B%(!<%8%'%s%H$NDj5A%U%!%$%k$r%m!<%I$9$k!#(B
  (require (lookup-agent-class agent)))

(defun lookup-agent-ref (agent key)
  ;; $B%(!<%8%'%s%H$NI8=`@_Dj$r;2>H$9$k!#(B
  (get (lookup-agent-class agent) key))

(defun lookup-agent-command-p (agent command &rest args)
  ;; $B%(!<%8%'%s%H!&%3%^%s%I$r%A%'%C%/$9$k!#(B
  (lookup-agent-ref agent command))

(defun lookup-agent-command (agent command &rest args)
  ;; $B%(!<%8%'%s%H!&%3%^%s%I$r<B9T$9$k!#(B
  (apply (lookup-agent-ref agent command) agent args))

;; Options:
;;
;; $B%(!<%8%'%s%H$O$$$/$D$+$N%*%W%7%g%s$r;}$D$3$H$,=PMh$k!#%*%W%7%g%s$O(B
;; $B%f!<%6$K$h$j@_Dj$5$l$k$,!"%W%m%0%i%`$G$=$N%G%U%)%k%HCM$r@_Dj$9$k$3$H$b(B
;; $B=PMh$k!#@_Dj$K$O<!$N<oN`$,$"$k!#(B
;;
;; $B@_Dj;~%*%W%7%g%s(B - $B%(!<%8%'%s%H$r@_Dj$9$k$H$-$K;XDj$5$l$k%*%W%7%g%s!#(B
;;   $BJQ?t(B `lookup-search-agents' $B$r;2>H!#(B
;; $BJQ?t(B `lookup-agent-options-alist', `lookup-default-agent-options' -
;;   $B%f!<%6;XDj$N%*%W%7%g%s@_Dj!#(B
;; $B%G%U%)%k%H(B - $B%W%m%0%i%`$K$h$k@_Dj!#(B`lookup-agent-set-default' $B$r;2>H!#(B
;; $BI8=`@_Dj(B - $BDj5A%U%!%$%k$K8GDj$5$l$?@_Dj!#(B

(defun lookup-agent-set-default (agent key value)
  ;; $B%(!<%8%'%s%H$N%G%U%)%k%H$N@_Dj$r9T$J$&!#$3$l$O%f!<%6$K$h$k%*%W%7%g%s(B
  ;; $B;XDj$,$J$$>l9g$KMxMQ$5$l$k!#(B
  (let ((defaults (plist-put (lookup-agent-defaults agent) key value)))
    (lookup-agent-set-defaults agent defaults)))

(defun lookup-agent-option (agent key)
  ;; $B%(!<%8%'%s%H$N%*%W%7%g%s$r;2>H$9$k!#@_Dj;~$K;XDj$5$l$?%*%W%7%g%s!"(B
  ;; $BJQ?t(B `lookup-agent-options-alist'$B!"(B`lookup-default-agent-options'$B!"(B
  ;; $B%G%U%)%k%H!"I8=`@_Dj$N=g$KC5:w$9$k!#(B
  (or (plist-get (lookup-agent-options agent) key)
      (lookup-assq-ref (lookup-assoc-ref lookup-agent-options-alist
					 (lookup-agent-id agent)) key)
      (lookup-assq-ref lookup-default-agent-options key)
      (plist-get (lookup-agent-defaults agent) key)
      (lookup-agent-ref agent key)))

;; Initialize:
;;
;; $B8!:w%(!<%8%'%s%H$N=i4|2=$O;M$D$NCJ3,$G9T$J$o$l$k!#(B
;;
;; 1) `lookup-new-agent'
;;    $BJQ?t(B `lookup-search-agents' $B$G@_Dj$5$l$?FbMF$r2r@O$7!"%(!<%8%'%s%H!&(B
;;    $B%*%V%8%'%/%H$r@8@.$9$k!#B3$$$F(B `lookup-agent-init' $B$r8F$S=i4|2=$9$k!#(B
;; 2) `lookup-agent-init'
;;    $B%(!<%8%'%s%H(BID $B$d%?%$%H%k$J$I$r@_Dj$9$k!#<-=q$N%;%C%H%"%C%W$O$^$@!#(B
;; 3) `lookup-agent-setup'
;;    $B%(!<%8%'%s%HKh$N(B setup $B%3%^%s%I$r<B9T$7!"<-=q$N=i4|2=$r9T$J$&!#(B
;; 4) $B3F(B setup $B4X?t(B
;;    $B%(!<%8%'%s%HKh$N%;%C%H%"%C%W$r9T$J$$!"<-=q$N%j%9%H$rJV$9!#(B

(defun lookup-new-agent (spec)
  (let (class location options)
    (setq class (car spec) spec (cdr spec))
    (if (stringp (car spec))
	(setq location (car spec) spec (cdr spec)))
    (setq options spec)
    (lookup-agent-init (lookup-make-agent class location options))))

(defun lookup-agent-init (agent)
  ;; set the agent ID
  (let ((class (symbol-name (lookup-agent-class agent)))
	(location (or (plist-get (lookup-agent-options agent) ':alias)
		      (lookup-agent-location agent))))
    (lookup-agent-set-id agent (if location
				   (concat class "+" location)
				 class)))
  ;; set the agent title
  (lookup-agent-set-title agent (or (lookup-agent-option agent ':title)
				    (lookup-agent-id agent)))
  agent)

(defun lookup-agent-setup (agent)
  (unless (lookup-agent-get-property agent 'setup)
    (let ((lookup-proceeding-message
	   (format "Setting up %s" (lookup-agent-id agent)))
	  dicts enable disable select unselect name)
      (lookup-proceeding-message nil)
      ;; Setup agent by calling the setup function of each agent.
      (lookup-agent-require agent)
      (setq dicts (lookup-agent-command agent 'setup))
      ;; Restrict dictionaries by the options `:enable' and `:disable'.
      (setq enable (lookup-agent-option agent ':enable)
	    disable (lookup-agent-option agent ':disable))
      (when (or enable disable)
	(setq dicts (lookup-grep (lambda (dict)
				   (setq name (lookup-dictionary-name dict))
				   (and (or (not enable) (member name enable))
					(not (member name disable))))
				 dicts)))
      ;; Rearrange the order of dictionaries by the option `:enable'.
      (when enable
	(setq dicts (sort dicts (lambda (d1 d2)
				  (let ((name1 (lookup-dictionary-name d1))
					(name2 (lookup-dictionary-name d2)))
				    (> (length (member name1 enable))
				       (length (member name2 enable))))))))
      ;; Select dictionaries by the options `:select' and `:unselect'.
      (setq select (lookup-agent-option agent ':select)
	    unselect (lookup-agent-option agent ':unselect))
      (lookup-foreach (lambda (dict)
			(setq name (lookup-dictionary-name dict))
			(if (and (or (not select) (member name select))
				 (not (member name unselect)))
			    (lookup-dictionary-set-selected dict t)))
		      dicts)
      ;; Initialize dictionaries.
      (lookup-foreach 'lookup-dictionary-init dicts)
      (lookup-agent-set-dictionaries agent dicts)
      (lookup-proceeding-message t)
      (lookup-agent-put-property agent 'setup t))))

(defun lookup-agent-clear (agent)
  (when (lookup-agent-get-property agent 'setup)
    (let ((lookup-proceeding-message
	   (concat "Clearing " (lookup-agent-id agent))))
      (lookup-proceeding-message nil)
      (lookup-agent-command agent 'clear)
      (lookup-proceeding-message t))))


;;;;;;;;;;;;;;;;;;;;
;: Dictionary
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B<-=q$rI=$o$9%G!<%?%?%$%W!#A4$F$N<-=q$O$3$N%?%$%W$N%*%V%8%'%/%H$H$7$F(B
;; $B@8@.$5$l$k!#(B

;; Structure:
;;
;;   [:dictionary AGENT CODE NAME TITLE PLIST]
;;
;; AGENT - $B<-=q$,B0$9$k8!:w%(!<%8%'%s%H$N%*%V%8%'%/%H!#(B
;; CODE  - $B<-=q$rFCDj$9$k$?$a$NG$0U$N%*%V%8%'%/%H!#(BAGENT $B$H(B CODE $B$NAH$K$h$j(B
;;         $B<-=q$,0l0U$K7hDj$5$l$k!#(B
;; NAME  - $B<-=qL>!#(B
;; TITLE - $B<-=q$N%?%$%H%k!#(B
;; PLIST - $BB0@-%j%9%H$NJ];}!#(B

;; Properties:
;;
;; id       - $B<-=q(BID$B!#(B
;; selected - $B<-=q$,A*Br$5$l$F$$$k>l9g$K(B non-nil$B!#(B
;; defaults - $B<-=q$N%G%U%)%k%H$N@_Dj$rJ];}$9$k!#(B
;; methods, gaiji-table, headings - $B%*%W%7%g%s$NJ];}!#(B

(lookup-defstruct dictionary (agent code name title)
  :with-properties '(id methods headings gaiji-table defaults))

(defun lookup-new-dictionary (agent code name &optional title)
  (let ((dictionary (lookup-make-dictionary agent code name nil)))
    (if title (lookup-dictionary-set-default dictionary ':title title))
    dictionary))

(defsubst lookup-dictionary-selected-p (dictionary)
  (lookup-dictionary-get-property dictionary 'selected))

(defsubst lookup-dictionary-set-selected (dictionary value)
  (lookup-dictionary-put-property dictionary 'selected value))

;; Definitions:
;;
;; $B<-=q$O8!:w$N$?$a$N$$$/$D$+$N%3%^%s%I$r;}$D!#%3%^%s%I$O!"$=$l$,B0$9$k(B
;; $B8!:w%(!<%8%'%s%H$HF1%U%!%$%k$K$FDj5A$5$l$k!#<-=q%3%^%s%I$O%(!<%8%'%s%H!&(B
;; $B%3%^%s%I$HF1MM$K$7$FDj5A$5$l!"8=:_!"<!$N$b$N$,0UL#$r;}$D!#(B
;;
;; search DICTIONARY QUERY
;;   $B<-=q$+$i(B QUERY $B$r8!:w$9$k!#(BQUERY $B$N2r<a$O<-=q$K0lG$$5$l$k!#(B
;;   $B>e5-(B `Query' $B$N@a$r;2>H!#8!:w$N7k2L8+IU$+$C$?(B entry $B$N%j%9%H$rJV$9!#(B
;; content DICTIONARY ENTRY
;;   ENTRY $B$NFbMF$rJ8;zNs$H$7$FJV$9!#FC$K@07A$O9T$J$o$:$K!"<-=q$+$iF@$i$l$k(B
;;   $B$^$^$=$N$^$^$rJV$;$P$h$$!#@07A$OJL$N%W%m%;%9$K$h$C$F9T$J$o$l$k!#4X?t(B
;;   `lookup-insert-content' $B$r;2>H!#(B
;; open DICTIONARY ENTRY
;;   ENTRY $B$rFCJL$J4X?t$G%*!<%W%s$9$k!#Nc$($P(B ENTRY $B$,(B URL $B$N>pJs$rJ];}$7$F(B
;;   $B$$$k$J$i!"%V%i%&%6$G$=$l$r%*!<%W%s$9$k$3$H$,9M$($i$l$k!#(B
;; gaiji DICTIONARY CODE
;;   $B<-=q$,30;z(B($BDL>o$NJ8;z%;%C%H$K4^$^$l$J$$J8;z(B)$B$r;}$D>l9g!"$=$N>pJs$rJV$9!#(B
;;   CODE $B$O30;z$rFCDj$9$k$?$a$NJ8;zNs!#JV5QCM$O!"(Bxbm $B7A<0$N%S%C%H%^%C%W$r(B
;;   $BI=$o$9J8;zNs$H$9$k!#(B
;;
;; $B<-=q$O$$$/$D$+$N8GM-$NI8=`@_Dj$r;}$D!#$3$l$O%(!<%8%'%s%H$NI8=`@_Dj$H(B
;; $BF1MM$KDj5A$5$l!"%f!<%6!&%*%W%7%g%s$K$h$j>e=q$-2DG=$H$J$C$F$$$k!#<!$N(B
;; `Options' $B$N9`$r;2>H!#(B

(defun lookup-dictionary-command-p (dictionary command)
  ;; $B<-=q%3%^%s%I$r%A%'%C%/$9$k!#(B
  (let ((agent (lookup-dictionary-agent dictionary)))
    (lookup-agent-ref agent command)))

(defun lookup-dictionary-command (dictionary command &rest args)
  ;; $B<-=q%3%^%s%I$r<B9T$9$k!#(B
  (let ((agent (lookup-dictionary-agent dictionary)))
    (apply (lookup-agent-ref agent command) dictionary args)))

;; Options:
;;
;; $B<-=q$O$$$/$D$+$N%*%W%7%g%s$r;}$D$3$H$,=PMh$k!#%*%W%7%g%s$O%f!<%6$K$h$j(B
;; $B@_Dj$5$l$k$,!"%W%m%0%i%`$G$=$N%G%U%)%k%HCM$r@_Dj$9$k$3$H$b=PMh$k!#(B
;; $B@_Dj$K$O<!$N<oN`$,$"$k!#(B
;;
;; $BJQ?t(B `lookup-dictionary-options-alist', `lookup-default-dictionary-options'
;;   - $B%f!<%6;XDj$N%*%W%7%g%s@_Dj!#(B
;; $B%G%U%)%k%H(B - $B%W%m%0%i%`$K$h$k@_Dj!#(B`lookup-dictionary-set-default' $B$r;2>H!#(B
;; $B%(!<%8%'%s%H!&%*%W%7%g%s(B - $B<-=q$,B0$9$k%(!<%8%'%s%H$N%*%W%7%g%s$r7Q>5$9$k!#(B

(defun lookup-dictionary-set-default (dictionary key value)
  ;; $B<-=q$N%G%U%)%k%H$N@_Dj$r9T$J$&!#$3$l$O%f!<%6$K$h$k%*%W%7%g%s;XDj$,(B
  ;; $B$J$$>l9g$KMxMQ$5$l$k!#(B
  (let ((defaults (plist-put (lookup-dictionary-defaults dictionary)
			     key value)))
    (lookup-dictionary-set-defaults dictionary defaults)))

(defun lookup-dictionary-option (dictionary key &optional inherit)
  ;; $B<-=q$N%*%W%7%g%s$r;2>H$9$k!#JQ?t(B `lookup-dictionary-options-alist'$B!"(B
  ;; `lookup-default-dictionary-options'$B!"%G%U%)%k%H$N=g$KC5:w$9$k!#(B
  ;; $B%*%W%7%g%s0z?t(B INHERIT $B$,(B non-nil $B$N>l9g!"0z$-B3$$$F%(!<%8%'%s%H!&(B
  ;; $B%*%W%7%g%s$+$i$bC5:w$9$k!%(B
  (or (lookup-assq-ref (lookup-assoc-ref lookup-dictionary-options-alist
					 (lookup-dictionary-id dictionary))
		       key)
      (lookup-assq-ref lookup-default-dictionary-options key)
      (plist-get (lookup-dictionary-defaults dictionary) key)
      (if inherit
	  (lookup-agent-option (lookup-dictionary-agent dictionary) key))))

;; Initialize:
;;
;; $B<-=q$O%(!<%8%'%s%H$N(B :setup $B%3%^%s%I$K$h$j@8@.$5$l!"%(!<%8%'%s%H$HF1;~$K(B
;; $B=i4|2=$5$l$k!#$$$/$D$+$NBeI=E*$JB0@-$,%;%C%H$5$l$k!#(B

(defsubst lookup-dictionary-default-method (dictionary)
  (or (lookup-dictionary-option dictionary ':default-method t)
      lookup-default-method))

(defsubst lookup-dictionary-hiragana (dictionary)
  (lookup-dictionary-option dictionary ':hiragana t))

(defsubst lookup-dictionary-expander (dictionary)
  (lookup-dictionary-option dictionary ':expander t))

(defsubst lookup-dictionary-expand-filter (dictionary)
  (lookup-dictionary-option dictionary ':expand-filter t))

(defsubst lookup-dictionary-stemmer (dictionary)
  (lookup-dictionary-option dictionary ':stemmer t))

(defun lookup-dictionary-init (dictionary)
  (let ((id (concat (lookup-agent-id (lookup-dictionary-agent dictionary))
		    ":" (lookup-dictionary-name dictionary))))
    (lookup-dictionary-set-id dictionary id))
  (let ((title (or (lookup-dictionary-option dictionary ':title)
		   (lookup-dictionary-name dictionary))))
    (lookup-dictionary-set-title dictionary title))
  (if (lookup-dictionary-hiragana dictionary)
      (require 'lookup-kanji))
  (let ((methods (lookup-dictionary-option dictionary ':methods t)))
    (if (or (lookup-dictionary-hiragana dictionary)
	    (lookup-dictionary-expander dictionary))
	(setq methods (append methods '(expansion))))
    (if (lookup-dictionary-stemmer dictionary)
	(setq methods (append methods '(stemming))))
    (lookup-dictionary-set-methods dictionary methods))
  (let ((gaiji-table (or (lookup-dictionary-option dictionary ':gaiji-table)
			 (lookup-make-gaiji-table))))
    (lookup-dictionary-set-gaiji-table dictionary gaiji-table))
  ;; Keep some options as properties to save search step.
  (let ((headings (lookup-dictionary-option dictionary ':headings t)))
    (lookup-dictionary-set-headings dictionary headings))
  dictionary)


;;;;;;;;;;;;;;;;;;;;
;: Entry
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B%(%s%H%j$N>pJs$rI=$o$9%G!<%?%?%$%W!#A4$F$N%(%s%H%j$O$3$N%?%$%W$N(B
;; $B%*%V%8%'%/%H$H$7$F@8@.$5$l$k!#(B

;; Structure:
;;
;;   [:entry DICTIONARY CODE HEADING PLIST]
;;
;; DICTIONARY $B$O!"%(%s%H%j$,B0$9$k<-=q$N%*%V%8%'%/%H!#(B
;; CODE $B$O!"%(%s%H%j$rFCDj$9$k$?$a$NG$0U$N%*%V%8%'%/%H!#(BDICTIONARY $B$H(B CODE
;;   $B$NAH$K$h$C$F%(%s%H%j$O0l0U$K7hDj$5$l$k!#(B
;; HEADING $B$O!"%(%s%H%j$N8+=P$78l!#$3$l$OI=<($N:]$KMQ$$$i$l$k!#(B
;; PLIST $B$O!"B0@-%j%9%H$NJ];}$K;H$o$l$k!#(B

;; Properties:
;; 

(lookup-defstruct entry (dictionary code heading)
  :with-properties '(prefix compound reference jump))

(defun lookup-new-entry (dictionary code heading)
  (let ((entry (lookup-make-entry dictionary code heading)))
    (lookup-arrange-heading entry)))

(defsubst lookup-entry-refered-p (entry)
  (lookup-contents-cache-get entry lookup-enable-format))

;; Functions:

(defun lookup-entry-id (entry)
  (concat (lookup-dictionary-id (lookup-entry-dictionary entry))
	  "/" (lookup-entry-heading entry)))

(defun lookup-copy-entry (entry)
  (lookup-make-entry (lookup-entry-dictionary entry)
		     (lookup-entry-code entry) (lookup-entry-heading entry)))

(defun lookup-entry-compare (e1 e2)
  (and (eq (lookup-entry-dictionary e1) (lookup-entry-dictionary e2))
       (equal (lookup-entry-code e1) (lookup-entry-code e2))))

(defsubst lookup-unique-entries (entries)
  (lookup-nunique entries 'lookup-entry-compare))


;;;;;;;;;;;;;;;;;;;;
;: Reference
;;;;;;;;;;;;;;;;;;;;

(defun lookup-make-reference (dictionary code heading)
  (let ((entry (lookup-make-entry dictionary code heading)))
    (aset entry 0 ':reference)
    entry))

(defun lookup-reference-p (object)
  (and (vectorp object) (eq (aref object 0) ':reference)))

(defsubst lookup-reference-dynamic-p (reference)
  (lookup-entry-get-property reference 'dynamic-search))

(defsubst lookup-reference-make-dynamic (reference function)
  (lookup-entry-put-property reference 'dynamic-search function))

(defun lookup-reference-refer (reference)
  (let ((function (lookup-entry-get-property reference 'dynamic-search)))
    (when function
      (lookup-reference-set-entries reference (funcall function reference))
      (lookup-reference-make-dynamic reference nil)
      (lookup-entry-put-property reference 'refered t))))

(defsubst lookup-reference-refered-p (reference)
  (or (lookup-entry-get-property reference 'refered)
      (unless (lookup-entry-get-property reference 'dynamic-search)
	(let ((entries (lookup-reference-entries reference)))
	  (catch 'result
	    (while entries
	      (unless (lookup-entry-refered-p reference)
		(throw 'result nil))
	      (setq entries (cdr entries)))
	    t)))))

(defsubst lookup-reference-entries (reference)
  (lookup-entry-get-property reference 'linked-entries))

(defsubst lookup-reference-set-entries (reference entries)
  (lookup-entry-put-property reference 'linked-entries entries))

;; link:

(defun lookup-set-link (start end reference)
  ;; $B%P%C%U%!$N(B START $B$+$i(B END $B$^$G$N%j!<%8%g%s$r(B REFERENCE $B$X$N%j%s%/$K$9$k!#(B
  (add-text-properties start end (list 'face 'lookup-reference-face
				       'mouse-face 'highlight
				       'lookup-reference reference)))

(defun lookup-get-link (position)
  ;; $B%P%C%U%!$N(B POSITION $B0LCV$+$i%j%s%/$5$l$F$$$k(B entry $B$rJV$9!#(B
  (get-text-property position 'lookup-reference))

(defun lookup-goto-next-link ()
  ;; $B%P%C%U%!$N<!$N%j%s%/0LCV$K%8%c%s%W$9$k!#$J$1$l$P(B nil $B$rJV$9!#(B
  (let ((p (point)))
    (and (setq p (next-single-property-change p 'lookup-reference))
	 (or (get-text-property p 'lookup-reference)
	     (setq p (next-single-property-change p 'lookup-reference)))
	 (goto-char p))))

(defun lookup-goto-previous-link ()
  ;; $B%P%C%U%!$NA0$N%j%s%/0LCV$K%8%c%s%W$9$k!#$J$1$l$P(B nil $B$rJV$9!#(B
  (let ((p (point)))
    (and (setq p (previous-single-property-change p 'lookup-reference))
	 (or (get-text-property p 'lookup-reference)
	     (setq p (previous-single-property-change p 'lookup-reference)))
	 (goto-char p))))


;;;;;;;;;;;;;;;;;;;;
;: Session
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B8!:w7k2L$rJ];}$9$k$?$a$N%G!<%?%?%$%W!#0l2s$N8!:w$,9T$J$o$l$kEY$K$3$N(B
;; $B%?%$%W$N%G!<%?$,@8@.$5$l!"8!:wMzNr$N;2>HEy$N$?$a$KMQ$$$i$l$k!#(B

;; Structure:
;;
;;   [:session QUERY ENTRIES PLIST]
;;
;; QUERY $B$O!"8!:w%Q%?!<%s$N%*%V%8%'%/%H!#(B`Search Query' $B$r;2>H!#(B
;; ENTRIES $B$O!"8!:w$N7k2L8+IU$+$C$?%(%s%H%j$N%j%9%H!#(B
;; PLIST $B$O!"B0@-%j%9%H$NJ];}$K;H$o$l$k!#(B

;; Properties:
;;
;; excursion - $B%+!<%=%k0LCVEy$N<B9T>uBV$NJ];}!#(B

(lookup-defstruct session (module type)
  :with-properties '(query entries excursion))

(defsubst lookup-session-ref (session key)
  (get (lookup-session-type session) key))

(defun lookup-session-display (session)
  (if lookup-last-session
      (lookup-session-save-excursion lookup-last-session))
  (funcall (lookup-session-ref session 'display) session)
  (setq lookup-current-session session
	lookup-last-session session))

(defun lookup-session-save-excursion (session)
  (let ((func (lookup-session-ref session 'excursion)))
    (when func
      (lookup-session-set-excursion session (funcall func)))))


;;;;;;;;;;;;;;;;;;;;
;: History
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B8!:wMzNr$rJ];}$9$k$?$a$N%G!<%?%?%$%W!#$3$l$OFbIt$K%*%V%8%'%/%H$N%j%9%H(B
;; $B$r;}$A!"$=$l$rDI2C!&;2>H$9$k$3$H$,=PMh$k!#MzNr$O%j%9%H$N0LCV(B(position)
;; $B$b5-21$7$F$*$j!"MzNr$X$N%*%V%8%'%/%H$NDI2C$O!"%j%9%H$N:G8e$K;2>H$5$l$?(B
;; $B%*%V%8%'%/%H$ND>8e$X$H9T$J$o$l$k!#(B($B$D$^$j!"MzNr$rLa$C$?$H$3$m$G%*%V%8%'(B
;; $B%/%H$rDI2C$9$k$H!"0JA0$N7R$,$j$O<:$J$o$l$k(B)

;; Structure:
;;
;;   (STACK . POSITION)
;;
;; STACK $B$O!"MzNr$r@Q$s$G$$$/%*%V%8%'%/%H$N%j%9%H!#(B
;; POSITION $B$O!"8=:_;2>H$5$l$F$$$kMzNr$N0LCV!#(B
;;
;; $B?7$7$$%*%V%8%'%/%H$[$I!"(BSTACK $B$N@hF,$KCV$+$l$k!#MzNr$N0LCV$O(B 1 $B$+$i(B
;; $B;O$^$k!#$?$@$7!"MzNr$X$N%"%/%;%9$O2<5-$N@lMQ4X?t$K$F9T$J$$!"$3$l$i$N(B
;; $BJQ?t$OD>@\A`:n$7$J$$J}$,$h$$!#(B

(defalias 'lookup-make-history 'cons)
(defalias 'lookup-history-stack 'car)
(defalias 'lookup-history-set-stack 'setcar)
(defalias 'lookup-history-position 'cdr)
(defalias 'lookup-history-set-position 'setcdr)

(defsubst lookup-new-history ()
  ;; $B?7$7$$(B history $B%*%V%8%'%/%H$r@8@.$7$FJV$9!#(B
  (lookup-make-history nil 0))

;; Interface:
;;
;; $BMzNr$X$N%*%V%8%'%/%HDI2C$O!"4X?t(B `lookup-history-push' $B$K$h$j9T$J$&!#(B
;; $BDI2C$5$l$?%*%V%8%'%/%H$O!"4X?t(B `lookup-history-ref' $B$K$h$j;2>H$9$k$3$H(B
;; $B$,=PMh$k!#4X?t(B `lookup-history-length' $B$K$h$j!"MzNr$ND9$5$rCN$k$3$H$,(B
;; $B=PMh$k!#(B

(defun lookup-history-push (history object)
  ;; $B8=:_$N;2>H0LCV$K%*%V%8%'%/%H$rDI2C$9$k!#$b$7$=$N0LCV$KF1$8%*%V%8%'%/%H(B
  ;; $B$,4{$KDI2C$5$l$F$$$l$P2?$b$7$J$$!#(B
  (let ((stack (lookup-history-stack history))
	(position (lookup-history-position history)))
    (setq stack (nthcdr (- (length stack) position) stack))
    (unless (eq object (car stack))
      (lookup-history-set-stack history (cons object stack))
      (lookup-history-set-position history (1+ position)))))

(defun lookup-history-ref (history &optional n do-not-move)
  ;; $B8=:_$N;2>H0LCV$+$i(B N $BHVL\$K$"$k%*%V%8%'%/%H$rJV$9!#(B
  ;; $BIi$N?t$,;XDj$5$l$?>l9g$K$O!"MzNr$r2a5n$KLa$k!#@5$N?t$@$H!"$=$N5U!#(B
  ;; $B>JN,$5$l$?>l9g$K$O8=:_$N;2>H0LCV$N$b$N$rJV$9!#(B
  ;; $BMzNr$,$J$$>l9g$K$O(B `no-object'$B!":G=i$N0LCV$+$iA0$KLa$m$&$H$7$?$i(B
  ;; `first'$B!":G8e$+$i@h$K?J$b$&$H$7$?$i(B `last' $B$rJV$9!#(B
  ;; DO-NOT-MOVE $B$K(B non-nil $B$r;XDj$7$?>l9g!";2>H8e$N0\F0$r9T$J$o$J$$!#(B
  (let* ((stack (lookup-history-stack history))
	 (position (lookup-history-position history))
	 (length (length stack)))
    (setq n (or n 0))
    (cond
     ((eq length 0) 'no-object)
     ((and (= position 1) (< n 0)) 'first)
     ((and (= position length) (> n 0)) 'last)
     (t
      (setq position (+ position n)
	    position (if (< position 1) 1
		       (if (> position length) length position)))
      (unless do-not-move
	(lookup-history-set-position history position))
      (car (nthcdr (- length position) stack))))))

(defun lookup-history-length (history)
  ;; $BMzNr$ND9$5$rJV$9!#(B
  (length (lookup-history-stack history)))

(defun lookup-history-clear (history)
  ;; $BMzNr$r%/%j%"$9$k!#(B
  (lookup-history-set-stack history nil)
  (lookup-history-set-position history 0))


;;;;;;;;;;;;;;;;;;;;
;: Gaiji
;;;;;;;;;;;;;;;;;;;;

;; Description:
;;
;; $B30;z$r07$&$?$a$N4X?t72!#$3$l$K$O<!$N;0$D$N%G!<%?%?%$%W$,$"$k!#(B
;;
;; $B30;z(B         - $B30;z>pJs$rJ];}$9$k$?$a$N%*%V%8%'%/%H(B
;; $B30;z%0%j%U(B   - $B30;z$=$N$b$N$G$"$k%*%V%8%'%/%H(B
;; $B30;z%F!<%V%k(B - $B30;z$r5-O?!&;2>H$9$k$?$a$N%*%V%8%'%/%H(B
;;
;; $B30;z$N07$$$O!"(BEmacs $B$N<oN`$d4D6-$K$h$C$F0[$J$k!#(B

(cond
 ((featurep 'xemacs)
  (defun lookup-glyph-compose (xbm)
    (let (width height data)
      (with-temp-buffer
	(insert xbm)
	(goto-char (point-min))
	(if (re-search-forward "width[ \t]+\\([0-9]+\\)")
	    (setq width (string-to-int (match-string 1))))
	(if (re-search-forward "height[ \t]+\\([0-9]+\\)")
	    (setq height (string-to-int (match-string 1))))
	(while (re-search-forward "0x\\(..\\)" nil t)
	  (setq data (cons (string-to-int (match-string 1) 16) data)))
	(setq data (concat (nreverse data))))
      (make-glyph (vector 'xbm :data (list width height data)))))

  (defun lookup-glyph-paste (start end glyph)
    (set-extent-property (extent-at start nil 'lookup-gaiji) 'invisible t)
    (let (extent extents)
      (while (setq extent (extent-at start nil nil extent 'at))
	(if (eq (extent-start-position extent) (extent-end-position extent))
	    (setq extents (cons extent extents))))
      (while extents
	(set-extent-endpoints (car extents) end end)
	(setq extents (cdr extents)))
      (set-extent-begin-glyph (make-extent end end) glyph)))
  )
 ((featurep 'mule)
  (defun lookup-bitmap-compose (xbm)
    (require 'bitmap)
    (with-temp-buffer
      (insert xbm)
      (let ((cmp (bitmap-decode-xbm
		  (bitmap-read-xbm-buffer (current-buffer)))))
	(bitmap-compose (aref cmp 0)))))

  (defun lookup-bitmap-paste (start end glyph)
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'after-string glyph)))
  ))

;; Gaiji:
;;
;; $B30;z%*%V%8%'%/%H$O<!$N9=B$$r;}$D!#(B
;;
;;   (GLYPH . ALTERNATE)
;;
;; GLYPH $B$O!"2<5-$N30;z%0%j%U!#(B
;; ALTERNATE $B$O!"30;z$KBP1~$9$kBeBXJ8;zNs!#(B
;;
;; GLYPH $B$O!"30;z$NI=<($N:]$KMQ$$$i$l$k!#$7$+$730;z$O!"FI$`$N$K$OE,$7$F$$$k$,(B
;; $B8!:w$d%3%T!<$K$O8~$$$F$$$J$$!#$=$N$?$a!"$=$l$i$r9T$J$&:]$K$O(B ALTERNATE $B$,(B
;; $BBe$o$j$KMxMQ$5$l$k!#(B

(defalias 'lookup-make-gaiji 'cons)
(defalias 'lookup-gaiji-glyph 'car)
(defalias 'lookup-gaiji-set-glyph 'setcar)
(defalias 'lookup-gaiji-alternate 'cdr)
(defalias 'lookup-gaiji-set-alternate 'setcdr)

(defun lookup-gaiji-insert (gaiji)
  (let ((glyph (lookup-gaiji-glyph gaiji))
	(alter (lookup-gaiji-alternate gaiji))
	(start (point)))
    (insert alter)
    (if (not (eq glyph alter))
	(put-text-property start (point) 'lookup-gaiji glyph))))

;; Gaiji Glyph:
;;
;; $B30;z%0%j%U$O!"30;z$N30;z$?$k$f$($s!"G$0U$N%S%C%H%^%C%W$r<($9%*%V%8%'%/%H!#(B
;; $B$3$l$O(B GNU Emacs $B$H(B XEmacs $B$H$G07$$$,0[$J$k!#(B
;;
;; 1) GNU Emacs / Mule $B$N>l9g(B
;;
;; $B30;z$O(B bitmap-mule $B%Q%C%1!<%8$rMxMQ$7$F@8@.$5$l$k!#FbItE*$K$O$3$l$O!"(B
;; `compose-string' $B$K$h$j@8@.$5$l$kJ8;zNs$G$"$k!#(B
;;
;; 2) XEmacs $B$N>l9g(B
;;
;; $BAH$_9~$_$N(B glyph $B$N5!G=$rMxMQ$7$F@8@.$9$k!#$D$^$j!"30;z$r0l<o$N2hA|(B
;; $B%G!<%?$H$7$F07$&!#(B
;;
;; $B30;z$r@8@.$9$k4X?t$O!"JQ?t(B `lookup-gaiji-compose-function' $B$K$h$j;XDj(B
;; $B$5$l$k!#$3$l$O(B bitmap-mule $B$rMxMQ$9$k>l9g$K$O(B `lookup-bitmap-compose'
;; $B$H$J$j!"(BXEmacs $B$N>l9g$K$O(B `lookup-glyph-compose' $B$H$J$k!#$3$l$i$N4X?t$O(B
;; "lookup-gaiji.el" $B$GDj5A$7$F$$$k!#(B
;;
;; $B$^$?!"F1MM$K30;z$rA^F~$9$k4X?t$O!"JQ?t(B `lookup-gaiji-insert-function'
;; $B$K$h$jM?$($i$l$k!#$3$l$O$=$l$>$l!"(B`insert' $B5Z$S(B `lookup-glyph-insert'
;; $B$H$J$k!#(B

(defun lookup-gaiji-glyph-possible-p ()
  ;; $B30;z%0%j%U$rI=<(2DG=$J>uBV$K$"$k$+$I$&$+%A%'%C%/$9$k!#(B
  (and window-system lookup-gaiji-compose-function))

(defun lookup-gaiji-glyph-compose (xbm)
  ;; $B30;z%0%j%U$r@8@.$9$k!#%G!<%?$O(B XBM $B7A<0$NJ8;zNs$H$7$FM?$($k!#(B
  (funcall lookup-gaiji-compose-function xbm))

(defun lookup-gaiji-glyph-paste (start end glyph)
  ;; $B30;z%0%j%U$rE=$jIU$1$k!#(B
  (funcall lookup-gaiji-paste-function start end glyph))

;; Gaiji Table:
;;
;; $B30;z%F!<%V%k$O!"30;z%*%V%8%'%/%H$r5-O?!&;2>H$9$k$?$a$N%*%V%8%'%/%H!#(B
;; $B30;z%*%V%8%'%/%H$O?t$,B?$$>e$KIQHK$K;2>H$5$l$k$?$a!"%O%C%7%e(B(obarray)$B$r(B
;; $BMQ$$$F9bB.$K;2>H=PMh$k$h$&$K$7$F$$$k!#(B

(defsubst lookup-make-gaiji-table ()
  ;; $B30;z%F!<%V%k$r@8@.$9$k!#(B
  (make-vector 377 0))

(defun lookup-gaiji-table-set (table code gaiji)
  ;; $B30;z%F!<%V%k$K30;z%*%V%8%'%/%H$r%;%C%H$9$k!#(B
  (set (intern code table) gaiji))

(defun lookup-gaiji-table-ref (table code)
  ;; $B30;z%F!<%V%k$+$i30;z%*%V%8%'%/%H$r;2>H$9$k!#(B
  (let ((symbol (intern code table)))
    (if (boundp symbol) (symbol-value symbol))))

;; Table Format:
;;
;; $B4X?t(B `lookup-new-gaiji-table' $B$K$h$j!"30;z%F!<%V%k$N=i4|CM$rDj5A$9$k$3$H$,(B
;; $B=PMh$k!#$3$l$K$h$j!"30;z$rFCDj$NJ8;zNs$GCV$-49$($k$h$&$J$3$H$,2DG=$H$J$k!#(B
;; $B$3$l$O<!$N$h$&$K$7$F9T$J$&!#(B
;;
;;   (lookup-new-gaiji-table
;;    '((CODE1 GLYPH1 [ALTERNATE1]) (CODE2 GLYPH2 [ALTERNATE2]) ...))
;;
;; CODE $B$O<-=q$K$*$1$k30;z%3!<%I!#(BGLYPH $B$O30;z%0%j%U%*%V%8%'%/%H$+G$0U$N(B
;; $BJ8;zNs!"$"$k$$$O$=$N$I$A$i$+$rJV$9I>2A2DG=$J<0!#(BALTERNATE $B$O>JN,2DG=$G!"(B
;; $B30;z$NBeBXJ8;zNs$r;XDj$9$k!#(B
;;
;; $BF1$8J8;zNs$r@_Dj$9$k>l9g$K$b!"(BGLYPH $B$H(B ALTERNATE $B$H$G$O0UL#$,0[$J$k!#(B
;; GLYPH $B$O2hLL=PNO$KMQ$$$i$l$k$?$a!"%"%/%;%s%HJ8;z$J$I$r4^$`G$0U$NJ8;z(B
;; $BNs$r;XDj=PMh$k$,!"(BALTERNATE $B$O8!:wEy$KMQ$$$i$l$k$?$a!"0lHLE*$J(B ASCII 
;; $B%3!<%I$J$I$r;XDj$7$?J}$,$h$$!#(B

(defun lookup-new-gaiji-table (spec)
  ;; SPEC $B$N;XDj$K=>$C$F30;z%F!<%V%k$r:n@.$9$k!#(B
  (let ((table (lookup-make-gaiji-table)) form glyph alter)
    (while spec
      (setq form (car spec) glyph (cadr form) alter (or (nth 2 form) glyph))
      (if (and (featurep 'xemacs) glyph)
	  (setq glyph (make-glyph (vector 'string ':data glyph))))
      (lookup-gaiji-table-set table (car form) (lookup-make-gaiji glyph alter))
      (setq spec (cdr spec)))
    table))

(provide 'lookup-types)

;;; lookup-types.el ends here
