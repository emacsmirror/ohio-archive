;;; mew-guess.el --- Guess header and template file in draft for Mew

;; Author:  OBATA Noboru <obata@nippon-control-system.co.jp>
;; Created: Mar 22, 1999
;; Revised: Aug 31, 1999

;;; Commentary:

;; Shun-ichi GOTO <gotoh@taiyo.co.jp> $B$5$s$K46<U$7$^$9!#(B

;; $B$3$N%Q%C%1!<%8$O!"%I%i%U%H%b!<%I$K$*$$$F!"4{B8$N%X%C%@$NFbMF$+$i!"B>(B
;; $B$N%X%C%@$NFbMF$r?dB,$7$F=q$-49$($k5!G=$rDs6!$7$^$9!#Nc$($P!"(BTo: $B%X%C(B
;; $B%@$+$i(B From: $B$rJQ99$7$?$j!"(BConfig: $B$rA^F~$9$k$3$H$,$G$-$^$9!#(B
;;
;; $B$^$?!"$b$&$R$H$D$N5!G=$H$7$F!"J8F,$K%F%-%9%H%U%!%$%k(B ($B%F%s%W%l!<%H(B) 
;; $B$rA^F~$9$k$3$H$,$G$-$^$9!#A^F~$9$k%U%!%$%kL>$b!"%X%C%@$+$i?dB,$5$;$k(B
;; $B$3$H$,$G$-$^$9!#(B
;;
;; $B%$%s%9%H!<%k$NJ}K!!#(B
;;
;;  - $B$3$N%U%!%$%k$r(B emacs $B$,8+IU$1$i$l$k>l=j$KCV$-$^$9!#(B
;;
;;  - .emacs $B$K<!$N5-=R$rDI2C$7$^$9!#(B(define-key ...) $B$O%-!<%P%$%s%I$NNc(B
;;  $B$G$9!#$*9%$_$K9g$o$;$FJQ$($F2<$5$$!#(B
;;
;;    (add-hook 'mew-init-hook
;;      (lambda ()
;;        (require 'mew-guess)
;;        (define-key mew-draft-header-map "\C-c\C-v" 'mew-guess-template)
;;        (define-key mew-draft-header-map "\C-c\C-d" 'mew-guess-header)
;;        (define-key mew-draft-body-map "\C-c\C-v" 'mew-guess-template)
;;        (define-key mew-draft-body-map "\C-c\C-d" 'mew-guess-header)))
;;
;; $B@_Dj$NNc!#(B
;;
;;  - $B%X%C%@?dB,$NNc!#(B
;;
;;    (setq mew-guess-header-alist
;;          '(
;;            ("From:"
;;             ;; From: $B$N?dB,$N%k!<%k(B
;;             ("To:"                   
;;              ;; To: $B$NFbMF$,!"(B"sorry@" $B$K%^%C%A$7$?$i!"(BFrom: $B$NFbMF$r(B 
;;              ;; "$B>.H((B $B>:(B <obata@nippon-control-system.co.jp>" $B$K=q$-(B
;;              ;; $B49$($^$9!#(B
;;              ("sorry@" "$B>.H((B $B>:(B <obata@nippon-control-system.co.jp>")
;;              ;; $BF1MM$K!"(BTo: $B$NFbMF$,(B "@linux\\.or\\.jp" $B$K%^%C%A$7$?(B
;;              ;; $B$i!"(BFrom: $B$r(B "OBATA Noboru <obata@hh.iij4u.or.jp>" $B$K(B
;;              ;; $B=q$-49$($^$9!#(B
;;              ("@linux\\.or\\.jp" "OBATA Noboru <obata@hh.iij4u.or.jp>")
;;              )
;;             ("Config:"
;;              ;; Config: $B$NFbMF$K$h$C$F=q$-49$($?$$>l9g!#(B
;;              ("office" "OBATA Noboru <obata@nippon-control-system.co.jp>"))
;;             ;; $B<!$NFCJL$J5-K!$K$h$C$F!"%G%U%)%k%HCM$r;XDj$7$^$9!#(B
;;             (t "OBATA Noboru <obata@nippon-control-system.co.jp>"))
;;            ))
;;
;;  $BCV498e$NJ8;zNs$H$7$F!"CM$H$7$FJ8;zNs$r;}$DJQ?tL>$d!"J8;zNs$rJV$94X?t(B
;;  $BL>$d%i%`%@<0$r5-=R$G$-$^$9!#Nc$($P!"<!$N$h$&$K=q$1$P(B Config: $B$K(B
;;  mew-config-imget $B$NCM$r@_Dj$G$-$^$9!#(B
;;
;;    (setq mew-guess-header-alist
;;          '(
;;            ("Config:"
;;             (t mew-config-imget)
;;             )
;;            ))
;;
;;  - $B%F%s%W%l!<%H?dB,$NNc!#(B
;;
;;    (setq mew-guess-template-alist
;;          '(("To:"
;;             ;; To: $B$NFbMF$,(B "foo" $B$K%^%C%A$7$?$i!"J8F,$K%U%!%$%k(B 
;;             ;; "~/.ff-foo" $B$rA^F~$7$^$9!#(B
;;             ("foo" "~/.ff-foo")
;;             ;; $BF1MM!#(B
;;             ("bar" "~/.ff-bar")
;;             )
;;            ))
;;
;;  - $B%-!<%o!<%ICV49$NNc!#>e$NJ}K!$G$O!"?dB,$N%k!<%k$@$1%U%!%$%k$r:n$i$J(B
;;  $B$/$F$O$$$1$J$$$N$G!"LLE]$G$9!#%F%s%W%l!<%H%U%!%$%k$K(B |>keyword<| $B$N(B
;;  $B=q<0$G%-!<%o!<%I$rKd$a9~$_!"$=$N%-!<%o!<%I$NCV49$r;XDj$9$k$3$H$,$G$-(B
;;  $B$^$9!#(B
;;
;;    (setq mew-guess-template-alist
;;          '(("To:"
;;             ;; To: $B$NFbMF$,(B "foo" $B$K%^%C%A$7$?$i!"J8F,$K%U%!%$%k(B 
;;             ;; "~/.ff-foo" $B$rA^F~$7$^$9!#$=$N$H$-!"%U%!%$%kFb$N%-!<%o!<(B
;;             ;; $B%I(B |>me<| $B$r!"(B"$B$U!<(B" ($B%@%V%k%/%)!<%F!<%7%g%s$O$J$7(B) $B$K(B
;;             ;; $BCV$-49$($^$9!#(B
;;             ("foo" "~/.ff-foo" ("me" . "$B$U!<(B"))
;;             ;; $BF1MM!#(B
;;             ("bar" "~/.ff-foo" ("me" . "$B$P!<(B"))
;;             )
;;            ;; $B%G%U%)%k%H(B
;;            (t "~/.ff-foo")
;;            ))
;;
;;  $B$=$N:]!"%-!<%o!<%ICV49$N4{DjCM$r!"<!$N$h$&$K5-=R$9$k$3$H$,$G$-$^$9!#(B
;;  $BCV498e$NJ8;zNs$H$7$F!"JQ?tL>!"4X?tL>!"%i%`%@<0$r5-=R$G$-$^$9!#(B
;;
;;    (setq mew-draft-replace-alist
;;          '(("me" . "$B>.H((B")
;;            ("email" . mew-mail-address)
;;            ("time" . (lambda () (current-time-string)))
;;            ;; ("time" . current-time-string) $B$b(B OK
;;            ))

;;; Code:


(defvar mew-guess-query-when-replaced nil
  "*If non-nil, make query to accept result of replacement.")

;; Guess

(defun mew-guess-by-alist (alist)
  (let (name header sublist key val ent ret)
    (while (and alist (not ret))
      (setq name (car (car alist)))
      (setq sublist (cdr (car alist)))
      (cond
       ((eq name t)
	(setq ret sublist))
       ;;((eq name nil)
       ;;(setq ret sublist))
       (t
	(setq header (mew-header-get-value name))
	(if header
	    (while (and sublist (not ret))
	      (setq key (car (car sublist)))
	      (setq val (cdr (car sublist)))
	      (if (and (stringp key) (string-match key header))
		  (cond
		   ((stringp (car val))
		    (setq ent
                          (mew-refile-guess-by-alist2 key header (car val))))
		   ((or (functionp (car val))
			(symbolp (car val)))
		    (setq ent (car val)))
		   ((listp (car val))
		    (setq ent (mew-guess-by-alist val)))))
              (if ent (setq ret val))
              (setq sublist (cdr sublist))))))
      (setq alist (cdr alist)))
    ret))

;; Header

(defvar mew-guess-header-alist nil
  "*Alist to guess header contents.
The syntax is:

    (HEADER-GUS (HEADER-CND (KEY VALUE)... )... )...

HEADER-GUS is the target header which you want to guess and modify.

HEADER-CND and KEY specify the condition to guess. If regexp KEY matches
to contents of HEADER-CND, contents are replaced with string VALUE.")

(defun mew-guess-header ()
  "Guess and modify header according to \"mew-guess-header-alist\"."
  (interactive)
  (let ((alist mew-guess-header-alist)
	header-gus sublist header-cnd glist undo changed)
    (save-excursion
      (mew-header-goto-end)
      (setq undo (buffer-substring 1 (point)))
      (while alist
	(setq header-gus (car (car alist)))
	(setq glist (mew-guess-by-alist (cdr (car alist))))
	(if glist
	    (mew-header-replace-value header-gus (car glist)))
	(setq alist (cdr alist)))
      ;; compare with original
      (mew-header-goto-end)
      (setq changed (not (string= undo (buffer-substring 1 (point)))))
      (if (not changed)
	  (if (interactive-p)
	      (message "Nothing changed")) ; nothing done
	;; something changed! query if need
	(mew-highlight-header)
        (mew-draft-header-keymap)
	(if (or (not mew-guess-query-when-replaced)
		(y-or-n-p "Headers are changed. Accept this? "))
	    (message "Some headers are changed") ; accepted
	  ;; restore original
	  (kill-region 1 (point))
	  (insert undo)
	  (mew-header-goto-end)
	  (mew-highlight-header)
	  (mew-draft-header-keymap)
	  (message "Changes are canceled"))))))


(defun mew-header-replace-value (field value)
  "Replace header contents."
  (interactive)
  (let ((newvalue (cond
                   ((stringp value) value)
                   ((symbolp value)
		    (cond 
		     ((eq value 'delete) nil) ; delete this line
		     ((fboundp value) 
		      (funcall value))	; use function result
		     ((and (boundp value)
			   (stringp (symbol-value value)))
		      (symbol-value value)) ; use value of variable
		     (t nil)))
                   ((functionp value) (funcall value))
                   (t nil)))
	orgvalue)
    (if (not (and (stringp field)
		  (or (null newvalue) (stringp newvalue))))
	(error "Invalid field pair in mew-header-replace-alist")
      (setq orgvalue (mew-header-get-value field))
      (if (and orgvalue
	       newvalue
	       (string= (downcase newvalue) (downcase orgvalue)))
	  ()				; same ... don't replace
	(if orgvalue
	    (mew-header-delete-lines (list field))
	  (mew-header-goto-end))
	(if newvalue
	    (insert field " " newvalue "\n"))))))

;; Template

(defvar mew-guess-template-alist nil
  "*Alist to guess template file.
The basic syntax is:

    (HEADER (KEY TEMPLATE)... )...

If regexp KEY matches to contents of HEADER, file TEMPLATE is guessed
and guess is finished. Note that there is no dot (.) between KEY and
TEMPLATE.

You can specify alists for keyword replacement like:

    (HEADER (KEY TEMPLATE (REPLACE-FROM . REPLACE-TO)... )... )...

Alists in this form take precedence over \"mew-draft-replace-alist\".

For example:

    (setq mew-guess-template-alist
          '((\"To:\"
             (\"mew-dist@mew.org\" \"~/.ff-mew-dist\"
              (\"hello\" . \"Mew friends,\")))
             (\"foo@hoge.hoge\" \"~/.ff-other\")
	    (t \"~/.ff-default\")
            ))

There is exceptional form as you can see in the example:

    (t TEMPLATE [(REPLACE-FROM . REPLACE-TO)...])

You can specify default TEMPLATE in this form, putting it on the last.")

(defun mew-guess-template ()
  "Insert template file on the top of the draft message."
  (interactive)
  (let* ((glist (mew-guess-by-alist mew-guess-template-alist))
         (file (car glist)) (kwlist (cdr glist)) deleted efile)
    (if file
        (progn
          (setq efile (expand-file-name file))
          (if (not (file-exists-p efile))
              (message "No template file %s" efile)
            (progn
              (forward-char
               (mew-draft-insert-file-and-replace
                efile 'top (list kwlist mew-draft-replace-alist)))))))))

;; Misc

(defvar mew-draft-replace-alist nil
  "*Alist for keyword replacement in draft.
Keywords \"|>keyword<|\" in the template file and signature file (not
yet) are replaced with it's associated value. It is possible to specify
string, function, variable and lambda expression as the associated
value, which is evaluated when replacement occurs.

You can replace keyword to the string flushed on the right. For example:

    (setq mew-draft-replace-alist
          '((\"name\" . \"HOGE Hoge\")
            (\"email\" . \"foo@hoge.hoge\")
            (\"time\" .
             (lambda () (format (format \"%%%ds\" fill-column)
                                (current-time-string))))
            ))")

(defun mew-draft-insert-file-and-replace (file pos &optional alists)
  "Insert file and replace keyword.
Insert file FILE on position POS (possible values are top, bottom and
here), and replace keyword according to ALISTS (list of alist)."
  (interactive)
  (cond
   ((eq pos 'top)
    (goto-char (mew-header-end))
    (forward-line))
   ((eq pos 'bottom)
    (if (null (mew-attach-p))
        (goto-char (point-max))
      (goto-char (mew-attach-begin))
      (forward-line -1)
      (end-of-line)
      (insert "\n"))))
  (let (bytes)
    (save-restriction
      (narrow-to-region
       (point) (+ (point) (car (cdr (insert-file-contents file)))))
      (while alists
        (mew-draft-replace-by-alist (car alists))
        (setq alists (cdr alists)))
      (mew-fib-delete-frame)
      (setq bytes (- (point-max) (point-min))))))

(defun mew-draft-replace-by-alist (alist)
  "Fill |>item<| by alist."
  (interactive)
  (save-excursion
    (let (begin end str)
      (goto-char (point-min))
      (while (re-search-forward "|>\\([^<]+\\)<|" nil t)
	(setq begin (match-beginning 1)
	      end (match-end 1)
	      str (buffer-substring begin end))
	(delete-region begin end)
	(backward-char 2)
	(insert (let ((obj (cdr (assoc (downcase str) alist))))
                  (cond
		   ((null obj) str)
		   ((stringp obj) obj)
		   ((functionp obj) (funcall obj))
		   ((symbolp obj)
		    (if (fboundp obj) (funcall obj)
		      (if (and (boundp obj)
			       (stringp (symbol-value obj)))
			  (symbol-value obj))))
                   (t str))))))))

(provide 'mew-guess)

;;; mew-guess.el ends here
