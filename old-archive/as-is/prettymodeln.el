;;;Date: 13 Sep 87 18:34:59 GMT
;;;From: Silver <topaz.rutgers.edu!gaynor@RUTGERS.EDU>
;;;Organization: Rutgers Univ., New Brunswick, N.J.
;;;Subject: emacs convenience

;;;		     Pretty Mode Line Formatting

;;;Long file names (i.e. /u2/luser/foobar/bletch/src/0.1/global.h) in the
;;;mode line are a pain in the ass.  They suck up the whole mode line,
;;;and are a strain on the eyes to differentiate.  To avoid this problem,
;;;you can just display the buffer name.  But that's a little too
;;;uninformative for me.

;;;You will find here a mode-line-format scheme that is fairly nice.  It
;;;displays the buffer name if the buffer is not associated with a file.
;;;Otherwise, it displays the file name, but only after abbreviating it
;;;as per a list of abbreviations you provide.

;;;The abbreviations are emacs regular expressions.  They are replaced as
;;;per replace-regexp.  For more info, consult searches and regexps in
;;;*info* (i.e. "C-hi").

;;;Here's how it looks in my .emacs.  Comments on installation are
;;;preceded by ;;;;;;;;;;.  It's not hard, really.


;; Set up mode-line, by making mode-line-buffer-identification local
;; to every buffer.  A find-file-hook abbreviates the buffer-file-name
;; to something a little easier to read.
;;
;;   file name, originally = buffer-file-name
;;   abbreviations = file-name-abbreviation-alist
;;   means of abbreviation = string-replace-regexp-alist
;;   find-file-hook = abbreviate-mode-line-buffer-identification


;; Customize the mode-line-format and it's constituents.  Remember,
;; mode-line-buffer-identification MUST be used to identify the
;; buffer.  mode-line-modified is retained because it is in emacs'
;; default default-mode-line-format, and emacs may do some clever
;; tricks with it.
;;;;;;;;;; Make sure you use mode-line-buffer-identification to
;;;;;;;;;; identify the buffer in your mode-line-format.  This
;;;;;;;;;; variable must be buffer-local.
(make-variable-buffer-local 'mode-line-buffer-identification)
(setq-default mode-line-buffer-identification '("%b"))
(setq-default mode-line-modified '("--%*%*--"))
(setq-default default-mode-line-format
  '("----Emacs: " ;; Always want a string first...
    mode-line-buffer-identification
    mode-line-modified
    "%[("
    mode-name
    minor-mode-alist
    "%n"
    mode-line-process
    ")%]----%p%-"))


;; (1) Replace imbedded "u2/gaynor" or leading "/u2/gaynor" with "~".
(defvar file-name-abbreviation-alist
  '(("\\(^/\\|\\)u2/gaynor" . "~"))
"Alist of embedded filename patterns vs corresponding abbreviations.
Each element is of the form (<regexp> . <to-string>) (see
replace-regexp).")

;;;;;;;;;; Let's say that I often played with the files in
;;;;;;;;;; /u2/luser/foobar/bletch.  Then I might want to replace
;;;;;;;;;; leading instances of this path with "bletch" by including
;;;;;;;;;; the association ("^/u2/luser/foobar/bletch" .  "bletch").
;;;;;;;;;;
;;;;;;;;;; Let's say that I wanted to display only the last directory
;;;;;;;;;; in the path.  In this case, I would add the association
;;;;;;;;;; ("^.*/\\([^/]*/\\)" . "\\1").


(defun string-replace-regexp-alist (s al)
"Given a string s, replace instances of regexps (cars of elements in
al) with their associated to-string (cdrs of elements in al) as per
replace-regexp."

  (save-excursion
    (let (tal ss)

      ;; Dang, have to use a temporary buffer to pull this one off...
      (set-buffer (get-buffer-create "!@#$%^&*"))
      (insert s)

      ;; Walk down al with tal, regexp-replacing instances of
      ;; (car (car tal)) with (cdr (car tal)).
      (setq tal al)
      (while tal
	(goto-char (point-min))
	(while (re-search-forward (car (car tal)) nil t)
	  (replace-match (cdr (car tal))))
	(setq tal (cdr tal)))
      (setq ss (buffer-string))
      (kill-buffer "!@#$%^&*")
      ss)))


(defun abbreviate-mode-line-buffer-identification ()
"Abbreviates mode-line-buffer-identification locally, as per
string-replace-regexp-alist and file-name-abbreviation-alist."
  (setq mode-line-buffer-identification
    (list (string-replace-regexp-alist buffer-file-name
				       file-name-abbreviation-alist))))


;;;;;;;;;; Add abbreviate-mode-line-buffer-identification to
;;;;;;;;;; find-file-hooks.
(setq find-file-hooks '(abbreviate-mode-line-buffer-identification))

;;;Enjoy,
;;;Silver.

;;;Andy Gaynor   201-545-0458   81 Hassart St, New Brunswick, NJ 08901
;;;  gaynor@topaz.rutgers.edu   ...!rutgers!topaz.rutgers.edu!gaynor
