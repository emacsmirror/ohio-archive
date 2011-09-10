;;;Date: 26 Sep 88 13:58:24 GMT
;;;From: John Sturdy <mcvax!ukc!eagle.ukc.ac.uk!icdoc!qmc-cs!harlqn!jcgs@uunet.uu.net>
;;;Subject: inserting shell command names
;;;Organization: Harlequin (Cambridge, UK) Ltd.

;;;Someone has asked for the code, despite the warning about its slowness,
;;;so here it is. Suggestion from jr: save the data it finds in a file
;;;between emacs sessions.
;;; shell-command.el
;;; Last edited: Fri Sep 23 16:23:24 1988 by jcgs (John Sturdy) on harlqn

(defvar unix-command-path-string (getenv "PATH")
  "String containing the Unix shell command search path.")

(defun rec-pick-path-parts (string start)
  "Pick out directory names in STRING starting at START, returning them
as a list."
  (let ((colon-place
         (string-match "\\:" string start)))
    (if colon-place
         (cons (substring string start colon-place)
         (rec-pick-path-parts string (1+ colon-place)))
      (cons
        (substring string start nil)
       nil))))

(defvar unix-command-path (rec-pick-path-parts unix-command-path-string 0)
  "List of directory names in unix command path.")

(defun file-executable-p (filename)
  "Return whether the file named is executable as a shell command by the user."
  (let ((attribute-string (nth 8 (file-attributes filename))))
    (and (string= (substring attribute-string 3 4) "x")
         (not
          (string= (substring attribute-string 0 1) "d")))))

(defun list-executables-in-file-list (file-list already-there)
  "Return a list of executable files from amongst FILE-LIST, tacked onto
the front of ALREADY-THERE."
  (if file-list
      (if (file-executable-p (car file-list))
          (cons (cons (file-name-nondirectory (car file-list)) nil)
                (list-executables-in-file-list (cdr file-list)
                                               already-there))
        (list-executables-in-file-list (cdr file-list)
                                       already-there))
    already-there))

(defun list-executables-on-path (path)
  "Return a list of executable files (not including directory names) in
directories on PATH."
  (if path
      (list-executables-in-file-list
       (directory-files (car path) t)
       (list-executables-on-path (cdr path)))
    nil))

(defvar unix-command-list nil
  "alist whose element's cars are strings containing the names of all shell
commands defined by executable files in directories mentioned in
unix-command-path.")

(defvar last-shell-command ""
  "The last shell command read by the shell command completion mechanism.")

(defun insert-shell-command ()
  "Read a shell command, with completion on names of executable files in
directories along your shell PATH variable; insert the command in the
current buffer at point, and advance point."
  (interactive)
  (if (null unix-command-list)
      (progn
        (message "Scanning your shell command path, this may take a while...")
        (setq unix-command-list
              (list-executables-on-path unix-command-path))))
  (setq last-shell-command
        (completing-read "Shell command: " unix-command-list
                         nil            ; no predicate
                         nil            ; match not required
                         nil))          ; no default string
  (insert last-shell-command " "))

;;; end of shell-plus.el

__John (jcgs@uk.co.harlqn)
