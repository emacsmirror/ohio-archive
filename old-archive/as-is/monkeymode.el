;Date: 25 Jan 89 03:41:17 GMT
;From: lord+@andrew.cmu.edu  (Tom Lord)
;Subject: new monkey (dired replacement)
;To: info-gnu-emacs@prep.ai.mit.edu
;
;
;I've gotten lots of update requests for monkey - my replacement for dired.
;Some users are running versions that are several years old.  Therefore,
;I am posting the most recent version to the net.  All known bugs have been
;fixed, and some features added.
;
;Biggest Changes:
;
;  Performing an operation on marked files no longer clears the marks. They
;  must be cleared explicitly.
;
;  Files created by rename or copy operations are immediately inserted in all
;  monkey buffers containing the affected directory (either as the
;  top level dir for that buffer, or as an expanded subdirectory).
;
;
;Concientious emacs maintainers should break the background stuff
;out of monkey.el and set it up as a seperate background.el.  It is
;very useful in its own right.
;
;monkey.el:
;; Copyright (C) 1988 Tom Lord <lord+@andrew.cmu.edu>
;; Refer to the GNU Emacs General Public License for copyright info
;
; monkey mode. a mode good at bopping around on (directory) trees.
;  monkey is a good replacement for dired.
;
; To use monkey instead of dired, put these lines in your .emacs:
; (load "monkey.el")
;
;(global-set-key "\C-X\C-F" 'monkey-file)
;(global-set-key "\C-X\C-V" 'monkey-alternate-file)
;(global-set-key "\C-x4f" 'monkey-file-other-window)
;(global-set-key "\C-x4\C-f" 'monkey-file-other-window)
;(global-set-key "\M-g" 'monkey-current-directory)

(defvar monkey-be-fast nil
  "Controls whether files displayed are stated")
(defvar monkey-mode-map nil "Local keymap for monkey-mode buffers.")
(setq monkey-mode-map (make-keymap))
(suppress-keymap monkey-mode-map)

(define-key monkey-mode-map "\C-c\C-m" 'monkey-mark-by-regexp)
(define-key monkey-mode-map "\C-c+" 'monkey-mark-by-regexp)
(define-key monkey-mode-map "\C-c=" 'monkey-mark-by-regexp)
(define-key monkey-mode-map "\C-cm" 'monkey-mark-by-regexp)
(define-key monkey-mode-map "\C-m" 'monkey-mark-this)
(define-key monkey-mode-map "\M-m" 'monkey-mark-all)
(define-key monkey-mode-map "\M-+" 'monkey-mark-all)
(define-key monkey-mode-map "\M-=" 'monkey-mark-all)
(define-key monkey-mode-map "\M-\C-m" 'monkey-mark-all)
(define-key monkey-mode-map "+" 'monkey-mark-this)
(define-key monkey-mode-map "=" 'monkey-mark-this)

(define-key monkey-mode-map "\C-c\C-u" 'monkey-unmark-by-regexp)
(define-key monkey-mode-map "\C-c\C-c" 'monkey-unmark-all)
(define-key monkey-mode-map "\C-cu" 'monkey-unmark-by-regexp)
(define-key monkey-mode-map "\M-u" 'monkey-unmark-all)
(define-key monkey-mode-map "u" 'monkey-unmark-this)
(define-key monkey-mode-map "\C-?" 'monkey-unmark-this-back)

(define-key monkey-mode-map "t" 'monkey-toggle-this)
(define-key monkey-mode-map "\C-ct" 'monkey-toggle-marked-by-regexp)
(define-key monkey-mode-map "\C-c\C-t" 'monkey-toggle-marked-by-regexp)
(define-key monkey-mode-map "\M-t" 'monkey-toggleall)

(define-key monkey-mode-map "\C-n" 'monkey-next-line)
(define-key monkey-mode-map "\C-p" 'monkey-previous-line)
(define-key monkey-mode-map " "  'monkey-next-line)
(define-key monkey-mode-map "n" 'monkey-next-line)
(define-key monkey-mode-map "p" 'monkey-previous-line)
(define-key monkey-mode-map "\M-n" 'monkey-next-directory)
(define-key monkey-mode-map "\M-p" 'monkey-previous-directory)
(define-key monkey-mode-map "\C-c\C-n" 'monkey-next-same-level)
(define-key monkey-mode-map "\C-c\C-p" 'monkey-previous-same-level)
(define-key monkey-mode-map "\M-<" 'monkey-beginning-of-buffer)
(define-key monkey-mode-map "\M->" 'monkey-end-of-buffer)
(define-key monkey-mode-map "\M-v" 'monkey-scroll-down)
(define-key monkey-mode-map "\C-v" 'monkey-scroll-up)

(define-key monkey-mode-map "\C-cn" 'monkey-next-same-level)
(define-key monkey-mode-map "\C-cp" 'monkey-previous-same-level)
(define-key monkey-mode-map "\C-c\C-f" 'monkey-past-subdirectory)
(define-key monkey-mode-map "\C-c\C-b" 'monkey-directory-heading)
(define-key monkey-mode-map "\C-c\C-s" 'monkey-mark-subdirectory)
(define-key monkey-mode-map "\C-cs" 'monkey-mark-subdirectory)
(define-key monkey-mode-map "\C-c^" 'monkey-directory-heading)

(define-key monkey-mode-map "\C-cc" 'monkey-copy-by-regexp)
(define-key monkey-mode-map "\C-cr" 'monkey-rename-by-regexp)
(define-key monkey-mode-map "\M-h" 'monkey-unhide-all)
(define-key monkey-mode-map "#" 'monkey-mark-auto-save-files)
(define-key monkey-mode-map "*" 'monkey-mark-executables)
(define-key monkey-mode-map "." 'monkey-mark-dotfiles)
(define-key monkey-mode-map "/" 'monkey-mark-directories)
(define-key monkey-mode-map "?" 'monkey-summary)
(define-key monkey-mode-map "@" 'monkey-mark-links)
(define-key monkey-mode-map "A" 'monkey-gee)
;(define-key monkey-mode-map "G" 'monkey-change-group)
;(define-key monkey-mode-map "M" 'monkey-change-mode)
;(define-key monkey-mode-map "W" 'monkey-change-owner)
(define-key monkey-mode-map "^" 'monkey-parent)
(define-key monkey-mode-map "`" 'monkey-parent)
(define-key monkey-mode-map "\\" 'monkey-parent)
(define-key monkey-mode-map "!" 'monkey-shell-command)
(define-key monkey-mode-map "&" 'monkey-background)
(define-key monkey-mode-map "C" 'monkey-copy-marked)
(define-key monkey-mode-map "c" 'monkey-copy-this)
(define-key monkey-mode-map "D" 'monkey-delete-marked)
(define-key monkey-mode-map "d" 'monkey-delete-this)
(define-key monkey-mode-map "E" 'monkey-edit-marked)
(define-key monkey-mode-map "e" 'monkey-edit-this)
(define-key monkey-mode-map "F" 'monkey-edit-marked)
(define-key monkey-mode-map "f" 'monkey-edit-this)
(define-key monkey-mode-map "g" 'monkey-gee)
(define-key monkey-mode-map "h" 'monkey-hide-this)
(define-key monkey-mode-map "H" 'monkey-hide-marked)
(define-key monkey-mode-map "j" 'monkey-edit-this-and-trash-this-buffer)
(define-key monkey-mode-map "k" 'monkey-mark-by-type)
(define-key monkey-mode-map "l" 'monkey-list-this-long)
(define-key monkey-mode-map "L" 'monkey-list-long)
(define-key monkey-mode-map "m" 'monkey-mark-this)
(define-key monkey-mode-map "o" 'monkey-edit-this-other-window)
(define-key monkey-mode-map "O" 'monkey-edit-marked-other-window)
(define-key monkey-mode-map "q" 'monkey-toggle-quietness)
(define-key monkey-mode-map "Q" 'monkey-stat-marked)
(define-key monkey-mode-map "r" 'monkey-rename-this)
(define-key monkey-mode-map "R" 'monkey-rename-marked)
(define-key monkey-mode-map "s" 'monkey-mung-this-subdirectory)
(define-key monkey-mode-map "S" 'monkey-mung-marked-subdirectories)
(define-key monkey-mode-map "V" 'monkey-edit-marked-and-trash-this-buffer)
(define-key monkey-mode-map "v" 'monkey-edit-this-and-trash-this-buffer)
(define-key monkey-mode-map "w" 'monkey-copy-this-file-name)
(define-key monkey-mode-map "W" 'monkey-copy-marked-file-names)
(define-key monkey-mode-map "x" 'monkey-mark-by-extension)
(define-key monkey-mode-map "~" 'monkey-mark-backup-files)
(define-key monkey-mode-map "]" 'monkey-shove)
(define-key monkey-mode-map "\M-/" 'monkey-mkdir)

;; Monkey mode is suitable only for specially formatted data.
(put 'monkey-mode 'mode-class 'special)

(defvar monkey-visible-directories ()
  "List of directories displayed in a monkey-mode directory.")
(make-variable-buffer-local 'monkey-visible-directories)

(defun monkey-canonicalize-dirname (name)
  (let ((full-name (expand-file-name name)))
    (if (string= "/" (substring full-name -1))
        (setq full-name (substring full-name 0 -1)))
    full-name))

(defun monkey-visible-directory (name)
  (setq monkey-visible-directories
        (cons (list (monkey-canonicalize-dirname name))
              monkey-visible-directories)))

(defun monkey-is-visible (dir)
  (assoc (monkey-canonicalize-dirname dir) monkey-visible-directories))

(defun monkey-rem-visable (dir lst)
  (cond ((null lst) nil)
        ((string= dir (car (car lst))) (cdr lst))
        (t (cons (car lst) (monkey-rem-visable dir (cdr lst))))))

(defun monkey-invisible-directory (name)
  (setq monkey-visible-directories
        (monkey-rem-visable (monkey-canonicalize-dirname name)
                          monkey-visible-directories)))

(defun monkey-mode ()
  "Mode for \"editing\" directory listings.
In monkey, you are \"editing\" a list of the files in a directory.
You can move using the usual cursor motion commands.
Letters no longer insert themselves.

In monkey, you may operate on any number of files at a time.  You do
this by `marking' those files you are interested in.  If you do not
mark any files, then the file on the line containing the point is
considered `marked'.

Many commands come in pairs, one version that affects the file on the
line with the point, and one version that affects the marked files.
In the default bindings, these pairs are bound to the lower and
uppercase of some character.

The format of lines in a monkey buffer is important.  It consists of
three fields: <markfield><typefield> <filename>.  The markfield is
empty for unmarked files, and is a `+' for marked files.  The
typefield contains a character describing the type of the file: `/'
for directories, `@' for symbolic links, `*' for executables, `,' for
character devies, and `$' for block devices. For example, a marked
directory named `foo' would look like

+/ foo

while an unmarked text file named `bar' would look like:

   bar


The monkey-mode commands are summarized below.  There are many
intuitive aliases for the more common commands (for example, `+' is an
alias for `m' to mark the current file).  These aliases are not
summarized below.

Basic movement commands:
n       move down one line.
SPACE   move down one line.
p       move up one line.

Note: with a prefix arg, all movement commands iterate.


Basic marking commands:

Note: with a prefix arg, all marking commands become
unmarking commands, and vice versa.  Also, ESC is a general
prefix meaning `do this to everything'.

RET     mark this file.
u       unmark this file.
DEL     unmark and move backwards.
t       toggle this mark.
ESC m   mark everything
ESC u   unmark everything
ESC t   toggle all marks
~       mark all backup files.
#       mark all check point files.
.       mark all `dot' files.
@       mark all symbolic links.
*       mark all executables.
/       mark all directories.
k       mark files by typefield.
        (e.g. k/ is an alias for /)


Basic Operations.
e       edit this file.
E       edit marked files.
v       edit this file and trash this buffer.
V       edit maked files and trash this buffer.
o       edit this file in the other window.
O       edit this file in the other windows
l       show a long directory listing for the current file.
L       show a long directory listing for the marked files.
c       copy (this can take a dir as destination).
C       copy marked files.
r       rename.
R       rename marked files.
d       delete.
D       delete named files.
w       copy the current filename to the kill ring.
W       copy the marked filenames to the kill ring.
]       shove the marked filenames into a scratch buffer
        (especially useful with shell-comman-on-region).
!       execute a shell command on the marked files.
&       execute a background command on the marked files.


Subdirectory commands:
s       expand in-situ the current subdirectory.
S       expand in-situ the marked subdirectories.
        With a prefix arg, s and S unexpand subdirectories.
C-cC-s  mark this subdirectory.
C-uC-cC-s unmark this subdirectory. (remember the rule about marking
               commands and prefix args?)
C-cC-f  move past this subdirectory.
C-cC-b  move before this subdirectory.
C-cC-n  move forward skipping subdirectories.
C-cC-p  move backward skipping subdirectories.
ESC n   move to the next directory.
ESC p   move to the previous directory.
C-c^    move to the directory line for this subdir.
Hiding commands:
h       hide this file.
H       hide marked files.
        With a prefix arg, unhide rather than hide.
ESC h   unhide at this line.
ESC H   unhide all hidden files.

Regexp commands:
C-cC-m  mark files matching a regexp.
C-cu    unmark files matching a regexp.
C-cc    copy by regexp (see below).
C-cr    rename by regexp.
x       mark files matching .*\\.REGEXP.  REGEXP is prompted for.
        (i.e. x o <RET> marks all object files)

Copying and renaming by regexp is an unusual feature.  It is useful
for operating on groups of files, when the name changes are regular.
For example, suppose that in some directory, I have a groop of files with
names like m-sun.h, m-ibmrt.h etc., and I wish to move them all to a
directory called `machines', stripping them of the `m-' affix in the
process.  Then I
would
1) Mark them using `C-cm m-\\(.*\\) RET'
2) Rename them using `C-cr machines/\\1 RET'

Pretty cool, huh?

Misc commands:
q       turn off file stating.  When file stating is off, monkey is very
        fast,
        but all type fields show up as '?'.
g       use this when you find yourself saying `Gee, that can't be right!'
^       edit the parent of this directory."

  (kill-all-local-variables)
  (setq major-mode 'monkey-mode)
  (setq mode-name "Monkey")
  (setq mode-line-buffer-identification '("Monkey: %17b"))
  (setq case-fold-search nil)
  (setq buffer-read-only t)
  (setq selective-display t)
  (use-local-map monkey-mode-map)
  (setq markive-display t)
  (monkey-visible-directory default-directory)
  (run-hooks 'monkey-mode-hook)
  (message "Does your minkey have a license?"))







;
;  generally useful functions that I wish came with emacs.
;

(defun tail (string1 string2)
  "Strip string1 from string2 if it is present."
  (let ((n (length string1))
        (y (length string2)))
    (if (and (>= y n) (string= string1 (substring string2 0 n)))
        (substring string2 n y)
      string2)))

(defun abs (x)
  (cond ((< x 0) (- x))
        (t x)))

(defun signum (x)
  (if (< x 0)
      -1
    1))

(defun delete-directory (file)
  "This little loose of a function should be in C.
And should do error checking."
  (call-process "rmdir" nil nil nil file))

(defun delete-file-properly (file &optional ok-if-directory)
  "Delete FILE.  If FILE is a nonempty directory, signal an error.  If FILE
is an empty directory, the course of action depends on the optional
parameter OK-IF-DIRECTORY.  If nil, an error is raised, if numeric,
the user is asked for permission to delete it, otherwise, the file is
silently deleted."
  (interactive "fDelete File: \np")
  (cond ((not (file-attributes file))
         (error "You don't have access to %s." file))
        ((not (eq t (car (file-attributes file))))
         (delete-file file))
        ((not ok-if-directory)
         (error "%s is a directory." file))
        ((not (eq (length (directory-files file)) 2))
         (error "%s is not an empty directory." file))
        ((or (not (numberp ok-if-directory))
             (y-or-n-p (format "Delete directory %s? " file)))
         (delete-directory file))
        (t (error "%s not deleted."))))


(defun copy-file-properly (file destination &optional ok-if-already-exists)
  "Copy FILE to DESTINATION. If DESTINATION is a directory,
then copy FILE into DESTINATION."
  (interactive "fCopy file: \nfCopy to: \np")
  (let ((real-destination
         (if (and (not (file-directory-p file))
                 (file-directory-p destination))
             (concat (file-name-as-directory destination)
                    (file-name-nondirectory file))
           destination)))
    (copy-file file real-destination ok-if-already-exists)
    real-destination))

(defun rename-file-properly (file newname &optional ok-if-already-exists)
  "Rename FILE as NEWNAME.  If NEWNAME is the name of a directory,
then move FILE to that directory. See rename-file for more."
  (interactive "fRename File: \nFRename to: \np")
  (let ((real-destination
         (if (file-directory-p destination)
             (concat
              (file-name-as-directory destination)
              (file-name-nondirectory file))
           destination)))
    (rename-file file real-destination ok-if-already-exists)
    real-destination))


(defun eol-point (&optional count)
  "Return the point at the end of the current line."
  (save-excursion
    (end-of-line count)
    (point)))

(defun bol-point (&optional count)
  "Return the point at the beginning of the current line."
  (save-excursion
    (beginning-of-line count)
    (point)))


;
; monkey-buffer format munging.
;    see the comment in monkey-mode for a description of
;    monkey-buffers.
;
;
(setq monkey-status-fields-regexp "[ +&][ $*?@/,] *")

; it is hoped that the above will be faster than
; the `true' regexp which is:   "[ +][ \\$\\*\\?@/,] *"

(setq monkey-filename-regexp
  (concat monkey-status-fields-regexp
          "\\(\\([^\n\r/]\\|\\(/[^\n\r]\\)\\)*\\)"))

(setq  monkey-marked-file-regexp
  (concat "^\\+. *" "\\(\\([^\n\r/]\\|\\(/[^\n\r]\\)\\)*\\)"))


(defvar monkey-always-hide-regexp nil
  "*Regexp matching those files which should be hidden after a
directory is listed in a minkey buffer.")


(defun monkey-filetype ()
  "Return the file type of the current file.  This assumes there is a
filename on this line."
  (save-excursion
    (beginning-of-line)
    (char-after (1+ (point)))))

(defun monkey-filename-beginning (&optional important)
  "Return the point position of the first char of the filename on the
current monkeybuffer line.  If the optional parameter IMPORTANT is
non-nil, then signal an error if there is no filename on this line.
Otherwise, returns nil if no file is found."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at monkey-filename-regexp) (match-beginning 1))
          (important (error "There is no file on this line."))
          (t nil))))

(defun monkey-filename-end (&optional important)
  "Return the point position of the end of the filename on the
current monkeybuffer line.  If the optional parameter IMPORTANT is
non-nil, then signal an error if there is no filename on this line.
Otherwise, returns nil if no file is found."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at monkey-filename-regexp) (match-end 1))
          (important (error "There is no file on this line."))
          (t nil))))

(defun monkey-filename (&optional important)
  "Return the filename on the current line.  If the optional parameter
IMPORTANT is nil, then signal an error. Otherwise, return nil if no
file is found."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at monkey-filename-regexp)
           (buffer-substring (match-beginning 1) (match-end 1)))
          (important (error "There is no file on this line."))
          (t nil))))

(defconst monkey-insert-distance 2
  "*Number of spaces to indent for each level of subdirectoriness. This
can be any number greater than 0.")

(defun occurences (char string)
  "Return the number of occurences of CHAR in STRING."
  (let ((len (length string))
        (x 0)
        (total 0))
    (while (< x len)
      (if (eq char (aref string x))
          (setq total (1+ total)))
      (setq x (1+ x)))
    total))

(defun monkey-insert-filename (name &optional top-level)
  "Insert a line for NAME in the current buffer.  Name should be a path
specification relative to the current directory."
  (let* ((buffer-read-only nil)
         (name (if top-level
                  name
                (tail default-directory
                      (if (eq ?~ (string-to-char name))
                         (expand-file-name name)
                       name))))
         (indent-spaces
          (if (or top-level (eq ?/ (string-to-char name)))
              1
            (1+ (* monkey-insert-distance
                  (occurences ?/ name))))))
    ;(monkey-move-to-insertion-point name)
    (beginning-of-line)
    (if (or (eq (char-after (point)) ?\n)
            (= (point) (point-max)))
        nil
      (end-of-line)
      (insert ?\n))
    (insert
     " "
     (cond (monkey-be-fast ??)
           ((file-directory-p name) ?/)
           ((file-symlink-p name) ?@)
           ((file-readable-p name) " ")
           (t ??)))
    (insert-char 32 indent-spaces)
    (insert " " name)))

;            (t (let* ((mode (nth 8 attr))
;                     (char (string-to-char mode)))
;                (cond ((eq char ?b) ?$)
;                     ((eq char ?c) ?,)
;                     ((and (eq char ?-) (string-match "x" mode)) ?*)
;                     (t " ")))))
(defun monkey-sorted-insert-filename (name &optional no-unhide)
  "Move to the alphabetically correct place, and insert NAME.
Optional NO-UNHIDE prevents dealing with hidden files."
  (let* ((buffer-read-only nil)
         (name (tail default-directory
                    (if (eq ?~ (string-to-char name))
                       (expand-file-name name)
                      name))))
    (or no-unhide (monkey-temp-unhide))
    (save-excursion
      (goto-char (catch 'FOUND-POS
                  (progn
                    (monkey-map-file
                     '(lambda () (if (string-lessp (monkey-filename) name)
                                   nil
                                 (beginning-of-line)
                                 (save-excursion (insert ?\n))
                                 (throw 'FOUND-POS (point)))))
                    ; this is reached if filename should
                     ; be added at the bottom
                    (goto-char (point-max))
                    (insert "\n")
                    (throw 'FOUND-POS (point)))))
      (monkey-insert-filename name))
    (or no-unhide (monkey-un-temp-unhide))))

(defun buffer-mode-name (&optional b)
  "Return the mode of a buffer"
  (save-excursion
    (set-buffer (or b (current-buffer)))
    mode-name))

(defun monkey-distribute-filename (name)
  "Put a filename in all monkey-buffers that should know about it."
  (let* ((buffers (buffer-list))
        (full-name (expand-file-name name))
        (directory (file-name-directory full-name)))
    (while buffers
      (if (string= (buffer-mode-name (car buffers)) "Monkey")
          (save-excursion
            (set-buffer (car buffers))
            (if (monkey-is-visible directory)
               (monkey-sorted-insert-filename
                (tail default-directory full-name)))))
      (setq buffers (cdr buffers)))))


(defun monkey-delete-line ()
  "Remove the current file line from a monkey buffer."
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (or (bobp) (backward-delete-char 1))
    (while (not (or (eobp)
                   (let ((char (char-after (point))))
                     (or (eq char ?\n) (eq char ?\r)))))
      (delete-char 1))
    (and (bobp) (delete-char 1))))

(defun monkey-expanded-p ()
  "Returns *t* if the current filename is that of an expanded subdir"
  (eq ?/ (char-after (monkey-filename-end t))))


(defun monkey-hide-line ()
  "Hide the current file line.  If the file is the name of an expanded
subdir, then hide the entire subdir."
  (let* ((buffer-read-only nil)
         (expanded (monkey-expanded-p))
         (name (and expanded (monkey-filename))))
    (save-excursion
      (monkey-unmark)
      (beginning-of-line)
      (or (bobp)
          (backward-delete-char 1))
      (insert ?\r)
      (and expanded
           (monkey-map-matches 'monkey-hide-line
                             (concat (regexp-quote (concat name "/"))
                                    ".*"))))))

(defun monkey-temp-unhide ()
  "Unhide all hidden lines temporarily."
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (replace-string "\r " "\n&"))))

(defun monkey-un-temp-unhide ()
  "c.f. monkey-temp-unhide"
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (replace-string "\n&" "\r "))))

(defun bounded-replace-string (from to start end)
  (save-excursion
    (goto-char start)
    (while (search-forward from end t)
      (replace-match to t nil))))



(defun monkey-unhide-line (&optional fail-silently mark-unhidden-lines)
  "Unhide files hidden on this line."
  (let ((buffer-read-only nil))
    (unwind-protect     ; for some reason, this doesn't work without
                      ; an unwind-protect.  redisplay doesn't happen
                      ; correctly.  go figure.
        (save-excursion
          (beginning-of-line)
          (or (search-forward "\r" (eol-point) t)
              fail-silently
              (error "Nothing is hidden here!"))
          (beginning-of-line)
          (if mark-unhidden-lines
              (bounded-replace-string "\r " "\n+"
                                   (point) (eol-point))
            (subst-char-in-region (point) (eol-point) ?\r ?\n t))
          (beginning-of-line)
          (while (eq ?\n (char-after (point)))
              (delete-char 1))))))



(defun monkey-list-directory (&optional directory)
  "Insert a directory listing of the default directory or optionally of
DIRECTORY."
  (let ((dir (or directory default-directory))
        (top-level (not directory)))
    (mapcar '(lambda (x) (monkey-insert-filename x top-level))
            (directory-files dir directory nil))
    (if monkey-always-hide-regexp
        (monkey-map-matches 'monkey-hide-line
                          monkey-always-hide-regexp))))





;
;  inserting and removing subdirectories in situ
;
(defun monkey-expand-subdirectory ()
  "Expand in-situ the contents of a subdirectory."
  (let* ((name (monkey-filename t))
         (base-name (file-name-nondirectory name))
        (buffer-read-only nil))
    (if (not (file-directory-p name))
        (error "%s is not a directory." name))
    (if (or (string= "." base-name)
            (string= ".." base-name))
        (error "Why would you expand `%s'?" name))
    (goto-char (monkey-filename-end))
    (if (eq (char-after (point)) ?/)
        (error "%s has already been expanded." name))
    (insert ?/)
    (save-excursion (monkey-list-directory name))
    (monkey-visible-directory name)))

(defun monkey-delete-matching-lines (regexp)
  "Delete all the lines that match regexp.  This effects
hidden as well as visible lines."
  (save-excursion
    (goto-char (point-min))
    (replace-regexp
     (concat "^" monkey-status-fields-regexp regexp "\\([/\n\r]\\|$\\)")
     "")
    (goto-char (point-min))
    (replace-regexp
     (concat "\r" monkey-status-fields-regexp regexp "\\([/\n\r]\\|$\\)")
     "")
    (goto-char (point-min))
    (replace-regexp "\n\n" "\n")
    (goto-char (point-min))
    (replace-regexp "\n$" "")))


(defun monkey-unexpand-subdirectory ()
  "Unexpand in-situ the contents of a subdirectory."
  (or (monkey-expanded-p)
      (monkey-directory-heading 1))
  (let* ((name (monkey-filename t))
         (buffer-read-only nil)
         (subdir-regexp (concat (regexp-quote (concat name "/")) ".*")))
    (goto-char (monkey-filename-end))
    (delete-char 1)
    (save-excursion
      (monkey-map-matches
       '(lambda ()
          (monkey-unhide-line t))
       subdir-regexp))
    (monkey-delete-matching-lines subdir-regexp)
    (monkey-invisible-directory name)))







;
; mark status munging
;

(defun monkey-mark ()
  "Mark the current file line."
  (monkey-filename t)
  (save-excursion
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert ?+))))

(defun monkey-unmark ()
  "Unmark the current file line."
  (monkey-filename t)
  (save-excursion
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (delete-char 1)
      (insert " "))))


(defun monkey-marked-p ()
  "True if the current file line is marked."
  (save-excursion
    (beginning-of-line)
    (eq (char-after (point)) ?+)))

(defun monkey-toggle ()
  "Change the mark status of the current line."
  (if (monkey-marked-p)
      (monkey-unmark)
    (monkey-mark)))



;
; mapping functions. these exist to make the interactive functions
; easier to write
;
;
;

(defun nice-monkey ()
  "Make everything look nice."
  (and (save-excursion
         (re-search-backward
          "\r"
          (save-excursion (beginning-of-line) (point))
          t))
       (forward-line))
  (goto-char (or (monkey-filename-beginning) (point))))


(defun monkey-map-file (fun)
  "Apply FUNCTION to each fileline in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (and (monkey-filename) (apply fun ()))
      (forward-line)))
  (nice-monkey))

; it will be faster to use direct searching on the buffer
;(defun monkey-map-matches (function regexp)
;  "Apply FUNCTION to each file line matching REGEXP.
;The REGEXP must match the entire file name."
;  (monkey-map-file
;   '(lambda ()
;      (let ((name (monkey-filename)))
;       (and (string-match regexp name)
;            (eq (match-beginning 0) 0)
;            (eq (match-end 0) (length name))
;            (apply function ()))))))

(defun monkey-map-matches (function regexp)
  "Apply FUNCTION to each file line matching REGEXP.
The REGEXP must match the entire file name."
  (save-excursion
    (let ((srchfor (concat "^" monkey-status-fields-regexp "\\(" regexp "\\)"
                         "\\([\n\r]\\|$\\)")))
      (goto-char (point-min))
      (while (re-search-forward srchfor nil t)
        (if (not (eobp)) (forward-char -1))
        (let ((end (monkey-filename-end)))
          (if (eq (char-after end) ?/)
              (setq end (1+ end)))
          (and end
               (= (point) end)
               (apply function ()))))))
  (nice-monkey))




(defun monkey-map-type (function typefield)
  "Apply FUNCTION to each file that has TYPEFIELD in its type field."
  (monkey-map-file
   '(lambda ()
      (and (eq (monkey-filetype) typefield)
           (apply function ())))))

;(defun monkey-map-marked (function &optional dont-unmark call-on-any-line)
;  "Apply FUNCTION to each file line which is marked.
;Optional DONT-UNMARK, if non-nil, means don't unmark marked lines.
;Optional CALL-ON-ANY-LINE means apply this function even if no files are
;marked and there is no mark on the current line."
;
; using searches will be faster than this.
;  (let ((were-any nil))
;    (save-excursion
;      (monkey-map-file
;       '(lambda ()
;         (and (monkey-marked-p)
;              (progn
;               (setq were-any t)
;               (or dont-unmark (monkey-unmark))
;               (apply function ()))))))
;    (or were-any
;       (and (not call-on-any-line)
;            (not (monkey-filename t)))
;       (progn
;         (apply function ())
;         (nice-monkey))))
;  (nice-monkey))

(defun monkey-map-marked (function &optional dont-unmark call-on-any-line)
  "Apply FUNCTION to each file line which is marked.
Optional DONT-UNMARK if non-nil, means don't unmark marked lines.
Optional CALL-ON-ANY-LINE means apply this function even if no files are
marked and there is no mark on the current line."
  (let ((were-any nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward monkey-marked-file-regexp nil t)
        (setq were-any t)
        (or dont-unmark (monkey-unmark))
        (apply function ())))
    (or were-any
        (and (not call-on-any-line)
             (not (monkey-filename t)))
        (apply function ()))
    (nice-monkey)))

;
;
;  interactive functions
;
;
;
;

;
; cursor motion
;

(defun monkey-next-line (&optional prefix)
  "Move to the next line of a monkey buffer."
  (interactive "p")
  (forward-line prefix)
  (nice-monkey))

(defun monkey-previous-line (&optional prefix)
  "Move to the previous line of a monkey buffer."
  (interactive "p")
  (monkey-next-line (- (or prefix 1))))

(defun monkey-beginning-of-buffer ()
  "Move to the beginning of a monkey buffer."
  (interactive)
  (goto-char (point-min))
  (nice-monkey))

(defun monkey-end-of-buffer ()
  "Move to the bottom of a monkey buffer."
  (interactive)
  (goto-char (point-max))
  (nice-monkey))

(defun monkey-scroll-up (&optional prefix)
  "Scroll up nicely in a monkey-buffer"
  (interactive "p")
  (scroll-up (and current-prefix-arg prefix))
  (nice-monkey))

(defun monkey-scroll-down (&optional prefix)
  "Scroll down nicely in a monkey-buffer"
  (interactive "p")
  (scroll-down (and current-prefix-arg prefix))
  (nice-monkey))

(defun monkey-next-directory (&optional count)
  "Move forward to the next directory."
  (interactive "p")
  (let ((count (abs (or count 1)))
        (direction (signum (or count 1))))
    (goto-char
     (save-excursion
       (while (> count 0)
         (catch 'found
           (while (not (eobp))
             (forward-line direction)
             (and (monkey-expanded-p)
                 (throw 'found nil)))
           (error "No more directories."))
         (setq count (1- count)))
       (point))))
  (nice-monkey))

(defun monkey-previous-directory (&optional count)
  "Move backward to the previous directory."
  (interactive "p")
  (monkey-next-directory (- (or count 1))))
(defun monkey-directory-heading (count)
  "Move to the line containing the directory name for the current
subdirectory."
  (interactive "p")
  (let ((count (abs (or count 1))))
    (while (> count 0)
      (let ((target-name (file-name-directory (monkey-filename))))
        (or target-name (error "You are not in a subdirectory."))
        (goto-char (point-min))
        (re-search-forward (concat (regexp-quote target-name)
                                "\\(\r.*\\)?$")))
      (beginning-of-line)
      (setq count (1- count))))
  (nice-monkey))


(defun monkey-past-subdirectory (count)
  "Move past one extreme of the current subdirectory. This is of
questionable worth if the buffer is not sorted alphabetically."
  (interactive "p")
  (let ((count (abs (or count 1)))
        (direction (signum (or count 1))))
    (while (> count 0)
      (and (not (monkey-expanded-p))
           (monkey-directory-heading 1))
      (let ((dirname (concat (monkey-filename t) "/")))
        (while (if (> direction 0)
                  (search-forward dirname nil t)
                (search-backward dirname nil t))
          (+ 1 1))     ;dumbass while exits if the BODY is empty!
        (forward-line direction))
      (setq count (1- count))))
  (nice-monkey))

(defun monkey-before-subdirectory (count)
  "Move before the current subdirectory."
  (interactive "p")
  (monkey-past-subdirectory (- count)))

(defun monkey-depth ()
  "Return an integer describing the level of subdirectory nesting.
This integer is NOT the level of subdirectory nesting, but can be used
to decide which of two files is nested deeper."
  (- (monkey-filename-beginning t) (bol-point)))


(defun monkey-next-same-level (&optional count)
  "Move to the next line which is at the same depth of subdirectoriness."
  (interactive "p")
  (let ((depth (monkey-depth))
        (count (abs (or count 1)))
        (direction (signum (or count 1))))
    (while (> count 0)
      (forward-line direction)
      (while (not (or (bobp) (eobp) (<= (monkey-depth) depth)))
        (forward-line direction))
      (setq count (1- count))))
  (nice-monkey))

(defun monkey-previous-same-level (&optional count)
  "Move to the previous line which is at the same depth of
subdirectoriness."
  (interactive "p")
  (monkey-next-same-level (- (or count 1))))





;
; marking and unmarking
;

(defun monkey-action-and-move (function &optional count move-first)
  "Provide the apply-and-move semantics of the -this functions.
FUNCTION is the function to apply to each line, COUNT is a repitition count
(may be nil) and the optional MOVE-FIRST, if non-nil, means move before
applying the function."
  (let ((direction (signum (or count 1)))
        (count (abs (or count 1))))
    (while (> count 0)
      (if move-first
          (forward-line direction))
      (apply function ())
      (if (not move-first)
          (forward-line direction))
      (setq count (1- count)))
    (nice-monkey)))


(defun monkey-mark-this (&optional count)
  "Mark and move to the next line."
  (interactive "p")
  (monkey-action-and-move 'monkey-mark count))

(defun monkey-unmark-this (&optional count)
  "Unmark and move to the next line."
  (interactive "p")
  (monkey-action-and-move 'monkey-unmark count))

(defun monkey-toggle-this (&optional count)
  "Unmark and move to the next line."
  (interactive "p")
  (monkey-action-and-move 'monkey-toggle count))


(defun monkey-mark-this-back (&optional count)
  "Mark this line and move backwards one line."
  (interactive "p")
  (monkey-action-and-move 'monkey-mark (- (or count 1)) t))


(defun monkey-unmark-this-back (&optional count)
  "Unmark this line and move backwards one line."
  (interactive "p")
  (monkey-action-and-move 'monkey-unmark (- (or count 1)) t))

(defun monkey-toggle-this-back (&optional count)
  "Unmark this line and move backwards one line."
  (interactive "p")
  (monkey-action-and-move 'monkey-toggle (- (or count 1))))

(defun monkey-mark-all ()
  "Mark all files. With a prefix arg, unmarks all files."
  (interactive)
  (monkey-map-file
   (if current-prefix-arg
       'monkey-unmark
     'monkey-mark)))

(defun monkey-unmark-all ()
  "Unmark all files. With a prefix arg, marks all files."
  (interactive)
  (monkey-map-file
   (if current-prefix-arg
       'monkey-mark
     'monkey-unmark)))

(defun monkey-toggleall ()
  "Exchange the set of marked files with the set of unmarked files."
  (interactive)
  (monkey-map-file 'monkey-toggle))

(defvar monkey-last-mark-regexp nil
  "The last regexp used to mark files in a monkey buffer.")
(make-variable-buffer-local 'monkey-last-mark-regexp)

(defun monkey-mark-by-regexp (regexp)
  "Mark all files matching REGEXP.  Unmarks with a prefix arg."
  (interactive "sRegexp: ")
  (save-excursion
    (monkey-map-matches
     (if current-prefix-arg
         'monkey-unmark
       'monkey-mark)
     regexp)
    (setq monkey-last-mark-regexp regexp)))

(defun monkey-unmark-by-regexp (regexp)
  "Unmark all files matching REGEXP.  Marks with a prefix arg."
  (interactive "sRegexp: ")
  (save-excursion
    (monkey-map-matches
     (if current-prefix-arg
         'monkey-mark
       'monkey-unmark)
     regexp)
    (setq monkey-last-mark-regexp regexp)))

(defun monkey-toggle-marked-by-regexp (regexp)
  "Exchange the set of marked files that match REGEXP with the set of
unmarked files that match."
  (interactive "sRegexp: ")
  (monkey-map-matches 'monkey-toggle regexp))

(defun monkey-mark-auto-save-files ()
  "Mark all the autosave files.  Unmarks them with a prefix."
  (interactive)
  (monkey-mark-by-regexp "#.*#"))
(defun monkey-mark-backup-files ()
  "Mark all the backup files.  Unmarks them with a prefix."
  (interactive)
  (monkey-mark-by-regexp ".*~"))

(defun monkey-mark-dotfiles ()
  "Mark all files beginning with a `.'."
  (interactive)
  (monkey-mark-by-regexp "\\..*"))

(defun monkey-mark-by-type (type)
  "Mark all the files of type TYPE.  What a loose."
  (interactive "cType: ")
  (monkey-map-type
   (if current-prefix-arg
       'monkey-unmark
     'monkey-mark)
   type))

(defun monkey-mark-directories ()
  "Mark all the directories."
  (interactive)
  (monkey-mark-by-type ?/))
(defun monkey-mark-links ()
  "Mark all the symbolic links."
  (interactive)
  (monkey-mark-by-type ?@))

(defun monkey-mark-executables ()
  "Mark all the executable files."
  (interactive)
  (monkey-mark-by-type ?*))

(defun monkey-mark-subdirectory ()
  "Mark the current subdirectory."
  (interactive)
  (save-excursion
    (or (monkey-expanded-p)
        (monkey-directory-heading 1))
    (let ((dirname (monkey-filename)))
      (monkey-map-matches
       (if current-prefix-arg
           'monkey-unmark
         'monkey-mark)
       (concat (regexp-quote (concat dirname "/")) ".+")))))
(defun monkey-mark-by-extension (regexp)
  "Mark all files that end with .REGEXP"
  (interactive "sRegexp for extension: ")
  (monkey-mark-by-regexp (concat ".*\\." regexp)))



;
; actions on marked files
;
;
(defun monkey-shove ()
  "Put the names of the marked file into a scratch buffer."
  (interactive)
  (let ((list-buffer (get-buffer-create "*File List*")))
    (save-excursion
      (set-buffer list-buffer)
      (erase-buffer))
    (monkey-map-marked
     '(lambda ()
        (let ((name (monkey-filename)))
          (save-excursion
            (set-buffer list-buffer)
            (goto-char (point-max))
            (insert name "\n")))) t)
    (switch-to-buffer-other-window list-buffer)))


(defun monkey-mung-marked-subdirectories ()
  "Expand or collapse marked subdirectories.  Prefix makes a difference as
to which happens."
  (interactive)
  (save-excursion
    (monkey-map-marked
     (if current-prefix-arg
         'monkey-unexpand-subdirectory
       'monkey-expand-subdirectory) t))
  (nice-monkey))

(defun monkey-mung-this-subdirectory ()
  "Expand or collapse a subdirectory. Prefix determines which."
  (interactive)
  (save-excursion
    (if current-prefix-arg
        (monkey-unexpand-subdirectory)
      (monkey-expand-subdirectory)))
  (nice-monkey))

(defun monkey-hide-marked ()
  "Hide all marked files.  With a prefix, unhide near marked lines."
  (interactive)
  (monkey-map-marked
   (if current-prefix-arg
       'monkey-unhide-line
     'monkey-hide-line)
   nil t))

(defun monkey-hide-this ()
  "Hide this file.  With a prefix, unhide near this line."
  (interactive)
  (if current-prefix-arg
      (monkey-unhide-line)
    (monkey-hide-line))
  (monkey-next-line))


(defun monkey-unhide-all ()
  "Unhide all files."
  (interactive)
  (let ((buffer-read-only nil))
    (save-excursion
      (unwind-protect   ;redisplay does not happen correctly w/out this
          (progn (subst-char-in-region (point-min) (point-max) ?\r ?\n t)
                (goto-char (point-min))
                (replace-string "\n\n" "\n")
                (goto-char (point-min))
                (and (= (following-char) ?\n)
                     (delete-char 1))))))
  (nice-monkey))


(defun monkey-list-long ()
  "Show the output of ls -l'ing the marked files."
  (interactive)
  (with-output-to-temp-buffer "*monkey-ls-output*"
    (monkey-map-marked
     '(lambda ()
        (call-process "ls" nil "*monkey-ls-output*" t "-ld"
                     (monkey-filename))) t)))

(defun monkey-list-this-long ()
  "Show the output of ls -l'ing the marked files."
  (interactive)
  (save-window-excursion
    (with-output-to-temp-buffer "*monkey-short-ls-output*"
      (call-process "ls" nil "*monkey-short-ls-output*" t "-ld"
                   (monkey-filename)))
    (set-buffer "*monkey-short-ls-output*")
    (goto-char (point-min))
    (message (buffer-substring (bol-point) (eol-point)))))



(defun monkey-confirm-deletions (deletion-list)
  "Make sure that DELETION-LIST meets with the users approval."
  (if (= (length deletion-list) 1)
      (y-or-n-p (format "Delete %s ?" (car deletion-list)))
    (save-excursion
      (with-output-to-temp-buffer "*Deletions*"
        (set-buffer "*Deletions*")
        (while deletion-list
          (insert (car deletion-list) "\n")
          (setq deletion-list (cdr deletion-list)))
        (display-buffer "*Deletions*")
        (yes-or-no-p "Delete these files?")))))


(defun monkey-delete-marked ()
  "Delete all the marked files."
  (interactive)
  (save-window-excursion
    (let (deletion-list)
      (monkey-map-marked
       '(lambda ()
          (setq deletion-list
               (cons (monkey-filename t)
                     deletion-list)))
         t)
      (if (not (monkey-confirm-deletions deletion-list))
          (message "Nothing Deleted.")
        (monkey-map-marked
         '(lambda ()
            (delete-file-properly (monkey-filename t) 1)
            (monkey-delete-line)))))))

(defun monkey-delete-this ()
  "Delete all the current file."
  (interactive)
  (save-window-excursion
    (let ((deletion-list (list (monkey-filename t))))
      (if (not (monkey-confirm-deletions deletion-list))
          (message "Nothing Deleted.")
        (delete-file-properly (car deletion-list))
        (monkey-delete-line)
        (monkey-next-line 1)))))

(defun monkey-copy-file (destination &optional unhidden)
  "Copy the current file to DESTINATION."
  (monkey-distribute-filename
   (copy-file-properly (monkey-filename t) destination 1)))

(defun monkey-copy-this (destination)
  "Copy the current file to DESTINATION."
  (interactive "FCopy to: ")
  (monkey-copy-file destination)
  (nice-monkey))

(defun monkey-copy-marked (destination)
  "Copy the marked files to DESTINATION."
  (interactive "FCopy to: ")
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-map-marked
       '(lambda () (monkey-copy-file destination t)) t)
    (monkey-un-temp-unhide)))

(defun monkey-rename-file (destination &optional unhidden)
  "Rename the current file to DESTINATION."
  (save-excursion
    (monkey-distribute-filename
     (rename-file-properly (monkey-filename t) destination 1)))
  (monkey-delete-line))

(defun monkey-rename-this (destination)
  "Rename the current file to DESTINATION."
  (interactive "FRename to: ")
  (monkey-rename-file destination)
  (nice-monkey))
(defun monkey-rename-marked (destination)
  "Rename the marked files to DESTINATION."
  (interactive "FRename to: ")
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-map-marked
       '(lambda ()
          (monkey-rename-file destination t)) t)
    (monkey-un-temp-unhide)))
;
; various ways of editting marked files
;

(defun monkey-edit-file ()
  "If the current file is a directory, create a monkey-buffer for it.
Otherwise, find-file it, but don't switch to the new buffer.
Someday, this should offer to execute executables.  Returns the
new buffer, if any."
  (save-excursion
    (let ((name (monkey-filename t)))
      (if (file-directory-p name)
          (monkey-directory (file-name-as-directory name))
        (find-file name))
      (current-buffer))))

(defun monkey-edit-marked ()
  "Edit all the marked files.  Switch to the edit buffer for the first one
in the list."
  (interactive)
  (let ((destination-buffer nil))
    (monkey-map-marked
     '(lambda ()
        (message "finding %s..." (monkey-filename))
        (let ((x (monkey-edit-file)))
          (setq destination-buffer
               (if destination-buffer 'dont-switch x)))) t)
    (message "done.")
    (or (eq destination-buffer 'dont-switch)
        (switch-to-buffer destination-buffer))))

(defun monkey-edit-this ()
  "Edit this file."
  (interactive)
  (switch-to-buffer (monkey-edit-file)))
(defun monkey-edit-marked-other-window ()
  "Edit all the marked files.  Switch to the edit buffer for the first
one in the other window."
  (interactive)
  (let ((destination-buffer nil))
    (monkey-map-marked
     '(lambda ()
        (message "finding %s..." (monkey-filename))
        (let ((x (monkey-edit-file)))
          (setq destination-buffer (or destination-buffer x)))) t)
    (switch-to-buffer-other-window destination-buffer)))

(defun monkey-edit-this-other-window ()
  "Edit this file in the other window."
  (interactive)
  (switch-to-buffer-other-window (monkey-edit-file)))

(defun monkey-edit-marked-and-trash-this-buffer ()
  "Edit all the marked files and trash this buffer."
  (interactive)
  (save-excursion
    (monkey-edit-marked))
  (kill-buffer (current-buffer)))

(defun monkey-edit-this-and-trash-this-buffer ()
  "Edit all the marked files and trash this buffer."
  (interactive)
  (save-excursion
    (monkey-edit-this))
  (kill-buffer (current-buffer)))
;
; the fancy copy and rename by regexp commands
;
;



(defun monkey-copy-by-regexp (copy-by-regexp-target)
  "Using the regexp used last for monkey-mark-by-regexp, match each marked
filename, then copy it."
;
; the following piece of crap could be eliminated if version 19 includes
; the function rewrite-regexp.  Rewrite-regexp should take arguments
; SOURCE-REGEXP, SOURCE, TARGET.
; it should match SOURCE against SOURCE-REGEXP and then rewrite TARGET
; in the fasion of replace-match.  An optional parameter should control the
; behaviour of the function
; in the case that SOURCE-REGEXP and SOURCE can not be matched.
  (interactive "sCopy to: ")
  (if (null monkey-last-mark-regexp)
      (error "You must mark files using a regexp first."))
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-map-marked
       '(lambda ()
          (goto-char (monkey-filename-beginning))
          (if (not (looking-at monkey-last-mark-regexp))
              (error "%s does not match the source regexp."
                    (monkey-filename)))
          (let ((old-name (monkey-filename))
               (buffer-read-only))
            (re-search-forward monkey-last-mark-regexp nil nil)
            (replace-match copy-by-regexp-target nil nil)
            (let ((new-name (monkey-filename)))
              (delete-region
               (monkey-filename-beginning)
               (monkey-filename-end))
              (insert old-name)
              (monkey-copy-file new-name t)))) t)
    (monkey-un-temp-unhide)))

(defun monkey-rename-by-regexp (rename-by-regexp-target)
  "Using the regexp used last for monkey-mark-by-regexp, match each marked
filename, then rename it."
  (interactive "sRename to: ")
  (or monkey-last-mark-regexp
      (error "You must mark files using a regexp first."))
  (monkey-temp-unhide)
  (unwind-protect
      (monkey-map-marked
       '(lambda ()
          (goto-char (monkey-filename-beginning))
          (if (not (looking-at monkey-last-mark-regexp))
              (error "%s does not match the source regexp."
                    (monkey-filename)))
          (let ((old-name (monkey-filename))
               (buffer-read-only nil))
            (re-search-forward monkey-last-mark-regexp nil nil)
            (replace-match rename-by-regexp-target nil nil)
            (let ((new-name (monkey-filename)))
              (delete-region
               (monkey-filename-beginning)
               (monkey-filename-end))
              (insert old-name)
              (monkey-rename-file new-name t)))) t)
    (monkey-un-temp-unhide)))



;
;  misc. monkey functions
;
;
(defun monkey-gee ()
  "Empty the buffer, and re-list the directory. Start from
scratch."
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (monkey-list-directory)
    (goto-char (point-min)))
  (nice-monkey))
(defun monkey-parent ()
  "Edit the directory `..'."
  (interactive)
  (monkey-directory ".."))



(defun monkey-summary ()
  "Give the luser a summary of monkey commands."
  (interactive)
  (message
   (substitute-command-keys
    "\\[monkey-mark-this] mark, \\[monkey-unmark-this] unmark,
\\[monkey-delete-marked] delete, many many more...M-x describe-mode")))




;
; invoking monkey
;
;
(defun monkey-directory-noselect (directory)
  "Make a buffer for DIRECTORY, but don't select it.
The buffer is returned."
  (let* ((name (file-name-as-directory (expand-file-name directory)))
         (buffer (get-buffer-create name)))
    (save-excursion
      (set-buffer buffer)
      (and (eq (buffer-size) 0)
           (progn (setq buffer-read-only t)
                 (setq default-directory name)
                 (set-buffer-modified-p nil)
                 (monkey-list-directory)
                 (goto-char (point-min))
                 (goto-char (or (monkey-filename-beginning) (point)))
                 (monkey-mode)))
      buffer)))


(defun monkey-directory (directory)
  "Make a buffer for directory and monkey around with it."
  (interactive "DDirectory: ")
  (switch-to-buffer (monkey-directory-noselect directory)))

(defun minkey (directory)
  "A nice alias for monkey-directory."
  (interactive "DDirectory: ")
  (monkey-directory directory))

(defvar monkey-state-stack nil
  "Used for pushing and popping monkey states.")

;
; these make good replacements for
;  find-file, find-alternate-file, and find-file-other-window

(defun monkey-file (file)
  (interactive "FFile: ")
  (if (file-directory-p file)
      (monkey-directory file)
    (find-file file)))


(defun monkey-alternate-file (file)
  (interactive "FAlternate file: ")
  (kill-buffer (current-buffer))
  (monkey-file file))

(defun monkey-file-other-window (file)
  (interactive "FFile other window: ")
  (if (not (file-directory-p file))
      (find-file-other-window file)
    (let ((pop-up-windows t))
      (pop-to-buffer (monkey-directory-noselect file)))))

(defun monkey-toggle-quietness ()
  "Turn file stating on or off"
  (interactive)
  (if (setq monkey-be-fast (not monkey-be-fast))
      (message "monkey: file stating turned off")
    (message "monkey: file stating turned on")))

(defun monkey-stat-marked ()
  "Fill in the type field for the marked files."
  (interactive)
  (let ((monkey-be-fast nil)
        (p (point)))
    (monkey-temp-unhide)
    (monkey-map-marked
     '(lambda ()
        (let ((name (monkey-filename)))
          (monkey-delete-line)
          (monkey-distribute-filename
           (concat default-directory name)))) t)
    (monkey-un-temp-unhide)
    (goto-char p)
    (nice-monkey)))


(defun monkey-copy-this-file-name ()
  "Copy the current file name into the kill ring"
  (interactive)
  (copy-region-as-kill (monkey-filename-beginning) (monkey-filename-end)))

(defun monkey-copy-marked-file-names (&optional flag)
  "Copy all of the marked file names into the kill ring
seperated by a space.  Prefix arg causes filenames to be
sperated by a newline."
  (interactive "P")
  (save-excursion
    (let ((list-buffer (get-buffer-create "*Cut File List*"))
          (sep (if flag "\n" " ")))
      (save-excursion
        (set-buffer list-buffer)
        (erase-buffer))
      (monkey-map-marked
       '(lambda ()
          (let ((name (monkey-filename)))
            (save-excursion
              (set-buffer list-buffer)
              (goto-char (point-max))
              (insert name sep)))) t)
      (set-buffer list-buffer)
      (copy-region-as-kill (point-min) (point-max)))))

(defun monkey-shell-command ()
  "Prompt for a shell command using the marked filenames as
a default command string."
  (interactive)
  (monkey-copy-marked-file-names)
  (shell-command-on-region (point) (point)
                         (read-string "Shell command: "
                                    (car kill-ring)) nil))




;
;
;  added extra bonus courtesy of Joe Keane
;
;
(defun monkey-background ()
  "Invoke a background process on the marked files."
  (interactive)
  (monkey-copy-marked-file-names)
  (background (read-string "& " (car kill-ring))))


(defun monkey-mkdir (directory)
  "Make DIRECTORY by forking mkdir."
  (interactive "FMake directory: ")
  (and (file-exists-p directory)
       (error "directory `%s' already exists" directory))
  (call-process "mkdir" nil nil nil (expand-file-name directory))
  (or (file-exists-p directory)
      (error "can't make directory `%s'" directory))
  (monkey-distribute-filename directory))


;; Fun with background jobs.
;; Copyright (C) 1988 Joe Keane <jk3k+@andrew.cmu.edu>
;; Refer to the GNU Emacs General Public License for copyright info

;; patches to environment
;; so Andrew applications realize they're running under wm
;; where are setenv and unsetenv?
(and (string= (getenv "TERM") "wm-term")
 (let ((old process-environment)
       (new '("TERM=wm")))
   (while old
     (let ((entry (car old)))
       (setq old (cdr old))
       (or (string-match "TERM" entry) (nconc new (list entry)))))
   (setq process-environment new)))

;; user variables
(defvar background-show t
  "*If non-nil, background jobs' buffers are shown when they're started.")
(defvar background-select nil
  "*If non-nil, background jobs' buffers are selected when they're started.")

;; patches to shell-mode
(require 'shell)
(define-key shell-mode-map "\C-c\C-b" 'continue-shell-subjob)
(define-key shell-mode-map "\C-c\C-f" 'continue-shell-subjob)
(define-key shell-mode-map "\C-c\C-k" 'kill-shell-subjob)
(defun continue-shell-subjob ()
  "Continue this shell's current subjob."
  (interactive)
  (continue-process nil t))

(defun background (command &optional noshow)
  "Run COMMAND in the background like csh.  A message is displayed when
the job starts and finishes.  The buffer is in shell mode, so among
other things you can control the job and send input to it.  The
process object is returned if anyone cares.  See also shell-mode and
the variables background-show and background-select."
  (interactive "s%% \nP")
  (let*
      ((job-number 1)
       (showit (and (not noshow) background-show))
       (process
        (let ((job-name "%1"))
          (while (process-status job-name)
            (setq job-name (concat "%" (setq job-number (1+ job-number)))))
          (setq default-directory
           (prog1
               (if (string-match
                   "^cd[\t ]+\\([^\t ;]+\\)[\t ]*;[\t ]*"
                   command)
                  (prog1
                      (file-name-as-directory
                      (expand-file-name
                       (substring command
                        (match-beginning 1) (match-end 1))))
                    (setq command (substring command (match-end 0))))
                default-directory)
             (if background-select (pop-to-buffer job-name)
               (and showit (with-output-to-temp-buffer job-name))
               (set-buffer (get-buffer-create job-name)))))
          (start-process job-name job-name shell-file-name "-c" command))))
    (message "[%d] %d" job-number (process-id process))
    (erase-buffer)
    (insert "% cd " default-directory "\n% " command ?\n)
    (set-marker (process-mark process) (point))
    (set-process-sentinel process 'background-sentinel)
    (shell-mode)
    (myshell-init-buffer)
    (setq mode-name "Background")
    process))

(defun background-sentinel (process msg)
  "Called when a background job changes state."
  (message
   "[%s] %s %s"
   (substring (process-name process) 1)
   (setq msg
    (cond
     ((string= msg "finished\n") "Done")
     ((string-match "^exited" msg)
      (concat "Exit " (substring msg 28 -1)))
     ((zerop (length msg)) "Continuing") ;;Emacs bug
     (t (concat (upcase (substring msg 0 1)) (substring msg 1 -1)))))
   (nth 2 (process-command process)))
  (if (buffer-name (process-buffer process))
      (and
       (memq (process-status process) '(signal exit))
       (set-buffer
        (prog1 (current-buffer)
          (set-buffer (process-buffer process))
          (and
           (prog1 (eobp)
             (save-excursion
               (goto-char (point-max))
               (insert ?\n msg ?
               (substring (current-time-string) 11 19) ?\n)))
           (goto-char (point-max)))
          (set-buffer-modified-p nil))))
    (set-process-buffer process nil)))

