;;; listbuf.el --- build buffer menu for use in Buff-menu-mode

;; Copyright (C) 1991, 1994, 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions, gratuitous frobs
;; Created: 1991-10-03
;; Status: Works in any version of Emacs or XEmacs

;; LCD Archive Entry:
;; listbuf|Noah Friedman|friedman@prep.ai.mit.edu|
;; build buffer menu for use in Buff-menu-mode|
;; 10-Apr-1995|1.8|~/misc/listbuf.el.gz|

;; $Id: listbuf.el,v 1.8 1995/04/10 16:46:19 friedman Rel $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; The standard list-buffers is a primitive in Emacs 18, but no other
;; primitives call it (that I know of), so it can be replaced in a reliable
;; way here.  In most variants of Emacs 19, it's a lisp function anyway.
;; Put this program in a separate file somewhere in your load path and put
;; the line
;;
;;        (load "listbuf")
;; or
;;        (autoload 'listbuf "listbuf" nil t)
;;
;; somewhere in your .emacs file.  You might also wish to do one (but
;; probably not both) of the following:
;;
;;        (substitute-key-definition 'list-buffers 'listbuf ctl-x-map)
;;        (defalias 'list-buffers 'listbuf)
;;

;;; Code:


(defconst listbuf-lucid-p
  (let ((m (match-data)))
    (prog1
        (string-match "GNU Emacs [0-9.]+\\( XEmacs\\)* Lucid "
                      (emacs-version))
      (store-match-data m)))
  "Non-`nil' if this is XEmacs or Lucid Emacs.")

(defvar listbuf-buffer-name "*Buffer List*"
  "*The name of buffer-menu buffer.")

(defvar pre-listbuf-hook nil
  "*Hook to run by listbuf before anything else.")

(defvar post-listbuf-hook nil
  "*Hook to run by listbuf after everything else.")

(defvar listbuf-field-alist
  '(("MR"       2    2  listbuf-get-MR)
    ("Buffer"  14   20  listbuf-get-Buffer)
    ("Rev"      3   10  listbuf-get-Rev)
    ("Proc"     4    6  listbuf-get-Proc)
    ("Size"     4    6  listbuf-get-Size)
    ("Mode"     4   20  listbuf-get-Mode)
    ("File"     0  nil  listbuf-get-File))
  "Association list used to determine how to fill fields in the buffer menu.

Each member of the alist should contain the following elements to define a
field:

  * The name of the field, as a string.  This name will appear in the
    header of the list of buffers.

  * The minimum width of the column.
    If the actual width of the entry in this column doesn't exceed this
    value and any fields come after it, it will be padded with whitespace.

  * The maximum tolerated width of a column, or `nil' if no maximum is
    desired.  This value is ignored if it is in the last member of the
    alist.

    If a column exceeds this width, it may be truncated or padded depending
    on the setting of `listbuf-oversized-fields-action'.

  * A function which returns a string containing the information about
    that field.  This function will always be run with the current buffer
    set to the buffer for which information is being gathered.
    For example, for the `Mode' field the function should just return
    the value of the buffer-local variable `mode-name'.

The fields will be listed in the buffer menu in the same order they appear
in this alist.

Note that in order for the buffer menu mode to work properly, the buffer
name must be the second field, and the first column must always have a
consistent width.  Besides that, there is relative freedom in the display
of other fields.")

(defvar listbuf-oversized-field-action 'none
  "*Specify what to do with fields that are abnormally long.
The value should be one of the following symbols:

* `pad' means ignore any maximum width specification for that field in
  `listbuf-field-alist' and fully justify all fields so that no text from
  one field runs into another.
  If you have buffers with very long names, this can push most of the rest
  of the fields for all buffers out of view, so that you have to scroll
  horizontally to see them.

* `truncate' means truncate any entry that is longer than the maximum
  length tolerated in `listbuf-field-alist' for that field.
  This can have the affect that long buffer names (for example) are
  inaccessible from the buffer menu, since they don't have proper names.

* `none' means let oversized entries run over into other fields, moving the
  rest of the fields for that line over as necessary.
  This is the default.")

(defvar listbuf-ignore-buffer-name-regexp-list
  '("^ ")
  "*List of regexps matching buffer names to ignore.
For example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is \"^ \".")

(defvar listbuf-file-field-table
  '((dired-mode listbuf-get-dired-file)
    (w3-mode    listbuf-get-w3-file))
  "*Alist of major-mode to function mapping for getting visited file names.
Some buffers have no file associated with them at all; they just contain
local data.  Others are associated with files they are visiting, or URLs,
or a specific directory (in the case of dired mode), etc.

This alist should be a map between each major mode and a function name
(or lambda expression) which retrieves this information, assuming the
buffer in question is the current one.

If the major mode for a particular buffer does not appear in this list,
then the function `buffer-file-name' is used as the default.")

(defvar listbuf-buffer-file-name-filter 'listbuf-prettify-file-name
  "*Filter through which to pass file name before displaying in buffer list.
The function named by this variable should receive one argument, the name
of a file (as a string).  It should return a string (possibly the same one)
that serves as the name to print in the buffer list.

This variable may be set to `nil' to disable this feature.")

(defvar listbuf-sorting-predicate nil
  "*If non-`nil', a function used to determine how to sort buffer entries.

This variable specifies a function which should return `t' if the buffer
described by its first argument should come before the buffer described by
the second argument, in the buffer list.  The arguments are vectors
containing data gathered for a buffer according `listbuf-field-alist'.

If this variable is `nil', no sorting occurs.
The buffers are then listed in the order in which they are presently
arranged in emacs' internal ring, most recently selected buffer first.

An example sorting predicate is `listbuf-sort-by-buffer-name'.")


(defun listbuf (&optional files-only-p)
  "Display a list of names of existing buffers.

Non-null optional arg FILES-ONLY-P means mention only file buffers.

The list is displayed in a buffer specified by the variable
`listbuf-buffer-name'; by default this is `*Buffer List*'.

Buffers with names matching patterns specified by
`listbuf-ignore-buffer-name-regexp-list' are omitted.
By default this is any buffer name that begins with a space.

The M column contains a * for buffers that are modified.
The R column contains a % for buffers that are read-only.

Other fields may be added this the output generated by this command.
See the variable `listbuf-field-alist'.

The hook `pre-listbuf-hook' and `post-listbuf-hook' are run before and
after everything else, respectively."
  (interactive "P")
  (let ((menubuf (get-buffer-create listbuf-buffer-name))
        (orig-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer menubuf)
          (setq buffer-read-only nil)
          (erase-buffer)
          (run-hooks 'pre-listbuf-hook)
          (listbuf-1 files-only-p)
          ;; Buffer-menu-mode should reset buffer-read-only itself.
          ;(setq buffer-read-only t)
          (Buffer-menu-mode)
          (goto-char (point-min))
          (forward-line 2)
          (display-buffer menubuf)
          (make-local-variable 'revert-buffer-function)
          (setq revert-buffer-function
                (if files-only-p
                    (function (lambda (&rest ignored) (listbuf t)))
                  (function (lambda (&rest ignored) (listbuf)))))
          (run-hooks 'post-listbuf-hook))
      (set-buffer orig-buffer))))

(defun listbuf-1 (&optional files-only-p)
  (let* ((buflist (buffer-list))
         (info-list nil)
         (vlen (length listbuf-field-alist))
         (width-vector (make-vector vlen 0))
         (indent-vector (make-vector (1+ vlen) 0))
         i l
         min max len ilen)

    ;; Get buffer information
    (while buflist
      (setq result (listbuf-collect (car buflist) files-only-p))
      (and result
           (setq info-list (cons result info-list)))
      (setq buflist (cdr buflist)))

    (cond
     (info-list
      ;; Set width-vector slots initially to the maximum desired width of
      ;; each field.
      ;; Start with index 1 since elt 0 is just the buffer object.
      (setq i 1)
      (while (< i vlen)
        (setq len (length (nth 0 (nth (1- i) listbuf-field-alist))))
        (setq min (or (nth 1 (nth (1- i) listbuf-field-alist)) 0))
        (setq max (nth 2 (nth (1- i) listbuf-field-alist)))

        (setq l info-list)
        (while l
          (setq ilen (length (aref (car l) i)))
          (cond
           ((or (null max)
                (eq listbuf-oversized-field-action 'pad)
                (>= max ilen))
            (aset width-vector i (max (aref width-vector i) len ilen min)))
           (t
            (aset width-vector i (max (aref width-vector i) len min))))
          (setq l (cdr l)))
        (setq i (1+ i))))
     (t
      ;; There are no buffer names to list; just set the width vector to
      ;; be a list of the width of each field name, so the output isn't
      ;; all bunched together into a StudlyCaps mess.
      (setq i 1)
      (while (< i vlen)
        (aset width-vector i (length (nth 0 (nth (1- i) listbuf-field-alist))))
        (setq i (1+ i)))))

    ;; Now recompute width-vector slots to be column offsets in buffer.
    ;; This is the sum of the width in the current slot plus the (already
    ;; adjusted) offset in the previous slot.
    ;; Add 1 to each to account for intercolumn space.
    (aset width-vector 0 1)
    (setq i 1)
    (while (< i vlen)
      (aset width-vector i (+ 1
                              (aref width-vector i)
                              (aref width-vector (1- i))))
      (setq i (1+ i)))

    ;; Copy width-vector into indent-vector with an offset of 1.
    ;; Isn't there a better way of doing this?
    (setq i 0)
    (while (< i vlen)
      (aset indent-vector (1+ i) (aref width-vector i))
      (setq i (1+ i)))

    (if listbuf-sorting-predicate
        (setq info-list (sort info-list listbuf-sorting-predicate))
      (setq info-list (nreverse info-list)))

    (listbuf-print info-list indent-vector)))

;; Collect all the desired information about buffers into a vector.
(defun listbuf-collect (buffer &optional files-only-p)
  (save-excursion
    (set-buffer buffer)
    (cond
     ((and files-only-p
           (not (listbuf-buffer-file-p)))
      nil)
     ((listbuf-ignore-buffername-p buffer)
      nil)
     (t
      (let ((vect (make-vector (1+ (length listbuf-field-alist)) nil))
            (alist listbuf-field-alist)
            (i 1)
            result
            maxlen)
        (aset vect 0 buffer)
        (while alist
          (setq result (or (funcall (nth 3 (car alist)))
                           ""))
          (cond
           ((eq listbuf-oversized-field-action 'truncate)
            (setq maxlen (nth 2 (car alist)))
            (and maxlen
                 (< maxlen (length result))
                 (setq result (substring result 0 maxlen)))))
          (aset vect i result)
          (setq alist (cdr alist))
          (setq i (1+ i)))
        vect)))))

;; Determine if buffer has a file name associated with it.
;; Right now it returns t for any buffer that has anything to do with a
;; file, via the method lookups in listbuf-file-field-table.
;; Maybe this should only return t for things that have a true file
;; associated with them as indicated by `buffer-file-name'?
(defun listbuf-buffer-file-p ()
  (let ((name (listbuf-get-File)))
    (cond ((string= name "")
           nil)
          (name t)
          (t nil))))

(defun listbuf-ignore-buffername-p (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (let ((data (match-data))
        (bufname (buffer-name buffer))
        (re-list listbuf-ignore-buffer-name-regexp-list)
        (ignorep nil))
    (while re-list
      (cond
       ((string-match (car re-list) bufname)
        (setq ignorep t)
        (setq re-list nil))
       (t
        (setq re-list (cdr re-list)))))
    (store-match-data data)
    ignorep))

(defun listbuf-print (info-list indent-vector)
  (let ((alist listbuf-field-alist)
        str idx col
        (i 1))

    ;; Print the header
    (insert "\n")
    (forward-line -1)
    (beginning-of-line)
    (while alist
      (setq str (car (car alist)))
      (setq idx (aref indent-vector i))

      (indent-to idx)
      (insert str)

      (forward-line 1)
      (end-of-line)
      (indent-to idx)
      (insert (make-string (length str) ?-))

      (forward-line -1)
      (end-of-line)

      (setq alist (cdr alist))
      (setq i (1+ i)))
    (forward-line 1)
    (end-of-line)
    (insert "\n")

    ;; Print buffer list contents
    (while info-list
      (setq idx 1)
      (setq i (length (car info-list)))
      (while (< idx i)
        (setq str (aref (car info-list) idx))
        (setq col (aref indent-vector idx))
        (cond
         ((or (null str)
              (string= "" str)))
         ((= idx 2)
          ;; The buffer name is treated magically.
          ;;
          ;; In Lucid Emacs, if the first character of the
          ;; buffer name (which *must* be the 2nd field in order for
          ;; buff-menu to sensibly parse things) is a quotation mark, then
          ;; the file name is quoted against whitespace, and the beginning
          ;; quote must be set back one column in order for buff-menu to
          ;; recognize this.
          ;;
          ;; In Emacs 19.29 and later, buff-menu gets the name using a text
          ;; property instead of trying to parse the text of the buffer
          ;; itself.  Note that because of this, you can truncate the
          ;; buffer name yet still be able to reference it.
          (if (>= (current-column) col)
              (insert " ")
            (indent-to col 0))
          ;; Record the column where buffer names start.
          ;; In emacs 19.29 buff-menu.el, this is set by list-buffers-noselect.
          ;; In previous versions it was set in Buffer-menu-mode.
          ;; In any case, we should set it here as appropriate.
          ;; it's not column 4.
          (setq Buffer-menu-buffer-column (current-column))
          (cond
           ((and listbuf-lucid-p
                 (= ?\" (aref str 0)))
            (delete-char -1)
            (insert str))
           ((fboundp 'put-text-property)
            (let ((start (point)))
              (insert str)
              (put-text-property start (1- (point)) 'buffer-name
                                 (buffer-name (aref (car info-list) 0)))
              (put-text-property start (1- (point)) 'mouse-face 'highlight)))
           (t
            (insert str))))
         (t
          (if (>= (current-column) col)
              (insert " ")
            (indent-to col 0))
          (insert str)))
        (setq idx (1+ idx)))
      (insert "\n")
      (setq info-list (cdr info-list)))))


;; Field-accessing functions

(defun listbuf-get-MR ()
  (concat (if (buffer-modified-p) "*" " ")
          (if buffer-read-only    "%" " ")))

(defun listbuf-get-Buffer ()
  (cond
   (listbuf-lucid-p
    ;; In XEmacs/Lucid Emacs, if the buffer name contains any whitespace or
    ;; quote marks, it is quoted to disambiguate the name of the buffer
    ;; from trailing whitespace.
    (let ((b (buffer-name))
          (data (match-data)))
      (cond
       ((string-match "[ \t\n\"]" b)
        (setq b (prin1-to-string b))
        ;; prin1 doesn't convert newlines to \n.
        (while (string-match "\n" b)
          (setq b (concat (substring b 0 (match-beginning 0))
                          "\\n"
                          (substring b (match-end 0)))))))
      (store-match-data data)
      b))
   (t
    ;; Emacs
    ;; A space is appended to the buffer name because two spaces signals
    ;; the end of the buffer name in the menu.
    ;; Actually, in 19.29 and later, a text property is used instead, but
    ;; it's harmless to leave the space in and it's useful for computing
    ;; appropriate column widths for older versions of emacs.
    (concat (buffer-name) " "))))

(defun listbuf-get-Rev ()
  (cond
   ((and (boundp 'vc-mode)
         vc-mode)
    (substring vc-mode 1))
   ;; This is too slow.
   ;((buffer-file-name)
   ; ;; `bv-length' is a free variable used by backup-extract-version.
   ; ;; Ridiculous.
   ; (let ((bv-length (+ 2 (length (buffer-file-name))))
   ;       (fn (car (find-backup-file-name (buffer-file-name)))))
   ;   (and (>= (length fn) bv-length)
   ;        (int-to-string (backup-extract-version fn)))))
   (t nil)))

(defun listbuf-get-Size ()
  (int-to-string (buffer-size)))

(defun listbuf-get-Mode ()
  mode-name)

(defun listbuf-get-Proc ()
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc
         (symbol-name (process-status proc)))))

(defun listbuf-get-File ()
  (let* ((method (or (car (cdr (assq major-mode listbuf-file-field-table)))
                    'buffer-file-name))
         (name (cond ((and method
                           (funcall method)))
                     ((and (boundp 'list-buffers-directory)
                           list-buffers-directory
                           (file-name-as-directory default-directory))))))
    (if listbuf-buffer-file-name-filter
        (funcall listbuf-buffer-file-name-filter name)
      name)))


;; File name accessing functions for various major modes.

(defun listbuf-get-dired-file ()
  ;; vc.el sometimes makes this a cons.
  (if (stringp dired-directory)
      dired-directory
    (format "%s" (mapcar 'listbuf-prettify-file-name dired-directory))))

(defun listbuf-get-w3-file ()
  (and (fboundp 'url-view-url)
       (url-view-url t)))


;; Other random utility functions.

(defun listbuf-prettify-file-name (name)
  "Return a file name that compacts the user's home directory into `~'.
This also works for remote file names that use efs or ange-ftp."
  (let ((abbrev name))
    (and name
         (fboundp 'abbreviate-file-name)
         (setq abbrev (abbreviate-file-name name)))

    (cond
     ((null name)
      nil)
     ((not (string= name abbrev))
      abbrev)
     (t
      ;; I should be beaten with a wet noodle for doing this.
      (let* ((data (match-data))
             (regexp (cond
                      ((and (boundp 'efs-path-regexp) efs-path-regexp))
                      ;; emacs (rms-ftp)
                      ((and (boundp 'ange-ftp-name-format)
                            (car ange-ftp-name-format)))
                      ;; lemacs (ange-ftp)
                      ((and (boundp 'ange-ftp-path-format)
                            (car ange-ftp-path-format)))
                      (t "^/\\([^@:/]*@\\)?\\([^@:/]*\\):.*")))
             (prefix (if (string-match regexp name)
                         (concat (substring name 0 (match-end 2)) ":")
                       ""))
             (home (expand-file-name (concat prefix "~/")))
             (hlen (length home))
             (nlen (length name)))
        (cond
         ((> hlen nlen))
         ((string= (substring name 0 hlen) home)
          (setq name (concat prefix "~/" (substring name hlen)))))
        (store-match-data data))
      name))))

;; Sorting predicate which causes buffers to be sorted alphabetically by
;; buffer name (ignoring case).
;; See `listbuf-sorting-predicate'.
(defun listbuf-sort-by-buffer-name (v1 v2)
  (string-lessp (downcase (aref v1 2)) (downcase (aref v2 2))))

;; Sorting predicate which causes buffers to be grouped by major mode and then
;; sorted alphabetically by buffer name (ignoring case).
;; See `listbuf-sorting-predicate'.
(defun listbuf-sort-by-mode-and-buffer-name (v1 v2)
  (let ((mode1 (downcase (aref v1 6)))
	(mode2 (downcase (aref v2 6))))
    (or (string-lessp mode1 mode2)
	(and (string-equal mode1 mode2)
	     (string-lessp (downcase (aref v1 2)) (downcase (aref v2 2)))))))

;; A possibly interesting thing to do is to cause the buffer list to be
;; updated automatically after execution commands are finish (deleting
;; buffers, etc).  To do that, eval the following.  This only works in
;; emacs 19, since emacs 18 doesn't have post-command-hook or add-hook.
;;
;;   (add-hook 'post-listbuf-hook 'listbuf-revert-after-execute)
;;
(defun listbuf-revert-after-execute ()
  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook 'listbuf-do-revert-after-execute))

(defun listbuf-do-revert-after-execute ()
  (and (eq 'Buffer-menu-execute this-command)
       (revert-buffer)))


(provide 'listbuf)

;;; listbuf.el ends here
