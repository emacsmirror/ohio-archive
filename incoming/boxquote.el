;;; boxquote.el --- Quote text with a semi-box.
;; Copyright 1999 by Dave Pearson <davep@hagbard.demon.co.uk>
;; $Revision: 1.2 $

;; boxquote is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;;; Commentary:

;; boxquote provides a set of functions for using a text quoting style that
;; partially boxes in the left hand side of an area of text, such a marking
;; style might be used to show externally included text or example code.
;;
;; ,----
;; | The default style looks like this.
;; `----
;;
;; A number of functions are provided for quoting a region, a buffer, a
;; paragraph and a defun. There are also functions for quoting text while
;; pulling it in, either by inserting the contents of another file or by
;; yanking text into the current buffer.
;;
;; A couple of people have pointed out that there is some overlap between
;; this code and picture mode, I'd agree, there is some overlap. Once I've
;; finished having fun with this code I might look at splicing boxquote into
;; picture mode. Suggestions of how this might be done and what purpose this
;; might serve are more than welcome.

;;; Revision history:
;;
;; $Log: boxquote.el,v $
;; Revision 1.2  1999/08/04 06:55:16  davep
;; Added `boxquote-title'.
;; Added optional auto-titling when using `boxquote-insert-file'.
;; Added `boxquote-kill'.
;; Numerous changes to tidy things up.
;;
;; Revision 1.1  1999/08/02 07:41:23  davep
;; Initial revision
;;

;;; Thanks:

;; Thanks to Kai Grossjohann <URL:mailto:kai.grossjohann@cs.uni-dortmund.de>
;; for inspiring the idea of boxquote. I wrote this code to mimic the
;; "inclusion quoting" style in his Usenet posts. I could have hassled him
;; for his code but it was far more fun to write it myself.

;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl))
(require 'rect)

;; Attempt to handle older/other emacs.
(eval-and-compile
  
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring)))
  
  ;; If `line-beginning-position' isn't available provide one.
  (unless (fboundp 'line-beginning-position)
    (defun line-beginning-position ()
      "Return the `point' of the beginning of the current line."
      (save-excursion
        (beginning-of-line)
        (point))))

  ;; If `line-end-position' isn't available provide one.
  (unless (fboundp 'line-end-position)
    (defun line-end-position ()
      "Return the `point' of the end of the current line."
      (save-excursion
        (end-of-line)
        (point)))))

;; Customize options.

(defgroup boxquote nil
  "Mark regions of text with a half-box."
  :group  'editing
  :prefix "boxquote-")

(defcustom boxquote-top-and-tail "----"
  "*Text that will be used at the top and tail of the box."
  :type  'string
  :group 'boxquote)

(defcustom boxquote-top-corner ","
  "*Text used for the top corner of the box."
  :type  'string
  :group 'boxquote)

(defcustom boxquote-bottom-corner "`"
  "*Text used for the bottom corner of the box."
  :type  'string
  :group 'boxquote)

(defcustom boxquote-side "| "
  "*Text used for the side of the box."
  :type  'string
  :group 'boxquote)

(defcustom boxquote-title-format "[ %s ]"
  "*Format string to use when creating a box title."
  :type  'string
  :group 'boxquote)

(defcustom boxquote-title-files t
  "*Should a `boxquote-insert-file' title the box with the file name?"
  :type '(choice
          (const :tag "Title the box with the file name" t)
          (const :tag "Don't title the box with the file name" nil))
  :group 'boxquote)

(defcustom boxquote-file-title-function #'file-name-nondirectory
  "*Function to apply to a file's name when using it to title a box."
  :type  'function
  :group 'boxquote)

;; Main code:

(defun boxquote-points ()
  "Find the start and end points of a boxquote.

If `point' is inside a boxquote then a cons is returned, the `car' is the
start `point' and the `cdr' is the end `point'. NIL is returned if no
boxquote is found."
  (save-excursion
    (beginning-of-line)
    (let* ((re-top    (concat "^" (regexp-quote boxquote-top-corner)
                              (regexp-quote boxquote-top-and-tail)))
           (re-left   (concat "^" (regexp-quote boxquote-side)))
           (re-bottom (concat "^" (regexp-quote boxquote-bottom-corner)
                              (regexp-quote boxquote-top-and-tail)))
           (points
            (flet ((find-box-end (re &optional back)
                     (save-excursion
                       (when (if back
                                 (search-backward-regexp re nil t)
                               (search-forward-regexp re nil t))
                         (point)))))
              (cond ((looking-at re-top)
                     (cons (point) (find-box-end re-bottom)))
                    ((looking-at re-left)
                     (cons (find-box-end re-top t) (find-box-end re-bottom)))
                    ((looking-at re-bottom)
                     (cons (find-box-end re-top t) (line-end-position)))))))
      (when (and (car points) (cdr points))
        points))))
  
(defun boxquote-points-with-check ()
  "Get the `boxquote-points' and flag an error of no box was found."
  (or (boxquote-points) (error "I can't see a box here")))

;;;###autoload
(defun boxquote-title (title)
  "Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that the title will
be formatted using `boxquote-title-format'."
  (interactive "sTitle: ")
  (save-excursion
    (save-restriction
      (boxquote-narrow-to-boxquote)
      (setf (point) (+ (point-min)
                       (length (concat boxquote-top-corner
                                       boxquote-top-and-tail))))
      (unless (eolp)
        (kill-line))
      (unless (zerop (length title))
        (insert (format boxquote-title-format title))))))

;;;###autoload
(defun boxquote-region (start end)
  "Draw a box around the left hand side of a region bounding START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (flet ((bol-at-p (n)
               (setf (point) n)
               (bolp))
             (insert-corner (corner pre-break)
               (insert (concat (if pre-break "\n" "")
                               corner boxquote-top-and-tail "\n"))))
        (let ((break-start (not (bol-at-p start)))
              (break-end   (not (bol-at-p end))))
          (narrow-to-region start end)
          (setf (point) (point-min))
          (insert-corner boxquote-top-corner break-start)
          (let ((start-point (line-beginning-position)))
            (setf (point) (point-max))
            (insert-corner boxquote-bottom-corner break-end)
            (string-rectangle start-point
                              (progn
                                (setf (point) (point-max))
                                (forward-line -2)
                                (line-beginning-position))
                              boxquote-side)))))))

;;;###autoload
(defun boxquote-buffer ()
  "Apply `boxquote-region' to a whole buffer."
  (interactive)
  (boxquote-region (point-min) (point-max)))

;;;###autoload
(defun boxquote-insert-file (filename)
  "Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a title that
is the result applying `boxquote-file-title-funciton' to FILENAME."
  (interactive "fInsert file: ")
  (let ((target (current-buffer)))
    (with-temp-buffer
      (let ((source (current-buffer)))
        (insert-file-contents filename nil)
        (boxquote-buffer)
        (when boxquote-title-files
          (boxquote-title (funcall boxquote-file-title-function filename)))
        (with-current-buffer target
          (insert-buffer source))))))

;;;###autoload
(defun boxquote-yank ()
  "Do a `yank' and box it in with `boxquote-region'."
  (interactive)
  (let ((target (current-buffer)))
    (with-temp-buffer
      (let ((source (current-buffer)))
        (yank)
        (boxquote-buffer)
        (with-current-buffer target
          (insert-buffer source))))))

;;;###autoload
(defun boxquote-defun ()
  "Apply `boxquote-region' the current defun."
  (interactive)
  (mark-defun)
  (boxquote-region (region-beginning) (region-end)))

;;;###autoload
(defun boxquote-paragraph ()
  "Apply `boxquote-region' to the current paragraph."
  (interactive)
  (mark-paragraph)
  (boxquote-region (region-beginning) (region-end)))

;;;###autoload
(defun boxquote-boxquote ()
  "Apply `boxquote-region' to the current boxquote."
  (interactive)
  (let ((box (boxquote-points-with-check)))
    (boxquote-region (car box) (1+ (cdr box)))))

;;;###autoload
(defun boxquote-narrow-to-boxquote ()
  "Narrow the buffer to the current boxquote."
  (interactive)
  (let ((box (boxquote-points-with-check)))
    (narrow-to-region (car box) (cdr box))))

;;;###autoload
(defun boxquote-kill ()
  "Kill the boxquote and its contents."
  (interactive)
  (let ((box (boxquote-points-with-check)))
    (kill-region (car box) (1+ (cdr box)))))

;;;###autoload
(defun boxquote-unbox-region (start end)
  "Remove a box created with `boxquote-region'."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (setf (point) (point-min))
      (if (looking-at (concat "^" (regexp-quote boxquote-top-corner)
                              (regexp-quote boxquote-top-and-tail)))
          (let ((ends (concat "^[" (regexp-quote boxquote-top-corner)
                              (regexp-quote boxquote-bottom-corner)
                              "]" boxquote-top-and-tail))
                (lines (concat "^" (regexp-quote boxquote-side))))
            (loop while (< (point) (point-max))
                  if (looking-at ends)  do (kill-line t)
                  if (looking-at lines) do (delete-char 2)
                  do (forward-line)))
        (error "I can't see a box here")))))

;;;###autoload
(defun boxquote-unbox ()
  "Remove the boxquote that contains `point'."
  (interactive)
  (let ((box (boxquote-points-with-check)))
    (boxquote-unbox-region (car box) (1+ (cdr box)))))

(provide 'boxquote)

;;; boxquote.el ends here.
