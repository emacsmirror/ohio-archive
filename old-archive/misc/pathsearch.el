;;; pathsearch.el --- find a file on a specified path (esp. C include files)

;; Copyright (C) 1993 Christopher J. Madsen

;; Author: Christopher J. Madsen <ac608@yfn.ysu.edu>
;; Created: 05 Nov 1993
;; Version: 1.6 (1996/01/18 18:48:59)
;; Keywords: c, languages, tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; pathsearch|Christopher J. Madsen|ac608@yfn.ysu.edu|
;; Find a file on a specified path (esp. C include files)|
;; 18-Jan-1996|1.6|~/misc/pathsearch.el.gz|

;;; Commentary:
;;
;;    This is one of the first packages of Lisp I've written, and the
;;    first I've released, so don't use this as an example of great
;;    coding, because it isn't.  I wrote this code because I couldn't
;;    find any existing packages that perform the same task.  I've
;;    only tested it on my machine, but I'm releasing it because I
;;    hope someone may find it useful.  While I don't promise to fix
;;    any bugs, I am interested in bug reports or patches (who knows,
;;    I might even fix them).
;;
;;    To use pathsearch, put the following in your .emacs file:
;;     (autoload 'pathsearch-c-include-file "pathsearch"
;;       "Find C include file" t)
;;
;;    If you want to use pathsearch-for-file in your own Lisp
;;    programs, you can add the line:
;;     (autoload 'pathsearch-for-file "pathsearch")
;;    to your .emacs as well.  (Or just put `(require 'pathsearch)' in
;;    your Lisp code.)

;;; Code:

;; Read the comments before pathsearch-for-file for more about the
;; format of pathsearch-c-include-path.

(defvar pathsearch-c-include-path '("/usr/include" ".")
  "List of directories where C include files may be found.

This should be either a list of directory names, or a
symbol.  In the latter case, the value of the symbol should
be a list of directory names.  This is so you can set it
easily in a local variables list.")


;; This is the function that actually searches the path and finds the file.
;; It is in no way specific to C.  This is the general entry point for
;; Lisp programs.
;;
;; The PATH parameter should be a list of directory names.  It can
;; also be a symbol whose value is a list.  I did this because I have
;; two C compilers with different include files.  I wanted to be able
;; to set the include path for a file using the local variables list.
;; I didn't want to store the whole include path in the file, just
;; something like
;;    /* pathsearch-c-include-path: my-include-path */
;; but that does (setq pathsearch-c-include-path 'my-include-path), not
;; (setq pathsearch-c-include-path my-include-path).  So I changed the
;; function.

(defun pathsearch-for-file (filename path)
  "Find the first instance of FILENAME in PATH.

PATH can be either a list of directory names or a symbol.
In the latter case the value of the symbol should be a list
of directory names.

Returns the complete pathname of the loaded file.  Returns
nil if FILENAME is not found in PATH."
  ;; Convert a symbol to its value as a list:
  (if (symbolp path)
      (setq path (symbol-value path)))
  (catch 'pathsearch-found-file
    (let (directory)
      (while (setq directory (car path))
        (setq path (cdr path))
        (let ((fn (concat directory "/" filename)))
          (if (file-readable-p fn)
              (progn
                (find-file fn)
                (throw 'pathsearch-found-file
                       buffer-file-name)))))) ;We found it!
    nil))                                     ;We didn't find the file

;; This function grabs the filename out of an `#include' statement and
;; calls pathsearch-for-file to load it.
;;
;; See the comments before pathsearch-for-file for more about the format
;; of pathsearch-c-include-path.

(defun pathsearch-c-include-file (&optional prompt-for-name)
  "Load the file included by the current line into a buffer.

If the line says `#include <file.h>' then look for file.h in
the directories specified in pathsearch-c-include-path.

If the line says `#include \"file.h\"' then look for file.h
in the current directory.

If used with prefix arg, or if there is no `#include' on the current
line, prompt for a filename using the minibuffer and look for that
file along pathsearch-c-include-path."

  (interactive "P")
  (let ((term-char) (filename))
    (if (or prompt-for-name
            (catch 'get-file-name
              ;; Extract filename from current line:
              (save-excursion           ;Don't move point in the current buffer
                ;; Make sure this line is an #include:
                (beginning-of-line)
                (if (not (looking-at "^\\s-*#\\s-*include"))
                    (throw 'get-file-name t)) ;Need to read filename
                ;; Move cursor to beginning of filename:
                (forward-word 2)
                (backward-word 1)
                (skip-chars-backward "_!")
                ;; Find out what kind of #include this is:
                (cond
                 ((equal (preceding-char) ?\") (setq term-char "\""))
                 ((equal (preceding-char) ?\<) (setq term-char ">"))
                 (t (throw 'get-file-name t))) ;Need to read filename
                ;; Grab the filename:
                (let ((beg (point)))
                  (if (search-forward term-char nil t)
                      (setq filename (buffer-substring beg (- (point) 1)))
                    (throw 'get-file-name t)))) ;Need to read filename
              nil))                     ;Filename was read successfully
        ;; Read filename using minibuffer:
        (progn
          (setq filename (read-from-minibuffer "Find C include file: "))
          (setq term-char ">")))        ;Look on pathsearch-c-include-path.

    ;; Search for the file in the appropriate path:
    (let ((found-name (if (equal term-char ">")
                          (pathsearch-for-file filename
                                               pathsearch-c-include-path)
                        (pathsearch-for-file filename '(".")))))
      (if found-name
          (message "Found %s" found-name)
        (error "Unable to find %s" filename)))))

(provide 'pathsearch)

;;; pathsearch.el ends here
