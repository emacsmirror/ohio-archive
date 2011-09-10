;;; find-file.el --- find a file corresponding to this one given a pattern

;; Copyright (C) 1994 Henry Guillaume

;; Author: Henry Guillaume <henry@qbd.com.au>
;; Keywords: local, c
;; Version: 2.7

;; LCD Archive Entry:
;; find-file|Henry Guillaume|henryg@qbd.com.au|
;; Find a file associated with this buffer (from a .cc file to a .hh file).|
;; 29-Sep-1994|2.7|~/misc/find-file.el.Z|

;;; This file is not part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;;
;; This package features a function called ff-find-other-file, which I bind
;; to ^C-o, and it performs the following function:
;;
;;     When in a .cc file, it finds the corresponding .hh file in what
;;     it thinks is the correct directory and displays it; it performs
;;     in a similar way when in a .hh file, to find the .cc file.
;;
;; Many people maintain their include file in a directory separate to their
;; src directory, and very often you may be editing a file and have a need to
;; visit the "other file". This package searches through a set of directories
;; to find that file.
;;
;; The "other file", or "corresponding file" has the same basename, and will
;; have an extension as described by the ff-other-file-alist variable.
;;
;;   '(("\\.cc$"  (".hh" ".h"))
;;     ("\\.hh$"  (".cc" ".C" ".CC" ".cxx" ".cpp")))
;;
;; If the current file has a .cc extension, ff-find-other-file will attempt
;; to look for a .hh file, and then a .h file in some directory as described
;; below.
;;
;; Searching is carried out in a set of directories specified by the
;; ff-search-directories variable:
;;
;;     ("." "../../src" "../include/*" "/usr/local/*/src/*" "$PROJECT/src")
;;
;; This means that the "corresponding file" will be searched for first in
;; the current directory, then in ../../src, then in one of the directories
;; under ../include, and so on. The star is _not_ a general wildcard
;; character: it just indicates that the subdirectories of this directory
;; must each be searched in turn. Environment variables will be expanded in
;; the ff-search-directories variable.
;;
;; If the point is on a #include line, the file to be #included is searched
;; for in the same manner. This can be disabled by calling ff-get-other-file
;; instead of ff-find-other-file.
;;
;; If the file was not found, ff-find-other-file will prompt you for where
;; to create the new "corresponding file" (defaults to the current directory),
;; unless the variable ff-always-try-to-create is set to nil.
;;
;; If given an argument (with the ^U prefix), ff-find-other-file will
;; get the other file in another (the other?) window (see the function
;; find-file-other-window and switch-to-buffer-other-window). This can be
;; set on a more permanenet basis by setting ff-always-in-other-window to t
;; in which case the ^U prefix will do the opposite of what was described
;; above.
;;
;; The variables can be customised on an individual basis, and extended to
;; include other extensions...

;; Installation:
;;
;; To use, autoload in your ~/.emacs the specific functions you require:
;;
;; (require 'find-file)
;;
;; Alternatively,
;;
;; (autoload 'ff-find-other-file "find-file" nil t)
;; (autoload 'ff-get-other-file "find-file"  nil t)
;;
;; In both cases:
;;
;; (define-key global-map "\C-co" 'ff-find-other-file)
;;
;; or substitute ff-get-other-file for ff-find-other-file, depending on
;; your preference.
;;
;; To use the mouse, you might define something like this:
;;
;; in Emacs-19:
;;
;; (define-key global-map [C-mouse-2] 'ff-mouse-find-other-file)
;; (define-key global-map [S-C-mouse-2] 'ff-mouse-find-other-file-other-window)
;;
;; in Lucid Emacs-9.10:
;;
;; (define-key global-map [(control button3)] 'ff-mouse-find-other-file)
;; (define-key global-map [(shift-control button3)] 'ff-mouse-find-other-file-other-window)
;;

;; Feedback:
;;
;; This is hand written software.  Use it at your own risk.
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.

;; History:
;;
;; - 2.7   incorporated changes to ff-* variables suggested by Fritz Knabe 
;;         <Fritz.Knabe@ecrc.de>, to allow having a c-mode-hook that sets
;;         the ff-search-directories and ff-other-file-alist variables 
;;         properly *without* find-file already being loaded.
;;
;;         This of course means that you should not be setting these ff-*
;;         variables directly, but setting the cc- and ada- equivalents...
;;
;;         Added error handling in case directories cannot be accessed.
;;
;; - 2.6   made ff-other-file-alist & ff-search-directories buffer-local,
;;         so they aren't clobbered every time a new major mode sets them
;;         (with thanks to Fritz Knabe <Fritz.Knabe@ecrc.de>)
;;
;; - 2.5   fixed locate-file bug (wrong argument positions)
;;
;;         enhanced ff-all-dirs-under to take a list of dirs to ignore
;;
;; - 2.4   added support for 'import' construct for NeXTSTEP programmers
;;         (with thanks to Robert Lang <rrl@fnbc.com>)
;;
;;         defined locate-file for GNU Emacs-19 (as opposed to Lucid Emacs)
;;
;;         improved performance by skipping non-existent directories
;;
;;         fixed bug when deciding on #include line
;;
;; - 2.3   improved speed by passing extensions to ff-get-file instead
;;         of handling them in ff-find-other-file. Idea borrowed from
;;         Lucid Emacs' locate-file.
;;
;; - 2.2   added mouse support for Lucid Emacs (prompting from Heddy Boubaker
;;         <boubaker@cenatls.cena.dgac.fr>)
;;
;;         remove message when no matching file is found
;;
;; - 2.1   bug fix: case-fold-search was set in one buffer and reset elsewhere
;;
;; - 2.0   express associaton of files using regexps instead of extensions: 
;;         ("\\(.+\\)\\.cc$" ("\\1.hh" "\\1.h")) - this allows better control
;;         of file association, such as having file.h connected with Mfile.c
;;         (comment from Vasco Lopes Paulo <vlp@turbina.inesc.pt>). Of course,
;;         this will allow ("\\.cc$" (".hh" ".h")) [see auto-mode-alist].
;;
;;         code for ff-string-replace-match and ff-expand-newtext borrowed from
;;         dired-string-replace-match and dired-expand-newtext in dired-aux.el
;;         by Sebastian Kremer <sk@thp.uni-koeln.de>.
;;
;;         incorporated mouse support (based on code in dired-mouse-find-file-
;;         other-window dired.el by Sebastian Kremer <sk@thp.uni-koeln.de>).
;;
;;         variables ff-quiet-mode and ff-case-fold-search added.
;;
;; - 1.6   added ff-get-other-file: gets the file's other file, after a request
;;         from Kevin Pereira <kevin@tusc.com.au>
;;
;; - 1.5   inserted patches from Rolf Ebert <a2c0102@sunmail.lrz-muenchen.de>
;;         for Ada and Modula-2 support.
;;
;;         renamed cc- to ff- where appropriate in an effort to generalise the
;;         package. Provided backward compatibility function cc-find-other-file
;;
;; - 1.4   incorporated Trevor West's <cst@goanna.cs.rmit.oz.au> pet hate: to
;;         be able to switch off automatic creation of the other file with ff-
;;         always-try-to-create (defaults to t).
;;
;;         fixed handling of file-less buffers.
;;
;; - 1.3   used assoc rather than memq when determining whether the extension
;;         required was defined or not.
;;
;; - 1.2   correct initialisation of ff-other-file-alist and ff-search-
;;         directories
;;
;;         fixed prompting for file when no extension existed
;;
;;         added argument to ff-find-other-file to select file in another 
;;         window
;;
;; - 1.1   extensions other than the first were not being considered - fixed
;;
;;         look for buffer before searching for the file (idea and a bit of 
;;         code borrowed from Mark A. Plaksin <mplaksin@ai.uga.edu>)
;;
;; - 1.0   initial version.

;;
;; ToDo
;;
;; I'd like to have a means of generating the name of the other file via a
;; function, which by default would just take the car of each element of
;; ff-other-file-alist and return each element of the cdr, but could be
;; overridden so that more complex changes would be possible. We have a C++
;; coding standard which, for various reasons (grrr...), specifies that the
;; .cc file have a prefix, but not the .hh file. So we want a relationship
;; between
;;
;;             src/fooZap.cc  and  include/FOO/zap.hh 
;;
;; which is just about impossible to determine with a regexp...
;;
;; It would allow the means to remove certain characters from filenames.
;; Heddy Boubaker's <boubaker@cenatls.cena.dgac.fr> excellent mode-compile
;; package specifies a regexp to allow conversion of a .hh file to a .cc by
;; removing characters (zap_p.h and zap.h both yield zap.c), but the
;; mechanism he uses only allows one way conversion (namely removal of
;; characters).
;;
;; Of course the default function would mean that nothing would change to
;; the behaviour of this package as it stands now (v2.7).
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ff-case-fold-search nil
  "Non-nil means ignore cases in matches (see case-fold-search).
If you have extensions in different cases, you will want this to be nil.")

(defvar ff-always-in-other-window nil
  "If non-nil, always open the other file in another window, unless an
argument is given to ff-find-other-file.")

(defvar ff-always-try-to-create t
  "If non-nil, always attempt to create the other file if it was not found.")

(defvar ff-quiet-mode nil
  "If non-nil, traces which directories are being searched.")

(defvar cc-other-file-alist
  '(
    ("\\.cc$"  (".hh" ".h"))
    ("\\.hh$"  (".cc" ".C" ".CC" ".cxx" ".cpp"))

    ("\\.c$"   (".h"))
    ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

    ("\\.C$"   (".H"  ".hh" ".h"))
    ("\\.H$"   (".C"  ".CC"))

    ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
    ("\\.HH$"  (".CC"))

    ("\\.cxx$" (".hh" ".h"))
    ("\\.cpp$" (".hh" ".h"))
    )
  "Alist of extensions to find given the current file's extension.

This list should contain the most used extensions before the others,
since the search algorithm searches sequentially through each directory
specified in ff-search-directories. If a file is not found, a new one
is created with the first matching extension (.cc yields .hh).
")

(defvar ada-other-file-alist
  '(
    ("\\.ads$" (".adb")) ;; Ada specs and bodies
    ("\\.adb$" (".ads")) ;; GNAT filename conventions
    )
  "Alist of extensions to find given the current file's extension.

This list should contain the most used extensions before the others,
since the search algorithm searches sequentially through each directory
specified in ada-search-directories. If a file is not found, a new one
is created with the first matching extension (.adb yields .ads).
")

(defvar modula2-other-file-alist
  '(
    ("\\.mi$" (".md")) ;; Modula-2 module definition
    ("\\.md$" (".mi")) ;; and implementation.
    )
  )

(defvar ff-other-file-alist 'cc-other-file-alist
  "*Alist of extensions to find given the current file's extension.

This list should contain the most used extensions before the others,
since the search algorithm searches sequentially through each
directory specified in ff-search-directories. If a file is not found,
a new one is created with the first matching extension (.cc yields .hh).
This alist should be set by the major-mode.")

(make-variable-buffer-local 'ff-other-file-alist)

(defvar cc-search-directories
  '("." "/usr/include/*" "/usr/local/include/*"))

(defvar ada-search-directories
  '("." "/usr/adainclude" "/usr/local/adainclude"))

(defvar ff-search-directories 'cc-search-directories
  "List of directories to search for a specific file.

This list is searched through with each extension specified in
ff-other-file-alist that matches this file's extension. So the
longer the list, the longer it'll take to realise that a file
may not exist.

Environment variables can be inserted between slashes ('/').
They will be replaced by their definition. If a variable does
not exist, it will (silently) be replaced with an empty string.

The stars are _not_ wildcards: they are searched for together with
the preceding slash. The star represents all the subdirectories except
'..', and each of these subdirectories will be searched in turn.
")

(make-variable-buffer-local 'ff-search-directories)

;;;### autoload
(autoload 'ada-make-filename-from-adaname "ada-mode"
  "determine the filename of a package/procedure from its own Ada name.")

(defun ff-string-match (regexp string &optional start)
  (let ((exact-match case-fold-search)
        match)
    (setq case-fold-search ff-case-fold-search)
    (setq match (string-match regexp string start))
    (setq case-fold-search exact-match)
    (setq match match)))

(defun ff-string-replace-match (regexp string newtext &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If it does not match, nil is returned instead of the new string.
Optional arg LITERAL means to take NEWTEXT literally.
Optional arg GLOBAL means to replace all matches."
  (if global
      (let ((result "") (start 0) mb me)
        (while (ff-string-match regexp string start)
          (setq mb (match-beginning 0)
                me (match-end 0)
                result (concat result
                               (substring string start mb)
                               (if literal
                                   newtext
                                 (ff-expand-newtext string newtext)))
                start me))
        (if mb          ; matched at least once
            (concat result (substring string start))
          nil))
    ;; not GLOBAL
    (if (not (ff-string-match regexp string 0))
        nil
      (concat (substring string 0 (match-beginning 0))
              (if literal newtext (ff-expand-newtext string newtext))
              (substring string (match-end 0))))))

(defun ff-expand-newtext (string newtext)
  ;; Expand \& and \1..\9 (referring to STRING) in NEWTEXT, using match data.
  ;; Note that in Emacs 18 match data are clipped to current buffer
  ;; size...so the buffer should better not be smaller than STRING.
  (let ((pos 0)
        (len (length newtext))
        (expanded-newtext ""))
    (while (< pos len)
      (setq expanded-newtext
            (concat expanded-newtext
                    (let ((c (aref newtext pos)))
                      (if (= ?\\ c)
                          (cond
                           ((= ?\& (setq c (aref newtext (setq pos (1+ pos)))))
                            (substring string
                                       (match-beginning 0)
                                       (match-end 0)))
                           ((and (>= c ?1) (<= c ?9))
                            ;; return empty string if N'th
                            ;; sub-regexp did not match:
                            (let ((n (- c ?0)))
                              (if (match-beginning n)
                                  (substring string
                                             (match-beginning n)
                                             (match-end n))
                                "")))
                           (t
                            (char-to-string c)))
                        (char-to-string c)))))
      (setq pos (1+ pos)))
    expanded-newtext))

(defun ff-list-replace-env-vars (search-list)
  "Get rid of environment variables in SEARCH-LIST"
  (let (list
        (var (car search-list)))
    (while search-list
      (if (string-match "\\(.*\\)\\$[({]*\\([a-zA-Z0-9_]+\\)[)}]*\\(.*\\)" var)
          (setq var
                (concat
                 (substring var (match-beginning 1) (match-end 1))
                 (getenv (substring var (match-beginning 2) (match-end 2)))
                 (substring var (match-beginning 3) (match-end 3)))))
      (setq search-list (cdr search-list))
      (setq list (cons var list))
      (setq var (car search-list)))
    (setq search-list (reverse list))))

(defun ff-get-other-file (&optional in-other-window)
  "Find the corresponding header or source file to this source or header
file. See also ff-find-other-file.

If optional IN-OTHER-WINDOW is non-nil, finds the file in the other window.

Arguments: (&optional in-other-window)
"
  (interactive "P")
  (ff-find-other-file in-other-window t))

(defun ff-find-other-file (&optional in-other-window ignore-include)
  "Find the corresponding header or source file to this source or header
file; being on a #include line pulls in that file.

If optional IN-OTHER-WINDOW is non-nil, finds the file in the other window.
If optional IGNORE-INCLUDE is non-nil, ignores being on #include lines.

Arguments: (&optional in-other-window ignore-include)
"
  (interactive "P")

  (let (match         ; matching regexp for this file
        suffixes      ; set of replacing regexps for the matching regexp
        fname         ; basename of this file
        stub          ; name of the file without extension
        alist         ; working copy of the list of file extensions
        pathname      ; the pathname of the file or the #include line
        default-name  ; file we should create if none found
        format        ; what we have to match    
        found         ; whether we found the file
        dirs          ; local value of ff-search-directories
        no-match)     ; whether we know about this kind of file

    (setq dirs
          (ff-list-replace-env-vars (symbol-value ff-search-directories)))

    (save-excursion
      (beginning-of-line 1)
      (cond

       ;; if we are looking at a #include line, we get that file and its dir
       ((and (not ignore-include)
             (looking-at 
              "^\#\\s *\\(include\\|import\\)\\s +[<\"]\\(.*\\)[>\"]"))
        (setq fname (buffer-substring (match-beginning 2) (match-end 2)))
        (setq default-name fname)
        (setq found 
              (ff-get-file dirs fname nil in-other-window)))

       ;; are we looking at some ada construct
       ((looking-at "^with[ \t]+\\([a-zA-Z0-9_\\.]+\\)")
        (setq fname (buffer-substring (match-beginning 1) (match-end 1)))
        (setq fname (concat (ada-make-filename-from-adaname fname)
			    ada-spec-suffix))
        (setq default-name fname)
        (setq found 
              (ff-get-file dirs fname nil in-other-window)))

       ;; let's just get the corresponding file
       (t
        (message "Working...")

        (setq alist    (symbol-value ff-other-file-alist)
              pathname (if (buffer-file-name)
                           (buffer-file-name)
                         "/none.none"))

        (string-match ".*/\\(.+\\)$" pathname)
        (setq fname (substring pathname (match-beginning 1) (match-end 1))
              no-match nil
              match (car alist))

        ;; find the table entry corresponding to this file
        (while (and match (not (ff-string-match (car match) fname)))
          (setq alist (cdr alist))
          (setq match (car alist)))

        ;; no point going on if we haven't found anything
        (if (not match)
            (setq no-match t)

          ;; otherwise, suffixes contains what we need
          (setq suffixes (car (cdr match))
                found nil)

          ;; build our filename stub
        (setq format (concat "\\(.+\\)" (car match)))
        (string-match format fname)
        (setq stub (substring fname (match-beginning 1) (match-end 1)))

        ;; if we find nothing, we should try to get a file like this one
        (setq default-name
              (concat stub (car (car (cdr match)))))

        ;; do the real work - find the file
        (setq found 
              (ff-get-file dirs
                           stub
                           suffixes 
                           in-other-window)))))

      ;; if we haven't found anything at all but we should have because
      ;; the extension is known to us in the ff-other-file-alist
      (if (and (not found) (not no-match) ff-always-try-to-create)
          (progn
            (let (name
                  pathname
                  (pwd (substring (pwd) 10 (length (pwd)))))

              (setq name
                    (expand-file-name
                     (read-file-name
                      (format "Find or create %s in: " default-name)
                      pwd default-name nil)))

              (setq pathname
                    (if (file-directory-p name)
                        (concat (file-name-as-directory
                                 name) default-name) (setq found t)
                                 name))

              (ff-find-file pathname in-other-window))))

      ;; we didn't find anything at all of interest
      (if (and (not no-match) (not ff-always-try-to-create) (not found))
          (message "no file found for %s" fname))

      (if no-match
          (message "")))))

(defun ff-get-file (search-dirs fname-stub &optional suffix-list other-window)
  "Find a file in the SEARCH-DIRS with the given FNAME. If SUFFIXES is nil, 
search for fname, otherwise search for fname with each of the given suffixes.
If IN-OTHER-WINDOW is non-nil, finds the file in the other window.

Arguments: (search-dirs fname-stub &optional suffix-list in-other-window)
"
  (let* (dirs                   ; working copy of dirs to search
         dir                    ; the current dir considered
         file                   ; filename being looked for
         rest                   ; pathname after first /*
         this-suffix            ; the suffix we are currently considering
         suffixes               ; working copy of suffix-list
         filename               ; built filename
         (found nil))           ; whether we have found anything

    (setq dirs search-dirs)
    (setq dir  (car dirs))
    (while (and (not found) dirs)

      (setq suffixes suffix-list)

      ;; if dir does not contain '/*', look for the file
      (if (not (string-match "\\([^*]*\\)/\\\*\\(/.*\\)*" dir))
          (progn 
            
            ;; suffixes is nil => fname-stub is the file we are looking for
            ;; otherwise fname-stub is a stub, and we append a suffix
            (if suffixes
                (setq this-suffix (car suffixes))
              (setq this-suffix "")
              (setq suffixes (list "")))
            
            (while (and suffixes (not found))

              (setq filename (concat fname-stub this-suffix))
              (setq file (concat dir "/" filename))
              
              (if (not ff-quiet-mode)
                  (message "finding %s..." file))

              (cond 
               ((bufferp (get-buffer filename))
                (ff-switch-to-buffer filename other-window)
                (message "")
                (setq found t))
               
               ((file-exists-p file)
                (ff-find-file file other-window)
                (message "%s found" file)
                (setq found t)))
              
              (setq suffixes (cdr suffixes))
              (setq this-suffix (car suffixes))))

        ;; otherwise dir matches the '/*', so search each dir separately
        (progn
          (if (match-beginning 2)
              (setq rest (substring dir (match-beginning 2) (match-end 2)))
            (setq rest "")
            )
          (setq dir  (substring dir (match-beginning 1) (match-end 1)))

          (let ((dirlist (ff-all-dirs-under dir '("..")))
                this-dir compl-dirs)

            (setq this-dir (car dirlist))
            (while dirlist
              (setq compl-dirs
                    (append
                     compl-dirs
                     (list (concat this-dir rest))
                     ))
              (setq dirlist  (cdr dirlist))
              (setq this-dir (car dirlist)))

            (if compl-dirs
                (setq found (ff-get-file compl-dirs
                                         fname-stub
                                         suffix-list
                                         other-window))))))
      (setq dirs (cdr dirs))
      (setq dir (car dirs)))

    (setq found found)))

(defun ff-basename (string)
  "Returns the basename of FILENAME"
  (interactive "sPathname: ")
  (setq string (concat "/" string))
  (string-match ".*/\\([^/]+\\)$" string)
  (setq string (substring string (match-beginning 1) (match-end 1)))
)

(defun ff-all-dirs-under (here &optional exclude)
  "Get all the directory files under the given directory.
Exclude all files in the optional EXCLUDE list.

Arguments: (here &optional exclude)
"
  (if (file-directory-p here)
      (condition-case nil
          (progn
            (let ((files (directory-files here t))
                  (dirlist (list))
                  file)
              (while files
                (setq file (car files))
                (if (and
                     (file-directory-p file)
                     (not (member (basename file) exclude)))
                    (setq dirlist (cons file dirlist)))
                (setq files (cdr files)))
              (setq dirlist (reverse dirlist))))
        (error nil))
    nil))

(defun ff-xor (a b)
  (or (and a (not b)) (and b (not a))))

(defun ff-find-file (file &optional in-other-window)
  (if (or
       (and in-other-window (not ff-always-in-other-window))
       (and (not in-other-window) ff-always-in-other-window))
      (find-file-other-window file)
    (find-file file)))

(defun ff-switch-to-buffer (file &optional in-other-window)
  (if (or
       (and in-other-window (not ff-always-in-other-window))
       (and (not in-other-window) ff-always-in-other-window))
      (switch-to-buffer-other-window file)
    (switch-to-buffer file)))

(defun ff-lucid-emacs ()
  (string-match "Lucid" emacs-version))

(defun ff-gnu-emacs-19 ()
  (string-match "19\\.2[0-9]\\.[0-9]" emacs-version))

(cond 
 ((ff-gnu-emacs-19)
  (defun ff-goto-click (event)
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event))))

  (defun ff-mouse-find-other-file (event)
    "Visit the file you click on."
    (interactive "e")
    (save-excursion
      (ff-goto-click event)
      (ff-find-other-file)))

  (defun ff-mouse-find-other-file-other-window (event)
    "Visit the file you click on."
    (interactive "e")
    (save-excursion
      (ff-goto-click event)
      (ff-find-other-file t)))

  (defun locate-file (fname dirs &optional suffix-list ignore-perms)
    "Defines Lucid-Emacs look-alike locate-file for GNU Emacs-19."
    (ff-get-file dirs fname suffix-list))
  )

 ((ff-lucid-emacs)
  (defun ff-mouse-find-other-file (event)
    "Visit the file you click on."
    (interactive "@e")
    (save-excursion
      (mouse-set-point event)
      (ff-find-other-file)))

  (defun ff-mouse-find-other-file-other-window (event)
    "Visit the file you click on."
    (interactive "@e")
    (save-excursion
      (mouse-set-point event)
      (ff-find-other-file t)))
  )
 )

(defun cc-find-other-file (&optional in-other-window)
  "This function is provided for backward compatibility only.
See the documentation for ff-find-other-file."
  (interactive "P")
  (ff-find-other-file in-other-window))

(provide 'find-file)

;; find-file.el ends here
