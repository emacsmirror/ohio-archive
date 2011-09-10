;;; mkhtml.el ---  Create HTML with links.  (Extensions to `htmlize.el'.)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Lisp Archive Entry
;; Package: mkhtml
;; Filename: mkhtml.el
;; Author: Drew Adams
;; Maintainer: Drew Adams 
;; Created: Tue Jul 18 13:11:51 2000
;; Version: $Id $
;; Last-Updated: August 17, 2000
;; Keywords: tools, hypermedia, www, info
;; Description: Create HTML with links.  (Extensions to `htmlize.el'.) 
;; Compatibility: Emacs 20.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (C) 2000-2001, Drew Adams, all rights reserved.
;;   Last modified on: Fri Jan  5 11:20:03 2001
;;   Update count: 2155
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Extensions to `htmlize.el', by Hrvoje Niksic (hniksic@xemacs.org).
;;
;; Hrvoje Niksic's `htmlize.el' creates HTML files from fontified
;; buffers, but it does not attempt to create any HTML links. This
;; file extends `htmlize.el' by trying to do that in a couple of
;; specific cases, interpreting mouse-face'd text in Info and Dired
;; buffers as links. A version (current, as of this writing) of
;; `htmlize.el' is provided with the MKHTML package.  You can get the
;; latest version of `htmlize.el' from Niksic's web site at
;; `http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el', or perhaps by
;; email (hniksic@xemacs.org).
;;
;; Somewhat related functionality is available with `info2www'.  This
;; is a shell script that converts Emacs Info files on the fly to
;; HTML.  See: `http://www-flash.stanford.edu/info2www/info2www.html'.
;;
;; See also the makeinfo utility (Note: texi2html is now obsolete,
;; replaced by Makeinfo).  Like mkhtml, makeinfo can convert entire
;; Info files to HTML.  The result of converting an Info file with
;; makeinfo is a single HTML file.  The result of converting an Info
;; file with `mkhtml-file' is an HTML file for each node in the Info
;; file.  To produce a single HTML file with multiple Info nodes using
;; mkhtml, you need to first merge the nodes with the command
;; `Info-merge-subnodes' (see file `info+.el').
;;
;; -------------------------------------------------------------------
;; Main new functions defined here:
;;
;;    `mkhtml-any-buffer', `mkhtml-convert-std-filename',
;;    `mkhtml-dired', `mkhtml-dired-files', `mkhtml-dired-link-fn',
;;    `mkhtml-file', `mkhtml-file-menu-item', `mkhtml-files',
;;    `mkhtml-get-basefont-size', `mkhtml-get-css-font-size',
;;    `mkhtml-get-font-family', `mkhtml-info-buffer',
;;    `mkhtml-info-link-fn', `mkhtml-insert-header',
;;    `mkhtml-make-HTML-file-name', `mkhtml-merged-info-buffer',
;;    `mkhtml-merged-info-link-fn', `mkhtml-non-file-menu-item',
;;    `mkhtml-particular-buffer', `mkhtml-plain-buffer',
;;    `mkhtml-plain-link-fn', `mkhtml-save-buffer'.
;; 
;; New user options (variables) defined here:
;;
;;    `mkhtml-dir-sep-replacement', `mkhtml-file-node-sep',
;;    `mkhtml-font-size', `mkhtml-hover-link-bg-color',
;;    `mkhtml-hover-link-fg-color', `mkhtml-link-color',
;;    `mkhtml-preserve-fonts', `mkhtml-visited-link-color'.
;;
;; Other variable defined here: `mkhtml-version'.
;;
;;
;;  ***** NOTE: The following functions defined in `htmlize.el' have
;;              been REDEFINED HERE:
;;
;;  `htmlize-css-specs' - Adds font-family and font-size too.
;;  `htmlize-css-insert-head' - Adds link color too.
;;  `htmlize-font-body-tag' - Adds  BASEFONT size too.
;; -------------------------------------------------------------------
;;
;;
;; COMMANDS
;; 
;; The main commands defined here are `mkhtml-any-buffer',
;; `mkhtml-dired-files' and `mkhtml-file'.  These all try to analyze
;; the context (Dired, Info, merged Info) of the buffer or file(s) to
;; determine how to create appropriate HTML links. The most powerful
;; of the commands here is `mkhtml-any-buffer'.
;;
;; If you know the context, you can alternatively use directly one of
;; the individual commands `mkhtml-dired', `mkhtml-info-buffer',
;; `mkhtml-merged-info-buffer', or `mkhtml-plain-buffer'.  Each of
;; these uses the generic function `mkhtml-particular-buffer'. You can
;; also use `mkhtml-particular-buffer' to create your own function for
;; creating HTML links in another special context.
;;
;; Loading this file adds to the `Info' menu-bar menu (in Info mode).
;;
;;
;; RELATED FILES
;;
;; The MKHTML package contains files `mkhtml.el', `def-face-const.el',
;; `htmlize.el', `info+.el', `setup-info.el', `strings.el', and
;; `thingatpt+.el'.  Only `htmlize.el' is required, but `info+.el' is
;; needed if you want to take advantage of certain functionalities
;; here.  Files `strings.el' and `thingatpt+.el' serve only cosmetic
;; purposes.
;;
;; Commands `mkhtml-merged-info-buffer' and `mkhtml-any-buffer',
;; defined here, are able to convert merged Info buffers, which are
;; created by command `Info-merge-subnodes' (see file `info+.el').  A
;; merged Info buffer contains a merge of an Info node with all of its
;; subnodes (perhaps recursively).  This can be useful for creating
;; larger HTML files containing several nodes (otherwise, a separate
;; HTML file is created for each node).
;;
;; Note that in addition to defining some new functions,file
;; `info+.el' redefines several standard functions defined in file
;; `info.el': `Info-find-emacs-command-nodes', `Info-find-node',
;; `Info-fontify-node', `Info-goto-emacs-command-node',
;; `Info-goto-emacs-key-command-node', `Info-mode' and `Info-search'.
;; You may not need or appreciate all of the "enhancements" in file
;; `info+.el'.  Consult `info+.el' for complete information.
;;
;; File `setup-info.el' can be useful if you want to make HTML
;; versions of large Info indexes such as the Emacs manual Key
;; Index. By default large index menus are not highlighted in Info, so
;; no HTML links are created for them. File `setup-info.el' allows for
;; highlighting larger menus by increasing variable
;; `Info-fontify-maximum-menu-size' to 100000. It also redefines the
;; faces in `Info-title-face-alist'.
;;
;; The order of loading the MKHTML package files should be:
;; `setup-info.el', `info+.el', `mkhtml.el', as follows:
;;
;;      (require 'setup-info) (require 'info+) (require 'mkhtml)
;;
;; File `info+.el' will load `strings.el', and `thingatpt+.el', if
;; available.  It also requires file `def-face-const.el' when it is
;; compiled or interpreted.
;;
;;
;;  The following binding is made here:
;;
;;    menu bar Tools menu        `mkhtml-any-buffer'
;;
;;  The following bindings are made here for Info mode
;;  (these commands are also added to the Info menu-bar menu):
;;
;;    `c'                        `mkhtml-info-buffer'
;;    `w'                        `mkhtml-file'
;;
;;  The command `mkhtml-merged-info-buffer' is added to the Info
;;  menu-bar menu for merged Info buffers.
;;
;;
;;
;;
;;
;; CURRENT LIMITATIONS / POSSIBLE IMPROVEMENTS:
;;
;;   1. On Windows, at least, `htmlize-character-table' (see
;;      `htmlize.el') seems to be incomplete and doesn't work with
;;      some non-ASCII chars.  This means that functions here that use
;;      it have the same problem.  We get an "Args out of range" error
;;      with, for example, the ae-combined ISO Latin 1 character.
;;   2. Perhaps it would be better to use relative paths, not absolute?
;;      Or to make it a user choice?
;;   3. No attempt has been made to support other than GNU Emacs
;;      (e.g. XEmacs).
;;   4. There is no treatment yet of file names in `Note:' (Info cross
;;      references).  For example, `Note: (emacs) Minibuffer::' =>
;;      a link to "#(emacs) Minibuffer" (merged Info buffer) or to
;;      "(emacs) Minibuffer.html" (normal Info buffer).
;;   5. Not yet taking into account `Note: xxx: yyy' (as opposed to
;;      `Note: xxx:: yyy'). Currently this links to xxx, not yyy.
;;   6. For a merged info buffer, there is no way of knowing if a link
;;      is to elsewhere in the same buffer or not. Most such links are
;;      menu items, so they will point to places in the same
;;      buffer. But cross refs (`Note:') may point beyond the buffer.
;;      Those are not treated differently here - all links are
;;      currently made to point to hashes (#) in the same HTML file
;;      derived from the same buffer.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: mkhtml.el,v $
;; RCS Revision 1.7  2001/01/05 19:20:14  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.6  2001/01/03 21:30:14  dadams
;; RCS Version 1.6: Updated Hrvoje Niksic's address.
;; RCS
;; RCS Revision 1.5  2001/01/03 00:57:37  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.4  2000/11/28 20:29:03  dadams
;; RCS Version 1.5:
;; RCS Optional require's via 3rd arg=t now.
;; RCS
;; RCS Revision 1.3  2000/11/21 19:49:24  dadams
;; RCS Version 1.4:
;; RCS 1. Added: htmlize-css-insert-head, mkhtml-insert-header,
;; RCS    mkhtml-hover-link-bg-color, mkhtml-hover-link-fg-color,
;; RCS    mkhtml-link-color, mkhtml-visited-link-color.
;; RCS 2. Use mkhtml-insert-header in: mkhtml-any-buffer, mkhtml-particular-buffer
;; RCS
;; RCS Revision 1.2  2000/11/15 19:03:36  dadams
;; RCS Version 1.3: Bug fix -
;; RCS Generating HTML from Dired buffer using mkhtml-any-buffer:was using all the
;; RCS mouse-faced text as link target; now uses just the file name.
;; RCS
;; RCS Revision 1.1  2000/09/14 17:23:17  dadams
;; RCS Initial revision
;; RCS
;
; 17/08/00 mkhtml-file bug fix: restore `Info-current-node' when done.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl) ;; push, unless, when
(require 'dired) ;; dired-get-filename, dired-get-marked-files,
                 ;; dired-replace-in-string
(require 'info+ nil t) ;; (no error if not found): Info-merged-menu, Info-merged-map
(require 'info) ;; Info-select-node, Info-get-token
(require 'htmlize) ;; htmlize-face-boldp, htmlize-face-hash, htmlize-face-italicp,
                   ;; htmlize-face-rgb-background, htmlize-face-rgb-foreground, 
                   ;; htmlize-faces-in-buffer, htmlize-face-strikep, 
                   ;; htmlize-face-underlinep, htmlize-head-tags, 
                   ;; htmlize-make-face-hash, htmlize-method, htmlize-output-type,
                   ;; htmlize-protect-string, htmlize-symbol-face-p, htmlize-version
(eval-when-compile (require 'easymenu)) ;; easy-menu-do-add-item


(provide 'mkhtml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst mkhtml-version "1.7"
  "This version of the MKHTML package (not just file `mkhtml.el'.")

;; Add command to main menubar menu.
(define-key menu-bar-tools-menu [mkhtml-any-buffer]
  '("HTMLize Buffer" . mkhtml-any-buffer))

;; Add commands to the Info menubar menus.
(when (and (boundp 'Info-mode-menu) Info-mode-menu) ; Defined in `info+.el'
  (define-key Info-mode-map "c" 'mkhtml-info-buffer)
  (define-key Info-mode-map "w" 'mkhtml-file)
  (cond ((lookup-key Info-mode-menu [Edit\ Node]) ; Place before Edit Node.
         (easy-menu-do-add-item Info-mode-menu
                                ["HTMLize Buffer" mkhtml-info-buffer t]
                                "Edit Node")
         (easy-menu-do-add-item Info-mode-menu
                                ["HTMLize Whole Info File" mkhtml-file t]
                                "Edit Node"))
        (t                              ; Place before Exit (else place at end).
         (easy-menu-do-add-item Info-mode-menu
                                ["HTMLize Buffer" mkhtml-info-buffer t]
                                "Exit")
         (easy-menu-do-add-item Info-mode-menu
                                ["HTMLize Whole Info File" mkhtml-file t]
                                "Exit"))))

(when (and (boundp 'Info-merged-menu) Info-merged-menu) ; Defined in `info+.el'
  (define-key Info-merged-map "c" 'mkhtml-buffer)
  (easy-menu-do-add-item Info-merged-menu ; Place before Quit.
                         ["HTMLize Buffer" mkhtml-merged-info-buffer t]
                         "Quit"))


(defvar mkhtml-file-node-sep "-"
  "*String to separate Info file and node names in HTML file names.")

(defvar mkhtml-dir-sep-replacement "--"
  "*String to replace `directory-sep-char' in Info node filenames.")

(defvar mkhtml-link-color "#02F"
  "*Color to use for unvisited hypertext links.")

(defvar mkhtml-visited-link-color "#808"
  "*Color to use for visited hypertext links.")

(defvar mkhtml-hover-link-fg-color "purple"
  "*Color to use for foreground when mouse is over hypertext links.")

(defvar mkhtml-hover-link-bg-color "#AFB"
  "*Color to use for background when mouse is over hypertext links.")

(defsubst mkhtml-file-menu-item ()
  "The file menu item surrounding point, or nil if none.
A file menu item is one that ends in `::', instead of just `:'."
  (Info-get-token (point) "\\* " "\\* \\([^:]*\\)::"))

(defsubst mkhtml-non-file-menu-item ()
  "The non-file menu item surrounding point, or nil if none.
A non-file menu item is one that ends in `:', instead of `::'."
  (Info-get-token (point) "\\* " "\\* [^:]*:[ \t]+\\([^\t,.\n]+\\)[\t,.\n]"))


;; This doesn't lose FILE's extension, like `htmlize-make-file-name'
;; does.  This is useful when, e.g., you have `xxx.cc' and `xxx.hh':
;; you get 2 different result files, `xxx.cc.html' & `xxx.hh.html',
;; instead of just having `xxx.html' be overwritten.
(defun mkhtml-make-HTML-file-name (file dir)
  "Make absolute HTML file name+path.
FILE names the file without suffix \"\.html\". DIR is the directory."
  (expand-file-name (concat (file-name-nondirectory file) ".html")
                    (or dir (file-name-directory file))))

(defun mkhtml-convert-std-filename (filename)
  "Convert FILENAME to an innocuous file name.
This uses `convert-standard-filename', after replacing occurrences of
both `directory-sep-char' and (on Windows only) \"\\\" with 
`mkhtml-dir-sep-replacement'. 

Note: We need to replace \"\\\" here explicitly because even on
Windows `directory-sep-char' is \"/\", not \"\\\"."
  (convert-standard-filename
   (dired-replace-in-string
    (concat "[" (char-to-string directory-sep-char)
            (and (string-match "i386" system-configuration) "\\")
            "]")
    mkhtml-dir-sep-replacement
    filename)))


;;; *** MAIN COMMAND ***
;;; Same as `htmlize-many-files-dired' (see `htmlize.el'), but with:
;;;      1) `mkhtml-file' in place of `htmlize-file'.
;;;      2) interactively, prompts for target HTML-DIR.
;;;      3) HTML-DIR is not optional.
;;;###autoload
(defun mkhtml-dired-files (arg html-dir)
  "HTMLize marked files in a Dired buffer. Mouse-face => HTML links.
Create an HTML file corresponding to each of the marked files.
If (prefix) ARG is an integer, use next ARG files, starting at cursor.
If ARG is otherwise non-nil, use current file (at cursor).
HTML-DIR is the directory where the HTML files are created."
  (interactive
   (list current-prefix-arg
         (read-file-name "Directory for HTML file: " nil default-directory)))
  (mkhtml-files (dired-get-marked-files nil arg) html-dir))


;;; Same as `htmlize-many-files' (see `htmlize.el'), but with:
;;;      1) `mkhtml-file' in place of `htmlize-file'.
;;;      2) interactively, prompts for target HTML-DIR.
;;;      3) HTML-DIR is not optional.
;;;###autoload
(defun mkhtml-files (files html-dir)
  "HTMLize files specified by FILES list. Mouse-face => HTML links.
Create an HTML file corresponding to each of the files in list FILES.
This calls `mkhtml-file' on each of the FILES.
HTML-DIR is the directory where resulting HTML files are created.
Normally, each HTML file is saved to the directory of its source file."
  (interactive
   (list
    (let (file file-list)
      ;; Check for `ommadawn', because checking against nil doesn't do
      ;; what you'd expect.
      (while (not (eq (setq file (read-file-name "HTML-ize file (RET to finish): "
						 (and file-list (file-name-directory
                                                                 (car file-list)))
						 'ommadawn t))
		      'ommadawn))
	(push file file-list))
      file-list)
    (read-file-name "Directory for HTML file: " nil default-directory)))
  (dolist (file files) (mkhtml-file file html-dir)))


;;; *** MAIN COMMAND ***
;;;###autoload
(defun mkhtml-file (file html-dir)
  "Convert FILE to an HTML file in directory HTML-DIR.
\(Source FILE is not altered.)
If FILE is an Info file, then create HTML file for each node in it.
When called interactively, the current buffer is used for FILE."
  (interactive
   (list nil                            ; FILE defined in body, below.
         (read-file-name "Directory for HTML file(s): " nil default-directory)))
  (let ((mode-line-id mode-line-buffer-identification)
        info-file)
    (when file (set-buffer (find-file-noselect file))) ; Visit FILE.
    (setq info-file (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (and (re-search-forward
                              "\n\^_\nFile: \\([^,: \t]+\\),?[ \t]+" nil t)
                             (setq file
                                   (buffer-substring-no-properties
                                    (match-beginning 1) (match-end 1)))))))
    (cond (info-file
           (let ((curr-node Info-current-node))
             (save-excursion
               (save-restriction
                 (widen)                    ; Make sure we do the whole info file.
                 (goto-char (point-min))
                 (while (re-search-forward "\n\^_\nFile: \\([^,: \t]+\\),?[ \t]+" nil t)
                   (Info-select-node)       ; Narrow to just one node. 
                   (mkhtml-any-buffer nil html-dir)
                   (widen))))
             (when curr-node (setq Info-current-node curr-node))) ; Restore origin.
           ;; Reset this, because `Info-select-node' changes it.
           (setq mode-line-buffer-identification mode-line-id))
          (t
           ;; Non-info file.
           (unless file                 ; E.g., interactive call.
             (setq file
                   (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
             (find-file-noselect file))
           (mkhtml-any-buffer (current-buffer) html-dir)))))


(defun mkhtml-insert-header (buffer)
  "Insert HTML file header."
  (insert (htmlize-method doctype) ?\n
          (format "<!-- Created by mkhtml-%s, using htmlize-%s in %s mode. -->\n"
                  mkhtml-version htmlize-version htmlize-output-type))
  (insert "<html>\n  <head>\n    <title>"
          (htmlize-protect-string
           (if (stringp buffer) buffer (buffer-name buffer)))
          "</title>\n" htmlize-head-tags)
  (htmlize-method insert-head)
  (insert "  </head>")
  (insert "\n  " (or (htmlize-method body-tag) "<body>") "\n    <pre>\n"))


;;; *** MAIN COMMAND ***
;;;###autoload
(defun mkhtml-any-buffer (&optional buffer html-dir)
  "HTMLize BUFFER (default: current). Mouse-face => HTML links.
Save buffer as a new HTML file in directory HTML-DIR.

Certain buffer types are treated specially, in that text with
`mouse-face' property is converted to appropriate HTML links.
This is true of Dired, normal Info, and merged Info buffers. 

Dired buffers are treated as by `mkhtml-dired'.

Info buffers are treated as by `mkhtml-info-buffer'.

Merged Info buffers are treated as by `mkhtml-merged-info-buffer'.
A merged Info buffer is one created by `Info-merge-subnodes' (defined
in file `info+.el'), which is a merge of an Info node with all of
its subnodes.

Other buffers are treated as by `mkhtml-plain-buffer': Note that HTML
links resulting from mouse-face'd text there are likely to point
nowhere.

\(Note: Buffers with no mouse-face'd text are treated as by 
`htmlize-buffer'.)"
  (interactive
   (list (buffer-name)
         (read-file-name "Directory for HTML file: " nil default-directory)))
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (run-hooks 'htmlize-before-hook)
    (htmlize-make-face-hash (cons 'default (htmlize-faces-in-buffer))))
  (save-window-excursion
    (let* ((html-buf (get-buffer-create "*html*"))
           next-face-change last-face-change next-mouse-change last-mouse-change
           face mouse-face face-object no-node-p link link-start link-end this-File)
      (set-buffer html-buf)
      (erase-buffer)
      (buffer-disable-undo)
      (mkhtml-insert-header buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (if (setq no-node-p           ; Merged Info or non-Info buffer
                    (not (looking-at "File:[ \t]+\\([^:,\t\n]+\\),?[ \t]*\
Node:[ \t]+[^:,\t\n]+,?")))
              (setq this-File (file-name-nondirectory (or (buffer-file-name)
                                                          (buffer-name))))
            (setq this-File (buffer-substring-no-properties
                             (match-beginning 1) (match-end 1)))) ; Info
          (setq next-face-change (point))
          (setq next-mouse-change (point))
          (while (not (eobp))
            (setq face (get-text-property (point) 'face))
            (setq mouse-face (get-text-property (point) 'mouse-face))
            (setq last-face-change next-face-change)
            (setq last-mouse-change next-mouse-change)
            (setq next-face-change (next-single-property-change
                                    (point) 'face nil (point-max)))
            (setq next-mouse-change (next-single-property-change
                                     (point) 'mouse-face nil (point-max)))
            (setq link-start (point))
            (setq link-end next-mouse-change)
            (when (consp face) (setq face (car face))) ; Choose the first face.
            (when (eq face 'default) (setq face nil))
            ;;`face' property can contain arbitrary stuff.
            (unless (htmlize-symbol-face-p face) (setq face nil))
            (when (or mouse-face
                      (and face (boundp info-file-face) (eq face info-file-face)))
              (cond

               ;; Either sub-node title in merged Info buffer
               ;; or "File:" zone of normal Info buffer. Ignore the latter.
               ((not mouse-face)
                (let ((menu-item (or (mkhtml-non-file-menu-item)
                                     (mkhtml-file-menu-item))))
                  ;; Sub-node title line in merged Info buffer (from
                  ;;`Info-merge-subnodes', defined in `info+.el').
                  ;; Create anchor.
                  (when menu-item
                    (princ (concat "<A name=\"" menu-item "\">") html-buf))))

               ;; mouse-face link in EITHER a merged Info buffer (from
               ;;                           `Info-merge-subnodes', defined in
               ;;                           `info+.el')
               ;;                    OR a non-Info buffer.
               ((and no-node-p (>= (point) last-mouse-change))
                (if (memq major-mode '(dired-mode vc-dired-mode))
                    ;; Dired buffer - cf. `mkhtml-dired-link-fn'.
                    ;; Create external link to this line's file.
                    (princ (concat "<A HREF=\"" (dired-get-filename t) "\">") html-buf)
                  ;; Create internal link (#),
                  (princ (concat "<A HREF=\"#"
                                 (or (mkhtml-non-file-menu-item) ; merged Info buffer
                                     (buffer-substring-no-properties
                                      link-start link-end)) ; non-Dired, non-Info buffer
                                 "\">")
                         html-buf)))
               
               ;; mouse-face link in a normal Info node buffer.
               ;; Create external link.
               ((>= (point) last-mouse-change) ; mouse-face, somewhere in a node
                (let ((non-file-menu-item (mkhtml-non-file-menu-item))
                      node-name)
                  (cond (non-file-menu-item ; menu item that doesn't name a file.
                         (if (string-match "^dir" this-File)
                             (princ (concat "<A HREF=\"" ; this is root menu item.
                                            (mkhtml-make-HTML-file-name
                                             (mkhtml-convert-std-filename
                                              (concat
                                               (substring
                                                non-file-menu-item
                                                1 (1- (length non-file-menu-item)))
                                               "-Top"))
                                             html-dir)
                                            "\">")
                                    html-buf)
                           (princ (concat "<A HREF=\"" ; normal menu item.
                                          (mkhtml-make-HTML-file-name
                                           (mkhtml-convert-std-filename
                                            (concat this-File mkhtml-file-node-sep
                                                    non-file-menu-item))
                                           html-dir)
                                          "\">")
                                  html-buf)))
                        ((string-match "^(dir)" ; link to the root Info node. 
                                       (setq node-name (buffer-substring-no-properties
                                                        link-start link-end)))
                         (princ (concat "<A HREF=\""
                                        (mkhtml-make-HTML-file-name
                                         (convert-standard-filename "dir-Top")
                                         html-dir)
                                        "\">")
                                html-buf))
                        (t              ; other link
                         (princ (concat "<A HREF=\""
                                        (mkhtml-make-HTML-file-name
                                         (mkhtml-convert-std-filename
                                          (concat this-File mkhtml-file-node-sep
                                                  node-name))
                                         html-dir)
                                        "\">")
                                html-buf)))))
               (t)))                    ; Face change inside mouse-face: no-op.
            (when (and face (>= (point) last-face-change))
              (setq face-object (gethash face htmlize-face-hash))
              (princ (htmlize-method face-prejunk face-object) html-buf))
            (princ (htmlize-protect-string
                    (buffer-substring-no-properties
                     (point) (min next-face-change next-mouse-change)))
                   html-buf)
            (when (and face (<= next-face-change next-mouse-change))
              (princ (htmlize-method face-postjunk face-object) html-buf))
            (when (and mouse-face (<= next-mouse-change next-face-change))
              (princ "</A>" html-buf))
            (goto-char (min next-face-change next-mouse-change)))))
      (insert "</pre>\n  </body>\n</html>\n")
      (goto-char (point-min))
      (run-hooks 'htmlize-after-hook)
      (buffer-enable-undo)
      (mkhtml-save-buffer this-File html-dir)
      (clrhash htmlize-face-hash))))    ; Allow next gc to free up used conses.

(defun mkhtml-save-buffer (filename html-dir)
  "Save HTML buffer into file FILENAME in directory HTML-DIR."
  (sgml-mode)
  (let (start end)
    (cond (;; In Info node (html from `Info-mode').
           (and (re-search-forward "[Nn]ode:\\s-*" nil 'move)
                (looking-at "<\\([^/ \n\t>]+\\)")) 
           ;; Skip opening tag <...>.
           (while (looking-at "<\\([^/ \n\t>]+\\)") (forward-list 1)) 
           (setq start (point))
           (re-search-forward "<" nil 'move) ; Closing tag.
           (backward-char 1)
           (setq end (point))
           (write-region
            (point-min) (point-max)     ; Between open & close tags.
            (mkhtml-make-HTML-file-name
             (mkhtml-convert-std-filename
              (concat filename mkhtml-file-node-sep
                      (buffer-substring-no-properties start end)))
             html-dir)))
          (t ;; Not an Info buffer - save it all.                            
           (write-region
            (point-min) (point-max)     ; Between open & close tags
            (mkhtml-make-HTML-file-name (mkhtml-convert-std-filename filename)
                                   html-dir))))))


;;;;;;;;;;;;;;; FUNCTIONS FOR TREATING SPECIFIC BUFFER TYPES ;;;;;;;;;;;;;;;;;

;;;###autoload
(defun mkhtml-dired (&optional buffer html-dir)
  "Save Dired BUFFER as a new HTML file in directory HTML-DIR.
HTML links are provided to each of the directory's files. 
HTML-DIR is the directory where resulting HTML file is created.
Defaults: BUFFER is current buffer. HTML-DIR is `default-directory'.

Hint: You can use such an HTML file to allow navigation to only
      certain files in a directory."
  (interactive
   (list (buffer-name)
         (read-file-name "Directory for HTML file: " nil default-directory)))
  (or buffer (setq buffer (current-buffer)))
  (mkhtml-particular-buffer buffer (file-name-nondirectory (buffer-name)) html-dir
                      'mkhtml-dired-link-fn))


(defun mkhtml-dired-link-fn (mouse-face last-mouse-change html-buf
                                        ignored-1 ignored-2 ignored-3
                                        ignored-4 ignored-5)
  ;; Function used by `mkhtml-dired' to create HTML links.
  (when (and mouse-face (>= (point) last-mouse-change))
    (princ (concat "<A HREF=\"" (dired-get-filename t) "\">") html-buf)))


;;;###autoload
(defun mkhtml-info-buffer (&optional buffer html-dir)
  "HTMLize Info BUFFER (default: current). Mouse-face => HTML links.
Save Info BUFFER as a new HTML file in directory HTML-DIR.

Text with `mouse-face' property is converted to HTML links. 

The HTML file is named after the buffer's Info node. Whenever a node 
name contains illegal filename characters the name is altered:
  1) `directory-sep-char' is replaced by `mkhtml-dir-sep-replacement'.
  2) `mkhtml-convert-std-filename' is used to replace other chars."
  (interactive
   (list (buffer-name)
         (read-file-name "Directory for HTML file: " nil default-directory)))
  (or buffer (setq buffer (current-buffer)))
  (let (html-file-sans)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (if (looking-at "File:[ \t]+\\([^:,\t\n]+\\),?[ \t]*\
Node:[ \t]+[^:,\t\n]+,?")
            (setq html-file-sans
                  (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
          (error "Not an Info buffer."))))
    (mkhtml-particular-buffer buffer html-file-sans html-dir 'mkhtml-info-link-fn)))


(defun mkhtml-info-link-fn (mouse-face last-mouse-change html-buf
                                       link-start link-end
                                       html-file-sans html-dir ignored)
  ;; Function `mkhtml-info-buffer' uses to create HTML links.
  (let ((html-buf (get-buffer-create "*html*")))
    (when (and mouse-face (>= (point) last-mouse-change))
      (let ((non-file-menu-item (mkhtml-non-file-menu-item))
            node-name)
        (cond (non-file-menu-item       ; Menu item that doesn't  name a file.
               (if (string-match "^dir" html-file-sans)
                   (princ (concat "<A HREF=\"" ; This is the root Info menu item.
                                  (mkhtml-make-HTML-file-name
                                   (mkhtml-convert-std-filename
                                    (concat (substring
                                             non-file-menu-item
                                             1 (1- (length non-file-menu-item)))
                                            "-Top"))
                                   html-dir)
                                  "\">")
                          html-buf)
                 (princ (concat "<A HREF=\"" ; normal menu item.
                                (mkhtml-make-HTML-file-name
                                 (mkhtml-convert-std-filename
                                  (concat html-file-sans mkhtml-file-node-sep
                                          non-file-menu-item))
                                 html-dir)
                                "\">")
                        html-buf)))
              ((string-match "^(dir)"   ; link to the root Info node. 
                             (setq node-name (buffer-substring-no-properties
                                              link-start link-end)))
               (princ (concat "<A HREF=\""
                              (mkhtml-make-HTML-file-name
                               (convert-standard-filename "dir-Top")
                               html-dir)
                              "\">")
                      html-buf))
              (t                        ; other link
               (princ (concat "<A HREF=\""
                              (mkhtml-make-HTML-file-name
                               (mkhtml-convert-std-filename
                                (concat html-file-sans mkhtml-file-node-sep
                                        node-name))
                               html-dir)
                              "\">")
                      html-buf)))))))

;;;###autoload
(defun mkhtml-merged-info-buffer (&optional buffer html-dir)
  "HTMLize merged Info BUFFER (default: current). Mouse-face => links.
Save merged Info BUFFER as a new HTML file in directory HTML-DIR.
A merged Info buffer is one created via `Info-merge-subnodes', 
which is defined in file `info+.el'.

Text with `mouse-face' property is converted to HTML links.
The HTML file is named after the buffer. 

The HTML file is named after the buffer's top node. Whenever it
contains illegal filename characters it is altered:
  1) `directory-sep-char' is replaced by `mkhtml-dir-sep-replacement'.
  2) `mkhtml-convert-std-filename' is used to replace other chars.

Note: For a merged Info buffer, there is no way of knowing if a link
      is to somewhere in the same buffer or not. Most such links are
      menu items, so they will point to places in the same buffer.
      But cross refs (`Note:') may point beyond the buffer.  We don't
      treat cross refs differently here - instead, all links are made
      to point to hashes (#) in the same HTML file derived from the
      same buffer."
  (interactive
   (list (buffer-name)
         (read-file-name "Directory for HTML file: " nil default-directory)))
  (or buffer (setq buffer (current-buffer)))
  (let (html-file-sans)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (setq html-file-sans (file-name-nondirectory
                              (or (buffer-file-name) (buffer-name))))))
    (mkhtml-particular-buffer buffer html-file-sans html-dir
                              'mkhtml-merged-info-link-fn)))

(defun mkhtml-merged-info-link-fn (mouse-face last-mouse-change html-buf
                                              link-start link-end
                                              ignored-1 ignored-2 face)
  ;; Function `mkhtml-merged-info-buffer' uses to create HTML links.
  (if mouse-face
      
      ;; mouse-face link. Create internal link (#)
      (when (>= (point) last-mouse-change)
        (princ (concat "<A HREF=\"#"
                       (or (mkhtml-non-file-menu-item)
                           (buffer-substring-no-properties link-start link-end))
                       "\">")
               (get-buffer-create "*html*")))
    
    ;; Sub-node title. Create anchor.
    (when (and face (boundp info-file-face) (eq face info-file-face))
      (let ((menu-item (or (mkhtml-non-file-menu-item) (mkhtml-file-menu-item))))
        (when menu-item
          (princ (concat "<A name=\"" menu-item "\">") html-buf))))))

;;;###autoload
(defun mkhtml-plain-buffer (&optional buffer html-dir)
  "Convert a BUFFER to HTML, changing its mouse-face text to links.
Save BUFFER as a new HTML file in directory HTML-DIR.

A link resulting from mouse-face'd text points to an HTML named after
that text. Note that in general this is likely to point nowhere, so
use this only where appropriate.

Buffers with no mouse-face'd text are treated as by `htmlize-buffer'."
  (interactive
   (list (buffer-name)
         (read-file-name "Directory for HTML file: " nil default-directory)))
  (or buffer (setq buffer (current-buffer)))
  (let (html-file-sans)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (setq html-file-sans (file-name-nondirectory
                              (or (buffer-file-name) (buffer-name))))))
    (mkhtml-particular-buffer buffer html-file-sans html-dir 'mkhtml-plain-link-fn)))

(defun mkhtml-plain-link-fn (mouse-face last-mouse-change html-buf
                                        link-start link-end
                                        ignored-1 ignored-2 ignored-3)
  ;; Function `mkhtml-plaiin-buffer' uses to create HTML links.
  (when (and mouse-face (>= (point) last-mouse-change))
    (princ (concat "<A HREF=\"#"
                   (buffer-substring-no-properties link-start link-end)
                   "\">")
           (get-buffer-create "*html*"))))

;;;###autoload
(defun mkhtml-particular-buffer (&optional buffer html-file-sans html-dir link-fn)
  "Save BUFFER as a new HTML file in directory HTML-DIR.
This is a general function that can be parameterized by a LINK-FN 
function to perform HTML link-generation for a specific kind of file. 

For example uses, see `mkhtml-dired', `mkhtml-info-buffer', 
`mkhtml-merged-info-buffer', and `mkhtml-plain-buffer'.

HTMLS-FILE-SANS is the name of the resulting HTML file, without the
\".html\" suffix.

This function skips through BUFFER, stopping at each text-property
change, where it calls LINK-FN which creates an HTML link if 
appropriate. 

LINK-FN is passed the current value of the mouse-face
text-property, the buffer positions of the last change in that
property, the likely link start and end, HTMLS-FILE-SANS, the HTML
buffer and HTML-DIR.

Note that LINK-FN can decide to create a link even when there is no
mouse-face - see `mkhtml-merged-info-buffer' for an example."
  (interactive
   (list (buffer-name)
         (read-file-name "HTML file: " nil (file-name-nondirectory
                                            (or (buffer-file-name) (buffer-name))))
         (read-file-name "Directory for HTML file: " nil default-directory)))
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (run-hooks 'htmlize-before-hook)
    (htmlize-make-face-hash (cons 'default (htmlize-faces-in-buffer))))
  (save-window-excursion
    (let* ((html-buf (get-buffer-create "*html*"))
           next-face-change last-face-change next-mouse-change last-mouse-change
           face mouse-face face-object no-node-p link link-start link-end)
      (set-buffer html-buf)
      (erase-buffer)
      (buffer-disable-undo)
      (mkhtml-insert-header buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (setq next-face-change (point))
          (setq next-mouse-change (point))
          (while (not (eobp))
            (setq face (get-text-property (point) 'face))
            (setq mouse-face (get-text-property (point) 'mouse-face))
            (setq last-face-change next-face-change)
            (setq last-mouse-change next-mouse-change)
            (setq next-face-change (next-single-property-change
                                    (point) 'face nil (point-max)))
            (setq next-mouse-change (next-single-property-change
                                     (point) 'mouse-face nil (point-max)))
            (setq link-start (point))
            (setq link-end next-mouse-change)
            (when (consp face) (setq face (car face))) ; Choose the first face.
            (when (eq face 'default) (setq face nil))
            ;;`face' property can contain arbitrary stuff.
            (unless (htmlize-symbol-face-p face) (setq face nil))
            (funcall link-fn
                     mouse-face last-mouse-change html-buf link-start link-end
                     html-file-sans html-dir face)
            (when (and face (>= (point) last-face-change))
              (setq face-object (gethash face htmlize-face-hash))
              (princ (htmlize-method face-prejunk face-object) html-buf))
            (princ (htmlize-protect-string
                    (buffer-substring-no-properties
                     (point) (min next-face-change next-mouse-change)))
                   html-buf)
            (when (and face (<= next-face-change next-mouse-change))
              (princ (htmlize-method face-postjunk face-object) html-buf))
            (when (and mouse-face (<= next-mouse-change next-face-change))
              (princ "</A>" html-buf))
            (goto-char (min next-face-change next-mouse-change)))))
      (insert "</pre>\n  </body>\n</html>\n")
      (goto-char (point-min))
      (run-hooks 'htmlize-after-hook)
      (buffer-enable-undo)
      (mkhtml-save-buffer html-file-sans html-dir)
      (clrhash htmlize-face-hash))))    ; Allow next gc to free up used conses.


;;;;;;;;;;;;;;;;;;;;;;;;;;ADD BASIC FONT INFO TO HTML OUTPUT;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;   Yes, I know this is only approximate, and browsers don't always deal with this
;;;   stuff well, but this may be better than nothing, and you can always turn it off.

(defvar mkhtml-preserve-fonts t
  "*Non-nil <=> an attempt is made to use similar fonts in HTML file(s)
to those in the Emacs buffer.")

(defvar mkhtml-font-size nil
  "*Size of text in output HTML file(s).
If nil, an attempt is made to approximate the text size of the Emacs
buffer's frame. If non-nil, it should be an integer that will be used
as the BASEFONT size value (if `htmlize-output-type' = 'font) or the
font-size value (if `htmlize-output-type' = 'css).")

(defsubst mkhtml-get-css-font-size ()
  "Get approximate HTML font size from current frame's font size."
  (let* ((font (cdr-safe (assq 'font (frame-parameters)))))
    (if (and font (string-match "[0-9]+" font))
        ;; Trial & error led to this fudge factor of 3, which looks about right
        ;; in Netscape 4.7 and IExplorer 5.00
        ;; Note: IExplorer doesn't seem to recognize font-size in BODY element.
        (- (string-to-number (substring font (match-beginning 0) (match-end 0))) 3)
      10)))

(defsubst mkhtml-get-basefont-size ()
  "Get approximate HTML font size from current frame's font size."
  ;; Trial & error with Netscape 4.7 and IExplorer 5.00 led to this fudge factor.
  (/ (- (mkhtml-get-css-font-size) 2) 3))
    
(defsubst mkhtml-get-font-family ()
  "Return family of current frame's font."
  (let ((name (x-resolve-font-name nil)))
    (if (or (string-match ".+-fontset-\\([^-*]+\\)" name)
            (string-match "^-[*]-\\([^-*]+\\)" name))
        (match-string 1 name)
      "Courrier")))                     ; Default


;; REPLACES ORIGINAL in `htmlize.el':
;; Adds (approximate) BASEFONT size and face, when `mkhtml-preserve-fonts'.
(defun htmlize-font-body-tag ()
  (let ((face-object (gethash 'default htmlize-face-hash)))
    (concat (format "<BODY text=\"%s\" bgcolor=\"%s\">"
                    (htmlize-face-rgb-foreground face-object)
                    (htmlize-face-rgb-background face-object))
            (and mkhtml-preserve-fonts
                 (format "<BASEFONT size=\"%d\" face=\"%s\">"
                         (or mkhtml-font-size (mkhtml-get-basefont-size))
                         (mkhtml-get-font-family))))))


;; REPLACES ORIGINAL in `htmlize.el':
;; Adds font-size and font-family, when `mkhtml-preserve-fonts'.
(defun htmlize-css-specs (face-object &optional default-face-object)
  (let (result)
    (when mkhtml-preserve-fonts
      (let ((font-size (or mkhtml-font-size (mkhtml-get-css-font-size)))
            (font-family (mkhtml-get-font-family)))
        (push (format "font-size: %dpt;" font-size) result)
        (push (format "font-family: %s;" font-family) result)))
    (when (or (not default-face-object)
	      (not (equal (htmlize-face-rgb-foreground face-object)
			  (htmlize-face-rgb-foreground default-face-object))))
      (push (format "color: %s;" (htmlize-face-rgb-foreground face-object))
	    result))
    (when (or (not default-face-object)
	      (not (equal (htmlize-face-rgb-background face-object)
			  (htmlize-face-rgb-background default-face-object))))
      (push (format "background-color: %s;"
		    (htmlize-face-rgb-background face-object)) result))
    (when (and (htmlize-face-boldp face-object)
	       (or (not default-face-object)
		   (not (htmlize-face-boldp default-face-object))))
      (push "font-weight: bold;" result))
    (when (and (htmlize-face-italicp face-object)
	       (or (not default-face-object)
		   (not (htmlize-face-italicp default-face-object))))
      (push "font-style: italic;" result))
    (when (and (htmlize-face-underlinep face-object)
	       (or (not default-face-object)
		   (not (htmlize-face-underlinep default-face-object))))
      (push "text-decoration: underline;" result))
    (when (and (htmlize-face-strikep face-object)
	       (or (not default-face-object)
		   (not (htmlize-face-strikep default-face-object))))
      (push "text-decoration: line-through;" result))
    (nreverse result)))


;; REPLACES ORIGINAL in `htmlize.el':
;; Adds link colors.
(defun htmlize-css-insert-head ()
  (insert "    <style type=\"text/css\">\n    <!--\n")
  (let ((default-face-object (gethash 'default htmlize-face-hash)))
    (insert "      BODY {\n        "
	    (mapconcat #'identity (htmlize-css-specs default-face-object)
		       "\n        ")
	    "\n      } /* default */\n")
    (maphash
     (lambda (face face-object)
       (let ((cleaned-up-face-name (symbol-name face)))
	 ;; If face name contains `--' or `*/', we must nix them out.
	 (while (string-match "--" cleaned-up-face-name)
	   (setq cleaned-up-face-name (replace-match "-" t t
						     cleaned-up-face-name)))
	 (while (string-match "\\*/" cleaned-up-face-name)
	   (setq cleaned-up-face-name (replace-match "XX" t t
						     cleaned-up-face-name)))
	 (unless (eq face 'default)
	   (let ((specs (htmlize-css-specs face-object default-face-object)))
	     (insert "      span." (htmlize-face-css-name face-object))
	     (if (null specs)
		 (insert " {")
	       (insert " {\n        "
		       (mapconcat #'identity specs "\n        ")))
	     (insert "\n      } /* " cleaned-up-face-name " */\n")))))
     htmlize-face-hash))
  ;; Code added to color links - Drew Adams.
  (insert (concat "     A:link { color: " mkhtml-link-color " }\n"))
  (insert (concat "     A:visited { color: " mkhtml-visited-link-color " }\n"))
  (insert (concat "     A:hover { color: " mkhtml-hover-link-fg-color "; background: "
                  mkhtml-hover-link-bg-color " }\n"))
  (insert "    -->\n    </style>\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `mkhtml.el' ends here
