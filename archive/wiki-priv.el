;;; wiki-priv.el --- manage private wiki pages

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: wiki-priv.el
;; Version: 1.0.0
;; Keywords: hypermedia
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Description: manage private wiki pages
;; URL: http://www.geocities.com/kensanata/wiki/WikiMode.html
;; Compatibility: Emacs20, XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This allows you to maintain wiki pages in yet another directory.
;; These pages are considered private.  They will not be published.  Now
;; you can use `wiki-publish-all' to publish the public files, and
;; continue to use `wiki-mode' to keep notes for yourself.

;; The directory for your public wiki pages are listed in
;; `wiki-directories' which defaults to ~/Wiki/.  This file adds another
;; variable, `wiki-private-directories' which defaults to ~/Notes/.

;; When you call `wiki-private-mode' you toggle between the two states.
;; You may also use `wiki-private-mode-on' or `wiki-private-mode-off'.
;; In wiki private mode, only the files in ~/Notes/ are wikis and
;; publishing is disabled.  In normal wiki mode, only the files in
;; ~/Wiki/ are wikis and publishing will put the HTML copies into
;; `wiki-pub-directory' which defaults to ~/WebWiki/.

;; In order to install, put (require 'wiki-priv) somewhere in your
;; ~/.emacs file.

;;; Code:

(require 'wiki)

(defcustom wiki-private-directories (list (expand-file-name "~/Notes/"))
  "List of directories where all private wiki files are stored."
  :group 'wiki
  :type '(repeat directory))

(easy-mmode-define-minor-mode
 wiki-private-mode
 "Wiki private mode restricts your wiki works.
When `wiki-private-mode' is enabled, `wiki-directories' will be set to
`wiki-private-directories' and `wiki-pub-directory' will be set to nil.")

(add-hook 'wiki-private-mode-hook 'wiki-private-mode-apply)

(defun wiki-private-mode-apply ()
  "Do the right thing according to `wiki-private-mode'."
  (if wiki-private-mode
      (wiki-private-mode-on)
    (wiki-private-mode-off)))

(defun wiki-private-mode-on ()
  "Set `wiki-private-mode' to on.
Set `wiki-directories' to `wiki-private-directories'.
Set `wiki-pub-directory' to nil.
Run `wiki-private-maybe'."
  (interactive)
  (setq wiki-directories wiki-private-directories
	wiki-pub-directory nil)
  (wiki-private-maybe)
  (message "Private Wiki mode on."))

(defun wiki-private-mode-off ()
  "Set `wiki-private-mode' to on.
Restore `wiki-directories'.
Restore `wiki-pub-directory'.
Run `wiki-private-maybe'."
  (interactive)
  (setq wiki-directories
	(eval (car (or (get 'wiki-directories 'saved-value)
		       (get 'wiki-directories 'standard-value))))
	wiki-pub-directory
	(eval (car (or (get 'wiki-pub-directory 'saved-value)
		       (get 'wiki-pub-directory 'standard-value)))))
  (wiki-private-maybe)
  (message "Private Wiki mode off."))

(defun wiki-private-maybe ()
  "Call `wiki-maybe' for all buffers."
  (let ((bufs (buffer-list))
	buf)
    (while bufs
      (setq buf (car bufs)
	    bufs (cdr bufs))
      (set-buffer buf)
      ;; ugly hack because wiki-private-mode is buffer local
      (setq wiki-private-mode (default-value wiki-private-mode))
      (when buffer-file-name
	(wiki-maybe)))))

(provide 'wiki-priv)

;; wiki-priv.el ends here

