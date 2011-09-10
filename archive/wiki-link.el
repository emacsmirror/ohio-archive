;;; wiki-link.el --- Extended links for wikis.

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>
;;                     John Wiegley <johnw@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: wiki-link.el
;; Version: 1.0.0
;; Keywords: hypermedia
;; Author: Alex Schroeder <alex@gnu.org>
;;         John Wiegley <johnw@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Description: Support extended link syntax for wikis.
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

;; This allows extended link names in your wikis:

;; Markup                  Link               Name
;; -----------------------------------------------------
;; [[LINK]]                LINK               LINK
;; [[LINK][NAME]]          LINK               NAME

;; NAME can be anything (that doesn't include bracket characters), and
;; LINK can be a URL or a WikiName.

;; Furthermore, anchors within wiki pages are supported.  To make an
;; anchor, put "#Anchor" at the beginning of the line you want to link
;; to.  To link to an anchor, use the WikiName#Anchor link pattern.

;;; Code:

(require 'wiki)

;; New rules for `wiki-publishing-markup'.  From most general to most
;; specific (since they are added to the front of the markup list).

(let ((additional-links
       (list '("^#\\(\\sw+\\)" . "<a name=\"\\1\">")
	     ;; FooBar#baz
	     (cons (concat "<a href=\"\\("
			   wiki-names
			   "\\)\\.html\">\\1</a>#\\(\\sw+\\)")
		   "<a href=\"\\1.html#\\2\">\\3</a>")
	     ;; [[LINK][NAME]]
	     '("\\[\\[<a href=\"\\([^]]+\\)\">[^]]+</a>\\]\\[\\([^]]+\\)\\]\\]"
	       . "<a href=\"\\1\">\\2</a>")
	     ;; [[LINK]]
	     '("\\[\\[<a href=\"\\([^]]+\\)\">[^]]+</a>\\]\\]"
	       . "<a href=\"\\1\">\\1</a>"))))
  (setq wiki-publishing-markup (append wiki-publishing-markup additional-links)))

(provide 'wiki-link)

;; wiki-link.el ends here

