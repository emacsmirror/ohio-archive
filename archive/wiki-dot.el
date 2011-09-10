;;; wiki-dot.el --- Export wiki pages to dot files

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: wiki-dot.el
;; Version: 3.0.1
;; Keywords: hypermedia
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Description: Export wiki pages to dot files
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

;; Produces a graph of the entire wiki.  The graph is described using
;; the dot language.  See http://www.graphviz.org/ for source and binary
;; distributions.  There are two tools available: dot for directed
;; graphs and neato for undirected graphs.  Customize `wiki-graph-type'
;; to control the output.

;; In order to convert the dot file to postscript (see the respective
;; man pages for other formats):

;; $ neato -Tps -o wiki.ps wiki.dot

;; I use the -Elen=1.3 parameter to make edges longer.

;; Please: If you have interesting ideas about how to improve usability
;; of the resulting graph, tell me all about it!  :) Problems I'm
;; having: How to produce a gif file -- it seems I have to install fonts
;; somewhere but I don't know how.  How to increase the size of the
;; resulting postscript file -- most of the time only part of the graph
;; is visible.

;;; Code:

(require 'wiki)

(defgroup wiki-dot nil
  "Options for the graphical rendition of the wiki links."
  :group 'wiki)

(defcustom wiki-graph-file "~/tmp/wiki.dot"
  "Filename to write graphs in the dot format to."
  :group 'wiki-dot
  :type 'file)

(defcustom wiki-graph-type 'undirected
  "The type of the graph used, either `directed' or `undirected'.
Directed graph output must be processed using the dot program.
Undirected graph output must be processed using the neato program."
  :group 'wiki-dot
  :type '(choice (const directed)
		 (const undirected)))

;; Main functions

(defun wiki-graph ()
  "Produce a dot file for a visual overview.
Save the result in a file and run the dot program on it in order to
visualize your wiki.  See `wiki-graph-type', `wiki-graph-start-with',
and `wiki-include-function'"
  (interactive)
  (save-some-buffers)
  (let ((buf (get-buffer-create "*wiki graph*")))
    (set-buffer buf)
    (erase-buffer)
    (cond ((eq wiki-graph-type 'undirected)
	   (insert "graph "))
	  (t
	   (insert "digraph ")))
    (insert "wiki {\n")
    (mapcar (lambda (page)
	      (wiki-graph-structure (car page) (cdr page) buf))
	    (reverse (wiki-filter (wiki-parse-files))))
    (insert "}\n")
    (write-file wiki-graph-file)
    (switch-to-buffer buf)))

(defun wiki-graph-structure (from to output)
  "Write links between FROM and each element in TO to buffer OUTPUT."
  (let ((standard-output output))
    (while to
      (wiki-graph-item from (car to))
      (setq to (cdr to)))))

(defun wiki-graph-item (from to)
  "Print the line linking node FROM and TO.
Takes `wiki-graph-type' into account."
  (princ (concat "  " from
		 (cond ((eq wiki-graph-type 'undirected)
			" -- ")
		       (t " -> "))
		 to ";\n")))

;; wiki-dot.el ends here

