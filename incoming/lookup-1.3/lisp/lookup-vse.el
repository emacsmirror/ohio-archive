;;; lookup-vse.el --- Lookup Virtual Search Engine
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-vse.el,v 1.5 1999/07/27 13:06:48 tsuchiya Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

;;;;;;;;;;;;;;;;;;;;
;: Search Query
;;;;;;;;;;;;;;;;;;;;

(defun lookup-vse-search-query (dictionary query)
  ;; DICTIONARY から QUERY を検索する。
  ;; 変数 `lookup-force-update' が non-nil の場合、キャッシュを無視する。
  ;; 実際に検索を行なうのは `lookup-vse-search-query-internal'.
  (let ((entries (lookup-entries-cache-get dictionary query)))
    (when (or (not entries) lookup-force-update)
      (setq entries (lookup-vse-search-query-internal dictionary query))
      (lookup-entries-cache-put dictionary query (or entries 'no-exists)))
    (unless (eq entries 'no-exists)
      entries)))

(defvar lookup-entries-cache nil)

(defun lookup-entries-cache-get (dictionary query)
  (lookup-multi-get 'lookup-entries-cache dictionary
		    (lookup-query-method query)
		    (lookup-intern-string (lookup-query-string query))))

(defun lookup-entries-cache-put (dictionary query entries)
  (lookup-multi-put 'lookup-entries-cache dictionary
		    (lookup-query-method query)
		    (lookup-intern-string (lookup-query-string query))
		    entries))

(defun lookup-entries-cache-clear ()
  (setq lookup-entries-cache nil))

(defun lookup-vse-search-query-internal (dictionary query)
  (let (entries)
    (cond
     ((eq (lookup-query-method query) 'expansion)
      ;; query-expansion を行う場合
      (let* ((method (lookup-dictionary-default-method dictionary))
	     (expander (if (lookup-dictionary-hiragana dictionary)
			   'lookup-kanji-get-readings
			 (lookup-dictionary-expander dictionary)))
	     (filter (if (lookup-dictionary-hiragana dictionary)
			 'lookup-kanji-filter
		       (lookup-dictionary-expand-filter dictionary)))
	     (string (lookup-query-string query))
	     (candidates (nreverse (funcall expander string)))
	     (prefix (concat "[" string " ->] ")))
	(setq entries
	      (apply 'nconc
		     (delq nil
			   (mapcar
			    (lambda (candidate)
			      (mapcar
			       (lambda (entry)
				 (setq entry (lookup-copy-entry entry))
				 (lookup-entry-set-prefix entry prefix)
				 entry)
			       (lookup-vse-search-query-internal dictionary
								 (lookup-make-query method candidate))))
			    candidates))))
	(if filter (setq entries (funcall filter string entries)))))
     ((eq (lookup-query-method query) 'stemming)
      ;; stemming を行う場合
      (let* ((method (lookup-dictionary-default-method dictionary))
	     (stemmer (lookup-dictionary-stemmer dictionary))
	     (string (lookup-query-string query))
	     (candidates (nreverse (funcall stemmer string))))
	(while candidates
	  (setq query (lookup-make-query method (car candidates)))
	  (setq entries (lookup-vse-search-query dictionary query))
	  (setq candidates (if entries nil (cdr candidates))))
	(if entries
	    (let ((prefix (concat "[" string " ->] ")))
	      (setq entries (mapcar (lambda (entry)
				      (setq entry (lookup-copy-entry entry))
				      (lookup-entry-set-prefix entry prefix)
				      entry)
				    entries))))))
     (t
      ;; デフォルトの動作
      (setq entries (lookup-dictionary-command dictionary 'search query))))
    (lookup-foreach 'lookup-arrange-heading entries)
    entries))


;;;;;;;;;;;;;;;;;;;;
;: Insert content
;;;;;;;;;;;;;;;;;;;;

(defun lookup-vse-insert-content (entry)
  ;; ENTRY の内容をバッファに挿入し、整形処理を行なう。
  ;; 変数 `lookup-force-update' が non-nil の場合、キャッシュを無視する。
  (let ((cache (lookup-contents-cache-get entry lookup-enable-format)))
    (if (or (not cache) lookup-force-update)
	;; insert content
	(let ((lookup-proceeding-message
	       (format "Inserting `%s'" (lookup-entry-heading entry)))
	      (dictionary (lookup-entry-dictionary entry)))
	  (lookup-proceeding-message nil)
	  (insert (lookup-dictionary-command dictionary 'content entry))
	  (if lookup-enable-format (lookup-arrange-content entry))
	  (lookup-contents-cache-put entry lookup-enable-format
				     (buffer-string))
	  (if lookup-enable-format (lookup-adjust-content entry))
	  (lookup-proceeding-message t))
      ;; use cache
      (insert cache)
      (if lookup-enable-format (lookup-adjust-content entry)))))

(defvar lookup-contents-cache nil)

(defun lookup-contents-cache-put (entry formatted content)
  (lookup-multi-put
   'lookup-contents-cache (lookup-entry-dictionary entry) formatted
   (lookup-assoc-set (lookup-multi-get 'lookup-contents-cache
				       (lookup-entry-dictionary entry)
				       formatted)
		     (lookup-entry-code entry) content)))

(defun lookup-contents-cache-get (entry formatted)
  (lookup-assoc-ref (lookup-multi-get 'lookup-contents-cache
				      (lookup-entry-dictionary entry)
				      formatted)
		    (lookup-entry-code entry)))

(defun lookup-contents-cache-clear ()
  (setq lookup-contents-cache nil))

;;;
;:: Interface functions
;;;

(defun lookup-arrange-heading (entry)
  (let* ((dictionary (lookup-entry-dictionary entry))
	 (funcs (lookup-dictionary-headings dictionary)))
    (when funcs
      (with-temp-buffer
	(insert (lookup-entry-heading entry))
	(lookup-foreach (lambda (func)
			  (goto-char (point-min))
			  (funcall func entry))
			funcs)
	(lookup-entry-set-heading entry (buffer-string))))))

(defun lookup-arrange-content (entry)
  (let* ((dictionary (lookup-entry-dictionary entry))
	 (arranges (lookup-dictionary-option dictionary ':arranges t)))
    (lookup-format entry arranges "formatting")))

(defun lookup-adjust-content (entry)
  (let* ((dictionary (lookup-entry-dictionary entry))
	 (adjusts (lookup-dictionary-option dictionary ':adjusts t))
	 (arranges (lookup-dictionary-option dictionary ':arranges t))
	 (work (concat "formatting" (make-string (length arranges) ?.))))
    (lookup-format entry adjusts work)))

;;;
;:: Internal functions
;;;

(defun lookup-format (entry functions work)
  (let ((n 1))
    (lookup-foreach (lambda (func)
		      (lookup-proceeding-message
		       (concat work (make-string (setq n (1+ n)) ?.)))
		      (widen)
		      (goto-char (point-min))
		      (funcall func entry))
		    functions)))

(defun lookup-heading-face (level)
  (or (nth (1- level) '(lookup-heading-1-face
			lookup-heading-2-face lookup-heading-3-face
			lookup-heading-4-face lookup-heading-5-face))
      'lookup-heading-low-face))

;;;
;:: Arrange functions
;;;

(defsubst lookup-make-region-heading (start end level)
  (add-text-properties start end (list 'face (lookup-heading-face level)
				       'lookup-heading level)))

(defun lookup-arrange-default-headings (entry)
  (lookup-make-region-heading (point) (progn (end-of-line) (point)) 1))

(defun lookup-arrange-references (entry)
  (let* ((case-fold-search nil)
	 (dictionary (lookup-entry-dictionary entry))
	 (pattern (lookup-dictionary-option dictionary ':reference-pattern t))
	 (regexp (car pattern)) (region-field (nth 1 pattern))
	 (heading-field (nth 2 pattern)) (code-field (nth 3 pattern))
	 region heading code reference)
    (while (re-search-forward regexp nil t)
      (save-match-data
	(setq region (if (integerp region-field)
			 (match-string region-field)
		       (eval region-field)))
	(setq heading (if (integerp heading-field)
			  (match-string heading-field)
			(eval heading-field)))
	(if (integerp code-field)
	    (setq code (match-string code-field)
		  reference (lookup-make-entry dictionary code heading))
	  (setq reference (lookup-make-reference dictionary heading heading))
	  (lookup-reference-make-dynamic reference code-field)))
      (replace-match region t t)
      (lookup-set-link (match-beginning 0) (point) reference)
      (lookup-arrange-heading reference))))

(defun lookup-dynamic-code-search (reference)
  "リファレンスのコードをダイナミックに検索する。"
  (let ((dictionary (lookup-entry-dictionary reference))
	(query (lookup-make-query 'exact (lookup-entry-code reference))))
    (lookup-vse-search-query dictionary query)))

(defun lookup-arrange-gaijis (entry)
  (let* ((case-fold-search t)
	 (dictionary (lookup-entry-dictionary entry))
	 (regexp (lookup-dictionary-option dictionary ':gaiji-regexp t))
	 start end gaiji)
    (while (re-search-forward regexp nil t)
      (setq start (match-beginning 0) end (match-end 0))
      (when (setq gaiji (lookup-vse-get-gaiji dictionary (match-string 1)))
	(delete-region start end)
	(lookup-gaiji-insert gaiji)))))

(defun lookup-arrange-fill-lines (entry)
  (let ((fill-column (if (integerp lookup-fill-column)
			 lookup-fill-column
		       (round (* (window-width) lookup-fill-column))))
	start)
    (while (not (eobp))
      (setq start (point))
      (end-of-line)
      (if (> (current-column) fill-column)
	  (fill-region start (point)))
      (forward-line))))

(defun lookup-arrange-fill-paragraphs (entry)
  (text-mode)
  (let ((fill-column (if (integerp lookup-fill-column)
			 lookup-fill-column
		       (round (* (window-width) lookup-fill-column)))))
    (while (not (eobp))
      (fill-paragraph nil)
      (forward-paragraph))))

(defun lookup-arrange-squeezed-references (entry)
  (if (lookup-dictionary-option
       (lookup-entry-dictionary entry) ':squeezed nil)
      (while (search-forward-regexp "→□\\(#0001\\|<gaiji:z0001>\\)?" nil t)
	(replace-match ""))))

;;;
;:: Adjust functions
;;;

(defun lookup-adjust-show-gaijis (entry)
  (lookup-map-over-property
   (point-min) (point-max) 'lookup-gaiji 'lookup-gaiji-glyph-paste))

(defun lookup-adjust-check-references (entry)
  (lookup-map-over-property
   (point-min) (point-max) 'lookup-reference
   (lambda (start end reference)
     (if (if (lookup-reference-p reference)
	     (lookup-reference-refered-p reference)
	   (lookup-entry-refered-p reference))
	 (put-text-property start end 'face 'lookup-refered-face)
       (put-text-property start end 'face 'lookup-reference-face)))))

(defun lookup-adjust-goto-min (entry)
  (goto-char (point-min)))


;;;;;;;;;;;;;;;;;;;;
;: Other Functions
;;;;;;;;;;;;;;;;;;;;

(defun lookup-vse-get-menu (dictionary)
  (let ((entries (lookup-dictionary-get-property dictionary 'menu-entries)))
    (unless entries
      (when (lookup-dictionary-command-p dictionary 'menu)
	(setq entries (lookup-dictionary-command dictionary 'menu))
	(lookup-foreach 'lookup-arrange-heading entries)
	(lookup-dictionary-put-property dictionary 'menu-entries entries)))
    entries))

(defun lookup-vse-open-entry (entry)
  (let ((dictionary (lookup-entry-dictionary entry)))
    (when (lookup-dictionary-command-p dictionary 'open)
      (lookup-dictionary-command dictionary 'open entry)
      t)))

(defun lookup-vse-get-gaiji (dictionary code)
  (let* ((table (lookup-dictionary-gaiji-table dictionary))
	 (gaiji (lookup-gaiji-table-ref table code))
	 (glyph (if gaiji (lookup-gaiji-glyph gaiji)))
	 (alter (if gaiji (lookup-gaiji-alternate gaiji))))
    (unless (and gaiji glyph alter)
      (if (or (not lookup-enable-gaiji) (not (lookup-gaiji-glyph-possible-p)))
	  (if alter (lookup-gaiji-set-glyph gaiji alter))
	(setq alter (format lookup-gaiji-alternate code))
	(if glyph
	    (lookup-gaiji-set-alternate gaiji alter)
	  (when (setq glyph (lookup-dictionary-command dictionary 'gaiji code))
	    (setq glyph (lookup-gaiji-glyph-compose glyph))
	    (if gaiji
		(lookup-gaiji-set-glyph gaiji glyph)
	      (setq gaiji (lookup-make-gaiji glyph alter))
	      (lookup-gaiji-table-set table code gaiji))))))
    gaiji))

(provide 'lookup-vse)

;;; lookup-vse.el ends here
