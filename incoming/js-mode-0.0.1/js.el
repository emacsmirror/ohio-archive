;; -*- Emacs-Lisp -*-
;;
;; js.el - A major mode for editing Javascript (ECMAScript)
;; 
;; Copyright (C) 2000 Iain Lowe
;;
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; 

(defvar js-mode-hook nil
  "*List of functions to be called upon entering Javascript (ECMAScript) mode.")

(defvar js-mode-map nil
  "Keymap for Javascript (ECMAScript) major mode.")

(if js-mode-map
    nil                             ; Leave it alone if it exists
  (progn                            ; Otherwise...
    (setq js-mode-map (make-sparse-keymap))
    (define-key js-mode-map "\C-c\C-a" 'setup-buffer)))

(defvar js-mode-syntax-table nil
  "Syntax table for Javascript (ECMAScript) major mode.")

(if js-mode-syntax-table
    nil                             ; do nothing if it's already set up
  (setq js-mode-syntax-table (make-syntax-table text-mode-syntax-table))

  ;; Define the whitespace characters
  (modify-syntax-entry ?\  " " js-mode-syntax-table)
  (modify-syntax-entry ?\t " " js-mode-syntax-table)
  (modify-syntax-entry ?\r " " js-mode-syntax-table)
  (modify-syntax-entry ?\n " " js-mode-syntax-table)

  ;; Define open and close parenthesis characters
  (modify-syntax-entry ?(  "()  " js-mode-syntax-table)
  (modify-syntax-entry ?)  ")(  " js-mode-syntax-table)

  (modify-syntax-entry ?[  "(]  " js-mode-syntax-table)
  (modify-syntax-entry ?]  ")[  " js-mode-syntax-table)

  (modify-syntax-entry ?{  "(}  " js-mode-syntax-table)
  (modify-syntax-entry ?}  "){  " js-mode-syntax-table)

  ;; Define the comment characters
  (modify-syntax-entry ?*  ". 23" js-mode-syntax-table)
  (modify-syntax-entry ?/  ". 124b" js-mode-syntax-table))

;; Font lock stuff
(defvar js-mode-font-lock-keywords
  '(("\\<function\\>" . font-lock-keyword-face)

    ("^function \\([^(]+\\)[\\ |\\\t]*"
     1 font-lock-function-name-face)

    ("^function \\(.+\\)[\\ |\\\t]*(\\([^)]+\\)"
     2 font-lock-variable-name-face)

    ("var \\([^;=\\ \\\t]+\\)" 1 font-lock-variable-name-face)

    ("\\(<!--.*\\)$" 1 font-lock-comment-face)
    ("\\(<!--.*-->\\)" 1 font-lock-comment-face)
    ("\\(//.*\\)$" 1 font-lock-comment-face)
    ("\\(/\\*.*\\*/\\)" 1 font-lock-comment-face)

    ("\\<true\\>" . font-lock-keyword-face)
    ("\\<false\\>" . font-lock-keyword-face)

    ("\\<continue\\>" . font-lock-keyword-face)
    ("\\<break\\>" . font-lock-keyword-face)

    ("\\<if\\>" . font-lock-keyword-face)
    ("\\<else\\>" . font-lock-keyword-face)
    
    ("\\<while\\>" . font-lock-keyword-face)
    
    ("\\<for\\>" . font-lock-keyword-face)
    ("\\<in\\>" . font-lock-keyword-face)

    ("\\<return\\>" . font-lock-keyword-face)
    
    ("\\<var\\>" . font-lock-type-face)

    ("\\<with\\>" . font-lock-keyword-face)))

(defun js-mode ()
  "A major mode for editing Javascript (ECMAScript).
Complete with syntax colouring, keybindings and script
templates."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'js-mode)
  (setq mode-name "Javascript (ECMAScript)")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(js-mode-font-lock-keywords t))
  (use-local-map js-mode-map)
  (run-hooks 'js-mode-hook))

(provide 'js)



