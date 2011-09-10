;;; iss-mode.el --- Mode for InnoSetup install scripts

;; Copyright (C) 2000-2001 by Stefan Reichoer

;; Emacs Lisp Archive Entry
;; Filename: iss-mode.el
;; Author: Stefan Reichoer, <reichoer@web.de>
;; Version: 1.1

;; $Id: iss-mode.el,v 1.2 2001/04/06 08:51:33 reichr Exp $

;; iss-mode.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; iss-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; InnoSetup is an Application Installer for Windows
;; See: http://www.jrsoftware.org/isinfo.htm
;; This version of iss-mode.el is tested with InnoSetup v2.0

;; To use iss-mode put the following two lines in your .emacs:
;; (autoload 'iss-mode "iss-mode" "Innosetup Script Mode" t)
;; (setq auto-mode-alist (append '(("\\.iss$"  . iss-mode)) auto-mode-alist))

;; You also have to set the iss-compiler-path to your InnoSetup installation
;; in your .emacs:
;; (setq iss-compiler-path "c:/Programme/InnoSetup2/")

;; iss-mode provides the following features:
;; * Syntax coloring for InnoSetup scripts
;; * Integration of the InnoSetup commandline compiler iscc.exe
;;   - Compilation via iss-compile
;;   - Jump to compilation error via M-x next-error
;; * Start Innosetup help via M-x iss-compiler-help
;; * Test the installation via M-x iss-run-installer

;; Of course you can bind this commands to keys (e.g. in the iss-mode-hook)

;; Comments / suggestions welcome!

;;; Code:

(defvar iss-compiler-path nil "Path to the iss compiler")

;;; End of user settings

(defvar iss-mode-syntax-table nil
  "Syntax table in use in iss-mode buffers.")

(if iss-mode-syntax-table
    ()
  (setq iss-mode-syntax-table (make-syntax-table))

  ; ";" starts a comment
  ;(modify-syntax-entry ?\; "<" iss-mode-syntax-table)
  (modify-syntax-entry ?\; ". 12" iss-mode-syntax-table)
  ;; and \n and \^M end a comment
  (modify-syntax-entry ?\n ">"    iss-mode-syntax-table)
  (modify-syntax-entry ?\^M ">"   iss-mode-syntax-table)

  (modify-syntax-entry ?\" "."   iss-mode-syntax-table)

  (modify-syntax-entry ?_ "w"    iss-mode-syntax-table))


(defvar iss-font-lock-keywords nil "Expressions to highlight in iss mode.")

(setq iss-font-lock-keywords
  (list
   (cons (concat "^;\.*")
	 'font-lock-comment-face)
   (cons (concat "\\sw+: ")
	 'font-lock-keyword-face)
   (cons "^[ \t]*\\[\.+\\]" 'font-lock-function-name-face) ;font-lock-constant-face)
   (cons (concat "^[ \t]*\\<\\(appname\\|appvername\\|appcopyright\\|appid\\|"
                 "appmutex\\|beveledlabel\\|defaultdirname"
                 "\\|defaultgroupname\\|minversion\\|outputdir\\|outputbasefilename\\|"
                 "allownoicons\\|uninstallfilesdir\\|"
                 "sourcedir\\|disableprogramgrouppage\\|alwayscreateuninstallicon\\)\\>")
	 'font-lock-type-face)
   (cons (concat "\\<\\(alwaysskipifsameorolder\\|uninsneveruninstall\\|"
                 "comparetimestampalso\\|restartreplace\\|isreadme\\|"
                 "uninsdeletekeyifempty\\|uninsdeletekey\\)\\>")
	 'font-lock-variable-name-face)
   (cons (concat "\\<\\(HKCU\\|HKLM\\|dirifempty\\)\\>")
	 'font-lock-constant-face)))

(defvar iss-mode-map () "Keymap used in iss-mode buffers.")

(cond ((not iss-mode-map)
       (setq iss-mode-map (make-sparse-keymap))))

(easy-menu-define
 iss-menu
 iss-mode-map
 "InnoSetup script menu"
 (list
  "ISS"
  ["Compile"         (iss-compile)  t]
  ["Run Installer"   (iss-run-installer)  t]
  ["InnoSetup Help"  (iss-compiler-help)  t]
  ))
(easy-menu-add iss-menu)


(defun iss-mode ()
  "Major mode for editing InnoSetup script files. Upon startup iss-mode-hook is run."
  (interactive)
  (kill-all-local-variables)
  (use-local-map iss-mode-map)
  (setq major-mode 'iss-mode)
  (setq mode-name "iss")
  (set-syntax-table iss-mode-syntax-table)
  (set (make-local-variable 'comment-start) "^;")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-multi-line) nil)

  (set (make-local-variable 'compilation-error-regexp-alist)
        '(("\\(Error on line\\) \\([0-9]+\\):" nil 2)))
  (set (make-local-variable 'compilation-file-regexp-alist)
          '(("iscc \\(.*\\)$" 1)))

  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(iss-font-lock-keywords nil t))
  (run-hooks 'iss-mode-hook))

(defun iss-compiler-help ()
  "Start the online documentation for the InnoSetup compiler"
  (interactive)
  (if iss-compiler-path
      (w32-shell-execute 1 (concat iss-compiler-path "ISetup.hlp"))))

(defun iss-compile ()
  "Compile the actual file with the InnoSetup compiler"
  (interactive)
  (if iss-compiler-path
      (let ((compilation-process-setup-function 'iss-process-setup))
        (compile (concat iss-compiler-path "iscc " (buffer-file-name))))))

(defun iss-process-setup ()
  "Set up `compilation-exit-message-function' for `iss-compile'."
  (set (make-local-variable 'compilation-exit-message-function)
       'iss-compilation-exit-message-function))

(defun iss-compilation-exit-message-function (process-status exit-status msg)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;scroll down one line, so that the compile command is parsed to:
    ; -> get the filename of the compiled file
    (insert "\n"))
  (cons msg exit-status))

(defun iss-find-option (option)
  (let ((search-regexp
         (concat option "[ \t]*=[ \t]*\\(.*\\)$")))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp search-regexp nil t)
            (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun iss-run-installer ()
  (interactive)
  (let ((executable
         (concat (or (iss-find-option "outputdir") "Output\\")
                 (or (iss-find-option "outputbasefilename") "setup")
                 ".exe")))
    (w32-shell-execute 1 executable)))
