;; @(#) ada-xref.el --- use Gnat for lookup and completion in Ada mode

;; Copyright (C) 1994-1999 Free Software Foundation, Inc.

;; Author: Markus Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de>
;;      Rolf Ebert <ebert@inf.enst.fr>
;;      Emmanuel Briot <briot@gnat.com>
;; Maintainer: Emmanuel Briot <briot@gnat.com>
;; Ada Core Technologies's version:   $Revision: 1.70 $
;; Keywords: languages ada xref

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

;;; Commentary:
;;; This Package provides a set of functions to use the output of the
;;; cross reference capabilities of the GNAT Ada compiler
;;; for lookup and completion in Ada mode.
;;;
;;; The functions provided are the following ones :
;;;    - `ada-complete-identifier': completes the current identifier as much as
;;;      possible, depending of the known identifier in the unit
;;;    - `ada-point-and-xref': moves the mouse pointer and shows the declaration
;;;      of the selected identifier (either in the same buffer or in another
;;;      buffer
;;;    - `ada-goto-declaration': shows the declaration of the selected
;;;      identifier (the one under the cursor), either in the same buffer or in
;;;      another buffer
;;;    - `ada-goto-declaration-other-frame': same as previous, but opens a new
;;      frame to show the declaration
;;;    - `ada-compile-application': recompile your whole application, provided
;;;      that a project file exists in your directory
;;;    - `ada-run-application': run your application directly from emacs
;;;    - `ada-reread-prj-file': force emacs to read your project file again.
;;;      Otherwise, this file is only read the first time emacs needs some
;;;      informations, which are then kept in memory
;;;    - `ada-change-prj': change the prj file associated with a buffer
;;;    - `ada-change-default-prj': change the default project file used for
;;;      every new buffer
;;;
;;; If a file *.`adp' exists in the ada-file directory, then it is
;;; read for configuration informations. It is read only the first
;;; time a cross-reference is asked for, and is not read later.

;;; You need Emacs >= 20.2 to run this package

;; ----- Requirements -----------------------------------------------------

(require 'compile)
(require 'comint)

;; ----- Dynamic byte compilation -----------------------------------------
(defvar byte-compile-dynamic nil)
(make-local-variable 'byte-compile-dynamic)
(setq byte-compile-dynamic t)

;; ------ Use variables
(defcustom ada-xref-other-buffer t
  "*if non-nil then either use a buffer in the same frame or another frame.
If Nil, always jump to the declaration in the same buffer"
  :type 'boolean :group 'ada)

(defcustom ada-xref-create-ali t
  "*if non-nil, run gcc whenever it is needed
if nil, the cross-reference mode will never run gcc"
  :type 'boolean :group 'ada)

(defcustom ada-xref-confirm-compile nil
  "*if non-nil, ask for command confirmation before compiling or
running the application"
  :type 'boolean :group 'ada)

(defcustom ada-krunch-args "0"
  "*Maximum number of characters for filename create by gnatkr
Set to 0, if you don't use crunched filenames."
  :type 'string :group 'ada)

(defcustom ada-prj-default-comp-cmd "${cross_prefix}gcc -c -g -gnatq -I${src_dir}"
  "*Default command to be used to compile a single file.
Emacs will add the filename at the end of this command.
This is the same syntax as in the project file."
  :type 'string :group 'ada)

(defcustom ada-prj-default-make-cmd
  (concat "${cross_prefix}gnatmake ${main} -aI${src_dir} -aO${obj_dir} "
          "-g -gnatq -cargs ${comp_opt} "
          "-bargs ${bind_opt} -largs ${link_opt}")
  "*Default command to be used to compile the application.
This is the same syntax as in the project file."
  :type 'string :group 'ada)

(defcustom ada-prj-default-project-file ""
  "*Non nil means always use this project file, no matter what the
directory is. Emacs will not try to use the standard algorithm to
find the project file.
Note: you can use M-<TAB> in the customization buffer for completion"
  :type '(file :must-match t) :group 'ada)

(defcustom ada-gnatstub-opts "-q -I${src_dir}"
  "*List of the options to pass to gnatsub when generating the body from
a spec file. This has the same syntax as in the project file (with
variable substitution"
  :type 'string :group 'ada)

(defcustom ada-always-ask-project nil
  "*Non-nil means ask for the name of a project file to use when none is
found by the standard algorithm.
Nil means use default values when no project file was found")

;; ------- Nothing to be modified by the user below this
(defvar ada-other-file-alist
  (list
   (list (concat "\\" ada-spec-suffix "$") 'ada-find-other-file)
   (list (concat "\\" ada-body-suffix "$") 'ada-find-other-file)
   )
  "Variable used by find-file to find the name of the other package.
See `ff-other-file-alist'"
  )

(defvar ada-last-prj-file ""
  "Name of the last project file entered by the user, when the
default algorithm did not find any possible project file")

(defvar ada-check-switch " -gnats "
  "Switch added to the command line to check the current file")

(defvar ada-project-file-extension ".adp"
  "The extension used for project files")

(defconst is-windows (memq system-type (quote (windows-nt)))
  "true if we are running on windows NT or windows 95")

(defvar ada-xref-pos-ring '()
  "This is the list of all the positions we went to with
`ada-xref-goto-declaration'. This is used to go back to these positions.")
(defconst ada-xref-pos-ring-max 16
  "Number of positions kept in the list ada-xref-pos-ring")

(defvar ada-operator-re
  "\\+\\|-\\|/\\|\\*\\|=\\|mod\\|and\\|not\\|or\\|xor\\|<=\\|<\\|>=\\|>"
  "Regexp to match for operators")

(defvar ada-xref-default-prj-file nil
  "name of the default prj file, per directory.
Every directory is potentially associated with a default project file
If it is nil, then the first prj file loaded will be the default for this
emacs session")

;; These variables will be overwritted by buffer-local variables
(defvar ada-prj-prj-file nil
  "Name of the project file for the current ada buffer")
(defvar ada-prj-src-dir nil
  "List of directories to look into for ada sources")
(defvar ada-prj-obj-dir nil
  "List of directories to look into for object and .ali files")
(defvar ada-prj-comp-opt nil
  "Switches to use on the command line for the default compile
command (gcc)")
(defvar ada-prj-bind-opt nil
  "Switches to use on the command line for the default bind
command (gnatbind)")
(defvar ada-prj-link-opt nil
  "Switches to use on the command line for the default link
command (gnatlink)")
(defvar ada-prj-comp-cmd nil
  "Command to use to compile the current file only")
(defvar ada-prj-make-cmd nil
  "Command to use to compile the whole current application")
(defvar ada-prj-run-cmd nil
  "Command to use to run the current application")
(defvar ada-prj-debug-cmd nil
  "Command to use to run the debugger")
(defvar ada-prj-main nil
  "Name of the main programm of the current application")
(defvar ada-prj-remote-machine nil
  "Name of the machine to log on before a compilation")
(defvar ada-prj-cross-prefix nil
  "Prefix to be added to the gnatmake, gcc, ... commands when
using a cross-compilation environment.
A '-' is automatically added at the end if not already present.
For instance, the compiler is called `ada-prj-cross-prefix'gnatmake")

;; ----- Keybindings ------------------------------------------------------

(defun ada-add-keymap ()
  "Add new key bindings when using ada-xrel.el"
  (interactive)
  (if ada-xemacs
      (progn
        (define-key ada-mode-map '(shift button3) 'ada-point-and-xref)
        (define-key ada-mode-map "\C-co" 'ada-xemacs-find-file)
        (define-key ada-mode-map '(control tab) 'ada-complete-identifier))
    (define-key ada-mode-map [C-tab] 'ada-complete-identifier)
    (define-key ada-mode-map "\C-co"    'ff-find-other-file)
    (define-key ada-mode-map [S-mouse-3] 'ada-point-and-xref))

  (define-key ada-mode-map "\C-c5\C-d" 'ada-goto-declaration-other-frame)
  (define-key ada-mode-map "\C-c\C-d" 'ada-goto-declaration)
  (define-key ada-mode-map "\C-c\C-s" 'ada-xref-goto-previous-reference)
  (define-key ada-mode-map "\C-c\C-x" 'ada-reread-prj-file)
  (define-key ada-mode-map [f10] 'next-error)
  (define-key ada-mode-map "\C-c\C-c" 'ada-compile-application)
  (define-key ada-mode-map "\C-cb"  'ada-buffer-list)
  (define-key ada-mode-map "\C-cc"  'ada-change-prj)
  (define-key ada-mode-map "\C-cd"  'ada-change-default-prj)
  (define-key ada-mode-map "\C-cg"  'ada-gdb-application)
  (define-key ada-mode-map "\C-cr"  'ada-run-application)
  (define-key ada-mode-map "\C-c\C-o" 'ada-goto-parent)
  (define-key ada-mode-map "\C-c\C-r" 'ada-find-references)
  (define-key ada-mode-map "\C-c\C-v" 'ada-check-current)
  )

;; ----- Menus --------------------------------------------------------------
(defun ada-add-ada-menu ()
  "Add some items to the standard Ada mode menu (the menu defined in
ada-mode.el)"
  (interactive)

  (if ada-xemacs
      (progn
        (add-menu-button '("Ada") ["Check file" ada-check-current t] "Goto")
        (add-menu-button '("Ada") ["Compile file" ada-compile-current t] "Goto")
        (add-menu-button '("Ada") ["Build" ada-compile-application t] "Goto")
        (add-menu-button '("Ada") ["Run" ada-run-application t] "Goto")
        (add-menu-button '("Ada") ["Debug" ada-gdb-application t] "Goto")
        (add-menu-button '("Ada") ["--" nil t] "Goto")
        (add-submenu '("Ada") '("Project"
                                ["Associate"   ada-change-prj t]
                                ["Set Default" ada-set-default-project-file t]
                                ["List" ada-buffer-list t])
                     "Goto")
        (add-menu-button '("Ada" "Goto") ["Goto Parent Unit" ada-goto-parent t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["Goto References to any entity" ada-find-any-references t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["Goto References" ada-find-references t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["Goto Declaration Other Frame"
                                          ada-goto-declaration-other-frame t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["Goto Declaration/Body" ada-goto-declaration t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["--" nil t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Edit") ["Complete Identifier" ada-complete-identifier t]
                         "Indent Line")
        (add-menu-button '("Ada" "Edit") ["--------" nil t]
                         "Indent Line")
        )

    ;; for Emacs
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Check]
      '("Check file" . ada-check-current) 'Customize)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Compile]
      '("Compile file" . ada-compile-current) 'Check)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Build]
      '("Build" . ada-compile-application) 'Compile)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Run]
      '("Run" . ada-run-application) 'Build)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Debug]
      '("Debug" . ada-gdb-application) 'Run)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [rem]
      '("--" . nil) 'Debug)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Project]
      (cons "Project" (easy-menu-create-keymaps
                       "Project"
                       '(["Associate" ada-change-prj t]
                         ["Set Default" ada-set-default-project-file t]
                         ["List" ada-buffer-list t])))
      'rem)

    (let ((help-submenu (lookup-key ada-mode-map [menu-bar Ada Help]))
          (goto-submenu (lookup-key ada-mode-map [menu-bar Ada Goto]))
          (edit-submenu (lookup-key ada-mode-map [menu-bar Ada Edit])))

      (define-key help-submenu [Gnat_ug]
        '("Gnat User Guide" . (lambda() (interactive) (info "gnat_ug"))))
      (define-key help-submenu [Gnat_rm]
        '("Gnat Reference Manual" . (lambda() (interactive) (info "gnat_rm"))))
      (define-key help-submenu [Gcc]
        '("Gcc Documentation" . (lambda() (interactive) (info "gcc"))))
      (define-key help-submenu [gdb]
        '("Ada Aware Gdb Documentation" . (lambda() (interactive) (info "gdb"))))
      (define-key goto-submenu [rem]    '("----" . nil))
      (define-key goto-submenu [Parent] '("Goto Parent Unit" . ada-goto-parent))
      (define-key goto-submenu [References-any]
        '("Goto References to any entity" . ada-find-any-references))
      (define-key goto-submenu [References]
        '("Goto References" . ada-find-references))
      (define-key goto-submenu [Prev]
        '("Goto Previous Reference" . ada-xref-goto-previous-reference))
      (define-key goto-submenu [Decl-other]
        '("Goto Declaration Other Frame" . ada-goto-declaration-other-frame))
      (define-key goto-submenu [Decl]
        '("Goto Declaration/Body" . ada-goto-declaration))

      (define-key edit-submenu [rem] '("----" . nil))
      (define-key edit-submenu [Complete] '("Complete Identifier"
                                            . ada-complete-identifier))
      )
    ))

;; ----- Utilities -------------------------------------------------

(defun ada-require-project-file ()
  "If no project file is assigned to this buffer, load one"
  (if (not ( my-local-variable-if-set-p 'ada-prj-src-dir (current-buffer)))
      (ada-parse-prj-file (ada-prj-find-prj-file))))

(defun my-local-variable-if-set-p (variable &optional buffer)
  (and (local-variable-p variable buffer)
       (save-excursion
         (set-buffer buffer)
         (symbol-value variable))))

(defun ada-xref-push-pos (filename position)
  "Push (FILENAME, POSITION) on the position ring for cross-references"
  (setq ada-xref-pos-ring (cons (list position filename) ada-xref-pos-ring))
  (if (> (length ada-xref-pos-ring) ada-xref-pos-ring-max)
      (setcdr (nthcdr (1- ada-xref-pos-ring-max) ada-xref-pos-ring) nil)))

(defun ada-xref-goto-previous-reference ()
  "Go to the previous cross-reference we were on"
  (interactive)
  (if ada-xref-pos-ring
      (progn
        (let ((pos (car ada-xref-pos-ring)))
          (setq ada-xref-pos-ring (cdr ada-xref-pos-ring))
          (find-file (car (cdr pos)))
          (goto-char (car pos))))))

(defun ada-convert-file-name (name)
  "Function to convert from the buffer file name to the name given in
argument to the ada-compile-current function.  This function is
overridden on VMS to convert from VMS filename to Unix filenames"
  name)

(defun ada-set-default-project-file (name)
  (interactive "fName of project file:")
  (set 'ada-prj-default-project-file name)
  (ada-reread-prj-file t)
  )

;; ------ Handling the project file -----------------------------

(defun ada-replace-substring (cmd-string search-for replace-with)
  "Replace all instances of SEARCH-FOR with REPLACE-WITH in
string CMD-STRING"
  (while (string-match search-for cmd-string)
    (setq cmd-string (replace-match replace-with t t cmd-string)))
  cmd-string)

(defun ada-treat-cmd-string (cmd-string)
  "Replace meta-sequences like ${...} with the appropriate value in CMD-STRING.
The current buffer must be the one where all local variable are definied (that
is the ada source)"

  (if ( my-local-variable-if-set-p 'ada-prj-src-dir (current-buffer))
      (if  (string-match "\\(-[^-\$I]*I\\)\${src_dir}" cmd-string)
          (progn
            (let ((str-def (substring cmd-string (match-beginning 1)
                                      (match-end 1))))
              (setq cmd-string
                    (ada-replace-substring cmd-string
                                           "\\(-[^-\$I]*I\\)\${src_dir}"
                      (mapconcat
                      (lambda (x) (concat str-def x))
                      ada-prj-src-dir " ")))))))
  (if ( my-local-variable-if-set-p 'ada-prj-obj-dir (current-buffer))
      (if (string-match "\\(-[^-\$O]*O\\)\${obj_dir}" cmd-string)
          (progn
            (let ((str-def (substring cmd-string (match-beginning 1)
                                      (match-end 1))))
              (setq cmd-string
                    (ada-replace-substring cmd-string
                                           "\\(-[^-\$O]*O\\)\${obj_dir}"
                        (mapconcat
                         (lambda (x) (concat str-def x))
                         ada-prj-obj-dir
                         " ")))))))
  (if ( my-local-variable-if-set-p 'ada-prj-remote-machine (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${remote_machine}"
                                   ada-prj-remote-machine)))
  (if ( my-local-variable-if-set-p 'ada-prj-comp-opt (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${comp_opt}"
                                   ada-prj-comp-opt)))
  (if ( my-local-variable-if-set-p 'ada-prj-bind-opt (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${bind_opt}"
                                   ada-prj-bind-opt)))
  (if ( my-local-variable-if-set-p 'ada-prj-link-opt (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${link_opt}"
                                   ada-prj-link-opt)))
  (if ( my-local-variable-if-set-p 'ada-prj-main (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${main}"
                                   ada-prj-main)))
  (if ( my-local-variable-if-set-p 'ada-prj-cross-prefix (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${cross_prefix}"
                                   ada-prj-cross-prefix)))
  cmd-string)


(defun ada-prj-find-prj-file (&optional no-user-question)
  "Find the prj file associated with the current buffer
The rules are the following ones :
- If the buffer is already associated with a prj file, use this one
- else if there's a default prj file for the same directory use it
- else if a prj file with the same filename exists, use it
- else if there's only one prj file in the directory, use it
- else if there are more than one prj file, ask the user
- else if there is no prj file and no-user-question is nil, ask the user
  for the project file to use."
  (let* ((current-file (buffer-file-name))
         (first-choice (concat
                        (file-name-sans-extension current-file)
                        ada-project-file-extension))
         (dir          (file-name-directory current-file))
         (prj-files    (directory-files
                        dir t
                        (concat ".*" (regexp-quote ada-project-file-extension) "$")))
         (choice       nil)
         (default      (assoc dir ada-xref-default-prj-file))
         )

    (cond

     ((my-local-variable-if-set-p 'ada-prj-prj-file (current-buffer))
      ada-prj-prj-file)

     ;; global default project file
     ((and ada-prj-default-project-file
           (not (string= ada-prj-default-project-file "")))
      ada-prj-default-project-file)

     (default                       ;; directory default project file
       (cdr default))

     ((file-exists-p first-choice)
      first-choice)

     ((= (length prj-files) 1)
      (car prj-files))

     ((> (length prj-files) 1)
      ;; more than one possible prj file => ask the user
      (with-output-to-temp-buffer "*choice list*"
        (princ "There are more than one possible project file. Which one should\n")
        (princ "I use ?\n\n")
        (princ "  no.   file name  \n")
        (princ "  ---   ------------------------\n")
        (let ((counter 1))
          (while (<= counter (length prj-files))
            (princ (format "  %2d)    %s\n"
                           counter
                           (nth (1- counter) prj-files)))
            (setq counter (1+ counter))
            ) ; end of while
          ) ; end of let
        ) ; end of with-output-to ...
      (setq choice nil)
      (while (or
              (not choice)
              (not (integerp choice))
              (< choice 1)
              (> choice (length prj-files)))
        (setq choice (string-to-int
                      (read-from-minibuffer "Enter No. of your choice: "
                                            ))))
      (nth (1- choice) prj-files))

     ((= (length prj-files) 0)
      ;;  no project file found. Ask the user about it (the default value
      ;;  is the last one the user entered.
      (if (or no-user-question (not ada-always-ask-project))
          nil
        (setq ada-last-prj-file
              (read-file-name "project file:" nil ada-last-prj-file))
        (if (string= ada-last-prj-file "") nil ada-last-prj-file))
     )
  )))


(defun ada-parse-prj-file (prj-file)
  "Reads and parses the PRJ-FILE file if it was found.
The current buffer should be the ada-file buffer"

  (let ((tmp-src-dir  nil)
        (tmp-obj-dir  nil)
        (tmp-comp-opt nil)
        (tmp-bind-opt nil)
        (tmp-link-opt nil)
        (tmp-main     nil)
        (tmp-comp-cmd nil)
        (tmp-make-cmd nil)
        (tmp-run-cmd  nil)
        (tmp-debug-cmd nil)
        (tmp-remote-machine nil)
        (tmp-cross-prefix nil)
        (tmp-cd-cmd   (if prj-file
                          (concat "cd " (file-name-directory prj-file) " && ")
                        (concat "cd " (file-name-directory (buffer-file-name (current-buffer))) " && ")))
        (ada-buffer (current-buffer))
        )
    ;; tries to find a project file in the current directory
    (if prj-file
        (progn
          (find-file prj-file)

          ;; first look for the src_dir lines
          (widen)
          (goto-char (point-min))
          (while
              (re-search-forward "^src_dir=\\(.*\\)" nil t)
            (progn
              (setq tmp-src-dir (cons
                                 (file-name-as-directory
                                  (match-string 1))
                                 tmp-src-dir
                                 ))))
          ;; then for the obj_dir lines
          (goto-char (point-min))
          (while (re-search-forward "^obj_dir=\\(.*\\)" nil t)
            (setq tmp-obj-dir (cons
                               (file-name-as-directory
                                (match-string 1))
                               tmp-obj-dir
                               )))

          ;; then for the options lines
          (goto-char (point-min))
          (if (re-search-forward "^comp_opt=\\(.*\\)" nil t)
              (setq tmp-comp-opt (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^bind_opt=\\(.*\\)" nil t)
              (setq tmp-bind-opt (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^link_opt=\\(.*\\)" nil t)
              (setq tmp-link-opt (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^main=\\(.*\\)" nil t)
              (setq tmp-main (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^comp_cmd=\\(.*\\)" nil t)
              (setq tmp-comp-cmd (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^remote_machine=\\(.*\\)" nil t)
              (setq tmp-remote-machine (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^cross_prefix=\\(.*\\)" nil t)
              (setq tmp-cross-prefix (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^make_cmd=\\(.*\\)" nil t)
              (setq tmp-make-cmd (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^run_cmd=\\(.*\\)" nil t)
              (setq tmp-run-cmd (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^debug_cmd=\\(.*\\)" nil t)
              (setq tmp-debug-cmd (match-string 1)))

          ;; kills the project file buffer, and go back to the ada buffer
          (kill-buffer nil)
          (set-buffer ada-buffer)
          ))

    ;; creates local variables (with default values if needed)
    (set (make-local-variable 'ada-prj-prj-file) prj-file)

    (set (make-local-variable 'ada-prj-src-dir)
         (if tmp-src-dir (reverse tmp-src-dir)  '("./")))

    (set (make-local-variable 'ada-prj-obj-dir)
         (if tmp-obj-dir (reverse tmp-obj-dir)  '("./")))

    (set (make-local-variable 'ada-prj-comp-opt)
         (if tmp-comp-opt tmp-comp-opt ""))

    (set (make-local-variable 'ada-prj-bind-opt)
         (if tmp-bind-opt tmp-bind-opt ""))

    (set (make-local-variable 'ada-prj-link-opt)
         (if tmp-link-opt tmp-link-opt ""))

    (set (make-local-variable 'ada-prj-cross-prefix)
         (if tmp-cross-prefix
             (if (or (string= tmp-cross-prefix "")
                     (= (aref tmp-cross-prefix (1- (length tmp-cross-prefix))) ?-))
                 tmp-cross-prefix
               (concat tmp-cross-prefix "-"))
              ""))

    (set (make-local-variable 'ada-prj-main)
         (if tmp-main tmp-main
           (substring (buffer-file-name) 0 -4)))

    (set (make-local-variable 'ada-prj-remote-machine)
         (ada-treat-cmd-string
          (if tmp-remote-machine tmp-remote-machine "")))

    (set (make-local-variable 'ada-prj-comp-cmd)
         (ada-treat-cmd-string
          (if tmp-comp-cmd tmp-comp-cmd
            (concat tmp-cd-cmd ada-prj-default-comp-cmd))))

    (set (make-local-variable 'ada-prj-make-cmd)
         (ada-treat-cmd-string
          (if tmp-make-cmd tmp-make-cmd
            (concat tmp-cd-cmd ada-prj-default-make-cmd))))

    (set (make-local-variable 'ada-prj-run-cmd)
         (ada-treat-cmd-string
          (if tmp-run-cmd tmp-run-cmd
            (if is-windows "${main}.exe" "${main}"))))

    (set (make-local-variable 'ada-prj-debug-cmd)
         (ada-treat-cmd-string
          (if tmp-debug-cmd tmp-debug-cmd
            (if is-windows
                "${cross_prefix}gdb ${main}.exe"
              "${cross_prefix}gdb ${main}"))))

    ;; Add each directory in src_dir to the default prj list
    (if prj-file
        (mapcar (lambda (x)
                  (if (not (assoc (expand-file-name x)
                                  ada-xref-default-prj-file))
                      (setq ada-xref-default-prj-file
                            (cons (cons (expand-file-name x)
                                        prj-file)
                                  ada-xref-default-prj-file))))
                ada-prj-src-dir))

    ;; Add the directories to the search path for ff-find-other-file
    ;; Do not add the '/' or '\' at the end
    (set (make-local-variable 'ff-search-directories)
         (append (mapcar 'directory-file-name ada-prj-src-dir)
                 ada-search-directories))

    ;; Sets up the compilation-search-path so that Emacs is able to
    ;; go to the source of the errors in a compilation buffer
    (setq compilation-search-path ada-prj-src-dir)

    ))


(defun ada-find-references (pos )
  "Find every references to the entity under POS
Calls gnatfind to find every references"
  (interactive "d")
  (ada-require-project-file)

  (let* ((alist (ada-read-identifier pos))
         (alifile (ada-get-ali-file-name (nth 3 alist))))
    ;;  kill the .ali file buffer
    (kill-buffer (current-buffer))

    (set-buffer (get-file-buffer (nth 3 alist)))

    ;;  if the file is more recent than the executable
    (if (or (buffer-modified-p (current-buffer))
            (file-newer-than-file-p (nth 3 alist) alifile))
        (ada-find-any-references (car alist)
                                 (nth 3 alist)
                                 nil nil)
      (ada-find-any-references (car alist)
                               (nth 3 alist)
                               (nth 1 alist)
                               (nth 2 alist))))
  )

(defun ada-find-any-references (entity &optional file line column)
  "Search for references to any entity"
  (interactive "sEntity name: ")
  (ada-require-project-file)

  (let* ((command (concat "gnatfind -rf " entity
                          (if file (concat ":" (file-name-nondirectory file)))
                          (if line (concat ":" line))
                          (if column (concat ":" column)))))

    ;;  If a project file is defined, use it
    (if (my-local-variable-if-set-p 'ada-prj-prj-file (current-buffer))
        (setq command (concat command " -p" ada-prj-prj-file)))

    (compile-internal command
                      "No more references"
                      "gnatfind")
    )
  )

(defun ada-buffer-list ()
  "Display a buffer with all the ada-mode buffers and their associated prj file"
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*Buffer List*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq standard-output (current-buffer))
    (princ "The following line is a list showing the associations between
directories and project file. It has the format : ((directory_1 . project_file1)
(directory2 . project_file2)...)\n\n")
    (princ ada-xref-default-prj-file)
    (princ "\n
 Buffer              Mode         Project file
 ------              ----         ------------
\n")
    (let ((bl (buffer-list)))
      (while bl
        (let* ((buffer (car bl))
               (buffer-name (buffer-name buffer))
               this-buffer-mode-name
               this-buffer-project-file)
          (save-excursion
            (set-buffer buffer)
            (setq this-buffer-mode-name
                  (if (eq buffer standard-output)
                      "Buffer Menu" mode-name))
            (if (string= this-buffer-mode-name
                         "Ada")
                (setq this-buffer-project-file
                      (if ( my-local-variable-if-set-p 'ada-prj-prj-file
                                                   (current-buffer))
                          (expand-file-name ada-prj-prj-file)
                        ""))))
          (if (string= this-buffer-mode-name
                         "Ada")
              (progn
                (princ (format "%-19s  "  buffer-name))
                  (princ (format "%-6s " this-buffer-mode-name))
                  (princ this-buffer-project-file)
                  (princ "\n")
                  ))
          ) ;; end let*
        (setq bl (cdr bl))
        ) ;; end while
      );; end let
    ) ;; end save-excursion
  (display-buffer "*Buffer List*")
  (other-window 1)
  )

(defun ada-change-prj (filename)
  "Change the project file associated with the current buffer"
  (interactive "fproject file:")

  ;; make sure we are using an Ada file
  (if (not (string= mode-name "Ada"))
    (error "You must be in ada-mode to use this function"))

  ;; create the local variable if necessay
  (if (not ( my-local-variable-if-set-p 'ada-prj-prj-file (current-buffer)))
      (make-local-variable 'ada-prj-prj-file))

  ;; ask the user for the new file name
  (setq ada-prj-prj-file filename)

  ;; force emacs to reread the prj file next-time
  (ada-reread-prj-file)
  )

(defun ada-change-default-prj (filename)
  "Change the default project file used for all ada files from the
current directory"
  (interactive "ffile name:")

  (setcdr (assoc (file-name-directory (buffer-file-name))
                 ada-xref-default-prj-file)
          (expand-file-name filename))
  )


;; ----- Identifier Completion --------------------------------------------
(defun ada-complete-identifier (pos)
  "Tries to complete the identifier around POS.
The feature is only available if the files where compiled not using the -gnatx
option"
  (interactive "d")
  (ada-require-project-file)

  ;; Initialize function-local variablesand jump to the .ali buffer
  ;; Note that for regexp search is case insensitive too
  (let* ((curbuf (current-buffer))
         (identlist (ada-read-identifier pos))
         (sofar (concat "^[0-9]+[a-zA-Z][0-9]+[ *]\\("
                        (regexp-quote (car identlist))
                        "[a-zA-Z0-9_]*\\)"))
         (completed nil)
         (symalist nil)
         (insertpos nil))

    ;; we are already in the .ali buffer
    (goto-char (point-max))

    ;; build an alist of possible completions
    (while (re-search-backward sofar nil t)
      (setq symalist (cons (cons (match-string 1) nil) symalist)))

    (setq completed  (try-completion "" symalist))

    ;; kills .ali buffer
    (kill-buffer nil)

    ;; deletes the incomplete identifier in the buffer
    (set-buffer curbuf)
    (looking-at "[a-zA-Z0-9_]+")
    (replace-match "")
    ;; inserts the completed symbol
    (insert completed)
    ))

;; ----- Cross-referencing ----------------------------------------

(defun ada-point-and-xref ()
 "Calls `mouse-set-point' and then `ada-goto-declaration'."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-goto-declaration (point)))


(defun ada-goto-declaration (pos)
  "Displays the declaration of the identifier around POS.
The declaration is shown in another buffer if `ada-xref-other-buffer' is non-nil"
  (interactive "d")
  (ada-require-project-file)
  (push-mark pos)
  (ada-xref-push-pos (buffer-file-name) pos)
  (ada-find-in-ali (ada-read-identifier pos)))

(defun ada-goto-declaration-other-frame (pos)
  "Displays the declaration of the identifier around point.
The declation is shown in another frame if `ada-xref-other-buffer' is non-nil"
  (interactive "d")
  (ada-require-project-file)
  (push-mark pos)
  (ada-xref-push-pos (buffer-file-name) pos)
  (ada-find-in-ali (ada-read-identifier pos) t))

(defun ada-compile (command)
  "Start a compilation, on the machine specified in the project file,
using command COMMAND"

  (if (and (my-local-variable-if-set-p 'ada-prj-remote-machine (current-buffer))
           (not (string= ada-prj-remote-machine "")))
      (set 'command
           (concat "rsh " ada-prj-remote-machine " '"
                   command "'")))
  (compile command))

(defun ada-compile-application ()
  "Compiles the whole application, using the command find in the gnat.prj file"
  (interactive)
  (ada-require-project-file)

  ;; prompt for command to execute
  (ada-compile
   (if ada-xref-confirm-compile
       (read-from-minibuffer "enter command to compile: "
                             ada-prj-make-cmd)
     ada-prj-make-cmd))
  )

(defun ada-compile-current ()
  "Recompile the current file"
  (interactive)
  (ada-require-project-file)

  ;; prompt for command to execute
  (ada-compile
   (if ada-xref-confirm-compile
       (read-from-minibuffer "enter command to compile: "
                             (concat
                              ada-prj-comp-cmd " " (ada-convert-file-name (buffer-file-name))))
     (concat ada-prj-comp-cmd " " (ada-convert-file-name (buffer-file-name)))))
  )

(defun ada-check-current ()
  "Recompile the current file"
  (interactive)
  (ada-require-project-file)

  ;; prompt for command to execute
  (let ((command (concat ada-prj-comp-cmd ada-check-switch
                         (ada-convert-file-name (buffer-file-name)))))
    (compile
     (if ada-xref-confirm-compile
         (read-from-minibuffer "enter command to compile: " command)
       command))))


(defun ada-run-application ()
  "Run the application"
  (interactive)
  (ada-require-project-file)

  (if (and (my-local-variable-if-set-p 'ada-prj-cross-prefix (current-buffer))
           (not (string= ada-prj-cross-prefix "")))
      (error "This feature is not supported yet for cross-compilation environments"))

  (let ((command ada-prj-run-cmd)
        (buffer  (current-buffer)))
    ;; Search the command name if necessary
    (if (not (my-local-variable-if-set-p 'ada-prj-run-cmd (current-buffer)))
        (setq command (file-name-sans-extension (buffer-name)))
      )

    ;; Ask for the arguments to the command
    (setq command
          (read-from-minibuffer "Enter command to execute: "
                                command))

    ;; Run the command
    (save-excursion
      (set-buffer (get-buffer-create "*run*"))
      (goto-char (point-max))
      (insert "\nRunning " command "\n\n")
      (make-comint "run"
                   (comint-arguments command 0 0)
                   nil
                   (comint-arguments command 1 nil))
      )
    (display-buffer "*run*")

    ;;  change to buffer *run* for interactive programs
    (other-window 1)
    (switch-to-buffer "*run*")
    )
  )

(defun ada-gdb-application ()
  "Run the application"
  (interactive)

  (let ((buffer (current-buffer))
        gdb-buffer)
    (ada-require-project-file)

    (if (and (my-local-variable-if-set-p 'ada-prj-cross-prefix buffer)
             (not (string= ada-prj-cross-prefix "")))
        (error "This feature is not supported yet for cross-compilation environments"))

    ;;  Launch gdb
    (if (my-local-variable-if-set-p 'ada-prj-debug-cmd buffer)
        (gdb ada-prj-debug-cmd)
      (gdb "")
      )

    ;;  The current buffer is now the gdb buffer
    ;;  Switch back to the source buffer
    (set 'gdb-buffer (current-buffer))
    (switch-to-buffer buffer)

    ;;  Warning: on Emacs >= 20.3.8, same-window-regexps includes gud-*,
    ;;  so the following call to display buffer will instead select the
    ;;  buffer instead of displaying it in another window
    ;;  This is why the second argument to display-buffer is 't'
    (display-buffer gdb-buffer t)
    ))


(defun ada-reread-prj-file (&optional for-all-buffer)
  "Forces emacs to read the project file again.
Otherwise, this file is only read once, and never read again
If `for-all-buffer' is non-nil, or the function was called with \C-u prefix,
then do this for every opened buffer"
  (interactive "P")
  (if for-all-buffer

      ;; do this for every buffer
      (mapcar (lambda (x)
                 (save-excursion
                   (set-buffer x)
                   (if (string= mode-name "Ada")
                       (progn
                         (kill-local-variable 'ada-prj-src-dir)
                         (kill-local-variable 'ada-prj-obj-dir)
                         (ada-parse-prj-file (ada-prj-find-prj-file))))
                   ))
               (buffer-list))

    ;; else do this just for the current buffer
    (kill-local-variable 'ada-prj-src-dir)
    (kill-local-variable 'ada-prj-obj-dir)
    (ada-parse-prj-file (ada-prj-find-prj-file)))
  )

;; ------ Private routines

(defun ada-xref-current (file &optional ali-file-name)
  "Creates a new ali file from the FILE source file,
assuming the ali file will be called ALI-FILE-NAME.
Uses the function `compile' to execute the commands
defined in the project file."
  ;; kill old buffer
  (if (and ali-file-name
           (get-file-buffer ali-file-name))
      (kill-buffer (get-file-buffer ali-file-name)))
  ;; prompt for command to execute
  (setq compile-command (concat ada-prj-comp-cmd
                                " "
                                file))
  (compile
   (if ada-xref-confirm-compile
       (read-from-minibuffer "enter command to execute gcc: "
                             compile-command)
     compile-command))
  )

(defun ada-first-non-nil (list)
  "Returns the first non-nil element of the list"
  (cond
   ((not list) nil)
   ((car list) (car list))
   (t (ada-first-non-nil (cdr list)))
   ))


(defun ada-find-ali-file-in-dir (file)
  "Search for FILE in obj_dir
The current buffer must be the Ada file"
  (ada-first-non-nil
   (mapcar (lambda (x)
             (if (file-exists-p (concat (file-name-directory x)
                                        file))
                 (concat (file-name-directory x) file)
               nil))
           ada-prj-obj-dir))
  )

(defun ada-get-ali-file-name (file)
  "create the ali file name for the ada-file FILE
The file is searched for in every directory shown in the
obj_dir lines of the project file"

  ;; This function has to handle the special case of non-standard
  ;; file names (i.e. not .adb or .ads)
  ;; The trick is the following:
  ;;   1- replace the extension of the current file with .ali,
  ;;      and look for this file
  ;;   2- If this file is found:
  ;;      grep the "^U" lines, and make sure we are not reading the
  ;;      .ali file for a spec file. If we are, go to step 3.
  ;;   3- If the file is not found or step 2 failed:
  ;;      find the name of the "other file", ie the body, and look
  ;;      for its associated .ali file by subtituing the extension

  (save-excursion
    (set-buffer (get-file-buffer file))
    (let ((short-ali-file-name
           (concat (file-name-sans-extension (file-name-nondirectory file))
                   ".ali"))
          (ali-file-name ""))
      ;; First step
      ;; we take the first possible completion
      (setq ali-file-name (ada-find-ali-file-in-dir short-ali-file-name))

      ;; Second step (see above for description)
      (if ali-file-name
          (if (and (string-match (concat (regexp-quote ada-spec-suffix)
                                         "$") file)
                   (not (string= ada-spec-suffix ".ads")))
              (let ((body-ali (ada-find-ali-file-in-dir
                               (concat (file-name-sans-extension (file-name-nondirectory
                                                                  (ada-other-file-name)))
                                       ".ali"))))
                (if body-ali
                    (set 'ali-file-name body-ali))))

        ;;  else we did not find the .ali file
        ;;  Second chance: in case the files do not have standard names (such
        ;;  as for instance file_s.ada and file_b.ada), try to go to the
        ;;  other file and look for its ali file
        (setq short-ali-file-name
              (concat (file-name-sans-extension (file-name-nondirectory (ada-other-file-name)))
                      ".ali"))
        (setq ali-file-name (ada-find-ali-file-in-dir short-ali-file-name))

        ;; If still not found, try to recompile the file
        (if (not ali-file-name)
            (progn
              ;; recompile only if the user asked for this
              (if ada-xref-create-ali
                  (ada-xref-current file ali-file-name))
              (error "Ali file not found. Recompile your file")))
        )

      ;; same if the .ali file is too old and we must recompile it
      (if (and (file-newer-than-file-p file ali-file-name)
               ada-xref-create-ali)
          (ada-xref-current file ali-file-name)

        ;; else returns the correct absolute file name
        (expand-file-name ali-file-name)))
    ))

(defun ada-get-ada-file-name (file original-file)
  "create the complete file name (+directory) for FILE
The original file (where the user was) is ORIGINAL-FILE.
Search in project file for possible paths"

  (save-excursion
    (set-buffer (get-file-buffer original-file))
    ;; we choose the first possible completion and we
    ;; return the absolute file name
    (let ((filename
           (ada-first-non-nil (mapcar (lambda (x)
                          (if (file-exists-p (concat (file-name-directory x)
                                                     (file-name-nondirectory file)))
                              (concat (file-name-directory x)
                                      (file-name-nondirectory file))
                            nil))
                        ada-prj-src-dir))))

      (if filename
          (expand-file-name filename)
        (error (concat
                (file-name-nondirectory file)
                " not found in src_dir. Please check your project file")))

      )))


(defun ada-get-ali-buffer (file)
  "Reads the ali file into a new buffer, and returns this buffer's name"
  (find-file-noselect (ada-get-ali-file-name file)))


(defun ada-find-file-number-in-ali (file)
  "Reads the file number for FILE in the corresponding .ali file"
  (set-buffer (ada-get-ali-buffer file))
  (goto-char (point-min))
  (let ((file-unit-number 1))

    (while
        (and
         (re-search-forward "^D \\([A-Za-z0-9_.-]+\\)" nil t)
         (not
          (string= (match-string 1)
                   (file-name-nondirectory file)))
         )
      (setq file-unit-number (1+ file-unit-number))
      )
    file-unit-number
    ))



(defun ada-read-identifier (pos)
  "Reads the identifier around POS and switch to the .ali buffer
Returns a list of five elements: 1) the identifier itself as a string
                                 2) the line # of the identifier
                                 3) the column # of its first character
                                 4) the file name
                                 5) the file number in the .ali file"


  ;; If there's a compilation in progress, it's probably because the
  ;; .ali file didn't exist. So we should wait...
  (if compilation-in-progress
      (progn
        (message ".ali file not yet available. Reissue your command when compilation is finished")
        (setq quit-flag t)))


  ;; goto first character of the identifier
  (goto-char pos)
  (skip-chars-backward "a-zA-Z0-9_<>")

  ;;
  ;; check if it really is an identifier
  ;;
  (if (ada-in-comment-p)
      (error "Inside comment"))

  (let (identifier)
    ;; Just in front of a string => we could have an operator...
    (if (= (char-after) ?\")
        (forward-char 1))

    ;; if looking at an operator
    (if (looking-at ada-operator-re)
        (progn
          (if (and (= (char-before) ?\")
                   (= (char-after (+ (length (match-string 0)) (point))) ?\"))
              (forward-char -1))
          (setq identifier (concat "\"" (match-string 0) "\"")))

      (if (ada-in-string-p)
          (error "Inside string or character constant"))
      (if (looking-at (concat ada-keywords "[^a-zA-Z_]"))
          (error "No cross-reference available for reserved keyword"))
      (if (looking-at "[a-zA-Z0-9_]+")
          (setq identifier (match-string 0))
        (error "No identifier around")))

    ;;
    ;; build a list of name, line, column, file name and number
    ;;
    (list
     identifier
     (count-lines (point-min) (point))
     (1+ (current-column))
     (buffer-file-name)
     (ada-find-file-number-in-ali (buffer-file-name))
     )))


(defun ada-find-in-ali (identlist &optional other-frame)
  "Look in the .ali file for the definition of the identifier
if OTHER-FRAME is non nil, and `ada-xref-other-buffer' is non nil,
opens a new window to show the declaration"

  (let* ((ali-buffer (ada-get-ali-buffer (nth 3 identlist)))
         (definition-file nil)
         (line-found t)
         (declaration-found nil)
         (bound nil)
         )

    (set-buffer ali-buffer)
    (goto-char (point-min))


    ;;  We want to look for the declaration only in a definite interval
    ;;  (after the "^X ..." line for the current file, and before the next
    ;;  "^X" line

    (if (re-search-forward (concat "^X [0-9]+ "
                                   (file-name-nondirectory (nth 3 identlist)))
                           nil t)
        (progn
          (save-excursion
            (setq bound (re-search-forward "^X " nil t))
            )
          (setq declaration-found
                (re-search-forward (concat "^"
                                           (nth 1 identlist) ;; line
                                           "."
                                           (nth 2 identlist) ;; col
                                           "[ *]"
                                           (regexp-quote (car identlist)) ;; name
                                           " \\(.*\\)$"
                                           ) bound t))
          )
    (setq declaration-found nil)
    )

    ;;
    ;;  There are two differents possibilities :
    ;;  If we were on the specification of the function, procedure...
    ;;
    (if declaration-found
        (let ((current-line (match-string 1)))
          ;; add every continuation line to the current-line
          (save-excursion
            (next-line 1)
            (beginning-of-line)
            (while (looking-at "^\\.\\(.*\\)")
              (setq current-line (concat current-line (match-string 1)))
              (next-line 1))
            )
          ;;  find the body position in the file
          (if (string-match "\\([0-9]+\\)[bc]\\([0-9]+\\)" current-line)
              (let ((definition-line (string-to-number (match-string 1 current-line)))
                    (definition-col (1- (string-to-number
                                         (match-string 2 current-line))))
                    )
                ;;  it there was a file number in the same line
                (if (string-match "\\([0-9]+\\)|\\([^|bc]+\\)?[bc]" current-line)
                    (let ((file-number (match-string 1 current-line)))
                      (goto-char (point-min))
                      (re-search-forward "^D \\([a-zA-Z0-9_.-]+\\)" nil t
                                         (string-to-number file-number))
                      (setq definition-file (ada-get-ada-file-name
                                             (match-string 1)
                                             (nth 3 identlist)
                                             ))
                      )
                  (if (re-search-backward "^X [0-9]+ \\([a-zA-Z0-9_.-]+\\)"
                                          nil t)
                      (setq definition-file (ada-get-ada-file-name
                                             (match-string 1)
                                             (nth 3 identlist)
                                             )))
                  )

                (ada-xref-change-buffer definition-file
                                        definition-line
                                        definition-col
                                        identlist other-frame))
            (error "No body found"))
          )


      ;;
      ;;  Else it seems we were not on the declaration, and thus we should
      ;;  look for it
      ;;

      ;; first look up the reference in the ali file
      (goto-char (point-min))
      (if
          (not
           (re-search-forward
            (concat  (nth 4 identlist)  ;; file number
                     "|\\([0-9]+.[0-9]+ \\)*"
                     (nth 1 identlist)       ;; line number
                     "[^0-9]"
                     (nth 2 identlist)  ;; column number
                     )
            nil t))

          ;; if we did not find it, it may be because the first reference
          ;; is not required to have a  'unit_number|'  item included.
          ;; Or maybe are we already on the declaration...
          (if (not
               (re-search-forward
                (concat "^\\([a-zA-Z0-9_.\"]+[ *]\\)*"
                        (nth 1 identlist)       ;; line number
                        "[^0-9]"
                        (nth 2 identlist)) ;; column number
                nil t))

              ;; if not, there are two possibilities :
              ;; - the declaration doesn't exist
              ;; - the file has been modified since the last .ali update
              (setq line-found nil)
            )
        )

      ;; check that we are on the correct line (the .ali file may not be
      ;; up-to-date... )
      (beginning-of-line)
      (if line-found
          (progn
            ;; while we have a continuation line, go up one line
            (while (looking-at "^\\.")
              (previous-line 1)
              (beginning-of-line))
            (if (looking-at "[0-9]+.[0-9]+[ *]\\([\"a-zA-Z][-\"+=*<>/a-zA-Z0-9_]*\\)")
                (progn
                  (if (not
                       (string=  (downcase  (match-string 1))
                                 (downcase (car identlist))))
                      (setq line-found nil))
                  ))
            )
        )

      ;; If we couldn't find the declaration, try the other algorithm.
      ;; This case should only happen if the user doesn't want us to
      ;; recompile files automatically
      (if (not line-found)
          (if (not
               (ada-xref-find-in-modified-ali identlist))
              ;;  no more idea to find the declaration. abandon
              (progn
                (kill-buffer ali-buffer)
                (error (concat "No declaration of " (car identlist) " found."))
                ))
        )

      ;; otherwise, display the appropriate buffer
      (ada-xref-goto-declaration identlist other-frame)
      )))



(defun ada-xref-goto-declaration (identlist &optional other-frame)
  "Display the buffer containing the declaration of the identifier
IDENTLIST describes the identifier (name, position, file)
if OTHER-FRAME is non nil, and `ada-xref-other-buffer' is non nil,
opens a new window to show the declaration
The current buffer is the .ali buffer, and the identifier's line must
be the current line."

  (let* ((definition-file nil)
         (definition-line nil)
         (definition-column nil))

    ;; we now have to jump to the definition place
    (beginning-of-line)

    ;; have a look at the beginning of the line
    (looking-at "\\([0-9]+\\)[a-zA-Z+]\\([0-9]+\\)[ *]")
    (setq definition-line (match-string 1))
    (setq definition-column (match-string 2))
    (if (re-search-backward "^X [0-9]+ \\([a-zA-Z0-9_.-]+\\)"
                            nil t)
        (setq definition-file (ada-get-ada-file-name
                               (match-string 1)
                               (nth 3 identlist)
                               ))
      ;; Else there was no  X .. line
      (kill-buffer (current-buffer))
      (error "Wrong format for the .ali file (couldn't find file name)")
      )

    (ada-xref-change-buffer
     definition-file
     (string-to-number definition-line)        ;; line
     (1- (string-to-number definition-column)) ;; column
     identlist other-frame)                    ;; identifier

      )
  )


(defun ada-xref-change-buffer (definition-file definition-line definition-column
                                identlist &optional other-frame)
  "When calling this function, we should be in the .ali buffer, on
the correct line. This function finds the correct file, edit it and
go to the DEFINITION-LINE line and DEFINITION-COLUMN column"

  (let ((prj-file nil)
        (declaration-buffer)
        (ali-buffer (ada-get-ali-buffer (nth 3 identlist))))

    ;; get the current prj file for the source ada file
    (save-excursion
      (set-buffer (get-file-buffer (nth 3 identlist)))
      (setq prj-file ada-prj-prj-file))

    (if ada-xref-other-buffer
        (if other-frame
            (find-file-other-frame definition-file)
          (setq declaration-buffer (find-file-noselect definition-file))
          (set-buffer declaration-buffer)
          (switch-to-buffer-other-window declaration-buffer)
          )
      (find-file definition-file)
      )

    ;; If the new buffer is not already associated with a prj file, do it
    (if (not (my-local-variable-if-set-p 'ada-prj-prj-file (current-buffer)))
        (progn
          (make-local-variable 'ada-prj-prj-file)
          (setq ada-prj-prj-file prj-file)))

    ;; move the cursor to the correct position
    (push-mark)
    (goto-line definition-line)
    (move-to-column definition-column)
    ;; make sure we have found the real declaration
    (if (not (looking-at (car identlist)))
        ;; if not, search the nearest declaration
        (ada-xref-search-nearest (car identlist))
      )

    (kill-buffer ali-buffer)))


;; the following function handles the case when the source file has
;; been modified, that is when the .ali file is no longer valid
(defun ada-xref-find-in-modified-ali (identlist)
  "searches for IDENTLIST in the .ali file.
This feature is only available when we have not used the -gnatx option.
This function is disabled when identlist is an operator, and only works
for real identifiers"

  (if (not (= (string-to-char (car identlist)) ?\"))
      (progn
        (let* ((declist '()) ;;; ( (line_in_ali file line_in_ada) ( ... ))
               (my-regexp  (concat "[ *]" (regexp-quote (car identlist)) " "))
               (line-ada "--")
               (col-ada  "--")
               (line-ali 0)
               (len 0)
               (choice 0))

          (goto-char (point-max))
          (while
              (re-search-backward my-regexp nil t)
            (save-excursion
              (setq line-ali (count-lines (point-min) (point)))
              (beginning-of-line)
              ;; have a look at the line and column numbers
              (if (looking-at "^\\([0-9]+\\).\\([0-9]+\\)[ *]")
                  (progn
                    (setq line-ada (match-string 1))
                    (setq col-ada  (match-string 2)))
                (setq line-ada "--")
                (setq col-ada  "--")
                )
              ;; construct a list with the file names and the positions within
              (if (re-search-backward "^X [0-9]+ \\([a-zA-Z0-9._-]+\\)" nil t)
                  (setq declist (cons (list line-ali
                                      (ada-get-ada-file-name
                                       (match-string 1)
                                       (nth 3 identlist)
                                       )
                                      line-ada
                                      col-ada)
                                      declist))
                )
              )
            )

          ;;
          ;; how many possible declarations ?
          ;;
          (setq len (length declist))
          (cond
           ;;
           ;; none => error
           ;;
           ((= len 0)
            (kill-buffer (ada-get-ali-buffer (nth 3 identlist)))
            (error (concat "No declaration of "
                           (car identlist)
                           " recorded in .ali file")))
           ;;
           ;; one => should be the right one
           ;;
           ((= len 1)
            (goto-line (car (nth 0 declist)))
            )
           ;;
           ;; more than one => display choice list
           ;;
           (t
            (with-output-to-temp-buffer "*choice list*"

              (princ "Identifier is overloaded and Xref information is not up to date.\n")
              (princ "Possible declarations are:\n\n")
              (princ "  no.   in file                at line  col\n")
              (princ "  ---   ---------------------     ----  ----\n")
              (let ((counter 1))
                (while (<= counter len)
                  (princ (format "  %2d)    %-21s   %4s  %4s\n"
                                 counter
                                 (nth 1 (nth (1- counter) declist))
                                 (nth 2 (nth (1- counter) declist))
                                 (nth 3 (nth (1- counter) declist))
                                 ))
                  (setq counter (1+ counter))
                  ) ; end of while
                ) ; end of let
              ) ; end of with-output-to ...
            (setq choice nil)
            (while (or
                    (not choice)
                    (not (integerp choice))
                    (< choice 1)
                    (> choice len))
              (setq choice (string-to-int
                            (read-from-minibuffer "Enter No. of your choice: "))))
            (goto-line (car (nth (1- choice) declist)))
            ))))))


;; the following function handles the case when the destination file
;; has been modified : we are then looking for the nearest
;; declaration
(defun ada-xref-search-nearest (name)
  "Searches for NAME nearest to the position recorded in the Xref file.
It returns the position of the declaration in the buffer or nil if not found."
  (let ((orgpos (point))
        (newpos nil)
        (diff nil))

    (goto-char (point-max))
    ;;
    ;; loop - look for all declarations of name in this file
    ;;
    (while (search-backward name nil t)

      ;;
      ;; check if it really is a declaration
      ;;
      (if (and
           (not (save-excursion
                  (goto-char (match-end 0))
                  (looking-at "_")))
           (not (ada-in-string-or-comment-p))
           (or
            ;; variable declaration ?
            (save-excursion
              (skip-chars-forward "a-zA-Z_0-9" )
              (ada-goto-next-non-ws)
              (looking-at ":[^=]"))
            ;; procedure, function, task or package declaration ?
            (save-excursion
              (ada-goto-previous-word)
              (looking-at "\\<[pP][rR][oO][cC][eE][dD][uU][rR][eE]\\>\\|\\<[fF][uU][nN][cC][tT][iI][oO][nN]\\>\\|\\<[tT][yY][pP][eE]\\>\\|\\<[tT][aA][sS][kK]\\>\\|\\<[pP][aA][cC][kK][aA][gG][eE]\\>\\|\\<[bB][oO][dD][yY]\\>"))))

          ;;
          ;; check if it is nearer than the ones before if any
          ;;
          (if (or (not diff)
                  (< (abs (- (point) orgpos)) diff))
              (progn
                (setq newpos (point))
                (setq diff (abs (- newpos orgpos))))))
      ) ; end of loop

    (if newpos
        (progn
          (message "ATTENTION: this declaration is only a (good) guess ...")
          (goto-char newpos))
      nil)))


;; define a function to find the other file (used by ff-find-other-file)
(defun ada-find-other-file (arg)
  "return the name of the other file"
  (ada-require-project-file)

  (save-excursion

    (let* ((ali-file (ada-find-ali-file-in-dir
                      (concat (file-name-sans-extension
                               (file-name-nondirectory arg))
                              ".ali")))
           (buffer nil)
           (list   nil))

      (if ali-file
          (progn
            (setq buffer (find-file-noselect ali-file))
            (set-buffer buffer)
            (goto-char (point-min))
            (while (re-search-forward "^U " nil t)
              (if (looking-at "[^ \t]+[ \t]+\\([^ \t]+\\)")
                  (if (not (string= (match-string 1)
                                    (file-name-nondirectory arg)))
                      (setq list (cons (match-string 1)
                                       list)))))
            (kill-buffer buffer)
            )
        )
      (if list
          list
        (list (concat (file-name-sans-extension arg)
                      (if (string= (substring arg (- (length arg)
                                                     (length ada-spec-suffix)
                                                     ))
                                              ada-spec-suffix)
                          ada-body-suffix
                        ada-spec-suffix))
      ))
  )))

;; Find the parent library file of the current file
(defun ada-goto-parent ()
  "go to the parent library file"
  (interactive)
  (ada-require-project-file)

  (let ((buffer (ada-get-ali-buffer (buffer-file-name)))
        (unit-name nil)
        (body-name nil)
        (ali-name nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^U \\([^ \t%]+\\)%[bs][ \t]+\\([^ \t]+\\)")
      (setq unit-name (match-string 1))
      (if (not (string-match "\\(.*\\)\\.[^.]+" unit-name))
          (progn
            (kill-buffer buffer)
            (error "No parent unit !"))
        (setq unit-name (match-string 1 unit-name))
        )

      ;; look for the file name for the parent unit specification
      (goto-char (point-min))
      (re-search-forward (concat "^W " unit-name
                                 "%s[ \t]+\\([^ \t]+\\)[ \t]+"
                                 "\\([^ \t\n]+\\)"))
      (setq body-name (match-string 1))
      (setq ali-name (match-string 2))
      (kill-buffer buffer)
      )

    (setq ali-name (ada-find-ali-file-in-dir ali-name))

    (save-excursion
      ;; Tries to open the new ali file to find the spec file
      (if ali-name
          (progn
            (find-file ali-name)
            (goto-char (point-min))
            (re-search-forward (concat "^U " unit-name "%s[ \t]+"
                                       "\\([^ \t]+\\)"))
            (setq body-name (match-string 1))
            (kill-buffer (current-buffer))
            )
        )
      )

    (find-file body-name)
    ))

(defun ada-xemacs-find-file ()
  "Function used by Xemacs, where find-file is not defined.
It provides a simple emulation to find-file (although we do not
try to set the cursor at the correct position"
  (interactive)
  (find-file (car (ada-find-other-file (buffer-file-name (current-buffer)))
             )))

(defun ada-make-filename-from-adaname (adaname)
  "Determine the filename of a package/procedure from its own Ada name."
  ;; this is done simply by calling `gnatkr', when we work with GNAT. It
  ;; must be a more complex function in other compiler environments.
  (let (krunch-buf)
    (setq krunch-buf (generate-new-buffer "*gkrunch*"))
    (save-excursion
      (set-buffer krunch-buf)
      ;; send adaname to external process `gnatkr'.
      (call-process "gnatkr" nil krunch-buf nil
                    adaname ada-krunch-args)
      ;; fetch output of that process
      (setq adaname (buffer-substring
                     (point-min)
                     (progn
                       (goto-char (point-min))
                       (end-of-line)
                       (point))))
      (kill-buffer krunch-buf)))
  adaname
  )


(defun ada-make-body-gnatstub ()
  "Create an Ada package body in the current buffer.
This function uses the `gnatstub' program to create the body.
This function typically is to be hooked into `ff-file-created-hooks'."
  (interactive)

  (save-some-buffers nil nil)

  (ada-require-project-file)

  (delete-region (point-min) (point-max))

  ;; Call the external process gnatstub
  (let* ((gnatstub-opts (ada-treat-cmd-string ada-gnatstub-opts))
         (filename      (buffer-file-name (car (cdr (buffer-list)))))
         (output        (concat (file-name-sans-extension filename) ".adb"))
         (gnatstub-cmd  (concat "gnatstub " gnatstub-opts " " filename))
         (buffer        (get-buffer-create "*gnatstub*")))

    (save-excursion
      (set-buffer buffer)
      (compilation-minor-mode 1)
      (erase-buffer)
      (insert gnatstub-cmd)
      (newline)
      )
    ;; call gnatstub to create the body file
    (call-process shell-file-name nil buffer nil "-c" gnatstub-cmd)

    (if (save-excursion
          (set-buffer buffer)
          (goto-char (point-min))
          (search-forward "gnatstub: command not found" nil t))
        (progn
          (message "gnatstub was not found -- using the basic algorithm")
          (sleep-for 2)
          (kill-buffer buffer)
          (ada-make-body))

      ;; Else clean up the output

      ;;  Kill the temporary buffer created by find-file
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))

      (if (file-exists-p output)
          (progn
            (find-file output)
            (kill-buffer buffer))

        ;; display the error buffer
        (display-buffer buffer)
        )
      )))


(defun ada-xref-initialize ()
  "Function called by ada-mode-hook to initialize the ada-xref.el package.
For instance, it creates the gnat-specific menus, set some hooks for
find-file...."
  (ada-add-ada-menu)
  (make-local-hook 'ff-file-created-hooks)
  (setq ff-file-created-hooks 'ada-make-body-gnatstub)

  ;; Read the project file and update the search path
  ;; before looking for the other file
  (make-local-hook 'ff-pre-find-hooks)
  (add-hook 'ff-pre-find-hooks 'ada-require-project-file)

  ;; Completion for file names in the mini buffer should ignore .ali files
  (add-to-list 'completion-ignored-extensions ".ali")
  )


;; ----- Add to ada-mode-hook ---------------------------------------------

;;  Set the keymap once and for all, so that the keys set by the user in his
;;  config file are not overwritten every time we open a new file.
(ada-add-keymap)

(add-hook 'ada-mode-hook 'ada-xref-initialize)

(provide 'ada-xref)

;;; ada-xref.el ends here
