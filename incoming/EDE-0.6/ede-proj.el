;;; ede-proj.el --- EDE Generic Project file driver

;;;  Copyright (C) 1998, 1999  Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make
;; RCS: $Id: ede-proj.el,v 1.15 1999/05/22 14:29:25 zappo Exp $

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; EDE defines a method for managing a project.  EDE-PROJ aims to be a
;; generic project file format based on the EIEIO object stream
;; methods.  Changes in the project structure will require Makefile
;; rebuild.  The targets provided in ede-proj can be augmented with
;; additional target types inherited directly from `ede-proj-target'.

(require 'ede)

;;; Class Definitions:
(defclass ede-proj-target (ede-target)
  ((auxsource :initarg :auxsource
	      :custom (repeat (string :tag "File"))
	      :documentation "Auxilliary source files included in this target.
Each of these is considered equivalent to a source file, but it is not
distributed, and each should have a corresponding rule to build it.")
   (dirty :initform nil
	  :documentation "Non-nil when generated files needs updating.")
   )
  "Abstract class for ede-proj targets.")

(defclass ede-proj-target-makefile (ede-proj-target)
  ((makefile :initarg :makefile
	     :initform "Makefile"
	     :custom string
	     :documentation "File name of generated Makefile.")
   (configuration-variables
    :initarg :configuration-variables
    :initform nil
    :custom (repeat (cons (string :tag "Configuration")
			  (repeat
			   (cons (string :tag "Name")
				 (string :tag "Value")))))
    :documentation "Makefile variables appended to use in different configurations.
These variables are used in the makefile when a configuration becomes active.
Target variables are always renamed such as foo_CFLAGS, then included into
commands where the variable would usually appear.")
   (rules :initarg :rules
	  :initform nil
	  :custom (repeat (object :objecttype ede-makefile-rule))
	  :documentation
	  "Arbitrary rules and dependencies needed to make this target.")
   )
  "Abstract class for Makefile based targets.")

(defclass ede-proj-target-makefile-objectcode (ede-proj-target-makefile)
  (;; Give this a new default
   (configuration-variables :initform ("debug" . (("CFLAGS" . "-g")
						  ("LDFLAGS" . "-g"))))
   (headers :initarg :headers
	    :initform nil
	    :custom (repeat (string :tag "Header"))
	    :documentation "Header files included in the distribution.
EDE generated Makefiles make all sources dependent on all header files.
In automake mode, these headers are ignored, but automake generates
dependencies automatically.")
   )
  "Abstract class for Makefile based object code generating targets.
Belonging to this group assumes you could make a .o from an element source
file.")

(defclass ede-proj-target-makefile-program
  (ede-proj-target-makefile-objectcode)
  ((ldlibs :initarg :ldlibs
	   :initform nil
	   :custom (repeat (string :tag "Library"))
	   :documentation
	   "Libraries, such as \"m\" or \"Xt\" which this program dependso on.
The linker flag \"-l\" is automatically prepended.  Do not include a \"lib\"
prefix, or a \".so\" suffix."
	   )
   (ldflags :initarg :ldflags
	    :initform nil
	    :custom (repeat (string :tag "Link Flag"))
	    :documentation
	    "Additional flags to add when linking this target.
Use ldlibs to add addition libraries.  Use this to specify specific
options to the linker.")
   )
   "This target is an executable program.")

(defclass ede-proj-target-makefile-archive
  (ede-proj-target-makefile-objectcode)
  ()
  "This target generates an object code archive.")

(defclass ede-proj-target-makefile-shared-object
  (ede-proj-target-makefile-program)
  ((ldflags :custom (repeat (string :tag "Libtool flag"))
	    :documentation
	    "Additional flags to add when linking this target with libtool.
Use ldlibs to add addition libraries.")
   (libtool :initarg :libtool
	    :initform nil
	    :custom boolean
	    :documentation
	    "Non-nil if libtool should be used to generate the library.")
   )
  "This target generates a shared library using libtool.")

(defclass ede-proj-target-makefile-info (ede-proj-target-makefile)
  ((mainmenu :initarg :mainmenu
	     :initform ""
	     :custom string
	     :documentation "The main menu resides in this file.
All other sources should be included independently."))
  "Target for a single info file.")
   
(defclass ede-proj-target-lisp (ede-proj-target-makefile)
  ((load-path :initarg :load-path
	      :initform nil
	      :custom (repeat string)
	      :documentation "Additional load-path arguments.
When compiling from the command line, these are added to the makefile.
When compiling from within emacs, these are ignored.")
   (requirements :initarg :requirements
		 :initform nil
		 :custom (repeat string)
		 :documentation
		 "Additional packages that should be loaded before building.
When using eieio, tools generally need to be loaded before you can compile
them safely.")
   )
  "This target consists of a group of lisp files.
A lisp target may be one general program with many separate lisp files in it.")

(defclass ede-proj-target-scheme (ede-proj-target)
  ((interpreter :initarg :interpreter
		:initform "guile"
		:custom string
		:documentation "The preferred interpreter for this code.")
   )
  "This target consists of scheme files.")

(defclass ede-proj-target-aux (ede-proj-target)
  ()
  "This target consists of aux files such as READMEs and COPYING.")

(defvar ede-proj-target-alist
  '(("program" . ede-proj-target-makefile-program)
    ("archive" . ede-proj-target-makefile-archive)
    ("sharedobject" . ede-proj-target-makefile-shared-object)
    ("emacs lisp" . ede-proj-target-lisp)
    ("info" . ede-proj-target-makefile-info)
    ("auxiliary" . ede-proj-target-aux)
    ("scheme" . ede-proj-target-scheme)
    )
  "Alist of names to class types for available project target classes.")

(defclass ede-makefile-rule ()
  ((target :initarg :target
	   :initform ""
	   :custom string
	   :documentation "The target pattern.
A pattern of \"%.o\" is used for inference rules, and would match object files.
A target of \"foo.o\" explicitly matches the file foo.o.")
   (dependencies :initarg :dependencies
		 :initform ""
		 :custom string
		 :documentation "Dependencies on this target.
A pattern of \"%.o\" would match a file of the same prefix as the target
if that target is also an inference rule pattern.
A dependency of \"foo.c\" explicitly lists foo.c as a dependency.
A variable such as $(name_SOURCES) will list all the source files
belonging to the target name.")
   (rules :initarg :rules
	  :initform nil
	  :custom (repeat string)
	  :documentation "Scripts to execute.
These scripst will be executed in sh (Unless the SHELL variable is overriden).
Do not prefix with TAB.")
   (phony :initarg :phony
	  :initform nil
	  :custom boolean
	  :documentation "Is this a phony rule?
Adds this rule to a .PHONY list."))
  "A single rule for building some target.")

(defclass ede-proj-project (ede-project)
  ((makefile-type :initarg :makefile-type
		  :initform 'Makefile
		  :custom (choice (const Makefile)
				  ;(const Makefile.in)
				  (const Makefile.am)
				  ;(const cook)
				  )
		  :documentation "The type of Makefile to generate.
Can be one of 'Makefile, 'Makefile.in, or 'Makefile.am.
If this value is NOT 'Makefile, then that overrides the :makefile slot
in targets.")
   (variables :initarg :variables
	      :initform nil
	      :custom (repeat (cons (string :tag "Name")
				    (string :tag "Value")))
	      :documentation "Variables to set in this Makefile.")
   (configuration-variables
    :initarg :configuration-variables
    :initform ("debug" (("DEBUG" . "1")))
    :custom (repeat (cons (string :tag "Configuration")
			  (repeat
			   (cons (string :tag "Name")
				 (string :tag "Value")))))
    :documentation "Makefile variables to use in different configurations.
These variables are used in the makefile when a configuration becomes active.")
   (inference-rules :initarg :inference-rules
		    :initform nil
		    :custom (repeat
			     (object :objecttype ede-makefile-rule))
		    :documentation "Inference rules to add to the makefile.")
   (automatic-dependencies
    :initarg :automatic-dependencies
    :initform t
    :custom boolean
    :documentation
    "Non-nil to do implement automatic dependencies in the Makefile.")
   )
  "The EDE-PROJ project definition class.")

;;; Code:
(defun ede-proj-load (project)
  "Load a project file PROJECT."
  (save-excursion
    (let ((ret nil)
	  (subdirs (directory-files project nil "[^.].*" nil)))
      (set-buffer (get-buffer-create " *tmp proj read*"))
      (unwind-protect
	  (progn
	    (erase-buffer)
	    (insert-file (concat project "Project.ede"))
	    (goto-char (point-min))
	    (setq ret (read (current-buffer)))
	    (if (not (eq (car ret) 'ede-proj-project))
		(error "Corrupt project file"))
	    (setq ret (eval ret))
	    (oset ret file (concat project "Project.ede")))
	(kill-buffer " *tmp proj read*"))
      (while subdirs
	(let ((sd (concat project (car subdirs))))
	  (if (and (file-directory-p sd)
		   (ede-directory-project-p (concat sd "/")))
	      (oset ret subproj (cons (ede-proj-load (concat sd "/"))
				      (oref ret subproj))))
	  (setq subdirs (cdr subdirs))))
      ret)))

(defun ede-proj-save (&optional project)
  "Write out object PROJECT into its file."
  (save-excursion
    (if (not project) (setq project (ede-current-project)))
    (let ((b (set-buffer (get-buffer-create " *tmp proj write*")))
	  (cfn (oref project file)))
      (unwind-protect
	  (save-excursion
	    (erase-buffer)
	    (let ((standard-output (current-buffer)))
	      (oset project file (file-name-nondirectory cfn))
	      (object-write project ";; EDE project file."))
	    (write-file (oref project file) nil)
	    )
	;; Restore the :file on exit.
	(oset project file cfn)
	(kill-buffer b)))))

(defmethod ede-commit-local-variables ((proj ede-proj-project))
  "Commit change to local variables in PROJ."
  (ede-proj-save proj))

(defmethod eieio-done-customizing ((proj ede-proj-project))
  "Call this when a user finishes customizing this object.
Argument PROJ is the project to save."
  (call-next-method)
  (ede-proj-save proj))

(defmethod eieio-done-customizing ((target ede-proj-target))
  "Call this when a user finishes customizing this object.
Argument TARGET is the project we are completing customization on."
  (call-next-method)
  (ede-proj-save (ede-current-project)))

(defmethod ede-commit-project ((proj ede-proj-project))
  "Commit any change to PROJ to its file."
  (ede-proj-save proj))

(defmethod ede-find-target ((proj ede-proj-project) buffer)
  "Fetch the target in PROJ belonging to BUFFER or nil."
  (or ede-object
      (if (ede-buffer-mine proj buffer)
	  proj
	(let ((targets (oref proj targets))
	      (f nil))
	  (while targets
	    (if (or (member (ede-convert-path proj (buffer-file-name buffer))
			    (oref (car targets) source))
		    (and (slot-exists-p (car targets) 'headers)
			 (member
			  (ede-convert-path proj (buffer-file-name buffer))
			  (oref (car targets) headers)))
		    (and (slot-exists-p (car targets) 'auxsource)
			 (member
			  (ede-convert-path proj (buffer-file-name buffer))
			  (oref (car targets) auxsource))))
		(setq f (cons (car targets) f)))
	    (setq targets (cdr targets)))
	  f))))

(defmethod ede-buffer-mine ((this ede-proj-project) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (string= (oref this file)
	   (ede-convert-path this (buffer-file-name buffer))))

;;; Desireous methods
;;
(defmethod ede-want-file-p ((obj ede-proj-target-aux) file)
  "Return t if OBJ wants to own FILE."
  (string-match "README\\|\\.txt$" file))

(defmethod ede-want-file-p ((obj ede-proj-target-scheme) file)
  "Return t if OBJ wants to own FILE."
  (string-match "\\.scm$" file))

(defmethod ede-want-file-p ((obj ede-proj-target-lisp) file)
  "Return t if OBJ wants to own FILE."
  (string-match "\\.el$" file))

(defmethod ede-want-file-p ((obj ede-proj-target-makefile-info) file)
  "Return t if OBJ wants to own FILE."
  (string-match "\\.texi?$" file))

(defmethod ede-want-file-p ((obj ede-proj-target-makefile-objectcode) file)
  "Return t if OBJ wants to own FILE."
  ;; Only C targets for now.  I'll figure out more later.
  (string-match "\\.\\(c\\|C\\|cc\\|cpp\\|CPP\\|h\\|hh\\|hpp\\)$"
		file))


;;; EDE command functions
;;
(defvar ede-proj-target-history nil
  "History when querying for a target type.")

(defmethod project-new-target ((this ede-proj-project))
  "Create a new target in THIS based on the current buffer."
  (let* ((name (read-string "Name: " ""))
	 (type (completing-read "Type: " ede-proj-target-alist
				nil t nil '(ede-proj-target-history . 1)))
	 (ot nil)
	 (src (if (and (buffer-file-name)
		       (y-or-n-p (format "Add %s to %s? " (buffer-name) name)))
		  (buffer-file-name))))
    (setq ot (funcall (cdr (assoc type ede-proj-target-alist)) name :name name
		      :path (ede-convert-path this default-directory)
		      :source (if src
				  (list (file-name-nondirectory src))
				nil)))
    ;; If we added it, set the local buffer's object.
    (if src (setq ede-object ot))
    ;; Add it to the project object
    (oset this targets (cons ot (oref this targets)))
    ;; And save
    (ede-proj-save this)))

(defmethod project-delete-target ((this ede-proj-target))
  "Delete the current target THIS from it's parent project."
  (let ((p (ede-current-project))
	(ts (oref this source)))
    ;; Loop across all sources.  If it exists in a buffer,
    ;; clear it's object.
    (while ts
      (let* ((default-directory (oref this path))
	     (b (get-file-buffer (car ts))))
	(if b
	    (save-excursion
	      (set-buffer b)
	      (if (eq ede-object this)
		  (setq ede-object nil)))))
      (setq ts (cdr ts)))
    ;; Remove THIS from it's parent.
    ;; The two vectors should be pointer equivalent.
    (oset p targets (delq this (oref p targets)))
    (ede-proj-save (ede-current-project))))

(defmethod project-add-file ((this ede-proj-target) file)
  "Add to target THIS the current buffer represented as FILE."
  (setq file (file-name-nondirectory file))
  (if (not (member file (oref this source)))
      (oset this source (append (oref this source) (list file))))
  (ede-proj-save (ede-current-project)))

(defmethod project-add-file ((this ede-proj-target-makefile-objectcode) file)
  "Add to target THIS the current buffer represented as FILE."
  (setq file (file-name-nondirectory file))
  ;; Header files go into auxiliary sources.  Add more for more languages.
  (if (not (string-match "\\.\\(h\\|hh\\)$" file))
      ;; No match, add to regular sources.
      (call-next-method)
    (if (not (member file (oref this headers)))
	(oset this headers (append (oref this headers) (list file))))
    (ede-proj-save (ede-current-project))))

(defmethod project-remove-file ((target ede-proj-target) file)
  "For TARGET, remove FILE.
FILE must be massaged by `ede-convert-path'."
  ;; Speedy delete should be safe.
  (oset target source (delete (file-name-nondirectory file)
			       (oref target source)))
  (ede-proj-save))

(defmethod project-remove-file ((target ede-proj-target-makefile-objectcode)
				file)
  "For TARGET, remove FILE.
FILE must be massaged by `ede-convert-path'."
  ;; Speedy delete should be safe.
  (oset target headers (delete (file-name-nondirectory file)
			       (oref target headers)))
  ;; This will do sources, and save the project for us.
  (call-next-method))

(defmethod project-make-dist ((this ede-proj-project))
  "Build a distribution for the project based on THIS target."
  ;; I'm a lazy bum, so I'll make a makefile for doing this sort
  ;; of thing, and rely only on that small section of code.
  (let ((pm (ede-proj-dist-makefile this)))
    (ede-proj-makefile-create-maybe this pm)
    (compile (concat "make -f " pm " dist"))))

(defmethod project-compile-project ((proj ede-proj-project) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  (let ((pm (ede-proj-dist-makefile proj))
	(default-directory (file-name-directory (oref proj file))))
    (ede-proj-makefile-create-maybe proj pm)
    (compile (concat "make -f " pm " all"))))

;;; Target type specific compilations/debug
;;
(defmethod project-compile-target ((obj ede-proj-target) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (project-compile-project (ede-current-project) command))

(defmethod project-compile-target ((obj ede-proj-target-lisp))
  "Compile all sources in a Lisp target OBJ."
  (mapcar (lambda (src)
	    (let ((elc (concat (file-name-sans-extension src) ".elc")))
	      (if (or (not (file-exists-p elc))
		      (file-newer-than-file-p src elc))
		  (byte-compile-file src))))
	  (oref obj source)))

(defmethod project-compile-target ((obj ede-proj-target-makefile)
				   &optional command)
  "Compile the current target program OBJ.
Optional argument COMMAND is the s the alternate command to use."
  (ede-proj-makefile-create-maybe (ede-current-project)
				  (oref obj makefile))
  (compile (concat "make -f " (oref obj makefile) " " (ede-name obj))))

(defmethod project-debug-target ((obj ede-proj-target))
  "Run the current project target OBJ in a debugger."
  (error "Debug-target not supported by %s" (object-name obj)))

(defmethod project-debug-target ((obj ede-proj-target-makefile-program))
  "Debug a program target OBJ."
  (let ((tb (get-buffer-create " *padt*"))
	(dd (if (not (string= (oref obj path) ""))
		(oref obj path)
	      default-directory))
	(cmd nil))
    (unwind-protect
	(progn
	  (set-buffer tb)
	  (setq default-directory dd)
	  (setq cmd (read-from-minibuffer
		     "Run (like this): "
		     (concat (symbol-name ede-debug-program-function)
			     " " (ede-target-name obj))))
	  (funcall ede-debug-program-function cmd))
      (kill-buffer tb))))


;;; Target type specific autogenerating gobbldegook.
;;
(defun ede-proj-makefile-type ()
  "Makefile type of the current project."
  (oref (ede-current-project) makefile-type))

(defun ede-proj-automake-p ()
  "Return non-nil if the current project is automake mode."
  (eq (ede-proj-makefile-type) 'Makefile.am))

(defun ede-proj-autoconf-p ()
  "Return non-nil if the current project is automake mode."
  (eq (ede-proj-makefile-type) 'Makefile.in))

(defun ede-proj-make-p ()
  "Return non-nil if the current project is automake mode."
  (eq (ede-proj-makefile-type) 'Makefile))

(defmethod ede-proj-dist-makefile ((this ede-proj-project))
  "Return the name of the Makefile with the DIST target in it for THIS."
  (cond ((eq (oref this makefile-type) 'Makefile.am)
	 "Makefile.am")
	((eq (oref this makefile-type) 'Makefile.in)
	 "Makefile.in")
	((object-assoc "Makefile" 'makefile (oref this targets))
	 "Makefile")
	(t
	 (with-slots (targets) this
	   (while (and targets
		       (not (obj-of-class-p
			     (car targets)
			     'ede-proj-target-makefile)))
	     (setq targets (cdr targets)))
	   (if targets (oref (car targets) makefile)
	     "Makefile")))))
  
(defun ede-proj-regenerate ()
  "Regenerate Makefiles for and edeproject project."
  (interactive)
  (ede-proj-makefile-create
   (ede-current-project)
   ;; Just do this one for now.  Come up with a way to do all potential
   ;; makefiles in the future.
   (ede-proj-dist-makefile (ede-current-project))))

(eval-when-compile (require 'ede-pmake))

(defmethod ede-proj-makefile-create-maybe ((this ede-proj-project) mfilename)
  "Create a Makefile for all Makefile targets in THIS if needed.
MFILENAME is the makefile to generate."
  ;; For now, pass through until dirty is implemented.
  (require 'ede-pmake)
  (if (file-newer-than-file-p (oref this file) mfilename)
      (ede-proj-makefile-create this mfilename)))

;;; Lower level overloads
;;  
(defmethod project-rescan ((this ede-proj-project))
  "Rescan the EDE proj project THIS."
  (ede-with-projectfile this
    (goto-char (point-min))
    (let ((l (read (current-buffer)))
	  (fields (obj-fields this))
	  (targets (oref this targets)))
      (setq l (cdr (cdr l))) ;; objtype and name skip
      (while fields ;  reset to defaults those that dont appear.
	(if (and (not (assoc (car fields) l))
		 (not (eq (car fields) 'file)))
	    (oset-engine this (car fields)
			 (oref-default-engine this (car fields))))
	(setq fields (cdr fields)))
      (while l
	(let ((field (car l)) (val (car (cdr l))))
	  (cond ((eq field targets)
		 (let ((targets (oref this targets))
		       (newtarg nil))
		   (setq val (cdr val)) ;; skip the `list'
		   (while val
		     (let ((o (object-assoc (car (cdr (car val))) ; name
					    'name targets)))
		       (if o
			   (project-rescan o (car val))
			 (setq o (eval (car val))))
		       (setq newtarg (cons o newtarg)))
		     (setq val (cdr val)))
		   (oset this targets newtarg)))
		(t
		 (oset-engine this field val))))
	(setq l (cdr (cdr l))))))) ;; field/value
	
(defmethod project-rescan ((this ede-proj-target) readstream)
  "Rescan target THIS from the read list READSTREAM."
  (setq readstream (cdr (cdr readstream))) ;; constructor/name
  (while readstream
    (let ((tag (car readstream))
	  (val (car (cdr readstream))))
      (oset-engine this tag val))
    (setq readstream (cdr (cdr readstream)))))

(add-to-list 'auto-mode-alist '("Project\\.ede" . emacs-lisp-mode))

(provide 'ede-proj)

;;; ede-proj.el ends here
