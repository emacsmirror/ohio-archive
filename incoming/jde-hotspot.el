;;jde-hotspot.el - hot spot support for JDE

;;This file is not part of emacs


;;Author: Phillip Lord <p.lord@hgmp.mrc.ac.uk>
;;Maintainer: Phillip Lord

;;Copyright (c) 1999 Phillip Lord

;; COPYRIGHT NOTICE
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

;;; Status
;;
;; This software was designed, written and tested on a win95 machine
;; using NTEmacs, and JDE 2.1.6 beta 5 release. It will probably work
;; on other versions of JDE, but may not. The current version should
;; be available at http://genetics.ich.ucl.ac.uk/plord

;;;Installation
;;
;; This package requires the presence of the JDE. Place the package
;; in your load-path and then add 
;;
;; (require 'jde-hotspot) 
;;
;; to your .emacs file

;;;Description
;; 
;; Provides support for all of the hotspot options on the 
;; command line. Because hotspot requires the "-classic" tag 
;; first if it is going to be used I have to hack around with 
;; the core JDE functions, which this file over-writes. 


(require 'jde)
(defcustom jde-run-java-specify-classic-vm nil
  "*Specify the classic vm.
If this variable is non-nil the classic vm is specified on the command line.
If the HotSpot virtual machine is installed it is used by default
but can be specifically disabled by specifing the -classic argument first
on the command line"
  :group 'jde-project
  :type 'boolean)


(defcustom jde-run-option-mixed-mode-execution nil
  "*Specify mixed mode execution.
Enable mixed mode execution on the command line. This argument 
requires installation of the HotSpot virtual machine. It is
disabled by setting `jde-run-java-specify-classic-vm'. Its
set on by default so this seems a pointless argument"
  :group 'jde-run-options
  :type 'boolean)

(defcustom jde-run-option-interpreted-mode-execution nil
  "*Specify interpreted mode execution.
Enables interpreted mode execution. This argument requires
installation of the HotSpot virtual machine. It is 
disabled by setting `jde-run-java-specify-classic-vm'."
  :group 'jde-run-options
  :type 'boolean)

(defcustom jde-run-option-incremental-garbage-collection nil
  "*Specify incremental garbage collection.
Enables incremental garbage collection. This argument requires
installation of the HotSpot virtual machine. It is 
disabled by setting `jde-run-java-specify-classic-vm'."
  :group 'jde-run-options
  :type 'boolean)



;;;This function below is stolen straight from the JDE and is
;;;copyright by Paul Kinnucan, except for the bits that I have 
;;;changed!
(defun jde-run-get-vm-args ()
  "Builds a command-line argument string to pass to the Java vm.
This function builds the string from the values of the JDE
Run Option panel variables."
  (let (options
	(memory-unit-abbrevs
	 (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m"))))


    ;; Set the classpath option. Use the local
    ;; classpath, if set; otherwise, the global
    ;; classpath.
    (if jde-run-option-classpath
	(setq options
	      (list "-classpath"
		    (jde-run-build-classpath-arg
		     jde-run-option-classpath)))
      (if jde-global-classpath
	  (setq options
		(list "-classpath"
		      (jde-run-build-classpath-arg
		       jde-global-classpath)))))
    

    ;;Specify use of the classic vm on the command line
    ;;This has to be placed as the first argument or it wont work!
    (if jde-run-java-specify-classic-vm (setq options
					       (nconc (list "-classic" )
						      options)))
    
    
    ;;Mixed mode execution is set (pretty pointless as this is the
    ;;default)
    (if (not jde-run-java-specify-classic-vm)
	(if jde-run-option-mixed-mode-execution
	    (setq options (nconc (list "-Xmixed") options))))
    
    ;;Interpreted mode execution
    (if (not jde-run-java-specify-classic-vm)
	(if jde-run-option-interpreted-mode-execution
	    (setq options (nconc (list "-Xint") options))))

    ;;Incremental garbage collection
    (if (not jde-run-java-specify-classic-vm)
	(if jde-run-option-incremental-garbage-collection
	    (setq options (nconc (list "-Xincgc") options))))

    ;; Set the verbose options.
    (let ((print-classes-loaded
	   (nth 0 jde-run-option-verbose))
	  (print-memory-freed
	   (nth 1 jde-run-option-verbose))
	  (print-jni-info
	   (nth 2 jde-run-option-verbose)))
      (if print-classes-loaded
	  (setq options (nconc options (list "-v"))))
      (if print-memory-freed
	  (setq options (nconc options '("-verbosegc"))))
      (if print-jni-info
	  (setq options (nconc options '("-verbosejni")))))

    ;; Set properties arguments.
    (if jde-run-option-properties
	(let ((count (length jde-run-option-properties))
	      (n 0))
	  (while (< n count)
	    (let ((prop (nth n jde-run-option-properties)))
	      (setq options 
		    (nconc options
			   (list (concat "-D" (car prop) "=" (cdr prop))))))    
	    (setq n (1+ n)))))

    ;; Set heap size options.
    (let* ((start-cons (nth 0 jde-run-option-heap-size))
	   (start-size (format "%d%s" (car start-cons) 
			       (cdr (assoc (cdr start-cons)
				      memory-unit-abbrevs))))
	   (max-cons (nth 1 jde-run-option-heap-size))
	   (max-size (format "%d%s" (car max-cons) 
			     (cdr (assoc (cdr max-cons)
				    memory-unit-abbrevs)))))
      (if (not (string= start-size "1m"))
	  (setq options 
		(nconc options (list (concat "-Xms" start-size)))))
      (if (not (string= max-size "16m"))
	  (setq options 
		(nconc options (list (concat "-Xmx" max-size))))))

    ;; Set stack size options.
    (let* ((c-cons (nth 0 jde-run-option-stack-size))
	   (c-size (format "%d%s" (car c-cons) 
			       (cdr (assoc (cdr c-cons)
				      memory-unit-abbrevs))))
	   (java-cons (nth 1 jde-run-option-stack-size))
	   (java-size (format "%d%s" (car java-cons) 
			     (cdr (assoc (cdr java-cons)
				    memory-unit-abbrevs)))))
      (if (not (string= c-size "128k"))
	  (setq options 
		(nconc options (list (concat "-Xss" c-size)))))
      (if (not (string= java-size "400k"))
	  (setq options 
		(nconc options (list (concat "-Xoss" java-size))))))

    ;; Set garbage collection options.
    (let ((no-gc-asynch (not 
			 (nth 0 jde-run-option-garbage-collection)))
	  (no-gc-classes (not 
			  (nth 1 jde-run-option-garbage-collection))))
      (if no-gc-asynch
	  (setq options (nconc options '("-Xnoasyncgc"))))
      (if no-gc-classes
	  (setq options (nconc options '("-Xnoclassgc")))))

    ;; Set Java profile option.
    (let ((profilep (car jde-run-option-java-profile))
	  (file (cdr jde-run-option-java-profile)))
      (if profilep
	  (if (string= file "./java.prof")
	      (setq options (nconc options '("-Xprof")))
	    (setq options 
		  (nconc options 
			 (list (concat "-Xprof:" file)))))))

    ;; Set heap profile option.
    (let* ((profilep (car jde-run-option-heap-profile))
	   (prof-options (cdr jde-run-option-heap-profile))
	   (file (nth 0 prof-options))
	   (depth (nth 1 prof-options))
	   (top (nth 2 prof-options))
	   (sort 
	    (downcase (substring (nth 3 prof-options) 0 1))))
      (if profilep
	  (if (and (string= file "./java.hprof")
		   (equal depth 5)
		   (equal top 20)
		   (string= sort "a"))
	      (setq options (nconc options '("-Xhprof")))
	    (setq options
		  (nconc options
			 (list
			  (format 
			   "-Xhprof:file=%s,depth=%d,top=%d,sort=%s"
			   file depth top sort)))))))

    ;; Set verify options.
    (let ((verify-all (nth 0 jde-run-option-verify))
	  (verify-remote (nth 1 jde-run-option-verify)))
      (if verify-all
	  (setq options (nconc options '("-Xverify"))))
;      (if verify-remote
;	  (setq options (concat options "-Xverifyremote")))
      (if (and
	   (not verify-all)
	   (not verify-remote))
	  (setq options (nconc options '("-Xnoverify")))))

    ;; Set command line args.
    (if jde-run-option-vm-args
	(let ((len (length jde-run-option-vm-args))
	      (n 0))
	  (while (< n len)
	    (setq options (nconc options
				 (jde-run-parse-args
				  (nth n jde-run-option-vm-args))))
	    (setq n (1+ n)))))
	      
    options))




(provide 'jde-hotspot)


