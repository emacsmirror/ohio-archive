;; jde-cflow.el - java control statement expander
;; $Revision: 1.12 $
;; $Date: 2000/06/19 10:18:54 $

;; This file is not part of Emacs
;; Nor is it part of the JDE despite its name

;; Authors: Eric Friedman <ericdfriedman@hotmail.com>
;; Authors: Phillip Lord <plord@hgmp.mrc.ac.uk>
;; Maintainer: Phillip Lord

;; Copyright (c) 1999 Phillip Lord
;; Copyright (c) 1998 Eric Friedman

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
;; THIS SOFTWARE IS CURRENTLY CONSIDERED TO BE OF BETA QUALITY. 
;; 
;; The software was designed, written and tested on windows95 using 
;; NTEmacs, and a JDE 2.1.6 beta release. Please let me know if you 
;; get it working elsewhere. The current version should be available
;; at http://genetics.ich.ucl.ac.uk/plord
;;
;; It should also be noted that a slightly different version of this
;; package has now become incorporated into the main body of the JDE
;; so perhaps this software is a little bit redundant now. 
;;

;;; Installation
;;
;; This package requires the presence of the JDE. (Not for 
;; very much, but I would think anyone wanting to use this 
;; software would also be using the JDE). Install the JDE first
;; (see sunsite.auc.dk/jde).
;; Then place this file in your load path, and the following
;; in your .emacs file
;;
;;  (require 'jde-cflow)
;;
;; The standard behaviour is just to expand the templates. If
;; you want the prompts to work then you need to add
;; 
;; (setq tempo-interactive t)
;; 
;; or alternatively customise this variable to any non-nil value. 
;;

;;; Description
;;
;; This package automatically expands various abbrevations
;; into templates for the various control statements within java.
;; It uses tempo templates for this purpose, and is fully 
;; customisable so that you can change the templates as you wish.
;; or the abbreviations if you want. The command M-x jde-cflow-customize
;; enters this mode. 

;;; Note to jde-cflow.el version 1 users
;;
;; This package should simply overwrite the previous versions
;; abbreviations. If it doesnt you may want to use M-x edit-abbrevs 
;; to remove the old ones and then install this again. 

;;; Defining new templates
;;
;; Some people might want to add new templates. At the moment there is
;; no support for this, and Im not sure how to add it easily as custom
;; doesnt (to my knowledge) allow addition of new customisable
;; variables with itself (if anyone knows how to do this
;; "meta-customization" I would be pleased to hear). At the bottom of
;; this code there are some comments which can be copied, and used
;; though. Its a standard template.

;;; Limitations (or bugs depending on your point of view)
;;
;; 1) Long conditions statements in many control statements look messy 
;; by spreading across several lines. Because these are prompted for its
;; not possible to automaticaly new line these. It also messes up the comments
;; which suffer from the same problem. I hope to improve this by partially
;; parsing the clauses and putting in a few judicious new lines. 
;; 2) The way in which the abbreviation mappings are customised is 
;; a little bit clunky, but I dont know how to improve it. 
;; 3) The user gets prompted if they want to save the abbrevs file a little 
;; more frequently than is necessary. Various changes have been made which 
;; mean appears to occur only once per emacs session (and of course if
;; you reset the abbrevs!). Not perfect but better. 
;;

;;;Credits
;;
;; Eric Friedman wrote the original version of this package and 
;; was responsible for the idea, and templates. 
;; I (PL) added the the customisability and the documentation. 
;;

(defconst jde-cflow-version "$Revision: 1.12 $"
  "jde-cflow version number.")

(defun jde-cflow-version-number()
  "Returns jde-cflow version number."
  (string-match "[0123456789.]+" jde-cflow-version)
  (match-string 0 jde-cflow-version))

(defvar jde-cflow-tags nil
  "Tempo tags for JDE mode")

(defgroup jde-cflow nil
  "JDE Control Statements Expander"
  :group 'jde
  :prefix "jde-cflow")

(defun jde-cflow-customize()
  "Customisation of the group jde-cflow."
  (interactive)
  (customize-group "jde-cflow"))

(require 'jde)
(require 'tempo)
(add-hook 'jde-mode-hook '(lambda ()
                          (tempo-use-tag-list 'jde-cflow-tags)))


;;; JDE-Mode Templates
(defcustom jde-cflow-if-statement-template
  '("> \"if (\" (p \"if-clause: \" clause) \")\"  n>" 
      "\"{\" > n> r n" 
      "\"} // end of if (\" (s clause) \")\" > n>" )
  "*Template for the if statement."
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-cflow-if-statement
	    (tempo-define-template 
	     "jde-cflow-if-statement-template"
	     (jde-gen-read-template val)
	     nil
	     "Insert skeleton for if statement"
	     'jde-cflow-tags))
	  (set-default sym val)))


(defcustom jde-cflow-else-statement-template
  '("> \"else\" n>"
    "\{\" > n> r n" 
    "\"} // end of else" "> n>")
  "*Template for else statement."
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-else-statement
 	    (tempo-define-template 
 	     "jde-cflow-else-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton for else statement"
	     'jde-cflow-tags))
 	  (set-default sym val)))

(defcustom jde-cflow-if-else-statement-template
  '( "> \"if (\" (p \"if-clause: \" clause) \")\"  n>" 
     "\"{\" > n> r n "
     "\"} // end of if (\" (s clause) \")\" > n>"
     "> \"else\" n>" 
     "\"{\" > n> r n" 
     "\"} // end of if (\" (s clause) \")else\" > n>" )
  "*Template for if-else statement."
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-if-else-statement
 	    (tempo-define-template 
 	     "jde-cflow-if-else-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton for if else statement"
	     'jde-cflow-tags))
 	  (set-default sym val)))

(defcustom jde-cflow-while-statement-template
  '("> \"while (\" (p \"while-clause: \" clause) \")\" >  n>" 
    "\"{\" > n> r n" 
    "\"} // end of while (\" (s clause) \")\" > n>" )
  "*Template for while statement."
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-while-statement
 	    (tempo-define-template 
 	     "jde-cflow-while-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton for while statement"
	     'jde-cflow-tags))
 	  (set-default sym val)))

(defcustom jde-cflow-for-statement-template
  '("> \"for (\" (p \"for-clause: \" clause) \")\" >  n>" 
    "\"{\" > n> r n "
    "\"} // end of for (\" (s clause) \")\" > n>")
  "*Template for the for statement. 
Also see `jde-cflow-fori-statement-template' and `jde-cflow-ford-statement-template'"
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-for-statement
 	    (tempo-define-template 
 	     "jde-cflow-for-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton for the for statement"
	     'jde-cflow-tags))
 	  (set-default sym val)))

(defcustom jde-cflow-fori-statement-template
  '("> \"for (int \" (p \"variable: \" var) \" = 0; \" (s var)" 
    "\" < \"(p \"upper bound: \" ub)\"; \" (s var) \"++)\" > n> "
    "\"{\" > n> r n" "\"} // end of for (int \" (s var) \" = 0; \"" 
    "(s var) \" < \" (s ub) \"; \" (s var) \"++)\" > n>")
  "*Template of a for loop statement.
Also see `jde-cflow-for-statement-template' and `jde-cflow-ford-statement-template'"
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-fori-statement
 	    (tempo-define-template 
 	     "jde-cflow-fori-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton of a incrementing for loop in java statement"
	     'jde-cflow-tags))
 	  (set-default sym val)))

(defcustom jde-cflow-ford-statement-template
  '("> \"for (int \" (p \"variable: \" var) \" = 0; \" (s var)" 
    "\" < \"(p \"upper bound: \" ub)\"; \" (s var) \"++)\" > n> "
    "\"{\" > n> r n" "\"} // end of for (int \" (s var) \" = 0; \"" 
    "(s var) \" < \" (s ub) \"; \" (s var) \"++)\" > n>")
  "*Template of a for decrementing loop statement."
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-ford-statement
 	    (tempo-define-template 
 	     "jde-cflow-ford-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton of a decrementing for loop in java statement"
	     'jde-cflow-tags))
 	  (set-default sym val)))

(defcustom jde-cflow-switch-statement-template
  '("> \"switch (\" (p \"switch-condition: \" clause) \")\" >" 
    "n>" 
    "\"{\" > n" 
    "\"case \" (p \"first value: \") \":\" > n> p n"
    "\"break;\" > n> p n"
    "\"default:\" > n> p n"
    "\"break;\" > n"
    "\"} // end of switch (\" (s clause) \")\" > n>")
  "*Template for the switch statement."
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-switch-statement
 	    (tempo-define-template 
 	     "jde-cflow-switch-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton for the switch method."
	     'jde-cflow-tags))
 	  (set-default sym val)))

(defcustom jde-cflow-case-statement-template
  '("n \"case \" (p \"value: \") \":\" > n> p n"
    "\"break;\" > n> p")
  "*Template for the case statement."
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-case-statement
 	    (tempo-define-template 
 	     "jde-cflow-case-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton for the case method."
	     'jde-cflow-tags))
 	  (set-default sym val)))

(defcustom jde-cflow-main-statement-template
  '("> \"public static void main (String[] args)\" >  n>"
    "\"{\" > n> r n" 
    "\"} // end of main ()\" > n>")
  "*Template for the main method."
  :group 'jde-cflow
  :type '(repeat string)
  :set '(lambda (sym val)
 	  (defalias 'jde-cflow-main-statement
 	    (tempo-define-template 
 	     "jde-cflow-main-statement-template"
 	     (jde-gen-read-template val)
 	     nil
 	     "Insert skeleton for the main method."
	     'jde-cflow-tags))
 	  (set-default sym val)))

;;This is the generic template which can be used to create and 
;;entirely new abbrev. The definition given might appear not
;;to be very useful, but I find the easiest way to make a new template
;;is to start of with "blah" alter it using custom and then 
;;cut and paste it from the .emacs file if you want to set a default. 

; (defcustom jde-cflow-generic-statement-template
;   '( "\"Blah\"" )
;   "*Template for generic statement."
;   :group 'jde-cflow
;   :type '(repeat string)
;   :set '(lambda (sym val)
; 	  (defalias 'jde-cflow-genetic-statement
; 	    (tempo-define-template 
; 	     "jde-cflow-case-generic-template"
; 	     (jde-gen-read-template val)
; 	     nil
; 	     "Insert skeleton for the java generic statement"
;            'jde-cflow-tags))
; 	  (set-default sym val)))
  

(defvar jde-cflow-abbrev-table (make-abbrev-table)) 

(defun jde-cflow-set-abbrevs(abbrev)
  "This function sets the abbrev which will 
expand to a control structure template" 
  ;;define the new abbrev in the custom cflow abbrev table
  (define-abbrev jde-cflow-abbrev-table
    ;;the name is just the car of the abbrev passed,
    (car abbrev) 
    ;;there is no expansion
    ""
    ;;and the hook does all the work, which is the cdr of the abbrev
    (cdr abbrev)))

(defun jde-cflow-delete-abbrevs(abbrev)
  "This function unsets all of the abbrevs which 
were previously set to expand as a control structure template"
  (define-abbrev jde-cflow-abbrev-table
    ;;the name is just the car of the abbrev passed
    (car abbrev)
    ;;this nil value should delete the old value
    nil))
  
;; To me this seems like a really hacky way of doing this, when I should be 
;; able to just get this information out of the abbreviation tables, but I cant
;; see how to do this without specifically knowing the internal format of the 
;; abbreviation tables, which is really not going to be very future proof. And 
;; probably wouldnt work on XEmacs. 

(defvar jde-cflow-old-abbrevs nil
  "These are the old abbreviations which expand into control structure templates.
A copy of these have to be kept, because when if the key of the abbrev
is altered the new one will not overwrite the last. So a list is kept
and used to delete all the other old abbrevs. Its set automatically.")

(defun jde-cflow-set-abbrevs-lambda(sym val)
  ;;This used to be lambda set function for jde-cflow-abbrevs
  ;;but it had to get moved because I cant instrument that for debugging.
  ;;If there are any old abbrevs delete them
  (if jde-cflow-old-abbrevs
      ;;but only if they have changed. If they have then delete the 
      ;;old ones which will cause the new ones to be loaded
      (if (not (equal jde-cflow-old-abbrevs val))
	  (progn(mapc 'jde-cflow-delete-abbrevs jde-cflow-old-abbrevs)
		(setq jde-cflow-old-abbrevs nil))))
  ;;now set the new ones, if the old abbrevs is nil. This means the abbrevs
  ;;on loading and then not again. 
  (if (not jde-cflow-old-abbrevs)
      (progn(mapc 'jde-cflow-set-abbrevs val)
	    ;;Now store the new ones to be the old ones next time around
	    (setq jde-cflow-old-abbrevs val)))
  ;;And set the default
  (set-default sym val))


(defcustom jde-cflow-abbreviations
  (list (cons "if"  'jde-cflow-if-statement)
	(cons "else"  'jde-cflow-else-statement)
	(cons "ife"  'jde-cflow-if-else-statement)
	(cons "while"  'jde-cflow-while-statement)
	(cons "for"   'jde-cflow-for-statement)
	(cons "fori"  'jde-cflow-fori-statement)
	(cons "ford"  'jde-cflow-ford-statement)
	(cons "switch" 'jde-cflow-switch-statement)
	(cons "case"  'jde-cflow-case-statement)
	;;(cons "gen" 'jde-cflow-case-generic statment)
	(cons "main"   'jde-cflow-main-statement))
  "Abbreviations used for the jde-cflow-mode.
These abbreviations expand into control flow structures.
To use these abbreviations, you must enable abbrev-mode (see
`jde-enable-abbrev-mode'). To use an abbreviation, enter the
abbreviation followed by a white-space character. To suppress
expansion, enter C-q white-space.
You may also customise the default templates as you wish, using
the `tempo' templates."
  :group 'jde-cflow
  :type '(repeat 
	  (cons :tag "Custom Cflow abbrev"
		(string :tag "Abbreviation")
		(function :tag "Expansion Function" )))
  :set '(lambda(sym val)
	  (jde-cflow-set-abbrevs-lambda sym val)))




(provide 'jde-cflow)


;;
;;
;;  $Log: jde-cflow.el,v $
;;  Revision 1.12  2000/06/19 10:18:54  lord
;;  Changes in the JDE means that jde-cflow now uses its own abbreviation
;;  table.
;;  Updated documentation to mention JDE support for jde-cflow.
;;
;;  Revision 1.11  2000/04/11 19:28:44  phil
;;  Fixed bug in for-d template
;;
;;  Revision 1.10  2000/01/25 14:12:42  lord
;;  Initial checkin into CVS...
;;
;;  Revision 1.9  1999-07-31 18:01:15+01  phillip2
;;  Updated set method to limit the number of times the user is prompted
;;  to save abbrev_defs (hopefully once per emacs session now!)
;;
;;  Revision 1.8  1999-07-21 01:17:33+01  phillip2
;;  Updated documentation
;;
;;  Revision 1.7  1999-07-21 01:07:15+01  phillip2
;;  Requires jde, and added customize function
;;
;;  Revision 1.6  1999-07-21 00:58:16+01  phillip2
;;  Updated documentation, added decrementing for templ
;;
;;  Revision 1.5  1999-07-21 00:41:22+01  phillip2
;;  Fixed a few of the templates.
;;
;;  Revision 1.4  1999-07-20 01:16:42+01  phillip2
;;  Restored original templates
;;
;;  Revision 1.3  1999-07-20 00:18:33+01  phillip2
;;  Changed all custom-cflow to cflow
;;  Added tempo tag
;;  Changed documentation
;;
;;  Revision 1.2  1999-07-19 13:52:47+01  phillip2
;;  Added version number, log file, and provide call
;;
;;
;;






