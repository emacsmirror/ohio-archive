;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:     dm-xmr.el
;;; Author:   Ik Su Yoo <ik@ctt.bellcore.com>
;;; Date:     03/17/92
;;; Contents: Dynamic Macros for XMR mode.
;;;
;;; Copyright (c) 1992 Ik Su Yoo.
;;;
;;; May be redistributed only under the terms of the GNU Emacs General
;;; Public License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dmacro)

;;;
;;; Helping functions.
;;;

(defvar dmacro-last-input nil)

(defun right-trim (string charbag)
  (let ((endp (length string)))
    (while (and (> endp 0) (memq (elt string (- endp 1)) charbag))
      (setq endp (- endp 1)))
    (if (zerop endp)
	""
      (substring string 0 endp))))

(defun read-string-and-save (prompt-string)
  (setq dmacro-last-input (read-string prompt-string)))

(defun insert-pushbuttons (prompt-string)
  (insert-homogeneous-children "XmPushButton"))

(defun insert-togglebuttons (prompt-string)
  (insert-homogeneous-children "XmToggleButton"))

(defun insert-cascadebuttons (prompt-string)
  (insert-homogeneous-children "XmCascadeButton"))

(defun insert-homogeneous-children (class-name)
  (if (null dmacro-last-input)
      ""
    (apply 'concat
	   (mapcar '(lambda (name)
		      (format "*%s.wcClassName:\t%s\n\n"
			      (right-trim (symbol-name name) '(?,))
			      class-name))
		   (car (read-from-string (format "(%s)" dmacro-last-input)))))
    ))

;;;
;;; XMR mode dmacros.
;;;

(add-dmacros 'xmr-mode-abbrev-table
  (append
   (mapcar '(lambda (class-name)
	      (list
	       ;;
	       ;; Dmacro doesn't like upper case letters in the macro name.
	       ;;
	       (substring (downcase class-name) 2)
	       (format "~(mark)*~(prompt name).wcClassName:%s~@\n"
		       class-name)
	       nil
	       nil))
	   '(
	     "XmArrowButton"
	     "XmCascadeButton"
	     "XmDrawnButton"
	     "XmLabel"
	     "XmList"
	     "XmPushButton"
	     "XmScale"
	     "XmScrollBar"
	     "XmSelectionBox"
	     "XmSeparator"
	     "XmText"
	     "XmTextField"
	     "XmToggleButton"
	     ))
   (mapcar '(lambda (class-name)
	      (list
	       ;;
	       ;; Dmacro doesn't like upper case letters in the macro name.
	       ;;
	       (substring (downcase class-name) 2)
	       (format "~(mark)*~(prompt name).wcClassName:%s\n*~(prompt name).wcChildren:~@\n"
		       class-name)
	       nil
	       nil))
	   '(
	     "XmBulletinBoard"
	     "XmCommand"
	     "XmDrawingArea"
	     "XmFileSelectionBox"
	     "XmForm"
	     "XmFrame"
	     "XmMainWindow"
	     "XmManager"
	     "XmMessageBox"
	     "XmPanedWindow"
	     "XmRowColumn"
	     "XmScrolledWindow"
	     ))
   '(
     ("menubar"
      "~(mark)\
*~(prompt name).wcConstructor:	XmCreateMenuBar
*~(prompt name).wcChildren:	~(prompt children)
~@"
      nil
      nil)

     ("pulldownmenu"
      "~(mark)\
*~(prompt name).wcConstructor:	XmCreatePulldownMenu
*~(prompt name).wcChildren:	~(prompt children nil read-string-and-save)
*~(prompt name).wcManaged:	false

~(prompt children-spec nil insert-pushbuttons)~@"
      nil
      nil)

     ("optionmenu"
      "~(mark)\
*~(prompt name)Menu.wcConstructor:	XmCreatePullDownMenu
*~(prompt name)Menu.wcChildren:		~(prompt children nil read-string-and-save)
*~(prompt name)Menu.wcManaged:		false

*~(prompt name).wcClassName:		XmRowColumn
*~(prompt name).rowColumnType:		MENU_OPTION
*~(prompt name).subMenuId:		*~(prompt name)Menu

~(prompt children-spec nil insert-pushbuttons)~@"
      nil
      nil)

     ("radiobox"
      "~(mark)\
*~(prompt name).wcClassName:	XmRowColumn
*~(prompt name).wcChildren:	~(prompt children nil read-string-and-save)
*~(prompt name).radioBehavior:	true

~(prompt children-spec nil insert-togglebuttons)~@"
      nil
      nil)

     ("genericdialog"
      "~(mark)\
*~(prompt name).wcConstructor: XmCreatePromptDialog
*~(prompt name).wcCallback: \\
    WcUnmanageCB(*~(prompt name)*Selection, \\
		 *~(prompt name)*sb_text, \\
		 *~(prompt name)*Help)
*~(prompt name).wcChildren:~@"
      nil
      nil)

     ("filemenu"
      "~(mark)\
*fileMenu.wcConstructor:		XmCreatePulldownMenu
*fileMenu.wcChildren:			new open save saveAs menuSep quit
*fileMenu.wcManaged:			false

*new.wcClassName:			XmPushButton
*new.accelerator:			Ctrl <Key> N
*new.acceleratorText:			Ctrl-N
*new.labelString:			New
*new.mnemonic:				N

*open.wcClassName:			XmPushButton
*open.accelerator:			Ctrl <Key> O
*open.acceleratorText:			Ctrl-O
*open.labelString:			Open
*open.mnemonic:				O

*save.wcClassName:			XmPushButton
*save.accelerator:			Ctrl <Key> S
*save.acceleratorText:			Ctrl-S
*save.labelString:			Save
*save.mnemonic:				S

*saveAs.wcClassName:			XmPushButton
*saveAs.labelString:			Save as...
*saveAs.mnemonic:			a

*quit.wcClassName:			XmPushButton
*quit.accelerator:			Ctrl <Key> Q
*quit.acceleratorText:			Ctrl-Q
*quit.labelString:			Quit
*quit.mnemonic:				Q

*file.wcClassName:			XmCascadeButtonGadget
*file.labelString:			File
*file.mnemonic:				F
*file.subMenuId:			*fileMenu
~@
"
      nil
      nil)

     ("editmenu"
      "~(mark)\
*editMenu.wcConstructor:		XmCreatePulldownMenu
*editMenu.wcChildren:			cut copy paste menuSep undo
*editMenu.wcManaged:			false

*cut.wcClassName:			XmPushButton
*cut.accelerator:			<Key> Delete
*cut.acceleratorText:			Delete
*cut.labelString:			Cut
*cut.mnemonic:				C

*copy.wcClassName:			XmPushButton
*copy.accelerator:			Ctrl <Key> Delete
*copy.acceleratorText:			Ctrl-Delete
*copy.labelString:			Copy
*copy.mnemonic:				O

*paste.wcClassName:			XmPushButton
*paste.accelerator:			<Key> Insert
*paste.acceleratorText:			Insert
*paste.labelString:			Paste
*paste.mnemonic:			P

*edit.wcClassName:			XmCascadeButtonGadget
*edit.labelString:			Edit
*edit.mnemonic:				E
*edit.subMenuId:			*editMenu
~@"
      nil
      nil))
   ))

(define-key xmr-mode-map "\C-cd" 'insert-dmacro)

(provide 'xmr-dmacro)
