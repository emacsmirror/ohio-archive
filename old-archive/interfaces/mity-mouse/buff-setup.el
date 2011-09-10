; Setup code to allow special mouse bindings in Buffer-menu mode.  Mostly
; just provides a batch of functions to bind to mouse keys which first move
; point to beginning of mouse line, then invoke the normal buffer menu
; function.
;
; No default bindings are given, you will have to invent those yourself,
; then hang a function off buffer-menu-mode-hook to setup the binding.
;
; tahorsley@ssd.csd.harris.com (Tom Horsley)
; Dec 26, 1989
;

(provide 'buff-setup)

(defun x-buffer-menu (arg)
"Enter buffer menu mode from a mouse function key."
   (buffer-menu)
)

(defun x-Buffer-menu-select (arg)
  "Select mouse line's buffer; also display buffers marked with \">\".
You can mark buffers with the \\[Buffer-menu-mark] command."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-select)
)

(defun x-Buffer-menu-2-window (arg)
  "Select mouse line's buffer, with previous buffer in second window."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-2-window)
)

(defun x-Buffer-menu-1-window (arg)
  "Select mouse line's buffer, alone, in full screen."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-1-window)
)

(defun x-Buffer-menu-this-window (arg)
  "Select mouse line's buffer in this window."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-this-window)
)

(defun x-Buffer-menu-other-window (arg)
  "Select mouse line's buffer in other window, leaving buffer menu visible."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-other-window)
)

(defun x-Buffer-menu-save (arg)
  "Mark buffer on mouse line to be saved by \\[Buffer-menu-execute] command."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-save)
)

(defun x-Buffer-menu-delete (arg)
  "Mark buffer on mouse line to be deleted by \\[Buffer-menu-execute] command."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-delete)
)

(defun x-Buffer-menu-not-modified (arg)
  "Mark buffer on mouse line as unmodified (no changes to save)."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-not-modified)
)

(defun x-Buffer-menu-unmark (arg)
  "Cancel all requested operations on buffer on mouse line."
   (x-mouse-set-point arg)
   (beginning-of-lineBuffer-menu-execute 1)
   (Buffer-menu-unmark)
)

(defun x-Buffer-menu-mark (arg)
  "Mark buffer on mouse line for being displayed by \\[Buffer-menu-select] command."
   (x-mouse-set-point arg)
   (beginning-of-line 1)
   (Buffer-menu-mark)
)

(defun x-Buffer-menu-execute (arg)
  "Save and/or delete buffers marked with \\[Buffer-menu-save] or \\[Buffer-menu-delete] commands."
   (Buffer-menu-execute)
)
