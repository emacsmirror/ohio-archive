;From pierson@xenna.encore.com Mon Jul 23 15:28:30 1990
;Date: Mon, 23 Jul 90 11:47:40 EDT
;From: Dan Pierson <pierson@xenna.encore.com>
;To: drw@BOURBAKI.MIT.EDU, epoch@cs.uiuc.edu
;Subject: Epoch patches for emerge
;
;My previous patches to make emerge use Epoch buttons for highlighting
;were badly broken.  This set seems to work.

;;;; epoch-emerge-patch.el
;;; Written by Dan L. Pierson <pierson@encore.com>, 7/23/90

;;; We need these for both compilation and execution.

(require 'emerge)

(defvar-local emerge-button nil
  "Epoch button used to highlight differences.")

(defvar emerge-attribute nil
  "*Highlight Epoch attribute (\"color\" index) for emerge.")

;;; Epoch changes to emerge setup (pity emerge doesn't have hooks).

(defun emerge-epoch-setup ()
  (eval-in-buffer emerge-A-buffer
		  (if (null emerge-button)
		      (setq emerge-button (make-button))))
  (eval-in-buffer emerge-B-buffer
		  (if (null emerge-button)
		      (setq emerge-button (make-button))))
  (eval-in-buffer emerge-merge-buffer
		  (if (null emerge-button)
		      (setq emerge-button (make-button))))
  (if (null emerge-attribute)
      (progn
	(setq emerge-attribute (reserve-attribute))
	(set-attribute-global emerge-attribute
			      (get-color "white")
			      (get-color "black"))))
  )

(fset 'old-emerge-setup (symbol-function 'emerge-setup))
(defun emerge-setup (buffer-A file-A buffer-B file-B)
  (old-emerge-setup buffer-A file-A buffer-B file-B)
  (emerge-epoch-setup))
(fset 'old-emerge-setup-with-ancestor (symbol-function 'emerge-setup-with-ancestor))
(defun emerge-setup-with-ancestor (buffer-A file-A buffer-B file-B)
  (old-emerge-setup-with-ancestor buffer-A file-A buffer-B file-B)
  (emerge-epoch-setup))

;;; These functions redefined for Epoch highlighting support

(defun emerge-select-difference (n)
  (let ((diff-vector (aref emerge-difference-list n)))
    (emerge-place-flags-in-buffer emerge-A-buffer
				  (aref diff-vector 0) (aref diff-vector 1))
    (emerge-place-flags-in-buffer emerge-B-buffer
				  (aref diff-vector 2) (aref diff-vector 3))
    (emerge-place-flags-in-buffer emerge-merge-buffer
				  (aref diff-vector 4) (aref diff-vector 5))
    (epoch::redisplay-screen)
    ))

(defun emerge-place-flags-in-buffer1 (buffer before after)
  (move-button emerge-button before after)
  (set-button-attribute emerge-button emerge-attribute)
  ;; Set both the buffer's point and its window's point to the
  ;; beginning of the difference.  Note that this has to be
  ;; done outside of save-excursion, and without changing the
  ;; current buffer.
  (goto-char before)
  (let ((window (get-buffer-window buffer)))
    (if window
	(set-window-point window before))))

(defun emerge-unselect-difference (n)
  (let ((diff-vector (aref emerge-difference-list n)))
    (emerge-remove-flags-in-buffer emerge-A-buffer
				   (aref diff-vector 0) (aref diff-vector 1))
    (emerge-remove-flags-in-buffer emerge-B-buffer
				   (aref diff-vector 2) (aref diff-vector 3))
    (emerge-remove-flags-in-buffer emerge-merge-buffer
				   (aref diff-vector 4) (aref diff-vector 5))
    (epoch::redisplay-screen)
    ))

(defun emerge-remove-flags-in-buffer (buffer before after)
  (eval-in-buffer buffer
		  (if emerge-button
		      (delete-button emerge-button))))

;;; The original code is full of (1+ ...) and (1- ...) to correct for
;;; the marker lines.  Epoch buttons take up no space.

;;; BUG: the regions always appear changed after one alternative has
;;; been selected.

(defun emerge-select-A (&optional force)
  "Select the A variant of this difference.  Refuses to function if this
difference has been edited, i.e., if it is neither the A nor the B variant.
An ARGUMENT forces the variant to be selected even if the differences has
been edited."
  (interactive "P")
  (let ((buffer-read-only nil))
    (let* ((diff-vector
	    (aref emerge-difference-list emerge-current-difference))
	   (A-begin (aref diff-vector 0))
	   (A-end (aref diff-vector 1))
	   (B-begin (aref diff-vector 2))
	   (B-end (aref diff-vector 3))
	   (merge-begin (aref diff-vector 4))
	   (merge-end (aref diff-vector 5)))
      (if force
	  (emerge-select-A-edit merge-begin merge-end A-begin A-end)
	(if (emerge-compare-buffers emerge-B-buffer B-begin B-end
				    emerge-merge-buffer merge-begin merge-end)
	    (progn
	      (emerge-select-A-edit merge-begin merge-end A-begin A-end)
	    
	      (if emerge-auto-advance
		  (emerge-next-difference)))
	  (if (emerge-compare-buffers emerge-A-buffer A-begin A-end
				      emerge-merge-buffer merge-begin
				      merge-end)
	      (if emerge-auto-advance
		  (emerge-next-difference))
	    (ding)
	    (message "This difference region has been edited.")))))))

;; Actually select the A variant
(defun emerge-select-A-edit (merge-begin merge-end A-begin A-end)
  (eval-in-buffer
   emerge-merge-buffer
   (let ((start (marker-position merge-begin))
	 (end (marker-position merge-end)))
     (delete-region start end)
     (goto-char start)
     (insert-buffer-substring emerge-A-buffer A-begin A-end)
     (move-marker merge-begin start)
     (move-marker merge-end (+ start (- A-end A-begin))))
   (aset diff-vector 6 'a)
   (emerge-refresh-mode-line)))

(defun emerge-select-B (&optional force)
  "Select the B variant of this difference.  Refuses to function if this
difference has been edited, i.e., if it is neither the A nor the B variant.
An ARGUMENT forces the variant to be selected even if the differences has
been edited."
  (interactive "P")
  (let ((buffer-read-only nil))
    (let* ((diff-vector
	    (aref emerge-difference-list emerge-current-difference))
	   (A-begin (aref diff-vector 0))
	   (A-end (aref diff-vector 1))
	   (B-begin (aref diff-vector 2))
	   (B-end (aref diff-vector 3))
	   (merge-begin (aref diff-vector 4))
	   (merge-end (aref diff-vector 5)))
      (if force
	  (emerge-select-B-edit merge-begin merge-end B-begin B-end)
	(if (emerge-compare-buffers emerge-A-buffer A-begin A-end
				    emerge-merge-buffer merge-begin merge-end)
	    (progn
	      (emerge-select-B-edit merge-begin merge-end B-begin B-end)
	      (if emerge-auto-advance
		  (emerge-next-difference)))
	  (if (emerge-compare-buffers emerge-B-buffer B-begin B-end
				      emerge-merge-buffer merge-begin
				      merge-end)
	      (if emerge-auto-advance
		  (emerge-next-difference))
	    (ding)
	    (message "This difference region has been edited.")))))))

;; Actually select the B variant
(defun emerge-select-B-edit (merge-begin merge-end B-begin B-end)
  (eval-in-buffer
   emerge-merge-buffer
   (let ((start (marker-position merge-begin))
	 (end (marker-position merge-end)))
     (delete-region start end)
     (goto-char start)
     (insert-buffer-substring emerge-B-buffer B-begin B-end)
     (move-marker merge-begin start)
     (move-marker merge-end (+ start (- B-end B-begin))))
   (aset diff-vector 6 'b)
   (emerge-refresh-mode-line)))
