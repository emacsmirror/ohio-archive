;;;
;;; ISPELL-MODE.EL -- electric minor mode interface to ispell package.
;;; in auto-ispell-mode `space' or `return' automatically trigger `ispell-word'
;;; on previously typed word.

;;; Avishai Yacoby

;; LCD Archive Entry:
;; ispell-mode|Avishai Yacobi|avishaiy@mcil.comm.mot.com|
;; Electric minor mode interface to ispell package.|
;; 1993-02-16||~/modes/ispell-mode.el.Z|

;;
;; This file is not part of the GNU Emacs distribution (yet).
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defvar auto-ispell-flag nil)

(defvar aim-origin-keymap nil)

(defvar aim-space-hook nil)

(defvar aim-tab-hook nil)

(defvar aim-ret-hook nil)

(defun auto-ispell-mode ()
  "Toggle auto-ispell-mode.

In auto-ispell-mode, inserting a space or a new line applies ispell-word
on the previous word.

Pressing Esc-Tab activates ispell-complete-word.
"
  (interactive)
  (if (not auto-ispell-flag)
      (progn
        (make-local-variable 'auto-ispell-flag)
        (make-local-variable 'aim-ret-hook)
        (make-local-variable 'aim-tab-hook)
        (make-local-variable 'aim-space-hook)
        (make-local-variable 'aim-origin-keymap)
        (setq aim-origin-keymap (current-local-map))
        (setq aim-space-hook (if aim-origin-keymap (lookup-key aim-origin-keymap " ") nil))
        (setq aim-tab-hook (if aim-origin-keymap (lookup-key aim-origin-keymap "	") nil))
        (setq aim-ret-hook (if aim-origin-keymap (lookup-key aim-origin-keymap "\C-m") nil))
        (use-local-map (if aim-origin-keymap
                           (copy-keymap aim-origin-keymap)
                         (make-sparse-keymap)))
        (define-key (current-local-map) " " 'auto-ispell-space)
        (define-key (current-local-map) "\C-m" 'auto-ispell-ret)
        (define-key (current-local-map) "\t" 'ispell-complete-word)
        (setq-default auto-ispell-flag nil)
        (or (assq 'auto-ispell-flag minor-mode-alist)
            (setq minor-mode-alist
                  (cons '(auto-ispell-flag " Spell") minor-mode-alist)))
        (setq auto-ispell-flag t))
    (use-local-map aim-origin-keymap)
    (kill-local-variable 'auto-ispell-flag)
    (kill-local-variable 'aim-ret-hook)
    (kill-local-variable 'aim-tab-hook)
    (kill-local-variable 'aim-space-hook)
    (kill-local-variable 'aim-origin-keymap)
    (setq auto-ispell-flag nil))
  (set-buffer-modified-p (buffer-modified-p)))

(defun auto-ispell-space ()
  (interactive)
  (if (and (commandp aim-space-hook)
       (not (eq aim-space-hook 'self-insert-command))
       (not (eq aim-space-hook 'auto-ispell-space)))
      (run-hooks 'aim-space-hook)
    (insert " "))
  (if (> (current-column) fill-column)
      (run-hooks 'auto-fill-hook))
  (save-excursion
    (backward-word 1)
    (ispell-word)))

(defun auto-ispell-ret ()
  (interactive)
  (if (and (commandp aim-ret-hook)
       (not (eq aim-ret-hook 'self-insert-command))
       (not (eq aim-ret-hook 'auto-ispell-ret)))
      (run-hooks 'aim-ret-hook)
    (if (> (current-column) fill-column)
    (run-hooks 'auto-fill-hook))
    (insert "\n"))
  (save-excursion
    (backward-word 1)
    (ispell-word)))
