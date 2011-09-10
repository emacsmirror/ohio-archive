;From: mad@math.keio.JUNET (MAEDA Atusi)
;Newsgroups: comp.emacs
;Subject: boss.el  --  Emergency escape package.
;Message-ID: <MAD.89Sep16225543@cabbage.math.keio.JUNET>
;Date: 16 Sep 89 13:55:43 GMT
;Reply-To: mad@nakanishi.math.keio.ac.jp
;Organization: Faculty of Sci. and Tech., Keio Univ., Yokohama, Japan.
;Lines: 62
;
;This is boss.el, a half-serious program written by the author of GNUS.
;
;This will be most useful when used with getris.el.  Load this program
;before you run getris.  Hit ESC when your boss come around during the
;game.  C-c C-c continues the game.

;;;  Keio University
;;;    Faculty of Science and Technology
;;;      Department of Math
;;;		MAEDA Atusi (In Japan we write our family names first.)
;;;		mad@nakanishi.math.keio.ac.jp

;---- cut here -------- cut here -------- cut here -------- cut here ----
;;; Boss has come! by Masanobu UMEDA <umerin@flab.Fujitsu.JUNET>
;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'boss)

(defvar boss-windows nil
  "Previous window configuration.")

(defun boss-has-come ()
  "Emergency escape!"
  (interactive)
  (setq boss-windows (current-window-configuration))
  (delete-other-windows)
  (switch-to-buffer (generate-new-buffer "files.h"))
  (insert-file-contents "/usr/include/stdio.h")
  ;; Hack the buffer.
  (let ((point (% (random t) (buffer-size))))
    (if (< point 0)
	(setq point (- 0 point)))
    (delete-region point (point-max))
    (goto-char (point-max)))
  (local-set-key "\C-c\C-c" 'boss-goes-away))

(defun boss-goes-away ()
  "Have fun!"
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration boss-windows))

(if (featurep 'gnus)
    (progn
      (define-key gnus-Group-mode-map "\C-c\C-c" 'boss-has-come)
      (define-key gnus-Subject-mode-map "\C-c\C-c" 'boss-has-come)
      (define-key gnus-Article-mode-map "\C-c\C-c" 'boss-has-come)))
