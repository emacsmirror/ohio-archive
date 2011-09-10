;;; screen-lines.el --- a minor mode for point motion based on screen lines

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 0.32 $
;; Keywords: local, convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This package contains part of the code and the documentation of the
;; following packages:
;;
;;   simple.el by Free Software Foundation, Inc.
;;   window-lines.el by enami tsugutomo (enami@ptgd.sony.co.jp)
;;
;; All of these are distributed under GPL.
;; Thanks to the authors for writing these excellent packages.


;;; Commentary:

;; This package provides "Screen Lines" minor mode.
;; In this minor mode, the following standard commands move point
;; in terms of screen lines, as opposed to text lines.
;;
;;     `beginning-of-line'
;;     `end-of-line'
;;     `next-line'
;;     `previous-line'
;;
;; Screen Lines minor mode should be handy when you edit a file with
;; long lines like this:
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;;
;; `M-x screen-lines-mode' toggles the mode for the current buffer.

;; Text lines vs Screen lines:
;;                                             See "(elisp)Text Lines"
;;                                                 "(elisp)Screen Lines"
;;
;;   Screen lines are defined by the way the text appears horizontally
;;   on the screen, whereas text lines are defined in terms of newline
;;   characters as line delimiters.
;;   
;;   The Emacs standard line oriented point moving commands such as
;;   `next-line' and `previous-line' are based on text lines.
;;
;;   A text line is a single screen line:
;;     * if it is short enough to fit the width of the selected window, or
;;     * if it is truncated on the screen as follows:
;;
;;          |                                       |
;;          |The quick brown fox jumped over the la$|
;;          |                                       |
;;
;;   A text line is divided into several screen lines:
;;     * if it is folded on the screen as follows:
;;
;;          |                                       |
;;          |The quick brown fox jumped over the la\|
;;          |zy dogs.<RET>                          |
;;          |                                       |
;;
;;       The above text line consists of the two screen lines
;;       "The quick brown fox jumped over the la" and
;;       "zy dogs.<RET>".
;;


;; Requirements:

;; Tested with FSF Emacs 20.7.2 and XEmacs 21.1.10 on
;; Debian GNU/Linux woody.


;; Install:

;; 1: Put this file in one of the directories listed in `load-path'.
;;    You can see the contents of `load-path' by entering
;;    `M-x customize-option <RET> load-path'.
;;
;; 2: Enter `M-x byte-compile-file <RET>
;;           <DIR-YOU-PUT-THIS-FILE-IN>/screen-lines.el <RET>'
;;    to byte-compile this file.
;;
;; 3: Put the following lines in your .emacs file.
;;
;;    (autoload 'screen-lines-mode "screen-lines"
;;              "Toggle Screen Lines minor mode for the current buffer."
;;              t nil)
;;
;; 4: Restart Emacs or enter `M-x load-library <RET> screen-lines'.


;; Activation:

;; * Enter `M-x screen-lines-mode' to turn on/off the screen-lines
;;   minor mode for the current buffer.
;;
;; * When the mode is ON in a buffer, "SLines" should appears in the
;;   mode line (which sits at the bottom of the buffer's window).


;; Customization:

;; * Enter `M-x customize-group <RET> screen-lines' to customize
;;   this package. You might need to enter
;;   `M-x load-library <RET> screen-lines' in advance.
;;
;; * The `screen-lines-mode' option determines whether Screen Lines mode
;;   is on or off by default for all buffers.
;;
;; * This package respects the following standard options:
;;     `next-line-add-newlines'
;;     `track-eol'
;;     `goal-column'
;;   You can customize these options by
;;   `M-x customize-group <RET> editing-basics'.


;; Implementation notes:

;; All commands are based on the behavior of the standard `vertical-motion'
;; function.
;;
;; The idea of using vertical-motion comes from window-lines.el by
;; enami tsugutomo (enami@ptgd.sony.co.jp).
;;
;; See tscreen-lines.txt (This is a private file, not distributed).
;; See "(elisp)Special Properties" for `Intangible' text property.
;; See "(elisp)Invisible Text" for `Invisible' text property.


;;; Change Log:

;; Version 0.32 (10 Jul 2000):
;;  * Changed to provide a minor mode.
;;  * Should work with pc-select.el, now.
;;    Thanks to Pedja Raspopovic <pedja@lsil.com> for reporting the problem.
;;    Thanks to gregory@mcst.ru (Gregory A. Shimansky) for reporting a
;;    workaround for it.

;; Version 0.21 (29 Jun 2000):
;;  * First public release of this package.

;;; Code:

;; Global variable declarations
(eval-when-compile
  (defvar goal-column)
  (defvar last-command)
  (defvar minor-mode-alist)
  (defvar next-line-add-newlines)
  (defvar temporary-goal-column)
  (defvar track-eol)
  (defvar zmacs-region-stays)           ; for XEmacs
  )


;;; Mode
(defgroup screen-lines nil
  "This package provides \"Screen Lines\" minor mode.
In this minor mode, the following standard commands move point
in terms of screen lines, as opposed to text lines.

    `beginning-of-line'
    `end-of-line'
    `next-line'
    `previous-line'

Screen Lines minor mode should be handy when you edit a file with
long lines like this:
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

`M-x screen-lines-mode' toggles the mode for the current buffer."
  :group 'editing-basics
  :group 'convenience)

(defcustom screen-lines-mode nil
  "*Toggle Screen Lines minor mode for the current buffer.
This variable is buffer-local."
  :type 'boolean
  :require 'screen-lines
  :group 'screen-lines)
(make-variable-buffer-local 'screen-lines-mode)


(defcustom screen-lines-minor-mode-string " SLines"
  "*String to display in mode line when Screen Lines mode is enabled."
  :type 'string
  :group 'screen-lines)

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'screen-lines-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((screen-lines-mode
                             screen-lines-minor-mode-string)))))

;;;###autoload
(defun screen-lines-mode (&optional arg)
  "Toggle Screen Lines minor mode for the current buffer.
With ARG, turn the mode on if ARG is positive, off otherwise."
  (interactive "P")
  (setq screen-lines-mode
        (if (null arg) (not screen-lines-mode)
          (> (prefix-numeric-value arg) 0)))
  (when (interactive-p)
    (message "Screen Lines minor mode: %s" (if screen-lines-mode "ON" "OFF"))))

;; Turn on/off automatically for specific buffers and major modes.
(defvar screen-lines-mode-auto-alist
  '((dired-mode-hook . nil)             ; dired-next-line needs this
    (minibuffer-setup-hook . nil))      ; not necessarily needed
  "Alist of hook variable and the value of Screen Lines mode for it.")
(mapcar #'(lambda (assoc)
            (add-hook (car assoc)
                      `(lambda () (setq screen-lines-mode ,(cdr assoc)))))
        screen-lines-mode-auto-alist)


;;; Commands
(defsubst screen-lines-keep-region-active ()
  "Only for XEmacs."
  (when (boundp 'zmacs-region-stays)
    (setq zmacs-region-stays t)))

(defun Screen-lines-beginning-of-line (&optional count)
  "Move point to the beginning of the current screen line.
With an argument COUNT not nil or 1, move forward COUNT-1 screen lines and
then to the beginning of the line."
  (interactive "p")
  (screen-lines-keep-region-active)
  (unless count (setq count 1))
  (vertical-motion (1- count)))

(defun Screen-lines-end-of-line (&optional count)
  "Move point to the end of the current screen line.
With an argument COUNT not nil or 1, move forward COUNT-1 screen lines and
then to the end of the line."
  (interactive "p")
  (screen-lines-keep-region-active)
  (unless count (setq count 1))
  (when (= count (vertical-motion count))
    (backward-char)))

(defun screen-lines-current-column ()
  "Return the horizontal position of point.  Beginning of screen line is 0."
  (let ((cur-col  (current-column))
        (sbol-col (save-excursion (vertical-motion 0)
                                  (current-column))))
    (if (>= (- cur-col sbol-col) 0)
        ;; in case some `invisible' text stretches over multiple lines
        (- cur-col sbol-col)
      cur-col)))

(defun screen-lines-move-to-column (column)
  "Move point to COLUMN in the current screen line."
  (let* ((beg (save-excursion (vertical-motion 0)
                              (current-column)))
         (lim (save-excursion (when (= 1 (vertical-motion 1)) (backward-char))
                              (current-column))))
    (move-to-column (cond
                     ;; dirty hack for invisible property
                     ((<= lim beg) column)
                     ((< (+ beg column) lim) (+ beg column))
                     (t lim)))))

(defun screen-lines-move (arg)
  "The guts of `screen-lines-next-line' and `screen-lines-previous-line'.
ARG says how many lines to move."
  (unless (member last-command
                  '(Screen-lines-next-line Screen-lines-previous-line
                    next-line previous-line))
    (setq temporary-goal-column
          (if (and track-eol
                   (eolp)
                   ;; Don't count beg of empty line as end of line
                   ;; unless we just did explicit end-of-line.
                   (or (not (bolp))
                       (member last-command '(Screen-lines-end-of-line
                                              end-of-line))))
              9999
            (screen-lines-current-column))))

  (if (= arg (vertical-motion arg))
      (screen-lines-move-to-column (or goal-column temporary-goal-column))
    (signal (if (< arg 0)
                'beginning-of-buffer
              'end-of-buffer)
            nil)))

(defun Screen-lines-next-line (arg)
  "Move cursor vertically down ARG screen lines."
  (interactive "p")
  (screen-lines-keep-region-active)
  (if (and next-line-add-newlines (= arg 1))
      (let ((opoint (point)))
        (Screen-lines-end-of-line)
        (if (eobp)
            (newline 1)
          (goto-char opoint)
          (screen-lines-move arg)))
    (if (interactive-p)
        (condition-case nil
            (screen-lines-move arg)
          ((beginning-of-buffer end-of-buffer) (ding)))
      (screen-lines-move arg)))
  nil)

(defun Screen-lines-previous-line (arg)
  "Move cursor vertically up ARG screen lines."
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
          (screen-lines-move (- arg))
        ((beginning-of-buffer end-of-buffer) (ding)))
    (screen-lines-move (- arg)))
  nil)


;;; Advice
(defadvice next-line
  (around screen-lines last activate compile preactivate)
  "Move cursor vertically down ARG *screen lines* if Screen Lines mode is on."
  (if screen-lines-mode
      (condition-case err
          (Screen-lines-next-line (ad-get-arg 0))
        ((beginning-of-buffer end-of-buffer)
         (if (interactive-p)
             (ding)
           (signal (car err) (cdr err)))))
    ad-do-it))

(defadvice previous-line
  (around screen-lines last activate compile preactivate)
  "Move cursor vertically up ARG *screen lines* if Screen Lines mode is on."
  (if screen-lines-mode
      (condition-case err
          (Screen-lines-previous-line (ad-get-arg 0))
        ((beginning-of-buffer end-of-buffer)
         (if (interactive-p)
             (ding)
           (signal (car err) (cdr err)))))
    ad-do-it))

(defadvice beginning-of-line
  (around screen-lines last activate compile preactivate)
  "Move point to the beginning of the current *screen line*,
if Screen Lines mode is on."
  (if screen-lines-mode
      (Screen-lines-beginning-of-line (ad-get-arg 0))
    ad-do-it))

(defadvice end-of-line
  (around screen-lines last activate compile preactivate)
  "Move point to the end of the current *screen line*,
if Screen Lines mode is on."
  (if screen-lines-mode
      (Screen-lines-end-of-line (ad-get-arg 0))
    ad-do-it))

(provide 'screen-lines)
;;; screen-lines.el ends here
