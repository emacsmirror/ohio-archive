;;; cda.el --- control CD player using interface to the cda daemon

;;; Copyright (C) 2000 Matthew P. Hodges

;; Author: Matthew P. Hodges <pczmph@unix.ccc.nottingham.ac.uk>
;; Version: $Id: cda.el,v 1.18 2000/06/28 11:03:33 matt Exp $

;; cda.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; cda.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;; 
;; Package to control the cda daemon (which is part of the xmcd
;; package).
;;
;; Just run the cda-start command, which will pop up a *cda* buffer
;; with the status of the CD (eg Playing) and track/artist
;; information. Various functions are bound to keys in the *cda*
;; buffer, eg ">" is bound to cda-next-track -- use ? or C-h b to find
;; all the bindings.

;;; Code:

;; Variables associated with CD status

(defvar cda-current-status nil
  "Status of current CD.
Can be busy, stopped, or playing.")

(defvar cda-timer nil)

(defvar cda-timer-period 30
  "The number of seconds between updates of information.")

;; Variables associated with CD data

(defvar cda-current-artist nil
  "Artist of current CD.")

(defvar cda-current-title nil
  "Name of current CD.")

(defvar cda-current-track nil
  "Name of track being played (if any).")

(defvar cda-track-alist nil
  "List of tracks on current CD.")

(defvar cda-shuffle-status nil
  "Set to t if CD is not being shuffled.")

(defvar cda-repeat-status nil
  "Set to t if CD is not being repeated.")

;; Interactive commands

(defun cda-start ()
  "Start the cda daemon and setup the cda buffer."
  (interactive)
  (cda-start-daemon)
  (cda-refresh-display-buffer)
  (message
   "Welcome to the Emacs cda interface. Press ? or C-h b for keybindings."))

(defun cda-play-or-pause ()
  "Play the CD (or pause if it's playing)."
  (interactive)
  (if (string-equal cda-current-status "CD_Playing")
      (progn
        (cda-command "pause")
        (cda-stop-timer))
    (cda-command "play")))

(defun cda-stop ()
  "Stop the CD."
  (interactive)
  (cda-command "stop"))

(defun cda-eject ()
  "Eject the CD."
  (interactive)
  (cda-clear-cd-info)
  (cda-command "disc eject"))

(defun cda-load ()
  "Load the CD."
  (interactive)
  (cda-command "disc load"))

(defun cda-next-track ()
  "Choose the next track."
  (interactive)
  (if (string-equal cda-current-status "CD_Stopped")
      (error "CD is stopped"))
  (cda-command "track next"))

(defun cda-prev-track ()
  "Choose the next track."
  (interactive)
  (if (string-equal cda-current-status "CD_Stopped")
      (error "CD is stopped"))
  (cda-command "track prev"))

(defun cda-refresh-display-buffer ()
  "Refresh the displayed CD information."
  (interactive)
  ;; Set up the window
  (if (not (get-buffer "*cda*"))
      (progn
        (get-buffer-create "*cda*")
        (set-buffer "*cda*")
        (setq buffer-read-only t)
        (display-buffer "*cda*")
        (cda-mode)))
  ;; Get information and display it
  (cda-get-current-status)
  (if (not cda-current-artist)
      (cda-parse-cd-info))
  (with-current-buffer "*cda*"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "[Status = %s%s%s]\n"
                      cda-current-status
                      (if cda-shuffle-status
                          ", shuffle"
                        "")
                      (if cda-repeat-status
                          ", repeat"
                        "")))
      (if cda-current-artist
          (insert (format "%s / %s\n" cda-current-artist cda-current-title)))
      (if (or (string-equal cda-current-status "CD_Playing")
               (string-equal cda-current-status "CD_Paused"))
          (progn
            (insert (format "Track %s (of %d) -- %s"
                            cda-current-track
                            (length cda-track-alist)
                            (car (nth (1- cda-current-track)
                                      cda-track-alist))))
            (if (not cda-timer)
                (cda-start-timer)))
        (cda-stop-timer)))
    (set-buffer-modified-p nil)))

(defun cda-choose-track ()
  "Choose a track from `cda-track-alist'."
  (interactive)
  (let* ((completion-ignore-case t)
         (track (completing-read "Choose track: " cda-track-alist))
         (number (cdr (assoc track cda-track-alist))))
    (if number
        (cda-command (format "play %d" number))
      (error "%s is not a valid track selection" track))))

(defun cda-toggle-shuffle ()
  "Turn CD shuffling on/off."
  (interactive)
  (if cda-shuffle-status
      (cda-command "shuffle off")
    (cda-command "shuffle on")))

(defun cda-toggle-repeat ()
  "Turn CD repeating on/off."
  (interactive)
  (if cda-repeat-status
      (cda-command "repeat off")
    (cda-command "repeat on")))

(defun cda-show-help ()
  "Show key bindings."
  (interactive)
  (let ((prompt "Choose key to display command:")
        (map (cdr cda-mode-map)))
    ;; Build up prompt string
    (while map
      (setq prompt (format "%s %c" prompt
                            (caar map)))
      (setq map (cdr map)))
    ;; Now select a key
    (setq map (cdr cda-mode-map))
    (let* ((key (read-char prompt))
           (function (cdr (assoc key map))))
      (if function
          (message "'%c' runs %s -- %s" key
                   (symbol-name function)
                   (or (documentation function)
                       "no documentation available."))
        (message "'%c' is not bound to any function." key)))))

;; Internal functions

(defun cda-start-timer ()
  (if cda-timer (cancel-timer cda-timer))
  (setq cda-timer
        (run-with-timer cda-timer-period cda-timer-period
                        'cda-refresh-display-buffer)))

(defun cda-stop-timer ()
  (if cda-timer
      (progn
        (cancel-timer cda-timer)
        (setq cda-timer nil))))

(defun cda-start-daemon ()
  (call-process "cda" nil 0 nil "on"))

(defun cda-quit ()
  "Quit and clean up cda-related stuff."
  (interactive)
  (if (y-or-n-p "Do you want to kill the cda process? ")
      (call-process "cda" nil 0 nil "off"))
  (cda-clear-cd-info)
  (cda-stop-timer)
  (kill-buffer "*cda*"))

(defun cda-clear-cd-info ()
  (setq cda-current-artist nil
        cda-current-title nil
        cda-current-track nil
        cda-track-alist nil))

(defun cda-get-current-status ()
  "Get current track being played."
  (with-temp-buffer
    (call-process "cda" nil t nil "status")
    (goto-char (point-min))
    (if (looking-at "\\(CD_\\([^ ]+\\)\\|No_Disc\\).*\\([+-]\\)shuf.*\\([+-]\\)rept")
        (progn
          (setq cda-current-status (match-string 1))
          (if (string-equal (match-string 3) "+")
              (setq cda-shuffle-status t)
            (setq cda-shuffle-status nil))
          (if (string-equal (match-string 4) "+")
              (setq cda-repeat-status t)
            (setq cda-repeat-status nil)))
      (error "Cannot determine status of CD: %s"
             (buffer-substring (line-beginning-position)
                               (line-end-position))))
    (cond ((string-equal cda-current-status "CD_Playing")
           (if (looking-at "CD_Playing disc:[0-9]+\\s-+\\([0-9]+\\)")
               (setq cda-current-track (string-to-int (match-string 1)))
             (setq cda-current-track nil)
             (error "Cannot determine current track: %s"
                    (buffer-substring (line-beginning-position)
                                      (line-end-position)))))
          ((string-equal cda-current-status "CD_Busy")
           (error "CD player is busy")))))

(defun cda-parse-cd-info ()
  "Get CD info and parse it."
  (with-temp-buffer
    (call-process "cda" nil t nil "toc")
    (goto-char (point-min))
    ;; Get artist and title of CD
    (forward-line 1)
    (if (looking-at "\\(.*\\) / \\(.*\\)")
        (progn
          (setq cda-current-artist (match-string 1))
          (setq cda-current-title (match-string 2))))
    ;; Get track list
    (setq cda-track-alist nil)
    (forward-line 2)
    (while (looking-at ".\\([0-9]+\\) [0-9]+:[0-9]+\\s-+\\(.*\\)")
      (setq cda-track-alist
            (cons (cons (match-string 2)
                        (string-to-int (match-string 1))) cda-track-alist))
      (forward-line 1))
    (setq cda-track-alist (reverse cda-track-alist))))

(defun cda-command (command)
  "Pass COMMAND to the cda daemon."
  (call-process "cda" nil nil nil command)
  (cda-refresh-display-buffer))

;; define cda-mode for the *cda* buffer

(defvar cda-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ">") 'cda-next-track)
    (define-key map (kbd "<") 'cda-prev-track)
    (define-key map (kbd "e") 'cda-eject)
    (define-key map (kbd "l") 'cda-load)
    (define-key map (kbd "p") 'cda-play-or-pause)
    (define-key map (kbd "s") 'cda-stop)
    (define-key map (kbd "r") 'cda-refresh-display-buffer)
    (define-key map (kbd "q") 'cda-quit)
    (define-key map (kbd "c") 'cda-choose-track)
    (define-key map (kbd "R") 'cda-toggle-repeat)
    (define-key map (kbd "S") 'cda-toggle-shuffle)
    (define-key map (kbd "?") 'cda-show-help)
    map))
    
(defun cda-mode ()
  "Major mode for controlling the cda daemon from the *cda* buffer."
  (kill-all-local-variables)
  (use-local-map cda-mode-map)
  (setq major-mode 'cda-mode)
  (setq mode-name "cda")
  (run-hooks 'cda-mode-hook))

(provide 'cda)

;;; cda.el ends here
