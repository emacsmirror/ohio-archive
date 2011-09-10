;To: unix-emacs@bbn.com
;Date: 26 Apr 89 15:59:00 GMT
;From: Paul Davis <scr.slb.COM!davis@eddie.mit.edu>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: latex outline mode
;Source-Info:  From (or Sender) name not authenticated.
;
;
;
;Yes, I know that someone's working on a new improved outline mode that
;can be customised to fit other major modes, but if you're tired of
;waiting, here's an outline mode for LaTeX.
;
;Please see the doc string for latex-outline-mode to for a note on
;conditions when this will break.
;
;Also, of course, this won't work if you use non-standard sectioning commands.
;
;Otherwise,
;have fun until the real outline mode comes along,
;
;Paul
;----- cut here ----
;; Outline mode for LaTeX
;; Paul Davis <davis@blue.sdr.slb.com> June 1988


;; Copyright (C) Schlumberger Cambridge Research, 1989
;; under the same terms as the GNU Emacs general public licence.

(load "outline" nil t)

(make-local-variable 'outline-regexp)

(defvar latex-outline-regexp
"\\(\\\\doc\\|\\\\part\\|\\\\chapt\\|\\\\\\(sub\\)*section\\|\\\\\\(sub\\)*paragraph\\(.*$\\)\\)"
"Regexp for LaTeX standard section commands. Note the devious
skullduggery - since the length of the match with this regexp
determines the level of the heading, we try to extend the paragraph
entries by adding the rest of the line. This often won't work....")

(defun update-mode-line ()
  (set-buffer-modified-p (buffer-modified-p)))

(defconst latex-outline-mode-prefix-key "\C-c"
  "prefix key for all LaTeX outline mode commands")

(defvar before-latex-outline-mode-map nil
  "Buffer-local variable to hold a copy of the local keymap before
latex-outline-mode modifies it")

(defvar latex-outline-mode-map nil
  "Keymap for LaTeX outline mode")

(defvar latex-outline-mode nil
  "non-nil when latex-outline-mode is activated")

(defun latex-outline-show-children ()
  (interactive)
  (show-children 3))

(if latex-outline-mode-map
      nil
  (setq latex-outline-mode-map (copy-keymap TeX-mode-map))
  (define-key latex-outline-mode-map "n" 'outline-next-visible-heading)
  (define-key latex-outline-mode-map "p" 'outline-previous-visible-heading)
  (define-key latex-outline-mode-map "\C-i" 'latex-outline-show-children)
  (define-key latex-outline-mode-map "\C-s" 'show-subtree)
  (define-key latex-outline-mode-map "\C-h" 'hide-subtree)
  (define-key latex-outline-mode-map "\C-u" 'up-heading)
  (define-key latex-outline-mode-map "f" 'outline-forward-same-level)
  (define-key latex-outline-mode-map "b" 'outline-backward-same-level))
  
(fset 'latex-outline-mode-map latex-outline-mode-map)

(make-variable-buffer-local 'latex-outline-mode)

(defun latex-outline-mode (arg)
  "
An outline minor mode for LaTeX. Very similar to the  major
mode outline-mode, except for key binding changes, needed to accomodate
the existing TeX-mode map. For general information, see outline-mode.

This mode recognizes all standard LaTeX sectioning commands, with the
addition of the initial \\documentstyle[]{} (useful for hiding a messy
group of pagestyle and dimension-setting command). HOWEVER, because of
the way in which outline-mode works, there are difficulties with
LaTeX's \\paragraph and \\subparagraph. For LaTeX, these are inferior to the
\\subsubsection command, but not as long, so outline-level gets
mixed up over which is inferior to which. If you wish to use these
sectioning commands and outline mode at the same time, then you should
ensure that your section entries for these commands create the correct
ranking order by making \\paragraph{} lines longer than 14 chars and
\\subparagraph lines longer than \\paragraph lines. For example:

\\subsubsection{a story]

<text>

\\paragraph{A Suitable heading}

<text>

\\subparagraph{Another Suitable Heading}

<text>.

The following keybindings (prefixed by C-c) are available:
\\{latex-outline-mode-map}"

  (interactive "P")
  (setq latex-outline-mode
	(if (null arg)
	    (not latex-outline-mode)
	  (> (prefix-numeric-value arg) 0)))

  (update-mode-line)
  (if latex-outline-mode
      (progn
	(make-local-variable 'outline-regexp)
	(setq outline-regexp latex-outline-regexp)
	(make-local-variable 'outlining)
	(setq outlining (default-value 'outlining))
	(make-local-variable 'before-latex-outline-mode-map)
	(setq before-latex-outline-mode-map (current-local-map))
	(use-local-map (copy-keymap (current-local-map)))
	(local-unset-key latex-outline-mode-prefix-key)
	(local-set-key latex-outline-mode-prefix-key 'latex-outline-mode-map)
	(setq selective-display t)
	(message "LaTeX outline mode on")
	(make-local-variable 'paragraph-start)
	(setq paragraph-start (concat paragraph-start "\\|^\\("
				      outline-regexp "\\)"))
	(make-local-variable 'paragraph-separate)
	(setq paragraph-separate (concat paragraph-separate "\\|^\\("
				   outline-regexp "\\)"))
	(run-hooks 'latex-outline-mode-hook))
    (use-local-map before-latex-outline-mode-map)
    (message "LaTeX outline mode off")))

(provide 'latex-outline)

