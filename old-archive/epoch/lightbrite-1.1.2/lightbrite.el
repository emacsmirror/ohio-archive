;;; --------------------------------------------------------------------------
;;; File: --- lightbrite.el ---
;;; Author: Marc Andreessen (marca@ncsa.uiuc.edu)
;;; Copyright (C) National Center for Supercomputing Applications, 1992.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with your copy of Epoch or Lucid Emacs; if not, write to the
;;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;;
;;; CONTENTS
;;;
;;; Lightbrite: a minor mode for source code highlighting.
;;; Version 1.1.2.
;;;
;;; FEATURES
;;;
;;; o Accurate comment highlighting.
;;; o Regexp touchup highlighting.
;;; o Easy specification of visual attributes of multiple 
;;;   highlighting styles.
;;; o Instant highlighting (as you type) with adjustable 
;;;   responsiveness.
;;; o Buffer size thresholds for both comment and touchup 
;;;   highlighting.
;;; o Use of minor mode and local keymaps.
;;; o Re-highlight visible screen, current paragraph, current
;;;   function, immediate area, or entire buffer.
;;; o Large assortment of default mode-specific highlighting
;;;   patterns.
;;; o Automatic highlight on find-file and/or write-file.
;;; o Special handling of mail and rmail modes.
;;; o Works with both Epoch and Lucid Emacs transparently.
;;; o Automatic bug report generation.
;;;
;;; NEW FEATURES in 1.1.1
;;;
;;; o brite::mode-hook and brite::mode-exit-hook.
;;; o Menubar support for Lucid Emacs.
;;; o Pointer glyph changes to watch when Lightbrite is working
;;;   in both Epoch and Lucid Emacs.
;;;
;;; NEW FEATURES in 1.1.2
;;;
;;; o Lightbrite is now friendly to other zones/extents in the
;;;   buffer: it won't delete them.
;;;
;;; BUG REPORTS
;;;
;;; Bugs should be reported with brite::submit-bug-report.  Bug
;;; *fixes* are especially encouraged.
;;;
;;; HISTORY
;;;
;;; Sun Apr  5 22:09:51 1992 -- Marc Andreessen
;;;   Made available v1.0 with incomplete docs.
;;; Sat Sep 12 02:59:44 1992 -- Marc Andreessen
;;;   v1.1 functional.  Texinfo docs discarded for the time being.
;;; Sat Sep 12 04:42:23 1992 -- Marc Andreessen
;;;   Put initial version of 1.1 on ftp server.
;;; Sat Sep 12 20:37:04 1992 -- Marc Andreessen
;;;   Added Lucid Emacs menubar code; added brite::mode-hook
;;;   and brite::mode-exit-hook.
;;; Sat Sep 12 22:31:04 1992 -- Marc Andreessen
;;;   Added pointer munging code.
;;; Sun Sep 13 17:11:33 1992 -- Marc Andreessen
;;;   Made 1.1.1 available on ftp server.
;;; Sun Sep 13 17:12:43 1992 -- Marc Andreessen
;;;   Changed compare of change-counter with change-interval back
;;;   to >= (in case change-interval is decreased by user).
;;; Sun Sep 13 18:15:24 1992 -- Marc Andreessen
;;;   Added brite::submit-bug-report to menu.
;;; Mon Sep 14 03:28:55 1992 -- Marc Andreessen
;;;   Make Lightbrite friendly to other extents in buffer.
;;;   Don't highlight-on-find rmail-mode buffers.
;;;
;;; INTRODUCTION TO HIGHLIGHTING
;;;
;;; Lightbrite takes advantage of one the major extensions to GNU Emacs in
;;; both Epoch and Lucid Emacs: the concept of "zones" (or "extents").  A
;;; zone is an arbitrary region of a buffer that may have properties
;;; distinct from other regions in the buffer; such properties include
;;; foreground and background color, underlining, font, and read-only
;;; status.  Programs written in Emacs Lisp, like Lightbrite, can direct
;;; Epoch and Lucid Emacs to create any number of zones in a buffer based
;;; on whatever criteria the program sees fit to use.
;;; 
;;; Lightbrite is called a "highlighting" package because it highlights
;;; regions of buffers based on the buffer's syntactic content.  An
;;; example of a highlighted buffer would be a buffer of C code in which
;;; all `switch' statements are displayed in green and all `break'
;;; statements are displayed in red, while the normal text color is white
;;; and comments are displayed in blue.  To achieve the highlighted
;;; effect, a zone is created for each region of text Lightbright wants to
;;; highlight.
;;; 
;;; Zones are specified by their beginning and ending points in the
;;; buffer.  Editing actions that take place in or near zones will affect
;;; the zones themselves.  For example, inserting text in the middle of a
;;; zone expands the size of the zone, by moving the ending point further
;;; out.  Because of these and other reasons, it is often necessary to
;;; refresh a buffer's highlighting; this involves erasing existing zones
;;; and re-highlighting.  Lightbrite provides an easy way to do this; the
;;; key sequence 'C-z SPC' re-highlights the current screen.
;;; 
;;; Lightbrite also supports a preliminary implementation of an "instant
;;; highlighting" system, wherein your text is highlighted as you type.
;;; Theoretically, this should lead to completely transparent, always
;;; up-to-date highlighting; unfortunately, the current implementation is
;;; not quite that waterproof, but it is functional.
;;;
;;; BASIC USAGE
;;;
;;; Lightbrite requires either Epoch (4.2 or higher) or Lucid Emacs
;;; (19.3 or higher).  It will not work with vanilla GNU Emacs 18.x.
;;;
;;; To use lightbrite, install it and syntax-decode.el in a directory in
;;; your Elisp search path and byte-compile both files; then put:
;;;
;;; (require 'lightbrite)
;;;
;;; in your .emacs file.  Lightbrite automatically adapts itself to
;;; either Epoch or Lucid Emacs when it is loaded.  (If you plan to
;;; use the same .elc files with both Epoch and Lucid Emacs, you should
;;; byte-compile them with Epoch rather than Lucid Emacs.)
;;;
;;; Lightbrite assumes a dark (black) background.  If you have a
;;; light (white) background, you'll probably want to put the following
;;; lines in your .emacs file prior to (require 'lightbrite):
;;;
;;; (defvar brite::comment-styles '(("foreground" . "cadetblue"))
;;;   "*Style specification list for comment style.")
;;; (defvar brite::mild-styles '(("foreground" . "grey70"))
;;;   "*Style specification list for mild style.")
;;; (defvar brite::striking-styles '(("foreground"  . "royalblue"))
;;;   "*Style specification list for striking style.")
;;; (defvar brite::intense-styles '(("foreground" . "purple"))
;;;   "*Style specification list for intense style.")
;;; (defvar brite::blazing-styles '(("foreground" . "orangered"))
;;;   "*Style specification list for blazing style.")
;;; (defvar brite::hot-styles '(("foreground" . "darkgoldenrod"))
;;;   "*Style specification list for hot style.")
;;; (defvar brite::earnest-styles '(("foreground" . "darkorchid"))
;;;   "*Style specification list for earnest style.")
;;;
;;; By default, visiting a file will enable lightbrite-mode in the new
;;; buffer.  Then, when you have made changes to part of a file, press
;;; 'C-z SPC' to re-highlight the visible window.
;;;
;;; If you're using Lucid Emacs, Lightbrite will also set up a menubar
;;; entry for itself; from this menu (entitled "Light") you will be
;;; able to do most interesting Lightbrite-related actions.
;;;
;;; ADVANCED USAGE
;;;
;;; Lightbrite usually takes a while to highlight a large file.  To
;;; avoid total catastrophe when loading a huge file, Lightbrite
;;; allows you to set thresholds on buffer size for the two types of
;;; highlighting: comment highlighting and touchup (everything but
;;; comments) highlighting.  See the variables
;;; 'brite::comment-threshold' and 'brite::touchup-threshold' for more
;;; information.  By default, the thresholds are given reasonable
;;; values for a moderately fast RISC workstation; you may want to
;;; adjust them up or down based on your platform's speed.
;;;
;;; If you're typing along and you suddenly exceed one of the two
;;; thresholds, Lightbrite will stop highlighting in that manner
;;; (comment or touchup).  If you want to raise the threshold enough
;;; to continue editing the current file without having highlighting
;;; disappear, you can press 'C-z T' or 'C-z C' to "balloon" the
;;; touchup and comment highlighting thresholds respectively; the
;;; values are raised to the size of the buffer plus an extra 1000
;;; characters.
;;;
;;; Instant highlighting can be enabled on a per-buffer basis by
;;; pressing 'C-z a'.  When active, instant highlighting causes the
;;; visible window to be re-highlighted every 'brite::change-interval'
;;; changes to the buffer's text.  You may notice strange effects when
;;; using instant highlight.  For example, if the window is
;;; re-highlighted while you're typing in the middle of a string (with
;;; no closing quote), the Emacs expression parsing mechanism that
;;; Lightbrite uses will assume that text after point is in the
;;; current string when it really isn't, and that text will be
;;; highlighted (incorrectly) accordingly.
;;;
;;; Lightbrite has other, less interesting, features; they can be
;;; discovered by trolling through the code.
;;;
;;; KNOWN PROBLEMS
;;;
;;; Jamie Zawinski's mail-abbrevs package doesn't work well with
;;; Lightbrite at the moment.  A hacked version of mail-abbrevs that
;;; does work can be found on ftp.ncsa.uiuc.edu in
;;; /outgoing/marca/epoch.
;;;
;;; FUTURE ENHANCEMENTS
;;;
;;; Use map-extents and related Epoch primitive (probably won't
;;; enhance performance much, but still a good idea).
;;;
;;; Link in with Lucid Emacs font-lock primitives.
;;;
;;; Eventually, Lightbrite will provide extensive customization
;;; capabilities and additional functionality that I just don't have
;;; time to implement right now.  Ideas and code are more than
;;; welcome.
;;;
;;; ACKNOWLEDGEMENTS
;;;
;;; Code and ideas stolen from Ken Wood, Heinz W. Schmidt, Paul
;;; Nakada, Dan LaLiberte, Lynn Slater, Barry Warsaw, Michael
;;; Lamoureux, Simon Marshall, Jerry Graves, and others.
;;;
;;; LCD Archive Entry:
;;; lightbrite|Marc Andreessen|marca@ncsa.uiuc.edu
;;; |Source highlighting for Epoch and Lucid Emacs
;;; |13 Sep 1992|1.1.2|

;;; ----------------------------- Start 'er up. ------------------------------

(require 'syntax-decode)

(defconst brite::version "Version 1.1.2" "Lightbrite version string.")

;; Check for Epoch.
(defvar brite::running-epoch (boundp 'epoch::version)
  "Non-nil if running Epoch.")

;; Check for Lucid Emacs.
(defvar brite::running-lemacs (string-match "Lucid" emacs-version)
  "Non-nil if running Lucid Emacs.")

;;; ------------------- Lightbrite zone/style manipulation -------------------

;; These functions provide zone/style (extent/face) manipulation
;; functionality for both Epoch and Lucid Emacs.

(cond
 ;; LUCID EMACS LAYER
 (brite::running-lemacs
  (defun brite::zone-at (p)
    "Do extent-at."
    (extent-at p))
  (defun brite::add-zone (beginning end sty)
    "Do make-extent and set-extent-face; set extent data to 'lightbrite."
    (let ((extent (make-extent beginning end)))
      (set-extent-face extent sty)
      (set-extent-data extent 'lightbrite)))
  (defun brite::zone-list (&optional buffer)
    "Build a list of extents using next-extent."
    (let ((l nil) ext)
      ;; Grab the first extent in the buffer.
      (if buffer
          (setq ext (next-extent buffer))
        (setq ext (next-extent (current-buffer))))
      ;; Build a list of extents until we run out.
      (while ext
        (setq l (cons ext l))
        (setq ext (next-extent ext)))
      l))
  (defun brite::clear-zones (&optional buffer)
    "Clear extents that we created in BUFFER with delete-extent."
    (let ((l (brite::zone-list buffer)))
      (while (car l)
        ;; Make sure it's ours before we wipe it out.
        (if (eq 'lightbrite (extent-data (car l)))
            (delete-extent (car l)))
        (setq l (cdr l)))))
  (defun brite::epoch-version ()
    "Fake variable epoch::version."
    "Epoch?  We don' need no steenking Epoch!")
  (defun brite::clear-zones-in-region (start end)
    "Prune zones in region from START and END from zones in buffer."
    (let ((foo-list (brite::zone-list)))
      (while foo-list
        (let* ((foo (car foo-list)) (foo-start (extent-start-position foo)))
          ;; We remove zones that start inside this region, if we created them.
          (if (and (eq 'lightbrite (extent-data foo)) 
                   (>= foo-start start) (<= foo-start end))
              (delete-extent foo))
          (setq foo-list (cdr foo-list))))))
  (defvar brite::original-pointer nil "Original screen pointer.")
  (defun brite::set-wait-pointer ()
    "Set pointer to wait state."
    (let ((screen (selected-screen)))
      (setq brite::original-pointer 
            (cdr (assoc 'pointer (screen-parameters screen))))
      (x-set-screen-pointer screen x-gc-pointer-shape)
      (sit-for 0)))
  (defun brite::set-ready-pointer ()
    "Set pointer to ready state."
    (let ((screen (selected-screen)))
      (x-set-screen-pointer screen brite::original-pointer)
      (sit-for 0))))
 
 ;; EPOCH LAYER.
 (brite::running-epoch
  (defun brite::zone-at (p)
    "Do epoch::zone-at."
    (epoch::zone-at p))
  (defun brite::add-zone (beginning end sty)
    "Do epoch::add-zone; set zone data to 'lightbrite."
    (let ((zone (epoch::add-zone beginning end sty)))
      (epoch::set-zone-data zone 'lightbrite)))
  (defun brite::zone-list (&optional buffer)
    "Do epoch::zone-list."
    (epoch::zone-list buffer))
  (defun brite::clear-zones (&optional buffer)
    "Clear extents that we created in BUFFER with epoch::delete-zone."
    (let ((l (brite::zone-list buffer)))
      (while (car l)
        ;; Make sure it's ours before we wipe it out.
        (if (eq 'lightbrite (epoch::zone-data (car l)))
            (epoch::delete-zone (car l)))
        (setq l (cdr l)))))
  (defun brite::epoch-version ()
    "Look at epoch::version"
    epoch::version)
  (defun brite::clear-zones-in-region (start end)
    "Prune zones in region from START and END from zones in buffer."
    (catch 'brite::done-clearing-zones
      (let ((foo-list (brite::zone-list)))
        (while foo-list
          (let* ((foo (car foo-list)) (foo-start (epoch::zone-start foo)))
            ;; We remove zones that start inside this region.
            (if (and (eq 'lightbrite (epoch::zone-data foo))
                     (>= foo-start start) (<= foo-start end))
                (epoch::delete-zone foo))
            ;; If the zone starts after the end of this region,
            ;; we're done.
            (if (> foo-start end)
                (throw 'brite::done-clearing-zones t)))
          (setq foo-list (cdr foo-list))))))
  (defvar brite::original-pointer nil "Original screen pointer.")
  (defun brite::set-wait-pointer ()
    "Set pointer to wait state."
    (setq brite::original-pointer (epoch::cursor-glyph))
    (epoch::cursor-glyph 150)
    (sit-for 0))
  (defun brite::set-ready-pointer ()
    "Set pointer to ready state."
    (epoch::cursor-glyph brite::original-pointer)
    (sit-for 0)))
 
 ;; Vanilla Emacs.
 (t
  (error "Lightbrite requires Epoch or Lucid Emacs.")))

;;; --------------------------- brite::make-style ----------------------------

(defun brite::make-style (styles name)
  "Make a new style with STYLES with name NAME.
Each component of STYLES looks like (\"style\" . \"value\").

For example, to set the foreground color to red and the font to 9x15bold, use
        '((\"foreground\" . \"red\") (\"font\" . \"9x15bold\"))

The \"style\" is in fact concatenated onto \"set-style-\" for Epoch or
\"set-face-\" for Lucid Emacs, and this function is called to set the
style value.

The different Epoch styles are background, background-stipple,
cursor-background, cursor-foreground, cursor-stipple, font,
foreground, pixmap, stipple and underline.  See the corresponding
set-style-* functions for their effects.

The different Lucid Emacs styles are background, background-pixmap,
font, foreground, or underline-p (Lightbrite also understands style).
(Currently Lucid Emacs can only underline in the current foreground
color.)  See the corresponding set-face-* functions for their
effects."
  (let ((new-style 
         (cond
          (brite::running-lemacs
           (make-face name))
          (brite::running-epoch
           (make-style)))))
    (while styles
      (cond
       (brite::running-lemacs
        (funcall (intern
          (concat "set-face-" 
                  (if (string= (car (car styles)) "underline")
                      "underline-p"
                    (car (car styles)))))
         new-style (cdr (car styles))))
       (brite::running-epoch
        (funcall  (intern 
          (concat "set-style-" (car (car styles))))
         new-style (cdr (car styles)))))
      (setq styles (cdr styles)))
    new-style))

;;; --------------------------------------------------------------------------
;;; ------------------------------- VARIABLES --------------------------------
;;; --------------------------------------------------------------------------

;;; ------------------------- buffer-local variables -------------------------

;; Users can setq the *-default variables before loading this file
;; to affect defaults for buffer-local variables.

;; Define the default values.
(defvar brite::comment-highlight-on-default t
  "*Default value for brite::comment-highlight-on.")
(defvar brite::touchup-highlight-on-default t
  "*Default value for brite::touchup-highlight-on.")
(defvar brite::touchup-check-string-default t
  "*Default value for brite::touchup-check-string.")
(defvar brite::touchup-check-zone-default t
  "*Default value for brite::touchup-check-zone.")
(defvar brite::highlight-on-write-default nil
  "*Default value for brite::highlight-on-write.")

;; Define the buffer-local variables.
(defvar brite::comment-highlight-on nil
  "T if comments should be highlighted in this buffer; NIL otherwise.")
(defvar brite::touchup-highlight-on nil
  "T if syntax-directed touchup should be done in this buffer; NIL otherwise.")
(defvar brite::touchup-check-string nil
  "T if in-string checking should be done in this buffer; NIL otherwise.")
(defvar brite::touchup-check-zone nil
  "T if in-zone checking should be done in this buffer; NIL otherwise.")
(defvar brite::highlight-on-write nil
  "T if this buffer should be re-highlighted on write; NIL otherwise.")
(defvar brite::instant-highlight-on nil 
  "Flag for instant highlighting.  Call brite::toggle-instant-highlight-on
to turn instant highlighting on and off.")

;; Make the buffer-local variables buffer-local.
(make-variable-buffer-local 'brite::comment-highlight-on)
(make-variable-buffer-local 'brite::touchup-highlight-on)
(make-variable-buffer-local 'brite::touchup-check-string)
(make-variable-buffer-local 'brite::touchup-zone-string)
(make-variable-buffer-local 'brite::highlight-on-write)
(make-variable-buffer-local 'brite::instant-highlight-on)

;; Set the defaults.
(setq-default brite::comment-highlight-on brite::comment-highlight-on-default)
(setq-default brite::touchup-highlight-on brite::touchup-highlight-on-default)
(setq-default brite::touchup-check-string brite::touchup-check-string-default)
(setq-default brite::touchup-check-zone brite::touchup-check-zone-default)
(setq-default brite::highlight-on-write brite::highlight-on-write-default)
(setq-default brite::instant-highlight-on nil)

;;; --------------------------- control variables ----------------------------

;; Users can setq these before loading this file.

(defvar brite::highlight-on-find t
  "*T if we should highlight each new file when it's first loaded.")

;; Lucid Emacs does extents more efficiently than Epoch does zones,
;; so make thresholds respecitvely higher.
(defvar brite::comment-threshold (if brite::running-lemacs 200000 100000)
  "*Integer for maximum size of file for which comments are highlighted,
or NIL if comments should always be highlighted.")
(defvar brite::touchup-threshold (if brite::running-lemacs 75000 30000)
  "*Integer for maximum size of file for which touchup highlighting is done,
or NIL if touchup highlighting should always be done.")

;; Setting this value small really makes it look good, but gives
;; quite a performance hit.
(defvar brite::change-interval 16
  "*Number of changes between line highlighting, when after-change-function
is activated.")

(defvar brite::area-extent 160
  "*Half of the size of a local area.")

;;; --------------------------------- hooks ----------------------------------

(defvar brite::mode-hook nil 
  "*Hooks to run when entering lightbrite-mode.")
(defvar brite::mode-exit-hook nil 
  "*Hooks to run when leaving lightbrite-mode.")

;;; --------------------------- colors and styles ----------------------------

;; Users can set either brite::*-styles to a list of style alists,
;; or brite*-style directly to a desired style.

;; The following colors were chosen for a black background.  Suggested
;; foreground colors are wheat, grey80-grey90, #ffa07a, #ffaf7f,
;; #ff7f24, and #efafff.  An alternate set of colors for a white
;; background is upstairs in the initial comments.  If you come up
;; with a nice set of fg/bg/highlight colors, send them to me.

(defvar brite::comment-styles '(("foreground" . "#7f9fcf"))
  "*Style specification list for comment style.")
(defvar brite::mild-styles '(("foreground" . "red"))
  "*Style specification list for mild style.")
(defvar brite::striking-styles '(("foreground" . "hot pink"))
  "*Style specification list for striking style.")
(defvar brite::intense-styles '(("foreground" . "yellow"))
  "*Style specification list for intense style.")
(defvar brite::blazing-styles '(("foreground" . "green"))
  "*Style specification list for blazing style.")
(defvar brite::hot-styles '(("foreground" . "magenta"))
  "*Style specification list for hot style.")
(defvar brite::earnest-styles '(("foreground" . "alice blue"))
  "*Style specification list for earnest style.")

(defvar brite::comment-style
  (brite::make-style brite::comment-styles 'brite::comment-style)
  "*Style used to highlight comments.")
(defvar brite::mild-style 
  (brite::make-style brite::mild-styles 'brite::mild-style)
  "*Style used to highlight mildly.")
(defvar brite::hot-style 
  (brite::make-style brite::hot-styles 'brite::hot-style)
  "*Style used to highlight hotly.")
(defvar brite::earnest-style 
  (brite::make-style brite::earnest-styles 'brite::earnest-style)
  "*Style used to highlight in earnest.")
(defvar brite::intense-style 
  (brite::make-style brite::intense-styles 'brite::intense-style)
  "*Style used to highlight intensely.")
(defvar brite::blazing-style 
  (brite::make-style brite::blazing-styles 'brite::blazing-style)
  "*Style used to highlight blazingly.")
(defvar brite::striking-style 
  (brite::make-style brite::striking-styles 'brite::striking-style)
  "*Style used to highlight strikingly.")

;;; --------------------------------------------------------------------------
;;; --------------------------- internal variables ---------------------------
;;; --------------------------------------------------------------------------

;; You shouldn't tamper with these.  Your firstborn will be sacrificed
;; to the Great Eternal Lisp Engine if you do.

(defvar brite::highlight-done nil "Flag for highlighting done once here.")
(make-variable-buffer-local 'brite::highlight-done)
(setq-default brite::highlight-done nil)

(defvar brite::beginning-point 0 "Starting point for regexp searches.")
(defvar brite::ending-point 0 "Ending point for regexp searches.")

(defvar brite::change-counter 0 "Internal counter to track number of changes.")

(defvar brite::diagnostics-on nil
  "*If T, print diagnostics to minibuffer as appropriate.")

;; When enabled, infinite loop is hit for some patterns.  FIX IT, STUPID.
(defvar brite::clip-surrounding-whitespace nil
  "T if we should never highlight surrounding whitespace; NIL otherwise.
Current, this should not be set to T.")

;;; --------------------------------------------------------------------------
;;; ------------------------- MISCELLANEOUS ROUTINES -------------------------
;;; --------------------------------------------------------------------------

;;; ---------------------------- brite::in-zone-p ----------------------------

(defun brite::in-zone-p (point)
  "T if POINT is in a zone, NIL otherwise."
  (if (brite::zone-at point) t nil))

;;; ----------------------- brite::in-quoted-string-p ------------------------

(defun brite::in-quoted-string-p (point)
  "Check for POINT in quoted string; return T if so, NIL if not."
  (save-excursion
    (beginning-of-defun)
    (if (nth 3 (parse-partial-sexp (point) point)) t nil)))

;;; ------------------------- brite::regexp-touchup --------------------------

;; Ideas for a Mocklisp defun:
;;   zonify-pattern START END STYLE INTERNAL-REGEXP START-REGEXP END-REGEXP
;;     Scan current buffer from 'START' to 'END' and locate strings
;;     that match given REGEXP patterns; when found, add a zone with
;;     style STYLE.

(defun brite::regexp-touchup (regexp sty &optional everytime)
  "Highlight all matches of REGEXP in a given style.  Second arg STY 
is a style.  Optional third arg EVERYTIME indicates that pattern should 
always be highlighted, even when in a zone or string."
  (let (brite::beginning brite::end (case-fold-search nil))
    (save-excursion
      (goto-char brite::beginning-point)
      ;; Loop through all matches in this buffer.
      (while (re-search-forward regexp brite::ending-point t)
        ;; Preserve values of begin/end from search.
        ;; Now we skip whitespace on either side of a match
        ;; as well, by searching forward from start and
        ;; backward from end, if brite::clip-surrounding-whitespace
        ;; is t.
        (cond
         (brite::clip-surrounding-whitespace
          (setq brite::end (match-end 0)
                brite::beginning (progn
                                   (goto-char (match-beginning 0))
                                   (re-search-forward "[^\t ]")
                                   (match-beginning 0))
                brite::end (progn
                             (goto-char brite::end)
                             (re-search-backward "[^\t ]")
                             (match-end 0))))
         (t
          (setq brite::beginning (match-beginning 0)
                brite::end (match-end 0))))
        ;; If everytime flag not set, and brite::touchup-check-zone
        ;; not set, check for in-zone-p.
        (if (or everytime (not brite::touchup-check-zone) 
                (not (brite::in-zone-p brite::beginning)))
            ;; Either everytime is set, or we're not checking
            ;; strings, or we're not in a string.
            (if (or everytime
                    (not brite::touchup-check-string) 
                    (not (brite::in-quoted-string-p brite::beginning)))
                (brite::add-zone brite::beginning brite::end sty)))))))

;;; --------------------- convenience touchup functions ----------------------

(defun brite::regexp-touchup-mild (regexp)
  "Touchup REGEXP mildly."
  (brite::regexp-touchup regexp brite::mild-style))
(defun brite::regexp-touchup-hot (regexp)
  "Touchup REGEXP hotly."
  (brite::regexp-touchup regexp brite::hot-style))
(defun brite::regexp-touchup-earnest (regexp)
  "Touchup REGEXP in earnest."
  (brite::regexp-touchup regexp brite::earnest-style))
(defun brite::regexp-touchup-intense (regexp)
  "Touchup REGEXP intensely."
  (brite::regexp-touchup regexp brite::intense-style))
(defun brite::regexp-touchup-intense-always (regexp)
  "Touchup REGEXP intensely always."
  (brite::regexp-touchup regexp brite::intense-style t))
(defun brite::regexp-touchup-blazing (regexp)
  "Touchup REGEXP blazingly."
  (brite::regexp-touchup regexp brite::blazing-style nil))
(defun brite::regexp-touchup-striking (regexp)
  "Touchup REGEXP strikingly."
  (brite::regexp-touchup regexp brite::striking-style nil))
(defun brite::regexp-touchup-comment (regexp)
  "Touchup REGEXP as a comment."
  (brite::regexp-touchup regexp brite::comment-style nil))

;;; --------------------------------------------------------------------------
;;; -------------------------- COMMENT HIGHLIGHTING --------------------------
;;; --------------------------------------------------------------------------

;;; ----------------------- brite::highlight-comments ------------------------

;; This function is stolen almost verbatim from Ken Wood's package.
(defun brite::highlight-comments ()
  "Highlight all comments in a buffer if the buffer's size does not
exceed brite::comment-threshold."
  ;; Silently do nothing if there are no regexps to search with
  ;; or if the buffer is too large to highlight.
  (if (and syndecode-comment-start-regexp syndecode-comment-end-regexp
           brite::comment-threshold 
           (< (buffer-size) brite::comment-threshold))
      (let ((starting-point brite::beginning-point)
            comment-start-begin comment-start-end comment-end-end)
        (save-excursion
          (goto-char brite::beginning-point)
          ;; Algorithm is: search for start of a comment,
          ;; make sure it really is a comment; then highlight
          ;; from there to the end of the comment.
          ;; First, find a comment-start sequence.
          (while (re-search-forward syndecode-comment-start-regexp
                                    brite::ending-point t)
            (progn
              (setq comment-start-begin (match-beginning 0)
                    comment-start-end (match-end 0))
              ;; Check that the comment start sequence really does
              ;; indicate the start of a comment, and that it's not
              ;; inside a string etc.
              (if (nth 4 (parse-partial-sexp starting-point comment-start-end))
                  ;; Yes, this is really the start of a comment
                  (progn
                    (goto-char comment-start-end)
                    ;; Find the end of the comment by searching
                    ;; for a comment terminating sequence.
                    ;; Second arg was changed to brite::ending-point
                    ;; to keep emacs from complaining during per-line
                    ;; highlighting.
                    (re-search-forward syndecode-comment-end-regexp
                                       brite::ending-point t)
                    (setq comment-end-end (point))
                    ;; Highlight the comment
                    (brite::add-zone comment-start-begin comment-end-end
                                    brite::comment-style)
                    ;; Start the next syntax parse at the end of the
                    ;; comment just processed.
                    (setq starting-point comment-end-end)))))))))

;;; --------------------------------------------------------------------------
;;; -------------------------- TOUCHUP HIGHLIGHTING --------------------------
;;; --------------------------------------------------------------------------

;;; ------------------------- brite::true-mode-name --------------------------

;; Author: Lynn Slater (lrs@indetech.com).
(defun brite::true-mode-name ()
  "Returns the name of the mode in such a form that the mode may be
re-established by calling the function named by appending '-name' to
this string.  This differs from the variable called mode-name in that
this is guaranteed to work while the values held by the variable may
have embedded spaces or other junk."
  (let ((major-mode-name (symbol-name major-mode)))
    (substring major-mode-name 0
               (or (string-match "-mode" major-mode-name)
                   (length major-mode-name)))))

;;; ------------------------ brite::do-buffer-touchup ------------------------

;; Datatype must be preceded by whitespace, open paren, or open brace.
(defvar brite::c-datatype-prefix "[ \n\t({]")
;; Datatype must be followed by whitespace, close paren, or *.
(defvar brite::c-datatype-suffix "[ \n\t)*]")

;; Keyword must be preceded by whitespace or open brace.
(defvar brite::c-keyword-prefix "[ \n\t{]")
;; Keyword must be followed by whitespace, semicolon, or open paren.
(defvar brite::c-keyword-suffix "[ \n\t;(]")

(defvar brite::c-directives
  "\\(#endif\\|#else\\|#ifdef\\|#ifndef\\|#if\\|NULL\\)")
(defvar brite::c-defines
  "\\(#define\\|#include\\|#undef\\)")
(defvar brite::c-dataglobs 
  "\\(typedef\\|enum\\|struct\\|union\\|[ \t]case.*:\\|[ \t]default:\\)")
(defvar brite::c-datatypes
  (concat brite::c-datatype-prefix 
          "\\(int\\|float\\|Float\\|char\\|double\\|short\\|long\\|void\\)" 
          brite::c-datatype-suffix))
(defvar brite::c-data-attributes
  "\\(unsigned\\|extern\\|static\\|public\\|private\\)")
(defvar brite::c-flagged-keywords
  (concat brite::c-keyword-prefix  "\\(return\\|exit\\|assert\\)"
          brite::c-keyword-suffix))

;; Keyword must be preceded by whitespace or open brace.
(defvar brite::perl-keyword-prefix "[ \n\t{]")
;; Keyword must be followed by whitespace, semicolon, or open paren.
(defvar brite::perl-keyword-suffix "[ \n\t;(]")

(defvar brite::perl-directives
  "\\(#endif\\|#else\\|#ifdef\\|#ifndef\\|#if\\|#include\\|#define\\|#undef\\)")
(defvar brite::perl-flow
  (concat brite::perl-keyword-prefix 
          "\\(if\\|until\\|while\\|elsif\\|else\\|unless\\|for\\|foreach\\|continue\\|exit\\|die\\|last\\|goto\\|next\\|redo\\|return\\)" 
          brite::perl-keyword-suffix))

;; In the future this will be in table format and facilities will be
;; provided for extending both the modes supported and the regexp's
;; used.
(defun brite::do-buffer-touchup ()
  "Based on current mode, do series of calls to brite::regexp-touchup routines
to highlight certain regex patterns certain colors, if the buffer's size
is less than brite::touchup-threshold."
  ;; Allow touchup-threshold to be nil also.
  (if (or (null brite::touchup-threshold)
          (< (buffer-size) brite::touchup-threshold))
      (cond
       ;;-------------------------------------------------------------------
       ((string-equal (brite::true-mode-name) "text")
        ;; String between sets of three dashes is a section header.
        ;; Three equal-signs marks a subsection.
        ;; '-->' and 'o' are 'item' indicators.
        ;; String 'Justification:' is always red;
        ;; at the moment we flag comments (in shell scripts,
        ;; etc) in red also.
        ;; Also 1> 2> etc are red, as are (1) (2) etc.
        ;; UNRESOLVED/FUTURE/CORRECTION/TODO should be flagged.
        (brite::regexp-touchup-hot "\\( ===\\|=== \\)")
        ;; clip-surrounding-whitespace really does not like ^o[ \n\t]. WHY?
        ;; (brite::regexp-touchup-earnest "\\(-?-->\\|[ \n\t]o[ \t]\\|^o[ \n\t]\\)")
        (brite::regexp-touchup-earnest "\\(-?-->\\|[ \n\t]o[ \t]\\|^@[A-Z]+\\)")
        (brite::regexp-touchup-mild 
         "\\(Justification: \\|^[0-9]+>\\|[ \t]*[0-9]+>\\|^([0-9]+)\\|[ \t]*([0-9]+)\\|-----[-]*\\)")
        (brite::regexp-touchup-comment "#.*")
        (brite::regexp-touchup-intense 
         "\\(UNRESOLVED\\|FUTURE\\|CORRECTION\\|T[Oo][ ]?D[Oo]\\|--- .* ---\\)")
        )
       ;;-------------------------------------------------------------------
       ((string-equal (brite::true-mode-name) "rmail")
        ;; These don't have beginning-of-line rules ("^From" etc.) because
        ;; I like them highlighted in messages included in other messages.
        (brite::regexp-touchup-intense "From:.*")
        (brite::regexp-touchup-hot "Date:.*")
        (brite::regexp-touchup-earnest "Subject:.*"))
       ;;-------------------------------------------------------------------
       ((string-equal (brite::true-mode-name) "rmail-summary")
        ;; If this is a message we've sent, highlight the receiver.
        (brite::regexp-touchup-hot " to: [^ ]*")
        ;; Highlight the date column.
        (brite::regexp-touchup-earnest " [ 0-9][0-9]-[A-Za-z][A-Za-z][A-Za-z] "))
       ;;-------------------------------------------------------------------
       ((string-equal (brite::true-mode-name) "mail")
        (brite::regexp-touchup-mild "--text follows this line--")
        (brite::regexp-touchup-earnest "Subject:.*")
        (brite::regexp-touchup-hot "\\(^To:.*\\|^CC:.*\\)")
        (brite::regexp-touchup-striking "^BCC:.*")
        (brite::regexp-touchup-blazing "^In\-reply\-to:.*"))
       ;;-------------------------------------------------------------------
       ((string-equal (brite::true-mode-name) "texinfo")
        (brite::regexp-touchup-intense 
         "\\(^@node[^,]*\\|^@table\\|@itemize\\|^@end \\(table\\|itemize\\)\\)")
        (brite::regexp-touchup-striking 
         "\\(^@menu\\|^@item\\|^@defun\\|^@defvar\\|^@example\\|^@end \\(menu\\|defun\\|defvar\\|example\\)\\)")
        (brite::regexp-touchup-blazing 
         "\\(^@chapter.*\\|^@section.*\\|^@subs.*\\|^@unnumbered\\|^@appendix\\)")
        ;; Code fragments and keystrokes in earnest.
        (brite::regexp-touchup-earnest 
         "\\(@code{[^}]*}\\|\@kbd{[^}]*}\\|@file{[^}]*}\\)")
        ;; Menu entries and xrefs in magenta.
        (brite::regexp-touchup-hot 
         "\\(\*.*::\\|@xref{[^}]*}\\|@var{[^}]*}\\)")
        ;; (brite::regexp-touchup-intense 
        ;; "\\(^@chapter.*\\|^@section.*\\|^@unnumbered.*\\)")
        (brite::regexp-touchup-mild "\\(@dfn{[^}]*}\\|^@[a-z]index.*\\)")
        )
       ;;-------------------------------------------------------------------
       ((or (string-equal mode-name "AUC-LaTeX") (string-equal mode-name "LaTeX"))
        (brite::regexp-touchup-intense 
         "\\([\\]document.*[}]\\|[\\]input.*[}]\\|[\\]include.*[}]\\|includeonly.*[}]\\)")
        (brite::regexp-touchup-hot "\\([\\]begin.*[}]\\|[\\]end.*[}]\\)")
        ;; String between sets of three dashes is in a comment banner.
        (brite::regexp-touchup-intense-always "--- .* ---")
        ;; For font changes, pick up:
        ;;   o Opening brace, backslash, and keyword.
        ;;   o Any number of characters, except }.
        ;;   o Closing brace.
        ;; How to pick up entire \verb<char><string><char>?
        ;; How to handle multiline footnote?
        (brite::regexp-touchup-mild 
         "\\([\\]item\\|[{][\\]it[^}]*[}]\\|[{][\\]sc[^}]*[}]\\|[{][\\]em[^}]*[}]\\|[\\]verb\\|[\\]footnote\\|[\\]label.*[}]\\|[\\]ref.*[}]\\)")
        ;; Assume citation is one long word; always on one line.
        ;; Also get three dashes in normal text.
        (brite::regexp-touchup-earnest 
         "\\([\\]cite[^}]*[}]\\|[ \n]---[ \n]\\|[\\]chapter\\|[\\]section\\|[\\]subsection\\|[\\]subsubsection\\|[\\]appendix\\|[\\]tableofcontents\\|[\\]newpage\\)"))
       ;;-------------------------------------------------------------------
       ((or (string-equal mode-name "C") (string-equal mode-name "C++"))
        (brite::regexp-touchup-mild brite::c-directives)
        ;; These are removed for performance reasons.
        ;; (brite::regexp-touchup-hot brite::c-datatypes)
        (brite::regexp-touchup-hot "\\([ \t]switch[ (]\\|[ \t]break;\\)")
        (brite::regexp-touchup-earnest brite::c-data-attributes)
        (brite::regexp-touchup-intense brite::c-defines)
        (brite::regexp-touchup-blazing brite::c-dataglobs)
        (brite::regexp-touchup-striking brite::c-flagged-keywords)
        ;; Function declarations: if anyone has a better one than
        ;; this, PLEASE send it to me.
        (brite::regexp-touchup-hot
         "^[_a-zA-Z][^=\n]*(\\([^)]*);\\|[^{;]*\\)") 
        (brite::regexp-touchup-intense-always "--- .* ---"))
       ;;-------------------------------------------------------------------
       ((or (string-equal mode-name "Emacs-Lisp") 
            (string-equal mode-name "Lisp"))
        (brite::regexp-touchup-hot "(interactive[^)\n]*)")
        (brite::regexp-touchup-earnest 
         "\\(setq\\|setf\\|defvar\\|defconst\\|let[ \*]\\|global-set-key\\|define-key\\)")
        (brite::regexp-touchup-mild "\\(nil\\|epoch::[^ \t\n)]*\\)")
        ;; This gets strings, unless they contain quotes, which
        ;; throws it out of whack.
        ;; (brite::regexp-touchup-mild "\"[^\"]*\"")
        (brite::regexp-touchup-intense 
         "\\(return\\|(require[^)]*)\\|(load[^)]*)\\)")
        (brite::regexp-touchup-blazing
         "(provide[^)]*)")
        ;; This relies on whitespace clipping.
        (brite::regexp-touchup-striking "defun [^ ]*")
        ;; This doesn't, but it doesn't look real good.
        ;; (brite::regexp-touchup-blazing "defun [a-zA-Z\-\*:]+")
        (brite::regexp-touchup-intense-always "--- .* ---"))
       ;;-------------------------------------------------------------------
       ((string-equal (brite::true-mode-name) "gnus-Article")
        (brite::regexp-touchup-intense "From:.*")
        (brite::regexp-touchup-hot "Date:.*")
        (brite::regexp-touchup-earnest "Subject:.*"))
       ((string-equal (brite::true-mode-name) "gnus-Subject")
        ;; If this is a message we've sent, highlight the receiver.
        ;; (brite::regexp-touchup-hot " [0-9]+:[ +]")
        (brite::regexp-touchup-earnest "\\[[ 0-9][^\]]*\\]")
        ;; (brite::regexp-touchup-intense "^D")
        ;; (brite::regexp-touchup-mild "Re: ")
        )
       ((string-equal (brite::true-mode-name) "news-reply")
        (brite::regexp-touchup-intense "Newsgroups:.*")
        (brite::regexp-touchup-hot "In-reply-to:.*")
        (brite::regexp-touchup-earnest "Subject:.*")
        (brite::regexp-touchup-mild "--text follows this line--"))
       ((string-equal mode-name "Perl")
        (brite::regexp-touchup-hot "^[ \n\t]*sub.*\\{")
        (brite::regexp-touchup-blazing "[ \n\t{]*eval[ \n\t(;]")
        (brite::regexp-touchup-mild brite::perl-directives)
        (brite::regexp-touchup-earnest brite::perl-flow)
        (brite::regexp-touchup-intense-always "\\(--- .* ---\\|=== .* ===\\)"))
       )))

;;; --------------------------------------------------------------------------
;;; ---------------------- SEARCH CONSTRAINT FUNCTIONS -----------------------
;;; --------------------------------------------------------------------------

;;; --------------------- brite::set-search-whole-buffer ---------------------

(defun brite::set-search-whole-buffer ()
  "Tell the highlighting code to search the entire buffer."
  (setq brite::beginning-point (point-min)
        brite::ending-point (point-max)))

;;; -------------------- brite::set-search-this-function ---------------------

(defun brite::set-search-this-function ()
  "Tell the highlighting code to only search this function."
  (save-excursion
    (beginning-of-defun)
    ;; Catch leading comments too.
    (backward-paragraph)
    (setq brite::beginning-point (point))
    (end-of-defun)
    (setq brite::ending-point (point))))

;;; -------------------- brite::set-search-this-paragraph --------------------

(defun brite::set-search-this-paragraph ()
  "Tell the highlighting code to only search this paragraph."
  (save-excursion
    (backward-paragraph)
    (setq brite::beginning-point (point))
    (forward-paragraph)
    (setq brite::ending-point (point))))

;;; ---------------------- brite::set-search-this-line -----------------------

(defun brite::set-search-this-line ()
  "Tell the highlighting code to only search this line."
  (save-excursion
    (beginning-of-line)
    (setq brite::beginning-point (point))
    (end-of-line)
    (setq brite::ending-point (point))))

;;; ---------------------- brite::set-search-this-area -----------------------

(defun brite::set-search-this-area ()
  "Tell the highlighting code to only search the immediate area."
  ;; We assume the re-search routines will do range-checking
  ;; (definitely checking on bound is done).
  (setq brite::beginning-point (- (point) brite::area-extent)
        brite::ending-point (+ (point) brite::area-extent)))

;;; -------------------- brite::set-search-visible-window --------------------

(defun brite::set-search-visible-window ()
  "Tell the highlighting code to only search area visible in window."
  (save-excursion
    (move-to-window-line 0)
    (forward-line -5)
    (setq brite::beginning-point (point))
    (move-to-window-line -1)
    (forward-line 5)
    (setq brite::ending-point (point))))

;;; --------------------------------------------------------------------------
;;; ---------------------- HIGHLIGHTING ACTION ROUTINES ----------------------
;;; --------------------------------------------------------------------------

;;; ----------------------- brite::internal-highlight ------------------------

(defun brite::internal-highlight (&optional pointer-action)
  "Highlight as appropriate.  Optional arg POINTER-ACTION causes pointer to
not change if equal to 'static-pointer; else cursor changes to watch shape
and back when highlighting."
  (or (eq pointer-action 'static-pointer)
      (brite::set-wait-pointer))
  (unwind-protect
      (progn
        (if brite::comment-highlight-on (brite::highlight-comments))
        (if brite::touchup-highlight-on (brite::do-buffer-touchup)))
    (or (eq pointer-action 'static-pointer)
        (brite::set-ready-pointer))))

;;; ---------------------- brite::do-highlight-on-find -----------------------

(defun brite::do-highlight-on-find ()
  "Called by find-file-hooks; goes into lightbrite-mode as appropriate."
  (if (and brite::highlight-on-find 
           ;; Don't highlight TAGS buffer!
           (not (string= (buffer-name) "TAGS"))
           ;; Don't highlight result of rmail-input.
           (not (string= (brite::true-mode-name) "rmail")))
      (progn
        (condition-case ()
            (lightbrite-mode 1)
          (error nil)))))

;;; ---------------------- brite::do-highlight-on-mail -----------------------

(defun brite::do-highlight-on-mail ()
  "Called by mail-setup-hook; goes into lightbrite-mode."
  (lightbrite-mode 1))

;;; ---------------------- brite::do-highlight-on-write ----------------------

(defun brite::do-highlight-on-write ()
  "Called by write-file-hooks; does highlighting as appropriate."
  ;; Check to see if we highlight this buffer on write.
  (if brite::highlight-on-write
      (progn
        ;; Check to see if there is any highlighting currently in effect.
        (if brite::highlight-done
            ;; If so, remove & redo highlighting.
            (progn
              (brite::clear-zones)
              (brite::set-search-whole-buffer)
              (brite::internal-highlight))
          ;; Else, do highlighting for the first time.
          (progn
            (lightbrite-mode 1)))))
  ;; Have to return nil or write-file-hooks will get stuffed up.
  nil)

;;; ---------------------- brite::do-highlight-explicit ----------------------

(defvar brite::num-zones-begin nil "Number of zones before highlighting.")
(defvar brite::num-zones-end nil "Number of zones after highlighting.")

(defun brite::num-zones (&optional buffer)
  "Count the number of zones in the current buffer; optional BUFFER arg for
alternate buffer."
  (length (brite::zone-list buffer)))

(defun brite::do-highlight-explicit (&optional pointer-action)
  "Called interactively; does highlighting as appropriate.  Optional arg
POINTER-ACTION, when set to 'static-pointer, prevents pointer munging
from taking place."
  (interactive)
  (if lightbrite-mode
      (progn
        (if brite::diagnostics-on
            (setq brite::num-zones-begin (brite::num-zones)))
        ;; Check to see if there is any highlighting currently in effect.
        (if brite::highlight-done
            ;; If so, remove & redo highlighting.
            (progn
              (brite::clear-zones)
              (brite::set-search-whole-buffer)
              (brite::internal-highlight pointer-action))
          ;; Else, do highlighting for the first time.
          (progn
            (decode-syntax-table)
            (brite::set-search-whole-buffer)
            (brite::internal-highlight pointer-action)
            (setq brite::highlight-done t)))
        (if brite::diagnostics-on
            (setq brite::num-zones-end (brite::num-zones)))
        (if brite::diagnostics-on
            (message "Highlighting done; made %d zones." 
                     (- brite::num-zones-end brite::num-zones-begin))))
    ;; If we weren't in lightbrite-mode before, go to it now.
    (lightbrite-mode 1)))
    
;;; ----------------------- brite::rmail-show-message ------------------------

(defun brite::rmail-show-message ()
  "Called from rmail-show-message hook, and ensures that rmail buffers
will always be highlighted."
  ;; Save current value of touchup max size and temporarily balloon it
  ;; to nil.  This is safe since RMAIL only sets the buffer extents to
  ;; that part of the file that holds the current message, so
  ;; (point-min) is not necessarily 0 and (point-max) is not
  ;; necessarily (buffer-size); thus, we can highlight the whole
  ;; buffer for each message without incurring huge performance
  ;; penalties.
  (if lightbrite-mode
      ;; If lightbrite-mode already active, just do this message.
      (progn
        (let ((brite::touchup-threshold-temp brite::touchup-threshold))
          (setq brite::touchup-threshold nil)
          (brite::do-highlight-explicit 'static-pointer)
          (setq brite::touchup-threshold brite::touchup-threshold-temp)))
    ;; Otherwise, do lightbrite-mode and then then this message.
   (progn
     (let ((brite::touchup-threshold-temp brite::touchup-threshold))
       (setq brite::touchup-threshold nil)
       (lightbrite-mode 1)
       (setq brite::touchup-threshold brite::touchup-threshold-temp)))))
    
;;; ---------------------- brite::do-highlight-function ----------------------

(defun brite::do-highlight-function ()
  "Called interactively; highlights current defun only."
  (interactive)
  (message "Highlighting function.")
  ;; Check to see if there is any highlighting currently in effect.
  (if brite::highlight-done
      ;; If so, remove & redo highlighting.
      (progn
        ;; We have brite::set-search-this-function to set function
        ;; extents in globals, then clear that region.
        (brite::set-search-this-function)
        (brite::clear-zones-in-region brite::beginning-point 
                                      brite::ending-point)
        (brite::internal-highlight 'static-pointer))
    ;; Else, do highlighting for the first time.
    (progn
      (lightbrite-mode 1))))

;;; --------------------- brite::do-highlight-paragraph ----------------------

(defun brite::do-highlight-paragraph ()
  "Called interactively; highlights current paragraph only."
  (interactive)
  (message "Highlighting paragraph.")
  ;; Check to see if there is any highlighting currently in effect.
  (if brite::highlight-done
      ;; If so, remove & redo highlighting.
      (progn
        ;; We have brite::set-search-this-paragraph to set paragraph
        ;; extents in globals, then clear that region.
        (brite::set-search-this-paragraph)
        (brite::clear-zones-in-region brite::beginning-point 
                                      brite::ending-point)
        (brite::internal-highlight 'static-pointer))
    ;; Else, do highlighting for the first time.
    (progn
      (lightbrite-mode 1))))

;;; ------------------- brite::do-highlight-visible-window -------------------

(defun brite::do-highlight-visible-window ()
  "Called interactively; highlights visible window only."
  (interactive)
  ;; Check to see if there is any highlighting currently in effect.
  (if brite::highlight-done
    ;; If so, remove & redo highlighting.
    (progn
      ;; We have brite::set-search-visible-window to set window
      ;; extents in globals, then clear that region.
      (brite::set-search-visible-window)
      (brite::clear-zones-in-region brite::beginning-point 
                                    brite::ending-point)
      (brite::internal-highlight 'static-pointer))
    ;; Else, do highlighting for the first time.
    (progn
      (lightbrite-mode 1))))

;;; ----------------------- brite::highlight-on-change -----------------------

(defun brite::highlight-on-change (pos inspos dellen)
  "Highlights current line only.  Accepts arguments as per 
after-change-function.  Don't do anything if DELLEN nonzero.  Assume
highlighting has already been done here to speed things up.  Only do
this every brite::change-interval changes."
  ;; First make sure this isn't a deletion.
  (if (= dellen 0)
      (progn
        ;; Increment the change counter.
        (setq brite::change-counter (+ brite::change-counter 1))
        ;; Do line highlight if this is a good time.
        (if (>= brite::change-counter brite::change-interval)
            ;; Protect everything to keep this from messing up replaces.
            (save-excursion
              (let ((data (match-data)))
                (unwind-protect
                    (progn
                      (setq brite::change-counter 0)
                      ;; To search on the current line, use this:
                      ;; (brite::set-search-this-line)
                      ;; To search in the immediate area, do this:
                      ;; (brite::set-search-this-area)
                      ;; To search in the whole window, do this:
                      (brite::set-search-visible-window)
                      (brite::clear-zones-in-region brite::beginning-point 
                                                    brite::ending-point)
                      (brite::internal-highlight 'static-pointer))
                  (store-match-data data))))))))

;;; -------------------------------------------------------------------------- 
;;; ------------------- INTERACTIVE STATUS CHANGE ROUTINES -------------------
;;; --------------------------------------------------------------------------

(defun brite::toggle-comment-highlight-on ()
  "Toggle buffer-local comment highlighting."
  (interactive)
  (setq brite::comment-highlight-on (not brite::comment-highlight-on))
  (message "Setting comment highlighting to %s." brite::comment-highlight-on)
  (brite::do-highlight-explicit))

(defun brite::toggle-touchup-highlight-on ()
  "Toggle buffer-local touchup highlighting."
  (interactive)
  (setq brite::touchup-highlight-on (not brite::touchup-highlight-on))
  (message "Setting touchup highlighting to %s." brite::touchup-highlight-on)
  (brite::do-highlight-explicit))

(defun brite::toggle-touchup-check-string ()
  "Toggle buffer-local in-quoted-string checking."
  (interactive)
  (setq brite::touchup-check-string (not brite::touchup-check-string))
  (message "Setting touchup string-checking to %s."
           brite::touchup-check-string)
  (brite::do-highlight-explicit))

(defun brite::toggle-touchup-check-zone ()
  "Toggle buffer-local in-zone checking."
  (interactive)
  (setq brite::touchup-check-zone (not brite::touchup-check-zone))
  (message "Setting touchup zone-checking to %s."
           brite::touchup-check-zone)
  (brite::do-highlight-explicit))

(defun brite::toggle-highlight-on-write ()
  "Toggle buffer-local highlighting on write."
  (interactive)
  (setq brite::highlight-on-write (not brite::highlight-on-write))
  (message "Setting highlight on write to %s." brite::highlight-on-write))

(defun brite::balloon-comment-threshold ()
  "Balloon the maximum file size for comment highlighting."
  (interactive)
  (setq brite::comment-threshold (+ (buffer-size) 1000))
  (message "Ballooning comment highlighting max size to %d."
           brite::comment-threshold)
  (brite::do-highlight-explicit))

(defun brite::balloon-touchup-threshold ()
  "Balloon the maximum file size for touchup highlighting."
  (interactive)
  (setq brite::touchup-threshold (+ (buffer-size) 1000))
  (message "Ballooning touchup highlighting max size to %d."
           brite::touchup-threshold)
  (brite::do-highlight-explicit)
  (if brite::diagnostics-on
      (message "%d zones in buffer." (length (brite::zone-list)))))

(defun brite::toggle-instant-highlight-on ()
  "Toggle value of brite::instant-highlight-on."
  (interactive)
  (if (boundp 'after-change-function)
      (progn
        (setq brite::instant-highlight-on (not brite::instant-highlight-on))
        (if brite::instant-highlight-on
            (progn
              (make-variable-buffer-local 'after-change-function)
              (setq after-change-function 'brite::highlight-on-change)
              (message "Instant highlighting is active."))
          (progn
            (setq after-change-function nil)
            (message "Instant highlighting in inactive."))))))

;;; --------------------------------------------------------------------------
;;; ---------------------- brite::postpend-unique-hook -----------------------
;;; --------------------------------------------------------------------------

;; Author: Daniel LaLiberte (liberte@cs.uiuc.edu).

(defun brite::postpend-unique-hook (hook-var hook-function)
  "Postpend HOOK-VAR with HOOK-FUNCTION, if it is not already an element.
hook-var's value may be a single function or a list of functions."
  (if (boundp hook-var)
      (let ((value (symbol-value hook-var)))
        (if (and (listp value) (not (eq (car value) 'lambda)))
            (and (not (memq hook-function value))
                 (set hook-var (append value (list hook-function))))
          (and (not (eq hook-function value))
               (set hook-var (append value (list hook-function))))))
    (set hook-var (list hook-function))))

;;; --------------------------------------------------------------------------
;;; ------------------------------ BUG REPORTS -------------------------------
;;; --------------------------------------------------------------------------

;; This section is inspired by c++-mode (Barry Warsaw).

(defvar brite::mailer 'mail
  "*Mail package to use to generate bug report mail buffer.")
(defconst brite::help-address "lightbrite-bugs@wintermute.ncsa.uiuc.edu"
  "Address accepting submission of bug reports.")

(defun brite::dump-state ()
  (let ((buffer (current-buffer)))
    (set-buffer buffer)
    (insert (brite::epoch-version) "\n")
    (insert (emacs-version) "\n")
    (insert "Lightbrite " brite::version "\n")))

(defun brite::submit-bug-report ()
  "Submit via mail a bug report using the mailer in brite::mailer."
  (interactive)
  (funcall brite::mailer)
  (insert brite::help-address)
  (if (re-search-forward "^subject:[ \t]+" (point-max) 'move)
      (insert "Bug in Lightbrite " brite::version))
  (if (not (re-search-forward mail-header-separator (point-max) 'move))
      (progn (goto-char (point-max))
             (insert "\n" mail-header-separator "\n")
             (goto-char (point-max)))
    (forward-line 1))
  (set-mark (point))
  (insert "\n\n")
  (brite::dump-state)
  (exchange-point-and-mark))

;;; --------------------------------------------------------------------------
;;; --------------------------- LIGHTBRITE KEYMAP ----------------------------
;;; --------------------------------------------------------------------------

(defun brite::setup-keys ()
  "Set up Lightbrite's local keymap."

  ;; C-z C-SPC does buffer highlight.
  ;; C-z SPC does window highlighting.
  (local-set-key "\C-z\C-@" 'brite::do-highlight-explicit)
  (local-set-key "\C-z " 'brite::do-highlight-visible-window)
  
  ;; C-z c and C-z t toggle comment/touchup highlighting
  ;; for this buffer.
  (local-set-key "\C-zc" 'brite::toggle-comment-highlight-on)
  (local-set-key "\C-zt" 'brite::toggle-touchup-highlight-on)
  
  ;; C-z C and C-z T balloon the comment/touchup threshold for
  ;; all buffers.
  (local-set-key "\C-zC" 'brite::balloon-comment-threshold)
  (local-set-key "\C-zT" 'brite::balloon-touchup-threshold)
  
  ;; C-z q/z/w toggle misc parameters.
  (local-set-key "\C-zq" 'brite::toggle-touchup-check-string)
  (local-set-key "\C-zz" 'brite::toggle-touchup-check-zone)
  (local-set-key "\C-zw" 'brite::toggle-highlight-on-write)
  
  ;; C-z a toggles instant highlighting.
  (if (boundp 'after-change-function)
      (local-set-key "\C-za" 'brite::toggle-instant-highlight-on)))

;; Variable to track state of local keymap.
(defvar brite::old-local-keymap nil "Storage for old local keymap.")
(make-variable-buffer-local 'brite::old-local-keymap)
(setq-default brite::old-local-keymap nil)

(defun brite::turn-on-keymap ()
  "Active local Lightbrite keymap."
  (setq brite::old-local-keymap (current-local-map))
  (if (null brite::old-local-keymap)
      (setq brite::old-local-keymap (make-keymap)))
  (use-local-map (copy-keymap brite::old-local-keymap))
  (brite::setup-keys))

(defun brite::turn-off-keymap ()
  "Deactivate local Lightbrite keymap."
  (use-local-map brite::old-local-keymap)
  (setq brite::old-local-keymap nil))

;;; --------------------------------------------------------------------------
;;; ---------------------------- LIGHTBRITE-MODE -----------------------------
;;; --------------------------------------------------------------------------

;; Variable to track state of lightbrite-mode in each buffer.
(defvar lightbrite-mode nil "Flag for activation of lightbrite-mode.")
(make-variable-buffer-local 'lightbrite-mode)
(setq-default lightbrite-mode nil)

(defun lightbrite-mode (&optional arg)
  "Lightbrite is a minor mode for syntax-directed highlighting of
source code under Epoch 4.2 (or later) or Lucid Emacs 19.3 (or later).
Optional ARG turns lightbrite-mode on if and only if value is greater
than 0, else off."
  (interactive)
  (setq lightbrite-mode
        (if (null arg) (not lightbrite-mode)
          (> (prefix-numeric-value arg) 0)))
  (cond
   (lightbrite-mode
    ;; Avoid turning on keymap twice.
    (if (null brite::old-local-keymap)
        (brite::turn-on-keymap))
    ;; Do initial highlighting.
    (brite::do-highlight-explicit)
    ;; Run startup hooks.
    (run-hooks 'brite::mode-hook))
   (t ;; not lightbrite-mode
    (brite::turn-off-keymap)
    (brite::clear-zones)
    (run-hooks 'brite::mode-exit-hook))))

;;; --------------------------------------------------------------------------
;;; ------------------------- RUNTIME INITIALIZATION -------------------------
;;; --------------------------------------------------------------------------

;; Register lightbrite-mode as a minor mode.
(or (assq 'lightbrite-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(lightbrite-mode " Light") minor-mode-alist)))

;; Do the hooks.
(brite::postpend-unique-hook 'find-file-hooks 'brite::do-highlight-on-find)
(brite::postpend-unique-hook 'write-file-hooks 'brite::do-highlight-on-write)
(brite::postpend-unique-hook 'rmail-show-message-hook 
                             'brite::rmail-show-message)
(brite::postpend-unique-hook 'mail-setup-hook 'brite::do-highlight-on-mail)

;; The following hooks are not installed by default, but could be.
;; (brite::postpend-unique-hook 'gnus-Article-prepare-hook 
;;                              'brite::do-highlight-explicit)
;; (brite::postpend-unique-hook 'gnus-Subject-prepare-hook 
;;                              'brite::do-highlight-explicit)

;;; -------------------------------- Menubar ---------------------------------

(if brite::running-lemacs
    (progn
      (defvar brite::menu
        '("Lightbrite Commands"
          ["Highlight Window"          brite::do-highlight-visible-window t]
          ["Highlight Function"        brite::do-highlight-function       t]
          ["Highlight Paragraph"       brite::do-highlight-paragraph      t]
          ["Highlight Buffer"          brite::do-highlight-explicit       t]
          "----"
          ["Balloon Touchup Threshold" brite::balloon-touchup-threshold   t]
          ["Balloon Comment Threshold" brite::balloon-comment-threshold   t]
          "----"
          ["Toggle String-Checking"    brite::toggle-touchup-check-string t]
          ["Toggle Extent-Checking"    brite::toggle-touchup-check-zone   t]
          ["Toggle Highlight-On-Write" brite::toggle-highlight-on-write   t]
          "----"
          ["Toggle Touchup Highlights" brite::toggle-touchup-highlight-on t]
          ["Toggle Comment Highlights" brite::toggle-comment-highlight-on t]
          ["Toggle Instant Highlights" brite::toggle-instant-highlight-on t]
          "----"
          ["Submit Bug Report"         brite::submit-bug-report           t]
          ))

      (defun brite::install-menubar ()
        (if current-menubar
            (progn
              (set-buffer-menubar (copy-sequence current-menubar))
              (add-menu nil "Light" (cdr brite::menu)))))

      (defun brite::remove-menubar ()
        (if current-menubar
            (delete-menu-item '("Light"))))
                        
      (brite::postpend-unique-hook 'brite::mode-hook 'brite::install-menubar)
      (brite::postpend-unique-hook 'brite::mode-exit-hook
                                   'brite::remove-menubar)))

;;; -------------------------- provide the package ---------------------------

(provide 'lightbrite)

;;; ------------------------------ END OF CODE -------------------------------
