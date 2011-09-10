;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!sgtp.apple.juice.or.jp!shin Mon Feb 26 13:09:28 1990
;Article 1162 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!sgtp.apple.juice.or.jp!shin
;>From shin@sgtp.apple.juice.or.jp (Shinichirou Sugou)
;Newsgroups: gnu.emacs
;Subject: lispref.el
;Message-ID: <9002171123.AA01239@sgtp.apple.juice.or.jp>
;Date: 17 Feb 90 11:23:12 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 176
;
;[Having posted this article ago seems to fail... Try one more]
;
;If you are EmacsLisp programmer, you are tired of examining the function
;definition using the hard manual or Emacs info.
;
;lispref.el makes your life easier.  Enjoy.

;; lispref.el -- On line quick reference tool for lispref info manual.
;; Copyright (C) Shinichirou Sugou, Feb 1990

;; Any comment, bug report are welcome.
;;
;;   shin%sgtp.apple.juice.or.jp@uunet.uu.net

;; This file is not yet (and will not be forever) part of GNU Emacs.

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

;; SUMMARY
;;
;; lispref searches {function,command,variable...} you specified in lispref
;; info manual.  lispref searches twice, strictly and non-strictly.  For example,
;; if you specify "window", lispref searches the regexp pattern "\bwindow\b",
;; then the regexp pattern "\(\S +window\)\|(window\S +\).  Also, you can
;; use regexp.  For example, if you want to search something which includes
;; "undef" or "unset", specify "\(undef\|unset\)".  Some special regexp characters,
;; such as "^" or "$", cannot be used (if you use them, search merely fails).
;; See strict-pat/non-strict-pat in the program why these regexp isn't desirable.

;; INSTALLATION
;;
;; When you use lispref, you must have installed lispref.texinfo into Emacs
;; info directory.  You will find lispref.texinfo file in the latest GNU product.
;; Read info in Emacs info about the way how to install lispref.texinfo
;; into info directory.

;; USAGE
;;
;; Binding the command lispref-search to some key is a good idea.
;; (define-key global-map "\C-^i" 'lispref-search) will bind this function to
;; CTRL-^i though you can choose any key you want.
;; When lispref-search is invoked, it looks for the appropriate symbol around
;; the cursor for the default.  If you simply type <RET>, default symbol will
;; be searched.  Using Meta-^i(lisp-complete-symbol) makes life easier for typing
;; symbol.  If no appropriate symbol is found (or if the cursor is within info buffer),
;; typing <RET> will search the last symbol.


(require 'info)
(require 'cl)
(defconst lispref-search-strictly t "Control variable.")
(defconst lispref-search-last-symbol "" "")
(defun lispref-empty-string-p (str)
  (string= str ""))
(defun lispref-looking-back (str)
  "Return t if looking back reg-exp STR before point."
  (and (save-excursion (re-search-backward str nil t))
       (= (point) (match-end 0))))
(defun lispref-symbol-around-point ()
  "Return lisp symbol around the point, or nil if can't find."
  (condition-case stub
      (save-excursion
        (cond ((looking-at "\(")
               (skip-chars-forward "\(")
               (set-mark (point))
               (forward-sexp 1))
              ((looking-at "\\sw\\|\\s_")
               (forward-sexp 1)
               (set-mark (point))
               (backward-sexp 1))
              ((lispref-looking-back "\\(\\sw\\|\\s_\\)\\s *")
               (backward-sexp 1)
               (set-mark (point))
               (forward-sexp 1))
              ((looking-at "\\s +\(")
               (skip-chars-forward "\( \t")
               (set-mark (point))
               (forward-sexp 1))
              ((lispref-looking-back "\\(\\sw\\|\\s_\\)\)+\\s *")
               (skip-chars-backward "\) \t")
               (set-mark (point))
               (backward-sexp 1))
              (t
               (error "")))
        (car (read-from-string (buffer-substring (point) (mark)))))
    (error nil)))
(defun lispref-search (symbol)
  "Symbol is regexps.  Search lispref manual, display text in other-window."
  (interactive
   (let ((default nil)
         (prompt "Search symbol: ")
         input)
     (if (not (string= (buffer-name) "*info*"))
         (setq default (lispref-symbol-around-point)))
     (if default
         (setq prompt (concat prompt (format "(default %s) " default))))
     (setq input (read-string prompt))
     (list (or (and (not (lispref-empty-string-p input)) input)
               (and default (format "%s" default))
               ""))))

  ;; if symbol is empty string, it means that use last used symbol
  (catch 'bye
    (let ((old-buf (current-buffer))
          (from-beg nil)
          strict-pat non-strict-pat)
      (if (lispref-empty-string-p symbol)
          (setq symbol lispref-search-last-symbol)
        (setq lispref-search-last-symbol symbol
              lispref-search-strictly t
              from-beg t))
      (if (lispref-empty-string-p symbol)
          (error "No previous symbol defined"))
      (let ((pop-up-windows t))
        (pop-to-buffer "*info*"))
      (if (not (eq major-mode 'Info-mode))
          (Info-directory))
      (if from-beg
          (progn (Info-goto-node "(dir)")
                 (Info-menu "emacslisp")))
      ;; search lispref manual twice, strictly and non-strictly
      (setq strict-pat (concat (format "^\\* \\(%s\\|%s\\|%s\\|%s\\|%s\\): "
                                       "command" "function" "variable" "special form"
                                       "user option")
                               (format "\\(%s\\)\\($\\|\\s \\)"
                                       symbol)))
      (setq non-strict-pat (concat (format "^\\* \\(%s\\|%s\\|%s\\|%s\\|%s\\): "
                                           "command" "function" "variable" "special form"
                                           "user option")
                                   (format "\\(%s\\|%s\\)"
                                           (format "\\(\\(%s\\)\\S +\\)" symbol)
                                           (format "\\(\\S +\\(%s\\)\\)" symbol))))
      (setq pat (if lispref-search-strictly
                    strict-pat
                  non-strict-pat))
      (loop
       (condition-case stub
           (throw 'bye (progn (Info-search pat)
                              ;; found.  move point to the beginning of symbol
                              (beginning-of-line)
                              (re-search-forward pat)
                              (goto-char (match-beginning 2))))
         (search-failed ()))
       (if (not lispref-search-strictly) ; second trial failed
           (throw 'bye (progn
                         (pop-to-buffer old-buf) ; return back to old buffer
                         (error "%s not found" symbol))))
       (setq lispref-search-strictly nil
             pat non-strict-pat)))))





;CAUTION: (1) Reply-command in the mail system may NOT generate my address
;             correctly.  Please use the following address instead.
;
;               shin%sgtp.apple.juice.or.jp@uunet.uu.net
;
;         (2) I have no relation to Apple Computer Inc. :-)
;
;---
;S.Sugou
