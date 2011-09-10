;;; eev.el -- embed shell/tcl/perl code and elisp hyperlinks in plain text.
;;
;; This file was copylefted to prevent against patent psychopaths; if
;; you want a version with any other license you'll have to write it
;; yourself. More formally,
;;
;; Copyright (C) 1999 Eduardo Ochs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;;
;; Author:       Eduardo Ochs <edrx@mat.puc-rio.br>
;; Last updated: 1999dec12
;;
;; Look in <http://angg.twu.net/emacs.html> for more information and
;; in <http://angg.twu.net/escripts.html> for examples of e-scripts...
;; an e-script is typically a text file in which certain parts are
;; meant to be executed by the shell and certain comments are in fact
;; Emacs Lisp code; executing one of these comments with C-x C-e
;; (i.e., eval-last-sexp) brings up a file or an info node, and thus
;; works as a hyperlink.
;;
;; Many comments in this file can be executed in this way.
;;
;; More info below...



;;;;;
;;;;; ®find-¯
;;;;; Basic "find-..." functions
;;;;;
;
(defun my-jump-to (param)
  (if param (goto-line 0))
  (if (numberp param)
      (progn (goto-char (point-min))
	     (forward-line (1- param)))) ; instead of (goto-line line)
  (if (stringp param)
      (search-forward param)))

(autoload 'Info-goto-node "info")

(defun find-node (nodestr &optional line)
  (if (Info-goto-node nodestr)
      (if line (my-jump-to line))))

(defun find-fline (fname &optional line)
  (find-file (substitute-in-file-name fname))
  (my-jump-to line))



;;;;;
;;;;; ®eev_pre¯
;;;;; The "ee" family. Basic building blocks,
;;;;;
;
; (find-elnode "Writing to Files" "If START is a string")
;
(defun se-to-string (s e)
  (cond ((numberp s) (buffer-substring s e))
	((stringp s) s)))
;
(defun octal-to-num (str)
  (let ((lastv (- (string-to-char (substring str -1)) ?0))
	(rest (substring str 0 -1)))
    (if (string= "" rest) lastv (+ lastv (* 8 (octal-to-num rest))))))
;
(setq ee-file "$EE")
;
(defun write-ee-string (str &optional other-ee-file fmode)
  (let ((fname (substitute-in-file-name (or other-ee-file ee-file))))
    (write-region str nil fname)	; a standard kludge
    (if fmode (set-file-modes fname (octal-to-num fmode)))))
;
(defun write-ee (s e pre post &optional other-ee-file fmode)
  (write-ee-string (concat pre (se-to-string s e) post) other-ee-file fmode))


;;;;;
;;;;; ®eev¯
;;;;; eev and friends.
;;;;;
;
; "C-x eev" writes the "region" of the current buffer (i.e.,
; everything between the cursor and the "mark") to a file, so that it
; can be executed as a script. In fact it does better than that: the
; region is saved surrounded by "set -v" and "set +v" and we source it
; instead of running it as a script, to let it use and change the
; environment, the directory and the pool of aliases and functions of
; the current shell. Marking a block of text in emacs, typing "C-x
; evv" and then going to a shell and running "ee" (which should be
; aliased to `. $EE', where EE is usually set to ~/bin/emacs.ee, is
; almost 100% equivalent to typing each line to the shell.
;
; ®eev-howto¯
; That was too theoretical. Here's the "quick eev how-to": put a line
; like this in your .emacs:
;
;   (load-library "~/eev.el")
;
; and put these two lines in your .zshrc:
;
;   export EE=~/bin/emacs.ee
;   alias ee='. $EE'
;
; Restart zsh if needed, and enter Emacs. Write some shell commands;
; mark a block of text and run C-x eev. You should get a message like
; "wrote ~/bin/emacs.ee" in the echo area at the bottom of the screen.
; Switch to a zsh (e.g., in another virtual console or in another
; window) and run "ee". Voila`, your commands will be executed, and
; each of them will be displayed (without pause) before being
; executed.
;
; ®eev-bash¯
; If you use bash instead of zsh you'll have to put the "export/alias"
; lines in your .bashrc instead; all the rest is the same, except that
; the last line ("set +v") won't be overwritten by the next shell
; prompt and will remain on the screen. (Ugly. I need to fix this.)
;
; ®eev-tcsh¯
; If you use tcsh then things are a bit worse. This is what you should
; put in your .cshrc:
;
;   setenv EE ~/bin/emacs.ee
;   alias ee 'source $EE'
;
; and as the commands to put the shell in verbose mode are different
; in tcsh you should either use eevt instead of eev or, better, make
; the definition of eevt overwrite the definition of eev, by dropping
; the "t" in eevt's defun or by loading eev.el like this:
;
;   (load-library "~/eev.el")
;   (defun eev (s e &optional other-ee-file) (interactive "r")
;     (write-ee s e "set verbose\n" "\nunset verbose" other-ee-file))
;
; See:
; (find-node "(zsh)Shell Builtin Commands" "`source'")
; (find-node "(zsh)Shell Builtin Commands" "`set ")
; (find-node "(zsh)Shell Builtin Commands" "`.")
; (find-node "(zsh)Description of Options" "-v")
; (find-node "(bash)Bourne Shell Builtins" "`.'")
; (find-node "(bash)The Set Builtin" "  `-v'")
; (find-elnode "Markers")

(defun eev (s e &optional other-ee-file) (interactive "r")
  (write-ee s e "set -v\n" "\nset +v" other-ee-file))             ; zsh/bash
(defun eevt (s e &optional other-ee-file) (interactive "r")
  (write-ee s e "set verbose\n" "\nunset verbose" other-ee-file)) ; tcsh

; eev to some remote machines, using ange-ftp:
;
(defun eevb (s e) (interactive "r")
  (eev s e "/edrx@$SACI:bin/emacs.ee"))
(defun eevp (s e) (interactive "r")
  (eev s e "/patna:/home/root/bin/emacs.ee"))
(defun eevi (s e) (interactive "r")
  (eev s e "/ifigenia:/home/root/bin/emacs.ee"))

; eex: uses "-x" instead of "-v"
; ees: "silent", i.e., non-verbose
;
(defun eex (s e) (interactive "r")
  (write-ee s e "set -x\n" "\nset +x"))
(defun ees (s e) (interactive "r")
  (write-ee s e "" "\n"))

; eecd: change to the directory (try "M-x pwd"...)
;
(defun eecd () (interactive)
  (eev (concat "cd " default-directory) nil))

'(defun eel (s e) (interactive "r")
  (write-ee s e "lynx " "\n"))
(defun eeg (s e) (interactive "r")
  (write-ee s e "" "" "~/GDB/emacs.eeg"))

; perl, tcl/tk, expect
;
(defun eep (s e) (interactive "r")
  (write-ee s e "#!/usr/bin/perl\n" "" "~/bin/eep" "0777"))
(defun eew (s e) (interactive "r")
  (write-ee s e "#!/usr/bin/wish\n" "" "$EET" "0777"))
(defun eet (s e) (interactive "r")
  (write-ee s e "#!/usr/bin/tclsh\n" "" "$EET" "0777"))
(defun eex (s e) (interactive "r")
  (write-ee s e "#!/usr/bin/expect\n" "" "$EET" "0777"))
(defun eextk (s e) (interactive "r")
  (write-ee s e "#!/usr/bin/expectk\n" "" "$EET" "0777"))

; (find-es "page" "latte")
'(defun eel (s e) (interactive "r")
  (write-ee s e "latte-html -f - <<'--%%--'
                 {\\include /home/root/latte/sty.latte}\n"
	    "\n--%%--\n"))

; eed, for dosemu. Aaargh!!
;
(defun split-string (s)
  (if (string-match "\\([^\t\n ]+\\)[\t\n ]*\\(\\)" s)
      (cons (match-string 1 s)
	    (split-string (substring s (match-beginning 2))))))
(defun crlfify (s)
  (if (string-match "\\([^\n]*\\)\n\\(\\)" s)
      (concat (match-string 1 s) "\r\n"
	      (crlfify (substring s (match-beginning 2))))
    s))
(defun eed (s e) (interactive "r")
  (write-region (crlfify (se-to-string s e)) nil "/D/ee.bat"))


;;;;;
;;;;; ®eeman¯
;;;;; eeman: linking to specific points on manpages
;;;;;
;
; This function is a hybrid between the find-xxx's and the code-c-d's;
; it works as a link to a specific point on a manpage, but you need to
; go to a shell and type "ee" to let it call man.
;
; Examples of usage:
;
; (eeman "man"  "  -P")
; (eeman "less" "  \\+cmd")
; (eeman "expect" 972)
;
(defun eeman (str &optional line)
  (cond ((numberp line)
	 (write-ee-string (format "man -P 'less +%d' %s\n" line str)))
	((stringp line)
	 (write-ee-string (format "man -P \"less '+/%s'\" %s\n" line str)))
	(t
	 (write-ee-string (format "man %s\n" str)))))


;;;;;
;;;;; ®eelatex¯
;;;;; Things to run LaTeX on a block of text.
;;;;;
;
; Run this on a block of text and then do a "ee" on a shell; a file
; ~/LATEX/tmp.tex will be written and "make" will be called to
; generate a tmp.dvi from it.
;
; (find-fline "~/LATEX/tese2.sty")
;
(defun eelatex (s e) (interactive "r")
  (write-ee s e "cat > ~/LATEX/tmp.tex <<'--%%--'
                 \\input tese2.sty
                 \\begin{document}
                 % \\input mar99a.dnt
                 % \\input mar99b.dnt\n"
	    "\n\\end{document}\n--%%--
             cd ~/LATEX
             rm tmp.dvi
             make tmp.dvi\n"))

;;;;;
;;;;; ®bounded¯
;;;;; "Bounded" versions for some eev commands; instead of working
;;;;; with the region these functions select the text around point
;;;;; until some fixed delimiter strings.

; Some support functions, 
; (find-elnode "String Search")
;
(defun ee-search-backward (str)
  (+ (save-excursion (search-backward str))
     (length str)))
(defun ee-search-forward (str)
  (- (save-excursion (search-forward str))
     (length str)))
(defun ee-strbounded (fun sstr estr &rest rest)
  (apply fun
	 (ee-search-backward sstr)
	 (ee-search-forward estr)
	 rest))
(defun write-ee-bounded (sstr estr &rest rest)
  (apply 'ee-strbounded 'write-ee sstr estr rest))

; and the bounded ee's themselves:
;
(defun eelatex-bounded () (interactive)
  (ee-strbounded 'eelatex "\n%\n" "\n%\n"))
(defun eegdb-bounded () (interactive)
  (ee-strbounded 'write-ee "\n%\n" "\n%\n" "" "" "~/GDB/emacs.eeg"))
(defun eepf-bounded () (interactive)
  (ee-strbounded 'write-ee "%" "%" "" "" "/usr/src/pforth/ee.fth"))

'(global-set-key [f3] 'eegdb-bounded)
'(global-set-key [f3] 'eepf-bounded)
'(global-set-key [f3] 'eelatex-bounded)


;;;;;
;;;;; ®code-c-d¯
;;;;; code-c-d, a factory of hypertext functions
;;;;;
;
; (find-efile "etags.el")
(load-library "etags")
(setq tags-add-tables nil)		; for find-___tags
;
(defun format-and-eval (formatstr &rest rest)
  (eval (read (apply 'format (concat "(progn " formatstr ")") rest))))
;
(defun find-node2 (ifile nodename &optional line  code)
  (if code (setq ee-code-now code ee-ifile-now ifile))
  (find-node (concat "(" ifile ")" nodename) line))
;
(defun code-c-d (c d &optional ifile)
  (format-and-eval "
    (setq %sdir \"%s\")
    (setq %stagsfile \"%sTAGS\")
    (defun %sfile (str) (concat %sdir str))
    (defun set-%s-tags () (setq tags-file-name %stagsfile))
    (defun find-%sfile (str &optional line) (set-%s-tags)
      (find-fline (%sfile str) line))
    (defun find-%stag (str) (set-%s-tags) (find-tag str))
    (defun find-%sw3 (furl) (w3-open-local (concat %sdir furl)))
    (setq ee-code-now %S ee-ifile-now %S)
    " c d  c d  c c  c c  c c c  c c  c c  c ifile)
  (if ifile
     (format-and-eval "
       (defun find-%snode (nodename &optional line)
          (find-node2 %S nodename line  %S))" c ifile  c)))

; To understand exactly what a given "code-c-d" does, run manually the
; commented s-expression below and then the code-c-d; what would
; normally be eval'ed by the code-c-d will be inserted. For example,
; (code-c-d "awk" "/usr/src/gawk-3.0.3/" "gawk") gives
;
; (progn
;   (setq awkdir "/usr/src/gawk-3.0.3/")
;   (setq awktagsfile "/usr/src/gawk-3.0.3/TAGS")
;   (defun awkfile (str) (concat awkdir str))
;   (defun set-awk-tags () (setq tags-file-name awktagsfile))
;   (defun find-awkfile (str &optional line) (set-awk-tags)
;     (find-fline (awkfile str) line))
;   (defun find-awktag (str) (set-awk-tags) (find-tag str))
;   (defun find-awkw3 (furl) (w3-open-local (concat awkdir furl))))
;   (setq ee-code-now "awk" ee-ifile-now "awk")
; 
; (progn
;   (defun find-awknode (nodename &optional line)
;     (find-node2 "gawk" nodename line)))
;
; where the second progn block would not have been generated if the
; third argument to code-c-d (the name of an info file) had been
; suppressed.
;
; That is, the first argument to a code-c-d is the "prefix", the
; second if the "path", the third is the "info file". A call to
; code-c-d with a prefix of "xxx" defines xxxdir, xxxtagsfile,
; xxxfile, set-xxx-tags, find-xxxfile, find-xxxtag, find-xxxw3 and
; maybe find-xxxnode; there are lots of examples of usage of these
; functions in this file and in my e-scripts directory, which is at
; <http://www.mat.puc-rio.br/~edrx/e/>.

; Here is the hacked version of format-and-eval that I mentioned
; above. Note that the original definition of format-and-eval will be
; overwritten if you evaluate this one and then you will need to
; reevaluate the original definition.

'(defun format-and-eval (formatstr &rest rest)
  (insert (apply 'format (concat "(progn " formatstr ")") rest)))

; ®ccd-exs¯
; Some sample code-c-d's.
; A typical .emacs (e.g., mine:-) has over 150 calls to code-c-d:
; (fine-fline "~/.emacs")

(code-c-d "knuth" "$SCTAN/systems/knuth/")
(code-c-d "vldi" "/var/lib/dpkg/info/")
(code-c-d "k2" "/usr/src/linux-2.0/")
(code-c-d "k22" "/usr/src/linux-2.2/")
;
(code-c-d "e" "/usr/share/emacs/19.34/lisp/" "emacs-19/emacs")
(code-c-d "el" "/usr/share/emacs/19.34/lisp/" "elisp")
(code-c-d "eli" "/usr/src/emacs-lisp-intro-1.05/" "emacs-lisp-intro")



(defun find-pl5pod (radix &optional line)
  (find-pl5file (concat "pod/perl" radix ".pod") line))

(defun find-tagh (fname &optional tag)
  (if tag (find-fline fname (format "®%s¯" tag))
    (find-fline fname)))

(defun find-es (file &optional tag)
  (find-tagh (esfile (concat file ".e")) tag))
(defun find-angg (file &optional tag)
  (find-tagh (concat "~/" file) tag))

(defun find-slink (s) (deb-view (concat "/home/root/SLINK/" s)))


;;;;;
;;;;; Two extra functions for hypertext
;;;;;
;
; (find-elnode "Buffer File Name")
; (find-elnode "Interactive Examples")
; (find-efile "info.el")
; (find-etag "Info-set-mode-line")
;
(defun bfn (buffername)
  "Insert the full file name for a buffer."
  (interactive "bBuffer: ")
  (insert (buffer-file-name (get-buffer buffername))))
;
; ®inn¯
(defun inn (arg)
  "Insert an hyperlink to the current info node.  With an argument
try to use the short form, like `# (find-elnode \"Top\")'.  Without
an argument always write something like `(find-node \"(elisp)Top\")'."
  (interactive "P")
  (let (f node ee code s)
    (save-excursion
      (set-buffer "*info*")
      (string-match "info/\\(.*\\)$" Info-current-file)
      (setq f (match-string 1 Info-current-file))
      (setq node Info-current-node))
    (insert (if (and arg (string= f ee-ifile-now))
		(format "# (find-%snode \"%s\")\n" ee-code-now node)
	      (format "# (find-node \"(%s)%s\")\n" f node)))))


;;;;;
;;;;; ®dff¯
;;;;; dff - a macro to create find-xxx
;;;;; commands for debian packages
;;;;;
;
; (find-node "(elisp)Keyboard Macros")
;
(defun dff (N)
  "Converts N lines with a name of a debian package in each to N
``# (find-vldifile \"/xxx.list\")
  # (find-fline \"/usr/doc/xxx/\")''
pairs."
  (interactive "p")
  (execute-kbd-macro
   (read-kbd-macro "C-a # SPC (find-vldifile SPC \" NUL C-e ESC w .list\")
		    C-a <down> RET <up> # SPC (find-fline SPC \"/usr/doc/
		    C-y /\") C-a <down>")
   N))
