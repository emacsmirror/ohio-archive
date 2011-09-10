;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                       An interface to the astro/math/physics preprint
;; Atavachron.el -\|/-   archives from within emacs giving the user 
;;                       commands to get, store, and process papers get
;;                       figures with ease.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author          : Jim Hetrick
;; Created On      : Fri Jan 22 21:52:41 1993
;; Last Modified By: Jim Hetrick
;; Last Modified On: Thu Jan 28 21:23:05 1993
;; Update Count    : 7
;; Status          : Unknown, Use with caution!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  --- RCS ---
;; $Id: atavachron.el,v 1.2 1993/01/29 10:54:08 hetrick Exp $
;; $Log: atavachron.el,v $
;; Revision 1.2  1993/01/29  10:54:08  hetrick
;; Fixed spurious "X" in atv-Xinit-loc-dirs
;; shell-paper: append "/" to atv-shell-dir if needed
;;
;; Revision 1.1.1.1  1993/01/22  23:48:51  hetrick
;; shell-paper: asks for directory (default: atv-download-dir)
;; slice-and-dice: (atv-init-loc-dirs) + \\(.ps \\| .eps\\)
;;    bug: doesn't find right name for PS files < 20 lines
;;
;; Revision 1.1  1993/01/13  21:50:46  hetrick
;; Initial revision
;;
;; $Locker:  $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LCD Archive Entry:
;; Atavachron|James E. Hetrick|hetrick@phys.uva.nl|
;; An interface system to the astro/math/physics preprint archives|
;; 13-1-93|$Revision: 1.2 $|~/interfaces/atavachron.tar.Z|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (C) 1992, 1993  James E. Hetrick 
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; =======              The Atavachron         -\|/-       =========
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The Atavachron is a system for interacting with the preprint
; archives from within emacs. It gives the user a simple set of commands
; which can be called from the mini-buffer (M-x) to get, store, and list
; and process papers. It can be loaded into to your .emacs file
; by including one of these lines therein:
;
;  (load-file '"atavachron.el")     or
;  (load-library '"atavachron")     if you put it in your emacs load path.
;
; You might also want to byte-compile it.
;
; The atavachron's commands are listed here and described 
; below (-Functions-):
;
;    get-paper               : get paper above point
;    get-paper-from-data     : prompt for paper-archive/number
;    store-paper             : store directly to disk
;    store-paper-from-data   : as above, from paper data
;    process-paper           : process locally stored paper
;    slice-and-dice          : remove appended PostScript files
;    shell-paper             : shell region below #!/bin/ 
;    get-listing-from-data   : get monthly title listing
;    store-listing-from-data : store listing
;    get-abstract            : get abstract
;    get-abstract-from-data  : prompt for number, get abstract
;    interesting             : append abstract to atv-interesting-file
;    atavachron-mail         : report bugs with internal variable dump
;
; Please go through the -Configuration- and -Batteries Not Included- 
; sections (see manual or below) and set things appropriately for your 
; system. Note that the documentation here is meant to be somewhat 
; pedagogical for users new to emacs.
;
; If you find bugs, please report them; this is very easy with the
; function "atavachron-mail". You can also use this function for other
; correspondence as well... :-)
;
; Thanks very much to Paul Ginsparg, Greg Kilcup, Marcus Speh,
; and Bas de Bakker for many suggestions and beta tests.
;
; _____________________________________________________________
; James E. Hetrick            Institute for Theoretical Physics
; hetrick@phys.uva.nl                    -\|/-
;
; University of Amsterdam
; Valckenierstraat 65, 1018 XE Amsterdam
; Telephone: +31 20 525 5772     Fax: +31 20 525 5788
; _____________________________________________________________
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  The Atavachron:        -Configuration-              -\|/-
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Sample .emacs: (see manual or defvars below)
; --------------------.emacs-------------------------------
;
;@example
; ====================.emacs stuff ========================
;
;(load-library '"ange-ftp")  ;see ange-ftp docs for more details
;(setq ange-ftp-generate-anonymous-password t) ; sends email address
;                                              ; for password
;(load-library '"zcat")
;(load-library '"atavachron")
;
;(setq atv-main-ftp-site "/anonymous\@@babbage.sissa.it:") ;Europeans
;;(setq atv-main-ftp-site "/anonymous\@@xxx.lanl.gov:")  ;N+S Americans
;
;(setq atv-download-dir "~/tex/preprints/")   ;default download directory 
;(setq atv-uufiles-dir "~/tex/preprints/")    ;default place for uu output
;(setq atv-prompt-filename t)              ;prompt to rename paper locally
;(setq atv-interesting-file "~/interesting.atv") ;file for interesting abs.
;(setq atv-auto-tex-paper nil)     ;run tex on paper grabbed automatically
;(setq atv-auto-tex-command 'TeX-buffer)        ;command to run on buffer
;(setq atv-no-csh nil)                     ;set to t if you don't have csh
;
; ==========================================================
;@end example
;........................................................................
;
;       =================
(defvar atv-main-ftp-site "/anonymous\@babbage\.sissa\.it:"  
;       =================
"Your main archive. Papers on hep-lat, alg-geom, etc. are
automatically gotten from their respective archives, while branches
not supported at xxx.lanl.gov, such as cond-mat, are retrieved from
babbage.sissa.it. In your .emacs file put:

 setq atv-main-ftp-site \"/anonymous\@babbage\.sissa\.it:\" for  Europeans
     or                                                    (others chose)
 setq atv-main-ftp-site \"/anonymous\@xxx\.lanl\.gov:\"  for  N+S Americans
")
;.........................................................................
; 
;       ================
(defvar atv-download-dir "~/tex/preprints/"
;       ================
"This is your local download workspace: If you have branch
directories  hep-th, hep-lat, astro-ph, etc. below atv-download-dir,
papers, when saved, will automatically be put there, sorted by
branch. For example, your atv-download-dir is set to \"~/tex/preprints/\"
and you have the subdirs \"~/tex/preprints/hep-th/\" and
\"~/tex/preprints/cond-mat/\". Then getting hep-th/91120013 will put the 
file in tex/preprints/hep-th/. Getting cond-mat/920106 will put it in 
tex/preprints/cond-mat/. If these subdirs do not exist below 
tex/preprints/, both papers would be put in tex/preprints/.

Put this directory in your TEXINPUTS environment variable also!
Then tex can get at your newly grabbed files.")
;..........................................................................
;
;       ===============
(defvar atv-uufiles-dir atv-download-dir
;       ===============
"This is where uufiles (figures) will be unpacked. It should 
usually be the same as atv-download-dir, unless you have a good
reason to put the figs elsewhere [your dvi->ps processor can't 
get at them in atv-download-dir, for instance. (in which case you
should get dvips)]. If it's the same as atv-download-dir, it will
adjust itself to the subdirectory structure following atv-download-dir.")
;..........................................................................
;
;       ===================
(defvar atv-prompt-filename t         ; prompt to rename paper locally
;       ===================           ; t = true, nil = false
"This one toggles prompting for a name of the grabbed paper or 
listing. When t, you are prompted to name the incoming archive
file which is then appended with .tex. Hitting return at the prompt will
accept the default given in parenthesis.

Example: the point is located in the abstract or listing 
of Witten's paper hep-th/9204083. If atv-prompt-filename is t, 
Doing M-x get-paper, asks:

\"Local name for paper? [.tex appended] (9204083): \"

Typing \"witten\", renames this paper as witten.tex. Simply hitting 
[return], preserves it's archive name as 9204083.tex")
;...........................................................................
;       ====================
(defvar atv-interesting-file "~/interesting\.atv"  ; file for interesting
;       ====================                      ; abstracts.
"Default file to which abstracts stored with \"interesting\" function 
are appended.")
;...........................................................................
;
;       ==================
(defvar atv-auto-tex-paper nil 
    "If t, run tex on grabbed paper automatically.")
(defvar atv-auto-tex-command 'TeX-buffer
;       ====================
"Setting \"atv-auto-tex-paper\" to t, automatically starts the tex
processor in \"atv-auto-tex-command\" on the paper once it's loaded
into a local buffer, provided no shell command signal is found. The
function pointed to here is then a hook for custom processing of
the grabbed paper.")
;...........................................................................
;
;       ==========
(defvar atv-no-csh nil 
;       ==========
"Set to t if you don't have csh. No guarantees...")
;...........................................................................
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  The Atavachron:      -Batteries not included-           -\|/-
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 
; Other files and packages needed or recommended:
;
;         ange-ftp.el  (not included)   --- [needed]
;         zcat.el   (included at end)   --- [needed for month+ old papers] 
;        .netrc    (see below)          --- [highly recommended]
;        .dvipsrc  (see below)          --- [highly recommended]
;
;.....................................................................
;
; >>>  ange-ftp.el      (available from prep.ai.mit.edu)
; 
; atavachron.el requires Andy Norman's, essential, ftp interface 
; ange-ftp.el to do the remote file teleportation. If you don't have
; this loaded in your .emacs file,...! (check locally if you have
; it, or download it from prep.ai.mit.edu)
;
;.....................................................................
;
; >>>  zcat.el          (Included in distribution)
;
; Another file needed is Graham Gough's zcat.el which uncompresses
; .Z files on the fly. This makes loading older papers which are
; compressed at the archive transparent. Since it's short it is included 
; with the Atavachron package.
;
;.....................................................................
;
; >>>  .netrc           (write your own, see example: "---.netrc---")
;
; Ange-ftp will work much smoother if you keep a .netrc file in your 
; home directory, thus bypassing the need to send the "anonymous" login
; and email address when ftping into the archive. Simply clip out the
; following and name it ~/.netrc (don't forget to take out the ;'s
; and put your email address in for you@where.you.are).
;
; ---------------------------.netrc---------------------------
;machine ftp.scri.fsu.edu login anonymous password you@where.you.are
;machine xxx.lanl.gov login anonymous password you@where.you.are
;machine babbage.sissa.it login anonymous password you@where.you.are
;machine publications.math.duke.edu login anonymous password you@@where.you.are
;machine alcom-p.cwru.edu login anonymous password you@@where.you.are
; ------------------------------------------------------------
;
; Furthermore, it must be a protected file. Run: "chmod go-rwx .netrc"
; on it once created.
;
;.....................................................................
;
; >>>  .dvipsrc        (write your own, see example: "---.dvipsrc---")
;
; Finally, if you use dvips as your .dvi->.ps processor, put your
; atv-uufiles-dir (see -Configuration-) in its path by keeping
; a ~/.dvipsrc file with the following lines:
;
; --------------------------.dvipsrc--------------------------
;S ~/tex/preprints:
;H ~/tex/preprints:
;W "reading your .dvipsrc file"
; ------------------------------------------------------------
;
; Having atv-uufiles-dir both in your dvips path and your TEXINPUTS 
; environment variable allows allows dvi and tex to access your 
; figure files. The trailing :'s above add the default paths.
;
; For more info, see your man pages on tex and dvips or your system 
; administrator, whichever is friendlier.
;
;.....................................................................
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  The Atavachron:    -Functions-                    -\|/-
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;    get-paper
;    get-paper-from-data
;
; When reading a daily abstract listing in emacs, either in one of the 
; mail readers or just as a file, placing the point (cursor) anywhere 
; in the abstract of choice and typing:  
;
;      (M-x)  get-paper    
;
; automatically opens an ftp channel to the right archive (via ange-ftp),
; transfers the paper, uncompresses it if necessary (via zcat.el), 
; and loads it into a tex-mode buffer. Once in the local buffer, the paper
; is ready for processing via the usual tex-mode facilities, C-c C-b for 
; (la)texing, C-c C-p for printing, or any other local tex or auc-tex 
; features, such as previewing.
;
; If the Atavachron finds the PostScript printer signal %! it will try 
; to extricate the tex file and the appended PostScript figures,
; by slicing them off into appropriate files. Of course, it saves the
; whole file first as "localname.tpl", before hacking away under 
; prompting. [see * Slice-and-dice]
;
; If it finds shell commands beginning with ^#!.*/bin/ it will shell out 
; the region from there to the end, producing a report "localname.rpt" 
; buffer of any shell output. This is handy to see what was just unpacked 
; in your download directory. If it finds a .tex file in this report, 
; it is loaded in tex-mode.
;
; Similarly atavachron will try to unpack a .tar.Z file if the archived
; preprint turns out to be in this form.
;
; * Multiple files: *
;
; With the advent of the Paul Ginsparg's "uufiles" function (available
; in the macros) to package figures, data files, and other auxiliary 
; files, the occurance of "all-in-one" papers will hopefully deminish. 
;
; If the Atavachron finds multiple (sequentially numbered) files at the 
; archive, it will first try process these as uuencoded figure files 
; (as produced by uufiles). If they are true uufiles, they are unpacked 
; and a report "localname.rpt" is shown. The paper (papernum.1) is then 
; loaded in tex-mode.
;
; If the mutiple files are not uufiles, they are each gotten to a buffer,
; with sequential integer extensions "localname.n". A *Buffer List* is 
; produced from which they can be acted upon.   
;
; Note that the buffer(s) containing the paper are marked as changed
; so that you will be prompted to save it if you try to delete it.
;
; Similarly, get-paper-from-data, prompts the user for a preprint
; number of the form: hep-lat/9211003, then goes away and loads this
; preprint, performing the above steps.
;  
;.....................................................................
;
;    store-paper
;    store-paper-from-data
;
; Store-paper, and store-paper-from-data, work like get-paper and 
; get-paper-from-data resp. for input data, but the paper is stored 
; directly in the user's default download directory (set below in 
; -Configuration-), for later processing with [* "process-paper"]. 
; This is convenient when reading the abstracts remotely (on
; the weekend from home, say) when one wants to save processing and
; printing the tex file for later, or so that the file can be further
; downloaded via modem.
;
;.....................................................................
;
;    process-paper
;    slice-and-dice
;    shell-paper
;
; "process-paper" performs the steps described in [* get-paper] for
; unpacking, shelling, and uudecoding the stored paper as if it was
; just "gotten" from the archive.
;
; "slice-and-dice" is the function called by "get-paper" and 
; "process-paper" when they find appended PostScript. It goes to 
; the beginning of the buffer and trys to cut off PostScript
; figures one by one. It's provided as a standalone function as
; a utility.
;
; "shell-paper" extracts the region below "^#!.*/bin/" to the file
; "buffer-name", then runs "sh buffer-name". The echoed output
; of this shell is collected and shown in the buffer "localname.rpt". 
;.....................................................................
;
;    get-listing-from-data
;    store-listing-from-data
;    get-abstract
;    get-abstract-from-data
;
; Get-listing-from-data and store-listing-from-data work the same way,
; except on title listings. They prompt the user for the listing
; in the usual format: hep-th/9204, then "get" (to a buffer),
; or "store" (to a file) the given month's listing. 
;
; Note that if the listing is gotten with get-listing-from-data into
; a buffer, then "get-paper" will work in that buffer as in the daily
; abstract list. Simply put the cursor on the paper of interest (AFTER
; the line beginning with "Paper:" and type M-x get-paper. 
;
; Get-abstract(-from-data) are the equivalent functions for abstracts,
; and are useful to read the abstract of a title gotten with 
; get-listing-from-data, etc.
;.....................................................................
;
;    interesting
; 
; The point of "interesting" is to save having to 'save' the
; whole mail file which caontains an interesting abstract. 
; "interesting" will prompt for a filename (offering atv-interesting-file
; as the default), then append the paper and abstract of choice (in
; which the point is located) to that file. Papers thus saved can
; be gotten with "get-paper" in the usual way.
;
;.....................................................................
;
;    atavachron-mail
;
; This handy function allows you to easily send a bug report, 
; comment, or question to the author. It opens a mail buffer
; addressed to me and dumps a few current variables which will be 
; useful reconstructing the situation if you wish to report a bug.
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    The Atavachron:    -End of Source File Documentation-     -\|/-    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'atavachron)
(require 'ange-ftp)

(setq atv-A "NONE") (setq atv-C "NONE") (setq atv-E "NONE") 
(setq atv-bogus-uu "NONE") (setq atv-branch "NONE") 
(setq atv-buf-height "NONE") (setq atv-chpd-figs "NONE") 
(setq atv-dir "NONE") (setq atv-dirs-eq "NONE")
(setq atv-dot-2 "NONE") (setq atv-dot-Z "NONE") 
(setq atv-dot-ind "NONE") (setq atv-dot-tar "NONE") 
(setq atv-fig-name "NONE") (setq atv-fig-name-guess "NONE")
(setq atv-file-complet "NONE") (setq atv-first-file "NONE") 
(setq atv-ftp-site "NONE") (setq atv-i "NONE") (setq atv-ii "NONE")
(setq atv-loc-fullname "NONE") (setq atv-loc-name "NONE") 
(setq atv-more-figs "NONE") (setq atv-multi-1 "NONE") 
(setq atv-n "NONE") (setq atv-n-files "NONE") (setq atv-name "NONE")
(setq atv-name-stub "NONE") (setq atv-nth "NONE") 
(setq atv-org-buf-name "NONE") (setq atv-paperno "NONE") 
(setq atv-proc-stub "NONE") (setq atv-remote-complet "NONE") 
(setq atv-remote-ith "NONE") (setq atv-shell-dir "NONE")
(setq atv-shelled-tex "NONE") (setq atv-shelled-tex-n "NONE") 
(setq atv-sh-command "NONE") (setq atv-slash "NONE") 
(setq atv-uuget-no "NONE") (setq atv-whole-data "NONE") 
(setq atv-yymm "NONE")


(defun atv-make-stub (atv-stub-full)
   (if (string-match "\\." atv-stub-full)
      (progn 
	 (setq atv-dot-ind (match-beginning 0))
         (setq atv-name-stub (substring atv-stub-full 0 atv-dot-ind)))
         (setq atv-name-stub atv-stub-full)))

(defun atv-get-name (atv-get-name-prompt atv-get-default atv-get-name-suffix)
   (setq atv-name (read-string atv-get-name-prompt))
   (if (not (string= atv-name ""))
       (setq atv-loc-name (concat atv-name atv-get-name-suffix)) 
       (setq atv-loc-name (concat atv-get-default atv-get-name-suffix)))
   (atv-make-stub atv-loc-name)
   (setq atv-name ""))

(defun atv-shrink-to-fit ()
   (interactive)
   (goto-char (point-max))
   (setq atv-buf-height (count-lines (window-start) (point)))
   (if (< atv-buf-height (window-height))
      (if (> atv-buf-height window-min-height)
         (shrink-window (- (window-height) atv-buf-height 1))
	 (shrink-window (- (window-height) window-min-height))))
   (goto-char (point-min)))

(defun atv-var-to-string (atv-trans-var)
  (cond
     ((stringp atv-trans-var) (format atv-trans-var))
     ((integerp atv-trans-var) (int-to-string atv-trans-var))
     ((if (eq atv-trans-var t) (format "t")))
     ((if (eq atv-trans-var nil) (format "nil")))))

(defun interesting ()
    (interactive)
    (save-excursion
      (re-search-backward "^Paper.*: ")
      (previous-line 2)
      (set-mark (point))
      (forward-line 2)
      (if (re-search-forward "\\\\\\\\")
         (if (re-search-forward "\\\\\\\\") (sit-for 0 1)))
      (exchange-point-and-mark)
    (atv-get-name (concat "Append to? (" atv-interesting-file "): ")
		 atv-interesting-file "")
    (append-to-file (mark) (point) atv-loc-name)))

(defun atv-extract-data-from-buffer ()
    (re-search-backward "^Paper.*: ")
    (re-search-forward ": ")
    (setq atv-A (point))
    (re-search-forward "/")
    (setq atv-branch (buffer-substring atv-A (point)))
    (setq atv-ftp-site atv-main-ftp-site)
    (setq atv-ftp-site (cond 
              ((string= "hep-lat/" atv-branch)
               "/anonymous\@ftp\.scri\.fsu\.edu:")
	      ((string= "alg-geom/" atv-branch)
	       "/anonymous@publications\.math\.duke\.edu:")
	      ((string= "lc-om/" atv-branch)
	       "/anonymous\@alcom-p\.cwru\.edu:")
              ((or (string= "astro-ph/" atv-branch)
                   (string= "cond-mat/" atv-branch)
                   (string= "funct-an/" atv-branch)
                   (string= "math-ph/" atv-branch))
	       "/anonymous\@babbage\.sissa\.it:")
              (atv-ftp-site))) 
    (setq atv-A (point))
    (forward-char 4)
    (setq atv-yymm (buffer-substring atv-A (point)))
    (end-of-line)
    (setq atv-paperno (buffer-substring atv-A (point)))
    (setq atv-dir (concat atv-ftp-site atv-branch "papers/" atv-yymm "/")))

(defun atv-ask-bozo-for-data (atv-listing)
   (if (eq atv-listing t)
      (setq atv-whole-data (read-string
	     "Enter branch/yymm [ex: hep-lat/9211]: ")) 
      (setq atv-whole-data (read-string
             "Enter paper number [format: cond-mat/9211003]: ")))
   (string-match "/" atv-whole-data)
   (setq atv-slash (match-beginning 0))
   (setq atv-branch (substring atv-whole-data 0 (+ atv-slash 1))) 
   (setq atv-ftp-site atv-main-ftp-site)
   (setq atv-ftp-site (cond 
              ((string= "hep-lat/" atv-branch)
               "/anonymous\@ftp\.scri\.fsu\.edu:")
	      ((string= "alg-geom/" atv-branch)
	       "/anonymous@publications\.math\.duke\.edu:")
	      ((string= "lc-om/" atv-branch)
	       "/anonymous\@alcom-p\.cwru\.edu:")
              ((or (string= "astro-ph/" atv-branch)
                   (string= "cond-mat/" atv-branch)
                   (string= "funct-an/" atv-branch)
                   (string= "math-ph/" atv-branch))
	       "/anonymous\@babbage\.sissa\.it:")
              (atv-ftp-site)))
    (setq atv-yymm (substring atv-whole-data (+ atv-slash 1) (+ atv-slash 5)))
    (setq atv-paperno (substring atv-whole-data (+ atv-slash 1)))
    (if (eq atv-listing t)
      (setq atv-dir (concat atv-ftp-site atv-branch "listings/")) 
      (setq atv-dir (concat atv-ftp-site atv-branch "papers/" atv-yymm "/"))))

(defun atv-init-loc-dirs ()
; *** append "/"
      (if (not (string= (substring atv-download-dir -1) "/"))
	 (setq atv-download-dir (concat atv-download-dir "/")))
      (if (not (string= (substring atv-uufiles-dir -1) "/"))
	 (setq atv-uufiles-dir (concat atv-uufiles-dir "/")))
; *** atv-download-dir =? atv-uufiles-dir
      (if (string= atv-download-dir atv-uufiles-dir)
	 (setq atv-dirs-eq t))
; *** Check existence of atv-download-dir
      (if (not (file-directory-p atv-download-dir))
         (progn 
	   (message (concat 
			  "Default download dir: " 
	  atv-download-dir " doesn't exist! ...I'm setting it to: ~/"))
	   (sit-for 3 1)
	   (setq atv-download-dir "~/")))
; *** Check for branches below local atv-download-dir
      (setq atv-download-dir 
          (cond ((file-directory-p (concat atv-download-dir atv-branch))
                   (concat atv-download-dir atv-branch))
                (atv-download-dir)))
      (if (eq atv-dirs-eq t) (setq atv-uufiles-dir atv-download-dir)))

(defun atv-chop-figs ()
  (message "Entering slice and dice mode...")
  (sit-for 1 1)
  (atv-init-loc-dirs)
  (setq atv-org-buf-name (buffer-name (current-buffer)))
  (set-visited-file-name (concat atv-download-dir atv-name-stub "\.tpl"))
  (save-buffer)
  (message (concat "Whole original is saved in: " atv-download-dir 
	   (buffer-name (current-buffer))))
  (sit-for 2 1)
  (save-excursion 
    (switch-to-buffer (concat atv-name-stub "\.figs"))
    (set-visited-file-name (concat atv-download-dir atv-name-stub "\.figs"))
    (insert-string (concat
" * -\\|/- The Atavachron  (1.1) * \n"
"Below are the PostScript figures extracted from: " atv-name-stub "\.tex\n"
"-----------------------------------------------\n")))
  (beginning-of-line)
  (setq atv-E (point))
  (end-of-line)
  (setq atv-chpd-figs 0)
  (setq atv-more-figs t)
  (while (eq atv-more-figs t)
    (progn 
      (beginning-of-line)
      (setq atv-C (point))
      (previous-line 20)
      (if (re-search-forward "\\([-a-zA-Z0-9\_]+\\(\\.ps\\|\\.eps\\)\\)"
			     atv-C t nil)
	   (setq atv-fig-name-guess 
		 (buffer-substring (match-beginning 1) (match-end 1)))
	   (setq atv-fig-name-guess "* NONE FOUND *"))
      (goto-char atv-C)
      (set-mark (point))
      (end-of-line)
      (if (re-search-forward "^\%\!" nil t nil)
	 (beginning-of-line)
	 (progn (goto-char (point-max)) (setq atv-more-figs nil)))
      (exchange-point-and-mark)
      (setq atv-fig-name (read-string (concat
	 "Name of figure file below this point? ("
	 atv-fig-name-guess "): ")))
      (if (string= atv-fig-name "")
	 (setq atv-fig-name atv-fig-name-guess))
      (exchange-point-and-mark)
      (write-region (mark) (point) (concat atv-uufiles-dir atv-fig-name))
      (message (concat "Extracted " atv-fig-name))
      (save-excursion
	(switch-to-buffer (concat atv-name-stub "\.figs"))
	(goto-char (point-max))
	(insert-string (concat atv-fig-name "\n")))
      (sit-for 2 1)
      (setq atv-chpd-figs (1+ atv-chpd-figs))
      (end-of-line)))
    (pop-to-buffer (concat atv-name-stub "\.figs"))
    (save-buffer)
    (atv-shrink-to-fit)
    (pop-to-buffer (concat atv-name-stub "\.tpl"))
    (message (concat "Surgery complete: " 
		     atv-chpd-figs " figure(s) removed.   Good Luck..."))
    (sit-for 2 1)
    (goto-char atv-E)
    (set-mark (point))
    (kill-region (mark) (point-max))
    (goto-char (point-min))
    (rename-buffer atv-org-buf-name))

(defun slice-and-dice ()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^\%\!" nil t nil)
  (atv-chop-figs))

(defun atv-type-check (atv-type-dir atv-type-name)
      (atv-init-loc-dirs)
; *** find all remote files matching paper name in type-dir
      (setq atv-remote-complet 
	    (file-name-all-completions atv-type-name atv-type-dir))
; *** remove abstract and check file completion list for types:
      (setq atv-i 0)
      (setq atv-dot-Z nil)
      (setq atv-dot-2 nil)
      (setq atv-dot-tar nil)
      (setq atv-file-complet (make-list 0 ""))
      (while (< atv-i (length atv-remote-complet))
         (progn 
	    (setq atv-remote-ith (elt atv-remote-complet atv-i))
            (if (or (< (length atv-remote-ith) 4)
                    (not (string= (substring atv-remote-ith -4 nil)
                               "\.abs")))
	       (progn
                 (setq atv-file-complet
                      (cons atv-remote-ith atv-file-complet))
		 (if (and (not atv-dot-Z) 
		     (string-match "\.Z" atv-remote-ith))
		        (setq atv-dot-Z t))
		 (if (and (not atv-dot-2) 
		     (string-match (concat atv-type-name "\.2") atv-remote-ith))
		        (setq atv-dot-2 t))
		 (if (and (not atv-dot-tar) 
		     (string-match (concat atv-type-name "\.tar\.Z") 
				                             atv-remote-ith))
		        (setq atv-dot-tar t))))
	        (setq atv-i (1+ atv-i))))
      (setq atv-n-files (length atv-file-complet))
      (setq atv-first-file (elt atv-file-complet 0)))

(defun atv-proc-unique (atv-u-dir atv-u-name atv-u-loc atv-listing)
;---Setup local file:---
            (atv-make-stub atv-u-name)
            (if (and (eq atv-prompt-filename t) (not (eq atv-u-loc t)))
               (atv-get-name 
                  (concat "Local name for paper? [.tex appended] (" 
			  atv-name-stub "): ") atv-name-stub "\.tex")
	          (setq atv-loc-name (concat atv-name-stub "\.tex")))
;---Get remote:---
            (find-file-other-window (concat atv-u-dir atv-first-file))
            (if (not (string= atv-loc-name (buffer-name (current-buffer))))
               (rename-buffer atv-loc-name))
            (if (string= (substring atv-loc-name -2 nil) "\.Z")
                (rename-buffer 
                   (substring atv-loc-name 0 
			      (- (length atv-loc-name) 2))))
	    (set-visited-file-name (concat atv-download-dir 
					   (buffer-name (current-buffer)))) 
;---Have a peek:---
            (if (not (eq atv-listing t)) 
              (progn
                (cond 
;    ---#!/bin/shell ouch! (--> atv-shell-out-buf)
                  ((and (goto-char (point-min))
	                 (re-search-forward "^\#\!.*/bin" nil t nil))
		     (progn
                       (if (y-or-n-p 
		     "Primary file contains shell commands. Unpack? ")
			  (progn
			    (atv-shell-out-buf atv-download-dir 
					     (concat atv-name-stub "\.sh")
					     atv-name-stub t)
			    (goto-char (point-min))
			    (next-line 3)
			    (setq atv-shelled-tex-n 0)
			    (while (re-search-forward 
				 "\\([-a-zA-Z0-9\_]+\\.tex\\)" nil t nil)
			       (progn
                      	         (setq atv-shelled-tex
				    (buffer-substring (match-beginning 1) 
						      (match-end 1)))
				 (setq atv-shelled-tex-n 
				                     (1+ atv-shelled-tex-n))))
			       (if (eq atv-shelled-tex-n 1)
				 (find-file-other-window
				    (concat atv-download-dir atv-shelled-tex)))
                               (if (eq atv-shelled-tex-n 1)
			         (progn 
				   (message (concat
			 "Can't find any tex file in shell echo output: " 
			 atv-name-stub "\.rpt"))
				   (sit-for 3 1)
				   (message (concat
			     "Check file: " 
			     atv-name-stub "\.sh  manually"))))))))
;---Appended PostScript: chop chop (--> atv-chop-figs)
                  ((and (goto-char (point-min))
                     (re-search-forward "^\%\!" nil t nil))
                       (progn 
			 (message 
   "Looks like there are appended PostScript figures...")
			 (sit-for 2 1)
			 (if (y-or-n-p 
   "Shall I surgically remove the figures? ")
			    (progn
			       (atv-chop-figs)
			       (tex-mode)
			       (if (eq atv-auto-tex-paper t) 
				   (funcall atv-auto-tex-command)))
			    (message "I understand..."))))
;---Regular old fashioned paper:---
                  (t (progn
                     (tex-mode)
                     (if (eq atv-auto-tex-paper t) 
			(funcall atv-auto-tex-command))))))))

(defun shell-paper ()
   (interactive)
   (atv-init-loc-dirs)
   (setq atv-name (read-string 
           (concat "Directory for shell operation? (" atv-download-dir
                   "): ")))
   (if (not (string= atv-name ""))
       (setq atv-shell-dir atv-name) 
       (setq atv-shell-dir atv-download-dir))
   (if (not (string= (substring atv-shell-dir -1) "/"))
       (setq atv-shell-dir (concat atv-shell-dir "/")))
   (atv-make-stub (buffer-name (current-buffer)))
   (atv-shell-out-buf atv-shell-dir (buffer-name (current-buffer))
		     atv-name-stub t))

(defun atv-shell-out-buf (atv-sh-dir atv-sh-file atv-sh-rpt atv-show-rpt)
   (goto-char (point-min))
   (if (re-search-forward "^\#\![ \t]*\\(.*/bin/.*\\)$" nil t)
      (progn
        (setq atv-sh-command (buffer-substring (match-beginning 1)
					       (match-end 1)))
	(beginning-of-line)
        (if (and (eq atv-no-csh t) (search-forward "bin/csh"))
           (progn                  ;  takes #!/bin/csh
              (backward-char 3)    ;      --->  #!/bin/sh
	      (delete-char 1)      ;  for those lacking csh
              (beginning-of-line)
              (setq atv-sh-command "/bin/sh")))
        (write-region (point) (point-max) (concat atv-sh-dir atv-sh-file))
        (message "Executing shell script...")
        (shell-command (concat "cd " atv-sh-dir "; "
                         atv-sh-command " " atv-sh-file))
        (set-buffer-modified-p nil)
        (bury-buffer)))
        (pop-to-buffer "*Shell Command Output*")
        (rename-buffer (concat atv-sh-rpt "\.rpt"))
        (goto-char (point-min))
        (insert-string (concat
" * -\\|/- The Atavachron  (1.1) * \n"
"This is the output of: " atv-sh-command " " atv-sh-dir atv-sh-file "\n"
"Below should be a manifest of the enclosed files (or shell output):\n"
"------------------------------------------------------------------\n"))
        (set-visited-file-name (concat atv-sh-dir 
				   (buffer-name (current-buffer)))) 
	(atv-shrink-to-fit)
        (if (not (eq atv-show-rpt t))
	   (progn 
              (save-buffer)
              (kill-buffer (current-buffer))
              (delete-window))))

(defun atv-get-and-store (atv-s-dir atv-s-name atv-s-listing)  
; *** assumes previous call to atv-type-check
    (if (and (eq atv-prompt-filename t) (not (eq atv-s-listing t)))
       (atv-get-name 
          (concat "Local name for paper? (" atv-paperno 
		  "): ") atv-paperno "") 
       (setq atv-loc-name atv-paperno))
    (cond 
;--- check that something matched paper number at archive
      ((not (stringp atv-first-file))
         (message (concat "No files match: " atv-s-dir atv-s-name)))

;--- unique: (plain or .Z)
      ((and (eq atv-n-files 1) (not (eq atv-dot-tar t)))
        (if (eq atv-dot-Z t)
	   (progn
             (copy-file (concat atv-s-dir atv-first-file) 
		      (concat atv-download-dir atv-loc-name "\.Z") 1)
	     (message (concat "Copied " atv-download-dir atv-loc-name "\.Z")))
	   (progn
	     (copy-file (concat atv-s-dir atv-first-file) 
		      (concat atv-download-dir atv-loc-name) 1)
	     (message (concat "Copied " atv-download-dir atv-loc-name)))))
;--- unique: tarred remote
         ((eq atv-dot-tar t)
	   (progn
             (copy-file (concat atv-s-dir atv-first-file) 
		      (concat atv-download-dir atv-loc-name "\.tar\.Z") 1)
	     (message (concat "Copied " atv-loc-name "\.tar\.Z"))))
;--- uufigs (.2,...)
   ((eq atv-dot-2 t)
     (progn
	(setq atv-n 1)
	(setq atv-uuget-no 0)
	(while (<= atv-n atv-n-files)
	  (progn
	    (setq atv-ii 0)
	    (while (< atv-ii atv-n-files)
	      (progn
;    >--- file matches "atv-s-name.n"
		(if (string-match
		     (concat atv-s-name "\." (int-to-string atv-n))
		          (elt atv-file-complet atv-ii))
		   (progn
		     (if (eq atv-dot-Z t)
			(copy-file (concat atv-s-dir atv-s-name "\." 
					   (int-to-string atv-n) "\.Z")
		                   (concat atv-download-dir atv-loc-name "\." 
					   (int-to-string atv-n) "\.Z") 1)
		        (copy-file (concat atv-s-dir atv-s-name "\." 
					   (int-to-string atv-n))
			           (concat atv-download-dir atv-loc-name "\." 
					   (int-to-string atv-n)) 1))
		  (setq atv-uuget-no (1+ atv-uuget-no))))
                  (setq atv-ii (1+ atv-ii))))
           (setq atv-n (1+ atv-n))))
     (message (concat "Copied " atv-uuget-no " files."))))

;--- distributed in more than 2 files (!?!)
         ((and (> atv-n-files 2) (not (eq atv-dot-2 t)))
            (if (y-or-n-p (concat 
                 "Paper distributed in " 
                  (int-to-string atv-n-files) 
                 " files without integer suffices!  Continue? "))
            (progn
	       (message 
             "Copying--- (archive filenames are preserved since I'm confused)")
               (sit-for 3 1)
	       (setq atv-i 0)
               (while (< atv-i atv-n-files)
		 (setq atv-nth (elt atv-file-complet atv-i))
		 (message (concat "Copying: " atv-nth))
		 (sit-for 1 1)
		 (setq atv-i (1+ atv-i))
		 (copy-file (concat atv-dir atv-nth) 
			       (concat atv-download-dir atv-nth))))))
       (t (message 
   "BOGUS!: Can't decide what to do... (report bug with atavachron-mail)")))
    (if (not (eq atv-s-listing t))
       (progn (sit-for 2 1)
       (message (concat "Use \"process-paper\" on \"" atv-loc-name
		     "\" at your convenience..."))))) 


(defun atv-untar (atv-t-dir atv-t-file atv-t-stub atv-t-suffix atv-tarview)
;            untar:   dir,  file,  name stub, add "suffix",   |
;                                                 if t keep output in view
   (message "Untarring...")
   (shell-command (concat "cd " atv-t-dir "; "
          "zcat " atv-t-file " | tar -xvf -"))
   (pop-to-buffer "*Shell Command Output*")
   (rename-buffer (concat atv-t-stub atv-t-suffix))
   (goto-char (point-min))
   (insert-string (concat
" * -\\|/- The Atavachron  (1.1) * \n"
"This is the output of: zcat " atv-t-dir atv-t-file " | tar -xvf - \n"
"Below should be a manifest of the enclosed files:\n"
"------------------------------------------------\n"))
   (set-visited-file-name (concat atv-t-dir 
				   (buffer-name (current-buffer)))) 
   (save-buffer)
   (atv-shrink-to-fit)
   (if (not (eq atv-tarview t))
      (progn
	(kill-buffer (current-buffer))
	(delete-window))))

(defun atv-get-and-proc (atv-g-dir atv-g-name atv-g-loc atv-listing)
;--- assumes call to atv-type-check to set atv-n-files, etc.
; *** GET file:
;               * unique:
;		- tarred? 
;               * distributed:
;		- uufigs (.2,...)
;		- other (?!)

    (atv-make-stub atv-g-name)
    (cond 
;--- check that something matched paper number at archive
      ((not (stringp atv-first-file))
         (message (concat "No files match: " atv-g-dir atv-g-name)))

;--- unique: readable (plain or .Z)
      ((and (eq atv-n-files 1) (not (eq atv-dot-tar t))) 
           (atv-proc-unique atv-g-dir atv-g-name atv-g-loc atv-listing))


;--- unique: tarred remote
         ((and (eq atv-n-files 1) (eq atv-dot-tar t))
	  (if (not (eq atv-g-loc t))
	   (progn
              (message
                (concat "Paper is archived: " atv-first-file))
	      (sit-for 1 1)
              (if (eq atv-prompt-filename t)
                (atv-get-name 
		   (concat "Rename incoming tarfile? [.tar.Z appended] (" 
			   atv-g-name "): ") atv-g-name "\.tar\.Z") 
		(setq atv-loc-name atv-first-file))
	      (setq atv-loc-fullname (concat atv-download-dir atv-loc-name))
	      (message "Copying...")
              (copy-file (concat atv-g-dir atv-first-file) atv-loc-fullname 1)
	      (message (concat "Copied " atv-loc-fullname))
	      (if (y-or-n-p "Unpack tarfile now? ")
               (atv-untar atv-download-dir atv-loc-name atv-name-stub "\.rpt" t)
	       (message (concat "Use \"process-paper\" on \"" atv-name-stub
				"\" at your convenience..."))))
	   (progn
	     (message (concat 
		       "File is tarred: " atv-g-name ".tar.Z; Unpacking..."))
	     (sit-for 2 1)
	     (atv-untar atv-g-dir (concat atv-g-name "\.tar\.Z")
		       atv-g-name "\.rpt" t))))

; Distributed: Figures (atv-paperno.2, or more)

   ((eq atv-dot-2 t)
     (progn
       (message (concat 
 "Paper distributed in " atv-n-files " files."))
       (sit-for 2 1)
       (if (and (eq atv-prompt-filename t) (not (eq atv-g-loc t)))
          (atv-get-name 
             (concat "Local name for files? [.tex or .n appended] (" 
			  atv-name-stub "): ") atv-name-stub "")
          (setq atv-loc-name atv-name-stub))
       (message "Checking for uu-encoded figures or files...")
       (sit-for 2 1)
       (setq atv-n 2)
       (setq atv-uuget-no 0)
       (setq atv-bogus-uu nil)
       (while (<= atv-n atv-n-files)
	  (progn
	    (setq atv-ii 0)
	    (while (< atv-ii atv-n-files)
	      (progn
;  --- file matches "atv-g-name.n"
		(if (string-match
		     (concat atv-g-name "\." (int-to-string atv-n))
		          (elt atv-file-complet atv-ii))
		   (progn
		     (if (eq atv-dot-Z t)
			(find-file (concat atv-g-dir atv-g-name "\." 
					   (int-to-string atv-n) "\.Z"))
		        (find-file (concat atv-g-dir atv-g-name "\." 
					   (int-to-string atv-n))))
		  (setq atv-uuget-no (1+ atv-uuget-no))

; --- process suspected uufile:
		(goto-char (point-min))
	  (if (search-forward "file created by csh script  uufiles" nil t)
	     (progn
                (if (not (string= 
                 (concat atv-name-stub  "\." (int-to-string atv-n))
		 (buffer-name (current-buffer))))
                   (rename-buffer (concat atv-name-stub "\." 
				       (int-to-string atv-n))))
		(set-visited-file-name (concat atv-uufiles-dir 
					   (buffer-name (current-buffer)))) 
	        (previous-line 1)
		(beginning-of-line)
		(if (looking-at "#!/bin/csh -f.*")
		    (progn
		      (message "unpacking uufile...")
		      (if (eq atv-no-csh t)
                         (progn                 ;  takes #!/bin/csh
			   (forward-char 7)     ;      --->  #!/bin/sh
			   (delete-char 1)      ;  for those lacking csh
			   (beginning-of-line)))
		      (write-region (point) (point-max) 
			    (concat atv-uufiles-dir atv-loc-name 
				    "\." (int-to-string atv-n)))
		      (shell-command (concat "cd " atv-uufiles-dir "; "
                         "sh " atv-loc-name "\."  (int-to-string atv-n)))
                      (set-buffer-modified-p nil)
		      (bury-buffer)))
		(pop-to-buffer "*Shell Command Output*")
		(rename-buffer (concat atv-loc-name 
				       (int-to-string atv-n) "\.rpt"))
		(goto-char (point-min))
		(insert-string (concat
" * -\\|/- The Atavachron  (1.1) * \n"
"This is the output of: sh " atv-branch atv-paperno "\." 
(int-to-string atv-n) "\n"
"Below should be a manifest of the enclosed files:\n"
"------------------------------------------------\n"))
		(set-visited-file-name (concat atv-uufiles-dir 
					   (buffer-name (current-buffer)))) 
		(save-buffer)
                (atv-shrink-to-fit))

             (progn
                (message (concat atv-loc-name "\." (int-to-string atv-n)
	   " is not a uufile;  Please process by hand."))
                (sit-for 3 1)
                (setq atv-bogus-uu t)
                (if (not (string= 
		  (concat atv-loc-name "\." (int-to-string atv-n))
			  (buffer-name (current-buffer))))
                  (rename-buffer 
                        (concat atv-loc-name "\." (int-to-string atv-n))))
		(set-visited-file-name (concat atv-uufiles-dir 
				       (buffer-name (current-buffer))))))))
       (setq atv-ii (1+ atv-ii))))
     (setq atv-n (1+ atv-n))))
; ---- and finally the paper: paperno.1
                (if (not (eq atv-bogus-uu t))
                   (progn
                      (message "Figures processed, getting paper...")
                      (setq atv-multi-1 "\.tex"))
                   (setq atv-multi-1 "\.1"))
                (if (eq atv-dot-Z t)
                  (find-file-other-window (concat atv-g-dir atv-g-name "\.1\.Z"))
		  (find-file-other-window (concat atv-g-dir atv-g-name "\.1")))
		(if (not (string= (concat atv-loc-name atv-multi-1) 
                                            (buffer-name (current-buffer))))
		   (rename-buffer (concat atv-loc-name atv-multi-1)))
                (if (and (> (length atv-loc-name) 1) 
                         (string= (substring atv-loc-name -2 nil) "\.Z"))
                   (rename-buffer 
                       (substring atv-loc-name 0 
			      (- (length atv-loc-name) 2))))
	        (set-visited-file-name (concat atv-download-dir 
 				     (buffer-name (current-buffer))))
                (if (eq atv-bogus-uu t)
                   (progn
                      (list-buffers)
                      (atv-shrink-to-fit)
                      (sit-for 0)
                      (if (> atv-uuget-no 1)
                         (message (concat "Also fetched " 
                                (int-to-string atv-uuget-no) 
                         " other unprocessed files;  see *Buffer List*"))
                         (message (concat "Also fetched: " atv-loc-name 
            "\.2  (see *Buffer List*);  please process files manually.")))
                      (sit-for 4 1))
                      (setq atv-bogus-uu nil))
                (cond 
                  ((and (goto-char (point-min))
                     (re-search-forward "^\%\!" nil t nil))
                       (message (concat
                                "Primary file contains PostScript! "
                                "Try \"slice-and-dice\" [see documentation]")))
                  ((and (goto-char (point-min))
	             (re-search-forward "^\#\!.*/bin" nil t nil))
                       (message (concat 
                                "Primary file contains shell commands! "
                                "Try \"shell-paper\" [see documetation]")))
                  ((not (eq atv-bogus-uu t))
                     (progn
                       (tex-mode) 
                       (if (eq atv-auto-tex-paper t) 
                          (progn (sit-for 2 1) 
                            (funcall atv-auto-tex-command))))))))

; ------------ distributed in non-integer .ext files (?)
         ((and (> atv-n-files 2) (not (eq atv-dot-2 t)))
            (if (y-or-n-p (concat 
                 "Paper distributed in " 
                  (int-to-string atv-n-files) 
                 " files without integer suffices!  Continue? "))
            (progn
	       (message 
     "Copying--- (archive filenames are preserved since I'm a bit confused)")
               (sit-for 3 1)
	       (setq atv-i 0)
               (while (< atv-i atv-n-files)
		 (setq atv-nth (elt atv-file-complet atv-i))
		 (message (concat "Copying: " atv-nth))
		 (sit-for 1 1)
		 (setq atv-i (1+ atv-i))
		 (copy-file (concat atv-dir atv-nth) 
			       (concat atv-download-dir atv-nth))))))
       (t (message 
   "BOGUS!: Can't decide what to do... (report bug with atavachron-mail)")))) 

(defun get-paper ()
"--------- searches backward to the first occurance of the regexp
\"^Paper.*: \", and extracts the paper data from the buffer. This
paper is then gotten from the appropriate archive using ange-ftp.
tex-mode is loaded in the buffer, and the buffer and visited file
names are renamed appropriately."
   (interactive)
   (atv-extract-data-from-buffer)
   (atv-type-check atv-dir atv-paperno)
   (atv-get-and-proc atv-dir atv-paperno nil nil))

(defun process-paper ()
"------------- prompts for a local paper name (such as Smith, Jones-A,
or 9212001), and processes the file(s) exactly as would \"getting\"
them with *get-paper. Thus appended PostScript is extracted, uufiles
decoded, or tar files unpacked, depending on the paper format of the
stored files. Works only on papers stored in atv-download-dir."
  (interactive)
  (setq atv-proc-stub (read-string 
      "Name of stored paper? [without suffix]: "))
  (setq atv-name-stub atv-proc-stub)
  (atv-type-check atv-download-dir atv-proc-stub)
  (atv-get-and-proc atv-download-dir atv-proc-stub t nil))

(defun get-paper-from-data ()
"------------------- prompts the user for the paper data
in the usual form: hep-th/9204083
This paper is then gotten from the appropriate archive using
ange-ftp.  tex-mode is loaded in the buffer, and the buffer and
visited file names are renamed appropriately."
   (interactive)
   (atv-ask-bozo-for-data nil)
   (atv-type-check atv-dir atv-paperno)
   (atv-get-and-proc atv-dir atv-paperno nil nil))

(defun get-listing-from-data ()
"--------------------- gets a month's titles from input of the 
form: hep-th/9204. Once this listing is loaded, get-paper can be 
run on any paper, provided the point (cursor) is below the line
beginning with \"Paper\""
   (interactive)
   (atv-ask-bozo-for-data t)
   (atv-type-check atv-dir atv-paperno)
   (atv-get-and-proc atv-dir atv-paperno nil t))

(defun store-paper ()
"----------- searches backward to the first occurance of the regexp
\"^Paper.*: \", and extracts the paper data from the buffer. This
paper is then gotten from the appropriate archive using ange-ftp.
The paper is stored in the directory given by atv-download-dir, or
the appropriate subdir (hep-th, hep-lat, etc.) if it exists."
   (interactive)
   (atv-extract-data-from-buffer)
   (atv-type-check atv-dir atv-paperno)
   (atv-get-and-store atv-dir atv-paperno nil))

(defun store-paper-from-data ()
"--------------------- prompts the user for the paper data
in the usual form: hep-th/9204083
This paper is then gotten from the appropriate archive using
ange-ftp. The paper is stored in the directory given by atv-download-dir, 
or the appropriate subdir (hep-th, hep-lat, etc.) if it exists." 
   (interactive)
   (atv-ask-bozo-for-data nil)
   (atv-type-check atv-dir atv-paperno)
   (atv-get-and-store atv-dir atv-paperno nil))

(defun store-listing-from-data () 
"----------------------- gets a month's titles from input of 
the form: hep-th/9204, and stores it in atv-download-dir."
   (interactive) 
   (atv-ask-bozo-for-data t)
   (atv-type-check atv-dir atv-paperno)
   (atv-get-and-store atv-dir atv-paperno t))

(defun get-abstract () 
"--------- searches backward to the first occurance of the regexp
\"^Paper.*: \", and extracts the paper data from the buffer.
This paper's abstract is then gotten to a separate window."
   (interactive) 
   (atv-extract-data-from-buffer)
   (find-file-other-window (concat atv-dir atv-paperno "\.abs")))

(defun get-abstract-from-data () 
"----------------------- gets a paper's abstract from input of 
the form: hep-lat/9212005."
   (interactive) 
   (atv-ask-bozo-for-data nil)
   (find-file-other-window (concat atv-dir atv-paperno "\.abs")))

(defun atavachron-mail ()
"------------- opens a *Mail* buffer addressed to the author of 
atavachron.el (Jim Hetrick, hetrick@phys.uva.nl), dumps some environment 
variables and current atv- variables for reconstructing bugs, and leaves
the point after \"User comments:\", for entering correspondence."
  (interactive)
  (mail nil "hetrick@phys\.uva\.nl" "Atavachron-* report")
  (goto-char (point-max))
  (insert-string (concat 
"
 * -\\|/- The Atavachron (v1.1) *  == Automatic System Report ==

 User Comments:




................................................................
 >  user: " (user-full-name) "
 >  machine: " (system-name) "
 >  system type: " (symbol-name (symbol-value 'system-type)) "
 >  emacs version: " (emacs-version) "
 >  environment: 
    - TERM: " (or (getenv "TERM") "") "
    - SHELL: " (or (getenv "SHELL") "") "
    - TEXINPUTS: " (or (getenv "TEXINPUTS") (getenv "TEX_INPUTS") "") "
 >  main archive: " atv-main-ftp-site "
 >  most recent paper:
    - site:   " atv-ftp-site  "
    - branch: " atv-branch "
    - number: " atv-paperno "
    - full: " atv-paperno "
 >  local download dir: " atv-download-dir "
 >  local paper name: " atv-loc-name "
 >  Internal:
" 
(atv-var-to-string atv-A) "::"
(atv-var-to-string atv-C) "::"
(atv-var-to-string atv-E) "::"
(atv-var-to-string atv-bogus-uu) "::\n"
(atv-var-to-string atv-branch) "::"
(atv-var-to-string atv-buf-height) "::"
(atv-var-to-string atv-chpd-figs) "::"
(atv-var-to-string atv-dir) "::\n"
(atv-var-to-string atv-dirs-eq) "::"
(atv-var-to-string atv-dot-2) "::"
(atv-var-to-string atv-dot-Z) "::"
(atv-var-to-string atv-dot-ind) "::\n"
(atv-var-to-string atv-dot-tar) "::"
(atv-var-to-string atv-fig-name) "::"
(atv-var-to-string atv-fig-name-guess) "::"
(atv-var-to-string atv-file-complet) "::\n"
(atv-var-to-string atv-first-file) "::"
(atv-var-to-string atv-ftp-site) "::"
(atv-var-to-string atv-i) "::"
(atv-var-to-string atv-ii) "::\n"
(atv-var-to-string atv-loc-fullname) "::"
(atv-var-to-string atv-loc-name) "::"
(atv-var-to-string atv-more-figs) "::"
(atv-var-to-string atv-multi-1) "::\n"
(atv-var-to-string atv-n) "::"
(atv-var-to-string atv-n-files) "::"
(atv-var-to-string atv-name) "::"
(atv-var-to-string atv-name-stub) "::\n"
(atv-var-to-string atv-nth) "::"
(atv-var-to-string atv-org-buf-name) "::"
(atv-var-to-string atv-paperno) "::"
(atv-var-to-string atv-proc-stub) "::\n"
(atv-var-to-string atv-remote-complet) "::"
(atv-var-to-string atv-remote-ith) "::"
(atv-var-to-string atv-shell-dir) "::"
(atv-var-to-string atv-shelled-tex) "::\n"
(atv-var-to-string atv-shelled-tex-n) "::"
(atv-var-to-string atv-sh-command) "::"
(atv-var-to-string atv-shelled-tex-n) "::"
(atv-var-to-string atv-slash) "::\n"
(atv-var-to-string atv-uuget-no) "::"
(atv-var-to-string atv-whole-data) "::"
(atv-var-to-string atv-yymm) "::"
))
(goto-char (point-min))
(search-forward "User comments")
(next-line 2)
(insert-string "       "))
