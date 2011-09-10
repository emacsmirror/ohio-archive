;;; iso-tex.el --- Translating TeX to ISO-8859/1 while editing a file
;;*****************************************************************************
;; $Id: iso-tex.el,v 1.14 1994/06/11 22:08:22 gerd Exp gerd $
;;*****************************************************************************
;;
;; Description:
;;	When using (La)TeX it is highly desirable to display national
;;	characters contained in the ISO-8859/1 character set. Editing
;;	files with ISO-8859/1 characters is enabled in GNU Emacs since
;;	version 19.
;;
;;	Two ways can be envisaged. The first one is to teach TeX the
;;	meaning of the extended character set (e.g. via a sty file in
;;	LaTeX). This solution has the disadvantage that it may collide
;;	with other extended character sets.
;;
;;	The second solution is used in iso-tex.el. The extended
;;	character set is used only temporarily during editing of a
;;	file in Emacs. The file processed by TeX contains pure ASCII
;;	representations of the extended characters (if possible).
;;
;;	iso-tex.el provides a minor mode which anchors itself in
;;	various hooks to perform translations when reading or writing
;;	files.
;;
;;	No provisions are made to insert ISO-8859/1 characters since
;;	other packages are availlable for this purpose.
;;
;;
;; Installation:
;;	1. Ensure that iso-tex.el is on the load-path.
;;	2. For efficiency it might be desirable to byte-compile 
;;	   iso-tex.el.
;;	3. Put the following in your .emacs file or a similar place
;;	   where it is loaded when needed.
;;
;;	   (autoload 'iso-tex-minor-mode
;;		     "iso-tex"
;;		     "Translate TeX to ISO 8859/1 while visiting a file."
;;		     t)
;;
;;	4. Enable the iso-tex minor mode for the appropriate
;;	   files. This depends on the major mode you use for editing
;;	   (La)TeX files. For this purpose you can use the entry hook
;;	   of this mode. E.g.
;;
;;	   (setq TeX-mode-hook 
;;		 (function (lambda () (interactive)
;;			     (iso-tex-minor-mode 1)
;;				; and other initializations
;;				; ...
;;			     )))
;;
;;	   Alternatively tex-mode-hook, latex-mode-hook, or LaTeX-mode-hook
;;	   might be places to perform the initialization.
;;
;; For users of german.sty:
;;
;;	Instead of the autoload command mentioned under point 3. use the
;;	following initializations. They cause the german.sty umlaut
;;	variants to be inserted.
;;
;;	(load "iso-tex")
;;      (define-iso-tex "Ä" "\"A"	"\\\"A" "\"A"	) ; german.sty
;;      (define-iso-tex "Ë" "\"E"	"\\\"E" "\"E"	) ; german.sty
;;      (define-iso-tex "Ï" "\"I"	"\\\"I" "\"I"	) ; german.sty
;;      (define-iso-tex "Ö" "\"O"	"\\\"O" "\"O"	) ; german.sty
;;      (define-iso-tex "Ü" "\"U"	"\\\"U" "\"U"	) ; german.sty
;;      (define-iso-tex "ß" "\"s"	"\\ss" "\"s" "\\3" ) ; german.sty
;;      (define-iso-tex "ä" "\"a"	"\\\"a" "\"a"	) ; german.sty
;;      (define-iso-tex "ë" "\"e"	"\\\"e" "\"e"	) ; german.sty
;;      (define-iso-tex "ï" "\"i"	"\\\"\\i" "\"i"	) ; german.sty
;;      (define-iso-tex "ö" "\"o"	"\\\"o" "\"o"	) ; german.sty
;;      (define-iso-tex "ü" "\"u"	"\\\"u" "\"u"	) ; german.sty
;;
;;
;;	The same trick can be used to redefine any string
;;	representation of the ISO characters. E.g. 
;;
;;      (define-iso-tex "µ" "\\MYmu "	"\\MYmu" )
;;
;;	inserts the string "\MYmu" for µ. \MYmu can now be defined to
;;	produce a µ in any environment. E.g. in LaTeX by
;;
;;	\newcommand\MYmu{\mbox{\(\mu\)}}
;;
;;
;; Bugs and Problems:
;;	- Some of the TeX sequences which are inserted by iso-tex are
;;	  not defined in plain (La)TeX. Don't use them or provide defs.
;;
;;	- Some of the TeX sequences which are inserted by iso-tex are
;;	  only defined inside/outside math environments. Be careful!
;;
;;	- There might be problems when saving a narrowed buffer.
;;	- Point might not be restored properly.
;;
;;	- There are quite a few ways to code accented characters in
;;	  TeX. Only some are captured in this program.
;;
;;	- Writing of a region is not supported. There seems to be no
;;	  appropriate hook.
;;
;;
;; To do:
;;	- iso-tex can be used for a wider range of translations when
;;	  reading and writing. Maybe it's worth extracting those
;;	  routines which are more general and make iso-tex a sample
;;	  instance of the general routines.
;;
;;
;; Changes:
;;	- enhanced patterns for recognition of accented characters in TeX
;;	- german.sty (partially) supported
;;	- minor bugs fixed to generate accented i.
;;
;;	- after-save-hooks seems to be after-save-hook now.
;;
;;	- german.sty support partially removed because it interfered e.g.
;;	  with BibTeX strings.
;;
;;	- after-save-hooks and after-save-hook are both set.
;;
;;	- Default TeX sequences changed to conform to BibTeX coding standart.
;;
;;
;; Author:	
;;	Gerd Neugebauer
;;	Ödenburger Str. 16
;;	64295 Darmstadt (Germany)
;;
;;	Net: gerd@imn.th-leipzig.de
;;	     gerd@intellektik.informatik.th-darmstadt.de
;;
;;*****************************************************************************
;; LCD Archive Entry:
;; iso-tex|Gerd Neugebauer|gerd@intellektik.informatik.th-darmstadt.de|
;; Translating TeX to ISO-8859/1 while editing a file.|
;; $Date: 1994/06/11 22:08:22 $|$Revision: 1.14 $|~/modes/iso-tex.el.Z|
;;*****************************************************************************
;;
;; Copyright (C) 1994 Gerd Neugebauer
;;
;; iso-tex.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; iso-tex.el, but only under the conditions described in the
;; GNU General Public License.	 A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.	Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;

;;;----------------------------------------------------------------------------
;;; Variable definitions and initializations

(defvar iso-tex-minor-mode nil
  "Variable indicating when iso-tex-minor-mode is active.")

(or (assq 'iso-tex-minor-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(iso-tex-minor-mode " ISO-TeX")
				 minor-mode-alist)))

(defvar iso-tex-minor-mode-initialized nil
  "Variable indicating if iso-tex-minor-mode is already initialized in this
buffer. This variable is buffer local.")

(make-variable-buffer-local 'iso-tex-minor-mode-initialized)

(defvar tex-2-iso-regex
  (concat
   "\\\\[\"'`^~]\\([a-zA-Z]\\|\\\\i\\)?\\({}\\)?"	"\\|"
   "\\\\[a-zA-Z@]+\\({}\\|[ \t]?\\)"			"\\|"
   "\\\\[c\"'`^~]{[a-zA-Z]?}"				"\\|"
   "{\\\\[c\"'`^~]{[a-zA-Z]?}}"				"\\|"
   "{\\\\[\"'`^~]\\([a-zA-Z]\\|\\\\i\\)}"		"\\|"
   "{\\\\[a-zA-Z]+[ \t]*}"				"\\|"
   "\"[a-zA-Z]"						"\\|" ; for german.sty
   "[!?]`"
  )
  "Regular expression to pre-select substrings to be translated by tex-2-iso.")
(defvar iso-2-tex-regex "[€-ÿ]"
  "Regular expression to pre-select substrings to be translated by iso-2-tex.")
(defvar iso-2-tex-alist nil
  "Alist of CHAR.STRING pairs used by iso-2-tex.")
(defvar tex-2-iso-alist nil
  "Alist of STRING.ISO-CHAR pairs used by tex-2-iso.")

(defun define-iso-tex (char &optional string &rest names)
  "Define a translation between ISO-8859/1 characters and TeX sequences.
CHAR is the ISO-8859/1 character code as a single letter string.
If STRING is non nil then it is used as representation of CHAR.
The optional remaining arguments are used to translate TeX sequences to
characters."
  (if string (setq iso-2-tex-alist (cons 
				    (cons (string-to-char char) string) 
				    iso-2-tex-alist)))
  (while names
    (setq tex-2-iso-alist (cons (cons (car names) char) 
				tex-2-iso-alist))
    (setq names (cdr names))
  )
)

(if (null iso-2-tex-alist)
    (progn
      (define-iso-tex "¡" "!`"		"!`"		)
      (define-iso-tex "¢" "{\\cent}"	"\\cent"	) ; usually undefined
      (define-iso-tex "£" "{\\pounds}"	"\\pounds"	)
      (define-iso-tex "¤" "{\\currency}" "\\currency"	) ; usually undefined
      (define-iso-tex "¥" "{\\yen}"	"\\yen"		) ; usually undefined
      (define-iso-tex "¦" "{\\MiD}"	"\\MiD"		) ; usually undefined
      (define-iso-tex "§" "{\\S}"	"\\S"		)
      (define-iso-tex "¨" "{\\\"{}}"	"\\\""		)
      (define-iso-tex "©" "{\\copyright}" "\\copyright" )
      (define-iso-tex "ª" "{\\OIfem}"	"\\OIfem"	) ; usually undefined
      (define-iso-tex "«" "\\ll "	"\\ll"		) ; math mode only
      (define-iso-tex "¬" "\\neg "	"\\neg"		) ; math mode only
      (define-iso-tex "­" "\\-" ) ; It's too hard to distinguish from -
      (define-iso-tex "®" "{\\Registered}" "\\Registered") ; usually undefined
      (define-iso-tex "¯" "{\\macron}"	"\\macron"	) ; usually undefined
      (define-iso-tex "°" "{\\degree}"	"\\degree"	) ; usually undefined
      (define-iso-tex "±" "\\pm "	"\\pm"		) ; math mode only
      (define-iso-tex "²" "\\SuperTwo " "\\SuperTwo"	) ; usually undefined
      (define-iso-tex "³" "\\SuperThree " "\\SuperThree") ; usually undefined
      (define-iso-tex "´" "{\\'{}}"	"\\'"		)
      (define-iso-tex "µ" "\\mu "	"\\mu"		) ; math mode only
      (define-iso-tex "¶" "{\\P}"	"\\P"		)
      (define-iso-tex "·" "\\cdot "	"\\cdot"	)
      (define-iso-tex "¸" "{\\c{}}"	"\\c"		)
      (define-iso-tex "¹" "\\SuperOne " "\\SuperOne"	) ; usually undefined
      (define-iso-tex "º" "{\\OImasc}"	"\\OImasc"	) ; usually undefined
      (define-iso-tex "»" "\\gg "	"\\gg"		) ; math mode only
      (define-iso-tex "¼" "\\OneQuater " "\\OneQuater"	) ; usually undefined
      (define-iso-tex "½" "\\OneHalf "	"\\OneHalf"	) ; usually undefined
      (define-iso-tex "¾" "\\ThreeQuaters " "\\ThreeQuaters" ) ; usually undef
      (define-iso-tex "¿" "?`"		"?`"		)
      (define-iso-tex "À" "{\\`A}"	"\\`A"		)
      (define-iso-tex "Á" "{\\'A}"	"\\'A"		)
      (define-iso-tex "Â" "{\\^A}"	"\\^A"		)
      (define-iso-tex "Ã" "{\\~A}"	"\\~A"		)
      (define-iso-tex "Ä" "{\\\"A}"	"\\\"A" 	) ; "\"A" german.sty
      (define-iso-tex "Å" "{\\AA}"	"\\AA"		)
      (define-iso-tex "Æ" "{\\AE}"	"\\AE"		)
      (define-iso-tex "Ç" "{\\c{C}}"	"\\c{C}"	)
      (define-iso-tex "È" "{\\`E}"	"\\`E"		)
      (define-iso-tex "É" "{\\'E}"	"\\'E"		)
      (define-iso-tex "Ê" "{\\^E}"	"\\^E"		)
      (define-iso-tex "Ë" "{\\\"E}"	"\\\"E" 	) ; "\"E" german.sty
      (define-iso-tex "Ì" "{\\`I}"	"\\`I"		)
      (define-iso-tex "Í" "{\\'I}"	"\\'I"		)
      (define-iso-tex "Î" "{\\^I}"	"\\^I"		)
      (define-iso-tex "Ï" "{\\\"I}"	"\\\"I" 	) ; "\"I" german.sty
      (define-iso-tex "Ğ" "{\\Dstroke}"	"\\Dstroke"	) ; usually undefined
      (define-iso-tex "Ñ" "{\\~N}"	"\\~N"		)
      (define-iso-tex "Ò" "{\\`O}"	"\\`O"		)
      (define-iso-tex "Ó" "{\\'O}"	"\\'O"		)
      (define-iso-tex "Ô" "{\\^O}"	"\\^O"		)
      (define-iso-tex "Õ" "{\\~O}"	"\\~O"		)
      (define-iso-tex "Ö" "{\\\"O}"	"\\\"O" 	) ; "\"O" german.sty
      (define-iso-tex "×" "\\times "	"\\times"	)
      (define-iso-tex "Ø" "{\\O}"	"\\O"		)
      (define-iso-tex "Ù" "{\\`U}"	"\\`U"		)
      (define-iso-tex "Ú" "{\\'U}"	"\\'U"		)
      (define-iso-tex "Û" "{\\^U}"	"\\^U"		)
      (define-iso-tex "Ü" "{\\\"U}"	"\\\"U" 	) ; "\"U" german.sty
      (define-iso-tex "İ" "{\\'Y}"	"\\'Y"		)
      (define-iso-tex "Ş" "{\\Thorn}"	"\\Thorn"	) ; usually undefined
      (define-iso-tex "ß" "{\\ss}"	"\\ss"  "\\3"	) ; "\"s" german.sty
      (define-iso-tex "à" "{\\`a}"	"\\`a"		)
      (define-iso-tex "á" "{\\'a}"	"\\'a"		)
      (define-iso-tex "â" "{\\^a}"	"\\^a"		)
      (define-iso-tex "ã" "{\\~a}"	"\\~a"		)
      (define-iso-tex "ä" "{\\\"a}"	"\\\"a" 	) ; "\"a" german.sty
      (define-iso-tex "å" "{\\aa}"	"\\aa"		)
      (define-iso-tex "æ" "{\\ae}"	"\\ae"		)
      (define-iso-tex "ç" "{\\c{c}}"	"\\c{c}"	)
      (define-iso-tex "è" "{\\`e}"	"\\`e"		)
      (define-iso-tex "é" "{\\'e}"	"\\'e"		)
      (define-iso-tex "ê" "{\\^e}"	"\\^e"		)
      (define-iso-tex "ë" "{\\\"e}"	"\\\"e" 	) ; "\"e" german.sty
      (define-iso-tex "ì" "{\\`\\i}"	"\\`\\i"	)
      (define-iso-tex "í" "{\\'\\i}"	"\\'\\i"	)
      (define-iso-tex "î" "{\\^\\i}"	"\\^\\i"	)
      (define-iso-tex "ï" "{\\\"\\i}"	"\\\"\\i" 	) ; "\"i" german.sty
      (define-iso-tex "ğ" "{\\dstroke}"	"\\dstroke"	) ; usually undefined
      (define-iso-tex "ñ" "{\\~n}"	"\\~n"		)
      (define-iso-tex "ò" "{\\`o}"	"\\`o"		)
      (define-iso-tex "ó" "{\\'o}"	"\\'o"		)
      (define-iso-tex "ô" "{\\^o}"	"\\^o"		)
      (define-iso-tex "õ" "{\\~o}"	"\\~o"		)
      (define-iso-tex "ö" "{\\\"o}"	"\\\"o" 	) ; "\"o" german.sty
      (define-iso-tex "÷" "{\\div}"	"\\div"		)
      (define-iso-tex "ø" "{\\o}"	"\\o"		)
      (define-iso-tex "ù" "{\\`u}"	"\\`u"		)
      (define-iso-tex "ú" "{\\'u}"	"\\'u"		)
      (define-iso-tex "û" "{\\^u}"	"\\^u"		)
      (define-iso-tex "ü" "{\\\"u}"	"\\\"u" 	) ; "\"u" german.sty
      (define-iso-tex "ı" "{\\'y}"	"\\'y"		)
      (define-iso-tex "ş" "{\\thorn}"	"\\thorn"	) ; usually undefined
      (define-iso-tex "ÿ" "{\\\"y}"	"\\\"y"		)
))


;;;----------------------------------------------------------------------------
;;; Definition of the minor mode

(defun iso-tex-minor-mode (&optional arg)
  "Minor mode to translate TeX sequences into ISO 8859/1 characters while 
visiting a file.
Provisions are made to translate them back when writing."
  (interactive)

  (if (null iso-tex-minor-mode-initialized)
    (progn
      (setq iso-tex-minor-mode-initialized t)
      (add-hook 'write-contents-hooks 'iso-tex-write)
      ;; 
      (make-local-variable 'after-save-hook)
      (add-hook 'after-save-hook 'iso-tex-after-write)
      ;; There seem to be two versions of this hook around
      ;; Horrible to use such undocumented features :-) 
      (make-local-variable 'after-save-hooks)
      (add-hook 'after-save-hooks 'iso-tex-after-write)
      ;;
      (make-local-variable 'iso-tex-minor-mode)
    )
  )
  (setq iso-tex-minor-mode
	(if (null arg) (not iso-tex-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if iso-tex-minor-mode (tex-2-iso) (iso-2-tex) )
)

(defun iso-tex-write ()
  "Function anchored in the local-write-file-hooks. It is not removed but 
disabled with the iso-tex-minor-mode."
  (if iso-tex-minor-mode (iso-2-tex))
)

(defun iso-tex-after-write ()
  "Function anchored in the after-save-hooks. It is not removed but 
disabled whith the iso-tex-minor-mode."
  (if iso-tex-minor-mode (tex-2-iso))
)

(defun iso-2-tex ()
  "Translate ISO-8859/1 extended characters into TeX sequences.
The variable iso-2-tex-regex is used to preselect a character which is then
translated using the variable iso-2-tex-alist.
Use the function define-iso-tex instead of setting iso-2-tex-alist."

  (let ((buffer-read-only nil) 
	(state (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp iso-2-tex-regex (point-max) t)
	(let ((new (assq (string-to-char 
			   (buffer-substring 
			    (- (point) 1) 
			    (point))) 
			  iso-2-tex-alist)))
	  (if new (progn (delete-backward-char 1)
			 (insert (cdr new)) )
	  )
	)
      )
    ) 
    (set-buffer-modified-p state)
  ) 
  nil
)

(defun tex-2-iso ()
  "Translate TeX sequences into ISO-8859/1 extended characters.
The variable tex-2-iso-regex is used to preselect a character which is then
translated using the variable tex-2-iso-alist.
Use the function define-iso-tex instead of setting tex-2-iso-alist."

  (let ((buffer-read-only nil) 
	(state (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp tex-2-iso-regex (point-max) t)
	(let ((hit (buffer-substring (match-beginning 0) (match-end 0)))
	      (b (match-beginning 0))
	      (e (match-end 0)) )
					; Apply some simplifications
					; to reduce the numer of
					; entries in the alist
	  (cond
	    ( (string-match "\\(\\\\['`^\"]\\\\i\\)\\({}\\|[ \t]+\\)" hit )
	      (setq hit (substring hit (match-beginning 1) (match-end 1)))
	    )
	    ( (string-match "\\(\\\\[a-zA-Z0-9'`^\"]*\\)\\({}\\|[ \t]+\\)" hit)
	      (setq hit (substring hit (match-beginning 1) (match-end 1)))
	    )
	    ( (string-match "{\\(\\\\.*\\)}" hit )
	      (setq hit (substring hit (match-beginning 1) (match-end 1)))
	    )
	    ( (string-match "{\\(\\\\.{.}\\)}" hit )
	      (setq hit (substring hit (match-beginning 1) (match-end 1)))
	    )
	    ( (string-match "\\(\\\\['`^\"]\\){\\(.\\)}" hit )
	      (setq hit (concat 
			 (substring hit (match-beginning 1) (match-end 1))
			 (substring hit (match-beginning 2) (match-end 2))))
	    )
	  )
	  (setq hit (assoc hit tex-2-iso-alist))
	  (if hit
	      (progn
		(delete-region b e)
		(insert (cdr hit))
	      )
	  )
	)
      )
    )
    (set-buffer-modified-p state)
  )
  nil
)

;  ¡¢£¤¥¦§
; ¨©ª«¬­®¯
; °±²³´µ¶·
; ¸¹º»¼½¾¿
; ÀÁÂÃÄÅÆÇ
; ÈÉÊËÌÍÎÏ
; ĞÑÒÓÔÕÖ×
; ØÙÚÛÜİŞß
; àáâãäåæç
; èéêëìíîï
; ğñòóôõö÷
; øùúûüışÿ
