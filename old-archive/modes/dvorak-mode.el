;From ark1!uakari.primate.wisc.edu!uflorida!mephisto!rutgers!njin!princeton!phoenix!tbrakitz Tue May  8 14:22:38 1990
;Article 1883 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!uflorida!mephisto!rutgers!njin!princeton!phoenix!tbrakitz
;>From tbrakitz@phoenix.Princeton.EDU (Byron Rakitzis)
;Newsgroups: comp.emacs
;Subject: Dvorak keyboard minor mode.
;Message-ID: <15836@phoenix.Princeton.EDU>
;Date: 30 Apr 90 20:50:40 GMT
;Organization: Princeton University, NJ
;Lines: 110
;
;Here's a minor mode for a Dvorak keyboard I hacked up last night.
;Control keys are mapped as QWERTY, since there is no way to get C-.
;for example, to send something reasonable. 
;
;Enjoy. (notice that C-h f dvorak-mode will give a nice view of the
;Dvorak keyboard layout)
;
;-------------------------------CUT HERE----------------------------------


(defun dvorak-mode nil "Dvorak keyboard mode:
-------------------------------------------------------------------------
| Esc| 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 0  | [  | ]  |  <-  |
-------------------------------------------------------------------------
| Tab | /  | ,  | .  | p  | y  | f  | g  | c  | r  | l  | ;  | =  |     |
------------------------------------------------------------------- |   |
| Ctrl | a  | o  | e  | u  | i  | d  | h  | t  | n  | s  | -  |   <-    |
-------------------------------------------------------------------------
| Shift  | '  | q  | j  | k  | x  | b  | m  | w  | v  | z  | Shift |
---------------------------------------------------------------------
"
  (interactive)
  (if (boundp 'dvorak-mode)
      nil
    (setq minor-mode-alist (cons '(dvorak-mode " Dvorak") minor-mode-alist))
    (make-variable-buffer-local 'dvorak-mode)
    (set-default 'dvorak-mode nil))
  (setq dvorak-mode (not dvorak-mode))
  (use-local-map (if dvorak-mode dvorak-mode-map nil)))
(defvar dvorak-mode-map nil)
(if dvorak-mode-map nil
  (setq dvorak-mode-map (make-keymap))
  (define-key dvorak-mode-map "q" "/")
  (define-key dvorak-mode-map "w" ",")
  (define-key dvorak-mode-map "e" ".")
  (define-key dvorak-mode-map "r" "p")
  (define-key dvorak-mode-map "t" "y")
  (define-key dvorak-mode-map "y" "f")
  (define-key dvorak-mode-map "u" "g")
  (define-key dvorak-mode-map "i" "c")
  (define-key dvorak-mode-map "o" "r")
  (define-key dvorak-mode-map "p" "l")
  (define-key dvorak-mode-map "[" ";")
  (define-key dvorak-mode-map "]" "=")
  (define-key dvorak-mode-map "a" "a")
  (define-key dvorak-mode-map "s" "o")
  (define-key dvorak-mode-map "d" "e")
  (define-key dvorak-mode-map "f" "u")
  (define-key dvorak-mode-map "g" "i")
  (define-key dvorak-mode-map "h" "d")
  (define-key dvorak-mode-map "j" "h")
  (define-key dvorak-mode-map "k" "t")
  (define-key dvorak-mode-map "l" "n")
  (define-key dvorak-mode-map ";" "s")
  (define-key dvorak-mode-map "'" "-")
  (define-key dvorak-mode-map "z" "'")
  (define-key dvorak-mode-map "x" "q")
  (define-key dvorak-mode-map "c" "j")
  (define-key dvorak-mode-map "v" "k")
  (define-key dvorak-mode-map "b" "x")
  (define-key dvorak-mode-map "n" "b")
  (define-key dvorak-mode-map "m" "m")
  (define-key dvorak-mode-map "," "w")
  (define-key dvorak-mode-map "." "v")
  (define-key dvorak-mode-map "/" "z")
  (define-key dvorak-mode-map "Q" "?")
  (define-key dvorak-mode-map "W" "<")
  (define-key dvorak-mode-map "E" ">")
  (define-key dvorak-mode-map "R" "P")
  (define-key dvorak-mode-map "T" "Y")
  (define-key dvorak-mode-map "Y" "F")
  (define-key dvorak-mode-map "U" "G")
  (define-key dvorak-mode-map "I" "C")
  (define-key dvorak-mode-map "O" "R")
  (define-key dvorak-mode-map "P" "L")
  (define-key dvorak-mode-map "{" ":")
  (define-key dvorak-mode-map "}" "+")
  (define-key dvorak-mode-map "A" "A")
  (define-key dvorak-mode-map "S" "O")
  (define-key dvorak-mode-map "D" "E")
  (define-key dvorak-mode-map "F" "U")
  (define-key dvorak-mode-map "G" "I")
  (define-key dvorak-mode-map "H" "D")
  (define-key dvorak-mode-map "J" "H")
  (define-key dvorak-mode-map "K" "T")
  (define-key dvorak-mode-map "L" "N")
  (define-key dvorak-mode-map ":" "S")
  (define-key dvorak-mode-map "\"" "_")
  (define-key dvorak-mode-map "X" "Q")
  (define-key dvorak-mode-map "C" "J")
  (define-key dvorak-mode-map "V" "K")
  (define-key dvorak-mode-map "B" "X")
  (define-key dvorak-mode-map "N" "B")
  (define-key dvorak-mode-map "M" "M")
  (define-key dvorak-mode-map "<" "W")
  (define-key dvorak-mode-map ">" "V")
  (define-key dvorak-mode-map "?" "Z")
  (define-key dvorak-mode-map "-" "[")
  (define-key dvorak-mode-map "=" "]")
  (define-key dvorak-mode-map "_" "{")
  (define-key dvorak-mode-map "+" "}")
  (define-key dvorak-mode-map "Z" "\""))



;-- 
;Just try taking your VAX down to Jiffy-Lube these days!
;			
;Byron "Bo knows parallel computational geometry" Rakitzis. 
;				(tbrakitz@phoenix.Princeton.EDU)
