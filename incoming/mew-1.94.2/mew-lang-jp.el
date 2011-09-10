;;; mew-lang-jp.el --- Japanese specific stuff for Mew

;; Author:  Mito <mit@nines.nec.co.jp>
;;          Kazu Yamamoto <Kazu@Mew.org>
;; Created: Sep 08, 1997
;; Revised: Aug 24, 1998

;;; Code:

(defconst mew-lang-jp-version "mew-lang-jp.el version 0.02")

;; from jisx0201.el
(defvar mew-katakana-alist
  '(( 161 . "(I'(B" )
    ( 162 . "(I1(B" )
    ( 163 . "(I((B" )
    ( 164 . "(I2(B" )
    ( 165 . "(I)(B" )
    ( 166 . "(I3(B" )
    ( 167 . "(I*(B" )
    ( 168 . "(I4(B" )
    ( 169 . "(I+(B" )
    ( 170 . "(I5(B" )
    ( 171 . "(I6(B" )
    ( 172 . "(I6^(B" )
    ( 173 . "(I7(B" )
    ( 174 . "(I7^(B" )
    ( 175 . "(I8(B" )
    ( 176 . "(I8^(B" )
    ( 177 . "(I9(B" )
    ( 178 . "(I9^(B" )
    ( 179 . "(I:(B" )
    ( 180 . "(I:^(B" )
    ( 181 . "(I;(B" )
    ( 182 . "(I;^(B" )
    ( 183 . "(I<(B" )
    ( 184 . "(I<^(B" )
    ( 185 . "(I=(B" )
    ( 186 . "(I=^(B" )
    ( 187 . "(I>(B" )
    ( 188 . "(I>^(B" )
    ( 189 . "(I?(B" )
    ( 190 . "(I?^(B" )
    ( 191 . "(I@(B" )
    ( 192 . "(I@^(B" )
    ( 193 . "(IA(B" )
    ( 194 . "(IA^(B" )
    ( 195 . "(I/(B" )
    ( 196 . "(IB(B" )
    ( 197 . "(IB^(B" )
    ( 198 . "(IC(B" )
    ( 199 . "(IC^(B" )
    ( 200 . "(ID(B" )
    ( 201 . "(ID^(B" )
    ( 202 . "(IE(B" )
    ( 203 . "(IF(B" )
    ( 204 . "(IG(B" )
    ( 205 . "(IH(B" )
    ( 206 . "(II(B" )
    ( 207 . "(IJ(B" )
    ( 208 . "(IJ^(B" )
    ( 209 . "(IJ_(B" )
    ( 210 . "(IK(B" )
    ( 211 . "(IK^(B" )
    ( 212 . "(IK_(B" )
    ( 213 . "(IL(B" )
    ( 214 . "(IL^(B" )
    ( 215 . "(IL_(B" )
    ( 216 . "(IM(B" )
    ( 217 . "(IM^(B" )
    ( 218 . "(IM_(B" )
    ( 219 . "(IN(B" )
    ( 220 . "(IN^(B" )
    ( 221 . "(IN_(B" )
    ( 222 . "(IO(B" )
    ( 223 . "(IP(B" )
    ( 224 . "(IQ(B" )
    ( 225 . "(IR(B" )
    ( 226 . "(IS(B" )
    ( 227 . "(I,(B" )
    ( 228 . "(IT(B" )
    ( 229 . "(I-(B" )
    ( 230 . "(IU(B" )
    ( 231 . "(I.(B" )
    ( 232 . "(IV(B" )
    ( 233 . "(IW(B" )
    ( 234 . "(IX(B" )
    ( 235 . "(IY(B" )
    ( 236 . "(IZ(B" )
    ( 237 . "(I[(B" )
    ( 239 . "(I\(B" ) ; (I\(B -> $B%o(B $B$KJQ49$9$k$h$&$K(B
    ( 238 . "(I\(B" ) ; $B%o$H%n$N=gHV$,8r49$7$F$"$k!#(B
    ( 240 . "(I((B" )
    ( 241 . "(I*(B" )
    ( 242 . "(I&(B" )
    ( 243 . "(I](B" )
    ( 244 . "(I3^(B" )
    ( 245 . "(I6(B" )
    ( 246 . "(I9(B" )))

(defvar mew-katakana-kigou-alist
  '(( 162 . "(I$(B" )
    ( 163 . "(I!(B" )
    ( 166 . "(I%(B" )
    ( 171 . "(I^(B" )
    ( 172 . "(I_(B" )
    ( 188 . "(I0(B" )
    ( 214 . "(I"(B" )
    ( 215 . "(I#(B" )))

(defvar mew-dakuon-list
  '( ?$B%+(B ?$B%-(B ?$B%/(B ?$B%1(B ?$B%3(B
     ?$B%5(B ?$B%7(B ?$B%9(B ?$B%;(B ?$B%=(B
     ?$B%?(B ?$B%A(B ?$B%D(B ?$B%F(B ?$B%H(B
     ?$B%O(B ?$B%R(B ?$B%U(B ?$B%X(B ?$B%[(B))

(defvar mew-handakuon-list (memq ?$B%O(B mew-dakuon-list))

(defun mew-search-henkan-alist (ch list)
  (let ((ptr list)
	(result nil))
    (while ptr
      (if (string= ch (cdr (car ptr)))
	  (progn
	    (setq result (car (car ptr)))
	    (setq ptr nil))
	(setq ptr (cdr ptr))))
    result))

(defun mew-zenkaku-katakana-region (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\ck" nil t)
	(let ((ch (preceding-char))
	      (wk nil))
	  (cond
	   ((= ch ?(I^(B)
	    (save-excursion
	      (backward-char 1)
	      (setq wk (preceding-char)))
	    (cond ((= wk ?$B%&(B)
		   (delete-char -2)
		   (insert "$B%t(B"))
		  ((setq wk (memq wk mew-dakuon-list))
		   (delete-char -2)
		   (insert (1+ (car wk))))
		  (t
		   (delete-char -1)
		   (insert "$B!+(B"))))
	   ((= ch ?(I_(B)
	    (save-excursion
	      (backward-char 1)
	      (setq wk (preceding-char)))
	    (if (setq wk (memq wk mew-handakuon-list))
		(progn
		  (delete-char -2)
		  (insert (+ 2 (car wk))))
	      (progn
		(delete-char -1)
		(insert "$B!,(B"))))
	   ((setq wk (mew-search-henkan-alist
		      (char-to-string ch) mew-katakana-alist))
	    (progn
	      (delete-char -1)
	      (if mew-xemacs-p
		  (insert (mew-make-char mew-lc-jp 37 (- wk 128)))
		(insert (mew-make-char mew-lc-jp ?\245 wk)))))
	   ((setq wk (mew-search-henkan-alist
		      (char-to-string ch) mew-katakana-kigou-alist))
	    (progn
	      (delete-char -1)
	      (if mew-xemacs-p
		  (insert (mew-make-char mew-lc-jp 33 (- wk 128)))
		(insert (mew-make-char mew-lc-jp ?\241 wk)))))))))))

(provide 'mew-lang-jp)

;;; Copyright Notice:

;; Copyright (C) 1997, 1998 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-lang-jp.el ends here
