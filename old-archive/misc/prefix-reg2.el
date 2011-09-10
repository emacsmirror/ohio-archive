;From: erlkonig@walt.cc.utexas.edu (Christopher North-Keys)
;Newsgroups: comp.emacs
;Subject: prefix-region command (rmail) update
;Message-ID: <16516@ut-emx.UUCP>
;Date: 3 Aug 89 00:57:14 GMT
;Reply-To: erlkonig@walt.cc.utexas.edu (Christopher North-Keys)
;Organization: Packaging/Interconnect, M.C.C.
;Lines: 32
;Keywords: prefix-region

;; Prefix region (for rmail, in particular) -- simple & robust.
;; Christopher North-Keys, 1989
(defun prefix-region (start end string)
  "Insert STRING, default '> ', at the start of each line
in or intersecting region while preserving indentation.
Called from a program, takes three arguments,START, END and STRING."
  (interactive "r\nsString:  ")
  (if (or (equal string "") (equal string nil))
      (setq string "> "))
  ;; Adjust start and end to extremes of
  ;; lines so lines don't get broken.
  (goto-char end)
  (end-of-line)
  (setq end (point))
  (goto-char start)
  (beginning-of-line)
  (setq start (point))
  ;; There is another command, replace-regexp, that did not work well.
  ;; If you narrowed as one would expect, you could not widen to the
  ;; previous narrow.  Saving the old narrow extremes failed, as this
  ;; routine expands the region.  Sadmaking.
  (let (line)
    (setq lines (count-lines start end))
    (while (> lines 0)
      (insert string)
      (search-forward "\n")
      (setq lines (- lines 1))
      )))
;------------------------------------/\----------------------------------------
;Seo:  Harp[@Mcc.Com]               /  \/\ ^*^           Christopher North-Keys
;Tha mi gu trang a'cluich.         /    \ \          Systems Administrator, MCC
;------------------------------------------------------------------------------
