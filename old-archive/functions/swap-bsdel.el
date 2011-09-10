;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!rpi!netserv2!deven Mon Feb 12 10:18:02 1990
;Article 1126 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!rpi!netserv2!deven
;From deven@rpi.edu (Deven T. Corzine)
;Newsgroups: gnu.emacs
;Subject: Re: how to use emacs with 'backspace' as 'delete'
;Message-ID: <DEVEN.90Feb9200209@netserv2.rpi.edu>
;Date: 10 Feb 90 01:01:42 GMT
;References: <7813@chaph.usc.edu> <1990Feb3.183918.17186@eagle.lerc.nasa.gov>
;Organization: Rensselaer Polytechnic Institute, Troy, NY
;Lines: 69
;In-Reply-To: mikef@sarah.lerc.nasa.gov's message of 3 Feb 90 18:39:18 GMT
;
;
;On 3 Feb 90 18:39:18 GMT, mikef@sarah.lerc.nasa.gov (Mike J. Fuller) said:
;
;mikef> I'm posting this because I have seen more than a few requests
;mikef> for this recently.  Just put this in your .emacs file:
;
;mikef> (progn
;mikef>   (setq keyboard-translate-table (make-string 128 0))
;mikef>   (let ((i 0))
;mikef>     (while (< i 128)
;mikef>       (aset keyboard-translate-table i i)
;mikef>       (setq i (1+ i)))))
;mikef> (aset keyboard-translate-table ?\^? ?\^h)
;mikef> (aset keyboard-translate-table ?\^h ?\^?)
;
;This seems to be the solution everyone has posted.  I wrote a somewhat
;more extensive function, which is intended to work even if
;keyboard-translate-table already exists, and it swaps the real
;bindings of backspace and delete, not hardcoding the real values.
;Also, the function can be called interactively to swap the keys at any
;time, and can be called noninteractively, forcing swapped or unswapped.
;(When forcing one or the other mapping, it does use hardcoded bs or
;del chars instead of what's in their positions in the current
;keyboard-translate-table.
;
;Here is the function:

(defun swap-bs-del (&optional arg)
  "Accepts single optional argument.  If no argument, or nil, swaps values
for backspace and delete keys in keybpoard-translate-table, initializing
table if necessary.  If arg is t or an integer greater than 0, forces a
mapping of bs->del and del->bs.  Any other argument forces untranslated
mapping of bs->bs and del->del."
  (interactive)
  ;; initialize keyboard-translate-table
  (if (not keyboard-translate-table)
      (setq keyboard-translate-table ""))
  (let ((char (length keyboard-translate-table)))
    (while (< char 128)
      (setq keyboard-translate-table
            (concat keyboard-translate-table (char-to-string char))
            char (1+ char))))
  ;; check arg
  (if (not arg)
      ;; no arg or nil, swap backspace and delete
      (let ((bs (aref keyboard-translate-table 8))
            (del (aref keyboard-translate-table 127)))
        (aset keyboard-translate-table 8 del)
        (aset keyboard-translate-table 127 bs))
    ;; argument given
    (if (or (eq arg t)
            (and (integerp arg)
                 (> arg 0)))
        ;; t or integer greater than zero, force bs->del, del->bs
        (progn
          (aset keyboard-translate-table 127 8)
          (aset keyboard-translate-table 8 127))
      ;; other argument, force untranslated bs->bs, del->del
      (aset keyboard-translate-table 8 8)
      (aset keyboard-translate-table 127 127))))

;Enjoy!
;
;Deven
;-- 
;Deven T. Corzine        Internet:  deven@rpi.edu, shadow@pawl.rpi.edu
;Snail:  2151 12th St. Apt. 4, Troy, NY 12180   Phone:  (518) 274-0327
;Bitnet:  deven@rpitsmts, userfxb6@rpitsmts     UUCP:  uunet!rpi!deven
;Simple things should be simple and complex things should be possible.


