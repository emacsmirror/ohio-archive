;From: tom@ssd.harris.com (Tom Horsley)
;Newsgroups: comp.emacs
;Subject: Formatting with hanging indents
;Message-ID: <TOM.89Sep8072759@hcx2.ssd.harris.com>
;Date: 8 Sep 89 11:27:59 GMT
;Organization: Harris Computer Systems Division
;Lines: 194
;
;Someone was asking about formatting paragraphs with hanging indents. This is
;a package I wrote which I now use to do all my paragraph filling.  It uses
;several empirical rules to recognize a "paragraph" which seem to work well
;in practice, at least for the sorts of things I am always typing.
;
;For instance:
;
;   /* This is a sample C comment to be formatted by paraform
;    * this version is
;    * very ragged, but it will come out nicely filled
;    * once formatted.
;    */
;
;1) This is another sort of
;   paragraph, which can be filled by paraform with no mode
;   changes or variable settings required
;   since filling
;   the C comment.
;
;becomes:
;
;   /* This is a sample C comment to be formatted by paraform this version is
;    * very ragged, but it will come out nicely filled once formatted.
;    */
;
;1) This is another sort of paragraph, which can be filled by paraform with
;   no mode changes or variable settings required since filling the C
;   comment.
;
;The requirement for paraform to figure out what goes on is that in both of
;the above examples, the cursor was sitting on the 'T' in the first word
;(This) in each paragraph when it was filled.
;
;I replaced the standard fill paragraph key binding with this, but people who
;normally fill paragraphs with the cursor sitting at the end may not want to
;do that, paraform only works with the cursor on the first character of the
;paragraph, also it cannot work at all if you want paragraphs with the first
;line indented.
;
;-------------------cut here for paraform.el------------------------------
;; This is a paragraph formatting command  that combines many elements
;; of the standard paragraph formatting routines in a  more convenient
;; to use  functionality than the  standard fill-paragraph  command of
;; emacs -   this is  NOT  a functional  replacement  -    it  behaves
;; differently.
;;
;; The  primary  improvement is in the   area of formatting  text with
;; hanging indents. You MUST put the  cursor on the first character of
;; the paragraph. If  there is text to the  left of the cursor, it  is
;; assumed to be the paragraph label. It  searches down from the first
;; character for a line  that does not  look  like it  belongs  to the
;; paragraph.  The criteria are:
;;
;;   1) The end of the buffer is hit.
;;   2) The line is not long enough.
;;   3) The line has white space in the cursor column.
;;   4) The prefix is not identical to  the prefix of  the second line
;;      (for lines beyond the second line).
;;
;; The area defined  by this  match criteria is  formatted.  The first
;; line  remains prefixed  by  the  first  line prefix, any additional
;; lines are prefixed by the second line prefix.
;;
;; With a prefix arg it will right justify the text.

(defun paraform (prf)
"Format  a paragraph  starting at point  the cursor is on.  Anything to
the left of the cursor  is interpreted as  a prefix for line 1  of the
paragraph.  The paragraph ends at the first  line following the cursor
line  which contains  white space  in the cursor  column,  or does not
match  the   previous lines. Any prefix   on  the  second line  of the
paragraph is used as a fill prefix  for all remaining lines.  A prefix
argument right justifies  the   paragraph.  The variable   fill-column
determines where the right margin is."
   (interactive "P")
   (let
      (
         (start-pos (point))
         (start-col (current-column))
         (orig-fill-column fill-column)
         (orig-fill-prefix fill-prefix)
         (first-line-prefix
            (buffer-substring
               (save-excursion (beginning-of-line 1) (point))
               (point)
            )
         )
         (second-line-prefix nil)
         (delete-final-newline nil)
         start-marker
         end-marker
      )
      ; Make sure this file has a final newline (otherwise while
      ; loop below might infinite loop).
      (if (/= (char-after (1- (point-max))) ?\n)
         (save-excursion
            (goto-char (point-max))
            (insert "\n")
            (setq delete-final-newline t)
         )
      )
      ; Insert a blank line above this so it looks like start
      ; of paragraph. Record the newline position in start-marker.
      (save-excursion
         (beginning-of-line 1)
         (insert "\n")
         (backward-char 1)
         (setq start-marker (point-marker))
      )
      ; Delete the first line prefix
      (delete-region (save-excursion (beginning-of-line 1) (point)) (point))
      ; This loop moves to first line after paragraph
      (while
         (and (equal (forward-line 1) 0)
              (equal (move-to-column start-col) start-col)
              (looking-at "[^ \t\n]")
              (or (not second-line-prefix)
                  (equal second-line-prefix
                     (buffer-substring
                        (save-excursion (beginning-of-line 1) (point))
                        (point)
                     )
                  )
              )
         )
         ; remember second line prefix
         (if second-line-prefix
            nil
            (setq second-line-prefix
               (buffer-substring
                  (save-excursion
                     (beginning-of-line 1)
                     (point)
                  )
                  (point)
               )
            )
         )
         ; delete prefix of each line in paragraph
         (delete-region
            (save-excursion (beginning-of-line 1) (point))
            (point)
         )
      )
      ; get a marker to blank line at end of paragraph
      (beginning-of-line 1)
      (insert "\n")
      (backward-char 1)
      (setq end-marker (point-marker))
      ; go fill the paragraph
      (goto-char (marker-position start-marker))
      (forward-line 1)
      (setq fill-column (- fill-column start-col))
      (setq fill-prefix nil)
      (fill-paragraph prf)
      (setq fill-column orig-fill-column)
      (setq fill-prefix orig-fill-prefix)
      ; if only 1 line, use same prefix for all.
      (if second-line-prefix
         nil
         (setq second-line-prefix first-line-prefix)
      )
      ; Insert first line prefix at first line and
      ; second line prefix in all other lines.
      (goto-char (marker-position start-marker))
      (set-marker start-marker nil)
      (delete-char 1)
      (insert first-line-prefix)
      (while
         (and (equal (forward-line 1) 0)
              (< (point) (marker-position end-marker))
         )
         (insert second-line-prefix)
      )
      (set-marker end-marker nil)
      (delete-char 1)
      ; Leave buffer terminated the way we found it.
      (if delete-final-newline
         (save-excursion
            (goto-char (point-max))
            (backward-delete-char 1 nil)
         )
      )
      ; go back to the beginning of the paragraph
      (goto-char start-pos)
   )
)
;-------------------cut here, end of paraform.el------------------------------
;--
;=====================================================================
;    usenet: tahorsley@ssd.harris.com  USMail: Tom Horsley
;compuserve: 76505,364                         511 Kingbird Circle
;     genie: T.HORSLEY                         Delray Beach, FL  33444
;======================== Aging: Just say no! ========================
