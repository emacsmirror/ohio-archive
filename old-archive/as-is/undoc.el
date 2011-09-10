;;;Date: 1 Oct 87 16:10:50 GMT
;;;From: Paul Stygar <pitstop!sundc!hadron!inco!pauls@SUN.COM>
;;;Organization: McDonnell Douglas-INCO, McLean, VA
;;;Subject: Re: gnuemacs programmers guide

;;;> Does anyone out there know of a programmers guide for gnuemacs?  After
;;;> reviewing some of the source files I can do some minimal coding (being
;;;> proficient in Common Lisp helps) but there are some things that would be
;;;> difficult to know by just reading the code.  Even a summary of all of the
;;;> forms would help (yes, I have looked at the documentation string but even
;;;> that isn't enough).  Well, how about it?

;;;Have you looked at the file /usr/local/emacs/etc/DOC ?  It has over 200KB.
;;;Apparently it is what is used by the C-h f (describe-function) and the 
;;;C-h v (describe-variable),  so it's relatively complete.
;;;It's fairly close to a "summary of all the forms". 

;;;Some cut&paste would be needed to put DOC into a outline suitable for
;;;a programmer's manual.  About 80% of the material is organized: ie.,
;;;logically related items often appear in sequence.  For example, to find
;;;all the info on "mark",  there is no index,  so one does string searches,
;;;and there are several clusters of info on "mark" scattered throughout.
;;;Think of it as a gold mine.  Emacs provides all the tools needed to search
;;;and retrieve the various treasures and potions et al.

;;;The files TO_DO and TO-DO have RMS's wish list for GnuEmacs,  and LISP
;;;Manual is definitely on the wish list.

;;;Since DOC uses ^_F and ^_V to indicate the beginning of a describe-function
;;;and a describe-variable entry respectively,  it was a useful exercise to
;;;write some Gnu Lisp code to scan DOC and insert CR CR TAB in place of each
;;;entry and wrap parentheses around each function:

(defun undoc () " Reformat DOC into a Forms Summary:
	Convert ^_Fstring to CR CR TAB (string) CR
	Convert ^_Vstring to CR CR TAB string CR "
 (while (not (eobp))
  (cond ((= (following-char) ?\C-_ )  
         (delete-char 1)
         (newline 2)
         (insert ?\t) 
         (cond ((= (following-char) ?V)
		(delete-char 1)
		(end-of-line))
               ((= (following-char) ?F)
		(delete-char 1)
                (insert ?( )
                (end-of-line)
                (insert ?) )))
         (newline)))
  (forward-char)))

;;;To try this code,  put "undoc" in a file "undoc.el" in your work directory,
;;;copy DOC to your directory, add the following to .emacs in your home
;;;directory:

;;;     (setq load-path (cons "/your-work-pwd" load-path))

;;;Then just do "emacs DOC", M-ESC (load "undoc"), M-ESC (undoc) and wait ...
;;;You can C-G to stop (undoc) at any point,  then just M-ESC (undoc) to resume.

;;;The result:  a do-it-yourself Forms Summary.    Enjoy!


;;;-- 
;;; |~	\^/		 |~		    |~
;;;o|     /O.o\	 |~	o|	  |~	   o|		    Paul Stygar**
;;;	( )	o|		 o|			 (703) 442-7960**
;;;	_!_			     ...!seismo!sundc!hadron!inco!pauls**
;;;**McDonnell Douglas-Inco, Inc., 8201 Greensboro Drive, McLean, VA 22102**
;;;**BEGIN*DISCLAIMER;*****The*views*expressed*above*in*no*way*reflect*the**
;;;**views*of*McDonnell*Douglas*or*its*subsidiaries.*******END*DISCLAIMER;**
