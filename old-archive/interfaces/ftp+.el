; -*- lisp-interaction -*-

;; new feature: auto-describe (hard-wired on).
;; - automatically shows the full description of the site in a
;;   "reasonably sized window".  Note that this eats a minimum of two
;;   lines off your screen, and this works best if the description
;;   window is the bottom window of a two window configuration, which
;;   is what you get automatically if you invoke ftp+ with only a
;;   single window...
;; - the '0' key, which used to be for bringing up the description
;;   will now scroll the description window (for very long
;;   descriptions).
;; - If you have a a tiny window, and want to pop up a bigger window
;;   using some mechanism which isn't aware of this window resizing
;;   business, the best alternative is to preceed that command with
;;   Ctrl-X 1 (which wipes out all memory of other window sizes).
;;   I've got a better solution in mind, but it will take a little
;;   playing around to get working...
;; - some of the comments in this package still reflect the old style
;;   of doing things.  Raul D. Miller-Rockwell 1992 Feb 10

;;
;; Site Additions:
;; 
;; - PCN parallel language at info.mcs...
;; - chinese chess from cs.alberta...
;; - Database of Distributed Operating Systems from titania...
;; - statlib statistical analysis package from lib.stat...
;; - public domain scientific software archive at netlib...
;; - SIMD simulator at mcnc.mcnc....
;;
;; -> send me the sites you know of!!!
;;

;; LCD Archive Entry:
;; ftp+|Terrence Brannon|brannon@jove.cs.caltech.edu
;; |FTP interface with database of ftpable software. Multiple connects at once.
;; |92-02-20|0.6|~/interfaces/ftp+.el.Z|

;; this program is available for anonymous ftp from
;; archive.cis.ohio-state.edu in the directory
;; pub/gnu/emacs/elisp-archive/interfaces 

					;
					; major code
					; segment:
					;
					; OBJECTIVE
					; 

;; The Objectives of ftp.plus.el

;; 1: provide a quickly accessible database of free software and other files
;;    available via ftp
;; 2: to provide easy use of the FTP commands via 2-keystroke sequence
;; 3: to maintain a mailing of those interested who will receive the updated
;;    program free on a monthly basis
;; 4: to receive a new copy of ftp+ each month with an updated sitelist
;;    email me at this address: tb06@pl118f.cc.lehigh.edu
;; 5: if you know of a package which is not in my sitelist,
;;    PLEASE PLEASE email and I will add it for the next months mailing list
;;    and I will also thank you

					;
					; major code
					; segment:
					;
					; contributions
					;
 
;;; Raul D. Miller-Rockwell 1992 Feb 10
;;    auto-describe (hard-wired on).
;; - automatically shows the full description of the site in a
;;   "reasonably sized window".  Note that this eats a minimum of two
;;   lines off your screen, and this works best if the description
;;   window is the bottom window of a two window configuration, which
;;   is what you get automatically if you invoke ftp+ with only a
;;   single window...
;; - the '0' key, which used to be for bringing up the description
;;   will now scroll the description window (for very long
;;   descriptions).
;; - If you have a a tiny window, and want to pop up a bigger window
;;   using some mechanism which isn't aware of this window resizing
;;   business, the best alternative is to preceed that command with
;;   Ctrl-X 1 (which wipes out all memory of other window sizes).
;;   I've got a better solution in mind, but it will take a little
;;   playing around to get working...
;; - some of the comments in this package still reflect the old style
;;   of doing things.  Raul D. Miller-Rockwell 1992 Feb 10
;;;

		 ;;; -/- many thanks to Tim McDaniel
	    ;;; -\- I was on a roll, just hacking away at
	   ;;; -/- the Elisp, then hit a snag... no member
	    ;;; -\- function?! But then I remembered that
	     ;;; -/- package I snagged off arc-ohio-state
	   ;;; -\- and there was that beautiful list-to-set
	   ;;; -/- function which I gleefully yanked out of
		  ;;; -\- his code, and into my ftp+
			       ;;; =8-))


;;; ???@mit.???? DATE(?) i lost this guys name but he put in some
;;; hooks and i am sorry to disrespect you like this

					; major code
					; segment:
					;
					; to be done
					; 

; o ;  integrate with either hyperbole or wrolo.el ... 
; o ;  BINARY-MODE-ON-CONNECT ... better yet snarf the code from
; ange-ftp.el which figures out whether you should automatically
; switch to binary mod
; o ;  Send/recv files with these options
    ;   send all in directory
    ;   send a list of files
    ;   filename completion
; o ;  Add add-site command so that there will be no
;   ;  mistake when entering new sites
; o ;  HELP must be displayed a different way
; o ;  Move a used site to front of list if POPULAR-SORT is t
; o ;  Record all attempts to connect if LOG-CONNECTION is t
; o ;  Minibuffer not the best choice because of folk wanting to yank things
; o ;  Kill the profanity in the docs
; o ;  Send-file should list all possible files to send from current dir
; o ;  ...or maybe multiple directories
; o ;  setq BINARY-MODE-ON-CONNECT t


					;
					; major code
					; segment:
					;
					; documenatation
					; 


; ; Quick Notes for the Advanced:


;*> Connecting to multiple sites can be confusing after about 4 or 5 connects.
;   To see which buffer is handling which site type M-x buffer-menu. You will
;   see site names in the mode column of the menu.
;   To choose any of these sites, scroll down using C-n and C-p and type
;   f or 1 resume interaction with that transaction

;*> if you cant see all of a comment in a minibuffer, type 0 then q to quit
;   the view mode

;*> type C-h m to see the keys you can use. 
;   also note that the keys you can use vary depending on whether or not
;   you are connected. 7, 8, 9, and 0 are used to scroll through , connect
;   to and get info on sites, not respectively. once you connect, they
;   their local bindings are killed so that they are rebound back to
;   self-insert-command

;*> have you noticed how when you hit TAB-g to get a file, the cursor
;   automatically moves to the end of the buffer? To quickly get back to
;   where you were, type C-x C-x (switches point and mark)

;*> this is one of those things that isnt a bug, but about half of the
;   people will like it as it is and the other half will like it the
;   other way. ok, if you type M-x ftp+ once, but do not connect and then
;   type M-x ftp+ again and this time scroll to a site and choose it, the
;   global variable ftp+current-site will be set to the site of the second
;   invocation. if you C-x o to the other site and hit 8 to connect, it will
;   connect to the same site since it just accesses the same global variable
;   the pro: if you want multiple connects to the same site, you are 
;   automatically at the same site
;   the con: the current-site parameter of the site you C-x o back to is
;   wrong. 
;   the bottom line for right now: it is best to immediately connect to the
;   site you want until i make a buffer-local variable to solve the situtation
;   note i didnt say bug 8^)) heheh
;*> typing TAB-q only kills the current ftp+ session. each individual ftp+
;   session must be killed separately

                  
                                        ;
					; major code segment
                                        ;
                                        ; in-depth documentation
					; [\] if you get lost,
					; {\} email me and tell me where
					; [\] gap in the information flow lies
					; {\} and I will fix the flaw
     ; *> if you want to redo this comprehensively and remove the
     ; profane nature of the docs, that would be terrific!

;*> Running the Program (for those new to Emacs)
;

; If you are absolutely new to Emacs, let me explain some of the
; conventions for anything dealing with Emacs:
;   1 C is short for control if I write C-x that translates to:
;     "hold down the control key while pressing lowercase x"
;   2 M is short for meta which is generally ESC on most keyboards.
;     Similarly M-x means hold down the meta key while pressing lowercase "x"
;   3 RET means hit return. Thus, M-x load-file RET means hold down the
;     meta key while pressing x then type load-file and press return
;   4 When in trouble hit C-g  ... never fails (usually)

; Now with that explained, let me tell you how to run my program. Assuming
; that you have transferred the file to your local disk do the following:
;  1 Use your favorite text editor to remove any text before the line which
;    states ; -*- lisp-interaction -*- 
;  2 Save this in a file named ftp.plus.el
;  3 Exit to shell
;  4 Now go to the same directory in which you save the file ftp.plus.el
;  5 type emacs. now type M-x load-file RET ftp.plus.el RET
;  6 now type M-x ftp+ and use the keys described below:


; use this command after you have connected and are prompted for
; a password
; send password   	... TAB-p   

; use this command after you have connected and want to disconnect
; close connection	... TAB-c or type "close"

; when connected use this to switch between binary and ascii modes
; for the transfer of files... binary is neccesary for files with
; files ending in .Z or .tar or both.
; Binary/Ascii toggle	... TAB-m or type "binary" or "ascii"

; when connected and you want to move up a directory on the remote 
; machine type this
; up 1 dir		... TAB-u or type "cd .." or "cdup"

; to list the current directory
; dir			... TAB-d or type "dir" or "ls -l"

; After using C-p and C-n to move through the files in the directory
; if you see a directory you want to change to or a file you want to
; get, type. NOTE: this will only work on a fill directory listing
; generated by typing TAB-d or dir or ls -l. it will not work is you
; just type ls because ftp+ will not be able to parse the directory
; listing to see if the first letter on the line is a d, indicating
; a directory
; get/cd	  	... TAB-g or type "cd" or "get"

; To send a file across to the remote ftp:
; right now, you must lcd to the directory of the file of specify
; a correct relative or absolute pathname
; send			... TAB-s or type "send"

; To end the session
; quit			... TAB-q or type "quit" or "bye"

; these 3 commands allow completion... after typing the
; indicated letters, just hit TAB immediately, or type
; in as much as you can remember or care type type and the
; let Emacs complete it for you

; choose-site-by-sitename   cs  <type "c" followed immediately by "s">
; choose-site-by-topic  ... ct  <type "c" followed immediately by "t">
; choose-site-by-files-it-has   cf  <type "c" followed immediately by "f">

; ALSO about the above commands... it is best to use completion
; even when you are positive of the name because misspelling will 
; cause the ftp+scroll-to-chosen-site algorithm to loop forever. 
; if this happens, no problem- just type C-g

; choose-site-backward	... 7 note: only works when not connected
; choose-site-forward	... 9 note: only works when not connected
; connect-to-site	... 8 note: only works when not connected
; view-comment-field	... 0 note: only works when not connected

; All local commands such as lcd and !ls should be typed at a command-line
; the very next mod is to make this an interactive shell using
; CMU's cmushell/comint. until then shell commands can only be typed when
; you are at the end of the buffer AND you have a prompt AND there is a
; space between your typed command and the prompt. like so:
; ftp> lcd /scratch/china.primitive
; the above will work. this wont right now:
; ftp>lcd /scratch/china.primitive
; and this wont either:
; ftp> ftp> lcd /scratch/china.primitive



;;; In my opinion, the best way to learn is by doing. Therefore these docs
;;; will take you through a sample run of using ftp+. I am well aware of
;;; the existence of ange-ftp and have great respect for the program
;;; however, I prefer my program for many reasons:

; 0 the ability to search for a site based on a particular topic. if I want
;   to find all sites that have a TeX program, i type c t TeX TAB and they
;   are all listed
; 1 the ability to keep a list of sites in the program. whenever i read
;   usenet and see a site that interests me, i just add the site to the
;   variable ftp+site-list.
; 2 the interactive nature of my program. i hate being in the dark.
;
; 
;*> Scenario One
;   ------------
;;; 
;;; 2 in the morning and amidst candy bar wrappers and empty coke cans we
;;; see Jenny Hacker scanning through the newsgroups on usenet. all of a
;;; sudden up pops a site in ireland that archives the lyrics to celtic
;;; pholk music. jenny does a C-x C-f ftp.plus.el and then adds this record
;;; to ftp+site-list:
;;; 
;;; ("yeolde.defjam.archive" "anonymous" "jennyhack@hacker.central"
;;;  "An archive of all the lyrics to celtic pholk musik since 32 ad. Also
;;; contains a cookbook for Molotav cocktails and other recipes for
;;; hosting your phavorite phriends."
;;; 
;;; Satisfied, Jenny does a C-x C-s. The next day she does an M-x
;;; load-file ftp.plus.el and scrolls (more on that later) through her
;;; list of sites. When she gets to this one (which she has forgotten
;;; about after a day of waiting tables at the roller restaurant) she
;;; glances at the description in the echo area. Unable to see the full
;;; description, she hits 0 to view the full bupher. Bells go off
;;; she connects up to this site and wonders what she would have done if
;;; she had instead scribbled the number down on a scrap of paper...
;;; p.s. She is also pleased to note that the 7 8 9 0 keys wich helped
;;; her scroll thru the sites can now be entered as regular text, which
;;; means she can type in file names which have these characters.

; 
;*> Scenario Two
;   ------------

;;; Back from a 3 year trip to the Belgian Congo, Alf Strauss strips off
;;; his sweaty loincloth and logs into hacker central. After emacs is
;;; totally loaded and he has autoloaded ftp.plus.el he invokes ftp+.
;;; 
;;; Things are coming back now... <YOU FEEL COMPELLED TO FOLLOW ALONG>
;;; 1) He places the index finger of the right hand on the 7. 
;;; 2) places the middle finger on the 8.
;;; 3) places the 4th phinger on the 9.
;;; 4) places the pinky on 0
;;; 
;;; 
;;; Now he's set. He starts scrolling (remember
;;; that word?) through sites by hitting 7 and 9. When he
;;; wants a detailed description of a site that wont fit in the minibuffer
;;; he hits 0. When he wants to connect, he hits 8. After 
;;; connecting and seeing the password prompt on his screen, he hits TAB-p 
;;; then return (or edits the password).
;;; 
;;; He is now logged in. To get a directory, he hits TAB-d. To change to a
;;; directory he does one of two things:
;;; 1) uses C-p and C-n to move up, then hits TAB-g when he is on the same
;;; line as the directory he wants to change to... he can be ANYWHERE on
;;; the line of the directory he wants to change to, ANYWHEREman.
;;; 2) type "cd directory"
;;; 
;;; After plunging deep in the directory, he wants to go up a directory,
;;; again you can have it your way:
;;; 1) type TAB-u
;;; 2) type "cd .."
;;; 
;;; Strauss is now in a directory with a compressed file that he is itchin
;;; to get. BUT he must go to binary mode first. He can:
;;; 1) type TAB-m 
;;; 2) type "binary"
;;; 
;;; to switch back later
;;; 0) type TAB-m
;;; 1) type "ascii"
;;; 
;;; After getting the file, he wants to send one of his own. After
;;; ensuring that the mode is correct and he used the lcd command to
;;; switch to the correct directory, he can:
;;; 0) type TAB-s local-filename
;;; 1) type "send local-filename"
;;; 
;;; After a hard day of FTPing, helga is home from a day of cooking soul
;;; food. Strauss is ready to launch that erotic assault. So he forgets
;;; that he could <type "quit"> and hurriedly hits TAB-q and hustles off to
;;; greet helga bambaata-strauss

; 
;*> Scenario Three
;   --------------

;;; Professor Knucklehead dashes headlong into the computer lab and
;;; frantically logs in. Everyone stares at this frizzy-headed 
;;; labcoat-wearing individual who is mumbling.. need that program NOW!
;;; With not a femtosecond of delay, he cracks off a 
;;; M-x load-file ftp.plus.el then M-x ftp+ and he off to the races:
;;; He next types "c" followed by "f" to <c>hoose a site searching
;;; by <f>ilename. Since he doesnt remember the full name to the
;;; package, he types a few letters and hits tab. Up pops the filename
;;; A deft smash of the return key and another tab lists all the sites
;;; with the file he wants. He chooses the first and wonders what he would have
;;; done without ftp+

;;; After the day is over. Dr. Knucklehead wants a little relaxation and
;;; what could be more fun than using FTP? Not much for this well-rounded
;;; character, so off he goes back to the computer lab.
;;; After logging in, and invoking ftp+, he wants a list of all sites
;;; with the recipes for Jiffy Popcorn. so he types "c" followed by "t"
;;; which is short for <c>hoose site based on <t>opic and types in
;;; his chosen subject. He connects up, pulls in that yummy recipe and
;;; its time for some GOOD EATIN!!

;;; After a popcorn feast, Knucklehead is back to showcase the last 
;;; pheature of his phavorite package. So he invokes ftp+ and this time
;;; he knows just the site he wants. He doesnt want to scroll through the
;;; huge list of sites so he hits "c" followed by "s" which is short for
;;; <c>hoose site based on <s>itename types in the name, or part of the
;;; name if he wants completion, and then hits 8 to connect.



;*> N o t e s
;   -----------------------
;   - topic names may contain punctutation
;;;   using headers is a good way to search for code of a
;;;   certain category
;   - addition of a field to the site-list such as the topic list or file list
;;;   is a simple matter of adding __?_ to functions __?__ if you wish to
;;;   add fields with differing delimiters or formats, then you must write
;;;   the readers and writers for this. i would like to see the resultsof 
;;;   such work.
;   - IMPORTANT: if you decide to add new sites,
;;;   it is not a good idea to leave a null field as "" because
;;;   you wont be able to do read completion on this field by hitting
;;;   tab. you will have to hit space. better yet, as requested above,
;;;   if you know of new and interesting software or files, email me 
;;;   i will add it so that everyone can benefit.


					;				   
					; major code 
					; segment 
					;
					; variable-defines
					;

(defvar ftp+mode-hook 'nil
  "If not nil, this hook will be called when ftp+ starts up. One pretty useful
thing to do would be to use this to set an alternate ftp+site-list")

(defvar ftp+disconnect-hook 'nil
  "If not nil, this hook will be called upon disconnect.")

(setq ftp+status-list '(
(nil nil nil nil) (nil nil nil nil) (nil nil nil nil)
(nil nil nil nil) (nil nil nil nil) (nil nil nil nil)
(nil nil nil nil) (nil nil nil nil) (nil nil nil nil)
(nil nil nil nil) (nil nil nil nil) (nil nil nil nil)
(nil nil nil nil) (nil nil nil nil) (nil nil nil nil)
(nil nil nil nil) (nil nil nil nil) (nil nil nil nil)
(nil nil nil nil) (nil nil nil nil)))

(setq ftp+connected-index 0)
(setq ftp+binary-mode-index 1)

(setq ftp+Interaction-bupha (get-buffer-create "dummy"))

(setq process-objects-vector (make-vector 50 (start-process "ftp+" ftp+Interaction-bupha "ls")))

;; if popular-sort is true, each site that you select, regardless of method,
;; will be moved to the front of the list so that it will be quickly chosen
;; the next time you want it. if it is false, sites will retain their places
;; in the variable ftp+site-list . to be implemented

(setq ftp+list-ctr 0)  ; used iterate thru ftp+site-list

(setq ftp+Instruction-bupha "*ftp+help-buffer*")
(setq ftp+buffer-count -1)

(setq SITENAME-INDEX 0)
(setq USERNAME-INDEX 1)
(setq PASSWORD-INDEX 2)
(setq TOPIC-INDEX 3)
(setq FILENAME-INDEX 4)
(setq COMMENT-INDEX 5)

;; set this list to whatever fields you want shown as you
;; scroll thru the the sitelist with keys 

(setq DISPLAY-FIELDS '(TOPIC-INDEX FILENAME-INDEX))
(setq DISPLAY-CUSTOM t)

(setq ftp+site-list '(
("dsl.cis.upenn.edu" "anonymous" "anonymous" "Gifs" "Gifs" "")


("fionavar.mit.edu" "anonymous" "anonymous" "TeX" "LaTeX styles" "directory of latex styles")
("zurich.ai.mit.edu" "anonymous" "scheme" "Scheme" "Scheme" "") 
("ftp.ai.mit.edu" "anonymous" "scheme" "Women Computer Scientists/Robotics" "Women Computer Scientists/Reactive Robotics" "pub/ellens/womcs*.ps") 
("prep.ai.mit.edu" "anonymous" "china.primitive@divineland" "Gnu" "Gnu Multiple Precision Arithmetic/Gnu Spreadsheet (Oleo)/Gnu Debugger/Gnu Dynamic Loader/Epoch 4.0" "Major Gnu Archive") 
("export.lcs.mit.edu" "anonymous" "superkid@mo.jo.land" "X-Windows/Tex" "?" "x-source-list.Z contains X-win ftp sites. dir: contrib") 
("whitechapel.media.mit.edu" "anonymous" "superkid@mo.jo.land" "TeX/Postscipt Utilities" "psfig" "psfig allows the inclusion of postscript in latex documentsx-source-list.Z contains X-win ftp sites. dir: contrib

2) How can I include a PostScript figure in LaTeX?

   Perhaps the best way to do this is to use the psfig macros written
   by Trevor Darrell. They are available via anonymous ftp from
   whitechapel.media.mit.edu (18.85.0.125) in ./pub/psfig or 
   linc.cis.upenn.edu (130.91.6.8) in the directory ./dist/psfig. You 
   will also need a dvi to PostScript conversion program that supports 
   \specials. The ones mentioned in question 1 do, and the first two
   drivers come with a version of psfig ready to use with them. The psfig 
   macros work best with Encapsulated PostScript Files (EPS). In
   particular, psfig will need the file to have a BoundingBox (see 
   Appendix C of the _PostScript Language Reference Manual_). If you 
   don't have an EPS file, life can be difficult. For people who don't
   have ftp access or can't deal with tar files, the files are also
   available from ymir.claremont.edu (134.173.4.23) in
   [anonymous.tex.graphics.psfig].
") 



("eff.org" "anonymous" "china.primitive@divineland" "Phone Phreaking" "Phrack" "Phrack is in cud") 

("blackbox.hacc.washington.edu" "anonymous" "china.primitive@divineland" "Prose/Ethnic Tex" "Poorman's TeX/Translation of John Trevisa from Latin" "") 
("byron.u.washington.edu" "anonymous" "china.primitive@divineland" "X-Windows/Tex" "?" "") 
("june.cs.washington.edu" "anonymous" "china.primitive@divineland" "X-Windows/Tex" "?" "")
("ftphost.cac.washington.edu" "anonymous" "china.primitive@divineland" "Ethnic Tex" "chinese.tar.Z" "")

("netlib" "" "" "Public Domain Scientific Software" "Public Domain Scientific Software" "
                          ----------------------
                          Introduction to NETLIB
                          ----------------------

                           Frederick W. Chapman
                          Senior User Consultant
                     Lehigh University Computing Center

                         (fc03@ns.cc.lehigh.edu)

                            November 26, 1991


NETLIB is a network-based facility for the automated distribution of the
source code for public domain scientific software.  Most of this software is
written in FORTRAN, but some software is available in C, C++, and PASCAL.
Software is available for a wide variety of applications.  The following
list -- which is far from exhaustive -- should give the reader an idea of
the scope of the NETLIB collection.


NUMERICAL METHODS:

   * Linear Algebra (basic linear algebra subroutines, eigenvalue and
       eigenvector computations, matrix factorizations, least squares)
   * Sparse Matrix Calculations
   * Numerical Optimization
   * Spline Interpolation
   * Ordinary Differential Equations
   * Fast Fourier Transforms
   * Special Functions


WELL-KNOWN SOFTWARE:

   * Collected algorithms of the Association of Computing Machinery
       (ACM) Transactions on Mathematical Software (TOMS)
   * LINPACK and EISPACK subroutine libraries
   * AMS TeX and SIAM typesetting macros
   * MATLAB applications packages


SOFTWARE FOR ADVANCED COMPUTER ARCHITECTURES:

   * Libraries for supporting parallel computation
   * Libraries for computation on vector-processor machines


MISCELLANEOUS:

   * Multiple-precision floating-point arithmetic packages (e.g.,
       Brent's MP; Smith's FM, from TOMS algorithms)
   * Benchmark programs for comparing computing platforms
   * Collections of problems for testing numerical software
   * Programming aids such as single-precision-to-double-precision and
       FORTRAN-to-C conversion utilities
   * Companion software to various textbooks (e.g., Cheney & Kincaid;
       Forsythe, Malcolm, and Moler)
   * Bibliographies


Information and software (e.g., an index of available software, or the
source code to LINPACK) are obtained by sending requests -- via electronic
mail -- to a NETLIB mail server at one of the following Internet addresses:

     netlib@research.att.com          (AT&T Bell Labs, New Jersey, USA)
     netlib@ornl.gov                  (Oak Ridge Nat. Lab, Tenn., USA)
     netlib@ukc.ac.uk                 (Univ. of Kent, UK)
     netlib@nac.no                    (Oslo, Norway)
     netlib@draci.cs.uow.edu.au       (Univ. of Wollongong, NSW, Australia)

Upon receipt of an appropriately worded request, the NETLIB mail server
responds by sending the information or software requested to the electronic
mail address of the requestor.  Depending on your location, the amount of
network traffic, and other factors, you may actually receive a reply within
minutes of submitting your request!

Note that the software available and services provided may differ somewhat
>from one NETLIB site to another.  For example, the AT&T Bell Labs site,
research.att.com, also provides:  (1) access to the NETLIB software via FTP
(login with username "netlib"), and (2) access to an interactive biblio-
graphic search system via TELNET (login with username "walk").

To obtain more detailed instructions on the use of NETLIB, as well as an
index of the software currently available from a particular NETLIB site,
send the following one-line message to a NETLIB mail server (selected from
the above list -- usually the geographically closest site):

     send index

Note that some mail trailers may possibly confuse the NETLIB mail servers.
If you encounter problems, you might want to suppress the use of your usual
signature file when sending requests to NETLIB via electronic mail.


 //////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
|  Frederick W. Chapman |   Campus Phone:  8-3218   | Junior Adult Men's     |
|  User Services Group  |   fc03@ns1.cc.lehigh.edu  | Free-Skating Bronze    |
|  Computing Center     |     -.-. --.-   -.. .     | Medalist, Mid-Atlantic |
|  Lehigh University    | N3IJC (2m band, FM voice) | Competition, 1991!     |
 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////////////////////////////////
")




("lib.stat.cmu.edu" "statlib" "brannon@jove.cs.caltech.edu" "Statistics" "Statistics" "To get started using StatLib, send the one line message
	send index
to statlib@lib.stat.cmu.edu

StatLib also has an anonymous FTP facility.
Use ftp to connect to lib.stat.cmu.edu and login with user name
"statlib".  PLEASE send your e-mail address as your password.  This
will enable us to notify you of changes to the software and notify the
contributors about who has requested the software.

A reminder that there is also an archive of SAS related material.
Send the message
	send sas from general
for details about the SAS-L archive.
") 

("ted.cs.uidaho.edu" "anonymous" "china.primitve@divineland" "Logic" "HigherOrderLogic Theorem Prover" "pub/hol") 
("wolfen.cc.uow.edu.au" "anonymous" "china.primitve@divineland" "Computer Graphics" "Gif Viewer for PC's" "pub/pc") 
("hanauma.stanford.edu" "anonymous" "china.primitve@divineland" "Ethnic Tex" "Bejing-24.bdf/PinYin pronunciation tables/Pinyin->GB code table/Best of comp.graphics/World Map/Zhongwen" "") 
("neon.stanford.edu" "anonymous" "china.primitve@divineland" "Ethnic Tex" "ChTeX Modified" "") 
("lurch.stanford.edu" "anonymous" "china.primitve@divineland" "Postscript" "Episode Viewer" "") 
("anna.stanford.edu" "anonymous" "whodat@up.backupin.there" "X-Windows/ADA/Tex" "Annotated ADA" "Sriram Sankar's ada macros for TeX..") 
("labrea.stanford.edu" "anonymous" "china.primitive@divineland" "Tex" "dvips" "quasi-office repositor for TeX and related toys") 
("sun-valley.stanford.edu" "anonymous" "china.primitive@divineland" "GCC Hacks" "GCC for 68k" "pub/GNU for docs and binaries From marc@sun-valley.stanford.edu Sat Nov 16 23:40:10 1991
") 
("interviews.stanford.edu" "anonymous" "china.primitive@divineland" "Object-Oriented Graphics" "Interviews" "home of Interviews, an object-oriented windowing system")

("venus.ycc.yale.edu" "anonymous" "venus@venus.edu" "Editors/X-Windows/TeX/Gifs" "Gifs/Unix-TECO/TeXtyl" "TeXtyl allows postprocessing with any available fonts")
("yalevm.ycc.yale.edu" "anonymous" "venus@venus.edu" "Mathematics" "Mathematical Abstracts Database" "Yale University is developing an electronic network for the distribution of
mathematics preprints.  Abstracts  of the preprints are stored in a database,
available via the Internet.  Currently abstracts are available from various
mathematicians at Yale University, the University of Texas at Austin, Oklahoma
State University, Washington University in St. Louis, and the University of
Paris.   The full text of the preprints exist on a series of files at
participating universities which can be downloaded via anonymous FTP (File
Transfer Protocol).    

The file of abstracts for preprints from all participating universities has
been mounted on an IBM3083 at the Yale Computer Center, using the Documaster
text management software.  Once the abstracts are entered into the Documaster
file, they can be searched by author, title, author's address, date, the status
of the preprint (accepted for publication, preliminary draft, class notes, book
draft, etc), words in the abstract, keywords supplied by the author, and the
Math Reviews subject classification code.   Mathematicians interested in
obtaining the full text of the preprint will download directly from the
originating university.

TO SEARCH THE IMP DATABASE

To use IMP via Internet, you need a copy of TN3270.  Telnet to
yalevm.ycc.yale.edu .  

You can also dial up using standard long distance lines, if you have ASCII
terminal emulation.  Set your communications software for even parity, 7 data
bits, and 1 stop bit (1200 baud, 203-432-5800; 2400 baud 203-432-5804)

PASSWORDS

After the connection is made, you will be prompted:

USERID:		Math1 (or Math2, 3, 4, or 5)
PASSWORD: 	Math1 (or Math2, 3, 4, or 5)

OPERATOR ID:	Math1  

After this point, the screen menus will guide your search.

FOR INSTITUTIONAL PARTICIPATION

We are actively encouraging more institutions to begin submitting abstracts to
IMP.  Each participating institution should designate a local coordinator. 
This person should be responsible for certain aspects of coordinating their
institution's participation and needs an electronic mail account that can be
used to send and receive electronic mail via BITNET or the Internet.  The
coordinator needs to make sure that the following tasks are delegated or
assigned:

- --Submitting abstracts of your university's preprints via electronic mail to
Yale University.  
- --Arranging for the storage of the full text of their institution's preprints
on a computer that is accessible via the Internet through anonymous FTP. 
- --Writing a README file, based on a sample that Yale will provide, so that
mathematicians at other institutions can download electronic copies of your
institution's preprints.   
- --Obtaining a copy of tn3270 and distributing it appropriately to those
individuals at your institution who wish to use IMP.
- --Distributing for signature and retaining signed copies of the IMP License
Agreement for all original authors of abstracts.

FOR MORE INFORMATION

A brochure (electronic or printed) describing IMP is available from Katherine
Branch, Kline Science Library, Yale University, New Haven CT  06511,
katherine_branch@yccatsmtp.ycc.yale.edu.

General suggestions/inquiries/policies/participation:  Katherine Branch,
katherine_branch @yccatsmtp.ycc.yale.edu, 203-432-3439 (voice);
Search System Bugs:  David Bruce, bruce@yalevm (Abstract searching only);
Problems:  Victor Wickerhauser, victor@jezebel.wustl.edu 	
")
("weedeater.math.yale.edu" "anonymous" "china.primitive@divineland" "Graphics/Ray Tracing" "Utah Raster Toolkit" "") 
("bulldog.cs.yale.edu" "anonymous" "anonymous" "Spell Check" "Ispell" "")

("pl118g.cc.lehigh.edu" "tb06" "" "?" "?" "Packard Sun Lab") 
("pl118f.cc.lehigh.edu" "tb06" "" "?" "?" "Packard Sun Lab") 
("pl118e.cc.lehigh.edu" "tb06" "" "?" "?" "Packard Sun Lab") 
("pl118d.cc.lehigh.edu" "tb06" "" "?" "?" "Packard Sun Lab") 
("pl118c.cc.lehigh.edu" "tb06" "" "?" "?" "Packard Sun Lab") 
("pl118b.cc.lehigh.edu" "tb06" "" "?" "?" "Packard Sun Lab") 
("pmatlab5.csee" "terrence" "" "?" "?" "tektronix 4317") 

("kth.se" "anonymous" "china.primitive@divineland" "Ethnic Tex" "ChTeX" "mirror site for Ethnic Typographic/TeX things") 
("vaxb.acs.unt.edu" "anonymous" "dong@east.zhongguo" "IBM" "IBM Internet BBS Program" "This is to inform you that HYTELNET version 4.0, the utility which gives 
an IBM-PC user instant-access to all Internet-accessible library 
catalogs, Freenets, CWISs, Library BBSs, etc. is now available. You can 
get it via anonymous ftp from:
")




("trantor.harris-atd.com" "anonymous" "china.primitive@divineland" "?" "?" "") 
("irisa.irisa.fr" "anonymous" "china.primitive@divineland" "Ray Tracing/Graphics" "MicroEmacs/VT100 Art/Kermit Docs/Zoo Archiver" "") 

("ifi.informatik.uni-stuttgart.de" "anonymous" "china.primitve@divineland" "Ethnic Tex" "ArabTeX" "Home of ArabTeX") 
("pavern@uk.ac.man.cs" "anonymous" "china.primitive@divineland" "X Games" "Xpipeman" "Xpipeman is an addictive strategy game") 
("kythera.nmsu.edu" "anonymous" "china.primitve@divineland" "Ethnic TeX/Chinese Metafonts" "Arabic/Japanese/Korean/ChTeX/ArabTeX" "lots of language and tex-related stuff.") 

("msdos.archive.umich.edu" "anonymous" "china.primitve@divineland" "Ethnic Tex" "Duke Chinese Typist" "") 


("bull.cs.williams.edu" "anonymous" "anonymous" "?" "ParaGraph" "")

("ccb.ucsf.edu" "anonymous" "anonymous" "comp.sources.unix/TeX/Bitmaps/Games/Graphics" "Bitmaps/comp.sources.unix/wp2latex/CommonTeX2.9/ckermit/Uemacs_3.10/UC-Berkeley Software Catalog" "")
("cs.arizona.edu" "anonymous" "anonymous" "NSF-Japan/Software Development/Pnrogramming Languages/SB-Prolog" "NSF-Japan/SB-Prolog/Japanese Computer Science Info/Scorpion Software Development System/SR Programming Language/Janus Programming Language/Icon Programming Language/Xkernel/Agrep/Anrep" "")
("iear.arts.rpi.edu" "anonymous" "china.primitive@divineland" "Graphics" "Star Gifs/Ray Tracing" "") 
("cs.umn.edu" "anonymous" "china.primitive@divineland" "X-Windows/Tex" "ChTex/ArabTeX/Image" "") 


("rascal.ics.utexas.edu" "anonymous" "anonymous" "LISP/Logic" "thm (theorem prover)/Kyoto Common Lisp" "   e-mail.

Check out Austin Kyoto Common LISP which is available by anonymous ftp
>from rascal.ics.utexas.edu in "pub". It is a free LISP which, to allow
greater portability, translates LISP to C, compiles the C with the
system's standard C compiler and then loads the object file into the
running LISP. 

- -- 
	 Chris Whatley - The UNIX Guy - UT-Austin Mathematics
     E-mail: chari@{math,emx,cs}.utexas.edu, chari@gnu.ai.mit.edu
	    (NeXT Mail format acceptable but not desired)
		   Ph: 512/471-7107(O),499-0475(H)
")

("ftp.cs.cornell.edu" "anonymous" "graphics.fiend@lehigh.edu" "X-Windows/Tex" "Fig/TransFig" "")
("tesla.ee.cornell.edu" "anonymous" "anonymous" "Parallel Computing Environment" "Paragon Parallel Programming Environment" "What is Paragon?
Paragon is a collection of data-parallel program constructs
implemented in C++.  Paragon is intended to be a research vehicle for
investigating issues for parallel execution of large-scale scientific
programs.  We use it to develop and evaluate parallel programming
constructs and techniques.  We also use it in the Parallel Processing
course at Cornell to demonstrate the data-parallel programming
paradigm. 

What does it run on?
There are two versions of the Paragon library.  The uniprocessor
version should run on any machine with a working C++ compiler.  It is
known to work on Sun workstations with either AT&T's Cfront 2.0
compiler or GNU's g++ 1.39.0  The uniprocessor version has also been
tested on, the IBM RS 6000 and 3090 computers.

The multiprocessor version should run on the Intel iPSC/2 and iPSC/860
hypercube.  The multiprocessor version has also been ported to the
Parallel Virtual Machine (PVM).  This release of Paragon supports PVM
to provide parallel execution on heterogeneous networks of
workstations. 

How do I get it?
The paragon release is provided in three tar files.  The complete
distribution is available on tesla.ee.cornell.edu in:
pub/paragon/paragon-1.1.tar.Z

If you're just interested in the serial version, get
pub/paragon/uniproc.tar.Z

If you're just interested in some reading material, the LaTeX source
and raw postscript for the Paragon User's Guide is available in

pub/paragon/uguide.tar.Z.  

The Paragon paper presented at the 1991 International Conference on
Parallel Processing is available as 
pub/paragon/ICPP-91.ps.Z 

and an updated version of this paper is included as
pub/paragon/ICPP-91_revised.ps.Z

Please send all comments, suggestions, bug reports, and complaints to
paragon@ee.cornell.edu
")


("cs.utk.edu" "anonymous" "anonymous" "Parallel Computing" "Parallel Virtual Machine" "PVM (parallel virtual machine) is a software
package which allows the utilization for a heterogeneous network of
computers.")



("genbank.bio.net" "anonymous" "china.primitive@divineland" "Genbank-EMBL-DDBJ/Molecule Folding/Medical Information" "Molecule Folding" "Another possibility is to use a program called MOLECULE.  It will
draw the molecule from the Zuker FOLD program using the .CT file
which can be generated.  This is a DOS program so you do not need
a terminal emulation (and can do it at home 8-) ).  It can be
obtained from the EMBL fileserver, or (I believe) from
GENBANK.BIO.NET by ftp.  There are different versions depending on
your video card - EGA, Herc., etc.
") 
("sirius.ucs.adelaide.edu.au" "anonymous" "china.primitive" "?" "?" "") 
("nic.funet.fi" "anonymous" "china.primitive@divineland" "Protein Primary Structure" "ProfileGraph" "") 
("kampi.hut.fi" "anonymous" "china.primitive@divineland" "?" "?" "") 
("garbo.uwasa.fi" "anonymous" "china.primitive@divineland" "?" "?" "") 
("ousrvr.oulu.fi" "anonymous" "china.primitive@divineland" "?" "?" "") 
("tukki.jyu.fi" "anonymous" "china.primitive@divineland" "?" "?" "") 
("tolsun.oulu.fi" "anonymous" "china.primitive@divineland" "?" "?" "") 

("menudo.uh.edu" "anonymous" "china.primitive@divineland" "Protein Primary Structure" "ProfileGraph" "") 
("menudo.uh.edu" "anonymous" "china.primitive@divineland" "Protein Primary Structure" "ProfileGraph" "") 
("ftp.bio.indiana.edu" "anonymous" "china.primitive@divineland" "Drosophila Genetic Map/Protein Primary Structure" "ProfileGraph/FlyBase (Drosophila)" "") 
("karyon.bio.net" "anonymous" "china.primitive@divineland" "Restriction Enzyme Dbase" "" "Version 9111 of Richard Roberts Cold Spring Harbor Laboratory
restriction enzyme database (REBASE) is now available for anonymous
FTP from karyon.bio.net [134.172.1.251] in the directory
pub/db/REBASE.  The data are contained in the file type2.lst and
literature references are in references.lst.  Questions about the
database should be directed to Dr. Roberts at roberts@cshl.org.

                                Sincerely,

                                Dave Kristofferson
                                GenBank Manager

                                kristoff@genbank.bio.net

") 



("decuac.dec.com" "anonymous" "china.primtive@zhongguo" "X-Windows/Games" "None" "")

("bioftp.unibas.ch" "anonymous" "yoyo@yoyo.yo" "Biology" "Biology" "               Setup of the Server bioftp.unibas.ch
               ====================================

                                    ~/
                 +-------------------+-----------------------------+
                 |                                                 |
               biology                                         programs 
   +-------------+-----------+--------------------+      uu and zoo source code
   |                         |                    |     
   EMBnet                    cd                  database
   |           The last EMBL CD-ROM (resources    +------------------------+
   |          permitting - drive used elsewehere)                          |
   |                                                                       |
   +-----+-------------+-----------+--------------------+                  |
         |             |           |                    |                  |
    XSwissprot         |     XEMBL.VAX_ZOO           Xembl                 |
(not yet; scheduled)   |           |                    |                  |
    +----+--+          |   zoo'ed    VAX/VMS       The contents            |
data.dir   indices.dir |   GCG / NBRF formatted    of the dir's            |
Updates to the current |   current XEMBL.ref       below containing        |
Swissprot database     |   and XEMBL.seq files     Entry code, and         |
                       |   as well as XXEMBL       title line              |
            DATABASE.ZOO                                 |                 |
The zoo'ed databases currently                           |                 |
availavle at BIOCENTER in VAX        +--------------------------+          |
/VMS and GCG/NBRF formatted          |                          |          | 
zoo files: EMBL,SWISSPROT,PIR        |                          |          |
                                   data.dir                    indices.dir |
+----+----+----+----+----+----+----+-+--+----+----+----+----+  containing  |
fun  inv  mam  org  phg  pln  pri  pro  rod  syn  una vrl vrt  indices in  |
Entries in EMBL format, *ONE PER FILE*. The files are named    EMBL format |
using the entry code as file name. NEW in the next release;                | 
extension 'dat', update to existing release, extension 'upd'.              |
                                                                           |
Variuos databases available on various other servers as well,mainly updated, 
                           but not entirely produced, by Amos Bairoch, Geneva 
                                                                           |
 +---+----+------+---+----+---+---+----+------+----+-----+--+---+----+----++
 |   |    |      |   |    |   |   |    |      |    |     |  |   |    |    |
ecd  | fans_ref  |  info  |  ngdd |  seqanalr |  enzyme  | fly  | jour_tab|
     |           |        |       |           |          |      |       trna
   prosite      tfd      alu      epd     gcg_codon    limb   rebase")




("lhc.nlm.nih.giv" "anonymous" "yoyo@yoyo.yo" "Medical Information" "Datafile of Biological Scientists" "I am in the process of updating the database.  If you would like to be
included, please fill out the following form, and send it to me at
hunter@nlm.nih.gov.  Thank you very much.

				Larry Hunter
Lawrence Hunter, PhD.
National Library of Medicine
Bldg. 38A, MS-54
Bethesda. MD 20894
(301) 496-9300
(301) 496-0673 (fax)
hunter@nlm.nih.gov (internet)

- ----- begin file ~/ftp-pub/lhc/database-entry-form -----
feel free to use more than one line per field.

name:
title/degree:
job title:
organization:
department:
address:
area code:
phone number:
fax number:
email address:
alt. email address:
research interests:
comments:

- ----- end file ~/ftp-pub/lhc/database-entry-form -----
")
("ncbi.nlm.nih.gov" "anonymous" "hmm@hmm.noe" "Medical Information" "Eukaryotic Promoter Database" "./repository/EPD" "
- - David Ghosh, NCBI  
  ghosh@ncbi.nlm.nih.gov
")

("gatekeeper.dec.com" "anonymous" "china.primitive@divineland" "Recipes/Games/comp.sources.games/comp.sources.misc/comp.sources.sun/comp.sources.unix" "c++2latex/shX" "comp.sources.misc")
("arthur.cs.purdue.edu" "anonymous" "china.primitive@divineland" "TeX" "TeXPS" "") 
("cs.purdue.edu" "anonymous" "china.primitive@divineland" "Ethnic TeX" "cxterm/celvis/cless/cclib16* fonts" "") 
("j.cc.purdue.edu" "anonymous" "china.primitive@divineland" "comp.binaries.amiga/comp.sources.amiga/comp.sources.games/comp.sources.sun/comp.sources.unix/comp.sources.x/comp.sys.next" "hqx" "") 
("ftp.uu.net" "anonymous" "china.primitive@divineland" 
"TeX/Image Compression/Graphics/Ray Tracing" "epp2tex/JPEGxeval IMAGE Compression/Utah Raster Toolkit" "epp2tex is a tool which accepts a relatively easy to write file
of estimation results.  It generates a LaTeX table.  It has limited
commandline support for tuning the appearance of the table.

It is part of the distribution of a program named "ols" which
was posted on comp.sources.reviewed today.  You can pick it up 
>from there, or by anonymous ftp from uunet.uu.net, in the 
directory comp.sources.reviewed/volume01/ols.
") 
("ftp.nisc.sri.com" "anonymous" "china.primitive@divineland" "?" "?" "") 
("lion.waterloo.edu" "anonymous" "china.primitive@divineland" "Dikumud" "?" "dikumud on port 4000") 
("watserv1.waterloo.edu" "anonymous" "china.primitive@divineland" "Programming Language" "Fortran2C/Pascal2C/J (programming language)" "
		Installation and Start-Up Procedure
		for J, Version 3.2, on Unix (TM) systems


0.  The compressed tape archive contains the following files:

     j				The executable file
     readme.doc			Copyright notice
     status.doc			Implementation status
     install 			Something like this file
     message 			Message in a Bottle
     isiinfo.doc		Information about ISI
     tutorial.js		Script for running a tutorial
     tut			Sub-directory needed by tutorial.js
     tut/tut*.js		Files needed by tutorial.js

1.  These installation steps are suggested:
Start with the file j_XXX_3.2_.tar.Z, the compressed Tape ARchive
file for the system identified by XXX.

     mkdir ~/j			Create a new directory called j.
     mv j_*.tar.Z ~/j		Move the package to the new directory.
     cd ~/j			Change to the new directory.
     uncompress j_*		Expand the compressed tar file.
     tar xof j_*.tar		Extract files from the tape archive.

2.  If yours is a one user system, these steps are suggested:

     alias j ~/j/jXXX		Set an alias for j.
     j				To execute the alias.

In this way, J will be available to you from any directory.
The J tutorial (mentioned above) will be available to you
whenever you make ~/j your current directory.

3.  When you start J, you should soon see the message

   J Version 3.2  Copyright (c) 1990 1991, Iverson Software Inc.

   and a 3-space prompt.  At this point, the system is ready to
   interpret J sentences.  To terminate the session, enter   )off  .

4.  A tutorial is available.  Within a session, enter:

     )sscript 'tutorial.js'	Make the necessary definitions
     tutorial ''		Start the tutorial

5.  For shared use on a multi-user system, an appropriate installation
is needed.  The directory used for the binary file  j  will depend
on the conventions in use at your installation.  Some system
administrators may like  J  itself to be installed as /bin/j.
The  J  tutorial files may be installed in a different directory,
such as  /software/j/doc.  If this directory path is used, the
files needed by the tutorial would named: 

	/software/j/doc/tutorial.js
	/software/j/doc/tut/tut*.js	# (47 of these, numbered 0 to 46)

In such a setting, for the file  tutorial.js  to run correctly, it must
be modified.  In this file, the line

	frames=.<&read ('tut/tut'&,)&(,&'.js')&:&.>i.47

may be replaced by

	Path =. '/software/j/doc/tut'
	frames=.<&read ((Path,'/tut')&,)&(,&'.js')&:&.>i.47

Then users may be instructed to do these steps,

	% cp /software/j/doc/tut/tutorial.js .
	% j

	      )sscript 'tutorial.js'
	      tutorial ''

Or users may be instructed to do these steps:

	% j

		)sscript '/software/j/doc/tutorial.js'
		tutorial ''


- -------------------------------------------------------------------------------

                          What is J?

			An introduction
			    Part 1


			     by

			Leroy J. Dickey

			September, 1991


J is a high powered general purpose programming language.  This dialect
of APL uses the ASCII character set, has boxed arrays, complex numbers,
and phrasal forms.  Any time one wants to do calculations with more
than one number at a time, or more than one name in a list, a
collective (an array) is the right structure to use, and J is designed
to make it easy to do collective calculations.

People who write programs for themselves and who want their answers
quickly will be happy with the swift and concise way programs can be
written in J.

This article consists of several examples that illustrate some of the
power of the language J.  In each of the examples presented, output
>from an interactive J session has been captured, and few lines of
explanation have been added.  Lines that are indented with four spaces
are those that were typed in to the J interpreter, and the lines that
are indented with only one space are the responses from J.  Other text
lines are comments that have been added later.  The topics that have
been chosen for inclusion do not come close to telling everything about
J, but some of them represent subjects that at one time or another the
author wished that he had known and understood.

Because my professional interests are mathematical, my examples are
primarily mathematical in nature.  But J is a general purpose computing
language, and is well suited to a broad spectrum of programming needs.


Example 1:  Functional notation

This example shows how calculations are done with a list of numbers and
how the notation is functional.  That is, each verb (function) acts on
the data to its right.   It is easy to create a collective.  Here, for
instance, are 9 numbers:

    i. 9
 0 1 2 3 4 5 6 7 8

    a =. i. 9
    a
 0 1 2 3 4 5 6 7 8

The factorials of these numbers are:
    ! a
 1 1 2 6 24 120 720 5040 40320

The reciprocals of the above:
    % ! a
 1 1 0.5 0.166667 0.0416667 0.00833333 0.00138889 0.000198413 2.48016e_5

And the sum of reciprocals of the factorials of a is:
    +/ % ! a
 2.71828


Those who know some mathematics may recognize an approximation to the
number ``e'', the base for the natural logarithms.  Of course J has
other ways to calculate this number; the point here is the use of the
natural right to left reading of the meaning of the sequence symbols
+/%!a as the sum of the reciprocals of the factorials of a.

Those who have done some programming in almost any other language, will
see that the expression +/%!i.9 is a remarkably short one to produce
an approximation to ``e''.  Moreover, it is possible to pronounce this
program in English in a way that precisely conveys its meaning, and
those who know the meaning of the words in the sentence will be able to
understand it.  And anybody who knows the mathematics and J will
recognize that the program does exactly what the words say and that it
will produce an approximation to ``e''.

The author finds this expression in J much easier to think about and
to understand than a corresponding program in a ``traditional'' language
intended to compute the same number.  Here, for example, is what one
might write in Fortran, to accomplish the same result:


	REAL SUM, FACTRL
	SUM = 1.0
	FACTRL = 1.0
	DO 10 N = 1, 8
	    FACTRL = FACTRL * N
	    SUM = SUM + 1.0 / FACTRL
     10 CONTINUE
	PRINT, SUM
	STOP
	END


Compare this Fortran program with the J program that uses only a few
key strokes: +/ % ! i. 9 .  Not only is the J program shorter, but it
is easier to understand, even for the person who has just learned the
meaning of the symbols.  Some have estimated that for typical
applications, the ratio of the number of lines of Fortran or C code to
the number of lines of J code is about 20 to 1.



Example 2:

In this example, two verbs are defined.  As before, lines indented
with four spaces were given as input during a J session, and
the ones immediately after, with one leading space, were produced by
the J interpreter.  Other lines are comments, and were added later.


    Sum =. +/
    Average =. Sum % #
    c =. 1 2 3 4
    Average c
 2.5

The first two lines create pro-verbs, the third creates a pro-noun,
and the fourth invokes the function Average with the data c.
The meaning of   Average c  is this:

		(+/ % #) 1 2 3 4

and this may be thought of as:  find Sum c, (add up the entries
in c), find # c (the count of c), and then find the quotient of those
two results.

Example 3:  Continued fractions

In this example, the verb ``pr'' (plus reciprocal), is considered.

    pr =. +%

The next input line shows a use of this hook and you can see that
the meaning is 5 plus the reciprocal of 4:

    5 pr 4
 5.25

    4 pr 5
 4.2

The function pr is defined above as  +% , and this diagram 
for    4 +% 5  may help you to understand its meaning.

 					+
 				       /  \
 				      4    %
 				           |
 				           5

Because of the shape of the diagram, the combination of two verbs
in this way is called a hook.  To continue with our example,

    1 pr 1 pr 1			
 1.5

The above input line may be read as ``1 plus the reciprocal
of (1 plus the reciprocal of 1)''.  And here below is an analogous
line with four ones:

    1 pr 1 pr 1 pr 1
 1.66667

    1 pr (1 pr (1 pr 1))
 1.66667

    pr / 1 1 1 1		
 1.66667

    pr / 4 # 1
 1.66667

The above input line has exactly the same meaning as the three input
lines that come before it.  Notice that (of course) the value
of the result is the same.  The / is an adverb called over
and the result of ``pr /'' is a new verb, whose meaning is
derived from the verb ``pr''.

This expression could be written another way, using a traditional
mathematical notation the way that mathematicians have been using
to write continued fractions for years:

		1
   1 + ------------------------
		1
	1 + ------------------
			1
		1 + ------------
			1

This particular continued fraction is a famous one because it is
known to converge to the golden ratio.  That means that if you just
keep on going, with more and more ones, the value gets closer and
closer to the golden ratio.  One can see the values of the continued
fraction expansion converging by using \, the scan adverb.  It
creates a sequence of initial sequences.  Here is a nice, compact
expression that shows the values for the first 15 partial continued
fractions.

    pr /\ 15$ 1
 1 2 1.5 1.66667 1.6 1.625 1.61538 1.61905 1.61765 1.61818 1.61798
      1.61806 1.61803 1.61804 1.61803

In this, you can see that the numbers seem to be getting closer and
closer to some limit, probably between 1.61803 and 1.61804.  If you
concentrate only on the odd numbered terms, (the first, the third, the
fifth, and so on), you will see a sequence of numbers that is
monotonically increasing.  On the other hand, the even numbered terms,
( 2, 1.66667, 1.625, and so on), form a sequence of numbers that is
monotonicly decreasing.  Of course these observations do not constitute
a proof of convergence, but they might convince you that a proof can
be found, and in the light of these observations, the proof probably
follows along these lines.

Can you think of a simpler way to represent and evaluate continued
fractions?  I can not!


Example 4.  Using rank operator, and fork.

In this example, a second phrasal form, the fork, is discussed.
First some data are built.

    a =. 100 200
    b =. 10 20 30
    c =. 1 2 3 4

This is the beginning of the data construction.  The verb Sum, a
modification of +, and defined as before, is used.  But in this example,
the usage is different from that used before.  So that you can have a
glimpse at what the dyadic verb Sum does, notice see what it does with
b and c:

    b Sum c
 11 12 13 14
 21 22 23 24
 31 32 33 34

Once you have seen this example, you are be ready to see and understand
the next step.  Here the collective ``data'' is built, using a, b, and c.

    data =. a Sum b Sum c
    data
 111 112 113 114
 121 122 123 124
 131 132 133 134

 211 212 213 214
 221 222 223 224
 231 232 233 234

This rank three array may be thought of as having has two
planes, with each plane having two rows and three columns.
Now see what Sum does with the data:

    Sum data
 322 324 326 328
 342 344 346 348
 362 364 366 368

Can you see what is happening?  It is adding up corresponding
elements in the two planes.  Twelve different sums are performed,
and the result is an array that is 3 by 4.

But we might wish to add up numbers in rows.  We can do it this way:

    Sum 1 data
 450 490 530
 850 890 930

And to add up numbers in each column:

    Sum 2 data
 363 366 369 372
 663 666 669 672

The expressions 1 and 2 are read as 'rank 1' and 'rank 2'.
The rank 1 objects are the rows, and the rank 2 objects are the
planes, and the rank 3 object is the whole object itself.

Now, recall the definition of Average.

    Average =. Sum % #

Now apply this proverb Average to the pronoun data, to get:

    Average data
 161 162 163 164
 171 172 173 174
 181 182 183 184

The result is the average of corresponding numbers in the two planes.
And, if we would like to know the average of the numbers in the rows,
(remember above, we asked about the sum of the entries in the rows?)
we type:

    Average 1 data
 112.5 122.5 132.5
 212.5 222.5 232.5

and finally, averaging the columns of the two planes:

    Average 2 data
 121 122 123 124
 221 222 223 224


Again, compare the sizes of these results with the sizes of
the sizes of the results above where we were asking simply about
sums.

    Sum 1 data
 450 490 530
 850 890 930

    Sum 2 data
 363 366 369 372
 663 666 669 672

What is exciting about this example is not only how easily the nouns
(data) and verbs (functions) were built, but how every verb acts in a
uniform and predictable way on the data.  Rank one action is the same
kind of action whether one is summing, or averaging, or whatever.  It
has been said that the concept of rank is one of the top ten ideas to
come along in computing in the last decade.

Conclusion:

What has been written in this article only begins to scratch the surface
of the power of J, but it does show some of the flavor.
J is powerful because one get results quickly.  It is a programming
language for people who need to write programs for themselves, but
who don't want to spend inordinately large amounts of time getting
their results.  If you have to write programs for others, this could
still be a language for you, if you get paid by the job, and not
by time.

Availability:

J is available from watserv1.waterloo.edu in the directory
languages/apl/j and is also available from certain other servers, but
the others usually have some specific hardware orientation.  At the
Waterloo site, versions of J are available that run on several kinds of
computers.

Learning J:

Because J is new, there are not many materials about J yet.
The novice should get a copy of J and let J be the guide and
the interpreter (pun intended).  There is a tutorial that has
tons of material in a small space.  This tutorial comes with
the interpreter, free of charge.  There is also a status.doc
file that comes with J.  This tell what features have been
implemented and which have not.

The author of J:

The inventor-developer of J is Kenneth E. Iverson, the same man who
invented APL, a real pioneer of computing.  Some years ago, he retired
>from IBM and went to work for I.P.Sharp Associates, a Toronto company
that developed the world's first private packet switching network,
the finest APL available, and was at one time the worlds largest
time sharing service, all APL.  The descendant of this company,
Reuter:file, still has offices in many of the world's major cities.
Now Iverson is retired (again) and lives in Toronto, where he spends
his time developing and promoting J.

The programmer for J is Roger Hui.  Roger hails from Edmonton, and has
lived in Toronto since he joined I.P.Sharp Associates in 1975.  The
source code for J, written in C, is worthy of some comment, because it
makes extensive use of macro expansion.  Its style, influenced by the
work of Arthur Whitney of Morgan Stanley in New York, reflects Roger's
strong background in APL.  Today, Roger spends much of his time
developing J and does occasional contract work in APL.

Copyright (c) 1991 by Leroy J. Dickey
Permission is granted to use this for
non-profit and educational purposes.
It may be given away, but it may not be sold.
") 

("jove.cs.caltech.edu" "brannon" "" "?" "?" "") 
("moxie.oswego.edu" "anonymous" "no.pass.req" "Ethnic TeX/Latin Font/Cyrillic Font/X-Windows Fonts" "KOI-7/8 fonts" "cd to COUP/TL/xkoi-8.7.tar")
("usc.edu" "anonymous" "anonymous" "Pattern Processing" "Teco" "teco is in pub/teco") 
("csvax.cs.caltech.edu" "brannon" "" "C Declaration Parser/Pascal2C/Emacs Calculator" "PascaltoC/C Declaration Parser/GnuCalc" "dave gillespies calc adds floating point ops to elisp!") 

("unix.sri.com" "anonymous" "anonymous" "Artificial Intelligence" "Artificial Intelligence Lisp Code" "pub/norvig") 

("alw.nih.gov" "anonymous" "anonymous" "Graphics" "NIH Image 1.19" "") 
("alf.uib.no" "anonymous" "china.primitive@siva" ?" "?" "") 

("monu6.cc.monash.edu.au" "anonymous" "anonymous" "Japanese Tutorial" "Kanji Guess" "in /pub/Nihongo on monu6.cc.monash.edu.au. KG is a Japanese
language learning aid, with lots of user-modifiable quizzes, flashcards.
etc. and hiragana/katakana gojuuonjun quizzes. It runs well on XT->486
using Herc and VGA (I haven't tried EGA, and CGA has problems). It is
best with a mouse, but works without.

I was one of KG's beta testers, and I found it excellent, and a lot of fun too.
KG is Shareware. Please register and pay if you use it.

Also on monu6 is V1.5 of JDIC (Japanese/English Dictionary Display). This is
an upgrade of the V1.4 which has been around for a few months, and was recently
distributed via comp.binaries.ibm.pc.
")

("giza.cis.ohio-state.edu" "anonymous" "meishaboy@kick.it" "?" "?" "duplicates most of the stuff on export.lcs.mit.edu") 
("archive.cis.ohio-state.edu" "anonymous" "china.primitive@siva" "Gnu/Perl" "dvi3ps/Gnu/Perl" "Major Gnu Archive ... dvi3ps is in pub") 

("cs.orst.edu" "anonymous" "china.primitive@divineland" "?" "?" "") 
("nova.cc.purdue.edu" "anonymous" "china.primitive@divineland" "NeXT" "?" "") 
("rsovax.circe.fr" "musictex" "tb06@pl118f.cc.lehigh.edu" "?" "?" "M U S T login as musictex and give your name as pw.. has MusicTeX") 
("mims-iris.waterloo.edu" "anonymous" "tb06@pl118f.cc.lehigh.edu" "Raster Fonts/MetaFonts" "Metafonts" "probably has all the HP fonts in the world") 

("ctrsci.math.utah.edu" "anonymous" "tb06@pl118f.cc.lehigh.edu" "Raster Fonts/MetaFonts" "Metafonts" "Raster fonts for Metafont use") 
("science.utah.edu" "anonymous" "tb06@pl118f.cc.lehigh.edu" "Raster Fonts/MetaFonts" "Metafonts" "Raster fonts for Metafont use") 
("cc.utah.edu" "anonymous" "mm.mm..m" "Gnu" "VMS Gnu Emacs" "Major Gnu Archive")
("cs.utah.edu" "anonymous" "mm.mm..m" "Graphics/Ray Tracing/Non-Rational B-Spline Software" "Utah Raster Toolkit/NURBS (Non-Uniform Ratl B-Splines)" "")


("ee.ecn.purdue.edu" "anonymous" "anonymous" "Webster" "Access Webster" "")
("foobar.colorado.edu" "anonymous" "anonymous" "X-Windows/TeX" "BDF Fonts/XTeX" "view dvi file within X-Windows")
("princeton.edu" "anonymous" "china.primitive@divineland" "Graphics/TeX to Document C Code (Spiderweb)/Music/Tex" "CWEB/WEB/Rayshade 3.0/Rayshade 4.0/Spiderweb" "has next wares AND is primary src for greek fonts for TeX. spiderweb is used to document c code within latex") 
("isatis.ensmp.fr" "anonymous" "china.primitive@divineland" "Parallel Computing" "Ease" "source code for parallel language ease is here") 
("titania.mathematik.uni-ulm.de" "anonymous" "china.primitive@divineland" "Parallel Computing" "Database of Parallel Info" "source code for parallel language ease is here") 
("chem.bu.edu" "anonymous" "china.primitive@divineland" "?" "?" "") 
("ftp.cs.titech.ac.jp" "anonymous" "china.primitive@divineland" "Japanese" "Nihongo Emacs" "") 
("freja.diku.dk" "anonymous" "china.primitive@divineland" "Sun RPC/NN/xypic" "Sun RPC/NN/xypic" "Denmark") 
("iesd.auc.dk" "anonymous" "china.primitive@divineland" "Statistics/Contingency Tables/EM Algorithm/TeX" "Statistics/Contingency Tables/EM Algorithm/AUC-TeX" "Comprehensive Environment for writing latex files") 
("hercules.csl.sri.com" "anonymous" "anonymous" "TeX" "LaTeX figures" "")
("herky.cs.uiowa.edu" "anonymous" "anonymous" "Mathematics" "Theorem Prover" "")
("xpssun.gmd.de" "anonymous" "china.primitive@divineland" "Knowledge Representation" "Mobal" "Mobal1.0 requires a Sun4, Openwindows2.0, from Sun and
HyperNeWS1.4 from Turing Institute, Glasgow 
Contact newsdev@turing.ac.uk for information on how to get HyperNeWS.

Mobal is a sophisticated system for developing operational models of
application domains. It integrates a manual knowledge acquisition and 
inspection environment, a powerful inference engine, machine learning 
methods for automated knowledge acquisition, and a knowledge revision tool.

By using Mobal's knowledge acquisition environment, you can
incrementally develop a model of your domain in terms of logical facts
and rules.  You can inspect the knowledge you have entered in text or
graphics windows, augment the knowledge, or change it at any time.
The built-in inference engine can immediately execute the rules you
have entered to show you the consequences of your inputs, or answer
queries about the current knowledge.  Mobal also builds a dynamic sort
taxonomy from your inputs.  If you wish, you can use machine learning 
methods to automatically discover additional rules based on the facts
that you have entered, or to form new concepts.  If there are
contradictions in the knowledge base due to incorrect rules or facts,
there is a knowledge revision tool to help you locate the problem and fix
it.
") 
("funic.funet.fi" "anonymous" "china.primitive@divineland" "X-Windows" "MandelSpawn" "in pub/X11/contrib/ms-0.05.tar.Z") 
("sunic.sunet.se" "anonymous" "china.primitive@divineland" "?" "?" "") 
("wuarchive.wustl.edu" 
"anonymous" 
"china.primitive@divineland" 
"Tex/Graphics/Gifs/Ray Tracing/Radiosity" 
"Astronomical Observatory FITS Viewer/Ray Tracing Bibliography/Radiosity Bibliography/Salem/CellSim/Graphics Gems/Quickdraw/Popi/Ray Tracing News/ColorQuant/DKBTracer/PVRay/DRT/Ohtas Ray Tracer/QRT/RayScene/VORT Ray Tracer/VM-pRAY/Polygon Processor/Sphigs/SRGP/Wave/Sphere/TeXhax digest" "major TeX archive. has TeXhax digest") 

("tutserver.tut.ac.jp" "anonymous" "brannon@jove.cs.caltech.edu" "Elisp" "Elisp" "") 
("ftp.ics.osaka-u.ac.jp" "anonymous" "japan.primitive@kiroshi" "X-Windows/Tex" "ChTex/ArabTeX/Image" "huge directory of styles. including one for the thesis.") 
("itstd.sri.com" "anonymous" "china.primitive@divineland" "?" "?" "") 
("nic.nyser.net" "anonymous" "china.primitive@divineland" "Gnu" "Gnu" "Major Gnu Archive") 
("jaguar.utah.edu" "anonymous" "ssss.s" "Gnu" "Gnu" "Major Gnu Archive")

("ftp.Hawaii.edu" "anonymous" "rickrubin@defjam.rec" "Natural Language Processing" "Text to Phoneme Program" "/pub/linguist/phone.tar.Z")

("nl.cs.cmu.edu" "anonymous" "rickrubin@defjam.rec" "Graphics/Ray Tracing" "Utah Raster Toolkit" "")
("spade.pc.cs.cmu.edu" "anonymous" "rickrubin@defjam.rec" "Natural Language Processing" "English Parser" "/usr/sleator/public")
("ajpo.sei.cmu.edu" "anonymous" "anonymous" "ADA" "ADA" "All the ADAD you could ask for")
("mcsun.eu.net" "anonymous" "yoyo.yoyo" "Gnu" "Gnu" "Major Gnu Archive")

("mango.miami.edu" "anonymous" "haha.ahah" "Gnu" "VMS G++" "Major Gnu Archive")
("louie.udel.edu" "anonymous" "china.primitive@divineland" "?" "?" "")


("rdss.ucar.edu" "anonymous" "china.primitive@divineland" "?" "?" "try this out to update its record") 


("ahkcus.org" "anonymous" "china.primitive@divineland" "Chinese News/Chinese Word Processors" "Big5->GB/Readnews" "contains a lot of Chinese word processors both for PC's and other")
("cs.ualberta.ca" "anonymous" "china.primitive@divineland" "Chinese Chess" "Chinese Chess" "")
("134.114.64.24" "anonymous" "china.primitive@divineland" "Genealogy" "Genealogy" "")



("mindseye.berkeley.edu" "anonymous" "china.primitive@divineland" "Japanese Terminal Emulator" "Japanese Terminal Emulator" "For the PC, you should use a terminal emulator hterm written by
Hirano-san at Tokyo U if your PC has VGA or EGA.  If your display
adapter is Hercules or CGA (!), then you have to settle with my
KD (kanji driver) + MS-Kermit.  Both of these are free and can
be obtained by anonymous FTP from mindseye.berkeley.edu
[128.32.232.19].  Both supports full JIS-level 2 Japanese
character set (6000+ characters).

If you use regular 'rn' be sure to use a command w | more -f
at the first pause after news header for an article is displayed.

- --
Izumi Ohzawa             [ $BBg_78^=;(B ]
USMail: University of California, 360 Minor Hall, Berkeley, CA 94720
Telephone: (510) 642-6440     Fax:  (510) 642-3323
Internet: izumi@pinoko.berkeley.edu (NeXT Mail OK)
") 
("peoplesparc.berkeley.edu" "anonymous" "china.primitive@divineland" "LISP/Mathematics" "Lisp Mathematica (mma)" "mathematica-like system written in Allegro common lisp") 
("scam.berkeley.edu" "anonymous" "china.primitive@divineland" "?" "?" "") 
("math.berkeley.edu" "anonymous" "booty.jones@funkyfunk" "TeX" "ChTex/ArabTeX/Image" "contains the addendum to the TeX FAQ in pub/raymond") 
("sprite.berkeley.edu" "anonymous" "booty.jones@funkyfunk" "Programming Languages" "TCL" "Paper in USENIX 89") 

("rusinfo.rus.uni-stuttgart.de" "anonymous" "anonymous" "TeX" "Babel" "
   Is it possible to build an lplain.fmt with to different hyphen-files? I
   get the impression that TeX 3.x supports some sort of a multilingual
   scheme, providing a way to easily swith between languages in the same
   document.

You should look at the Babel system, by Johannes Braams, obtainable by
mail from listserv@hearn.bitnet, listserv@dhdurz1.bitnet,
mail-server@rusinfo.rus.uni-stuttgart.de, or by anonymous ftp from
rusinfo.rus.uni-stuttgart.de [129.69.1.12].
")

("simtel20.army.mil" "anonymous" "blessedallah@mecca" "Mathematics" "SymbMath" "        SymbMath 1.4: A Symbolic Calculator with Learning

			by Dr. Weiguang HUANG
	5/6 Cara Road, Geelong, Vic. 3216, Australia

	SymbMath, (an abbreviation for Symbolic Mathematics), is not
only a symbolic calculator but also an expert system that can solve 
symbolically mathematical problems. SymbMath performs symbolic formula, 
as well as exact numeric computation. It can manipulate complicated 
formulas and return answers in terms of symbols, formulas and exact 
numbers, not just floating-point numbers. Symbolic mathematics is also 
called Computer Algebra, Symbolic Manipulation, Algebraic Manipulation, 
Symbolic Computation, or Algebraic Computation. 
	There are a number of software packages for symbolic 
computation, e.g. MACSYMA, Reduce, MAPLE, Mathematica, MicroCalc, etc. 
However, most of them require large memory, e.g. Mathematica requires 
at least 4 MBytes of RAM. MicroCalc is available for small IBM PC. 
MicroCalc provides graphical or numerical answers, and recognizes 
special symbols as constants and variables, but outputs formula only 
for derivatives.
	SymbMath is different from most of other software. (1) It 
only require 640 KBytes RAM, so it can run on small IBM PC under 
MS-DOS. (2) It is able to learn from human as machine learning. If 
users only show it one thing (e.g. one integral) without writing any 
line of program, it will learn many relative knowledges (e.g. many 
integrals) although it has no these predefined knowledges before. (3) 
Users easily edit the library (e.g. integral tables) in the formula 
format. (4) It can do something that other software cannot do.
	Its capabilities include facilities to provide analytical and 
numerical answers for:
	1. Differentiation: derivatives, higher order derivatives, 
partial derivatives, mixed derivatives, total derivatives, implicit
differentiation.
	2. Integration: indefinite integration, definite integration, 
multiple integration, infinite integration, parametric integration, 
iterated integration, line integration, surface integration, 
discontinuous integration, implicit integration.
	3. Solution of equations: polynomial equations, algebraic 
equations, systems of equations, differential and integral equations.
	4. Manipulation of expressions: simplification, factorisation, 
expansion, substitution, evaluation, user-defined functions, built-in 
standard functions (including the error function).
	5. Calculation: exact and floating-point numerical computation 
of integer, rational, real and complex numbers in the range from 
- -infinity to infinity, even with different units.
	6. Limits: real limits, complex limits, one-sided limits, 
including the indeterminate forms, (e.g. 0/0, infinity/infinity, etc.).
	7. Chemical calculation: the molecular weights, atomic weights, 
and concentrations, by entering the symbols of the chemical elements.
	8. Chemical reactions: inorganic and organic chemical reactions.
	9. Others such as extrema, Taylor series, lists, arrays, vectors,
matrices, sum, product, etc.
	Its features also include:
	1. Programming as an interpreter.
	2. Interface with other software (e.g. with PlotData for graph).
        3. Outputting two-dimentional displays, and the BASIC format.
	4. On-line help, on-line manual.
	5. Syntax check and the line editor for input.
	Shareware version of SymbMath, SM14A.ZIP, is available from 
the Calculator directory in SIMTEL20 on anonymous FTP sites.
") 

("ymir.claremont.edu" "anonymous" "booty.boot@funkdungeon" "Tex/Raster Fonts/MetaFonts" "dvips/dvitops/Web/Cweb/Spiderweb/MusicTeX" "2nd largest tex archive in the WORLD. Aggresively tracks down packages") 
("hmcvax.claremont.edu" "anonymous" "booty.boot@funkdungeon" "Ethnic TeX" "Cyrillic Fonts" "")
("archive.cs.ruu.nl" "anonymous" "pumpdatjam@bussit.edu" "Perl" "Perl Reference Guide" "Utrecht Univ, the Netherlands") 
("helios.cc.gatech.edu" "anonymous" "china.primitive@divineland" "Distributed Computing" "ChTex/ArabTeX/Image" "this is where a distributed lisp program is supposedly located") 
("schizo.samsun.com" "anonymous" "thatsmy@kinda.site" "X-Windows/Tex" "ChTex/ArabTeX/Image" "comp.sources.x") 
("wuarchive.wustl.edu" "anonymous" "toomuch@for.more.mortals" "X-Windows/Tex/Gifs" "ChTex" "[128.252.135.4] ... mirror wmsr-simtel20.army.mil") 
("yallara.cs.rmit.oz.au" "anonymous" "sowhassup@do.sumpin.den" "TeX" "Image" "[131.170.24.42] ... mirrors labrea for those down under") 
("godzilla.cgl.rmit.oz.au" "anonymous" "anonymous" "Cscheme" "Cscheme 6.2" "includes a GL interface")
("niord.shsu.edu" "anonymous" "gohead@witchy.bad.sef" "Tex" "ChTex/ArabTeX/Image" "run by george greenwade, a man on the up and up") 
("research.att.com" "anonymous" "oooochile@is.you.crazy" "X-Windows/Tex" "ChTex/ArabTeX/Image" "TeX, gcc, ghostscript, f2c") 

("laas.laas.fr" "anonymous" "anonymous" "X-Windows/Emacs Hacks" "ISO Latin-1 Emacs Mode" "input the ISO Latin-1 character set in a convenient way")
("convex.com" "anonymous" "anonymous" "Perl" "Perl" "")

("peace.waikato.ac.nz" "anonymous" "hehheh@that.aint.funny.fool" "X-Windows/Tex" "ChTex/ArabTeX/Image" "x11r4") 
("maddog.llnl.gov" "anonymous" "attaboy@i.feel.good.yah.yah" "X-Windows/Tex" "ChTex/ArabTeX/Image" "AWM X Tutorial, PCP") 
("lll-crg.llnl.gov" "anonymous" "bboys@the.funk.shack.sack" "X-Windows/Tex" "ChTex/ArabTeX/Image" "X11R4") 

("iraun1.ira.uka.de" "anonymous" "sowhat@in.sig.ni" "X-Windows/Tex" "ChTex/ArabTeX/Image" "GNU X11 comp.sources.unix") 
("qusuna.qucis.queensu.ca" "anonymous" "superkid@mo.jo.land" "Language Prototyping" "ChTex/ArabTeX/Image" "Release 5.3 of TXL, a rapid prototyping system for programming languages
       and program transformations, is now available via anonymous ftp from
       qusuna.qucis.queensu.ca (130.15.1.100) in the directory 'txl'.  Release
       5.3 fixes a number of bugs in release 5.2, in particular a bug
       in patterns targeted at left-recursive productions, and adds
       support for arbitrary comment conventions, in particular for C
       and C++ commenting and the %operators.
TXL 5.3 is distributed in portable ANSI C source form only, and you must
compile it for your particular Unix system.  It has been tested on all of
the VAX, Sun/3, Sun/4, NeXT, and DECstation MIPS, and meets 'gcc -ansi
- -pedantic' so should compile on almost anything.
Full information on the details of fetching TXL can be obtained by
fetching the 00README file, like so:

    myunix% ftp 130.15.1.100
    Connected to 130.15.1.100.
    220 qusuna FTP server (Version 5.56 Thu Apr 18 13:08:27 EDT 1991) ready.
    Name (130.15.1.100:cordy): anonymous
    331 Guest login ok, send ident as password.
    Password: cordy@myunix
    230 Guest login ok, access restrictions apply.
    ftp> cd txl
    250 CWD command successful.
    ftp> get 00README
    200 PORT command successful.
    150 Opening ASCII mode data connection for 00README (1688 bytes).
    226 Transfer complete.
    local: 00README remote: 00README
    1731 bytes received in 0.04 seconds (42 Kbytes/s)
    ftp> quit
    221 Goodbye.
    myunix% 

I will attempt to service any email requests from those who do not have
FTP access as well, but such requests will be serviced very slowly over
the next couple of months and I don't have time to try to fix any email
addresses that don't work from my site directly as sent to me.

For those of you who have forgotten what TXL is good for, I have
reproduced the TXL 5.3 ABSTRACT file below.

Jim Cordy
- ---
Prof. James R. Cordy				cordy@qucis.queensu.ca
Dept. of Computing and Information Science	James.R.Cordy@QueensU.CA
Queen's University at Kingston			cordy@qucis.bitnet
Kingston, Canada K7L 3N6			utcsri!qucis!cordy


- ----- TXL ABSTRACT -----

Subject: TXL 5.3, a Rapid Prototyping Tool for Computer Languages

Release 5.3 of TXL: Tree Transformation Language is now available via
anonymous FTP from qusuna.qucis.queensu.ca (130.15.1.100).

TXL 5.3, (c) 1988-1991 Queen's University at Kingston
- -----------------------------------------------------
Here's the language prototyping tool you've been waiting for!  TXL is a
generalized source-to-source translation system suitable for rapidly
prototyping computer languages and langauge processors of any kind.  It
has been used to prototype several new programming languages as well as
specification languages, command languages, and more traditional program
transformation tasks such as constant folding, type inference and source
optimization.  

TXL is NOT a compiler technology tool, rather it is a tool for use by
average programmers in quickly prototyping languages and linguistic tasks.
TXL takes as input an arbitrary context-free grammar in extended BNF-like
notation, and a set of show-by-example transformation rules to be applied
to inputs parsed using the grammar.  TXL will automatically parse inputs
in the language described by the grammar, no matter if ambiguous or
recursive, and then successively apply the transformation rules to the
parsed input until they fail, producing as output a formatted transformed
source.  

TXL is particularly well suited to the rapid prototyping of parsers (e.g.,
producing a Modula 2 parser took only the half hour to type in the Modula
2 reference grammar directly from the back of Wirth's book), pretty
printers (e.g., a Modula 2 paragrapher took another ten minutes to insert
output formatting clues in the grammar), and custom or experimental
dialects of existing programming languages (e.g., Objective Turing was
prototyped by transforming to pure Turing and using the standard Turing
compiler to compile the result).

TXL 5.3 comes with fully portable ANSI C source automatically translated
>from the Turing Plus original, self-instruction scripts and a pile of
examples of its use in various applications.  
- -- 
Send compilers articles to compilers@iecc.cambridge.ma.us or
{ima | spdcc | world}!iecc!compilers.  Meta-mail to compilers-request.") 
("ftp.cs.umb.edu" "anonymous" "fontanem@afadfalkj" "X-Windows/Tex" "ChTex/ArabTeX/Image" "Karl Berry's font naming scheme v1.0 192.12.26.23 
       /tex/fontname/fontname.texi") 

("hpserv1.uit.no" "anonymous" "yobustdis@homeboys.is.we.be" "Ray Tracing/Gifs" "Rayshade 3.0" "EXCELLENT SITE... tons of Gifs. up-to-date, huge, ls-lR.Z files in ~...HP stuff, X11, unix, etc")
("hydra.helsinki.fi" "anonymous" "slinkytoy@try.that.sucka" "X-Windows/Sun/Unix" "comp.sources.misc/comp.sources.sun/comp.sources.unix" "misc, TeX, X") 
("ics.uci.edu" "anonymous" "anonymous" "hashing" "Perfect Hash Gen" "")
("hemuli.tik.vtt.fi" "anonymous" "powerfulpexster@thelemic.magick" "X-Windows/Tex" "ChTex/ArabTeX/Image" "WorldMap X bind 4.8 finger (thats what the list said)") 
("extro.ucc.su.oz.au" "anonymous" "yeo@men.irish.iud" "X-Windows/Tex" "ChTex/ArabTeX/Image" "images, gnu, icon, kermit, and so forth, ghostscript patches") 
("cs.toronto.edu" "anonymous" "nothing@void.edu" "X-Windows/Tex" "ChTex/ArabTeX/Image" "X applications here") 
("titan.rice.edu" "anonymous" "ohman@dont.get.hungup" "X-Windows/Tex" "ChTex/ArabTeX/Image" "ascii2german.sh by dorai@egeria.rice.edu in /public 
 ascii2german converts " ae " " oe " " ue " and " ss " to the appropriate German
characters.  Not infallible due to ambiguities like Masse and
Ma{ss}e. another program of use is diac.sh") 
("e-math.ams.com" "anonymous" "whatwasthat@crazy.squeaking.sound" "AMSTeX" "AMSTeX" "AMSFonts, AMS-LaTeX, AMSTeX  NOTE: The AMS doesn't alert other 
       archive sites when there are changes to the contents 
       of e-math.ams.com.  Consequently, other sites may be slightly behind.
       Administrative contact for E-MATH:  tech-support@math.ams.com.") 
("ftp.cs.umb.edu" "anonymous" "here@we.are.com" "X-Windows/Tex" "ChTex/ArabTeX/Image" "per karl berry's article in TUGboat .. also said 'adobe font names'") 
("ftp.cs.toronto.edu" "anonymous" "brazil@s.amer.edu" "X-Windows/Tex" "AIList/Sun-Spots" "[128.100.1.105] ... Mirrors the clarkson style archives for 
       Canadian users contains pub/TeX/tex-styles") 
("cs.ubc.ca" "anonymous" "anonymous" "Mailing Lists/Graphics/TeX" "MuTeX/Raster Files/Huge Maling List Index (list-of-lists)" "")
("cse.ogc.edu" "anonymous" "anonymous" "Neuro-Evolution/Speech Recognition" "Neuro-Evolution/Speech Recognition" "")
("sun.soe.clarkson.edu" "anonymous" "shamba@gupta.edu" "X-Windows/Tex" "ChTex/ArabTeX/Image" "v2.1, pub/transfig/fig.tar.Z     Fig is a graphics editor under
xo       SunView.  Its output can be translated via TransFig [qv] into
       Postscript, pic, LaTeX picture envirnoment, PiCTeX, EEPIC, TeXtyl etc.") 
("svax.cs.cornell.edu" "anonymous" "yeahboy@bussit.edu" "X-Windows/Tex" "ChTex/ArabTeX/Image" "contains fig under pub/fig. this allows graphic editing which
       can be put in latex by transfig which is also here. and 
       sun.soe.clarkson.edu") 
("rugrcx.rug.nl" "anonymous" "malawamaoi@aboriginalaustralia" "Tex" "ChTex/ArabTeX/Image" "devanagari font from TeX. pub dir") 
("130.88.200.7" "anonymous" "idosay@old.chap.in.the.lorry" "X-Windows/Tex" "ChTex/ArabTeX/Image" "United Kingdom Chum...X11R5 dist, udda stuph") 
("crl.dec.com" "anonymous" "anony@mous.edu" "X-Windows/Tex" "ChTex/ArabTeX/Image" "X11R4") 
("a.cs.uiuc.edu" "anonymous" "letsgetdemwares@leech.line" "TeX" "TeX" "TeX related stuph")
("emr.cs.uiuc.edu" "anonymous" "china.primitive@divineland" "Elisp/Tex" "Calendar.el/Diary.el/tree.sty" "tree.sty and home site for Ed Reingold's calendar") 
("st.cs.uiuc.edu" "anonymous" "china.primitive@divineland" "Smalltalk Archive" "Smalltalk Archive" "  	1) send email to archive-server@st.cs.uiuc.edu
	    with a message like:
	
	 	path  yourname@your.internet.address
	 	help
") 
("vmd.cso.uiuc.edu" "MARK" "" "Quran" "Quran" "The login is 'MARK'
the password is 'IPFS'
The type 'ACCT QUOTE IRAN' for read password.

Quran.txt is the text file
desquran.zip is the PC program (includes the compressed version of the text).

I am also working on the Arabic version of the program.  I have the text
and I can use it with my word processor.  However, the text has no valves
(i'raab).

I hope you find this Info useful.

Ala U_c

p.s. The translation is by H.M. Shakir.
")
("128.148.31.66" "anonymous" "tb06@pl118f.cc.lehigh.edu" "X-Windows" "SPHIGS/SRGP" "sphigs and srgp are in pub... for an alternative to xlib") 
("wilma.cs.brown.edu" "anonymous" "tb06@pl118f.cc.lehigh.edu" "Hypertext" "Hyperbole" "") 

("128.252.135.4" "anonymous" "tb06@pl118f.cc.lehigh.edu" "Recipes" "Recipes" "") 
("sulaw.law.su.oz.au" "anonymous" "tb06@pl118f.cc.lehigh.edu" "Recipes" "Recipes" "") 


("slopoke.mlb.semi.harris.com" "anonymous" "hello@my.pretties" "Magick/Gifs" "ChTex/ArabTeX/Image" "Aleister Crowley GIF's and other magickal items") 
("ncube10.csee" "asaha" "" "Parallel Machine" "ChTex/ArabTeX/Image" "parallel machine")
("lynx.cs.orst.edu" "anonymous" "nopssreq" "Parallel Computing" "Parallax" "Parts of the book intro to parallel computing by lewis and el-rewini <prentice-hall, publishers> are available on the server. also the parallel software is available")

("archie.mcgill.ca" "anonymous" "anon" "FTP Database" "Xarchive Standalone" "has an x version non-x version and a perl version. all in the directory archive/clients")

("mcnc.mcnc.org" "anonymous" "nopssreq" "Parallel Computing" "SIMD Simulator" "/pub/blitzen ... email to brain@adm.csc.ncsu.edu")
("info.mcs.anl.gov" "anonymous" "nopssreq" "Parallel Computing" "PCN Parallel Language" "/pub/pcn")
("sh.cs.net" "anonymous" "nopssreq" "Gnu" "Gnu" "Major Gnu Archive")
("ftp.reed.edu" "anonymous" "nopssreq" "ftp" "ange-ftp mail archive" "")
("karazm.math.uh.edu" "anonymous" "attababe@charlies.fave.edu" "Parallel Language" "Parallel C/Parallel Fortran" "pub/Parallel/Tools/pc.1.1.1.tar.Z
C parallel  fortran is also being developed at Univ. of Houston. Contact
scott@uh.edu for information regarding pf.
J. Eric Townsend - jet@uh.edu - UH Dept of Mathematics
vox: (713) 749-2126  '91 CB750, DoD# 0378, TMRA# 27834")))

					; major code
					; segment:
					;
					; General-Functions

; ; these functions are called throughout the code by other
; ; functions. I am extremely grateful for the code from
; ; tim mcdaniel which was immensely useful in this part of
; ; the program

(defun get-record (index list)
  (if (position-exceeds-list-length index list)
      ()
  (nth index list)))

(defun get-field (index list)
  (if (position-exceeds-list-length index list)
      ()
  (nth index list)))

(defun return-up-to (char string)
 "return all chars up to but not including CHAR in STRING"
 (get-buffer-create "*hack-buffer*")
 (set-buffer "*hack-buffer*")
 (erase-buffer) 
 (goto-char (point-min))
 (insert string)
 (goto-char (point-min))
 (if (re-search-forward (concat ".?" char) (point-max) t)
     (progn
       (setq separator-in-string t)
       (buffer-substring (point-min) (1- (point))))
   (progn
     (setq separator-in-string nil)
     string)))

(defun search-for-substring (substring string)
 "search for SUBSTRING in STRING"
 (setq tmpctr 0) 
 (get-buffer-create "*hack-buffer*")
 (set-buffer "*hack-buffer*")
 (erase-buffer) (goto-char (point-min))
 
(insert string) (goto-char (point-min))
 (word-search-forward substring (point-max) t))

(defun create-alist-from (index list)
  (setq tmpctr 0) (setq tmplist '())
  (while (< tmpctr (length list))
    (setq tmplist (cons
		   (list (get-field index (get-record tmpctr list)) 
		   "dummy") tmplist))
    (setq tmpctr (1+ tmpctr))))

(defun delq-equal (element lst)
  "deletes by side effect all occurrences of element as a member of list.
the modified list is returned.  if element is not in list, list is
returned unchanged.  'equal' is the equality tester.  if the first
member of list is 'equal' to element, there is no way to remove it by
side effect; therefore, write
   (setq foo (del-equal element foo))
to be sure of changing foo.

c.f. the builtin function 'delq', which uses 'eq' and deletes all occurrences.
c.f. the function 'delq-first-equal', which uses 'equal' and deletes
     only the first occurrence."

  (condition-case err
      (progn
        (let
            (
             (scan (cdr lst))
             (prevscan lst)
            )
          (while scan
            (if (equal element (car scan))
                (setcdr prevscan (cdr scan))
              (setq prevscan scan)
            ) ; if
            (setq scan (cdr scan))
          ) ; while
        ) ; let
        ;; return value:
        (if (equal element (car lst))
            (cdr lst)
          lst
        ) ; if
      ) ; progn
    (wrong-type-argument
     (signal 'wrong-type-argument (list 'delq-equal lst)))
  ) ; condition-case
) ; defun delq-equal

(defun delq-first-equal (element lst)
  "deletes by side effect the first occurrence of element as a member of list.
if element is not in list, list is returned unchanged.  the modified
list is returned.  'equal' is the equality tester.  if the first
member of list is 'equal' to element, there is no way to remove it by
side effect; therefore, write
   (setq foo (del-first-equal element foo))
to be sure of changing foo.

c.f. the builtin function 'delq', which uses 'eq' and deletes all occurrences.
c.f. the function 'delq-equal', which uses 'equal' and deletes all occurrences."

  (condition-case err
      (if (equal element (car lst))
          (cdr lst)                        ; return value
        (let
            (
             (scan (cdr lst))
             (prevscan lst)
            )
          (while
              (and scan (not (equal element (car scan))))
            (setq
             prevscan scan
             scan (cdr scan)
            ) ; setq
          ) ; while
          (if scan
              (setcdr prevscan (cdr scan))
          ) ; if
          lst ; return value
        ) ; let
      ) ; if
    (wrong-type-argument
     (signal 'wrong-type-argument (list 'delq-first-equal lst)))
  ) ; condition-case
) ; defun delq-first-equal

(defun list-to-set (lst)
  "return list with duplicate elements reduced to one occurrence each.
list-to-set of a set does nothing.

a set is an unordered collection of items; each item of a set occurs
only once.  sets are represented as gnu emacs lisp lists (the elements
are usually numbers or symbols).  two elements are considered
\"identical\" or \"duplicates\" or \"the same\" if the 'equal' function
returns 't' for them."

  (let*
      (
       (retval
        (condition-case err
            (copy-sequence lst)
          (wrong-type-argument
           (signal 'wrong-type-argument (list 'list-to-set lst)))
        ) ; condition-case
       )
       (scan retval)
      )
    (condition-case err
        (while scan
          (delq-equal (car scan) scan)
          (setq scan (cdr scan))
        ) ; while
      (wrong-type-argument
       (signal 'wrong-type-argument (list 'list-to-set lst)))
    ) ; condition-case
    retval
  ) ; let*
) ; defun list-to-set

(defun memq-equal (element lst)
  "locates the first occurrence of element as a member of list.
returns the tail of list which has a car 'equal' to element.  returns
nil if element is not in list.

c.f. the builtin function 'memq', which uses 'eq'."
  (let
      (
       (scan lst)
      )
    (condition-case err
        (while
            (and scan (not (equal element (car scan))))
          (setq scan (cdr scan))
        ) ; while
      (wrong-type-argument
       (signal 'wrong-type-argument (list 'memq-equal lst)))
    ) ; condition-case
    scan                              ; return value
  ) ; let
) ; defun memq-equal

(defun position-exceeds-list-length (position list)
 (> position (length list)))

(defun Insert (element list position)
  "insert lisp-object in list at position indexed from 0. 
runtime examples:

(insert 'yellow-red '(3 4 5) 0)
(yellow-red 3 4 5)
(insert 'yellow-red '(3 4 5) 1)
(3 yellow-red 4 5)
(insert 'yellow-red '(3 4 5) 2)
(3 4 yellow-red 5)
(insert 'yellow-red '(3 4 5) 3)
(3 4 5 yellow-red)
(Insert 'yellow-red '(3 4 5) 4)
chosen position is greater than list length"

  (setq i-ctr 0)
  (if (position-exceeds-list-length position list)
      (format "chosen position is greater than list length")
    (progn
      (setq resultlist '())
      (while (< i-ctr position)
	(setq resultlist (cons (nth i-ctr list) resultlist))
	(setq i-ctr (1+ i-ctr)))
      (setq resultlist (cons element resultlist))
      (while (< i-ctr (length list))
	(setq resultlist (cons (nth i-ctr list) resultlist))
	(setq i-ctr (1+ i-ctr)))
      (setq resultlist (reverse resultlist)))))


					;				   
					; major code 
					; segment 
					;
					; interactive-commands
					;

;;; commands the user can call by hitting a key defined in
;;; the keymap in the function ftp+mode. beginnners, notice
;;; that each of these functions must begin with an interactive
;;; declaration. also note that the inclusion of an interactive
;;; declaration is what distinguishes a function from a command

(defun ftp+()
 "command to invoke the program."
  (interactive)
  (setq ftp+buffer-count (+ ftp+buffer-count 1))
  (setq ftp+transfer-mode "Ascii")
  (ftp+set-status ftp+binary-mode-index ftp+buffer-count nil)
  (ftp+set-status ftp+connected-index ftp+buffer-count nil)
  (setq ftp+Interaction-bupha (concat "*" ftp+buffer-count))
  (aset process-objects-vector ftp+buffer-count
	(start-process ftp+Interaction-bupha ftp+Interaction-bupha "ftp"))
  (get-buffer-create ftp+Interaction-bupha)  
  (switch-to-buffer ftp+Interaction-bupha)
  (ftp+choose-site-decrement)
  (ftp+choose-site)
  (ftp+mode)
  (run-hooks 'ftp+mode-hook))

(defun ftp+hyper-connect()
  (interactive)
  (setq ftp+tmp (region-beginning))
  (setq ftp+hc-tmp (buffer-substring ftp+tmp (region-end)))
  (ftp+)
  (setq ftp+current-site ftp+hc-tmp)
  (ftp+open-connection))


(defun same-site-other-window()
  (interactive)
  (ftp+)
  (ftp+open-connection))

(defun choose-site-by-topic ()
  (interactive)
  (choose-site-using "Topic" TOPIC-INDEX)
  (ftp+scroll-to-chosen-site))

(defun choose-site-by-filename ()
  (interactive)
  (choose-site-using "Filename" FILENAME-INDEX)
  (ftp+scroll-to-chosen-site))

(defun ftp+change-directory()
 "cd to the directory that the user selected by going through these steps:
  1) typing d to get a directory
  2) using c-p and c-n to move where he wanted to... it doesnt matter
     where on the line the cursor is... my program is a smart thang.
  3) typing g to 'get' the directory.. in this case change to it"
  (setq ftp+command-string (concat "cd " ftp+desired-file "\n"))
  (process-send-string (process-name (get-current-process-object)) ftp+command-string)
  (directory))

(defun ftp+cd-up()
 "go up a directory. initially i sent the string cdup but some
  ftp lines dont recognize this so i now send the explicit u__x command"
  (interactive)
  (end-of-buffer)
  (setq ftp+command-string (concat "cd ..\n"))
  (process-send-string (process-name (get-current-process-object)) ftp+command-string))

(defun ftp+get-file()
 "get-file is executed when the user types a g --- at least as long as the
  bindings dont change. getting is both downloading and also changing 
  directories."
  (interactive)
  ;; get the filename and also the first character on the line of the file
  ;; so we can tell if the file is a directory or actual file..
  ;; in other drwxr-xr-x is directory and will return a d while
  ;; -r-xr-xr-x will return - so it isnt a directory
  (ftp+pull-out-name) 
  (end-of-buffer)
  (if (equal ftp+first-char-on-line "d")
	(ftp+change-directory)
    (ftp+download-file)))

(defun ftp+download-file()
 "this sends the string get filename to the ftp process."
  (setq ftp+command-string (concat "get " ftp+desired-file "\n"))
  (message "Downloading %s..." ftp+desired-file)
  (process-send-string (process-name (get-current-process-object)) ftp+command-string))
  
(defun ftp+password-entry()
 "send the password across. this is done manually by you presssing p
  until if get up the libido to rite a philter for this program"
  (interactive)
  (setq ftp+password (concat (read-from-minibuffer "password (default listed) " ftp+current-password) "\n"))
  (process-send-string (process-name (get-current-process-object)) ftp+password))

(defun ftp+send-file()
 "uplaod to remote ftp line"
  (interactive)
  (setq ftp+sftemp (concat "send " (read-from-minibuffer "Send What File: ") "\n"))
  (process-send-string (process-name (get-current-process-object)) ftp+sftemp))

(defun ftp+open-connection()
 "open a connnection to a site"
  (interactive)
  (setq ftp+binary-mode nil)
  (setq ftp+site-to-open  (concat "open " ftp+current-site "\n"))
  (process-send-string (process-name (get-current-process-object)) ftp+site-to-open)
  (process-send-string (process-name (get-current-process-object)) ftp+current-username)
  (process-send-string (process-name (get-current-process-object)) "\n")
  (ftp+set-status ftp+connected-index (ftp+index-of-current-buffer) t)
  (message "C-h m to list commands while connected , TAB h for quick help")
  (ftp+mode))

(defun close-connection()
 "close a connection to a site. since each new ftp defaults to ascii mode
  of transfer, reset the transfter mode to ascii."
  (interactive)
  (process-send-string (process-name (get-current-process-object)) "close\n")
  (setq ftp+transfer-mode "Ascii")
  (ftp+set-status ftp+connected-index (ftp+index-of-current-buffer) nil)
  (ftp+mode))

(defun disconnect()
 "tab-c invokes this"
  (interactive)
  (ftp+set-status ftp+connected-index (ftp+index-of-current-buffer) nil)
;  (Process-send-string (process-name (get-current-process-object)) "bye\n")
  (kill-buffer (current-buffer))
  (delete-other-windows)
  (run-hooks 'ftp+disconnect-hook))

(defun describe()
 "call view-buffer to view detailed info a particular site."
  (interactive)
  (setq ftp+description     (get-field COMMENT-INDEX (get-record ftp+list-ctr ftp+site-list )))
  (get-buffer-create "ftp+description-buffer")
  (set-buffer "ftp+description-buffer") (erase-buffer)
  (insert ftp+description)
  (fill-region (point-min) (point-max))
  (min-size-window "ftp+description-buffer")
  (pop-to-buffer ftp+Interaction-bupha))

(defun ftp+scroll-description-buffer (&optional direction)
  "scroll the window displaying the description buffer"
  (interactive "p")
  (let ((here (current-buffer)))
    (unwind-protect
	(progn
	  (pop-to-buffer "ftp+description-buffer")
	  (condition-case ignore (scroll-up)
	    (end-of-buffer (progn (beep) (goto-char (point-min)))))
	  (pop-to-buffer here))
      (pop-to-buffer here))))

(defun min-size-window (buff &optional max-height)
  "display BUFF in as small a window as possible, with an upper limit
of no more than half the screen height (including status line), unless
an optional argument MAX-HEIGHT is given."
  (interactive "B")			;for test purposes
  (let ((max-height (or max-height 12))
	(here (current-buffer))
	(pop-up-windows t)
	(window-min-height 2))
    (pop-to-buffer buff)
    (goto-char (point-min))
    (shrink-window (- (window-height) (max (vertical-motion max-height) 1) 1))
    (goto-char (point-min))
    (pop-to-buffer here)))

(defun directory()
 "list the goodies on the target site"
  (interactive)
  (process-send-string (process-name (get-current-process-object)) "ls -l\n")
  (goto-char (point-max)))

(defun ftp+binary-mode-toggle()
  (interactive)
  (setq m (ftp+get-status ftp+binary-mode-index (ftp+index-of-current-buffer)))
  (ftp+set-status ftp+binary-mode-index (ftp+index-of-current-buffer) (not m))
  (if m 
      (progn
	(setq dummy "ascii\n")
	(setq ftp+transfer-mode "Ascii"))
    (progn		
      (setq dummy "binary\n")
      (setq ftp+transfer-mode "Binary")))
    
  (process-send-string (get-current-process-object) dummy)
  (ftp+mode))


(defun ftp+choose-site-decrement()
 "choose a new site in ftp+site-list going backwards"
  (interactive)
  (if (eq (ftp+get-status ftp+connected-index (ftp+index-of-current-buffer)) t)
      (message "cannot change sites while ftp+ is running child")
    (progn

  (if (eq ftp+list-ctr 0)
    (setq ftp+list-ctr (- (length ftp+site-list) 1))
  (setq ftp+list-ctr (- ftp+list-ctr 1)))
  (setq ftp+current-site     (get-field SITENAME-INDEX (get-record ftp+list-ctr ftp+site-list)))
  (setq ftp+current-username (get-field USERNAME-INDEX (get-record ftp+list-ctr ftp+site-list)))
  (setq ftp+current-password (get-field PASSWORD-INDEX (get-record ftp+list-ctr ftp+site-list)))
  (describe)
  (message "%s" (customized-display-string))
(ftp+mode))))

(defun ftp+choose-site()
 "choose site going forwards"
  (interactive)
  (if (eq (ftp+get-status ftp+connected-index (ftp+index-of-current-buffer)) t)
      (message "cannot change sites while ftp+ is running child")
    (progn

  (if (< ftp+list-ctr (- (length ftp+site-list) 1)) 
    (setq ftp+list-ctr (+ 1 ftp+list-ctr))
  (setq ftp+list-ctr 0))
  (setq ftp+current-site     (get-field SITENAME-INDEX (get-record ftp+list-ctr ftp+site-list)))
  (setq ftp+current-username (get-field USERNAME-INDEX (get-record ftp+list-ctr ftp+site-list)))
  (setq ftp+current-password (get-field PASSWORD-INDEX (get-record ftp+list-ctr ftp+site-list)))
  (describe)
  (message "%s" (customized-display-string))
  (ftp+mode))))

(defun ftp+eval-line()
 "this command is called if you hit return. it allows you to type ftp
  commands instead of using the tab-<char> and shft-<char> shortcuts
  it operates on the assumption that you are on a line with a prompt
  and then a space..."
  (interactive)
  (switch-to-buffer (window-buffer (selected-window)))
  (beginning-of-line)
  (ftp+fwd-until-space)
  (forward-char 1)
  (setq ftp+exec-string (concat 
			 (buffer-substring (point) (point-max)) "\n"))
  (kill-line)
  (process-send-string (process-name (get-current-process-object))
		       ftp+exec-string))


					;
					; major code
					; segment
					;
					; internal-functions
					;

;;; functions that are not invoked by keypress but
;;; are called by other functions only

(defun ftp+mode()
"Major Mode for Using ftp+. 
Dont be confused. Your current site for this buffer
is what precedes the word Mode above.
\\{ftp+mode-map}
\\[disconnect]: kill the current ftp process. 

\\[close-connection] the same as if you type bye from the keyboard. 
Once you type this, you can scroll through the sites to pick another

\\[ftp+choose-site-decrement]: move back one site
in the ftp+site-list variable. Only works when the ftp+ buffer you are in
is not connected to a site

\\[ftp+choose-site]: move forward one site in the
ftp+site-list variable. Only works when the ftp+ buffer you are in
is not connected to a site

\\[ftp+password-entry]: after you the site you have
connected to prompts you for a password, type this sequence to send the
default password or edit it then hit return to send the edited password. For
security reasons, it is best to leave passwords for privates sites blank.

\\[choose-site-by-name]: if you know the precise name
of the site you wish to connect to, type a few characters of the name and then
hit TAB to let emacs complete it for you... the desired site must be in the
ftp+site-list variable

\\[choose-site-by-topic]: if you want to browse by
topic, then enter this key sequence

\\[choose-site-by-filename]: if you are looking for
a particular file, then enter this key sequence

\\[open-connection]: once the site you wish to choose exists on the 
mode-line, type this key to establish a connection

\\[ftp+scroll-description-buffer]]: if all of the comment for this
site will not fit into the pop-up window, press this to scroll through
the text of the comment.

\\[directory]: same as typing dir or ls -l while remotely connected

\\[ftp+binary-mode-toggle]: switch between binary and ascii modes of file
transfer. if you are transferring a .tar or .Z file, you must use binary mode

\\[ftp+get-file]: this key does one of two things after you are connected
and you have used \\[previous-line] and \\[next-line] to place the cursor
anywhere on the line with a filename or directory name.
1> If it is a directory entry, pressing \\[ftp+get-file] will change to that
   directory.
2> If it is a filename entry, pressing \\[ftp+get-file] will transfer that
   file to your local current directory. YOU MUST HAVE ALREADY CHOSEN THE
   CORRECT FILE TRANSFER MODE using \\[ftp+binary-mode-toggle]
\\[ftp+send-file] is used to send a file from you to the remote ftp site
\\[ftp+cd-up] is used to move up one level on the directory tree
\\[ftp+eval-line] send the typed command to the remote ftp site.
All local commands such as lcd and !ls should be typed at a command-line
commands can only be typed when you are at the end of the buffer AND you 
have a prompt AND there is a space between your typed command and the prompt.
like so:
 ftp> lcd /scratch/china.primitive
 the above will work. this wont right now:
 ftp>lcd /scratch/china.primitive
 and this wont either:
 ftp> ftp> lcd /scratch/china.primitive

\\[ftp+brief-help] gives brief help in the minibuffer ONCE CONNECTED TO A SITE

The final command to be documented is a convenience. I have bound the 
function other-window to TAB TAB because it is quicker than C-x o
"
  (switch-to-buffer (window-buffer (selected-window)))
  (kill-all-local-variables)
  (setq major-mode 'ftp+mode)
  (setq mode-name ftp+current-site)
  (goto-char (point-max))
  (setq goal-column 75)
  (setq mode-line-buffer-identification (concat "Current Site: " ftp+current-site)) ; alter mode-line info
  (setq mode-line-format '("" "" "" "     " mode-line-modified mode-line-buffer-identification "   " global-mode-string ))
  (setq global-mode-string "")
  (setq mode-line-modified "")
  (setq dummy-variable (concat "TransferMode." ftp+transfer-mode "    "))
  (setq mode-line-buffer-identification (concat dummy-variable "Current Site: " ftp+current-site))
  (set-buffer-modified-p (buffer-modified-p))
  (setq ftp+mode-map (make-sparse-keymap))
  (if (eq (ftp+get-status ftp+connected-index (ftp+index-of-current-buffer)) nil)
      (progn
	(define-key ftp+mode-map "\C-i\S\S" 'same-site-other-window)
	(define-key ftp+mode-map "\C-i\q" 'disconnect)
	(define-key ftp+mode-map "\C-i\C-i" 'other-window)
	(define-key ftp+mode-map "\C-i\c" 'close-connection)
	(define-key ftp+mode-map "\C-i\p" 'ftp+password-entry)
	(define-key ftp+mode-map "cs" 'choose-site-by-name)
	(define-key ftp+mode-map "ct" 'choose-site-by-topic)
	(define-key ftp+mode-map "cf" 'choose-site-by-filename)
	(define-key ftp+mode-map "7" 'ftp+choose-site-decrement)
	(define-key ftp+mode-map "8" 'ftp+open-connection)
	(define-key ftp+mode-map "9" 'ftp+choose-site)
	(define-key ftp+mode-map "0" 'ftp+scroll-description-buffer))
    (progn
      (goto-char (point-max))
      (define-key ftp+mode-map "\C-i\S\S" 'same-site-other-window)
      (define-key ftp+mode-map "\C-i\C-i" 'other-window)
      (define-key ftp+mode-map "\C-i\d" 'directory)
      (define-key ftp+mode-map "\C-i\m" 'ftp+binary-mode-toggle)
      (define-key ftp+mode-map "\C-i\q" 'disconnect)
      (define-key ftp+mode-map "\C-i\p" 'ftp+password-entry)
      (define-key ftp+mode-map "\C-i\g" 'ftp+get-file)
      (define-key ftp+mode-map "\C-i\s" 'ftp+send-file)
      (define-key ftp+mode-map "\C-i\u" 'ftp+cd-up)
      (define-key ftp+mode-map "\C-i\c" 'close-connection)
      (define-key ftp+mode-map "\C-i\h" 'brief-help)
      (define-key ftp+mode-map "\C-m" 'ftp+eval-line)))
  (use-local-map ftp+mode-map)
  (goto-char (point-max)))

(defun customized-display-string ()
  (if DISPLAY-CUSTOM
      (progn
	(setq concstring "")
	(setq L DISPLAY-FIELDS)
	(while L
	  (setq concstring 
		(concat
		 concstring
		 "["
		(get-field (eval (car L)) (get-record ftp+list-ctr ftp+site-list))
		"] "))
	  (setq L (cdr L)))))
  concstring)


  (defun choose-site-using (category index)
  ;; create an alist of topic names so that we can use
  ;; completing read to get the user's choice
  (create-alist-from index ftp+site-list)
  
  ;; since the topic and file fields have entries separated by a slash,
  ;; we must parse out the names.. boy was this a headache!
  (split-out-names tmplist index)

  (message "removing duplicates. .")
  (setq new-alist (list-to-set J))
  (setq ftp+chosen-item
	(completing-read (concat 
			  "Which " category
			  " do you wish to choose by ") new-alist))

  ;; now that we know what topic/file she wants to choose by, we find all sites
  ;; which contain the topic/file and create the alist and call
  ;; completing-read. stepwise this request is solved by:
  ;; 1 iterating through site-list on element at a time
  ;;   a take the element which is a list and pull out the 
  ;;     topic/file-index field
  ;;   b is the topic/file field the chosen topic/file?
  ;;     - if yes, create an alist consisting of the site-name of this site
  ;;       and a dummy second element
  ;;     - if no, skip this element 
  ;; 2 next element
  (setq tmplist '())
  (setq tmpcounter 0)
  (while (< tmpcounter (length ftp+site-list))
    (message "%d" tmpcounter)
    (setq y (get-field index (get-record tmpcounter ftp+site-list))) ; 1a
    (if (search-for-substring ftp+chosen-item y)
	(setq tmplist (cons (list (get-field SITENAME-INDEX 
	    (get-record tmpcounter ftp+site-list)) "dummy") tmplist)) ; 1b if yes
			()); 1b if no 
      (setq tmpcounter (1+ tmpcounter))) ; 2
  (setq ftp+chosen-site
	(completing-read (concat 
			  "which site is your choice? press tab for completions ") tmplist)))


(defun ftp+back-until-space()
 "what you think child?"
  (while (progn (backward-char 1)
		(setq ftp+bustemp (buffer-substring (point) (+ (point) 1)))
		(not (equal ftp+bustemp " ")))))

(defun ftp+fwd-until-space()
  "guess"
  (while (progn (backward-char -1)
		(setq ftp+bustemp (buffer-substring (point) (+ (point) 1)))
		(not (equal ftp+bustemp " ")))))

(defun ftp+pull-out-name()
 "go to start of line and save the first char so we know if its a regular
file or a directory. go to end of line and save the buf position. go
back a word. return this substring. this will always be the filename.
using backward-word fails because the syntax table defines a period
as punctation.. instead of narking the syntax table. i rote a sureshot"
  (beginning-of-line nil)
  (setq ftp+first-char-on-line 
	(buffer-substring (point) (1+ (point))))
  (end-of-line)
  (backward-char 1)
  (setq ftp+pontemp (point))
  (ftp+back-until-space)
  (setq ftp+desired-file (buffer-substring (point) (+ ftp+pontemp 1))))

(defun ftp+scroll-to-chosen-site ()
  ;; this was one of those times where eq failed... myprogram just
  ;; kept looping through the sitelst and then i said aha, it must
  ;; be that eq instead of equal andwhadya know
  (while (not (equal ftp+current-site ftp+chosen-site))
    (progn
      (ftp+choose-site)
      (ftp+mode))))

(defun choose-site-by-name ()
  (interactive)
  (setq ftp+chosen-site 
	(completing-read "which site " ftp+site-list))
  (ftp+scroll-to-chosen-site))

(defun parse-by-index (list)
  (list 
   (nth ftp+given-position list)
   "dummy data"))

(defun process-alist-by (ftp+given-position alist-by-value)
  (setq ftp+current-position ftp+given-position)
  (setq m (mapcar 'parse-by-index 
      (mapcar 'car (mapcar 'cdr alist-by-value))))
  (list-to-set m))

(defun string-memq (element string)
  (setq string-memq-ctr 0)
  (while (and (< string-memq-ctr (length string))
    (not (string= (char-to-string (elt string string-memq-ctr)) element)))
    (setq string-memq-ctr (1+ string-memq-ctr)))
  (if (eq string-memq-ctr (length string))
      nil
    t))

(defun string-return-all-up-to-separator (sep string)
  (setq string-ctr 0)
  (setq new-string "")
  (setq overall-lth (length string)) 
  (while (and (< string-ctr overall-lth)
	  (not (string= (char-to-string (elt string string-ctr)) sep)))
    (setq new-string
	  (concat new-string (char-to-string (elt string string-ctr))))
    (setq string-ctr (1+ string-ctr)))
  (prog1
    (setq return-string (substring string 0 string-ctr))
    (setq rest-of-string (if (eq string-ctr (length string))
			     ""
			   (substring string (1+ string-ctr) nil)))))

(defun create-alist-from-slash-separated-string (element)
 (setq satan (get-field global-index-from-split-out element))
 (setq separator-in-string t)

 (while separator-in-string
   (setq J (cons (list (return-up-to "/" satan) "dummy") J))
   (setq satan (buffer-substring (point) (point-max)))
   (if separator-in-string (replace-match "")
     ())))

 ;; this was a headache until I decided to regexp replace and search
 ;; it was a headach a long tome after that too!
 ;; and even more later... ouch!
(defun split-out-names (tmplist INDEX)
 "takes ALIST and creates global variable new-alist with an alist 
of the names split into separate cons cells"
 (setq J '())
 (message "Splitting Out Key Field. .")
 (setq global-index-from-split-out 0) ;; the first field of the alist
 (mapcar 'create-alist-from-slash-separated-string tmplist)

 (message "Splitting Out Key Field...Done"))


;;;; Haack stuff .. will move to appropriate code section later
(defun brief-help()
  (interactive)
  (message "[tab] Getfile Sendfile cdUp Modetogl Directory Closeconnection"))

(defun get-current-process-object()
  "each buffer name is of the form *x where x is an integer. the corresponding
process object in the vector is at index x. we therefore take off the * by use
of the substring command and we have the index we need"
    (aref process-objects-vector (ftp+index-of-current-buffer)))

(defun ftp+index-of-current-buffer()
  (interactive)
    (string-to-int (substring (buffer-name (window-buffer)) 1)))


(defun ftp+set-status (list-index list-of-list-index value)
"set status of list-index in list-of-list-index to value"
  (replace-list-element-with
   value
   (elt ftp+status-list list-of-list-index)
   list-index))

(defun ftp+get-status (list-index list-of-list-index)
  (elt (elt ftp+status-list list-of-list-index) list-index))

(defun replace-list-element-with (element list index)
  (setq M list)
  (setcar (nthcdr index M) element))
