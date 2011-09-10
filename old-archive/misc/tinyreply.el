;; @(#) tinyreply.el -- A mail reply formatting package for rmail and gnus

;; @(#)$Id: tinyreply.el,v 1.70 1995/04/11 11:26:38 jaalto Release_2 jaalto $
;; @(#) $Keywords: mail, news, gnus, formatting $
;; $KnownCompatibility: FSF 19.28 $

;; This file is *NOT* part of GNU emacs

;;{{{ Documentation

;; Copyright (C) 1994,1995 Jari Aalto
;; Author:        Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Maintainer:    Jari Aalto <jaalto@tre.tele.nokia.fi>
;; First Created: Oct 29th 1994
;; Version:       $Revision: 1.70 $
;; Sate:          $State: Release_2 $
;;
;; To get information on this program use ident(1) or do M-x tir-version


;; LCD Archive Entry:
;; tinyreply|Jari Aalto|tre@tele.nokia.fi|
;; A mail reply formatting package for rmail and gnus, n'thing like supercite|
;; 11-Apr-1995|1.70|~/misc/tinyreply.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Intallation:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;; - Basicly this is standard installation, BUT you must manually
;;   pick up functions that you want to use.
;;
;;      ;;  Usual load, for every emacs
;;      (require 'tinyreply)
;;      (tir-key-reply-define)          ;see [ench1]
;;
;;      ;;  Autoload, you must use the Example setup method for this!
;;      (autoload 'tir-autoload "tinyreply" "Reply tool" t)
;;      (setq tir-autoload-hook 'my-tir-setup)
;;
;; - If you just want to install this .el without hassless,
;;   follow these steps, *but* remember that it configures your
;;   MAIL and GNUS hooks.
;;
;;      1.   M-x eval-current-buffer      ,for this file
;;      2.   M-x tir-example-install      ,installs the example setup for you
;;           to overwrite previous installation
;;           answer "no" is Q:s asked.
;;      3.   load-file <file>             ,load the setup
;;      4.   start RMAIL, GNUS            ,and your in the air
;;
;;
;; WARNING
;; - replaces ORIGINAL 18.57, 19.27-8 functions. Read the overload section.
;; - If your emacs versions isn't compatible with this, please look
;;   at the variable  tir-v-rmail-forward-overload
;;
;; HOW TO CONFIGURE
;; - search bookmark [ampersand + 'conf'] to have a look at conf section.
;; - please look at this file with folding.el 1.7, it makes more sense.
;;
;; ENCHANCEMENTS , ADDITIONS
;; [ench1] to rmail, rmail-summary
;; - If you want use the 'nuke' function [see example setup], then you
;;   might want to look at enchancements, where these keys have new
;;   meaning:
;;      r       ,reply and query CC: kill
;;      R       ,reply and kill CC: automatically without asking
;;
;; - However you can configure these keys as you want.
;;   See bookmark 'bind-' for more info.


;;; Commentary:

;;; Briefly:
;;  o  The IDEA of this packet is to provide ** SETS OF FUNCTIONS **
;;     that can be used inside mail-setup-hook or news-reply-header-hook.
;;
;;  In general
;;  o  Deletes headers and logos automatically
;;  o  Adds date & sender to the reply message          [non-configurable]
;;  o  Automatically detects UU mail, deletes it from reply
;;  o  Keeps repetitive mails in order 're: -> re2 -> re3 -> re4...'
;;
;;  Common field support functions
;;  o  Includes 'info:' field  when destination matches.
;;  o  CC:
;;     - whole field deletion, so that you don't reply to whole world.
;;     - If From & CC has same user, deletes user automatically
;;       from CC: field.
;;     - Automatic CC to sender if reply is done via GNUS
;;  o  FCC:
;;     - picks up right folder to record sent mail. Very powerfull
;;     - a) according to header
;;     - b) You can set 'study' criteas and pick folder according to matches.
;;
;;  Misc features
;;  o  Snips text region: adds few words [snip 8: Hi, I'm asking..]
;;     - detects snipped region as CODE or TEXT, you can set up more...
;;  o  Kills ctrl-characters from reply: that terminal cursor key garbage..
;;  o  Misc functions: cit deletion, add string to region etc...
;;
;;  Filling special
;;  o  Has a feature called 'adaptive filling'. <experimental only>
;;
;;  Big post special
;;  o  Almost detects if you have embedded code attached to the end of
;;     post. It tries to guess where the sender ended message and removes
;;     logos as usual.
;;
;;  Forwarding
;;  o  Included hook which runs after forward.
;;  o  Includes nice forward function, which gets REALLY
;;     NICE , when message is forwarded multiple times using this .el
;;
;;  Keybind enchancements, user settable
;;  o  r = reply and query CC: field kill, R = reply and kill CC: automatically
;;
;;  WARNING
;;  o  Replaces original RMAIL-FORWARD when 18.57, 19.2[78] detected


;; INTRODUCTION
;; ......................................................................
;; - I don't know how to call this packet. It helps me replying RMAIL mails
;;   and formatting followups in GNUS.
;; - You might call it as "reply add-in" packet or something like that.
;;
;; - If you're looking for citation customization, get supercite, and
;;   skip over this el. There is _NO_ fancy customization.
;;
;;   PLEASE LOOK AT THE END OF THIS FILE FOR COMPLETE AUTOMATION EXAMPLE
;; - It's "plug and play" configuration that does almost everything ready
;;   for you. Configure to your taste.
;;   o   not tested with ding yet, but it'll be compatible with it soon.
;;
;; EXPERIMENTAL NOTE
;; - The filling of mail isn't key part of this packet allthough it
;;   occupies many lines.  One day I just had to get it coded, so I
;;   coded it there.


;; Having a test try!
;; ......................................................................
;;
;; - Easiest way to see what this packet does is, that you
;;   1)  send mail to yourself, put there fancy logos, UUENCODED text,
;;       anything you might think of that might shut the mouth of this
;;       'crunch' tool...
;;   2a) reply with "r" in rmail, or "f" forward it to you and observe
;;       the results. The formatted message should quickly appear with
;;       few confirmations from you IF YOU COPIED THE SETUP at the end
;;       of this file. If you didn't, then see  2b.
;;   2b) Try some key bindings or functions to see what it can do...
;;       The 'fup' and 'snip' creatures are quite cute :-/
;;
;;
;; Answering to questions in this program
;; ......................................................................
;; - Answering to YES-NO questions accepts RET/SPC for default choice and
;;   BACKSPACE for NO choice.
;; - usually there is also 'a' or 'q' for aborting current task permanently.
;;   *Please* use 'a' or 'q' instead of Ctrl-G, because it allows program to
;;   clean up things when necessary.
;;
;; About citations
;; ......................................................................
;; - I'm not very keen on the convention the supercite uses:
;;      mike> jane> >> else_said> what
;;      mike> jane> >> else_said> ..
;; - That makes reading followups rather annoying, since text
;;   floods too much to the right. It also makes extracting *plain*
;;   text rather interesting headache for programmer.
;; - I prefer simpler. approach, where 'citation' will be any normal
;;   reply char, say:
;;      :|>>>> someone talked here
;;      :|>>
;;      : he said here
;;     and this is my rephrase
;;
;; - Above is 3 different 'cite' chars used, which is ok.
;; - Please stick to citation chars [TIR-CIT] used in this program.
;;   You'll find it best policy if you use this .el for replying mail.
;;   It's also not advisable to use more that 1 CIT char.
;;
;; If you must use something else, avoid using these citations characters
;; ......................................................................
;; #          it's used by shar messages, shell scripts: HIGHLY PROHIBITED!
;; () []      used by programming languages, and () heavily used in text: HP!
;; %          it's used by postscript text
;; $,&        heavily used in perl
;;
;; Logos and mailer's header deletion
;; .....................................
;; - This packet tries to identify logos. Please use standard logo
;;   separator '--', like the one GNUS automatically adds when it
;;   inserts .signature contents.
;; - Alternatively use empty line which separates logo from text at
;;   the end of messge. Nobody wants to include logo text inside
;;   replies and followups. Programs can more easy identify, remove or
;;   save them to file, whatever processing is necessary.
;;
;;   This is the most preferred logo format:
;;
;;      txt
;;      txt
;;                              [empty line]
;;      --                      [on line of its own, two dashes]
;;      logo, etc, name         [no empty lines anywhere between text]
;;
;; - I know everyone wants to make his own way, but If you are
;;   heavy poster, then you'll appreciate this standard way. It makes
;;   life lot easier... or at lest to this program :-O
;;
;; functions on this package
;; ..........................
;; - There are a bunch of functions you might be interested just for
;;   adding comments to C-shell scripts, deleting lisp comments etc..
;;   Please examine the function in this el that you could use elswhere.
;; - The functions should be well documented.



;; HELP
;; ......................................................................
;; - Call describe-function for any function or look the code of this .el
;; - There is an example setup for emacs for automatic
;;   use of these function at the end of this file.
;;
;; bookmarks  -- that enable searching items quicly in this file
;; - preceed every word with ampersand(&) to find the item with C-s
;; - You may find 'M-x Occur &...'interesting command.
;;   examp      ,example settings
;;   conf       ,USERS configure section
;;   var        ,other variable settings
;;   bind       ,binded keycodes
;;   snip       ,snipping text
;;   fwd        ,forwarded text adjusting
;;   original   ,REPLACED and overloaded functions
;;   adaptive   ,special filling
;;   fill       ,general filling
;;
;; CURRENT KEY BINDINGS
;; - Are set to mail/news buffer.
;; - I haven't really paid attention to these, there exist several good
;;   functions that does not have bindings. The original intent was to
;;   automate whole process without keybindings. See the end of file
;;   for example settings.
;;
;; RESERVERD prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tir-" in front of
;;   every function & variable. It stands for '(ti)ny (r)eply'
;;
;; EMACS VERSION NOTICE
;; - I have developed this with 18.57 special HP-UX edition/FSF 19.28,
;;   Notice that gnus version I used was 4.1. Thoise were the days
;;   when Supercite was on it's boyhood and I needed something more
;;   personal...
;; - Feb 15 -95 : now I have permanently moved to 19.28, and I don't
;;   plan to maintain 18.xx support, but I think this still works for it.



;; PROBLEMS?
;; - Please try to use simple, well known citation marks and you shouldn't
;;   have many problems with this .el
;; - If you have an error correction I'm happy to incluede it to this el. You
;;   will get all the credit and your name will be included here.
;; - Before making any corrections, please consult the maintainer, so
;;   that you can confirm that you do have the _latest_version_
;;   Don't rely on Ohio archive, since arhives do not snow up there
;;   immediately.
;;
;; - If you have any problems:
;;   -------- if you didn't get what you expected.
;;   1a) send FULL listing of mail you tried my functions on.
;;   1b) send results what happened
;;   1c) describe what you tried to do.
;;
;;   -------- if there was an error in lisp function
;;   2a) turn debug on with M-x set-variable RET debug-on-error RET t RET
;;   2b) do as you did
;;   2c) copy whole *trace buffer* and send that + 1a-c to me.
;;
;;   -------- in all cases
;;   3) send all settings you have changed.
;;
;;   - It's very important that I get just the same POST or MAIL you
;;     had problems with. If you don't want to send your private
;;     post to me, you can use M-x tir-scramble-reg to make BODY of the text
;;     unreadable [just mark the region for scrambling].
;;   - please LEAVE any trailing <logos> and <headers> as they were,
;;     don't scramble them, otherwise the post is no good for me.
;;     Best if you can post the text as it were.


;; CODE NOTE
;; ......................................................................

;; - There may be some debugging garbage left, like function 'd!',
;;   just pay no attentention to them. I don't want to polish them
;;   away because they are extremely necessary in order to catch up
;;   errors.
;;
;; About the -v-
;; - Lately I started using -v- to distinguish variables from
;;   functions, so that I could more easily find them from my code.
;; - The old variables do not have that -v- and I felt that it was too
;;   much work to convert everything to the new -v- convention.  So
;;   please tolerate the variety.
;;
;; defconst
;; - You may wonder why all variables aren't 'defvar', well it's subject of
;;   maintenance: this is a big program and if I change some variable's
;;   meaning so that I can test some new feature, I can just do C-x C-e
;;   behind variable. With defvar I need to have double file for variables.
;; - I don't see problem with this because:
;;   * make separate config file, say: .emacs.mail
;;   * load TIR in it and define all variables after the 'load' statement.
;;     This way any defconst is overriden.
;;   * use the autoload suggested above (see the hooks)


;; Thank You list:
;;
;; Feb 23 -95, cb@ironwood.cray.com (Christopher Brewster)              [cb]
;; -  Chris wanted a feature where 'r' in Rmail means reply to sender
;;    only and kill automatically all CC: and 'R' to have CC: included.
;; *  This has been implemented in v1.53

;;}}}
;;{{{ history of changes

;; HISTORY OF CHANGES
;; ......................................................................
;;
;; Apr   11     1995    [jaalto]        19.28   v1.70           NotReleased
;; - Adjusted tir-v-skip-msg-beg-re, so thet the "@" is not restrictive,
;;   when matched the email addr is supposed to be there.
;; - The in tir-fup-parse-from-name decoding ["] changed to use same
;;   procedure as in 1.69. Removed the old QUOTE 'from regexps'.
;; - {bug} found tis-s-xxx functions that do not belong to this e,
;;   changed them to tir-s-xxx.
;; + Added atomatic installation function "tir-example-install" that
;;   gets you running within one minute...
;;
;; Apr   10     1995    [jaalto]        19.28   v1.69           NotReleased
;; - Made small adjustment to tir-kill-get-jump-lines, so that it can more
;;   smarter skip over "Hello|thanks" text. This avoids adding false kill
;;   points.
;; - Added more regexp into tir-v-skip-msg-beg-re, this relates the the same
;;   thing.
;;
;; Apr   7      1995    [jaalto]        19.28   v1.68           NotReleased
;; - When parsing persons name like (Mike M.G.III. Monroe), my algorithm
;;   broke. It relied too much for the rigid RE matches, so I needed something
;;   mo sophisticated. Now the name matching is done as follows:
;;   1)  break data into words
;;   2)  drop all abbreviations, those with dots in words
;;   3)  what's left, determine suitable ones.
;;   This way it is pretty much more general than it used to be. At the
;;   same time I needed to code two additional functions for cases like this.
;; + tir-get-blocks: breaks string into blocks/words by RE
;; + tir-list-match-re : returns elements from list by RE, or NON-RE
;; - Improved the logo killing for persons NAME
;; - example setup updated.
;;
;; - The adaptive filling function tir-fill-adap-reg now accepts additional
;;   parameter. I planned it to be *too* smart, I mean, that I tried to make
;;   it an universal function, where you could just feed it with region and it
;;   would have determined if the individual paragraphs inside that region
;;   were normal text or code. If it detected code inside region it should
;;   have jumped over it.
;; - As it turned out, this "AI" made too often wrong guesses and it didn't
;;   indent paragraphs that it should have done. So I turned off that
;;   feature. Now you just show the region for it and it will happily
;;   fill all inside it. That's better, and that's what I usually wnat
;;   when composing the mail reply for long lines. I know what to fill :-/
;;
;;
;; Apr   4      1995    [jaalto]        19.28   v1.67           NotReleased
;; - tir-v-re-code-rest : content changed.  The adaptive
;;   filling rejected regions too easily, because of 'cond' which I
;;   used to mean a lisp 'condition' , but in normal speech it's like
;;   seCOND .. --> made more restrictive.
;; - tir-fup-parse-from-email : internal variable e changed to em,
;;   because the 'e' itself is reserved constant.
;; - the email parser now accepts 'account+@site' --> '+' mark.
;;   !! Should make it user configurable some time.
;; + tir-fup-v-re-wave, tir-fup-v-wave-single-re : added more words.
;; + tir-1-space : new function to remove spaces. The date parser is
;;   now much more robust against extra spaces.
;;
;; Mar 15       1995    [jaalto]        19.28   v1.66           NotReleased
;; - discovered that the RCS had made my previous releases unreadable due
;;   some internal error. Have no idea why it did that, so any previous version
;;   is unavailable permanently
;; - I deleted that RCS and made this -forced v1.66.
;; - Just while ago I found out that gnus had it's own CC method for the
;;   poster: gnus-auto-mail-to-author, that makes my tir-news-copy-to-sender
;;   function almost unnecessary ... pick your way.
;; + example setup updated.
;;
;;
;; Mar 15       1995    [jaalto]        19.28   v1.65           NotReleased
;; - RCS tree deleted, belongs to the 1.66
;;
;; Mar 15       1995    [jaalto]        19.28   v1.64           NotReleased
;; - Added support for gateway type addresses 'Joe=Smith%gtw%more@x.com',
;;   so that parses the person's name out of it.
;; - Added tir-kill-logo-getp-funcs, list of killing functions, where
;;   user may put his own too. Now detects context diff posts.
;; - example1 section updated. Now my personal libraries are hidden.
;;
;;
;; Mar 7        1995    [jaalto]        19.28   v1.63           Release_1
;; - I have changed this el so much after the first preRelease that it's
;;   time to send it to ohio. So here it comes.
;; - Solid break algorithm totally rewritten. Now it can handle any
;;   break mark _combinations_, anytime, anywhere.
;;
;; Mar 6        1995    [jaalto]        19.28   v1.53-1.62      NotReleased
;; - changes 1.43-1.52 mostly improvement to killing.
;;
;; o Most of these interest the developer only
;; - added Example2-section et the end. Made key prefixes user configurable.
;; - [cb] suggested kyes r=kill CC:, R=do not kill CC:
;; - added tir-reply-r tir-cc-kill-flag. Tthe "rR" keys are user configurable.
;; - made tir-fup-v-wave-single-re, more restrictive for words that
;;   are common in english language --> avoid false wave hits.
;; - added defconst explanation/docum.
;; - istr is now contructed via tir-myp function.
;; - The solid-break kill point routine rewritten. Now picks first and last.
;;
;; - Now makes use of the autoload feature. Example setup updated due to this.
;; - Corrected error in tir-fup-parse-from-email, where siteCharSet missed the
;;   '-' , so that my.site-general.fi was detected only as my.site
;; - Added new RE in tir-fup-v-re-code-wave, detects sh scripts now.
;; - added 3 name case for:  user@site \(name1 name2 surname)
;; - tir-fup-v-kill-hdr-flag to control automatic hdr deletion
;; - Added event reading instead of read-char, cancelling logo kill in
;;   19.28 is now much smoother. Corrected name kill.
;; - The gnus 'R' key caused some headache to me and there seemed to be no easy
;;   way to take over 'R'. The solution was to to use separate function for
;;   killing the yanked text. See the example setup at the end.
;;
;; Feb 10       1995    [jaalto]        19.28   v1.43   preRelease
;; - Sent to gnu.emacs.sources now when I finally had connection
;;   which accepted unlimited post size :-/
;;
;; Sep. 6       1994    [jaalto]         18.57  v1.1   First implementation
;; - Hmm, I just started to collect various functios I have used for
;;   replying messages. Somehow I didn't like the supercite,
;;   call me "stupid" if you like :-). Maybe I wanted packet that
;;   fits to my needs and which is extendable whenever I feel like it.
;; - I have no intentions to replace supercite, I'm sure those who
;;   uses it will continue to do so. The aim of this .el is to offer
;;   'relative' simple replying tool.


;;; To do list:
;; ......................................................................
;; - The fup function won't handle everything yet, I constantly
;;   add new kind of things whenever it fails identifying logos and
;;   headers and dates
;; - SNIP feature needs some corrections for picking the right words.

;;}}}



(provide 'tinyreply)

(require 'sendmail)                     ; can't bind keys otherwise
(require 'mail-utils)                   ; few functions that I'll use
(if (boundp 'news-reply-mode-map) nil   ; for NEWS keybindings
  (load "rnewspost"))





;;{{{ setup: BINDINGS

;;; Code:

;;; ... Keybingings ............................................ &bind ...
;;; C-c C-z  prefix , because it so easy to reach with left hand
;;;          (Z)ending mail map :-/
;;; - We make our own PREFIX here into mail and gnus maps.
;;;   Look carefully, you may easily recycle and use this for yourself !!
;;; - You can define the two prefix keys yourself if you want.


(defvar tir-pfx-char-1 "\C-c")
(defvar tir-pfx-char-2 "\C-z")
(defvar tir-pfx-keys (concat tir-pfx-char-1 tir-pfx-char-2))
(defvar tir-mode-map (lookup-key global-map tir-pfx-keys))


;;  Build up temporary variables
(defconst *err nil)
(defconst *map 'tir-mode-map )     ;trick to recycle following... :-)
(defconst *map-defined (not (eq nil tir-mode-map)))


;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...
(if (keymapp (eval  *map)) nil   ;already defined
  (set  *map  (make-sparse-keymap))
  (setq *map-defined t)

  (if (eq nil (lookup-key mail-mode-map tir-pfx-keys))
      (define-key mail-mode-map tir-pfx-keys (eval  *map))
    (setq *err " mail-mode-map "))

  (if (eq nil (lookup-key news-reply-mode-map tir-pfx-keys))
      (define-key news-reply-mode-map tir-pfx-keys (eval  *map))
    (setq *err (concat *err " news-reply-mode-map ")))

  (if (null *err) nil                   ;all went ok
    (message (concat "TIR.EL panic! :" *err "already occupied :-O"))
    (sit-for 2))
  )

;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...
(defun tir-install-keys (map)
  "Installs keybindings. Requires MAP"
  (define-key (eval  map) "c" 'tir-nuke-mail-cc-field)
  (define-key (eval  map) "r" 'tir-add-str)
  (define-key (eval  map) "R" 'tir-add-str-reg)
  (define-key (eval  map) "d" 'tir-del-re-reg)

  (define-key (eval  map) "c" 'tir-nuke-mail-cc-field)

  ;; (define-key (eval  map) "s" 'tir-mail-format-subject)
  (define-key (eval  map) "s" 'tir-snip)
  (define-key (eval  map) "y" 'tir-yank-mail)

  (define-key (eval  map) "v" 'tir-adap-move)
  (define-key (eval  map) "\C-q" 'tir-fill-para) ; see ESC q in emacs
  (define-key (eval  map) "Q" 'tir-fill-reg)
  (define-key (eval  map) "q" 'tir-fill-adap-reg)


  ;;  When you're sending mail, not replying, these are handy.
  ;;  You have to complete destination addr. for To: before you use these
  (define-key (eval  map) "f" 'tir-fcc-set-by-hdr)
  (define-key (eval  map) "i" 'tir-mail-ins-info)
  )

;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...
(if (null *map-defined) nil             ;there is no map for us
  (tir-install-keys *map))



;;; ................................................. &bind-additional ...
;; - This section redefines some default RMAIL keys or adds new keys
;;

(defvar tir-v-key-r-kill-cc "R"
  "*Key used when automatic CC kill is wanted")

(defvar tir-v-key-r-query-kill-cc "r"
  "*Key used when CC kill is queried.")


(defun tir-key-reply-define ()
  "reDefines few RMAIL keys: r and R"
  (interactive)
  (let ((kill  tir-v-key-r-kill-cc)
        (reply tir-v-key-r-query-kill-cc)
        )
    (require 'rmail)
    (load "rmailsum.el")                        ; doesn't have 'provide

    ;; Now configure killing the CC fields according to keys [Rr]
    (define-key rmail-summary-mode-map kill 'tir-reply-cc-kill)
    (define-key rmail-mode-map       kill 'tir-reply-cc-kill)
    (define-key rmail-summary-mode-map reply 'tir-reply)
    (define-key rmail-mode-map       reply 'tir-reply)
    nil                                 ; for possible hook
  ))




;;; ........................................................... &hooks ...

(defvar tir-fup-tidy-hooks nil
  "When constructing reply mail, you might wan't to clean up
something out from it. See tir-fup-prepare how UUencoded block
is removed.

This hook is not for formatting reply! And you should not try
to delete headers or logos, because they are handled by FUP
functions already. Doing so would confuse whole reply process.

So, only delete or add something BETWEEN header and logo if
there is something else to do.
")


(defvar tir-fup-after-hook nil
  "*This hook is run after the headers and Trailing logos are killed.")


;;; ........................................................ &autoload ...

(defvar tir-autoload-hook nil
  "Handy if you want to set some things after TIR is autoloaded.")

(defvar tir-v-autoloaded nil
  "Signals if autoload was called. User should not chage this, unless
he wan't to execute tir-autoload-hook multiple times when reloading TIR.")

(defun tir-autoload ()
  "Say you're in X, and want to keep RMAIL in one emacs only,
so there is no need to load this TIR package for other emacses. By calling
this function you can cause this package to be autoloaded. See example
setup for more.

Be sure to use \(autoload 'tir-autoload \"tinyreply\" \"Reply tool\" t\)
in your .emacs and set \(setq tir-autoload-hook 'my-tir-setup\) if
necessary.
"
  (interactive)

  ;;   I could have used the 'featurep' instead of this flag variable,
  ;;   but this way, altering the var, it is possible to force hooks
  ;;   to be run again.
  ;;
  (if tir-v-autoloaded nil              ;already loaded
    (setq tir-v-autoloaded t)
    (run-hooks 'tir-autoload-hook))
  nil                                   ;satisfies HOOK ret value
  )

;;}}}

;;{{{ setup: -- general variables

;;; ... General variables used later .......................... &v-gen ...
;;; Just general varables declared here that are used later on

(setq W "[ \t]*")                       ;Whitespace
(setq A "[-a-zA-Z]+")                   ;alpha, notice '-' included
(setq N "[0-9]+")                       ;numeric
(setq AN "[-a-zA-Z0-9]+")               ;alpha & numeric

(defconst tir-v-months
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Month names in short format.")

(defconst tir-v-months-re
  (mapconcat    'concat    tir-v-months " \\|")
  "Montsh with OR regexp.")



;;; Don't make too long or complicated regexps, prefer clean and
;;; and simple and MANY instead.
(defconst tir-v-re-address              ;perl
  (concat
   AN "@" AN
   )
  "Regexp that tries to identify programming language statements.")



;;  These are perl specific statements
;;  '$' could be dollar sign, but '$var = value' usn't
;;  'sub routineName {'    ,it must contain opening bracket
;;
(defconst tir-v-re-code1                ;perl
  (concat
   " \$.,*=\\|sub +..* [{]\\|#\\|"                      ;note space in " $"
   "print" W ".*[(\'\"]"
   )
  "Regexp that tries to identify programming language statements."
  )



(defconst tir-v-re-code2                ;lisp and BLOCKS
  (concat
   "[{}]\\|"
   "\(\(\\|\)\)\\|"                     ;lisp too
   "[a-z][a-z]\-[a-z][a-z]+\-[a-z][a-z]"     ;certainly lisp!
   )
  "Regexp that tries to identify programming language statements.")


(defconst tir-v-re-code-rest
  (concat
   "defun\\|progn\\|setq\\|goto[-]char\\|define[-]"             ;lisp
   "\\|strcpy\\|"                                       ;c-code
   "#\\(if\\|endif\\|define\\|include\\)"       ;C-directives
   )
  "Regexp that tries to identify programming language statements.")

;;; ... Debugging spotlights .................................. &v-bug ...
;;; - Sometimes functions will gather informat must contain opening bracket
;;
(defconst tir-v-re-code1                ;perl
  (concat
   " \$.,*=\\|sub +..* [{]\\|#\\|"                      ;note space in " $"
   "print" W ".*[(\'\"]"
   )
  "Regexp that tries to identify programming language statements."
  )



(defconst tir-v-re-code2                ;lisp and BLOCKS
  (concat
   "[{}]\\|"
   "\(\(\\|\)\)\\|"                     ;lisp too
   "[a-z][a-z]\-[a-z][a-z]+\-[a-z][a-z]"     ;certainly lisp!
   )
  "Regexp that tries to identify programming language statements.")


(defconst tir-v-re-code-rest
  (concat
   "defun\\|progn\\|setq\\|goto-char\\|define-\\|cond \\|"              ;lisp
   "strcpy\\|"                                  ;c-code
   "\\(if\\|for\\|while\\)[ \t]+\(\\|"          ;various languages
   "#\\(if\\|endif\\|define\\|include\\)"       ;C-directives
   )
  "Regexp that tries to identify programming language statements.")

;;; ... Debugging spotlights .................................. &v-bug ...
;;; - Sometimes functions will gather information of what they do
;;;   so that you can check which activities were triggered.
;;; - these are solely meant for debugging. Eg. look at function that
;;;   picks up killing points..
;;;

(setq Dvar-1 nil Dvar-2 nil)

;;}}}
;;{{{ setup: -- regexp

;;; ... modifications discouraged .............................. &v-re ...
(defconst tir-tmp-buf "*temp*"
  "Work buffer for this package.")

;;; You can't just change these chars to have functions to handle more
;;; citation characters...My code just isn't that flexible.
;;; Or at least I don't guarrantee the results.
;;;
(defconst tir-cit ":|>!"
  "Valid citation characters.")


(defconst tir-re1 (concat "[" tir-cit "]")
  "Valid citation characters in rex format")

(defconst tir-nre (concat "[^" tir-cit "]")
  "Non-citation characters")
(defconst tir-nrew  (concat "[^" tir-cit " \t\n]")
  "Non-citation characters including whitespaces")


(defconst tir-re2  (concat "[ \t" tir-cit "]")
  "Valid citation characters _with_ space Eg. '> > > >' ")

(defconst tir-nre2  (concat "[^ " tir-cit "]")
  "Valid non-citation characters _with_ space")

(defconst tir-cit-beg-re (concat "^" tir-re1 tir-re2 "*" )
  "Full beginning regexp for citations.")

(defconst tir-empty-cit   (concat tir-cit-beg-re "$")
  "Empty citation re. No text at all")

(defconst tir-nempty-cit
  (concat
   "^" tir-re1 tir-re2 "*"
   "\\(" tir-nrew "+\\)"
   )
  "Non Empty citation re text match. There _must_ be CIT at the beg of
line for this to work.")


;;}}}
;;{{{ setup: -- function variables

;;; ......................................................... &v-flags ...
;;; These are set by TIR, any previous value is overwritten

(defconst tir-v-cc-kill-flag nil
  "Non-nil means that the CC: field is killed while answering post
References: bookmark 'bind'. Not user variable.")


;;; ... Function study ...................................... &v-study ...


(defvar tir-v-sender-email nil
  "List \(EMAIL-ACCOUNT SITE EMAIL-FULL\) , which will be set in
fup functions. The data is used bu *-fup*getp to determine possible
killing point.")

(defvar tir-study-v-info nil
  "List where studied info will be stored. See tir-study-buf-info.
User should not change content of this.")

;;; ........................................................... &v-del ...

(defconst tir-del-re-look-until 17
"How many chars to look in a line for tir-re1 marks. Valid values
are 1..80. If the line has fewer characters than value, the number of chars
on the line will be used.

Lets suppese we have following followup'd line:
        :>> > > > >  > john> I have a question for you, what if...
        01234 6789 1234 6789 1234 67890
                                ^
Setting the value to 24, will enable searching up to position 24(^)
but no further. It is important that you don't allow full line scan,
because there might be RE marks at the end:
        :>> > > > >  > john> I have a question!
                                              ^RE mark here, kills up
                                               till here if limit 80
The tir-kill-re-region kills always to the last position allowed.
")

;;; ........................................................... &v-fup ...
(defvar tir-fup-v-kill-hdr-flag t
  "*Controls automatic killing of headers. Non-nil means that the headers are
automatically removed, and nil activates user query.")

(defvar tir-fup-kill-msg  "~~kill here"
  "*Message to insert to buffer at the point of kill start")


(defconst tir-fup-re (concat "^" tir-re1)
  "Valid citation beginning rexp when searching last cit.")


;;;     Try to match headers like "File:" , start with big alpha and look for
;;;     trailing : mark. Delimiter for junk headers I don't want to include
(defconst tir-fup-hdr-rexp (concat tir-re1 tir-re2 "*[A-Z]+[-a-z]: ")
  "Try to match yanked reply headers like 'File:'")

;;;     The ultimate limit where junk header MUST end
;;;     - totally empty line (may contain RE-marks) or "wr[io]te[s]:" clause
;;;     - Define max chars for header too if fails to match...
;;;     - Note ">>>" or "> > >" that's why two rexps...
(defconst tir-fup-hdr-brk
  (concat
   "^" tir-re1 tir-re2 "*$"
   "\\|^ *$\\|\\(wr[oi]tes?\\|said\\|says\\)[ \t]*:")
  "The reply header end position.")


(defconst tir-fup-max-hdr-pos 1000
  "Maximum char position for reply header/ Hard limit.")

;;;     What is considered to be a starting point of tail logo?
;;;     - suppose First lonely line (up paragraph) or
;;;       normal delimiter (this is primary over the other)
;;;


(defconst tir-fup-tail-nrm-brk  "[ \t]*--[ \t]*$"
  "normal logo ending with -- ,autom. GNUS added signature.")

(defconst tir-fup-tail-nrm-txt-start  "--text"
  "This is your starting text delimiter")


(defconst  tir-fup-v-wave-single-re
  '( "cheers" "hope this helps" "regards" "good luck" "later[ \t,]*$"
     "G'day" "take care" "any suggestions" "best[ \t,]*$" "greetin"
     "Ciao" "Chao" "Have fun" "Love[ \t,]*$" "go figure"
     )
  "This is a list of REs that are usually on their own line.
Remember that *some* words are used many times in normal language, so make them
as restrictive as possible to prevent false hits. Like word 'best'.

These variables are normally ORed together to form regexp.
")


;;   Someone just wrote GreetinX , not greetings :-/
(defconst tir-fup-v-re-wave
  (concat
   "cheers\\|hope this\\|regards\\|Have a? ?nice\\|good luck\\|"
   "later,?[ \t]*$\\|thanks\\|thanx\\|suggestions *[?]\\|any help\\|"
   "best,?[ \t]*$\\|G'day\\|take care\\|greetin\\|get this[ \t]*?[ \t]*$\\|"
   "Ciao\\|Chao\\|tia,?[ \t]*$\\|hth,?[ \t]*$\\|have fun\\|"
   "nuff said\\|Kudos\\|Love[ \t,]*$\\|go figure,?\\|"

   ;;  These are Finnish national greetings...
   "terveisin\\|[ \t]t[:.][ \t]*[A-Za-z][a-z]+[ \t]*$"
   )
  "Mail ending phrases to match.")

;; just for testing right syntax
;; (setq re tir-fup-v-re-wave)
;; (defun tt () (interactive) (re-search-forward re nil t))


(defconst tir-fup-v-re-code-wave
  (concat
   ";;+.*\.el.*--\\|"                   ; has an '.el' and '--'
   "#!\/.*bin\\|"                       ; typical shell script begin
   "clip here"
   )
  "There may me code attached to the mail, this is regexp that tries to match
such beginning point. Eg. among lisp programs it's quite common to use
following first line:

     ;;; myPacket.el -- description of this packet

NOTE: the message is first yanked with y-pfxs , then searched.
")


(defconst tir-v-skip-msg-area 10
  "There may be some headers int the message are that we'll leave
alone. This tells region to search for such things.

Remember that this area is searched repeatedly until no more hits, so
the point may move to the end of area and decided that it is the right
start position of message.
")

;;; Header rejection
;;  case 1
;;  ...........
;;  Mister XXX on Feb 13 1995 wrote:
;;  Blaah...here is the el
;;  ;; Feb 13 my.el -- does some fancy elisp            <--- this isn't header!
;;     ^^^^
;;     This would indicate header in header area (5 lines), but
;;     it's not.
;;
(defconst tir-v-stop-msg-area-re
  ";;+.*"
  "This is regexp that says not to use this line although it is
valid message header re. The searching will stop if this RE is
matched and latest point saved will be used instead.")


(defconst tir-v-skip-msg-beg-re
  (concat
   "wrote\\|writes\\|says"
   "\\|199[0-9]\\|@"
   "\\|Hi .*"
   "\\|[A-Za-z]+,[ \t]*$"               ;Normally person's name "Jari,"
   )
  "Defines little area at the top of header, which are skipped
when searching for trailing killing points. See tir-kill-get-jump-lines")



;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...

(defconst tir-fup-v-code-point  2300
  "If some post execeeds this point limit, which is about 3
screeens of rather airy text, program will go in more snoopy mode
to detect right logo cutting point. Instead of starting at the end, it
searches forward. Normally such a long post has _code_ tapped to the end of
message.

This is hard limit for function  tir-get-break-list to collect
possible break lines.
")



;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...

(defconst tir-v-break-mark-len-min 10
  "This says how long solid break mark is tried upon a line.")


(defconst tir-v-break-marks "-~_=+*><./zxv'`#:;,"
  "Collection of characters people usulally use for delimiting
Separate partiotions in their posting. You might wonder why there is
'zxv' , but imagine 80 char line with the. I have seen them.

NOTE 1: remember to have always '-' at front, because this is converted to
regexp [] form later.

NOTE 2: The beak algoritm used:
a1.   If line begins with one of these marks it _might_ be candidate
      for solid break
a2.   if all characters on the line are cat'd together, no spaces found,
      then it's solid break. This catches blocks like '-------------'
b1.   If line begins with one of these marks
b2.   If the line _is_not_ solid, ie. it contains whitespace, the read first
      block that end to whitespace. If full line is multiples of that
      block then it's solid break. This catches blocks like '-- -- --'

The algoritm isn't very selective, since after the initial break CHAR is found,
it doesn't care what chars follow it. That line may as well be uuencoded text

NOTE 2: What it can't detect yet
      Partial beaks, that hold text are not currently supported, like
      '--- look below --------------------'
")

;;; ..................................................... &v-fup-funcs ...


(defvar tir-fup-v-said-func 'tir-fup-mail-said
  "Function that will be called to compose the 'said' line at the top
of post. Function must accept one parameter, which is current date in
string form. The point is already placed at the top of post.

NOTE: this function will not be called if user haven't deleted the headers,
\(eg. by pressing abort\)
")

;;; .......................................................... &v-from ...

;;;     a) Initial SEARCH rexp, note leading space!
;;;        - Eg. "Resent-From:" is not what we are after...
;;;     b) Get first name and surname from that line matched
;;;        - Not necessarily have to be F S order, because rexp
;;;          tries to find both when searching signature.
(defconst tir-fup-sender-rexp " From:")

(defconst tir-A "[-a-zA-Z0-9.{|]"
  "Charset to represent persons name. Has scandinavian {| marks also.")

(defconst tir-AG
  (concat "\\("  tir-A "+\\)")
  "Goupped Aphabet matching re.")

(defconst tir-aG "\\([a-zA-Z0-9]+\\)"
  "Goupped Aphabet matching re::restrictive")

(defconst tir-fup-v-sender-re-name
  (concat tir-AG " +" tir-AG)
  "matches name  'From: Jari-Teppo Aalto jaalto@tre.tele.nokia.fi'")

(defconst tir-fup-v-sender-re-name-vax
  (concat "^" tir-AG "[ \t<\"]+[A-Z]+::" tir-AG)
  "matches VAX name 'TOMMI <\"TNCLUS::TSYVANEN\@tne01.tele.nokia.fi\"'")

(defconst tir-fup-v-sender-re-name-vax2
  (concat tir-AG "::" tir-AG)
  "matches VAX name '\"NTC01::KARINIEMI\"@tne01.tele.nokia.fi'
This is incomplete Name, it does not contain NAMES at all, but
we consider mail name as surname. The first group-RE is dummy.
")



(defconst tir-fup-v-sender-re-name-quotes-no
  (concat
   ;; finnish national phone abbrev = puh.
   "[.0-9]"
   )
  "*We can't be sure that the names are relly good names when we parse the
sender's From field. Let's see an example

        \"Person Someone p. nnn-nnn-nnn\"

There obviousously isn't 3rd name, it's used for phone abbrev. And the last
word is the actual phone number.
So this regexp tells which word matches are invalid when hit. If
this re is found, then those words are dopped out of possible candidates.
In this example it'd leave:
        \"Person Someone\"
")


(defconst tir-fup-v-sender-re-first-sur-email
  (concat tir-aG "\." tir-aG "@" )
  "matches address 'Job.Ganzevoort@cwi.nl', where person's name is compete
address.")


(defconst tir-fup-v-sender-re-gtw1
  (concat tir-aG "=" tir-aG "%" )
  "matches gateway-type addresses
'Marla=Bush%aoa.rdt%OS.DC@Ban-Gate.AoA.DHHS.EDU'")






;;; .......................................................... &v-subj ...
;;; Subject line specialities
;;;


(defconst tir-mail-subj-reply-prefix "Re! "
  "*If this is _first_ reply, add this in front of the subject text.
Other messages will have Re2: Re3: and so on.

References:
  If you are going to change this variable, you must update
  tir-mail-subj-re too.
")

(defconst tir-mail-subj-re "re[0-9]?[!:]"
  "What is matched when there is re: mark in subject line,
what are the possibilities...")

(defconst tir-mail-subj-fwd "\\[\\|\(fwd"
  "Rexp that identifies subject line which is forwarded.
Standard RMAIL gives     [mail_aaddr: orig-subject]
Elm and this TIR gives   orig-subj (fwd...)
")

(defconst tir-msg-sleep 4 "*Display time of informational messages")

;;}


;;}}}
;;{{{ setup: -- global variables

;;; ... Global variables, constants and like .................. &v-glo ...

(defconst tir-QA 1   "code to return when user requested abort.")
(defconst RET-1  nil "Global return value. ")
(defvar   tir-v-use-events nil     "The query method:use read-char or events")


;;  Check if we can find the event handling package
(if (fboundp 'event-to-character) nil   ;loaded ok
  (setq tir-v-use-events t)
  (condition-case err
      ;;   Have to use this: 19.28 distribution had no 'provide statement...
      (load-library "levents")
    (file-error
     (setq  tir-v-use-events nil)
     ;; too bad..., should we tell the 19.xx user about failure ?
     ;; Don't know if that el exist prior to 28, so...
     (if (or (null  (boundp 'emacs-minor-version))
             (< emacs-minor-version 28))
         nil                            ;prior to 19.28
       (message "Failed to load 'levents.el', please consider getting it.")
       (sit-for 2))
     )))




;;}}}
;;{{{ setup: -- VERSION

;;; ... version notice ...................................... &version ...

(defun tir-get-version ()
  "Returns version nbr"
  (substring  tir-version 11 15))

(defconst tir-version
  "$Revision: 1.70 $"
  "Latest version number.")

(defconst tir-version-id
  "$Id: tinyreply.el,v 1.70 1995/04/11 11:26:38 jaalto Release_2 jaalto $"
  "Latest modification time and version number.")

(defconst tir-version-doc
  "tinyreply.el --  A mail reply formatting package for rmail and gnus

First created: Oct 29th 1994
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tir-version ()
  "tinycom.el information."
  (interactive)
  (let* ((ob (current-buffer))
         (bp (get-buffer-create "*version*"))
         )
      (set-buffer bp)                   ;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tir-version-doc
       "\n\ncurrent version id:\n" tir-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;{{{ setup: user configurable

;;; ################################################### &user conf-beg ###
;;; ... user configurable ...................................... &conf ...
;;; - In general, you shouldn't only make changes here.
;;; - Of course you are free to experiment with other variables too,
;;;   but then you're on your own. I take no responsibility over the
;;;   responses or weird behaviour.
;;; - Some of the variables just aren't put here, because I wanted to
;;;   keep variables of same subject somehow together. I you need obvious
;;;   change, then just go ahead and pray it works :-/

;; tir-yank-mail uses mail-yank-prefix (emacs 19.22) variable if it exist
;;
(defvar tir-mail-yank-prefix
  (or (and (boundp 'mail-yank-prefix)   ;choose this if available
           mail-yank-prefix)
      ":")                              ;otherwise use this
"*Used by tir-yank-mail. The citation for replies,
please do _not_ put whitespace chars here. Use only /one/ char.")

(defvar tir-mail-yank-prefix-indent " "
  "*Used by tir-yank-mail. Spaces after tir-mail-yank-prefix before message.
Do not put _tabs_ here, only spaces.")


(defvar tir-yank-v-add-pfx t
  "*tir-yank-mail: When t, allows adding tir-mail-yank-prefix. See
hook tir-yank-hooks.")

(defvar tir-yank-hooks nil
  "*Hook that will be run when tir-yank-mail thinks it has yanked
the orinal message, possibly without any indentation. Note that
there may not be any text yanked due to GNUS. It doesn'keep text in
<yank buffer>, instead it uses insert-buffer *Article* or so...

References:
If you set variable tir-yank-v-add-pfx to nill, no furher processing
will be done. This means that yank prefixes aren't inserted.")

(defvar tir-yank-v-is-clean-post nil
  "*tir-yank-mail: will set this automatically, if yank were done.
nil = no text to examine ie. no yank happened.
0   = I think this is first message ie. no prev citations found.
1   = found citations, someone has already aswered to this.")


(defvar tir-yank-v-fmt-line-len 70
  "*tir-yank-format: Some user's accidentally text as continuous line
and replying to that don't look very nice. It's better to keep it
round 70, because you never know how many GNUS poster will reply to
it, gets flooded right quicly.")

;;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...
(defconst tir-kill-logo-sig-v-max-lines 20
  "The signature must be within 15 lines counting from the end of
message. Otherwise we might kill something which has nothing to do with sig.")


(defconst tir-kill-logo-getp-funcs
  '(tir-killp-context-diff
    )
  "*List of functions to be called when getting killing points.
Function acceps 1 argument, which is the starting point of
buffer where to look and it should return POINT or NIL. The point is
automatically fine tuned to nearest suitable killing position; left and
skipping empty lines upward.")


(defconst tir-kill-v-ctrl-re            ;Excludes tab,ff,cr,lf.
  "\\([\000-\010\016-\037]+\\|\\^[MJ]\\)"
  "*Regexp that is used by function tir-kill-ctrl-chars.
One day I got '^M^JThe Journal of' as plain '^' and 'M' so these
are included there too.
")

(defvar tir-kill-v-max-points 4
  "Set this value to maximum killing poins you care to see on screen.
There might not be that many found, but getting 20 is annoying...")

;;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...

(defvar tir-v-re-no-sender-copy ""
  "*Suppose you're using tir-news-copy-to-sender to send copy to author of
the post in some of the newsgroups. If you're regular poster in some group
and have go to known some of the people whom you know will read the group
in regular basis, then you might add the persons name in this variable.

Or if you have many accounts, that you're using for posting the messages,
it is appropriate that you don't get CC: to yourself if you reply to your
own post wg. for clearing things up, posting more info, solution found...

Anyway, this variable is REGEXP which is tried to match the 'From' field
of the senders HEADERS. If it matches, it prevents the automatic CC
field to be added when tir-news-copy-to-sender function is called.
")



;;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...
(defvar tir-fill-adap-v-ind " "
  "*This is one of those 'confortable' variables. When applying adaptive
filling it adds indentation when needed.

> john>> I could suggest you to..                    [1]
> john>>         - Design your project.              [2]
>I wouldn't be so sure about that..                  [3]
>Instead use..                                       [3]

Above are three regions that could be adaptive filled.  This variable
applies only to those cases where the text is immediately sticked with
CIT mark; in the example it would apply to case 3 only by making the lines
look like:
> I wouldn't be so sure about that..                 [3]
> Instead use..                                      [3]
 ^
 ^indent added here.
")


(defvar tir-fill-adap-v-s-reg "[-*o]"
  "*Some regions needs special handling:
- Mike said that..
  'This not the kind of subject I...'
- Marvin quoted instead...

This variable determines regions that have ^[WHITESPACE] DELIMITER WSPACE
combination to indicate regions, where the fill prefix is got from
the following line. Not current line.

Note, that there must be WSPACE on the right side, because someone might
write '--' or '-10 Celsius degree', or whatever.

There is possibility that someone writes with 1-num or A-Z like this:
   1.  See this
   2.  Here is another one
But that is beyond this packet. I will never try to fix filling to  those.
It just crunches them like usual paragraphs.
")

(defconst tir-fill-adap-v-no
  '(
    tir-v-re-code1
    tir-v-re-code2
    tir-v-re-code-rest
;;;    tir-fup-hdr-brk
    )
  "*List of VARIABLES that holds regexps which will be evaled later.
If any of the REs is found anywhere within the adaptive region,
it will be skipped.

It's better to define _small_ regexps because if the region is rejected,
you can find out which pattern caused rejection. If there is too many
ORs in _one_ pattern you might never know why it did it.

Look at the code of this .el to find out how you'd debug it this way.
")


;;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...

(defvar tir-study-v-re
  '("Newsgroups:" "Organization:"
   )
  "*List of REs that are used when studying buffer. If re matches,
whole line will be read. Use tir-study-buf-get-info to get
certain matched line.")

;;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...


;;; *** is just here because of context
(defconst tir-mail-my-info-none "no info"
  "Please do not change this. It is value for fields that hold no info")

;;;  These are just examples, the 'no info' prevents their usage.
;;;
(defvar tir-mail-my-info-list
  '(
    ;;  my local network addresses. This messae is sent to fellow workers
    ;;  only.
    ("tre\.tele\\|tnclus\\|tne0"
     "no info"                     ; keep this same as tir-mail-my-info-none!
     )
    ;; local country id. My friends all over the country.
    ("\.fi"
     "no info"
     )
   )
  "*List that contains 2 elements:
1) rexp
2) Info message string to be added if rexp match. Try to keep it
   relatively short, say max 5 lines. Add embedded \\n\\t for line breaks.

The rexp is tried on To: field, notice that the content of the field
is a matching pair:

      To: xxxxxx
          ^^^^^^^^^exclude word To: from the content.

You can then select different information messages to different
destinations. You can use your country id to supply message written in
your native language, or you can try to match your local network
addresses where you put another kind of info text.

It's better to include contact information on the header, so that the
trailing logo isn't crowded too much. Nobody is interested in reading end
logos after they have seen it multiple times, sufficent information will
do there; like your name and occupation perhaps.

Remember to have \\n at the end of message and put REs in the order
of evaluation: restrictive rgexps first.

In case none of these matches, the variable tir-mail-my-info-def
will be inserted only if it doesn't contain tir-mail-my-info-none.

NOTE:
-  There is no reason to define all possible mail REGEXP so that
   you can get the right Info field inserted.
-  When I'm sending mail within our internal host, there is no '@site'
   attached to the user's address. This means that the Info isn't
   automatically added. Occasionally I wan't to add the info field
   so that person gets my contact data.
-  What I do is temporarily add some of the REGEXP text to the user's
   address, just few words that I know will trigger Info insertion.
   Then I hit C-c C-z i to get the info and delete those extra chars
   I added.
")


(defvar  tir-mail-my-info-def tir-mail-my-info-none
  "*Default information to be added to messages sent elswhere.")

;;;  note #!/bin/perl , the '!' would otw be valid cit pos
;;;
(defvar tir-del-re-v-look-until
  '("http" "URL" "#!/" )
  "*Special words or rexps that restrict the maximum position of RE delete.
See more info by hitting describe-function tir-adjust-last-re.")


;;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...


(defconst tir-fwd-v-hdr-max-len 6
  "*When looking for Forward header 'sender' na empty line will terminate
search. However to prevent exessive long hits we need to delimit the
search somehow to reasonable limit. If header line count after hit exceeds
this, it is not considered valid header. See tir-fwd-get-info.
")

(defconst tir-fwd-v-start-re "Forwarded BEGIN"
  "*Other mailers may have their own way to forward message. This
regexp tells which kind of lines mean successive forwarded message's
header begin position in this .el. See tir-fwd-get-fb-pos

Update this if you change tir-fwd-v-start-str.
")

(defconst tir-fwd-v-start-str "~~~~~~ Forwarded BEGIN"
  "*string to be inserted at the beginning of forwarded body.
No newlines allowed within text

Keep this string relatively short, because INFO will be added to
the end of this string. See tir-fwd-format-->tir-fwd-get-info
")

(defconst tir-fwd-v-end-str "~~~~~~ Forwarded END"
  "*string to be inserted at the beginning of forwarded body.
No newlines allowed within text")

;;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...

;;  - This not complete list, just happens to include countries that
;;    keeps sending messages to GNUS groups I read.
;;  - This is used only for formatting DATE, so it's not very important issue.
;;  - If you need to add more, use the following in to your .emacs
;;* (setq tir-v-europe-sites-alist
;;*       (append
;;*        '(("other-country" 1)
;;*         ...
;;*        )
;;*        tir-v-europe-sites-alist)
(defconst tir-v-europe-sites-alist
  '(("fi" . 1) ("se" . 1) ("no" . 1) ("uk" . 1)
    ("de" . 1) ("it" . 1) ("ne" . 1) ("fr" . 1)
    )
  "*Defines sites that use Eurepean time format. If the country is
on this list the date format used for quoting is dd.mm.yy. This is just
for convenience to European users.")

;;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...

(defvar tir-v-fcc-by-hdrs
  '(("user-re" "~/.user-reply-folder") ; Just dummy values to demonstrate
    )
  "Folders by searching header area. The first item is regexp, which looks
first match within headers. I usually use FROM field's user for special
folders.")


(defvar tir-v-fcc-by-study
  '(("groups:.*\\(emacs\\|gnu\\)" "~/.post-gnu")
   )
  "*List of \(RE FOLDER) pairs. The RE is tried to every member in
tir-study-v-re results and first line matched will deteermine the folder.
So, be carefull with the order of regexp. Use most restrictive first, and
most general as last RE.

Set RE to \"\" if you want to disable it.
")

;;; .......................................................... &v-snip....



(defvar tir-v-snip-max-len 45
  "*How many _chars_ to include for paragraph begin snip. Hard limit.
The snip-words is tried first. If it exeecds this, it will be simply
cut down rudely.
")

(defvar tir-v-snip-words 10
  "*How many _words_ to include for paragraph begin snip. Soft limit.
If there are not that many words, it's due to hard limit.

Don't set this too high, because words are always gathered and concatenated,
before hard limit is examined. It takes time to collect.

See tir-v-snip-max-len.")


(defvar tir-snip-v-type-alist
  (list
   (list tir-v-re-code1 "code")
   (list tir-v-re-code2 "code")
   (list tir-v-re-code-rest     "code")
   )
  "Snip text types according to rexp. Notice that first one matched will
be used. Try to keep EXPLANATION TEXT as general as possible")

(defvar  tir-v-snip-skip-rex-line
  (list
   (concat ".*wr\\(o\\|i\\)tes?:\\|.*rom.*:\\|"
           ".*date.*:\\|[A-Z][a-z][a-z][ \t][0-9]\\|"
           ".*\\(hi,?\\|you:\\)"
           ))
  "*User may set the BEG END area for snipping whereever he wishes.
That means that the BEG might not be the best place to start collecting
meaninfull 'words' for quoting.

This is a 'looking-at' exclude line *_list_*. The cursor is always
positioned under word on the line before 'look' is tried.
That means that you cannot use '^' at the beginning of rexp meaning
phyysical line beginning. The cursor is positioned under word, because
only words can start the quoting. Example below shows cursor [x] position.

> > > > [x]John> Why can't I use...

REXP examples:
Oct 23|Wed 1   -- [A-Z][a-z][a-z] [0-9]

") ;; ...  ...  ...  ...  ...  ...  ...  ...   END rex-line    ............

;;; ################################################### &user conf-end ###
;;}}}


;;{{{ code: -- enchancements to RMAIL

;;; ################################################## &bind-additions ###

;;; ----------------------------------------------------------------------
;;;   This function have effect *only* if you are using the 'nuke' 
;;;   function keybinding.
;;;
(defun tir-reply-cc-kill (&optional arg)
  "Replies to mail and kills automatically CC: fields.  Args are ignored"
  (interactive "P")
  (let* ((func (if (eq major-mode 'rmail-summary-mode)
                   'rmail-summary-reply
                 'rmail-reply))
         (copy tir-v-cc-kill-flag)
         )

    ;;   Temporary change this value and restore it after replied.
    (setq  tir-v-cc-kill-flag t)
    (funcall func nil)
    (setq  tir-v-cc-kill-flag copy)
    ))

;;; ----------------------------------------------------------------------
;;;   function have effect only if you have copied the example setup
;;;   and use 'nuke' function.
(defun tir-reply (&optional arg)
  "Replies to mail and queries CC: field killing. Args are ignored"
  (interactive "P")
  (let* ((func (if (eq major-mode 'rmail-summary-mode)
                   'rmail-summary-reply
                 'rmail-reply)))
    (setq  tir-v-cc-kill-flag nil)
  (funcall func nil)))

;;}}}
;;{{{ code: -- replaced/modified  ORIGINAL EMACS functions

;;; ############################################### &original replaced ###
;;; Original functions of emacs customized for this packet.
;;; - modified, changed added etc...


;;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  rmail vvvvvv
;;; .......................................... self standing part ........
(defvar rmail-forward-hook nil
  "*run after forwarded message is inserted into mail buffer")

;;;  This function will be defined again elswhere, just informative here.
(defun rmail-forward-form-subj (orig-sender subj)
  "Called by forward. If this func returns nil then original format used."
  nil)


;;; ------------------------------------------------------------ 18.57 ---
;;; This function is from rmail.el for emacs 18.57
;;; - In order to do my own 'forward' message formatting, I had to  add
;;;   rmail-forward-hook which will be run after message is added
;;;   to buffer.
;;; - The Subject field can be controlled by doing own function
;;;   rmail-forward-form-subj (format subject), if it returns nil
;;;   then the original rmail subject formatting is used.
;;;
(defun rmail-forward18-57 ()
  "Forward the current message to another user."
  (interactive)
  ;;>> this gets set even if we abort. Can't do anything about it, though.
  (rmail-set-attribute "forwarded" t)
  (let* ((forward-buffer (current-buffer))
         (orig-sender  (mail-strip-quoted-names (mail-fetch-field "From")))
         (subj (or (mail-fetch-field "Subject") ""))
         (orig-subject (concat "[" subj ":" orig-sender "]"))
         (subject orig-subject)
         ret
         )

    ;;  ++ MY ADDITION
    ;;  ++ replace subject if something returned.
    (if (setq ret (rmail-forward-form-subj orig-sender subj))
        (setq subject ret))

    ;; If only one window, use it for the mail buffer.
    ;; Otherwise, use another window for the mail buffer
    ;; so that the Rmail buffer remains visible
    ;; and sending the mail will get back to it.

    ;; ++ Note, that when mail is called it runs mail-setup-hook
    ;; ++   --> only header it put, AND yank buffer contains nothing !
    (if (if (one-window-p t)
            (mail nil nil subject)
          (mail-other-window nil nil subject))
        (save-excursion
          (goto-char (point-max))
          (forward-line 1)
          (insert-buffer forward-buffer)

          ;; ++ added to original code ++
          (run-hooks 'rmail-forward-hook)
          )
      )
    ;; ------------ let, defun
    ))


;;; ------------------------------------------------------------ 19.28 ---
;;; Same changes than above
;;;
(defun rmail-forward19-28 (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `rmail-resend'."
 (interactive "P")
  (if resend
      (call-interactively 'rmail-resend)
    (let* (orig-sender                  ;++ added
           (subj (or (mail-fetch-field "Subject") "")) ;++ added
           (forward-buffer (current-buffer))
           (subject
            (concat "["
                    (let* ((from (or (mail-fetch-field "From")
                                    (mail-fetch-field ">From"))))
                      (setq             ;++ added
                       orig-sender      ;++ added
                       (if from
                           (concat (mail-strip-quoted-names from) ": ")
                         "")))
                    subj
                    "]")))

      ;;  ++ MY ADDITION
      ;;  ++ replace subject if something returned.
      (if (setq ret (rmail-forward-form-subj orig-sender subj))
          (setq subject ret))

      ;; If only one window, use it for the mail buffer.
      ;; Otherwise, use another window for the mail buffer
      ;; so that the Rmail buffer remains visible
      ;; and sending the mail will get back to it.
      (if (funcall (if (and (not rmail-mail-new-frame) (one-window-p t))
                       (function mail)
                     (function rmail-start-mail))
                   nil nil subject nil nil nil
                   (list (list (function (lambda (buf msgnum)
                                           (save-excursion
                                             (set-buffer buf)
                                             (rmail-set-attribute
                                              "forwarded" t msgnum))))
                               (current-buffer)
                               rmail-current-message)))
          (save-excursion
            ;; Insert after header separator--before signature if any.
            (goto-char (point-min))
            (search-forward-regexp
             (concat "^" (regexp-quote mail-header-separator) "$"))
            (forward-line 1)
            (insert-buffer forward-buffer)

            ;; ++ added to original code ++
            (run-hooks 'rmail-forward-hook)
            ;;   I tried to set curson on "TO:" field, but
            ;;   It didn't work. The caller hadles it somewhere else...
            )))))

;;; .......................................... customized part  ..........

(defconst tir-v-rmail-forward-overload nil
  "Determines if we can replace rmail-forward and use our own functions")
(setq tir-v-rmail-forward-overload
      (string-match
       "18.57\\|19.2[78]"
       emacs-version
       ))

;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
;;;
(if tir-v-rmail-forward-overload (progn

(defun rmail-forward (&optional arg)
  "Acts as umbrella function for various emacs versions. Calls
modified version of rmail-forward so that tinyreply.el [TIR]
can format forwarded message.
"
  (interactive)
  (cond
   ((string-match "18.57" emacs-version )
    (rmail-forward18-57))
   ((string-match "19.2[78]" emacs-version )
    (rmail-forward19-28 arg))
   ))


)) ;; END OVERLOAD (PROGN...)
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--



;; This installs hook
(or (memq 'tir-fwd-format rmail-forward-hook)
    (setq rmail-forward-hook (append rmail-forward-hook
                                  '(tir-fwd-format))))

;;}}}

;;{{{ code: macros
;;; ########################################################## &macros ###
;;;  To see what the'll become use for example:
;;;  (macroexpand '(dec x))

(defmacro inc (var)
  "Increments by 1. See GNU Lisp manual sec. 12 MACROS"
  (list 'setq var (list '1+ var)))

(defmacro inci (var i)
  "Increments by i."
  (list 'setq var (list '+ var i)))

(defmacro dec (var)
  "Decrement by 1."
  (list 'setq var (list '- var 1)))

(defmacro deci (var)
  "Decrement by i"
  (list 'setq var (list '- var i)))

(defmacro strcat (var arg)
  "Cat string to variable"
  (list 'setq var (list 'concat var arg)))

;; ** DO NOT USE, not working, sketch only  **
(defmacro strcat2 (var &rest args)
  "Cat string to variable"
  (list 'setq var (list 'mapconcat ''concat args " ")))

(defmacro tir-myp ()
  "Returns yank prefix used by this tir module."
   (concat  tir-mail-yank-prefix tir-mail-yank-prefix-indent))

(defmacro tir-cl ()
  "Returns current line from counting point-min. There may be off by -1 error."
  (count-lines (point-min) (point)))


;;}}}

;;{{{ code: misc general
;;; ############################################################ &misc ###
;;; General functions

(defun tir-yn (&optional msg choice strict)
  "Asks Y/N question by reading char. Message and choice configurable.
Parameter STRICT limits choices to ok/nok/abort only.

Uses 19.xx event reading if it's available, so it won't get upset by
mouseclicks or arrow keys.

RETURN
  nil           ,n or BACKSPACE
  1             ,[a]bort or [q]uit
  t             ,y or SPC/ENTER
  xx            ,any other entry returned as character code.
                 if the event is not recognized the tir-QA is returned.
"
  ;; (interactive)
  (let* (ret
         (EVENT tir-v-use-events)       ;can we use events ?
         event ch (loop t)
         )
    (if (eq nil msg) (setq msg ""))
    (if (eq nil choice) (setq choice " [y]/n:"))


    ;;  the use of %s allows displaying msg="%", otw it'd be error.
    (while loop
      (setq loop nil)
      (message "%s" (concat msg choice))
      (if (null EVENT)
          (setq ch (read-char))         ; emacs 18.xx
        ;;   By using event reading, we can accept any action and it doesn't
        ;;   break this function.
        (setq event (read-event))

        (setq ch (event-to-character event))
        (if ch
            (setq ch (downcase ch))
          ;;   send abort code if event not recognized
          (if (memq event '(return backspace)) nil
            (setq ch ?a)))
        )
      (message "")        ; clear

      (cond
       ((or (eq ch ?n) (eq ch ?\177)    ; the DEL character
            (eq event 'backspace))
        (setq ret nil))
       ((or (eq ch ?a) (eq ch ?q))
        (setq ret tir-QA))              ;send abort code
       ((or (eq ch 13) (eq ch 32) (eq ch ?y) ;RET or SPC, y
            (eq event 'return))
        (setq ret t))
       (t                               ;the keycode itself
        (setq ret ch loop t)))          ;not any of the above answers

      (if (null strict)
          (setq loop nil)       ;cancel the loop if NOT-STRICT
        (if (null loop) nil             ;Right answer
          (message "Please answer the question.")
          (sleep-for 1)))
      )
    ret
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-msg (mode &rest args)
  "Print tinyreply.el messages."
  (let* ((pfx "tir: ")                  ;prefix
         (err "*err ")                  ;when errors
         (msg pfx)
         (i 0)
         el
         (msg "")
         )

    ;; pasting elements together
    (while (< i (length args))
      (setq el (nth i args))

      (if (stringp el)
          (if (= 0 (length el)) (setq el "\"\"")))
      (if (eq nil el) (setq el "nil"))
      (if (eq t el) (setq el "t"))

      (setq msg (concat msg  el))
      (setq i (1+ i)))

    ;; is this error conditions ?
    (if mode (setq pfx (concat err pfx)))
    (message (concat pfx msg))
    (sleep-for tir-msg-sleep)
    nil))


;;; ----------------------------------------------------------------------
(defun tir-msg-quote-re (str)
  "Quotes that '%' which causes promlem with 'message' cmd."
  ;; **** NOT USED **** educational only
  ;; (interactive)
  (let* (p e (p1 "") l)
    (setq l str)
    (while (string-match "%" l)
      (setq p (+ 1 (match-beginning 0)))
      (setq p1 (concat p1 (substring l 0 p) "%"))
      (setq l (substring l p e)))
    (concat p1 l)))




;;; ----------------------------------------------------------------------
;;;
(defun tir-mail-get-field (field-name &optional any)
  "Gets named field from (R)MAIL buffer. If parameter any is non-nil
then The field can preceed any characters.

INPUT :
   field-name      ,field name without : char
   any             ,if non-nil, then drops the ^ from the  ^field: criteria
RETURN:
   \"\" or contents of field."
  (interactive "sField:")
  (let ((case-fold-search t)            ;ignore case = t
        (ret "")
        (re (if any
                (concat field-name ":")  ; pick first one met
              (concat "^" field-name ":"))); require STRICT HEADER

        ;;  Where is next field
        (r2 (if any
                ":[ \t]*[A-Z]"
              "^[A-Z][-a-z]*:[ \t]*"))
        ep
        )
    (save-excursion
      (goto-char (point-min))
      (if (null (re-search-forward re nil t)) nil
        (save-excursion                 ;now search end point
          (if (null (re-search-forward r2 nil t))
              (setq ep (progn (forward-line 1) (point))) ;no next field
            (setq ep (progn (beginning-of-line) (1- (point))))))
        (setq ret (buffer-substring (point) ep))))
    ret))



;;; ----------------------------------------------------------------------
;;;
(defun tir-get-words (&optional re Nre pos maxpos)
  "Convert Line to word list starting from current position to the EOL.
re     = skipped text , [ \t\n]+ default
Nre    = 'not' re     , [^ \t\n]+   , the opposite
pos    = start here   , default current pos
maxpos = end          , default next line start

You must give at least RE and NRE if you do not use defaults.

NOTE:
   This is not guarranteed to work anywhere else than in TIR.
"
  ;; (interactive)
  (let (li
        ep np p w
        (loop t)
        in-loop
        )
    (if pos (goto-char pos))            ; start here
    (setq re (or re "[ \t\n]+"))        ; default ?
    (setq Nre (or Nre "[^ \t\n]+"))

    (if maxpos nil                      ; skip, it's given
      (setq maxpos                      ; default is one line
            (save-excursion
              (end-of-line) (point))))

    (save-excursion
      (setq p (point))
      (while (and (< (point) maxpos)
                  (re-search-forward re  nil t))
        (setq in-loop t)                ; we were here!
        (setq np (point))               ; new point
        ;;  it skips liiitttle too much :-/
        (if (re-search-backward Nre nil t)
            (forward-char))

        (setq ep (point))               ; skipped portion start == word end
        (setq w (buffer-substring p ep))
;;;     (d! "WORD"  p ep np (point) maxpos w)
        (goto-char np) (setq p np)
        (setq li (append li (list w) nil))
        ))

    (if (null in-loop) nil
      (setq w
            (buffer-substring
             p
             (progn (end-of-line) (point))))
      (setq li (append li (list w) nil)))       ;handle last word

    ;; ------------------- save-excursion
    li
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-cnv-s2l (str &optional rep-rex)
  "Convert String to 1 space list. use REP-REX character as separators.
Default separators are [, \t].

Returns: bp  , buffer pointer where elements are listed.
"
  ;; (interactive)
  (let ((tb tir-tmp-buf)
        (re (or rep-rex "[ ,\t\n]+"))
        (loop t)
        bp
        li
        )
    (setq bp (get-buffer-create tb))

    (save-excursion
      (set-buffer bp) (erase-buffer) (insert str)
      (goto-char (point-min))
      (replace-regexp re " ")           ; convert all to one SPACE
      (goto-char (point-min))
      )
    bp
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-kill-ctrl-chars ()
  "Removes all control characters from buffer. See var. tir-kill-v-ctrl-re"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp tir-kill-v-ctrl-re "")))



;;; ----------------------------------------------------------------------
;;;
(defun tir-del-re (re str)
  "Removes the part that RE matches from string."
  (let* ( len mb me )
    (if (eq nil (string-match re str)) nil
      (setq mb (match-beginning 0))  (setq me (match-end 0))
      (setq len (length str))
      (cond
       ((eq 0 mb)                       ;remove beg
        (setq str (substring str me len)))
       ((eq len me)                     ;remove end
        (setq str (substring str 0 mb)))
       (t                               ;middle
        (setq str
              (concat
               (substring str 0 mb)
               (substring str me len))))
       ))
    str
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tir-del-my-cit (str)
  "Removes citation added by tir --> Gets original line."
  (let* ((istr (tir-myp))
         (len (length istr))
         )
    (if (< (length str) len)
        str                             ;too short
      (if (string-match (concat "^" istr) str)
          (substring str len (length str))))
    ))


(defun tir-del-list-re (list re)
  "Deletes all elements that matches RE. List must be string list."
  (let* (out el (i 0))
    (while (setq el (nth i list))
      (if (string-match re el) nil      ;drop
        (setq out
              (append out (list el) nil)))
      (inc i)
      )
    out
    ))




;;; ----------------------------------------------------------------------
;;; - I noticed that the idea of tir-1-space could be used here,
;;;   so the structure exactly the same.
;;;
(defun tir-get-blocks (str sep &optional inc)
  "Separares STR into words separated by SEP. If optional INC is
given, the matched SEP is included. Eg.

      \"Mike M.Monroe\"   SEP = [ .]+ , INC = [.]
   -->\(\"Mike\" \"M.\" \"Monroe\"\)

All leading SEP chars are ignored and they _are_not included allthough
INC has such chars. Thus:

       \"            \"   SEP = [ .]+ , INC = [ ]

returns effectively nil.
"
  (let (el
        (re (concat sep "\\|$"))
        SEP beg end ch ret
        )

    ;; Ignore leading matches ...
    (if (string-match (concat "^" sep) str)
        (setq str (substring str (match-end 0))))

    (while (and (> (length str) 0)
                (string-match re str))
      (setq ch ""  SEP nil  beg (match-beginning 0)  end (match-end 0))
      (if beg
          (setq SEP (substring str beg end)))

      (if (and beg                      ;should we include that match
               (null inc))
          nil
        (if (string-match inc SEP)      ;belongs to the INC ?
            (setq ch (substring SEP (match-beginning 0) (match-end 0))))

;;;     (d! SEP inc ch)
        )

      ;;  no more matches? , the "$" matched ...
      (if (eq beg (length str))
          (progn
            ;;  is the rest of it re?
            (if (string-match (concat sep "$") str) nil
              (setq str (concat str ch))
              (setq ret (append ret (list str))))
            (setq str ""))                      ;found empty space
        (setq el (concat (substring str 0 beg) ch))
        (setq ret (append ret (list el)))
        (setq str (substring str end)))
      )
    ret
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tir-end-slash (str)
  "Makes sure STR has terminating slash."
  (let (last-ch)
    (if (null (stringp str))
        nil
      (setq last-ch (substring str (- (length str) 1) (length str)  ))
      (if (not (equal "/" last-ch))
          (concat str "/")
        str))
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tir-get-load-path (lib paths)
  "Return full path name for library LIB accross the PATHS"
  (let ((dir paths)
        (loop t)
        file found
        )
    (while (and loop dir)
      (setq file (concat (tir-end-slash (car dir)) lib))
      (if (file-exists-p file)
          (setq found file  loop nil))
      (setq dir (cdr dir)))
    found
    ))


;;}}}
;;{{{ code: misc go, other special

;;; ----------------------------------------------------------------------
(defun tir-go-txt ()
  "Goes to the beginning of mail message. RET=nil if cannot found"
  (goto-char (point-min))
  (if (re-search-forward tir-fup-tail-nrm-txt-start nil t)
      (forward-line 1)
    nil
    ))


;;; ----------------------------------------------------------------------
;;; - People don't want to send their private mail to me when I asked the
;;;   mail that caused TIR to fail somehow. Still I need _exactly_ the same
;;;    post to determine what went wrong, so that I can debug the error.
;;; - With this, user can make the 'core' of the post, normally written
;;;   text, unreadable, while it still preserves the form of the text.
;;;
(defun tir-scramble-reg (beg end &optional char)
  "Scrables text so that it's not reasable any more. Preserves
words by substituting every [a-zA-Z] with optional CHAR.
"
  (interactive "r")
  (let* ((ch (if char                   ;pick the scramble char
                 (char-to-string char)
               "o"))
         )
    (save-excursion
      (save-restriction                 ;preserve prev narrowing
        (narrow-to-region beg end)
        (goto-char (point-min))
        (replace-regexp "[a-zA-Z]" ch)
        ))))


;;}}}
;;{{{ code: misc line handling



;;; ----------------------------------------------------------------------
;;;
(defun tir-is-cit (beg end &optional str)
  "Tests is this region or optional STR really is citation.
See tir-*last function also."
  (let* (;;  only if "> > mike>>"  form, all others are not citations
         (nre (concat "[^" tir-cit " \ta-zA-Z]"))
         )
    (if str nil                         ;skip it's given
        (setq str (buffer-substring beg end))) ; read it
    (if (string-match nre str) ; YES found s-thing else --> NOT citation
        nil t)
  ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-is-line-cit ()
  "Checks if line has citations at the beginning. Supposes that
there must be CIT at the beginning of line if line is citated
RET= t or nil"
  (save-excursion
    (beginning-of-line)
    (looking-at tir-cit)))

;;; ----------------------------------------------------------------------
;;;
(defun tir-get-wspc (&optional pp)
  "Gets whitespace following the point or optional PP point.
RET = '' or whitespace string
"
  (let* (mp op                          ;maximum point, end of line
         (re-w "[ \t]+")                ;whitespace
         )
    (save-excursion
      (if (null pp)
          (setq op (point))
        (setq op pp)
        (goto-char pp))
      (save-excursion (end-of-line) (setq mp (point)))
      (if (or (null  (looking-at re-w)) ;not sitting on whitespace
              (null (re-search-forward re-w mp t)))
          ""
        (buffer-substring op (point)))
  )))



;;; ----------------------------------------------------------------------
(defun tir-sqz (str)
  "squeeze empty spaces around '    txt    '"
  (if (string-match "^[ \t]*\\(.*\\)" str)
      (setq str
            (substring str (match-beginning 1) (match-end 1))))
  (if (string-match "[ \t]*$" str)
      (setq str
            (substring str 0  (match-beginning 0))))
  str)



;;; ----------------------------------------------------------------------
;;;
(defun tir-1-space (str)
  "Convers all multiple spaces/tabs in string into 1 space."
  (let* (
         (out "") beg end
         )
    (while (and (> (length str) 0)
                (string-match "[ \t]+\\|$" str))
      (setq beg (match-beginning 0) end (match-end 0))
      ;;  Take only 1 space (1+ ..
      ;;
      ;;  no more spaces ? , the "$" matched ...
      (if (eq beg (length str))
          (progn
            ;;  is the rest of it spaces ?
            (if (string-match "[ \t]+$" str) nil
              (setq out (concat out str)))
            (setq str ""))                      ;found empty space
        (setq out (concat out (substring str 0 (1+ beg))))
        (setq str (substring str end)))
      )
    out
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-kill-line (&optional p)
  "Kills line shifting others up. Optionally at Point.
Works on narrowed region as well. RET = t if line killed."
  ;; (interactive)
  (let ((ret t))
    (save-excursion
      (if p (goto-char p))
      (beginning-of-line)

      (if (null (looking-at "^$")) nil  ;Handle empty line
        (cond
         ((and (eobp) (bobp))           ;nothing to do
          (setq ret nil))
         ((eobp)
          (delete-backward-char 1))
         (t
          (kill-line))))

      (if (looking-at "^$") nil         ;Handle Something on the line
        (kill-line)
        (cond
         ((and (eobp) (bobp))           ;nothing to do more
          (setq ret nil))
         ((eobp)
          (delete-backward-char 1))
         (t
          (forward-line)
          (delete-backward-char 1))))
      ret
      )))



;;; ----------------------------------------------------------------------
;;;
(defun tir-is-line-len (beg end len)
  "Checks if there exist line longer than LEN. Returns beginning point
of suc line or nil."
  (let* (tt (pos nil))
    (if (> beg end)                 ;keep E > B
        (setq tt beg beg end end tt))
    (save-excursion
      (goto-char beg)
      (while (and (null pos) (< (point) end) (not(eobp)))
        (end-of-line)
        (if (<= (current-column) len)  nil
          (beginning-of-line) (setq pos (point)) )
        (forward-line 1)))
    pos
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-move-to-col (col)
  "Doesn't care about line length. Inserts spaces to get to COL."
  ;; (interactive "Nto:")
  (move-to-column col)
  (if (eq (current-column) col) nil     ;skip, ok we got there
    (while (not (eq (current-column) col))
      (insert " "))))

;;; ----------------------------------------------------------------------
;;;
(defun tir-move-to-para (&optional direc)
  "Moves forward para in reply bypassing empty lines
with citations. If DIREC is non-nil, moves backward.

RET = t if positioned at the beg of para."
  (interactive)
  (let ((op (point))
        (test-f (if direc 'beobp 'eobp))
        (move-f (if direc 'backward-line 'forward-line))
        (assert-f (if direc 'beginning-of-line  'end-of-line))
        )
    ;;   - if we're sitting on empty line move to PARA begin
    ;;   - if we're inside PARA , move to next para.'
    (while (and (not (tir-is-empty-line))       ; ** INSIDE para
                (null (funcall test-f))
                )       ;until PARA found
      (funcall move-f 1)
      (funcall assert-f))

    (if (tir-is-empty-line)
        (while (and (tir-is-empty-line)
                    (null (funcall test-f))
                    )   ;until PARA found
          (funcall move-f 1)
          (funcall assert-f))
      )

  (if (funcall test-f)
      (progn (goto-char op) nil)
    (beginning-of-line) t)
  ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-read-line (&optional len)
  "Reads whole line from buffer. Optionally LEN chars. If LEN is more than
line has characters then returns whole line."
  (let (line)
    (save-excursion
      (setq line (buffer-substring
                  (progn (beginning-of-line) (point))
                  (progn (end-of-line) (point))))
      (if (and len
               (< len (length line)))
          (substring line 0 len)
        line))))


;;; ----------------------------------------------------------------------
;;;
(defun tir-read-if-solid ()
  "Reads from current point all the character to the end of line, provided
that there is no whitespaces between the point and eol. Ignores
leading and trailing whitespace."
  (let* (beg
         (end (save-excursion (end-of-line) (point)))
         ret
         )
    (save-excursion
      (if (looking-at "[ \t]+")
          (skip-syntax-forward " "))
      (setq beg (point))
      (if (eolp) nil
        (skip-syntax-forward "^ " end)
        (if (eq (point) beg)            ;not moved
            (end-of-line))              ; no trailing spaces

        (if (eq (point) beg) nil        ;still no movement
          (if (null (looking-at "[ \t]*$")) nil
            (setq ret (buffer-substring beg (point)))))
        ))
    ret
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-read-syntax-word (syntax &optional back)
  "Reads block of chars from current point. If optional BACK is given,
then thelooks back to find start of point. Blocks are separated by SYNTAX.

If SYNTAX is 'w' then reads word.
"
  (let* (ret
         ;; set line limits
         (lb (save-excursion (beginning-of-line) (point)))
         (le (save-excursion (end-of-line) (point)))
         (beg  (point))
         end
         )
    (save-excursion
      (if back
        (save-excursion
          (skip-syntax-backward syntax lb) (setq beg (point))))
      (skip-syntax-forward syntax le) (setq end (point))
      (if (eq beg end) nil
        (setq ret (buffer-substring beg end))
        ))
    ret
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-read-block (&optional back)
  "Reads block of chars from current point. If optional BACK is given,
then thelooks back to find start of point. Blocks are separated by space, eol
or bol.
"
  (let* (ret
         ;; set line limits
         (lb (save-excursion (beginning-of-line) (point)))
         (le (save-excursion (end-of-line) (point)))
         (beg  (point))
         (re "[ \t]")
         )
    (save-excursion
      (if back
        (save-excursion
          (if (re-search-backward re lb t)
              (forward-char 1)
            (beginning-of-line))
          (setq beg (point))))

      (if (re-search-forward re le t)
          (forward-char -1)
        (end-of-line))

      (if (eq beg (point)) nil
        (setq ret (buffer-substring beg (point)))
        ))
    ret))



;;; ----------------------------------------------------------------------
;;;
(defun tir-line-len (&optional pp)
  "Length of current line.Optionally from location PP."
  ;; (interactive)
  (save-excursion
    (if (not (eq nil pp)) (goto-char pp))
    (end-of-line)
    (current-column)))



;;; ----------------------------------------------------------------------
;;;
(defun tir-is-empty-line (&optional pp)
  "Checks if line has only tir-cit characters. Look from optional PP point.
RET=t line is such.."
  (let* (
         ;;  add WHITE SPACE  to charset too
         (not-rex (concat "[^" tir-cit " \t" "]"))
         line
         )
    (save-excursion
      (if pp (goto-char pp))
      (setq line (tir-read-line))
      ;; remember that str-ma return nil or nbr position in line
      ;; nil means that all are citation characters
      (if (equal nil (string-match not-rex line)) t nil))))



;;; ----------------------------------------------------------------------
;;;
(defun tir-is-uuline (&optional p)
  "Determines if line is UUencoded. Optionally looks at POINT line
The line is considered as an uu line if it has only uppercase chars and has
length more than 50 chars.

Returns length of line if it's UU, nil if not.
"
  ;; (interactive)
  (let* (bp
         ep len ret
         (at-least 50)
         (case-fold-search nil)         ;case is important
         )
    (save-excursion
      (if p (goto-char p))
      (beginning-of-line) (setq bp (point))
      (end-of-line)       (setq ep (point))
      (setq len (- ep bp))

      (if (re-search-backward "[a-z]" bp t) nil ;--> not UU line
        (if (> len  at-least)   ;must be longer than 40 chars
            (setq ret len)))
      ret
      )))




;;; ----------------------------------------------------------------------
;;;
(defun tir-fwd-line (&optional count)
  "Moves vertically lines down. If COUNT is negative, then up.
NOTE:
  forward-line moves point always at the beginning
  of next line, and the elisp manual says not to use next-line in
  programs. That's why I coded my own function.

  This function behaves exactly as next-line. If the next line is shorter
  it moves to the end of line."
  ;; (interactive "P")
  (let* ((col (current-column)))
    (and (null count) (setq count 1))   ;No arg given
    (forward-line count)
    (move-to-column col)
    ))

;;}}}
;;{{{ code: misc lists

;;; ############################################################ &list ###
;;; Functions for list handling


;;; ----------------------------------------------------------------------
;;;
(defun tir-slice-list (mode arg l)
  "Gets portions of list L according to MODE and ARG."
  (let* ((len (length l))
         (n-len (1- len))               ;for nth command
         i ret
        )
    (if (null l) nil                    ;empty
      (cond
       ((eq mode 'last)
        (if (> arg len) nil             ;doesn't have that many
          (setq i (- len arg))          ;start pos
          (while (< i len)
            (setq el (nth i l))
            (setq ret (append ret (list el)))
            (inc i))))
       )
      ret
      )))




;;; ----------------------------------------------------------------------
;;;
(defun tir-nconc-num (num lst)
  "Adds NUM to numbered LST. Keeps nums in ascending order 1234..
No duplicate member is inserted and supposes that the list is already ordered.
Original list is not modified. "
  (let* ((loop t)
         (l (copy-sequence lst))        ;'cause there is setcdr
         (ptr l)
         (i 0)
         )
    (if (null l)
        (setq l (cons num l))           ;first element

      (if (memq num l) (setq loop nil)) ;already there

      (if (or (null loop)
              (> num (nth 0 l))) nil    ;add to the BEG
        (setq loop nil)
        (setq l (append (list num) l)))

      (if (or (null loop)               ;add to the END
              (> (nth (1- (length l)) l) num))
          nil   ;add to the END
        (setq loop nil)
        (setq l (append l (list num))))

      (while (and loop ptr)
        (if (< (car (cdr ptr)) num) nil ; look at the next element
          ;; the next numt was bigger, so add here
          (setcdr ptr (cons num (cdr ptr)))
          (setq loop nil))
        (setq ptr (cdr ptr)))           ;move list forward
      )
    l
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-list-match-re (re list &optional no)
  "Returns members from list that match the RE, or not RE is NO is given."
  (let ((ptr list)
        l el add
        )
    (while ptr
      (setq add nil  el (car ptr))
      (if no
          (if (not (string-match re el)) (setq add t))
        (if (string-match re el) (setq add t)))
      (if add (setq l (append l (list el) )))
      (setq ptr (cdr ptr))
      )
    l
    ))



;;}}}

;;{{{ code: normall filling

;;; ############################################################ &fill ###
;;; Functions for filling


;;; ----------------------------------------------------------------------
;;;
(defun tir-fill-reg (beg end)
  "Fills paragraphs in region. See tir-fill-para."
  (interactive "r")
  (let (tt)
    (if (> beg end)                 ;keep B > E
        (setq tt beg beg end end tt))
    (goto-char beg)
    (while (< (point) end)
      (tir-fill-para)
      (forward-paragraph))
    (goto-char beg)
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tir-fill-para (&optional para-sep)
  "Fills paragraph including FIRST line. If fill-column is under 5, the
char pos 70 will be used. Optional PARA-SEP can be given.

The region will be untabified also, so that there is no tabs between filled
words. Point will be at the beginning of paragraph.
"
  (interactive)
  (let* (from
          (copy-sep paragraph-separate)
          (copy-fc  fill-column)
          ;;   Just don't trust the default.... use mine always

          (fill-column (if (< fill-column 5) 70 fill-column))
          (fill-prefix (if fill-prefix fill-prefix ""))
          (re-w   "[ \t]*[^ \t]")               ;whitespace
          to markb marke
          del-blank-flag
          )
      (setq paragraph-separate (or para-sep "^[ \t]*$"))
      (if (looking-at "^[ \t]*$") nil   ;skip this
        (if (not(bobp))
            (backward-paragraph)))      ;we were inside paragraph, go to BEG

      ;;   - The backward paragraph doesn't put us the BEG of para!
      (beginning-of-line)
      (if (looking-at paragraph-separate)
          (forward-line 1)
        (goto-char (point-min)))        ;didn't put us para BEG
;;;      (d! "para #beg" (point))

      ;;   - Now handle the first line separately.
      ;;   - first kill whitespace, then add prefix.
      (if (null (looking-at (concat "^" re-w))) ; sticked to the LEFT margin
          nil
;;;     (d! "kill" (point)  (match-end 0))
        (kill-region (1- (match-end 0)) ;the RE last char you see
                     (progn (beginning-of-line) (point))))
      (beginning-of-line) (setq from (point))
      ;;  We must use marker, because marker moves along with text -->
      ;;  adjusts position when lines are moved, added. It retains
      ;;  _contextual_ position in respect of the text.
      (setq markb (point-marker))       ;set marker BEG para
      (save-excursion                   ;what's on next line ?
        (forward-line 1)                ;is there para ?
         (if (or (eobp)
                 (not (looking-at "[ \t]*$")))
             (setq del-blank-flag t)))

      (insert fill-prefix)

      (if (not(eobp)) (if (tir-move-to-para) (backward-char 1)))
;;;(d! "--fill end" (point))
      (if (looking-at paragraph-separate)
          nil                           ;there was another PARA
        (goto-char (point-max)))        ;didn't put us para END
      (setq marke (point-marker))       ;set marker END para


      ;;  Fix tabs in fill-prefix so that no: "here TAB is the memo.."
      (untabify (marker-position markb)
                (marker-position marke))

      ;;  There is one peculiarities in this function.
      ;;  - It will remove all empty lines from the BEG and END of
      ;;    paragraph region.
      ;;  - It will add NEW line at the end if there isn't any.

;;;      (d! "--filling--"        (marker-position markb) (marker-position marke))

      (fill-region-as-paragraph
       (marker-position markb)
       (marker-position marke) nil)
;;;      (d! "-# fill ok")

      ;;  if user was in the middle of the text, not on empty line,
      ;;  then there shoved up a NEWLINE, which we have to remove
      (goto-char (marker-position marke))
      (if (null del-blank-flag) nil             ;kill that empty line
        (forward-line 1) (delete-backward-char 1))

      (goto-char (marker-position markb))
      (setq paragraph-separate copy-sep) ; restore values changed
      (setq fill-column copy-fc)
      ))

;;}}}
;;{{{ code: adaptive filling

;;; ######################################################## &adaptive ###
;;; Functions for special filling

;;; ----------------------------------------------------------------------
(defun tir-adap-get-ind ()
  "Returns indentation string for adaptive filling"
  ;;  - I just put this behind function, so that It would be easy to
  ;;    modify/ disable later on.
  ;;  - Maybe the srting could be contructed dynamically in the future ?
  tir-fill-adap-v-ind
  )


;;; ----------------------------------------------------------------------
(defun tir-adap-no (beg end)
  "Checks if region contains something which prevents filling: like prg code.

Ret= t if region isn't suitable for filling."
  (let* ((ob (point))
         (ptr tir-fill-adap-v-no)
         re
         ret
         )
    (goto-char beg)
    (while (and (null ret) ptr)
      (setq re (eval (car ptr)))        ;get regexp
      (setq ret (re-search-forward re end t))
      (if (null ret) nil
        (insert "$")
        (d! "NO-REG:" (point) ret end re) (delete-backward-char 1)
        (tir-msg "NO-REG:" (point) ret end re)
        )
      (setq ptr (cdr ptr))              ;advance list
      )
    (goto-char ob) ;; Restore position, is this faster than save-excursion?
    ret
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-adap-move ()
  "Moves to next adaptive PARA. RET=nil if not moved."
  (interactive)
  (let* (
         (op (point))
         ;;  there may be any amount of CITs at font + SPECIAL marker
         (re  (concat  tir-fup-re tir-re2 "*"
                       tir-fill-adap-v-s-reg "[ \t]+[a-zA-Z]"))
         (ret t)
         sp np                          ;special, normal point
         )
    ;;  Check if there exist special PARAs ...
    (end-of-line)
    (save-excursion
      (if (re-search-forward re nil t)
          (setq sp (progn (beginning-of-line) (point)))))

    (save-excursion
      (if (tir-move-to-para)
          (setq np (point))))

    ;;  Move to special para, if found
    (cond
     ((and np (eq sp nil))              ;NP exist
      (goto-char np))
     ((and sp (eq np nil))              ;SP EXIST
      (goto-char sp))
     ((and sp np)                       ;SP first
      (if (< sp np) (goto-char sp) (goto-char np)))
     (t                                 ;nowhere to move
      (goto-char op)                    ;see EOL at the beg of this file
      (setq ret nil)))

;;;     (insert "$") (d! "adap-move"  (point)) (delete-backward-char 1)

     ret
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fill-adap-get-end ()
  "Returns region end that has same citation marker than current
line.
>>>reg1               <-- point here before calling
>>>reg1               <-- end
>>reg2

Sets global tir-adap-v-s-reg to t, if region is special
"
  ;; (interactive)
  (let ((re1 tir-re1)                   ; valid citation chars re
        (max tir-del-re-look-until)     ; how many chars to look maximum
        (len (length fill-prefix))
        (loop t)
        pfx-len
        brk
        space
        ;;  in case the loop didn't get round even one time
        (p (save-excursion (end-of-line) (point)))
        )
    (save-excursion


      (while (and loop
                  (not (eobp))) ;until different line found
        (beginning-of-line)             ;see end-of-line at the while last
        (setq brk (tir-last-re re1 max)) ;best possible citation
        (if (eq brk  -1)
            (progn
              (setq space (tir-get-wspc))
              (setq pfx-len (length space))  )

          (setq space (tir-get-wspc brk))
          (setq pfx-len (+ (- brk (point))  (length space))  )

          ;;   Check if this really is citation
          (if (null (tir-is-cit (point) brk))
              (setq pfx-len len))       ;consider as same, because false CIT
          )
;;;d    (d! "#GET: len pfx s"
;;;d        len pfx-len (length space)
;;;d        "p b" (point) brk
;;;d        fill-prefix
;;;d        (tir-read-line 20)
;;;d        )
        ;; ......... Now move forward to next line
        (if (not (eq len pfx-len)) ;must be same prefix
            (setq loop nil)
          (end-of-line)           (setq p (point))
          (forward-line 1)              ;automatically goes BEG of line
          (setq loop (and (looking-at fill-prefix)
                          (null (tir-is-empty-line)
                          )))

          ;; Some special case terminates region, while it does have
          ;; equal fill-prefix and indentation [spaces]
          (if (null (tir-adap-is-sreg)) nil
            (setq loop nil))
          (end-of-line)                 ;test eob
          ))

      (goto-char p) (insert "$") (d! "AREA" (point)) (delete-backward-char 1)
      p
    )))


;;; ----------------------------------------------------------------------
;;;
(defun tir-adap-is-sreg ()
  "Checks if the line indicates special region.
mike> My objectives are:
mike>    -    save the world..
mike>         This means that we must act..
mike>    -    look for peace

Above the '-' starts and ends special regions, because the fill-prefix
must be set according to NEXT line 'This means..'.  It indicates that
we must use another function than usual for filling such region.

Returns nil or ready fill-prefix to use for line 2nd special line.
"
  (let* ((look tir-del-re-look-until)   ; how many chars to look maximum
         (re  (concat  tir-fill-adap-v-s-reg "[ \t]"))
         (re1 tir-re1)                  ; valid citation chars re
         brk max bp cp p pfx
         (spc (make-string 80 ?\ ))
         )
    (save-excursion
      (setq max (progn (end-of-line) (point)))
      (setq bp (progn (beginning-of-line) (point)))
      (setq brk (tir-last-re re1 look))

      ;;  jump over CIT marks if there is one
      (if (not(eq brk -1))  (goto-char brk))
      (setq cp (point))                 ;CIT end
      (skip-chars-forward " \t")        ;skip spaces after it
      (setq p (point))

      (if (null (looking-at re))
          nil
        (setq pfx                               ;LEFTTMOST fill-prefix
            (if (eq p cp)
                ""
              (buffer-substring bp p)))
        (concat
         pfx
         (substring spc 0
                    (- (match-end 0) (match-beginning 0)))))
      )))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fill-adap-reg (beg end &optional arg)
  "_Adaptively_ fills paragraphs in region. See tir-fill-para.

Tries to determine paragraphs based on citation differencies, also
grabs automatically right citation for fill prefix.  Always considers
whole lines as region, so cursor position does not matter in setting
the area.

Ignores current fill-prefix. Does not save the point position, leaves the
cursor after last statement moved.

The adaptive paragraphs is determined as following example:
-  Basic rule is that, if indent or citation stays the same, the lines
   belong to the same adaptive PARA.
-  Special adaptive region is those like '-' whgich needs special
   first line handling.
-  NOTE: if only one _single_ line falls into adaptive region, it
   might not get filled. Just look at the code of this .el why...

oooooo oooooo oooooo oooooo             1
oooooo oooooo oooooo oooooo             1
   oooooo oooooo oooooo oooooo          2
   oooooo oooooo oooooo                 2
 oooooo oooooo oooooo                   3
> oooo>>oooooo oooooo oooooo            4
> oooo>> oooooo oooooo oooooo           5, note indent change !
> oooo>> oooooo oooooo oooooo           5
> oooo>>   - oooooo oooooo oooooo       6, special fill
> oooo>>     oooooo oooooo oooooo       6
> oooo>>   - oooooo oooooo oooooo       7, new special fiil chapter
> oooo>>>oooooo oooooo oooooo           8


NOTE:
-  This function tries to do the best it can, but don't be disappointed if
   it fails :-/ The text just is too complex for it.
-  Common case is that CODE is sticked next to TEXT without separating
   newline. Now because the _whole_ adaptive region, text + code, is found
   to contain some code, the whole region was rejected, thus not filled.
   if the optional ARG is supplied.
-  It is also **real slow** for long PARAs.

Arguments
   beg           , region beginning
   end           , region end
   arg           , if non-nil, means special region handling,
                   checks if filling of region is permitted. Normally
                   no checking is done.

References
   Check all varables starting with tir-fill-adap-v-...
"
  (interactive "r\nP")
  (let* (tt
        (copy-fp fill-prefix)           ; save original
        (look tir-del-re-look-until)    ; how many chars to look maximum
        (re1 tir-re1)                   ; valid citation chars re
        (re-no tir-fill-adap-v-no)      ; Do not fill this region
        (ind  (tir-adap-get-ind))
        (dum (make-string 80 ?x))       ; see SPECIAL-region filling
        (loop t)
        brk op bp
        me                              ;marker end == end pos
        rme                             ;region marker end
        fill-beg
        fill-end
        (space "")
        fp
        nel                             ;narrow end of line
        pfx1
        len
        bp                              ;narrowed region point-min
        no-fill                         ;t if no permission to fill
        line1                           ;if only one line in adap reg
        is-citated
        (CHECK arg)                     ;copy the value
        )
    (if (> beg end)                 ;keep E > B
        (setq tt beg beg end end tt))

    ;;  Try to find paragraph beginning if looking at whitespace
    (goto-char beg)                     ;setting the POINT up--down..
    (if (tir-is-empty-line)
        (if (tir-adap-move)             ;if there is PARA
            (setq beg (point))
          (setq beq nil end nil))
      (beginning-of-line)               ;okay, user within text
      (setq beg (point)))


    (if (or (eobp) (eq beg end))
        nil                             ;nothing to do
      ;;  We must use marker, because marker moves along with text -->
      ;;  adjusts position when lines are moved, added. It retains
      ;;  _contextual_ position in respect of the text.
      (goto-char end)
      (setq me (point-marker))          ;set marker to the end
      (goto-char beg)                   ;start here

;;;      (d! "#markers " me beg end "Point" (point) loop)
      (setq rme (point-marker))         ;just create it
      (goto-char beg)                   ;start here !!!

      ;; ....................................... WHILE ..............
      (while (and loop
                  (null (eobp))
                  (< (point) (marker-position me))
               )
        (setq bp (point))
        (setq fill-prefix "" fp "")                  ; *** RAW F-P ***

        ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
        ;;  - We must keep fill-prefix ORIGIBAL so that tir-fill-adap-get-end
        ;;    can find right area for us.
        ;;  - The fixed fill prefix will be held in fp, that we'll use
        (setq special-reg  (tir-adap-is-sreg))
        (if special-reg                              ; *** SPECIAL F-P ***
            (progn
              (forward-line 1)          ;skip over this line
              (setq fill-prefix special-reg fp special-reg)
              (d! "#set spe fpx" fill-prefix (length fill-prefix))
              )
          ;;   find best possible break position and  fill prefix
          (setq brk (tir-last-re re1 look))
          (if (eq brk  -1)
              (progn
                (setq space (tir-get-wspc))
                (setq fill-prefix space))
            ;; .......................................... citation found
            (setq fill-prefix (tir-read-line (- brk bp)))
            (setq space (tir-get-wspc brk))
            (setq fill-prefix (concat fill-prefix space)) ;space too !

            (setq fp fill-prefix)       ; fp == plain fill prefix
            ;;    check for sticked CITATION and correct --> fp
            (if (eq 0 (length space))    ; *** NOW THE REAL F-P TO USE ***
                (setq fp (concat fill-prefix ind)))

;;;         (d! "#space" space ind fill-prefix)
            ))

        ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
        ;;   Now define the region to fill
        (setq fill-beg bp)
        (setq fill-end (tir-fill-adap-get-end))

        ;;   Do we have perm. to fill this region
        ;;   - if ARG was given it means check filling, otw no checks.
        (setq no-fill
              (if CHECK
                  (tir-adap-no fill-beg  fill-end)
                nil))

;;;     (d! "#pFILL ok beg brk" no-fill bp brk "end" fill-end fill-prefix)
        (set-marker rme fill-end)


        ;; Only has 1 line and it's too short, skip over
        (setq line1
              (and (eq 1 (count-lines  fill-beg  fill-end))
                   (< (-  fill-end  fill-beg) fill-column)
                   ))

        ;;  Try to find a LONG line, if not found, then no need to
        ;;  fill at all.
        (if (null (tir-is-line-len  fill-beg fill-end fill-column))
            (setq no-fill t))

        ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
        ;;  Now do the filling
        (if (or no-fill line1)
            (progn
              nil
;;;           (d! "over" no-fill line1)
              )
          (save-restriction
            (narrow-to-region fill-beg fill-end)
            (goto-char (point-min))
            (replace-regexp (concat "^" fill-prefix) "")

;;;         (d! "replace ok" (point) bp "S-reg"  special-reg
;;;             fp space fill-column)
            (setq fill-prefix fp)       ; because of tir-fill-adap-get-end
            (if (null special-reg)              ;nothing special in it
                (progn
;;;               (d! "normal para fill ready?")
                  (tir-fill-para)
;;;               (d! "normal para fill ok...")
                  )
              ;;   The normal f-rap [fill-region-as...]
              ;;   can't handle 1st line different filling, so
              ;;   we trick it a little. We replace first pfx with
              ;;   dummy word and back, then it'll fill it ok.
              (setq bp (point-min)) (goto-char bp)
              (setq len (length fill-prefix))
              (setq pfx1 (tir-read-line len)) ; save 1st prefix
;;;           (d! "pfx" pfx1 bp (point-min))
              (delete-region bp (+ len bp)) ; delete it
              (insert (substring dum 0 len))  ; add dummy word instead
;;;           (d! "special fill" bp (point-min) fp (length fp))
              (fill-region-as-paragraph bp (point-max))
;;;           (d! "filled ok")
              (goto-char beg) (delete-region bp (+ len bp))
              (insert pfx1)             ;restore first prefix
              (goto-char (point-max))   ;get rid of added NEWLINE by f-rap
              (delete-backward-char 1)
;;;           (d! "special")
              )
            ))

        ;;  Go to next statement or PARA
        (if line1
             (forward-line 1)
          ;; (setq loop (tir-adap-move))
          (goto-char (marker-position rme)) ;next PARA
          (forward-line 1)
          )

;;;     (insert "$") (d! "#PARA"  (point)) (delete-backward-char 1)
        );; ........................... WHILE ...............................

      (setq fill-prefix copy-fp)
      (goto-char (marker-position me))
      ) ;; ----------- if PARA found
    ))

;;}}}


;;{{{ code: deleting/adding
;;; ###################################################### &add / &del ###
;;; Functions related to followup ADD / DELETE
;;; - Adding to the beginning of line
;;; - Deleting followup REXPS.


;;; ----------------------------------------------------------------------
;;;
(defun tir-add-str (str re)
  "Adds string in front of lines. Eg. comments out csh-lisp sections.
You could acoomplish *almost* the same with
   C-x f  (set fill-column)
   C-x .  (set fill-prefix)
   M-x fill-region
But this is interactive and allows you more controll over insertion.
1) Asks string to add
2) Asks pattern to match at the beginning of line where to put the strings.
   If you just hit RET, adds to every line.

If you supply numeric argument, the string will be added to the end of line.
"
  (interactive "sAdd string:\nsLook for rex:")
  (catch 'out
    (if (eq 0 (length re)) (setq re nil))
    (if (eq 0 (length str)) (throw 'out t)) ; nothing to add

    (save-excursion
      (let* (
             (yn t) (parg current-prefix-arg)
             put ask
             )
        (overwrite-mode 0)              ; make sure we ADD
        (while (and yn (not(eobp)))
          (setq ask t)                  ; set default
          (if (not (eq nil re))         ; do we look?
              (setq ask (looking-at re)))

          (if (eq nil ask)
              t                         ; skip the line
            (setq put (tir-yn "" "abort/[y]/n"))
            (if (equal 1 put) (setq yn nil)) ; abort requested
            (beginning-of-line)
            (if parg (end-of-line))
            (if (and yn put)  (insert str)))
          (forward-line 1)
          ) ; while
        )) ; let, save-ex
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tir-add-str-reg (beg end str re-look)
  "Adds string to region. You might use this as intend-region by
adding more spaces to any vertical position, but most likely
this is best function for commenting yout arbitrary blocks of code.

1) set mark to _exact_column_ where to add string
2) move cursor to destination line, column does not matter.

If you want to add string to specific lines only, supply
rex when you are asked for 'look for rex'. Remember that this
rex will be used from that mark column to the end of line, so whole line
is not looked. Here is one example:

      *mark here
    ;;; triple comment
    ; single comment

    ;;; another triplet
    *cursor here

Situation after you have supplied 'String to region: #' and 'look for rex: ;'

    ;;#; triple comment
    ; single comment

    ;;#; another triplet
      ^^^^^^^^^^^^^^^^^^^^ --> the REX match area, note not incl. leading!

Note that the single ';' isn't matched, because the mark's column position
is further away.
"

  (interactive "r\nsString to region: \nsLook for rex: ")
  (save-excursion
    (let* (
            (l 0) (i 0)
            pbl                         ; point beg. of line
            pel                         ; end..
            pc                          ; point char in line
            px                          ; point horizontal
            insF                        ; insert flag
            )
      ;; (overwrite-mode 0)             ; make sure we ADD
      (if (eq nil (length re-look)) (setq re-look nil))

      (catch 'cancel
        (if (= (length str) 0) (throw 'cancel t)); no string given
        (if (> beg end)         ; keep beg < end
            (progn (setq p beg) (setq beg end) (setq end p)))
        (setq l (count-lines beg end))
        (goto-char beg) (beginning-of-line)
        (setq pbl (point))
        (setq pc (- beg pbl))           ;where was the point? 0..80 pos in line

        (while (< i l)
          (end-of-line)         (setq pel (point))
          (beginning-of-line)   (setq pbl (point))  (setq px (+ pbl pc))

          (setq insF t)
          (goto-char px)                ; try keeping  vertical position
          (if (not (eq nil re-look))    ; look this line!
              (if (> (point) pel)
                  (setq insF nil)       ; we can't look, line too short
                (setq insF (looking-at re-look))))  ; we are still on line

          (if (and (< (point) pel)      ; stepped to another line ?
                   insF)                ; right line ?
              t                         ; point in the same line
            (goto-char pel)             ; add spaces at the end
            (while (< (point) px)
              (insert " ")))
          (if insF (insert str))
          (forward-line 1) (beginning-of-line)
          (setq i (+ i 1))))
      )))


;;; ----------------------------------------------------------------------
;;;
(defun tir-adjust-last-re (max)
  "The maximum column position is requested to max, but we may not be satisfied
to that. Lets see next example.
[case 1]
:> > my http://blup@brp.glup
              ^allowed max RE here, would delete http, because : belongs
               to vallid citation marks.
        ^Corrected max pos should be here.
[case 2]
:      #!/bin/perl
        ^RE found here, so the CIT is supposed to be up till here. Wrong...

To prevent this we search special words in a line and set more restrictive
max value which may not be exceeded.
"
  (let* ((list tir-del-re-v-look-until) ; copy global, list of special words
         re
         mp                             ;match point
         (i 0)
         line
         )
    (setq line (tir-read-line))
    (while (setq re (nth i list))
      (setq mp (string-match re line))
      (if (eq mp nil)
          t                             ;do nothing
        (if (> max mp ) (setq max mp))) ;make more restrictive
      (inc i))
    max))

;;; ----------------------------------------------------------------------
;;;
(defun tir-last-re (re &optional max)
  "Searches last re in line, up till optional MAX col in current line.
RET: -1 if not found, otw point"
  ;; (interactive)
  (let* ( (ret -1)
          b ep mp
          (case-fold-search t)          ; ignore case
          )
    (save-excursion
      (save-excursion   (end-of-line) (setq ep (point))) ;end point
      (beginning-of-line) (setq b (point))

      (if max           ; max must not exceed the second line, use MAX-POINT
          (if (> (+ b max) ep)
              (setq mp ep)
            (setq mp (+ b max)))
        (setq mp ep))
      (setq mp (+ b (tir-adjust-last-re max))) ; search special words

      (while (re-search-forward re mp t))
      (if (not (eq (point) b))
          (setq ret (point))) ;if RE-SEARCH moved

      ;; (d! "max" (point) max mp ret)
      ret
      )))

;;; ----------------------------------------------------------------------
;;;
(defun tir-del-re-reg (beg end  &optional parg)
  "Deletes citatation re's at the beg. of line. See tir-re1 and re2.
If called by prefix argument bypasses confirmation for each line"
  (interactive "r\nP")
  (save-excursion
    (let* (p
;;;        (parg current-prefix-arg)    ; argument given
           (re1 tir-re1)
           (l 0)                        ; lines in region
           (i 0)                        ; line counter
           line                         ; whole
           line2                        ; part of it
           copy                         ; line2 copy
           pa1                          ; part 1 & 2 around last citation
           pa2
           (look tir-del-re-look-until) ; how many chars to look
           pc                           ; point citation end
           brk                          ; point to break citations
           brkp                         ; '' in string
           plb                          ; point line beg
           yn-v
           )
      (overwrite-mode 0)                ; keep insert mode on
      (if (> beg end)                   ; kepp A > B
          (progn (setq p beg) (setq beg end) (setq end p)))
      (setq l (count-lines beg end))
      (goto-char beg) (beginning-of-line)

      (while (< i l)
        (setq plb (progn (beginning-of-line) (point)))
        (setq line (tir-read-line))
        ;;  If we beak a line, it's easier to match citation beginning
        ;;  counting from right
        ;;     "> john> xtxtx  '    see>>  "
        ;;  that ' is a cut point, reduces mistakes
        ;;

        (if (> (length line) look)
            (setq line2 (substring line 0 look))
          (setq line2 line))

        (setq brk (tir-last-re re1 look))
        (setq brkp (- brk plb))         ; string berak pos

        (catch 'lineok
          (if (< brkp  0)
              t                         ; rexp not found
            (if (tir-is-empty-line)
                (progn (kill-line) (throw 'lineok t)))

            (if (= (length line) brkp)
                t                       ; must not be at the end
              (setq pa1 (substring line  0 brkp))
              (setq pa2 (substring line brkp (length line)))
              (setq copy (concat pa1 "^^^^delete"))
              (setq yn-v t)             ; set default

              (if (eq nil parg)         ; arg not given, so ask confirmation
                  (setq yn-v (tir-yn copy)))
              (if (eq nil yn-v)
                  t                     ; skip line
                (beginning-of-line) (kill-line)
                (insert pa2))
              )))                       ;catch
        (forward-line 1)
        (setq i (+ i 1)))               ;while
      )))

;;}}}
;;{{{ code: information
;;; ##################################################### &information ###
;;; Functions related to examining buffer contents
;;; - We might be interested in certain header fields in GNUS post etc..
;;;
;;;

;;; ----------------------------------------------------------------------
;;;
(defun tir-study-buf-get-info (pos)
  "Returns POS information from the list that holds study results.
The POS arg is counted from zero, and it has value in respect to
tir-study-v-re's position.

Return
   str  ,whole line that matched re
   nil  ,if there was no such info.
"
  (if (> pos (length tir-study-v-info))
      (error "//tir-study-buf-get-info : overflow of pos")
    (nth pos tir-study-v-info))
  )


;;; ----------------------------------------------------------------------
;;;
(defun tir-study-buf-info (&optional header-too store-list re-list)
  "Studies buffer for certain information and it onto list.
The headers aren't normally included for scanning, but optional
argument HEADER-TOO will set the starting point to point-min.
If regexp is not matched it will have value nil put into study list.

You can also give STORE-LIST as 'symbol where the results are put
instead of the default.

When user supplied regexp is encountered, whole line will be read.
Calling this function will immediately destroy old contents of study info.

Returns
   nil,t      if study took place

References
   tir-study-v-re               ,user defined study regexps
   tir-study-buf-get-info       ,function to get studied values
"
  (let* (ptr
         re l
         (ok t) item
         )
    ;; build up pointer dereferences :-/
    (if (null store-list) (setq store-list 'tir-study-v-info))
    (if (null re-list) (setq re-list 'tir-study-v-re))

    (set store-list nil))               ;clear list
    (setq ptr (eval re-list))

    (save-excursion
      (if header-too
          (goto-char (point-min))
        (setq ok (tir-go-txt)))

      (if (null ok) nil                 ;couldn't find the start spot
        (while ptr
          (setq re (car ptr))
          (setq item nil)
          (if (or (null re) (eq (length re) 0)) ;nothing to do
            nil
            (save-excursion
              (if (null (re-search-forward re nil t))
                  nil   ;not found
                (setq item (tir-read-line))))
            (set  store-list (append (eval store-list) (list item)))
            )
        (setq ptr (cdr ptr))))          ;advance while pointer
      ok
      ))


;;}}}

;;{{{ code: CC, fields and header setups

;;; ########################################################### &field ###
;;; Functions related to header handling
;;; - Info field adding
;;; - Subject field's reply handling
;;;


;;; ----------------------------------------------------------------------
;;;
(defun tir-kill-field (field-re)
  "Deletes mail header field"
  (let (beg)
    (save-excursion
      (goto-char (point-min))
      (if (null (re-search-forward field-re nil t)) nil
        (goto-char (match-beginning 0))
        (setq beg (point))
        (forward-char 1)
        (re-search-forward "^[^ \t]" nil t)
        (backward-char 1)
        (delete-region beg (point)))
      )))


;;; ----------------------------------------------------------------------
;;;
(defun tir-news-copy-check (str)
  "Checks if the we are permitted to send copy to sender.
Returns t if sending copy is ok.

References: tir-v-no-sender-copy , user-login-name
"
  (let* (
         (user (user-login-name))       ;current sender
         (c tir-v-re-no-sender-copy)    ;regexp when no copy
         (re (if (or (null c)           ;not exist
                     (= 0 (length c)))  ;it's empty
                 "@!SDF%&CXZV"          ;something that will not match..
               c))                      ;accept user defined re
         )
    (if (or (string-match user str)
            (string-match re str))
        nil
      t)))

;;; ----------------------------------------------------------------------
;;;
(defun tir-news-copy-to-sender ()
  "Adds CC: field for sender when replying to his post within GNUS.

If the sender's name is same as the poster's name, it that case the current
user is replying to his own message, in that case the message isn't copied:
the CC isn't added.
"
  (interactive)
  (let* (
         sender                         ;who's speaking
         mail                           ;his addr.
         )
    (catch 'cancel
      (goto-char (point-min))
      (if (null (re-search-forward "^CC" nil t)) nil
        (tir-msg t "already CC'ed") (throw 'cancel t))

      (setq sender  (tir-mail-get-field "From" t))

      (if (tir-news-copy-check sender) nil
        (message "CC field not added.") (sleep-for 1) (throw 'cancel t))


      (if (not (equal "" sender)) nil
        (tir-msg t "Can't send copy, no 'From' field?") (throw 'cancel t))

      (setq mail (tir-fup-parse-from-email-full sender))
      (goto-char (point-min))

      (if (null (re-search-forward "^FCC" nil t)) nil ;add after this if found
        (beginning-of-line) (forward-line 1)
        (insert (concat "CC: " mail "\n"))
        (throw 'cancel t))

      ;;  no FCC? , ok put to below subject  then.
      (if (null (re-search-forward "^Subject" nil t))
          (progn
            ((tir-msg t "No subject? can't put CC copy...") (throw 'cancel t)))
        (beginning-of-line) (forward-line 1)
        (insert (concat "CC: " mail "\n"))
        (beginning-of-line))

      ;; ------------- catch
      )))


;;; ############################################################# &CC: ###
;;; CC -field handling

;;; ----------------------------------------------------------------------
;;;
(defun tir-cc-field-el-kill (re)
  "Kills all senders that matches re on CC: line."
  (interactive "sKill cc re:")
  (let* ((ob (current-buffer))
         bp
         cc
         ret
         cc-li el (i 0)
            )
    (save-excursion                     ; sendmail.el func
      (narrow-to-region                 ; limit scope to headers
       (point-min)
       (progn
         (goto-char 1)
         (search-forward "--text")
         (forward-line 1)
         (beginning-of-line) (point)))
      (if (null (mail-position-on-field "cc" t))
          nil                           ; must exist both
        (setq cc (mail-fetch-field "cc" t))
        (set-buffer (setq bp (tir-cnv-s2l cc)))
        (goto-char  1)

        (switch-to-buffer bp)
        (if (null (re-search-forward re nil t)) nil ;no such rexp
          (save-excursion               ; kill original CC in Mail
            (set-buffer ob)
            (tir-nuke-mail-cc-field 1))

          (goto-char 1)                 ; Now adjust new CCs
          (setq cc-li (tir-del-list-re (tir-get-words) re))
          (if (null cc-li) nil          ; empty
            (erase-buffer)
            (while (setq el (nth i cc-li))
              (insert (concat "\t" el ",\n"))
              (inc i))
            (set-buffer ob) (insert "CC: ")
            (insert-buffer bp))
          )
        )
      ;; ------------- save-ex
      (widen)
      )))



;;; ----------------------------------------------------------------------
;;;
(defun tir-cc-field-kill-to ()
  "Kills From: sender from CC: line, so that he doesn't get two copies.
People somethimes Send mail like:

    From: USER@site    --> when replied
    To: Me@here            To: USER@site
    CC: USER@site          CC: --killed--

because they want to keep copy when they send mail. However, if I
want to reply to that message It also includes that CC field :-/
So get rid of those cases.
"
  (interactive)
  (let* ((to (mail-fetch-field "to" t))
         bp
         )
    (if (null to) nil                   ;skip, no sender set
      (tir-cc-field-el-kill to)
      (if (null (setq bp (get-buffer tir-tmp-buf))) nil
        ;; this is needed, because when user sends message with C-cC-c
        ;; the *temp* buffer created pops up! That should be RMAIL...
        (kill-buffer bp))
      )
    ))



;;; ----------------------------------------------------------------------
;;; - I saw this func posted somewhere in 1994, thanks John!
;;; - Modified and the idea used here.
;;;
;;; From: jtk@atria.com (John T. Kohl)
;;; "I use this function to keep me from Cc'ing the world:"
;;;
(defun tir-nuke-mail-cc-field (&optional kill)
  "Ask if CC: line be kept, if not, nuke it."
  (interactive "P")
  (let* ((re "^CC:"))
    (save-excursion
      (goto-char (point-min))
      (if (null (re-search-forward re nil t)) nil
        (setq kill (or kill
                       (tir-yn "Nuke cc: line ")))
        (if (null kill) nil
          (tir-kill-field re)
          )))
    ))


;;; ############################################################ &subj ###
;;; Subj-field handling



;;; ----------------------------------------------------------------------
;;;
(defun tir-mail-subj-count-re ()
  "Supposes that the point is positioned under the Subject line Somewhere.
Counts how many Re:s are there in line. Returns number 0..xx
Uses global return variable RET-1 which holds subject line.

BUG NOTE:
When you receive message like this and hit 'r' to reply it
  Re: the appointment date

emacs will automatically remove that 'Re:' from the subject line
and it will write following into your mail message header
  Subject: the appointment date

Unfortunately this function scans the subject line *after* emacs
have included that stripped subject line, so it cannot notice if
there was 'Re:' at all. Because this function is called by
tir-mail-format-subject, it returns 0 to it, resulting
    Subject: Re! the appointment date

As if it were the first reply to message. To increase the Re(n): number
you just have to call tir-mail-format-subject again.

"
  ;; (interactive)

  (let (nline
        (re-mark tir-mail-subj-re)      ; copy global
        (count 0)
        (re-count 0)
        (add-up 0)
        line
        re
        b                               ; line beginning
        psubj                           ; subject start without res
        subj
        )
    (save-excursion

      (beginning-of-line)
      (setq b (point))
      (forward-line)
      (setq nline (point))              ; position of next line is this
      (forward-line -1)                 ; go to orig. pos.
      (setq RET-1 (tir-mail-get-field "subject")); set default value

      (setq case-fold-search t)         ; ignore case
      (catch 'out
        (catch 'endw
          (while (re-search-forward re-mark  nil t)
            (if (> (point) nline) (throw 'endw t))
            (setq psubj (point))
            (setq count (+ count 1))))

        (if (= 0 count)  (throw 'out t))  ; no match at all

        (goto-char psubj)
        (setq subj                      ; read line from buffer
              (buffer-substring (point) ; Subject beginning
                                (progn (end-of-line) (point))))

        (goto-char psubj) (beginning-of-line)           ; whole line
        (setq line                      ; read line from buffer
              (buffer-substring (point) ; Subject beginning
                                (progn (end-of-line) (point))))

        (if (string-match "re\\([0-9]+\\):" line)  ; See if there is Re2:
            (progn
              (setq re (substring line (match-beginning 1) (match-end 1)))
              (setq re-count (string-to-int re))))

        (setq RET-1 (tir-sqz subj))     ; delete extra spaces

        ;; count 1 : "re"
        ;; count 2 : "re: re:"
        ;; count 1 : "re2:"      should become Re3:

        (if (> re-count 0) (setq count  (- count 1)))

        (setq add-up (+ count  re-count)))

      ;; ------- catch out -------------------------------------------
      add-up)
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-mail-subj-replace (txt)
  "Replaces subject text. Subject:' 'txt. Adds that ' ' after kill."
  ;; (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (null (re-search-forward "^Subject:" nil t)) nil ;skip...
      (kill-line)
      (insert (concat " " txt)))))


;;; ----------------------------------------------------------------------
;;;
(defun tir-mail-format-subject ()
  "Formats Subject line in mail's reply.
Inserts variable tir-mail-subj-reply-prefix if this is first reply,
Otherwise counts
        \"re: re: text\"
on subject line and replaces them with
        \"re2: text\"

Referencies:
 tir-mail-subj-count-re   ,make sure you read desc of this!
"
  ;; (interactive)
  (let ((count 0)
        (pfx  tir-mail-subj-reply-prefix)
        (re-fwd tir-mail-subj-fwd )     ; rexp for SUBJECT that's forwarded
        str
        )
  (save-excursion
    (catch 'cancel
      (if (equal "" (setq to (tir-sqz (tir-mail-get-field "Subject"))))
          (progn
            ;; (tir-msg t "No Subject field found.")
            (throw 'cancel t)))

      ;;  ****  Don't touch forwarded messages
      (if (string-match re-fwd to)      ;skip, [blaah] -- rmail original
          (throw 'cancel t))

      (goto-char (point-min))
      (re-search-forward "^Subject" nil t)      ; definitely matches
      (setq count (1+ (tir-mail-subj-count-re)))

      (if (eq count 1)                          ; add only Re:
          (tir-mail-subj-replace (concat pfx  RET-1))
        ;; add Re2:, Re3...
        (setq str (concat "Re" (int-to-string count) ": " RET-1))
        (tir-mail-subj-replace str))
      ))))


;;; ############################################################ &info ###
;;; Info-field handling


;;; ----------------------------------------------------------------------
;;;
(defun tir-mail-ins-info ()
  "Insert mgs from tir-mail-my-info-list to appropriate destinations.
The matched rexp is displayed at the status line."
  (interactive)
  (let* (to
         (empty tir-mail-my-info-none)
         (def-info tir-mail-my-info-def)
         (re-li tir-mail-my-info-list)
         (nbr 0)
         (re "")
         (info "")
         (info-n -1)
         el
         )
    (save-excursion
      (catch 'cancel
        (if (equal "" (setq to (tir-mail-get-field "To")))
            (progn
              (tir-msg t "No To field, cannot insert Info:")
              (throw 'cancel t)))
        (if (not (equal "" (tir-mail-get-field "Info")))
            (progn
              (tir-msg t "Info already there.")
              (throw 'cancel t)))
        (goto-char (point-min))

        ;;  determine destination and select info for it.
        (catch 'info-ok
          (while (setq el (nth nbr re-li))
            (setq re (nth 0 el))        ; see structure of list
            (setq info (nth 1 el))

            (if (and (string-match re to)    ; try to find RE match
                     (not (string-match empty info)) )
                (progn
                  (setq info-n nbr)     ; inserted !!
                  (message re)          ; show rexp matched
                  (insert (concat "Info: " info))
                  (throw 'info-ok t)))
            (setq nbr  (+ 1 nbr))))

        ;;  insert default info only if it is written.
        (if (and (= -1 info-n) (not (string-match empty def-info )))
            (progn
              (setq info-n 100);                  ; just some value
              (insert (concat "Info: " def-info))))

        ;;(if  (= -1 info-n) (tir-msg nil "info: All rexp's empty "))

      ))))

;;}}}
;;{{{ code: FCC handling
;;; ############################################################# &Fcc ###
;;; Functions related to FCC  handling
;;; - Determining if default folder must be changed.
;;;
;;;



;;; ----------------------------------------------------------------------
;;;
(defun tir-fcc-set (folder)
  "Replaces FCC if it exists."
  (save-excursion                       ; replace folder
    (goto-char (point-min))
    (if (null (re-search-forward "FCC: " nil t)) nil
      (kill-line)
      (insert folder))))



;;; ----------------------------------------------------------------------
;;;
(defun tir-narr-to-hdrs()
  "Narrows to mail headers if there is tag '--text...'.
RET= nil if not narrowed."
  ;; (interactive)
  (let (p)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^--text" nil t)
          (setq p (progn (beginning-of-line) (point))))
      )
    (if (null p) nil
      (narrow-to-region (point-min) p)
      t)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fcc-determine ()
  "Looks if default folder must be changed. Tries to find RE given in
tir-v-fcc-by-hdrs by looking at header area.

Returns suggested NEW folder or nil."
  (let (ret el re f (i 0) )
    (save-excursion
      (save-restriction                 ;preserve possible previous narrow
        (if (null (tir-narr-to-hdrs))
            (tir-msg t "This is not mail/post message, no '--text' found.")
          (goto-char (point-min))
          (while (and (null ret)                ; stop when this is set
                      (setq el (nth i tir-v-fcc-by-hdrs)))
            (setq re (nth 0 el)) (setq f (nth 1 el))
;;;         (d! "FCC-hdr" re f)
            (if (re-search-forward re nil t) (setq ret f))
            (inc i))
        (widen))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tir-fcc-get-study-folder ()
  "Suggests folder for studied items."
  ;;       right folder setting
  (let* ((re-list tir-v-fcc-by-study)
         (info tir-study-v-info)
         (len (length re-list))
         folder fld (i 0)
         p2 re)
    (while (and (null folder)           ;until this is set
                re-list)
      (setq re (nth 0 (car re-list)))
      (setq fld (nth 1 (car re-list)))

      (if (eq (length re) 0)
          nil                           ;empty re
        (setq p2 info)                  ;scan the study info
        (while (and (null folder)       ;stop when found
                    p2)
          ;; (d! re fld (car p2))
          (if (and (stringp (car p2))   ;must be string
                   (string-match re (car p2)))
              (setq folder fld))
          (setq p2 (cdr p2))))          ;next study data
      (setq re-list (cdr re-list)))             ;next RE
    folder))



;;; ----------------------------------------------------------------------
;;;
(defun tir-fcc-set-by-study ()
  "Sets right FCC folder if a\) default FCC is set b\) and there is match
somewhere in saved lines from studied buffer. See  tir-v-fcc-by-study."
  ;;       right folder setting
  (interactive)
  (let (folder)
    (if (setq folder (tir-fcc-get-study-folder))
        (tir-fcc-set folder))))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fcc-set-by-hdr ()
  "Sets right FCC folder if a\) default FCC is set b\) and there is match in
tir-v-fcc-by-hdrs."
  ;;       right folder setting
  (interactive)
  (let (folder)
    (if (setq folder (tir-fcc-determine))
        (tir-fcc-set folder))))


;;}}}

;;{{{ code: date
;;; ############################################################ &date ###
;;; date handling


;;; ----------------------------------------------------------------------
;;;
(defun tir-s-month-n(str)
  "Returns month number for string. Accepts Jan or January with any case."
  ;; (interactive)
  (let (
        (alist
         '(("jan" . 1)     ("feb" . 2)     ("mar" . 3)     ("apr" . 4)
          ("may" . 5)     ("jun" . 6)     ("jul" . 7)     ("aug" . 8)
          ("sep" . 9)     ("oct" . 10)    ("nov" . 11)    ("dec" . 12)))
        s l e
        )
    (setq l (length str))
    (if (> l 3) (setq str (substring str 0 3)))  ; cut to 3 chars
    (setq s (downcase str))

    (if (null (setq e (assoc s alist)))
        ""                              ;not found? !!, I'm paranoid...
      (cdr e))
))


;;; ----------------------------------------------------------------------
;;;
(defun tir-parse-date (str)
  "Tries to parse date field. Supposes 'date:' -- ie. garbage is removed
from the string before calling. No leading spaces allowed.
RETURN:
  '\(dd mm yy tt wd m yy tz\)   or \"\" on field which weren't identified.
    0 dd     day, number
    1 mm     month, number
    2 yy     year, 2 last numbers
    3 tt     hh:mm
    4 wd     week day, eg. Mon
    5 m      month, string
    6 yy     year, 2 last numbers
    7 tz     time zone +-nnnn
"
  (let* ((wd "")
         (dd "")
         (mm "")
         (m  "")
         (yy "")
         (tt "")
         (tz "")
         p
         pyy py
         (ofs 0) (ofsdd 0)

         (rAaa "[A-Z][a-z][a-z]")
         (rd "[0-9][0-9]?")                    ; typical day nbr
         (rd4 "[0-9][0-9][0-9][0-9]")          ; typical year nbr
         (rt "[0-9:]+")                        ; time
         (rz "\\([+-][0-9]+\\|[A-Z]\\)+")      ; zone

         (re-y4                                ; - year 4
          (concat rd4 " " rt))                 ; 1994 08:52:25
         (re-y2                                ; - year 2
          (concat rd " " rt))                  ; 94 08:52:25

         (re-wd1                               ; - week day format
          (concat "^" rAaa ", " rd " " rAaa))  ; Mon, 24 Oct
         (re-wd1-1 (concat re-wd1 " " re-y4)) ; Mon, 24 Oct 1994 08:52:25 +0200
         (re-wd1-2 (concat re-wd1 " "re-y2))  ; Mon, 24 Oct 94 08:52:25 GMT

         ;;  Wed Oct 14 22:21:05 1987
         (re-wd2
          (concat "^" rAaa " " rAaa " " rd)) ;  Wed Oct 14
         (re-wd2-1                           ;  'current-time-string'
          (concat re-wd2 " " rt " " rd4))    ;  Wed Oct 14 22:21:05 1987

         (re-dd1 (concat "^" rd " " rAaa))   ; 24 Oct
         (re-dd1-1                           ;
          (concat re-dd1 " " re-y4))         ; 24 Oct 1994 00:28:04 GMT
         (re-dd1-2                           ;
          (concat re-dd1 " " re-y2))         ; 24 Oct 94 00:28:04 GMT


         )
    ;;   We get rid of all the extra spaces just leaving one to separate
    ;;   items..
    (setq str (tir-sqz (tir-1-space str)))

    (catch 'out

      ;;   "Tue, 1 Nov 1994 8:52:36 +0300 (EET)"
      (if (null (string-match re-wd1 str)) nil
        (if (null (string-match re-wd1-1 str)) nil ; select YEAR format
          (setq pyy 4) (setq ofs 2)                ; offset
          )                                ; set year pointer
        (if (null (string-match re-wd1-2 str)) nil
          (setq pyy 2))

        (if (null pyy) (throw 'out t))
        (setq wd (substring str 0 3))
        ;;    we remove trailing space  "2 " --> 2 --> "2"
        ;;    it's funny that the day can also be "02" ,which
        ;;    is checked with the 2nd OR case
        (setq dd (int-to-string (string-to-int (substring str 5 7))))

        (if (or (> (string-to-int dd) 9)
                (equal (substring str 5 6) "0"))
            nil
          (setq ofsdd -1)
          (setq ofs (- ofs 1)))

        (setq m (substring str (+ 8 ofsdd) (+ 11 ofsdd) ))

        (setq mm (int-to-string (tir-s-month-n m)))

        (setq p (+ 12 ofs))     ; start of -[YY] format
        (setq yy (substring str p  (+ p 2)))
        (setq tt (substring str (+ p 3)  (+ p 8)))

        (if (> (length str) 25)        ;it's only "Wed, 14 Dec 1994 08:35:51"
            (setq tz (substring str (+ p 12)  nil)))
        (throw 'out t)
        )

      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      (if (null (string-match re-dd1 str)) nil
        (if (null (string-match re-dd1-1 str)) nil ; select YEAR format
          (setq pyy 4) (setq ofs 2)                ; offset
          )                                ; set year pointer
        (if (null (string-match re-dd1-2 str)) nil
          (setq pyy 2))

        (if (null pyy) (throw 'out t))


        (setq dd (int-to-string (string-to-int (substring str 0 2))))
        (if (or (> (string-to-int dd) 9)
                (equal (substring str 0 1) "0"))
            nil
          (setq ofsdd -1)
          (setq ofs (- ofs 1)))

        (setq m (substring str (+ 3 ofsdd) (+ 6 ofsdd) ))
        (setq mm (int-to-string (tir-s-month-n m)))

        (setq p (+ 7 ofs))      ; start of -[YY] format
        (setq yy (substring str p  (+ p 2)))
        (setq tt (substring str (+ p 3)  (+ p 8)))
        (if (or (< (length str) 25)
             (null (string-match (concat rz "$") str)))
            nil ;skip
          (setq tz (substring str (+ p 12)  nil)))
        (throw 'out t)
        )
      )
    ;; ------------------  catch
    (list dd mm yy tt wd m yy tz)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tir-get-site-date (site li)
  "Formats date according to site's convention.
INPUT: site address, date list
RETURN: formatted string
"
  (let* ((europe-sites tir-v-europe-sites-alist)
         (fid "$get-site-date:")        ;function id
         ch2
         )

    (if (null (string-match ".*[.]\\(..\\)" site))
        (tir-msg t fid " No site extension found")
      ;; the country id
      (setq ch2 (substring site (match-beginning 1) (match-end 1)))
      )

    (if (assoc ch2 europe-sites)
        (progn                          ;Tue 25.10.94
          (concat
           (nth 4 li) " "
           (nth 0 li) "." (nth 1 li) "." (nth 2 li)))
      ;; Not European site, so use universal time format
      (concat                           ;Tue Oct 25 -94
       (nth 4 li) " "
       (nth 5 li) " " (nth 0 li) " -" (nth 2 li)))
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-get-date-fmt (date from)
  "Tries to parse Date line.
RETURN: date or \"\"
"
  ;; (interactive)
  (let* ((fid "$tir-get-date-fmt:")
         li
         (ret "")
         site

         )
    (setq li (tir-parse-date date))
    (if (equal "" (nth 0 li))   ;skip, couldn't parse
        (tir-msg t fid " unknown date format")
      (setq site (nth 1  (tir-fup-parse-from-email from )))

      (setq ret (tir-get-site-date site li))
      (setq ret (tir-sqz ret))                  ;rmove LEAD/TRAIL blanks
      )
    ret
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-get-reply-date ()
  "Reads sending date and formats it to national convention.
RETURN: date or \"\"
"
  ;; (interactive)

  (let* ((fld (tir-mail-get-field "date" t))
         ;;  It may not have leading space ": dateVal"
         (date (tir-sqz fld))
         (from (tir-sqz (tir-mail-get-field "from" t)))
         )
    (if (equal "" date)
        nil                             ;no date field...
      (tir-get-date-fmt date from)
      )))




;;}}}
;;{{{ code: fwd

;;; ############################################################# &fwd ###
;;; Functions related to forwarded message
;;; - formatting message layout


;;; ----------------------------------------------------------------------
;;; Will be my definition of subject.
(defun tir-fwd-get-b-pos  ()
  "Returns begin positions of 'forwarded begin' clause. See tir-fwd-v-start-re"
  (let* ((re tir-fwd-v-start-re)
         l)
    (save-excursion
      (if (null (tir-go-txt)) nil
        (while (re-search-forward re nil t)
          (beginning-of-line)
          (setq l (append l (list (point))))
          (end-of-line)))
      l
      )))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fwd-kill-hdr  (&optional bp)
  "Kills from point 'bol' to the next empty line. Header must be
shorter than tir-fwd-v-hdr-max-len for kill to take effect.

Returns: nil t   if killed."
  (let* ((re "^[ \t]*$")
         (max tir-fwd-v-hdr-max-len)
         op p
         (bp (or bp (point)))
         )

    (save-excursion
      (setq p (or bp (point)))
      (goto-char p) (beginning-of-line) (setq p (point))
      (if (or (null (re-search-forward re nil t)) ;no empty line separates
              (> (count-lines bp (point)) max)) ;can't be header
          nil
        (kill-region p (progn (end-of-line) (point)))
        t
        ))))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fwd-get-tag  ()
  "Gets some information from FWD headers, who passed the message.
User must supply the BP begin point of header. Next empty line
will terminate header information. The tir-fwd-v-hdr-max-len determines
if the empty line encountered delimits valid header.

Returns:
  \(email date\)
  \(nil nil\)             ,if nothing found
"
  (let* ((re "^[ \t]*$")
         (max tir-fwd-v-hdr-max-len)
         ep email date date-fmt
         (bp (point))
         )
    (if (or (null (re-search-forward re nil t)) ;no empty line separates
            (> (count-lines bp (point)) max)) ;can't be header
        nil
      (setq ep (point))                 ;end point
      (goto-char bp)
      (save-excursion
        (if (null (re-search-forward "^from:" ep  t)) nil
          (setq email (tir-fup-parse-from-email-full
                      (tir-read-line)))))

      (save-excursion
        (if (null (re-search-forward "^date:" ep  t)) nil
          (setq date (tir-del-re ".*Date: *" (tir-read-line)))))

      (if (and date email)
          (setq date-fmt  (tir-get-date-fmt date email)))
      )
    (list email date-fmt)
    ))

;;; ----------------------------------------------------------------------
(defun tir-fwd-get-info ()
  "Returns suffivent FWD info. Nil if couldn't return info"
  (let* ((data (tir-fwd-get-tag))
         (e (nth 0 data))
         (d (nth 1 data))
         )
    (if d (concat d "  " e) nil)))


;;; ----------------------------------------------------------------------
;;; Will be my definition of subject.
(defun rmail-forward-form-subj (orig-sender subj)
  "Called by forward. Formats suitable Subject line."
  (let* (who p)
    (setq who (tir-fup-parse-from-email orig-sender))
    (if (setq p (string-match "fwd: \\S " subj)) ;already forwarded
        (setq subj (substring subj 0 (1- p))))   ;remove that fwd info
    (concat subj " (fwd: " (nth 0 who)  ")" )
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fwd-format ()
  "TIR fwd'd message format hook function"
  ;; (interactive)
  (let* ((bstr tir-fwd-v-start-str)
         (estr tir-fwd-v-end-str)
         l
         (fwd-num (int-to-string (1+ (length (tir-fwd-get-b-pos)))))
         info
         )
    (save-excursion
      (if (null (tir-go-txt)) nil ;skip

        ;;   Make FWD heder more nicer when it's 2nd 3rd forward.
        (if (and (not (string=  fwd-num "1"))
                 (setq info (tir-fwd-get-info)))
            (tir-fwd-kill-hdr))         ;too many lines for nothing...
        (insert "\n" bstr " " fwd-num "  " (or info "") "\n")

        ;;    make sure no lines are left
        (goto-char (point-max)) (delete-blank-lines) (forward-line -1)
        (if (looking-at estr) nil       ;already forwarded
          (end-of-line)
          (insert "\n" estr))))
    ;; can't set cursor anywhere in here....
    nil
    ))

;;}}}
;;{{{ code: yank
;;; ############################################################ &yank ###
;;; Functions related to posted text handling
;;; - yanking greply text



;;; ----------------------------------------------------------------------
;;;
(defun tir-yank-study ()
  "Sets few variables variable by examining buffer with no CIT attached yet.

References
    tir-study-buf-info        ,func
    tir-yank-v-is-clean-post  ,this is ret val too.
"
  (interactive)
  (let* ((cit-re (concat "^[ \t]*" tir-re1))
         (t-re  tir-fup-tail-nrm-txt-start) ;start point
         (loop t)
         ret
         )

    (save-excursion
      (tir-study-buf-info)              ;get information
      (goto-char (point-min))
      (if (null (re-search-forward t-re nil t)) nil ;no mark ?
        (setq ret 0)
        (while (and loop (not(eobp)))
          (forward-line 1)
          (if (looking-at cit-re)
            (setq loop nil ret 1))
          (end-of-line)
          )
        (setq   tir-yank-v-is-clean-post ret)
        ))))


;;; ----------------------------------------------------------------------
;;;
(defun tir-yank-mail ()
  "
1) Inserts all text of sender's mail (like C-y) to current point and
   calls tir-yank-hooks.
2) adds mail-yank-prefix in front of every line if
   tir-yank-v-add-pfx is non-nil

"
  (interactive)
  (save-excursion
    (let* (sp
           (yb mail-reply-buffer)       ;where is the yank ?
           ;;  See this mail is called from GNUS
           ;;  - If GNUS isn't loaded, set buf name to nil
           (gnus-buf (if (boundp 'gnus-article-buffer)
                         gnus-article-buffer nil))

           ;;  Test the name only if buffer exist
           (gnus-r (if  (null gnus-buf)
                       nil
                     (string=
                      gnus-buf
                      (cond             ;might bu STR or BUFFER PTR
                       ((stringp yb) yb)
                       ((bufferp (buffer-name yb)))
                       (t ""))
                      )))

           (top (point))
           ;;  the indented prefix + spaces if specified.
           (istr (tir-myp))
           )
      (setq sp (point))
      ;;  (mail-yank-original '(4))     ; mimic C-u C-c C-y == no indent
      ;;  - bypass all, see sendmail.el :: defun mail-yank-original
      ;;    this is more robust, and runs no extra hooks
      ;;  - If in GNUS,the buffer will be *Articly*, which is
      ;;    narrowed to headers...widen the buffer before yanking.
      (if (null gnus-r)
          (mail-yank-original '(4))     ; normal mail
        (save-excursion (set-buffer yb) (widen))
        (insert-buffer yb))

      (save-excursion                   ;clean extra spaces from end
        (goto-char (point-max))
        (delete-blank-lines))

      (tir-yank-study)                  ; set's some globals
      (run-hooks tir-yank-hooks)

      (if (null tir-yank-v-add-pfx) nil ; not allowed to do this
        (goto-char sp)
        (save-excursion                 ; add indent string in front
          (while (not (eobp))
            (insert istr )
            (forward-line) (beginning-of-line)
            )))
      ;; -------- top level save excursion
      )))

;;}}}
;;{{{ code: snip

;;; ############################################################ &snip ###
;;; Functions related to area handling
;;; - snips text around region


;;; ----------------------------------------------------------------------
;;;
(defun tir-snip-reg-type ()
  "Tries to guess few types for the region beeing snipped.
Supposes that we are on buffer where snipped text is yanked.
"
  ;; (interactive)
  (let* ((types tir-snip-v-type-alist)
         (i 0)   (loop t)
         el re type
         )
    (save-excursion
      (goto-char (point-min))
      (while (and loop
                  (setq el (nth i types)))
        (setq re (nth 0 el))
        (save-excursion
          (if (null (re-search-forward re nil t)) nil ;skip
            (setq loop nil)
            (setq type (nth 1 el))))
        (inc i)
        ))
    type
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tir-snip-set-start ()
  "User may position BEG marker whereever, so we use a little intelligency
to determine good position for quoted snip text. The skipping stops when
it is considered valid position. See *-v-snip-rex* variables.

Starts moving from current position.
RETURN: position, also point sits there.
"
  ;; (interactive)
  (let* ( (s-list tir-v-snip-skip-rex-line)
          (case-fold-search t)
          (bp (point))                  ;save begin point
          p
          (loop t) i re
          GOOD
         )
    ;;  We move until good line is found
    (while (and loop
                 ;;  Must start with word, it's better policy
                (re-search-forward "[\t ][a-zA-Z]" nil t))
      (backward-char)                   ;See [A-z] isn't at front!
      ;; (setq ll       (buffer-substring (point) (+ 4 (point))))
      ;; (d! (point) ll)

      (setq i 0)
      (beginning-of-line)
      (while (and loop
                  (setq re (nth i s-list)))     ; get REX,that excludes line
        (if (not (looking-at re))
            (progn
              (setq loop nil)           ; good line!
              (setq GOOD t))
          (forward-line 1)              ; discard it
          (beginning-of-line))
        (inc i))
      )
    ;; ---------- end lines while
    (if (null GOOD)                     ; all discarded ?
        (goto-char bp))                 ; ok, use the beginning
    ;;    trick to get to the BEG of next word
    (forward-word 1) (forward-word -1)  ; remember, we were at the BEG of line
    (point)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tir-snip (beg end &optional arg)
  "Snipps text inside region. Inserts few words from the beg as a reminder.

It's better to snip several sections, than one huge section, if it contain
eg. code and text. This is due to heurestic used: if we detect any 'code'
inside snip region, then the whole region is flagged as code...this
might not be very informative for the reader. Intead, separate text and
code to their own snip sections.

Optional ARG forces always to use 'word quoting'.
"
  (interactive "r\nP")
  (let* ((tb tir-tmp-buf)
         (snip-pfx "snip")
         (max tir-v-snip-max-len)
         (cc tir-v-snip-words)          ;character count loop
         (end-re "[?/-+=!@#$%^&*(){}_~`<>:,]?[ \t]*$")
         bp                             ;buffer pointer
         (i 0)
         (msg  "") w
         lines
         p
         snip-type
         (loop t)
         )
    ;;   kill area and move it temporarily for handling
    ;;   --> it's easier to search/get/copy words,
    (kill-region beg end)    (setq bp (get-buffer-create tb))
    (save-excursion
      (set-buffer bp) (erase-buffer) (yank)
      (untabify 1 (point-max))          ; get rid of TABs
      (replace-regexp "[ ]+" " ")       ; delete extra spaces
      (goto-char (point-min))           ;

      (setq lines (count-lines (point-min) (point-max)))

      ;;   get ready...
      (if (null arg) (setq snip-type (tir-snip-reg-type)))
      (tir-snip-set-start)              ; good snip QUOTE point
      (while (and loop
                  (< i cc))             ;this many chars..

        ;;  EOL is special case due to C-f
        (if (null (looking-at end-re))
            nil                         ; not EOL
          (setq loop (forward-word 1))  ; jump over \n and
          (backward-word 1)             ; position at the beg of line
          (setq msg (concat msg " ")))

        (setq p (point))                ; end of word point
        (setq w "")                     ; empty it
        (if loop                        ; only if not EOB
            (setq w                     ; read the word
                  (progn
                    (forward-word 1)
                    (buffer-substring p (point)))))
        (setq msg (concat msg w))   ; add words

        (inc i))

      (if (< (length msg) max) nil      ;msg len within limit
        (setq msg (substring msg 0 max)))
      )
    ;; -------------- save-excursion
    (setq msg (concat msg "..."))       ;the phrase cont. marks
    (if snip-type (setq msg snip-type)) ;Discard the text if type was detected.
    (insert (concat
             "[" snip-pfx " " lines
             ": "  msg "]\n"))
    (beginning-of-line)
    (if (not (eobp)) (forward-line 1))
    ;; (recenter)
    ))

;;}}}

;;{{{ fup: general

;;; ############################################################# &fup ###
;;; Functions related to posted text handling
;;; - Header deletion
;;; - logo deletion





;;; ----------------------------------------------------------------------
;;;
(defun tir-fup-get-2re (re str)
  "Gets 2 rexps from string. Returns list:: ('' '') if not matched"
  (let* ((m1 "")
         (m2 "")
         )
    (if (eq nil (string-match re str))
        t                               ;do nothing, not matched
      (if (match-end 1)
          (setq m1 (substring str (match-beginning 1)
                                 (match-end 1))))

      (if (match-end 2)
          (setq m2 (substring str (match-beginning 2)
                                 (match-end 2))))
      )
    (list m1 m2)))

;;; ----------------------------------------------------------------------
;;;
(defun tir-fup-mchk ()
  "Checks that we are in mail mode.
Searches for rexp tir-fup-tail-nrm-txt-start, normally \"^--text\"
RETURN
  nbr   ,next line position after --text.
  0     ,no --text found.
"
  ;; (interactive)
  (save-excursion
    (let* ((ret 0)
           (re tir-fup-tail-nrm-txt-start)   ; copy global
           )
      ;;  make sure we are in POST mode
      (if (re-search-forward re nil t)
        (progn          ; mark where we are
          (forward-line) (beginning-of-line) (setq ret (point))))
      ret)))



;;; ----------------------------------------------------------------------
;;;
(defun tir-mail-txt-beg ()
  "Returns text start beginning pos for mail reply buf. RET 0=not found"
  ;;  (interactive)
  (let* ((ret 0)
         (re tir-fup-tail-nrm-txt-start))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward re nil t)
          (progn (forward-line) (beginning-of-line) (setq ret (point)))))
    ret))




;;; ----------------------------------------------------------------------
;;;
(defun tir-get-break-list ( marks blen &optional prefix)
  "Examines buffer for solid [marks] starting from current point.

The line is considered break line if
a.   it has continuous break char and nothing else.
     The break mark must be at least [blen]

If optional PREFIX is given, it is cat'd before the characters
to indicate line start prefix.

"
  ;;  (interactive)
  (let* (l
         re p
         (marks tir-v-break-marks)
         (len (length tir-v-break-marks))
         (re-cand (concat "[" tir-v-break-marks "]+")) ;candidate line!
         line is-break block
         )
    (save-excursion
      (while (not (eobp))
        ;;  - We must position cursor after citation first to do the looking
        ;;  - This way the prefix chars used are independednt of the break
        ;;    marks used. We can't try ^[:>]*[>]+ ...try yourself
        ;;                      CIT chars^^  ^^^BREAK marks list
        ;;               when line like ': > >  hello'
        ;;   The problem is that the same char '>' appears in both and lisp
        ;;   doesn't match such regexp right ... or should I say
        ;;   it just ignores the CIT because there was * modifier.
        (if (and prefix
                 (looking-at prefix))
            (goto-char (match-end 0)))

        ;; - if the CHAR is found, then examine if it's break
        ;; - The line  may contain any chars, as long as they are
        ;;   cat'd together.
        ;;   *  this may give misleading information, since an uuencoded
        ;;      solid text block is also considered as break.
        ;;   *  But this doesn not matter because the uuendode is
        ;;      removed prior to calling this.
        (if (not (looking-at re-cand))    ;no such regexp marks
                nil
          ;; read the line
          (setq line (tir-read-if-solid))

          (if line nil
            ;; Hmmm, it's not solid, is is repetitive ??
            (if (null (setq block (tir-read-block))) nil
              (setq re (concat "\\(" (regexp-quote block) "+[ \t]*\\)+$"))
              (if (null (looking-at re)) nil
                (setq line
                     (buffer-substring
                      (match-beginning 0) (match-end 0))))
              ))
          (setq is-break (if (< (length line) blen) nil t))

;;;       (d! "line" is-break line)
          ;;   - All these 'add-to-list' commands  immediately
          ;;     cause Garbage collecting like hell.... :-[ why ?
          ;;   - I didn't notice any of them to be speedier than the other ?
          ;;   - The append seems to garbage collect least ?
          ;;
          ;; (if ok (setq l (nconc l (list p)))) ;put BEG of line into list
          ;; (if ok (setq l (cons p l)))

          (if (null is-break) nil
            (beginning-of-line)
            (setq p (tir-kill-ftp))
            (setq l (append l (list p) ))
            )
          )
        (forward-line 1)
        )
      l
      )))



;;; ----------------------------------------------------------------------
;;;
(defun tir-get-breaks ()
  "Examines buffer for possible break marks.
This is master func to tir-get-break-list. Adds suitable prefix before calling

References:
[marks]          tir-v-break-marks
[blen]           tir-v-break-len-min
"
  (let* ((pfx tir-cit-beg-re)
         (blen tir-v-break-mark-len-min)
         (marks  tir-v-break-marks)
        )
    (save-excursion
      (goto-char (point-min))
      (tir-go-txt)                      ;try to find beginning of mail text
      (tir-get-break-list marks blen pfx)
      )))



;;}}}
;;{{{ fup: email




;;; ----------------------------------------------------------- &email ---
;;;
(defun tir-fup-parse-from-email (line)
  "Tries to parse various formats of 'From:' fields.
The line is scanned for @% marks, gives up if there isn't none.

Returns: '\(usrname site\), \(\"\" \"\"\) if cannot parse.
"
  (let* (li
         (fid "$parse-from-email")      ; function id for error messages
         ;; '.' is for firstname & surname combination
         ;; '=' is for gateway form
         ;; '|{' are scandinavian characters in name
         ;; '+' Believe or not, but I just saw account name like
         ;;     "Stephen M. Lacy" <sl31+@andrew.cmu.edu>

         (A "[-a-zA-Z|{0-9_=.+]+")      ; alphabet
         (As "[-a-zA-Z0-9.%]+")         ; site name

         ;;  Note that username can have scandinavian {| marks
         ;;  Normal site name
         ;;  o   Simon.Marshall@mail.bar.foo.fi (Simon Marshall)
         (re1 (concat "\\(" A "\\)@\\(" As "\\)"  ))

         ;;  Marla=Bush%aoa.rdt%OS.DC@Ban-Gate.AoA.DHHS.EDU
         (re2 (concat "\\(" A "\\)\\(%" As "\\)"  ))


         ;;  VAX address <"TNCLUS::TSYVANEN"@mailer.foo.fi>
         (re-vax (concat "\\(\"" A "::" A "\"\\)@\\(" As "\\)"  ))
         em                             ; email

         )

    (catch 'found

      (setq em (tir-fup-get-2re re1 line))
      (if (not (equal "" (nth 0 em)))     (throw 'found t))

      (setq em (tir-fup-get-2re re2 line))
      (if (not (equal "" (nth 0 em)))     (throw 'found t))


      (setq em (tir-fup-get-2re re-vax line))
      (if (not (equal "" (nth 0 em)))     (throw 'found t))
      )
    (if (< (length (nth 0 em)) 1)
        (tir-msg t fid " cannot parse:" line))
    em
    ))


;;; ----------------------------------------------------------------------
(defun tir-fup-parse-from-email-full (line)
  "Front end to tir-fup-parse-from-email.
Returns: email, \"\" if cannot parse
"
  (let* (li site)
    (setq li (tir-fup-parse-from-email line))
    (setq site (nth 1 li))

    (if (string-match "%" site)         ;special site/gateway name
        (concat (nth 0 li) site)
      (concat (nth 0 li) "@" (nth 1 li)))
    ))

;;}}}
;;{{{ fup: from

;;; ----------------------------------------------------------- &parse ---
;;; (tir-t-name-parse)
;;;
(defun tir-t-name-parse ()
  "Because the tir-fup-parse-from-name function is quite a complicated,
and slightest modification may render it it, I had to make a testing
function that I could run over it every time I correct it's behaviour or
add some new regexps. This ensures that the old functionality is preserved
in spite of changes.
"
  (let* (list
         e1 e2 li n
         )

    ;; No real site addresses
    (setq list
          '(
            "\"Joseph B. Ottinger\" <joeo@freenet.scri.fsu.edu>"
            "\"Mia Ingstr|m puh. 2407 600\" <INGSTROM@tne01.tele.nokia.fi>"
            "\"stephane (s.) boucher\" <sbo@bnr.ca>"
            "stern@amath.washington.edu (L.G. \"Ted\" Stern)"
            "eh@eclipse.in-berlin.de (H.J.Ehling)"
            "Marla=Bush%aoa.rdt%OS.DC@Ban-Gate.AoA.DHHS.EDU"
            "jaalto@mac.uu.net.fi \(Jari Aalto\)"
            "jaalto@mac.uu.net.fi \(Jari S. Aalto\)"
            "Jari-Teppo Aalto jaalto@mac.uu.net.fi"
            "TOM <\"TNCLUS::TLAHTINEN\@mac.uu.net.fi\""
            "\"NTC01::KARINIEMI\"@mac.uu.net.fi"
            "\"Mia Ingstr|m puh. 2407 600\" Mia@mac.uu.net.fi"
            "\"John B. Doe\" <foo@mac.uu.net.fi>"
            ))

    (setq ptr list)
    (while ptr
      (setq n (car ptr))
      (setq li  (tir-fup-parse-from-name n))
      (setq e1 (nth 0 li)) (setq e2 (nth 1 li))
      (read-from-minibuffer (concat e1 "," e2 "<"))
      (setq ptr (cdr ptr))
      )
    ))


;;; (tir-t-name-parse)
;;; ----------------------------------------------------------------------
;;;
(defun tir-fup-parse-from-name (line)
  "Tries to parse various formats of 'From:' fields.
Supposes that the 'From:' keyword is removed from the beginning.
Returns: '\(firstname surname\), \"\" if cannot parse.
"
  (let* (li
         (fs-re2 tir-fup-v-sender-re-name)
         (fs-vax tir-fup-v-sender-re-name-vax)
         (fs-vax2 tir-fup-v-sender-re-name-vax2)
         (q-no-re tir-fup-v-sender-re-name-quotes-no)
         (fs-fse tir-fup-v-sender-re-first-sur-email)
         (gtw-re1 tir-fup-v-sender-re-gtw1)
         cfs name pick
         w w1 w2 w3
         D                              ;debug
         beg end beg1 end1 beg2 end2
         )
    (setq cfs case-fold-search)         ;copy the value for a moment
    (catch 'found
      ;;  It's most important that the match test are made IN THIS ORDER
      ;;  - Quote test cannot precede vax name test.
      ;;  - Try most restrictive first.

      ;; ..............................................................
      ;;  VAX is identified by "::" marks
      ;;
      (if (eq nil (string-match "::" line)) nil ;skip, no match
        (setq li (tir-fup-get-2re fs-vax line))
        (if (equal "" (nth 0 li)) nil (setq D "vax1") (throw 'found t))

        (setq li (tir-fup-get-2re fs-vax2 line))
        (if (equal "" (nth 0 li)) nil (setq D "vax2") (throw 'found t))
        )

      ;; ............................................................
      ;; Try gateway addresses, rare, but seen in net still
      ;;
      (if (eq nil (string-match "%" line)) nil ;skip, no match
        (setq li (tir-fup-get-2re gtw-re1 line))
        (if (equal "" (nth 0 li)) nil (setq D "gtw1") (throw 'found t))
        )

      ;; ............................................................
      ;; And the rest , is there parens or ""  somewhere ?
      ;;
      ;;   - if we get multiple match "stephane (s.) boucher" ,
      ;;     (L.G. \"Ted\" Stern) , pick the one that's longer.
      ;;
      (if (string-match "\"\\(.*\\)\"" line)
          (setq beg1 (match-beginning 1)  end1  (match-end 1)))
      (if (string-match "[(]\\(.*\\)[)]" line)
          (setq beg2 (match-beginning 1)  end2  (match-end 1)))
      (cond
       ((and beg1 beg2)
        (if (> (- end1 beg1) (- end2 beg2))
            (setq beg beg1  end end1)
          (setq beg beg2  end end2)))
       (beg1
        (setq beg beg1  end end1))
       (beg2
        (setq beg beg2  end end2)))




      (if (null beg) nil                ;no matches
        ;;   - Get list of words into W
        ;;   - Someone wrote M. "Mack" Monroe, so the " is included
        ;;     in words separate list
        ;;   - The latter picks only NON-ABBREVIATED names, non-phones..
        ;;     M. "Mack" Monroe --> Mack Monroe
        ;;

        (setq pick (substring line beg end))
        (setq w (tir-get-blocks pick "[. \"]+" "[.]" ))

;;;     (d! "w-1" w)
        (setq w (tir-list-match-re q-no-re w t))
;;;     (d! "w-2" w)

        (cond
         ((> (length w) 3)              ;too much abbrev names
          ;;  pick first and last one
          (setq w1 (nth 0 w)  w2 (nth (1-(length w)) w)  )
          (setq li (list w1 w3)))
         ((eq 3 (length w))
          (setq w1 (nth 0 w)  w2 (nth 1 w)  w3 (nth 2 w))
          (cond
           ((string-match "[.]" w1)
            ;;  person is using middle name, like "M. Mike Monroe"
            (setq li (list w2 w3)))
           (t
            (setq li (list w1 w3)))
           ))
         ((eq 2 (length w))
          (setq w1 (nth 0 w)  w2 (nth 1 w))
          (setq li (list w1 w2)))
         ((eq 1 (length w))
          (setq li w))
         (t
          (setq li (list "" ""))))

        (if li (throw 'found t))
        )

      (setq li (tir-fup-get-2re fs-re2 line))
      (if (equal "" (nth 0 li)) nil (setq D "2.1") (throw 'found t))


     (if (eq nil (string-match fs-fse line)) nil ;skip, no match
        (setq li (tir-fup-get-2re fs-fse line))
        (if (equal "" (nth 0 li)) nil (setq D "2.2") (throw 'found t)))


      ;; ------------ catch
      )
;;;    (d! "parsed" D  li)
    (setq case-fold-search cfs)         ;restore value
    li
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fup-gfrom ()
  "Gets writers name by looking at \"From\" field in the header of replied
mail message.
RETURN
  list
    (line sender fn sn)
  nil    ,no such field found.
"
  ;; (interactive)

  (let* ((fid "$gfrom:")                        ; function id
         (send-re tir-fup-sender-rexp)  ; copy globals, shorter names
         (mp-hdr tir-fup-max-hdr-pos)

         (sender "")                    ; whole from line
         (line "")
         (textp (tir-mail-txt-beg))
         li                             ; list return value
         )
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward send-re  nil t)
          ;;   Speaker must be at the front of message, not in  tail..
          (if (< (point) textp)
              t                         ; skip, not allowed
            (setq line (tir-read-line))

            ;; delete that 'From' keyword:
            (setq sender (tir-del-re ".*From: *" line))

            (setq li (tir-fup-parse-from-name sender))
            (if (not (equal "" (elt li 0))) nil
              (setq tir-D line)         ;save the value
              (tir-msg t fid " Can't parse From field." line))

            )))
    (append  (list line) li)
    ))



;;}}}

;;{{{ fup: logo

;;; ------------------------------------------------------------ &logo ---
;;;
(defun tir-kill-logo-exec (txt choice p)
  "Kills from 'point && beg-of-line'  to point-max. Asks confirmation.
Checks that the point is not at text beginning.
INPUT
  txt    ,message
  choice ,what is users choices
  p      ,starting kill point

RET
  t     ,if killed.

SET GLOBAL
  RET-1 ,the value returned when question was asked
"
  (let* ((ret nil)
         ans
         (re-txt tir-fup-tail-nrm-txt-start)    ; starting re for text
         (msg tir-fup-kill-msg)
         pbeg
         mp                                     ; mesg point
         )
    (catch 'cancel
      ;;  make sure we're in Main mode
      (if (= (setq pbeg (tir-mail-txt-beg)) 1)     ; find text begin
          (throw 'cancel t))
      (save-excursion
        (goto-char p)                   ; start here
        (beginning-of-line)
        (if (< p  pbeg) (throw 'cancel t)) ; must be within message area
        (setq p (point))
        (insert msg)            (setq mp (point))
        (recenter)                      ;so that text is seen [bottom?]
        (setq ans (tir-yn txt choice))
        (if (eq t ans)
            (progn
              (kill-region p (point-max))
              (setq ret t))
          (kill-region p mp))           ; remove that message
        )
      (message "")
      )
    (setq RET-1 ans)                    ;SET GLOBAL the user's choice
    ret))



;;; ----------------------------------------------------------------------
;;;
;;; ftp = fine tune point
(defun tir-kill-ftp (&optional maxp leave direction)
  "When killing point is located, it may not be exactly the best
cutting position. It only means that the regexp os found from that line.
This function searches for empty lines, that separate paragraph, so
that:

      text text text text text
      text text
                                            <--fine tuned point

      [x]kill point USER NAME               <--this is found


The DIRECTION controls tuning movement. It's normally backward from
the point. If optional LEAVE is give, it tells how many lines may
be left to separate paragraph. So if leave is 1 and there is only 1
empty space separating parag. then the killing point isn't moved.

The MAXP sets limit of the looking area.
"
  (let* (
         (re-nempty tir-nempty-cit)
         (re-empty  tir-empty-cit)
         ;;  The killing point now
         (kp (save-excursion (beginning-of-line) (point)))
         (ret kp)                       ;default
         s-fun move-fun p
         start
         )

    (if direction
        (setq s-fun 're-search-forward
              move-fun 'backward-line start 'end-of-line)
      (setq s-fun 're-search-backward
            move-fun 'forward-line start 'beginning-of-line))

    (save-excursion
      ;; try to locate text beginning
      (funcall start)                   ;from where we will start ?
      (funcall s-fun re-nempty maxp t)
      (if leave (funcall move-fun leave))
      (setq p (point))

      (if direction
          (if (< p kp) nil
            (backward-line 1)
            (setq ret p))
        ;;  Is before the kill point ?
        (if (> p kp) nil
          (forward-line 1)              ;point is at the end of TEXT
          (setq ret (point))
          ))
      )                                 ;own line so that can COMMENT away..
;;;    (d! ret (symbol-name s-fun)(tir-read-line p))
    ret
    ))




;;; ----------------------------------------------------------------------
;;;
;;;
(defun tir-kill-get-jump-lines ()
  "Sometimes there is all that supercite header stuff and so on on the post.
This just tries to guess if we should ignore some lines = skip them over.

Supposes that point sits at right starting point: message start.

RETURN
  point      ,after some header stuff is skipped. Or might be the same
              as in start.
"
  (let* (
         (max tir-v-skip-msg-area)      ;XX line area should suffice
         (re tir-v-skip-msg-beg-re)     ;skipped phrases
         (ign-re tir-v-stop-msg-area-re)

         ;; - if there is many lines, then add this to
         ;;   skip items. We assume that user may then wrote
         ;; - the treshold value is MAX
         ;; hi XXX,
         ;; Thanks for the ...
         ;;
         ;;
         (lines (count-lines (point) (point-max)))
         (add-re "\\|thanks\\|thank you")
         (re (if (> lines max)
                 (concat re add-re) re))


         (op (point))                   ;original point
         (ret op)                       ;default ret val
         (p op)                         ;END marker

         ;; We don't look very far away, just some few lines...
         (ep                            ;end point
          (save-excursion (forward-line max) (point)))
         (loop t)
         )
    (save-excursion
      (while (and loop (re-search-forward re ep t)) ;search until ep
        (beginning-of-line)
        (if (null (looking-at ign-re))
            (setq p (point))
          (setq loop ni))               ;stop right there
        (end-of-line)                   ;continue search
        )
      (if (null p) nil                  ;no matches at all
        (forward-line 1)                ;found s'thing, skip the line
        (setq ret (point))))
    ;; (d! "--jump-lines" ret)
    ret
    ))







;;; ----------------------------------------------------------------------
;;;
(defun tir-killp-context-diff (start-pos)
  "Gets posssible 'diff' killing point."
  ;; - The begin signature is always in this form, and we take 3 samples
  ;;   to determine that it really is diff start
  ;; - people seldom send "normal diffs" via NET.
  ;;
  ;;   *** 3.9 1995/01/25 08:50:39
  ;;   --- wmpmanmx.cc 1995/03/14 11:30:24
  ;;   ***************
  ;;   *** 160,166 ****
  ;;
  (let* (ret
         p
         (re1 "[*][*][*] .")
         (re2 "--- .")
         (re3  (regexp-quote "***************"))
         )
    (goto-char start-pos)
    (if (null (re-search-forward re1 nil t)) nil ;no match
      (setq p (point))
      (goto-char (match-beginning 0))
      (tir-fwd-line)
      (if (null (looking-at re2)) nil
        (tir-fwd-line)
        (if (null (looking-at re3)) nil
          (setq ret p )))               ;it must be the one...
      )
    ret
    ))

;;; ----------------------------------------------------------------------
;;; Searches for good killing point candinates
;;;
(defun tir-kill-logo-getp (fn sn)
  "Gets posssible killing points
SETS GLOBALS
  Dvar-1   ,trigger criterias as list of strings

INPUT
  fn    ,first name
  sn    ,surname
RETURN
 \(point point point point\)  possible killing points"
  ;; (interactive)
  (let* (
         (op (point))
         (case-fold-search t)           ;ignore case
         (msg-start (tir-mail-txt-beg)) ;start line of message

         ;;  some special functions to search for killing points
         (kill-func-list tir-kill-logo-getp-funcs)

         ;; Don't know it this is usefull parameter...
         (sig-area tir-kill-logo-sig-v-max-lines); allowed lines for sig

         msg-start2                     ; msg-start + jump-lines

         (CIT tir-cit-beg-re)           ;citation beginning re

         (big-post (> (point-max)  tir-fup-v-code-point))
         (max-point (if big-post tir-fup-v-code-point (point-max)))

         (s-fun (if big-post 're-search-backward 're-search-forward))

         (myp tir-mail-yank-prefix)     ;** yank prefix **
         (istr (tir-myp))               ;** indent string **

         (user (nth 0 tir-v-sender-email)) ; >> set for us in FUP main
         (site (nth 1 tir-v-sender-email))
         (email (nth 2 tir-v-sender-email))

         ;;   For name signature
         re-nam

         (re-empty  tir-empty-cit)      ;last para marker
         (re-cit tir-fup-re)            ; valid re markers


         (re-wav tir-fup-v-re-wave)     ;
         (re-wave-single tir-fup-v-wave-single-re)
         (re-code tir-fup-v-re-code-wave)

         ;; Normal delimiter ,like in gnus '--'
         (re-nrm (concat istr tir-fup-tail-nrm-brk))
         (re-list (list re-nrm re-code))                ; RE list to match
         ptr

         (loop t)

         lines ok
         kp-list                        ;kill point list , RET VAL
         br-list                        ;break list
         s-limit
         p ptr
         i
         valid-point hit1 hit2
         re                             ;general RE
         n-abbrev                       ;name abbreviation
         lines-in-msg                   ;in whole buffer
         start2-line                    ;line number begin
         cur-line                       ;current-line
         line
         HIT_STR                        ;only for debug
         )
    (setq Dvar-1 "")                    ;*DEBUG , init variable

    (save-excursion
      ;;   First set up message point for sig scan.
      ;;   - the msg2 is area which is pacified. It includes
      ;;     reference fields: who is speaking and so on...
      (goto-char msg-start)

      (setq msg-start2 (tir-kill-get-jump-lines))
      ;;  go to end if SMALL
      (if big-post nil
        (goto-char (point-max)))

;;;      (d! "--msg-start2 " msg-start msg-start2)


      ;; This is just rough approx, since the trailing logo is included,
      ;; but it gives us a hint if post is long or not.
      ;; --> see name finding
      (setq lines-in-msg (count-lines (point-min) (point-max)))
      (setq start2-line (count-lines (point-min) msg-start2))

      ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
      ;;  PERSONS NAME SIGNATURE         #name
      ;;  - Name has to be at least 3 chars, because two char combination
      ;;    may mean anything     "Mike M"  --> reject that 'M'
      ;;  - Person may also write "Mike M." --> reject that 'M.' too
      ;;
      ;;  - There may be cases where the account name is not the best thing
      ;;    to match. Eg.
      ;;            Keinonen Kari <kk85613@cs.tut.fi>
      ;;    Then he informs:
      ;;            See my page: http://www.cs.tut.fi/~kk85613/
      ;;
      ;;    Because the account name "kk85613" is included in the name RE,
      ;;    it will match first --> have make it more restrictive by adding
      ;;    SPACE at fron of email name. See later.

      (if (< (length fn) 2) (setq fn nil))
      (if (or (< (length sn) 2)
              (and (= (length sn) 2) (string-match "\." sn))
              )
          (setq sn nil))

      ;;   to be sure that no special chars cause trouble
      (if fn (setq fn (regexp-quote fn)))
      (if sn (setq sn (regexp-quote sn)))

      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
      ;; Some people use *First* characters as their sig name.

      (cond
       ((and fn sn)
        ;; Some people use *First* characters as theis sig name.
        ;;  it must be on it's own line only
        (setq
         n-abbrev
         (concat  CIT
                 (substring fn 0 1) (substring sn 0 1) "[ \t]*$"))
        (setq re-nam (concat fn "\\|" sn "\\|" n-abbrev)))
       ((and fn (not sn)) (setq re-nam fn))
       ((and sn (not fn)) (setq re-nam sn))
       )

      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
      ;; add user's email account name too

      (if (> (length user) 2)
          (if (eq 0 (length re-nam))
              (setq re-nam user)
            ;;  make the email name restrictive if it has special chars,
            ;;  like id123123@bix.com
            ;;
            (if (string-match "[^-a-zA-Z._]" user)
                (setq user (concat "\\( " user "\\)"  )))       ; ++ ADD SPACE
            (setq re-nam (concat re-nam "\\|" user ))))

;;;      (d! "~name: " re-nam )
;;;      (setq RE re-nam)

      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
      ;;   - Name special case, because it will always be
      ;;     met first when scanned forward. The signature may have
      ;;     many references to name, so backward scanning is worse.

      (if (eq 0 (length re-nam))
          nil                           ;nothing to look for
        (save-excursion                 ;(1) *** NAME fn sn
          (setq valid-point nil p nil)  ;clear values
          (goto-char msg-start2)        ;Not right at the beginning...


          (if (null (re-search-forward re-nam nil t)) nil
            ;; - allthough we found reference to name it still isn't a
            ;;   gurrantee that the point is ok.
            ;; - Eg some one may write 'Hi mike,' and that is not an ending!
            ;;
            ;; - suppose point is ok, we change it to nil if not.
            ;;   First try the obvious...
            (setq p (point) line (tir-read-line)
                  cur-line (tir-cl)
                  talk-range (+ 3 start2-line)
                  re (concat "[hH][iI] +\\(" fn "\\|" sn "\\)"
                             ;;  discare any REFERENCE lines, who spoke what
                             ;;  - suppose year is on the line
                             "\\|.*95"
                             )
                  )

            (if (string-match re line) (setq p nil)) ;discard

;;;         (d! "~name-range " p talk-range cur-line "ms2" msg-start2)
            (if (and p (< cur-line talk-range ))
                (setq p nil))           ; start of mesasge perhaps

            ;; ........................ now it might be right point
;;;         (d! "~name-hit " (point) p)
            (if (null p ) nil
              ;;(beginning-of-line) (setq p (point))
;;; This test is unused yet...
;;;         (if (null big-post))
              (setq p (tir-kill-ftp))   ;fine tune the point
;;;           (d! "~name-ftp " p )
              (setq HIT_STR (concat HIT_STR " Name=" p))
              (setq kp-list (tir-nconc-num p kp-list)))
            )
          ))


      ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
      ;;   WAVES
      ;;   - Waves are not that simple. I just saw a post where
      ;;     person wrote in two lines "Any syggestions\n Thanks in advance\n"
      ;;   - The problem with RE is that it always matches first word that
      ;;     is included in the RE string :
      ;;          RE = "thanks\\|suggestions"
      ;;     This means that it'd pick 'thanks' first and forgeet the rest.
      ;;     ...We can't change the order because someone might just
      ;;     wave the opposite.
      ;;   - Okay, what we do is narrow the search limit every time there is
      ;;     a hit, until no more hits. Then we assume it's the first wave.
      ;;
      (setq loop t s-limit (point-max) p nil hit1 nil)
      (while loop                       ;until no hits
        (goto-char msg-start2)
        (setq loop (re-search-forward re-wav s-limit t))
;;;     (d! "~wave1-hit" loop s-limit (point) (tir-read-line))
        (if (null loop) nil             ;if hit, then make more restrictive
          (beginning-of-line)  (setq p (point)) ;last wave point
          (setq s-limit p)              ;this varies
          )
        )

      (if (or  (null p)                 ;only if search took place
               (<= p msg-start2))       ;within header ?
          nil
;;;     (d! "~wave1 raw " p)
        (goto-char p)                   ;for ftp to work
        (setq p (tir-kill-ftp))         ;fine tune point
;;;     (d! "~wave1-ft " p)
        (setq HIT_STR (concat HIT_STR " Wave1=" p))
        (setq kp-list (tir-nconc-num p kp-list)))

      ;;   - Now I saw post where there was 'thanks for the compliment'
      ;;     followed by lot of text. A full chapter in fact. Now, I
      ;;     don't want to delete it, so lets gather some more killing
      ;;     points for wave.
      ;;   - lets pick one from the end of msg too. I'm not using
      ;;     same regexp here, I might contruct more variations later if
      ;;     needed.

      (save-excursion
        (goto-char (point-max))
        ;; lets try with this
        (setq re (mapconcat 'concat re-wave-single "\\|"))

        (if (null (re-search-backward re msg-start2 t)) nil
          (setq p (tir-kill-ftp))
          (setq HIT_STR (concat HIT_STR " Wave2=" p))
          (setq kp-list (tir-nconc-num p kp-list))))


      ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--



      ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
      ;;   LAST PARAGRAPH         #para
      ;;
;;;      (d! "LAST PARA-" (symbol-name s-fun)  max-point)
      (save-excursion                   ;(1) *** NAME fn sn
        (setq p msg-start2)             ;a shorter name here
        (goto-char (point-max))
;;;     (goto-char max-point)
        (re-search-backward re-cit p t)           ;find citation
;;;     (d! "CIT" (point))
        (setq hit1 (re-search-backward re-empty p t))     ;para separator

        (beginning-of-line)

;;;x    (d! "para~1" hit1 (point) msg-start2)
        (if (or (null hit1)             ; not found empty line
                (< (point) msg-start2)) ; within header area
            nil
;;;       (d! "~para~" (point))
          (setq p (tir-kill-ftp))
;;;       (d! "~para-fine" p)
          (setq HIT_STR (concat HIT_STR " Para=" p))
          (setq kp-list (tir-nconc-num p kp-list))
          ))

      ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
      ;; REGESP-LIST scanning
      ;; - this is under construction

      (setq ptr re-list)
      (save-excursion
        (while ptr                      ; try all re's in the list
          (goto-char msg-start2)
          (setq re (car ptr))
;;;       (setq hit1 (funcall s-fun re nil t))
          (setq hit1 (re-search-forward re nil t))

          (if (null hit1) nil           ; NOT FOUND
;;;         (d! "FOUND" (point) re (tir-read-line))
            (setq p (tir-kill-ftp))
;;;         (d! "~re~" (point) p)
            (setq HIT_STR (concat HIT_STR " reg=" p))
            (setq kp-list (tir-nconc-num p kp-list)))
          (setq ptr (cdr ptr))))        ; advance pointer



      ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
      ;; SOLID-BREAKS , I meant those continuous lines in there
      ;;
      ;; Make the use of this restrictive, because it takes time to scan the
      ;; buffers.
      ;; - someday make it smarter, when to use it when not

;;;     (d! "Solid breaker,list len:" (length kp-list))
      (if (> (length kp-list) 3)        ;do not exec this if there is > 3-list
          nil
        (save-restriction               ;only interesting area
          ;; (narrow-to-region msg-start2  max-point)
          (goto-char  msg-start2 )
          (setq br-list (tir-get-breaks))
;;;       (d! "~breaks-found " (length br-list))

          ;;  Use only last and first breaks found, others do not interest us
          ;;
          (setq p (length br-list))
          (cond
           ((< p 3) nil)                ;do nothing, 1 or 2 found
           ((> p 2)
            ;; formerly used last 2 breaks
            ;; (setq br-list (tir-slice-list 'last 2 br-list)))
            ;; - now pick first and last
            (setq br-list (list (car br-list) (nth (1- p) br-list)))
            ))

          (while br-list                        ;examine points
            (setq p (car br-list))              ;get pos
;;;         (d! "SOLID pos" p)
            (setq HIT_STR (concat HIT_STR " br" p))
            (setq kp-list (tir-nconc-num p  kp-list))
            (setq br-list (cdr br-list))
          )
        ))



      ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
      ;; SPECIAL ONES
      ;; - Everything just can't be done via regexp. We need to move
      ;;   around a point if we make a RE hit to determine if the point
      ;;   if a good candidate for killing
      ;; - launch some functions that may return killing points to us
      ;; - The point is automatically fine-tuned.

      (setq ptr kill-func-list i 0)
      (while ptr
        (inc i)
        (setq p (funcall (car ptr) msg-start2))
        (if (null p) nil
          (goto-char p)
          (setq p (tir-kill-ftp))
          (setq HIT_STR (concat HIT_STR " F" i "=" p))

          (setq kp-list (tir-nconc-num p kp-list)))
        (setq ptr (cdr ptr)))



      ;; --``--  --``--  --``--  --``--  --``--  --``--  --``--  --``--
      ;; RETURN

      (goto-char op)                    ;return to original point
;;;      (d! msg-start2 HIT_STR)
      kp-list
      )))



;;; ----------------------------------------------------------------------
;;;
(defun tir-kill-logo (fn sn)
  "Kills trailing logos in mail reply. Main function"
  ;; (interactive "sFirst name: \nsSurname: ")
  (let* ((ok nil)
         (abort-code tir-QA)
         (kp-list (tir-kill-logo-getp fn sn))   ;get possible killing points
         (ll (mapconcat 'concat kp-list " "))
         (ptr kp-list)
         (loop t)
         (msg "kill logo ") (choice "[ret/del/abort]: ")
         killed
         )

;;;    (d! "points" ll)
    (while (and loop ptr)
      (if (tir-kill-logo-exec msg choice (car ptr))
          (progn
            (setq killed t)
            (setq loop nil)))           ; stop if killed
      (if (eq RET-1 abort-code) (setq loop nil)) ;abort request
      (setq ptr (cdr ptr))              ;advance killing point list
      )
    killed
    ))

;;}}}
;;{{{ fup: main


;;; ----------------------------------------------------------------------
;;;
(defun tir-fup-form-sender (str)
  "Formats sender line. Input is line with From: keyword
RET
  str  ,formatted line, without 'From:'
  nil  ,if cannot format
"
  (let (
        list
        fn                              ; first name
        sn                              ; surname
        sender                          ; ..obvious :->
        email
        (limit 60)                      ; sender line limit
        )
    ;;   Try finding names,  delete that 'From' keyword:
    (setq sender (tir-del-re ".*From: *" str))

    ;;   If the line is exessive long, say;
    ;;     "Mr. Foo the most spectacular..." <foo@camel.com>
    ;;   Then we make it smaller.
    (if (< (length sender) limit)
        sender
      (setq list (tir-fup-parse-from-name sender))
      (setq fn (nth 0 list)) (setq sn (nth 1 list))
      (setq email (tir-fup-parse-from-email-full sender))
      (concat fn " " sn " <" email ">") ; this should suffice
    )))



;;; ----------------------------------------------------------------------
;;;
(defun tir-fup-prepare ()
  "The mail can contain uuencoded stuff etc, which
we wish to remove from reply.

After this function finishes it calls tir-fup-tidy-hooks, where you
can do more inspecting about mail. PLEASE READ THE DOC of that hook,
before using it.
"
  (let* ((uu-b-re "begin [0-9][0-9]+ .")
         (uu-e-re "end$")
         (bf tir-tmp-buf)               ;copy global
         is-uu
         bp ep
         col
         )
    (save-excursion
      (goto-char (point-min))
      (save-excursion
        (setq ep (and (re-search-forward uu-e-re nil t)
                      (point))))

      ;;  leaves point at the BEG of UU
      (setq bp (and (re-search-forward uu-b-re nil t)
                    (point)))

      (if (or (null bp)
              (null ep)
              (< ep bp))                ;end must follow begin !
          nil                           ;skip, no UU found
        ;;  Go verically down from '(b)egin 644 file' line
        (beginning-of-line) (re-search-forward "b" nil t)
        (backward-char 1)               ;'cause of RE-SEARCH
        (tir-fwd-line 1)

        (setq col                       ;what's the length or rectangle?
              (save-excursion
                (progn (end-of-line) (current-column)))) ;chars on line

        ;;  We need to get take an sample of the code, take just one line
        (copy-rectangle-to-register     ;forget possible citations
         1
         (point)
         (save-excursion (end-of-line) (point)))

        ;;  move data to another buffer for inspection
        (save-excursion
          (set-buffer (get-buffer-create bf)) (erase-buffer)
          (insert-register 1)
          (setq is-uu (tir-is-uuline))
          (kill-buffer bf)
          )

        (if is-uu                       ;UU detected !
            (delete-region              ;I don't ask perm from user...*I'm bad*
             (progn (goto-char bp) (beginning-of-line) (point))
             (1+ ep))                   ;include newline
          ))

      ;;  What else user want's to clean?
      (run-hooks tir-fup-tidy-hooks)
      )))


;;; ----------------------------------------------------------------------
;;;
(defun tir-fup-mail-said ( date )
  "Inserts Reference line, who is speaking.
The point is already placed at the top of messge
"
  (let* (
         (cit tir-mail-yank-prefix)     ;the CIT char
         (myp tir-mail-yank-prefix)     ;needed for time insertion, no indent
         (istr (tir-myp))               ;indent string, full citation
         )
    ;;  Now insert my styled 'From'
    ;;  the MYP can be replaced with ISTR
    (insert (concat cit date ", " sender))
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tir-fup-mail ()
  "This was originally coded for formatting (gnus) follow up commands
text, but it'll suit formatting regular mail replys as well.
Leaves point at text beginning.

1) kills all header info from previous post.
   - Of course you could set mail-yank-ignored-headers defined
     in sendmail.el, but I don't feel like adding *all* kinds
     of different possibilieties for killing rexps..
2) adds FROM -- who is talking
3) removes trailing logo garbage. This is kind of heuristic
   analysis where user is let to chose what will be removed.

SETS GLOBAL
  tir-v-sender-email  ,list of 3 elements[user,site, u@s]

"
  (interactive)
  (let* (
         (TRANSIENT-MODE transient-mark-mode)

         (kill-msg tir-fup-kill-msg)    ;copy global
         (re-hdr-brk tir-fup-hdr-brk)   ;copy global
         (kill-hdr tir-fup-v-kill-hdr-flag)     ;automatic header kill ?
         (said-fun tir-fup-v-said-func) ; composes title
         (abort tir-QA)                 ; abort code

         b p                            ; begin mark
         sender                         ; ..obvious :->
         list
         fn                             ; first name
         sn                             ; surname
         o-sender                       ; original sender line
         sender                         ; ..obvious :->
         send1                          ; sender proposal 1

         (brk 0)
         list                           ; return list from funcs
         date                           ;
         ans                            ; Query to user
         logo-killed
         email-li                       ;[user site] list
         )


    ;;  We disable this temporarily if it's turned on. It makes killing
    ;;  area hiliting uncomfortable.
    (transient-mark-mode 0)

    ;;  This is used by *-getp, holds user email data
    (setq tir-v-sender-email (list "" "" "")) ;init >>GLO<<

    (save-excursion
      (goto-char (point-min))
      (catch 'cancel
        ;;  make sure we are in POST mode
        (setq b (tir-fup-mchk))    (if (equal 0 b) (throw 'cancel t))

        ;;   who's speaking ? save it...
        (setq list (tir-fup-gfrom))
        (setq o-sender (nth 0 list))

        ;;  Now we Add 3 elements to the list
        (setq email-li (tir-fup-parse-from-email o-sender))
        (setq tir-v-sender-email        ; >>GLO<< [user site u@s]
              (if (string-match "%" (nth 1 email-li))
                  (append
                   email-li             ;just cat together, it has '%' addr
                   (list (concat (nth 0 email-li) (nth 1 email-li))))
              (append email-li
                      (list (concat (nth 0 email-li) "@" (nth 1 email-li))))))

        (setq fn (nth 1 list )) (setq sn (nth 2 list )) ;first/sur name
        (if (equal "" o-sender) (throw 'cancel t))

        (setq sender (tir-fup-form-sender o-sender))

        (setq date (tir-get-reply-date))

        ;;   Check and remove certain things from mail reply.
        ;;   - Remove uuencoded text
        (tir-fup-prepare)

        ;;   try to locate REAL reply text begin
        ;;   Which is header end also.. REMOVE ALL HEADERS
        (tir-go-txt)

        (if (eq nil (re-search-forward re-hdr-brk  nil t)) nil ;skip, no hdr
          (end-of-line) (setq p (point)) (insert kill-msg)
          (if kill-hdr nil
            (setq (tir-yn "kill header" )) ;ans has more that just t/nil
            (setq kill-hdr (eq t ans)))
          (if kill-hdr
              (progn
                (kill-region b (point))
                ;; ........ who said what ....
                (funcall said-fun  date)
                )
            (kill-region p (point))     ; remove kill-msg, clean up
            (if (equal abort ans) (throw 'cancel t))
            )
          ;;  rexp failed, can't find header end line
          ;;  use --text beginning as end header point
          (setq brk b))

        ;;  Killing trailing logos...
        (setq logo-killed (tir-kill-logo fn sn))

        ))
    (tir-go-txt)


    ;;  Restore the mode if it were turned on
    (if TRANSIENT-MODE  (transient-mark-mode 1))

    (run-hooks tir-fup-after-hook)      ;something more to do?
    logo-killed
    ))

;;}}}


;;{{{ Example settings

;;; ....... example setup ................................... &example ...
;;;
;;; This is direct copy from my .emacs.mail
;;; - Gives you some ideas of how to use tinyreply's functions
;;; - Executes everytime I use mail or GNUS either for sending regular
;;;   mail or replying to them.
;;; - Just paste these function to your .emacs and use
;;;   query-replace (M-%) for removing comment prefixes ';;* '
;;; - If the code didn't retain it's lisp indentation after you removed
;;;   prefixes, select region and issue M-x indent-region command to
;;;   get 'nice' look back.


;;; Use this if you want automatic installation.
(defun tir-example-install (save)
  "Installs the example setup for you."
  (interactive "FSave setup to file: ")
  (let* (
         (file (tir-get-load-path "tinyreply.el" load-path))
         (re1 (regexp-quote "&example"))
         (comment-str  ";;* ")
         (re2 (concat "^" (regexp-quote comment-str)))
         (leave-re (concat re2 "\\|^$" ))
         (tb (get-buffer-create tir-tmp-buf))
         bp lines
         )
    (if (or (null file)
            (null (file-readable-p file)))
        (message (concat "Cannot locate/load " lib " within the load-path."))
      (save-excursion
        (set-buffer tb) (erase-buffer)
        (set-buffer (setq bp (find-file-noselect file)))
        (goto-char (point-max))

        (if (null (re-search-backward re1 nil t)) nil
          (setq lines (buffer-substring (point) (point-max)))
          (set-buffer tb)         (insert lines)  (goto-char (point-min))
          (delete-non-matching-lines leave-re )
          (replace-string comment-str "")
          (goto-char (point-min))
          (insert
           (concat
            ";;\n"
            ";; Mail reply setup for TIR v" (tir-get-version) "\n\n"))
          (write-file save)
          (kill-buffer tb)
          (message
           (concat "Install complete. Please execute load-file " save))
          ))
      )))

;;* ;;;  Setup the hooks for RMAIL and GNUS 3.14.1;NNTP 3.10
;;* ;;
;;* (setq mail-setup-hook 'mail-my-setup)
;;* (setq news-reply-header-hook 'mail-my-setup)
;;* (setq mail-default-headers nil)      ;I add these manually for GNUS only


;;* (defconst my-mail-default-headers
;;*   (concat
;;*    "Env-info: HP-UX A.09.05 A 9000/712,\n\temacs FSF 19.28.2\n"
;;*    "\tperl 4p36 rev 4.0.1.8\n"
;;*    )
;;*   "*When posting to the net, these will be included. notice TAB chars"
;;*   )
;;*

;;* ;;;  make TIR autoload compatible
;;* ;; - Insert call to function 'tir-autoload' inside mail-setup-hook and
;;* ;;   the TIR packet is automatically loaded; after that the hook
;;* ;;   is run. Little tricky eh :-/
;;* ;;
;;* (autoload 'tir-autoload "tinyreply" "Reply tool" t)
;;* (setq tir-autoload-hook 'my-tir-setup)
;;*
;;*
;;* (defun  my-tir-setup ()
;;*   "Configures TIR package for my taste."
;;*   (tir-key-reply-define)
;;*   (setq tir-mail-yank-prefix":")
;;*   (setq tir-mail-yank-prefix-indent " ")
;;*   (setq tir-mail-subj-reply-prefix "Re! ")
;;*
;;*   (setq tir-v-snip-max-len 45)
;;*   (setq tir-v-snip-words 10)
;;*   (setq tir-v-cc-kill-flag t)               ;automatic header killing
;;*   nil                                       ;hook must return this
;;*   )


;;* ;;; ----------------------------------------------------------------------
;;*
;;* (setq mail-citation-hook 'my-undo-yank)
;;*
;;* ;;  - The 'R' key in gnus-summary mode is difficult: chain goes like this
;;* ;;    --> gnus-summary-reply-with-original
;;* ;;    --> gnus-summary-reply
;;* ;;        -> funcall gnus-mail-reply-method...
;;* ;;    --> (gnus-mail-reply-using-mail)
;;* ;;        -> news-mail-reply              ;; this calls our mail-my-setup
;;* ;;        -> gnus-overload-functions
;;* ;;        -> mail-yank-original
;;* ;;           -> mail-citation-hook
;;* ;;           -> mail-yank-hooks
;;* ;;
;;* ;;  - As you can see, the mail-my-setup gets called in the middle of the
;;* ;;    functon: news-mail-reply inserts the header and our mail-my-setup
;;* ;;    adds the yanked text.
;;* ;;  - Now, at the end gnus yanks _itself_ the text into buffer which we
;;* ;;    already have put there...so we would get two text of the same post
;;* ;;  - I could of course overload the gnus-mail-reply-using-mail, but I
;;* ;;    don't want to make any modifications to orig. funcs unless it's
;;* ;;    absolutelu necessary.
;;* ;;  - In this case we fortunately can _undo_ *eh* the yank that happened
;;* ;;    by using  mail-citation-hook, which gets called.
;;* ;;  - Seems awfull ? Well, depends on the point of view, I just like all
;;* ;;    things to be controlled in one place: in mail-my-setup.
;;* ;;
;;* ;;  - It this seems too complicated:
;;* ;;    o   leave this func out and do not set mail-citation-hook
;;* ;;    o   Never use 'R' in gnus. Use 'F' and change the destination :->

;;* (defun my-undo-yank ()
;;*   "Removes yanked text that is not citated. Yank is handled elswhere."
;;*   (let (non-cit)
;;*     ;;  The TIR may not be loaded yet, that's why the boundp test
;;*     ;;  --> happens in GNUS for the first time
;;*
;;*     (if (null (boundp 'tir-mail-yank-prefix)) nil
;;*       (setq non-cit (concat "^[^" tir-mail-yank-prefix  "]"))
;;*       (tir-go-txt)
;;*       (if (null (re-search-forward non-cit nil t)) nil
;;*     (beginning-of-line)
;;*     (delete-region (point) (point-max)))
;;*     )))

;;* ;;; ----------------------------------------------------------------------
;;* (defun my-mail-tidy ()
;;*   "I want to fix some things in the reply post, caused by other mailers..."
;;*   (if (null (tir-go-txt)) nil
;;*     ;;  I feel this is unnecessary, makes text flood
;;*     (replace-regexp "In article +[<].*[>] *" "")
;;*     ))
;;*




;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun mail-my-setup ()
;;*   "Deletes line In-reply-to and calls various other functions to
;;* format reply. Used for both NEWS and MAIL.
;;*
;;* Normally does everything for you. See tinyreply.el for more functions
;;* and ideas.
;;*
;;* "
;;*   (interactive)
;;*   (let (p                           ;temporary pointer
;;*     (gnus-buf                       ;GNUS might not be loaded in this emacs
;;*      (if (boundp 'gnus-article-buffer)
;;*          gnus-article-buffer ""))   ;normally name is "Article"
;;*
;;*     (mail-buf (cond                 ;what is the mail YANK buffer name?
;;*                ((stringp mail-reply-buffer)
;;*                 mail-reply-buffer)
;;*                ((bufferp mail-reply-buffer)
;;*                 (buffer-name mail-reply-buffer))
;;*                (t "")))
;;*
;;*     (cc-kill (if (boundp 'tir-v-cc-kill-flag ) ;should CC be kept ?
;;*                  tir-v-cc-kill-flag
;;*                nil))                ;this is for AUTOLAOAD, first time.
;;*     (case-fold-search t)            ;ignore case. Do I Need to set this?
;;*     (mode (symbol-name major-mode))
;;*     in-reply
;;*     gnus-f                          ;if F or f pressed
;;*     gnus-r                          ;gnus 'news-mail-reply-->rnewspost.el
;;*     news-mode
;;*     folder
;;*     )
;;*
;;*     (if (fboundp 'my-mail-keys)             ;set up additional keys if func exist..
;;*     (my-mail-keys))
;;*     (if (null (featurep 'tinyindent)) nil ; use tinyindent.el if exist
;;*       (tii-mode) (tii-tt-mode))
;;*
;;*     (tir-autoload)                  ;loads if not already loaded
;;*
;;*     ;; FILL
;;*     ;; - I don't want to affect the setting of this variable
;;*     ;;   in general, so I make it local to this buffer for a moment
;;*     ;; - I also want to have line wrap, so that lines do not bocome too long
;;*     (make-local-variable 'fill-column)
;;*     (setq fill-column 70)
;;*     (auto-fill-mode 1)
;;*
;;*     (goto-char (point-min))         ;ready to rock and roll' :-'>
;;*
;;* ;;    (d! "mail-hook: cc-kill"  cc-kill)
;;*
;;*     ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;*     ;;       GNUS default header adjust
;;*     ;;
;;*     (save-excursion                 ;I don't want to send this..
;;*       (if (null (re-search-forward "^In-reply" nil t)) nil
;;*     (setq in-reply t)               ;signal deletion
;;*     (tir-kill-line)))
;;*
;;*     ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ FCC ^^^
;;*     ;; This is for general <mail> wheather from GNUS, RMAIL or MAIL
;;*     (tir-fcc-set-by-hdr)
;;*
;;*     ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GNUS ^^^
;;*     ;;    Detect news posting mode [Rr]
;;*     ;;    - The mail program uses YANK from the gnus buffer "*Article*"
;;*     ;;      So we can detect if this is gnus post
;;*     ;;
;;*     (setq gnus-r (string= gnus-buf mail-buf))
;;*     (setq news-mode  (or gnus-r (string-match "news" mode)))
;;* ;;;    (d! "mail-hook: reply-buf" gnus-r gnus-buf mail-reply-buffer )
;;*
;;*     ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ RMAIL ^^^
;;*     ;;  - There is subject line if "reply" pressed.
;;*     ;;  - If we are in "m" mail-mode only,  then ignore these
;;*     ;;
;;*     (if (or (equal "" (tir-mail-get-field "subject"))
;;*         news-mode)
;;*     nil                             ;skip
;;* ;;;      (d! "rmail")
;;*       ;;   Are we really in there ?
;;*       (if (null (string-match "mail" mode)) nil
;;*     (tir-cc-field-kill-to)             ; remove senders's addr. from CC
;;*     (tir-nuke-mail-cc-field cc-kill)   ; ask if CC is kept
;;*     (tir-mail-format-subject)          ; handle reply count
;;*     (goto-char (setq p (point-max)))
;;*     (tir-yank-mail)
;;*     ;;   Although you are in mail buffer, it does not mean
;;*     ;;   you have text in 'yank' buffer
;;*     ;;   - the 'f' command in gnus doesn't put text into buffer
;;*     ;;     so we cannot adjust it
;;*     (if (eq p (point-max))
;;*         nil                         ; no yank, point not moved
;;*       (tir-kill-ctrl-chars)         ; delete those ^H and other TTY chars..
;;*       (tir-mail-ins-info)
;;*       ;;   - This is for FINNISH language conversion only
;;*       ;;   - Not included in TIR library
;;*       (if (fboundp 'skandix) (skandix))
;;*       ;;   Now delete those headers and logos
;;*       (tir-fup-mail)
;;*       (my-mail-tidy)
;;*       ))
;;*       )
;;*
;;*     ;;  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GNUS ^^^
;;*     ;;
;;*     ;;
;;*     (if (null news-mode)  nil                  ;skip
;;*       (goto-char (point-min))
;;* ;;;      (d! "gnus")
;;*
;;*
;;*
;;*       ;;  - Has user pressed '[fF]' ? --> there is this field.
;;*       ;;  - It means that yanking is possible.
;;*
;;*       (save-excursion
;;*     (goto-char (point-min))
;;*     (if (null (re-search-forward "Distribution" nil t)) nil
;;*       (setq gnus-f t)))
;;*
;;*       (if (fboundp 'my-gnus-add-headers) ;add my ENV info and the like...
;;*       (my-gnus-add-headers))
;;*
;;*       (d! "fcc set")
;;*       ;;  - You could also use:  (tir-study-buf-info) (tir-fcc-set-by-study)
;;*       ;;  - You have to call study-buf first to get updated results !!
;;*       ;;  - I wan't to keep record of what I've sent. Easier to use one folder
;;*       (tir-fcc-set "~/.post")
;;*
;;*       ;;  - Gnus yanks text automatically when 'F'
;;*       ;;  - Remove that post, and use my own yanking
;;*       (save-excursion
;;*     (if (null (tir-go-txt)) nil
;;*       (kill-region (point) (point-max))))
;;*
;;*       ;;  Try yanking the text
;;*       (goto-char (point-max)) (setq p (point))
;;*       (tir-yank-mail)
;;*       (if (eq p (point-max))
;;*       nil                           ; no yank
;;*     (if gnus-f (tir-news-copy-to-sender)) ; not when '[arR]' pressed
;;*     (tir-kill-ctrl-chars)
;;*     (tir-fup-mail)
;;*     (my-mail-tidy)
;;* ;;; (d! "fup-killed")
;;*
;;*     )
;;*       ;; ------------ gnus setup end ---------------
;;*       )
;;*     ;;  NOTE: While this hook completes you can't set the CURSOR to
;;*     ;;  some specific position. I tried, and It didn't work. So
;;*     ;;  it might show up anywhere...
;;*     nil                                     ;hook must return this
;;*     ))


;;; ... for permanent use ................................... Example2 ...
;;; - I noticed using some of the functions all the time, so I wanted to
;;;   to have them in all modes. I didn't want to use C-c prefix, becaus
;;;   almost every el defines it's own keymaps there, so if I turned on
;;;   some mode, it took over C-c...

;;* ;;; ----------------------------------------------------------------------
;;* ;;; - Setting my own prefix key and few funcs to them.
;;* ;;;
;;* (setq my-key-pfx "\C-x\C-z")
;;* (setq my-map (lookup-key global-map my-key-pfx));private Ctrl-t map
;;* (if (keymapp my-map) nil   ;already defined
;;*   (setq my-map (make-sparse-keymap))
;;*   )
;;* (define-key global-map my-key-pfx my-map)
;;* (define-key my-map "\C-a" 'tir-add-str)
;;* (define-key my-map "a" 'tir-add-str-reg)
;;* (define-key my-map "\C-r" 'tir-del-re-reg)

;;}}}
;;{{{ End of file tag

;; Contructed with folding.el 1.7 and tinyfold.el 1.5
;; the above turns folding on automatically when file is visited.
;; Local variables:
;; folded-file: t
;; end:
;; ......................... end of tinyreply.el ...........................

;;}}}
