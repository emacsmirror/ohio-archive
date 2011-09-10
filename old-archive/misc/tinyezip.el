;; @(#) tinyezip.el -- 'load' support for compressed elisp files.

;; @(#) $Id: tinyezip.el,v 1.18 1995/09/18 11:52:39 jaalto Release_1 jaalto $
;; @(#) $Keywords: ompression, file, load $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs


;;{{{ Id

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Sep 7 1995
;;
;; To get information on this program use ident(1) or do M-x tinyezip-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinyezip|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Support for compressed elisp files: require, load, load-library,load-file..|
;; 18-Sep-1995|1.18|~/misc/tinyezip.el.Z|

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

;;}}}
;;{{{ Install

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file. *MUST BE FIRST ENTRY* before any other
;; 'load' or 'require' command. Otw compressed elisp files are not recognized.
;;
;;	(load "~/elisp/tinyezip.el")
;;
;; or
;;	(require 'tinyezip)
;;
;; and
;;     (global-set-key "\C-cZ" 'tinyezip-post-command-install)
;;
;; It is *highly* recommended that you bind this command to some key,
;; because the handler drops off from post-command-hook quite easily. Eg
;; C-g clears these handlers. This ensures that Keyborad bound commands,
;; that are autoloaded, will succeed from compressed files.
;;
;; To disable/enable totally this package, call
;;
;;	tinyezip-unadvice
;;
;; Warning: See limitations section.
;; Note   : See example setup at the end of file
;;

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o Allows commands LOAD, LOAD-LIBRARY, LOAD-FILE, REQUIRE and AUTOLOAD
;;   defined load to accept jka-compr supported lisp '.el' files that are
;;   compressed.  Note: you can't compress .elc files and have them loaded,
;;   only .el files.
;;
;; o Now you can compress all your lisp library files in your ~/elisp
;;   dir and save space eg. for quota limit reasons, while still being
;;   able to load them as usual.
;;
;; o You don't have to change anything in your .emacs startup file, all
;;   will work as usuall. (Note: see Limitations. You cannot use autoloaded
;;   function inside lisp code without preparations.)


;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - This package is special one, so the naming rules does not apply!
;; - few funcs are collected from my general tinylibXXX.el lisp libraries so
;;   that we don't need any external 'require' commands.
;; - In general, the names will have prefix 'ti::' referring to
;;   tinylib. Macros have no naming convention. The naming convention used
;;   is further explained in tinylib.el
;;
;; - file's name means (tiny) (e)lisp (zip) handling, where "zipping"
;;   is commonly used synonym for compression.


;; PREFACE
;; ========================================
;; - I got tired of watching my University's quota every time I found
;;   some useful elisp code I could stuff into my ~/elisp directory. I
;;   thought that "I must have this and that .el, because it's so
;;   useful..". Sooner or later I have to raise my hands and give
;;   up. My disk quota didn't allow any more lisp code, besides every
;;   time I received mail I got announcement:
;;
;;       "Quota exceeded, you have 7 days to remove excess files."
;;
;; - Annoying? You bet!
;; - After a while I came up with the idea of configuring basic
;;   emacs commands so that they could also load lisp packages
;;   that were in compressed form. Basicly the designing seemed
;;   simple, the commands should check for compressed files and load
;;   it, instead of the raw .el or .elc file. But there were a catch,
;;   jka-compr knew only files that were loaded into emacs for editing,
;;   not about lisp files that should be just 'evaled'. So, there I
;;   was, hacking up the basic lisp commands with defadvice... :_/
;;
;; jka-compr and this package
;; ........................................
;; - jka-compr has native support to un/compress any file that has specific
;;   extension. The handling is done via file-name-handler-alist and
;;   commands like
;;
;;	(load "my-el.gz")
;;
;;   will load properly, including any autoloads.
;;
;; - The obvious problem is that you have to manually go and change
;;   all you load commands so that they end in .gz so that jka-compr
;;   takes care of loading. What if you later uncompress the file? You
;;   have to go and update all the load commands in you .emacs.XXX
;;   files. This isn't very nice, since you should be able to
;;   un/compress elisp files whenever you wish and still have that
;;   statement
;;
;;	(load "my-el")
;;
;;   untouched.
;;
;; - Basicly this is what this package is all about; you don't have to
;;   worry if the file is compressed or not when you use these advised
;;   functions. Besides any 'require' command in any lisp module work too
;;   for compressed files.
;;
;; Using pre-command-hook
;; ........................................
;; - Every time programmer puts hook into [post|pre]-command-hook, emacs
;;   slows down a bit, because it increases overhead. For me, the
;;   pre-command-hook seememd to be last resort to intercept keyboard
;;   mapped autoload command, before it was handled by emacs.
;; - I tried to use smart check, which would not spend excess time
;;   in the hook and while I have loaded 55 lisp packages, I can't tell
;;   if emacs slowed down or not.
;;
;;
;; HOW IT WORKS
;; ========================================
;; - Basicly this way:
;;
;;   o  When user request 'load FILE', try to find some compressed file
;;      that JKA knows about by adding extensions ".gz" and ".Z" and
;;      whatever user has configured JKA to handle.  ** LIMITATION:
;;      only .gz and .Z and the like that compress one file at a time
;;      is currently supported. Don't try using .zip or similar.
;;   o  If the FILE contain absolute directory, then look from that
;;      directory only
;;   o  If no directory is given, find the file along the path.
;;   o  If there was somewhere a compressed file, just load it (because JKA
;;      will transparently uncompress it), eval it, and kill the buffer
;;      where file was some fract of second for evaling.
;;   o  and if NO COMPRESSED file was found, just follow normal emacs rules.
;;
;; Load failed -- is there a bug in this package?
;; .................................................
;; - If you have had habit of keeping debu-on-error t, you'll be seeing
;;   a little different error messages now, when the basic lisp load
;;   functions are advised. If the last messagege includes
;;   prefix "ad-Orig", say the 'load' command failed:
;;
;;	ad-Orig-load("myfile") Failed....
;;
;;   It means that the original lisp 'load' function couldn't locate
;;   the file you tried to visit. Messages like these are not *bugs* of
;;   this package. Bugs related to this package come in picture normally
;;   only if you have lisp files in _compressed_ formats and when  it fails to
;;   find them. Make sure that:
;;
;;   o  Your load-path contains the directory the file is loacated,
;;	if you gave load command without the directory part.
;;   o  Directory name is spelled right in the command
;;   o  Make sure that the pre-command-hook handler is installed !!
;;      It may drop off due to various reasons, like pressig C-g too hard.
;;      This the most common reason for failing to load, a typical error
;;      shown then is:
;;
;;	Signalling: (file-error "Cannot open load file"myfile.el")
;;
;;   o  You have configured jka-compr-compression-info-list properly
;;      and tata this program can decode the info right. Try
;;      running (ti::a-jka-compress-ext ) by hand and it should return
;;      valid identifiers like (".gz" ".Z")
;;
;; Ending
;; ========================================
;; - If you notice that your fat ~/elisp has loosed some(!) weight
;;   lately, drop me mail and tell me this package made it happen :-/
;; - Of course your emacs now starts ever _slower_ that it used to,
;;   but you also gained much more space. Try to optimize everything
;;   so that they use 'autoload' as much as possible and you're much
;;   happier with the startup time.
;;
;; Is this file Slow to load or what?
;; - Yup, I know. It's that advice thing that does it. Never mind
;;   about the load time. You want to save space, not time, eh ?

;;}}}
;;{{{ Bugs & limitations

;; .............................................. &t-bugs-limitations ...
;; - If any of the STD emacs functions handled here get overloaded,
;;   ie. replaced with smeone else's definition of that function,
;;   this package will break.
;; - Currently the only function handled specially is
;;
;;	execute-extended-command
;;
;;   which can be safely redefined [as is done in execcmd.el]
;; - The problems may not become visible, if the redefined func doesn't
;;   use following function in it's body:
;;
;;	(interactive-p)
;;
;;   The problem is that advice's ad-do-it can't pass the interactive flag
;;   information to the original functions.
;;
;; LISP SOURCE CODE
;; ..................
;; - It's not possible handle 'eval' command. This means that you cannot do
;;
;;        (function-call)C-xC-e
;;
;;   or execute behind any other arbitrary lisp expression that might
;;   include function call to autoloadable function that is stored in
;;   compressed package.. So, in general, if you use command that you KNOW
;;   is autoloaded, use appropriate 'require package' before you're going
;;   to use that command.
;;
;;   ie. *this is bad thing*
;;
;;	(autoload  'some-minor-rmode "some-minor-mode"  t t)
;;      (add-hook  'mail-setup-hook  'my-mail-setup-hook)
;;
;;	(defun my-mail-setup-hook ()
;;	  "My mail preparations."
;;	  ;;  I want to use this handy minor mode while I edit mail
;;	  (some-minor-rmode t)
;;
;;   What happens is, that lisp tries to execute expression, where
;;   "some-minor-mode" is used and which is declared as autoload and then
;;   it goes and tries find associated file to load.  But because this all
;;   is handled by lisp itself, there is no tinyezip handler to take care
;;   of detecting possible compressed some-minor-rmode.el.gz --> Your code
;;   breaks, and lisp barfs.
;;
;;   The solution is to use appropriate (require 'some-minor-mode)
;;   before using the command in lisp.

;;}}}
;;{{{ history


;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Sep	14	1995	[jari]		19.28	v1.18		Release_1
;; - Serious error in ti::a-zip-elisp-p corrected. When I added the
;;   "newer" file test, this function changed a lot and many variables were
;;    pointing to wrong files --> almost every time returned nil.
;; - Okay, this is first official release, because seems quite stable now.
;;
;; Sep	14	1995	[jari]		19.28	v1.17		NotReleased
;; - Mistakenly left out some Lib functions I needed in this new versions.
;; - + ti::f-newer-exist , ti::s-index
;;
;; Sep	14	1995	[jari]		19.28	v1.16		NotReleased
;; - Commented out previous code and started from 1.14 base.
;; - Now it can detect which file is newer [.el .el.gz] and load it instead of
;;   always loading ".gz" if one existed.
;; - Added example section to the end of file: hoiw can you accoplish
;;   automatic compression houshold everytime you start emacs.
;;
;; Sep	13	1995	[jari]		19.28	v1.15		NotReleased
;; - Experimental. Not working. Tried to use Hans suggestion for
;;   tinyezip-ad-do-it.
;;
;; Sep	11	1995	[jari]		19.28	v1.14		NotReleased
;; - Added documentation for " Error messages" doc section. Added
;;   keybinging install notice for keeping pre-hook installed.
;; - Note "Copessed takes precedence" --> put to "to do" list
;; - Added ro limitations "LISP SOURCE CODE" section.
;; - The load-library's interactive form was errorneous. Corrected.
;;
;;
;; Sep	10	1995	[jari]		19.28	v1.13		NotReleased
;; - Added more text to the limitations section, this time it involves
;;   writing lisp code.
;;
;; Sep	10	1995	[jari]		19.28	v1.12		NotReleased
;; - put 'compile' flag to execute-extended-command advice, because it
;;   included special ad-do-it macro, it woun't work without it.
;; - 'require' command used  mistakenly tinyezip-ad-do-it when it should
;;   have used regular ad-do-it.
;;
;; Sep	10	1995	[jari]		19.28	v1.12		NotReleased
;; - The file-eval wasn't smart enough: it let my folding.el to fold the
;;   file before emacs could eval it --> emacs didn't see a thing about
;;   the functions in the file! Now it will reset some emacs variables
;;   so that no-one can't take control while it loads the file into buffer.
;;
;; Sep	10	1995	[jari]		19.28	v1.11		NotReleased
;; - After tickling with the 'around' advises I found a way how
;;   to pass the interactive definition to original function. Now
;;   when I need special call to access the interactive forms of
;;   the original function, I use tinyezip-ad-do-it instead of
;;   ad-do-it. It will pass the interactive flag property to the
;;   original function too.
;; - Added limitations documentation.
;;
;; Sep	10	1995	[jari]		19.28	v1.10		NotReleased
;; - Removed advise definition for call-interactively due to problems.
;;   At the same time, found out that it was not needed, because
;;   PRE hook would take over it and detect the command prior it is
;;   passed to call-interactively.
;; - Error in function ti::a-zip-elisp-p corrected, when passed argument
;;   with no dir info --> couldn't find file.
;;
;; Sep	9	1995	[jari]		19.28	v1.9		NotReleased
;; - Function ti::a-jka-compress-ext returned "\\.gz" when it
;;   should have returned only ".gz" ie. without any regexp marks.
;; - Structure of ti::a-zip-elisp-p changed. Now it tries FILE.el and
;;   FILE.el.gz universally, no matter what the initial input was
;; - Due to these errors the package choke, when (require 'my "~/my/my.el")
;;   was tried.
;; - Hm, it seems that advice.el can't handle advising function
;;   call-interactively, because the ad-do-it does not pass the
;;   (interactive-p) to the function, thus efectively breaking the
;;   purpose of call-interactively.
;;
;;   **  You should not advice functions **
;;       o  call-interactively
;;       o  interactive-p
;;   because it'll break all functions that make interactive tests.
;;
;; - Corrected several functions, because there was no proper ad-do-it
;;   adter the special handling: call-interactively,
;;   execute-extended-command, command-execute
;;
;; Sep	9	1995	[jari]		19.28	v1.8		NotReleased
;; - Now when I put the defalias-p and autoload-p tests inside pre-func
;;   I made a substle mistake. I took (and (sequencep sym).. when
;;   I should have taken  (and (sequencep (symbol-function sym)) ..
;;   This prevented any autoload checkings and my handler wasn't called.
;; - typo in func name: tinyezip-post-.. --> tinyezip-pre.. in the install
;;   function.
;; - Now I'm going to run few test with this before I make it next aplha.
;;
;; Sep	9	1995	[jari]		19.28	v1.7		NotReleased
;; - I noticed several flaws. The autoload-p tests should return name of the
;;   real function symbol if it was aliases, not just boolean t -->
;;   corrects execute-extended-command, call-interactively, command-execute
;; - Inserted defalias-p and autoload-p tests directly into pre-command-hook,
;;   so that it would be even faster.
;; - I didn't notice that 'require' command might be passed optional filename
;;   parameter. Now it's been taken cared of too. Does anyone use the
;;   optional parameter? At least I haven't seen one around.
;; - tinyezip-post-command-install now has optional argument that removes
;;   the handler
;;
;; Sep	8	1995	[jari]		19.28	v1.6		Release_1
;; - Used fastest test I could imagine in tinyezip-pre-command, so that
;;   opverhead is reduced to minimum. If anybody have more faster test, I'd
;;   be happy to use it instead.
;; - Added 'pre-command-hook' documentation. Added back
;;   'execute-extended-command' advise.
;; - This seems to run as advertised, but wait when the
;;   bugs crawl out...
;;
;; Sep	8	1995	[jari]		19.28	v1.5		NotReleased
;; - The last problem is solved. The pre-command-hook can intercept
;;   any command prior it's execution, so now hitting a key that calls
;;   autoloadable function succeeds by loading definition from zipped file
;;   too. Boy, I'll go and pour some Cognac for that..!
;;
;; Sep	7-8	1995	[jari]		19.28	v1.4		NotReleased
;; - Sep 7. The autoload works only if user calls function via M-x
;;   FUNC, But I don't understand what happens when commands executes
;;   directly eg. through a key ? Autoload fails then... *hmpf* I'm going
;;   bed now.
;; - Next day: Okay, now it failed to detect autoload form when the function
;;   was aliased. Corrected the autoload-p test to handle aliased names.
;; - I added advice for the call-interactively, but haven't still found
;;   the way to handle direct calls to autoloaded files (direct kbd cmd)
;;
;; Sep	7	1995	[jari]		19.28	v1.3		NotReleased
;; - It's 01:00am and night is goig still, should go to bed soon, since
;;   I have to get to work at 06:45am .. Now I noticed that the damn
;;   autoload is tricky guy. Spent tracing the triger point long time and
;;   finally found it from execute-extended-command. Now it should
;;   work.
;;
;; Sep	7	1995	[jari]		19.28	v1.2		NotReleased
;; - Indeed it was beta. The 'require' advice missing altogether.
;;   Also changed code so that when absolute path name is
;;   given, the .el is tried there too.
;;
;; Sep	7	1995	[jari]		19.28	v1.1		NotReleased
;; - This is it! Running and working fine.
;; - Send this as "Beta" to gnu.emacs.sources group.

;; To do list:
;; ========================================
;; - If there is file.el and file.el.gz, it will load "file.el"
;;   with its own file-eval. For speed it maybe should use std
;;   lisp version... marginal effect, so maybe I don't fix this one.

;; Sinere thanks to:
;; ========================================
;; Hans Chalupsky <hans@cs.buffalo.edu> -- author of Advice.el
;; - He patiently responded to my questions about proper advising and
;;   solving some of my problems. Thanks!

;;}}}


;;; Code:

;;{{{ require

;;; ...................................................... &v-tinyezip ...

(require 'cl)
(require 'advice)
(require 'jka-compr)

;;   Why backquote ? Because the tinylibXXX macros are implemented with it
;;   for portability reasons. defsubst is 19.xx stuff
(require 'backquote)

;;}}}
;;{{{ variables

;;; ......................................................... &v-hooks ...

(defvar tinyezip-load-hook nil
  "*Run when file has been loaded.")

;;; ... user configurable .................................. &v-config ...


;;; ... private variables ................................. &v-private ...

;;}}}
;;{{{ version

;;; ... version info ...................................... &v-version ...

(defconst tinyezip-version
  "$Revision: 1.18 $"
  "Latest version number.")


(defconst tinyezip-version-id
  "$Id: tinyezip.el,v 1.18 1995/09/18 11:52:39 jaalto Release_1 jaalto $"
  "Latest modification time and version number.")

(defconst tinyezip-version-doc
  "tinyezip.el -- 'load' support for compressed elisp files.

First created: Sep 7 1995
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tinyezip-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tinyezip-version-doc
       "\n\ncurrent version:\n" tinyezip-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ Install/uninstall


(defun tinyezip-pre-command-install (&optional remove)
  "If it happens that this packages post command hook drops away,
duo to setting the post-command-hook to nil, you can enable
it with this function. It'll ensure you can load definition from
zipped elisp files.

In emergencies, you can supply optional PREFIX ARG to remove this
handler.
"
  (interactive "P")
  (if remove
      (remove-hook 'pre-command-hook 'tinyezip-pre-command)
    (add-hook 'pre-command-hook 'tinyezip-pre-command)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyezip-unadvice (&optional restore)
  "Will unadvice all functions in this package. Non-nil PREFIX
argument tells to advice functions again.
"
  (interactive "P")
  (cond
   (restore
    (ad-disable-advice 'load		'around 'ti::a-zip-load)
    (ad-disable-advice 'require		'around 'ti::a-zip-load)
    (ad-disable-advice 'load-file	'around 'ti::a-zip-load)
    (ad-disable-advice 'load-library	'around 'ti::a-zip-load)
    (ad-disable-advice 'execute-extended-command  'around 'ti::a-zip-load)

    ;;  The "autoload" handler
    (ad-disable-advice 'command-execute 'around 'ti::a-zip-load)

    (tinyezip-pre-command-install 'remove)
    )
   (t
    (ad-enable-advice 'load		'around 'ti::a-zip-load)
    (ad-enable-advice 'require		'around 'ti::a-zip-load)
    (ad-enable-advice 'load-file	'around 'ti::a-zip-load)
    (ad-enable-advice 'load-library	'around 'ti::a-zip-load)
    (ad-enable-advice 'command-execute  'around 'ti::a-zip-load)
    (ad-enable-advice 'execute-extended-command  'around 'ti::a-zip-load)

    (tinyezip-pre-command-install)
    ))

  (ad-update 'load)
  (ad-update 'require)
  (ad-update 'load-file)
  (ad-update 'load-library)
  (ad-update 'command-execute)
  (ad-update 'execute-extended-command)
  )

;;}}}

;;{{{ lib

;;; ...................................................... &lib-macros ...
;;; - These functions are 'preloaded' in this section, so that
;;;   we don't have to 'require' anything, thus making this file
;;;   self standing .
;;; - All these functions come from tinylibXXX.el [ti::], so don't borrow
;;;   them. Instead get the tinylibXXX.el libraries from ohio archive
;;;   and use normal 'require' commands.



;;; ----------------------------------------------------------------------
;;; - The problem with "loading into emacs" is that all kinds of hooks
;;;   are run, eg. folding and outline might get activated when the file is
;;;   loaded --> causes that the 'eval' can't see the functions if they are
;;;   behind selective display. That
;;;
(defun file-eval (file)
  "This works like load-file, but we physically load the file first
into emacs and only then we eval-current-buffer.

The advantage over load-file is that physical loading also uncompresses
the file if there is proper elisp package to handle it, thus your elisp
can be in any file *form* that packages allow for loading.

"
  (let* (
	 ;;   This makes sure we truly load the file.
	 ;;   If there were that file in emacs, emacs won't load it.
	 (bp  (generate-new-buffer "*tmp*"))

	 ;;   Phrohibit emacs from doing anything fancy while
	 ;;   we load it to buffer
	 (enable-local-eval  nil)
	 (find-file-hooks    nil)        ;; jka doen't use this
	 )
     (save-excursion
       (set-buffer bp)
       (insert-file-contents file)
       (eval-current-buffer)
;;;       (switch-to-buffer bp)  (d! "eval buffer" find-file-hooks)
       (kill-buffer bp)
       )))


(defmacro func-car-test (symbol test-val)
  "Tests first value from symbol-function. Function must be
symbol, not in lambda form."
  (`(if (and (not (sequencep (, symbol))) ;; list ?
	     (symbolp (, symbol))	  ;; chokes if sequencep
	     (fboundp (, symbol))
	     ;;  Eg. symbol-function 'car  doesn't return list.
	     (listp (symbol-function (, symbol)))
	     (eq (, test-val) (car (symbol-function (, symbol)))))
	(, symbol)			;return the function
      nil
      )))

(defmacro autoload-p (symbol)
  "Tests if function is in its autoload form. Works with aliased symbols too.

Returns:
  symbol     ,this can be truename of the function if it was aliased
  nil
"
  (`
   ;;  Get the REAL name if it is alias or use the func's SYMBOL name
   (let* ((func (or (defalias-p (, symbol)) (, symbol)))
	  )
     (func-car-test func 'autoload))))



(defun defalias-p (symbol)
  "If function is alias, return it's truename. Otw Return nil.
Input can be anything.
"
  (let* (sym
	 prev
	 ret
	 )
    (if (or (sequencep symbol)		;lambda form ?
	    (not (symbolp symbol))
	    (not (fboundp symbol)))
	nil
      (setq sym (symbol-function symbol))
      (if (not (symbolp sym))
	  nil
	(while (symbolp sym)		;was alias, go into nesting levels
	  (setq prev sym)
	  (setq sym (symbol-function sym))
	  )
	(setq ret prev)
	))
    ret
    ))



(defmacro NEXTP (list)
  "Advances list pointer with cdr."
  (` (setq (, list) (cdr (, list)))))


(defmacro list-add (list object &optional test)
  "Appends OBJECT to LIST.  When optional TEST
is non-nil, tests if OBJECT already exist before adding.
"
  (` (if (or (null (, test))
             (not (memq (, object) (, list)) ))
         (setq (, list)
               (append (, list) (list (, object)))))))


;;; ....................................................... &lib-funcs ...



;;; ----------------------------------------------------------------------
;;; 18 Dec 1994, gnu.emacs.help, nj104@amtp.cam.ac.uk (Neil Jerram)
;;;
(defun ti::s-index (str char)
  "Returns first position of CHAR in STRING. Postion is 0..nbr."
  (let ((len (length str))
	)
    (while (and (>= (setq len (1- len)) 0)
                (/= (aref str len) char)))
    (if (>= len 0)
	len
      nil      )))


;;; ----------------------------------------------------------------------
;;;
(defun ti::f-newer-exist (f1 f2)
  "Return file that is newer OR the one that exists

Return:
  str	,file
  nil   ,none of them exist
"
  (cond
   ((and (file-exists-p f1)
	 (file-exists-p f2))
    (if  (file-newer-than-file-p f1 f2)
	f1 f2))
   ((file-exists-p f1)
    f1)
   ((file-exists-p f2)
    f1)
   (t
    nil)
  ))




(defun ti::s-verify-ends (str re &optional add-str beg)
  "Makes sure STR matches RE and adds ADD-STR string to it when
necessary. if ADD-STR is not given, adds RE to the string.

Default is to check end of string, Optionally BEG of string.
The RE may not include anchors.

Returns:
  str    ,modified or not.
"
  (let* ((RE  (if beg
                  (concat "^" (regexp-quote re))
                (concat (regexp-quote re) "$")))
         (add (or add-str re))          ;which one to add.
         )
    (if (string-match RE str)
        str
      (if beg
          (concat add str)
        (concat str add)
        ))
      ))


(defun ti::f-get-load-path (lib paths)
  "Return full path name for library LIB accross the PATHS"
  (let ((dir paths)
        (loop t)
        file found
        )
    (if (not (listp paths))
         (error "Need path list" lib paths))

    (while (and loop dir)
      (setq file (concat (ti::s-verify-ends (car dir) "/") lib))
      (if (file-exists-p file)
          (setq found file  loop nil))
      (setq dir (cdr dir)))
    found
    ))

;;}}}
;;{{{ zip

;;; ...................................................... &c-tinyezip ...
;;; Functions/macros belonging to this package only
;;;



;;c ;; expertimental
;;c (defmacro tinyezip-ad-do-it (func args &rest interactive-form)
;;c   "Preserves interactive property when calling original function..
;;c Should be used in every 'around advice.
;;c
;;c Replaces advice command:
;;c   ad-do-it
;;c
;;c The ARGS must be declared (rest args) in the advice functions input
;;c parameters.
;;c "
;;c   (`
;;c     (cond
;;c      ((interactive-p)        ; handle interactive specially, ignore advice..
;;c       (ad-with-originals ((, func))
;;c            (call-interactively (quote (, func)))))
;;c       (t                      ; let advice handle this
;;c        (setq (, args)
;;c              (,@ interactive-form))
;;c       ad-do-it
;;c       ))
;;c     ))


;;; ----------------------------------------------------------------------
;;;
(defmacro tinyezip-ad-do-it (func-sym)
  "Preserves interactive property when calling original function..
Should be used in every 'around advice.

Replaces advice command:
  ad-do-it
"
  (`
    (cond
     ((interactive-p)        ; handle interactive specially, ignore advice..
      (setq ad-return-value
	    (call-interactively (ad-make-origname (, func-sym)))))
     (t                      ; let advice handle this
      ad-do-it
      ))
    ))



;;; ----------------------------------------------------------------------
;;;
(defun ti::a-jka-compress-ext ()
  "Returns possible compression endings handled by jka.

Return
 (ext ext ext ..)       like (\".gz\" \".Z\" ..)
"
  (let* ((table jka-compr-compression-info-list)
	 ret
	 re
	 pos
	 )
    (mapcar
     '(lambda (x)
	(setq re (elt x 0))

	;;  Drop "~?" away ...
	(if (setq pos (ti::s-index re ?~))
	    (setq re (substring re 0 pos)))

	;;  Drop also \\.
	(if (setq pos (ti::s-index re ?.))
	    (setq re (substring re pos)))

	(list-add ret re)
	)
     table)
    ret
    ))


;;; just testing ... (ti::a-zip-elisp-p "tinylib")
;;; ----------------------------------------------------------------------
;;;
(defun ti::a-zip-elisp-p (file)
  "Finds compressed file name for elisp file,
If FILE does not have directory portion, load-path will be searched.
If file does not have .el extension, it will be added and tried.

    <input>           <tried>
    file.el       --> [load-path]/file.el.gz , [load-path]/file.el.el.gz
    /dir/file.el  --> /dir/file.el.gz ,/dir/file.el.el.gz
    /dir/file     --> /dir/file.gz   , /dir/file.el.gz


If .el file is newer, function will return it instead in spite of
existing zip file:

    file.el           ;; return this if newer than zipped one
    file.el.gz

.elc files are not supported.


Remark:
  Yeah, it stupidily adds \".el\" every time. This way the internal flow is
  more straight forward.

Returns:
  str     ,full filename with path
  nil     ,not found

"
  (let* (
	 (fdir   (file-name-directory file))
	 (fndir  (file-name-nondirectory file))

	 ;;    Table of valid ZIP format's jka-compr can handle
	 (table  (ti::a-jka-compress-ext))

	 el1  el2
	 try1 try2
	 ret
	 )
					;
    (while (and (null ret) table)

      ;;  Read possible extension and cat to filename
      ;;  a)   FILE.gz
      ;;  b)   FILE.el.gz
      (setq el1  fndir   el2  (concat el1 ".el")) ;; non-zipped ones
      (setq try1 (concat el1  (car table)))       ;; zipped ones
      (setq try2 (concat el2  (car table)))       ;; .el.gz

;;;      (d! ">>" fdir fndir try1 try2)

      (cond ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cond beg ^^^
       (fdir				;user gave directory
        (setq try1 (concat fdir try1)
              try2 (concat fdir try2))
	(cond
	 ;;  Try with absolute name ?
	 ((file-exists-p (concat fdir try1))
	  (setq ret (ti::f-newer-exist try1 el1)))

	 ((file-exists-p (concat fdir try2))
	  (setq ret (ti::f-newer-exist try2 el2)))
	 ))

       (t				;no directory, search path then
;;;        (d! "path>>" try1 try2)
	(cond
	 ((setq ret (ti::f-get-load-path try1 load-path))
	  (setq fdir   (file-name-directory ret))
	  (setq el1    (concat fdir el1))
;;;          (setq t1 try1 e1 el1) (d! "path 1>>" ret el1)
	  (setq ret (ti::f-newer-exist ret el1))
;;;          (d! "1>" ret)
          ))

        (cond
	 ((setq ret (ti::f-get-load-path try2 load-path))
	  (setq fdir   (file-name-directory ret))
	  (setq el2    (concat fdir el2))
;;;          (setq t2 try2 e2 el2) (d! "path 2>>" ret el2)
	  (setq ret (ti::f-newer-exist ret el2))
;;;          (d! "2> " ret)
          ))
        )
       ) ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cond end ^^^

      (NEXTP table))

    ret
    ))


					;
;;; ----------------------------------------------------------------------
;;; Keypressed command come here
;;; #pre        ,bookmark
;;;
(defun tinyezip-pre-command ()
  "Checks some things before executing command. If command is in autoload
form, it will be handled specially.

Adds support for loading compressed elisp files.
"
  (let* ((arg     this-command)
	 file
	 path
	 sym prev
	 ret
	 )
					;
    ;;   Use as fast check as possible..
    (if (or (sequencep arg)		;lambda cmd form, usual in kbd case...
	    (not (symbolp arg))		;not function call ?
	    (not (fboundp arg))
	    )
	nil				;nothing to do for us

      ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  defalias . .
      ;; The defalias-p test is duplicated here for speed reasons
      ;;
      (setq sym (symbol-function arg))
      (while (and
	      (not (sequencep sym))	;symbolp cracks otw
	      (symbolp sym)		;discard lambda forms
	      )				;was alias, go into nesting levels
	(setq prev sym)
	(setq sym (symbol-function sym))
	)

      ;;  set SYM = function name
      (if prev
	  (setq sym prev)		;alias was found
	(setq sym arg))			;wasn't an alias

      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . autolad . .
      ;; Now test if it is in autoload form
      ;;
      (if (not (and (sequencep (symbol-function sym)) ;compiled ?
		    (eq 'autoload (car (symbol-function sym)))))
	  nil				;do nothing


	(setq file    (elt (symbol-function sym) 1))
	(setq path    (ti::a-zip-elisp-p file))

	(if path	      ;;  Okay, it's compressed, let's start rockin'
	    (file-eval path))
	;;  Make sure it's same when we exit
	(setq  this-command arg)
	))


    ))




;;}}}
;;{{{ Advice

;;; ........................................................ &c-advice ...

;;; ----------------------------------------------------------------------
;;; - M-x COMMAND case.
;;; - This function is handled specially, because someone may overload
;;;   completely this function. Eg. I use package execcmd.el, which will
;;;   take over it and print all keys the command is bound to. The keys
;;;   are shown only when the call is made interactively, so this
;;;   advise must pass the interactive property to that function too.
;;; - This advice MUST be compiled, because it includes special ad-do-it
;;;   macro, hence the 'compile' flag.
;;;
(defadvice execute-extended-command (around ti::a-zip-load act compile)
  "Makes it possible to load zipped autoload form elisp code"
  ;;  This prevents Advice from using original function's interactive
  ;;  forms, we handle it elswhere later on.
  (interactive (list nil))
  (let ((arg   (ad-get-arg 0))
	(this  'execute-extended-command)
        path
        file
        )
    (if (null (setq arg (autoload-p arg)))
	(tinyezip-ad-do-it this)
      (setq file    (elt (symbol-function arg) 1))
      (setq path    (ti::a-zip-elisp-p file))

      (if (null path)
          (tinyezip-ad-do-it this)
        (file-eval path)
	(ad-set-arg 0 arg)		;make sure it's original
	(tinyezip-ad-do-it this)
	))
    ))
;; (ad-unadvise 'execute-extended-command)




;;; ----------------------------------------------------------------------
;;;
(defadvice command-execute (around ti::a-zip-load act)
  "Makes it possible to load compressed autoload form elisp code,
when function is called.
"
  (let* ((arg  (ad-get-arg 0))
	 (this 'command-execute)
	 path
	 file
	 )

    (if (null  (setq arg (autoload-p arg)))
	ad-do-it
      (setq file    (elt (symbol-function arg) 1))
      (setq path    (ti::a-zip-elisp-p file))

      (if (null path)
	  ad-do-it
	(file-eval path)
	(ad-set-arg 0 arg)		;make sure it's original
	ad-do-it
	))
    ))


;;; ----------------------------------------------------------------------
;;; #req   ,bookmark
;;;
(defadvice require (around ti::a-zip-load act)
  "Makes it possible to load zipped elisp code."
  (let* ((arg     (ad-get-arg 0))
	 (opt     (ad-get-arg 1))	;the optional "file" parameter
	 (this    'require)
	 (lib     (or (and (stringp opt)
			   (expand-file-name opt))
		      (symbol-name arg)))
	 path
	 )
    (if (featurep arg)
	ad-do-it

      ;;  Okay, it may be  compressed...
      (setq path    (ti::a-zip-elisp-p lib))
      (if (null path)
	  ad-do-it
	(file-eval path)))
    ))
;; (ti::a-zip-elisp-p "tinylib")

;;; ----------------------------------------------------------------------
;;;
;;;
(defadvice load (around ti::a-zip-load act)
  "Makes it possible to load compressed elisp code."
  (let* ((fn     (ad-get-arg 0))
	 (path   (ti::a-zip-elisp-p fn))
	 (this   'load)
	 )
    (if path
	(file-eval path)
      ad-do-it
      )
    ))

;;; ----------------------------------------------------------------------
;;;
(defadvice load-library (around ti::a-zip-load act)
  "Makes it possible to load compressed elisp code."
  (interactive "slibrary: ")

  (let* (path)
    (setq path (ti::a-zip-elisp-p (ad-get-arg 0)))
    (if path
	(file-eval path)
      ad-do-it
      )
   ))


;;; ----------------------------------------------------------------------
;;; (ad-unadvise 'load)
;;;
(defadvice load-file (around ti::a-zip-load (file) act)
  "Makes it possible to load compressed elisp code."
  (interactive
   (list
    (read-file-name "Load file: " default-directory )))

  (let* (path)
    (setq path (ti::a-zip-elisp-p (ad-get-arg 0)))
    (if path
	(file-eval path)
      ad-do-it
      )
    ))

;;}}}

;;{{{ example setup

;;; ......................................................... &example ...
;;; ** WARNIG ** DO NOT USE THIS IF YOU DON'T UNDERSTAND IT
;;; ** WARNIG ** you may loose your files, try it on ~/tmp/ first

;;; Requirements before you use these functions
;;; ........................................
;;; - You need tinylibXXX.el files, because it uses library functions
;;; - Code can be easily extracted with function tinylib.el/ti::m-pkg-rip-magic

;;; Short intro
;;; ........................................
;;; - Okay, these are the functions I use to automatically compress
;;;   files everytime I start emacs in my university account.
;;; - The function (uta) detects if I'm running inside (system-name)
;;;   kielo.uta.fi  xxx.uta.fi ..., that is, one of my  University's machines
;;;
;;; - I put single command in my emacs: (my-compress-household), and all
;;;   runs automatically.
;;; - The reason I have autocompression is, that I "shadow" all my
;;;   files in my University account. I do not edit files there, instead I do
;;;   all my editing at work, and let the shadow.el (19.28 std distrib.)
;;;   to transfer all files, I have declared to be shadowed, to University
;;;
;;;   Now everytime I change specific .el file at work, it's automatically
;;;   copied to University. Naturally the copied file is in .el form,
;;;   not in .el.gz form what I would want.
;;;
;;;   But when I start my University's emacs, it check if there are newer
;;;   .el files and compresses them into .el.gz. it will also remove
;;;   all old files.
;;;
;;; ** ONCE MORE ** Do not use this if it's not clear what it does
;;;


;;* _
;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun my-compress-household ()
;;*   "updates shadowed elisp files."
;;*   (interactive)
;;*   (cond
;;*    ((uta)
;;*     (my-file-compress "~/elisp/" ".*el$")
;;*     ))
;;*   )
;;* _
;;* _
;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun my-file-compress (dir re &optional not-re)
;;*   "Compresses files in direcotry DIR and files matching RE.
;;* _
;;* NOT-RE excludes files.
;;* _
;;* Drops away files that are read-only, backups [~#] are automatically
;;* excluded. Deletes old files when doing compression.
;;* _
;;* If file contains backup file #file#, it will not be compressed.
;;* _
;;* "
;;*   (interactive "ddir: \nsre: ")
;;*   (let* ((files   (directory-files dir t re))
;;* 	 (files   (if (null not-re)	;exclude files
;;* 		      files
;;* 		    (ti::l-get-match files not-re 'not)))
;;* _
;;* 	 ;;   Do not touch these files !
;;* 	 (not-touch  "g?[Z#~]$")
;;* 	 (files   (ti::l-get-match files not-touch 'not))
;;* _
;;* 	 (ext     ".gz")		;gzip used for compression
;;* 	 (bp      (generate-new-buffer "*tmp*"))
;;* 	 (bpn     (buffer-name bp))
;;* 	 (obp     (current-buffer))
;;* _
;;* 	 fn-el fn-gz
;;* 	 bup				;backup ?
;;* 	)
;;*     (require 'jka-compr)
;;*     (set-buffer bpn)			;avoid set-excursion indentation
;;* _
;;* _
;;*     (while files
;;*       (setq fn-el (car files) )
;;*       (setq fn-gz (concat fn-el ext))
;;*       (erase-buffer)
;;* _
;;* ;;;      (d! fn-el fn-gz)
;;* _
;;* _
;;*       ;;  Check some things first, do not touch #files#
;;*       ;;  And those who are read-only
;;* _
;;*       (setq bup
;;* 	    (concat
;;* 	     (file-name-directory fn-el)
;;* 	     "#"
;;* 	     (file-name-nondirectory fn-el)
;;* 	     "#"))
;;* _
;;* _
;;*       (if (or (file-exists-p bup)
;;* 	      (and (file-exists-p  fn-el)
;;* 		   (not (file-writable-p fn-el)))
;;* 	      (and (file-exists-p  fn-gz)
;;* 		   (not (file-writable-p fn-gz)))
;;* 	      )
;;* 	  (message (concat "Skipped ..." fn-el))
;;* 	(cond
;;* 	 ((and (file-exists-p fn-el)	;.el and el.gz
;;* 	       (file-exists-p fn-gz)
;;* 	       (file-newer-than-file-p fn-el fn-gz);; is .el newer ?
;;* 	       )
;;* 	  (delete-file fn-gz)
;;* 	  (insert-file-contents fn-el)
;;* 	  (write-file fn-gz)
;;* 	  (rename-buffer bpn)
;;* 	  )
;;* 	 ((and (file-exists-p fn-el)
;;* 	       (file-exists-p fn-gz)	;.el is older ...
;;* 	       )
;;* 	  (delete-file fn-el)
;;* 	  )
;;* 	 ((file-exists-p fn-el)
;;* 	  (insert-file-contents fn-el)
;;* 	  (write-file fn-gz)
;;* 	  (rename-buffer bpn)
;;* 	  (delete-file fn-el)
;;* 	  )
;;* 	 (t
;;* 	  nil)				;only .gz left
;;* 	 ))
;;* _
;;*       (NEXTP files))
;;* _
;;*     (kill-buffer bpn)
;;*     (set-buffer obp)			;restore
;;*     ))

;;}}}


(tinyezip-pre-command-install)		; Final touch... activate kbd checks

(provide   'tinyezip)
(run-hooks 'tinyezip-load-hook)

;;; ............................................................. &eof ...
