;;; printing.el --- Printing utilities.

;; Copyright (C) 2000 Vinicius Jose Latorre

;; Author:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Maintainer:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Keywords:	wp, print, PostScript
;; Time-stamp:	<2000/06/17 20:03:00 vinicius>
;; Version:	3.2

(defconst pr-version "3.2"
  "printing.el, v 3.2 <2000/06/17 vinicius>

Please send all bug fixes and enhancements to
	Vinicius Jose Latorre <vinicius@cpqd.com.br>
")

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package provides some printing utilities that includes
;; previewing/printing a PostScript file, printing a text file and
;; previewing/printing some major modes (like mh-folder-mode,
;; rmail-summary-mode, gnus-summary-mode, etc).
;;
;; `printing' was inspired on:
;;
;;    print-nt.el	   Frederic Corne <frederic.corne@erli.fr>
;;	 Special printing functions for Windows NT
;;
;;    mh-e-init.el	   Tom Vogels <tov@ece.cmu.edu>
;;	 PS-print for mail messages
;;
;;    win32-ps-print.el	   Matthew O. Persico <mpersico@erols.com>
;;	 PostScript printing with ghostscript
;;
;; `printing' is prepared to run on Unix and NT systems.
;; On Unix system, `printing' depends on gs and gv utilities.
;; On NT system, `printing' depends on gstools (gswin32.exe and gsview32.exe).
;; To obtain ghostscript, ghostview and GSview see the URL
;; `http://www.cs.wisc.edu/~ghost/'.
;;
;; `printing' also depends on ps-print and lpr GNU Emacs packages.
;; To download the latest ps-print package see
;; `http://www.cpqd.com.br/~vinicius/emacs/ps-print.tar.gz'.
;; Please, see README file for ps-print installation instructions.
;;
;;
;; Using `printing'
;; ----------------
;;
;; To use `printing' insert in your ~/.emacs file (or c:/_emacs, if you're
;; using Windows 95/98/NT or MS-DOS):
;;
;;    (require 'printing)
;;
;; When `printing' is loaded, it replaces the Tools/Print menu by
;; Tools/Printing menu.  Please, see section Menu Layout below for menu
;; explanation.
;;
;; To use `printing' utilities you can use the Printing menu options, type M-x
;; followed by one of the commands below, or type a key associated with the
;; command you want (if there is a key binding).
;;
;; `printing' has the following commands:
;;
;;    pr-ps-buffer-preview
;;    pr-ps-buffer-using-ghostscript
;;    pr-ps-buffer-print
;;    pr-ps-buffer-ps-print
;;    pr-ps-region-preview
;;    pr-ps-region-using-ghostscript
;;    pr-ps-region-print
;;    pr-ps-region-ps-print
;;    pr-ps-mode-preview
;;    pr-ps-mode-using-ghostscript
;;    pr-ps-mode-print
;;    pr-ps-mode-ps-print
;;    pr-ps-file-preview
;;    pr-ps-file-up-preview
;;    pr-ps-file-using-ghostscript
;;    pr-ps-file-print
;;    pr-ps-file-ps-print
;;    pr-ps-file-up-ps-print
;;    pr-ps-fast-fire
;;    pr-despool-preview
;;    pr-despool-using-ghostscript
;;    pr-despool-print
;;    pr-despool-ps-print
;;    pr-printify-buffer
;;    pr-printify-region
;;    pr-txt-buffer
;;    pr-txt-region
;;    pr-txt-mode
;;    pr-txt-fast-fire
;;    pr-toggle-file-duplex
;;    pr-toggle-file-tumble
;;    pr-toggle-file-landscape
;;    pr-toggle-ghostscript
;;    pr-toggle-faces
;;    pr-toggle-spool
;;    pr-toggle-duplex
;;    pr-toggle-tumble
;;    pr-toggle-landscape
;;    pr-toggle-upside-down
;;    pr-toggle-line
;;    pr-toggle-zebra
;;    pr-toggle-header
;;    pr-toggle-lock
;;    pr-toggle-region
;;    pr-toggle-mode
;;    pr-customize
;;    lpr-customize
;;    pr-help
;;    pr-ps-name
;;    pr-txt-name
;;    pr-ps-utility
;;    pr-show-ps-setup
;;    pr-show-pr-setup
;;    pr-show-lpr-setup
;;
;; The general meanings of above commands are:
;;
;;    PREFIX:
;;    `pr-help'		help for printing package.
;;    `pr-ps-name'	select interactively a PostScript printer.
;;    `pr-txt-name'	select interactively a text printer.
;;    `pr-ps-utility'	select interactively a PostScript utility.
;;    `pr-show-*-setup'	show current settings.
;;    `pr-ps-*'		deal with PostScript code generation.
;;    `pr-txt-*'	deal with text generation.
;;    `pr-toggle-*'	toggle on/off some boolean variable.
;;    `pr-despool-*'	despool the PostScript spooling buffer.
;;    `pr-printify-*'	replace nonprintable ASCII by printable ASCII
;;			representation.
;;
;;    SUFFIX:
;;    `*-customize'		customization.
;;    `*-preview'		preview a PostScript file.
;;    `*-using-ghostscript'	use ghostscript to print.
;;    `*-fast-fire'		fast fire command (see it for documentation).
;;    `*-print'			send PostScript directly to printer.
;;    `*-ps-print'		send PostScript directly to printer or use
;;				ghostscript to print.  It depends on
;;				`pr-print-using-ghostscript' option.
;;
;;    INFIX/SUFFIX:
;;    `*-buffer*'	process a buffer.
;;    `*-region*'	process a region.
;;    `*-mode*'		process a major mode (see explanation below).
;;    `*-file-*'	process a PostScript file.
;;    `*-file-up-*'	process a PostScript file using a filter utility.
;;
;; Here are some examples:
;;
;;    `pr-ps-buffer-using-ghostscript'
;;	 Use ghostscript to print a buffer.
;;
;;    `pr-ps-file-print'
;;	 Print a PostScript file.
;;
;;    `pr-toggle-spool'
;;	 Toggle spooling buffer.
;;
;; So you can preview through ghostview, use ghostscript to print (if you don't
;; have a PostScript printer) or send directly to printer a PostScript code
;; generated by `ps-print' package.
;;
;; Besides operating one buffer or region each time, you also can postpone
;; previewing or printing by saving the PostScript code generated in a
;; temporary Emacs buffer.  This way you can save banner pages between
;; successive printing.  You can toggle on/off spooling by invoking
;; `pr-toggle-spool' interactively or through menu bar.
;;
;; If you type, for example:
;;
;;    C-u M-x pr-ps-buffer-print RET
;;
;; The `pr-ps-buffer-print' command prompts you for a n-up printing number and
;; a file name, and save the PostScript code generated to the file name instead
;; of sending to printer.
;;
;; This behavior is similar with the commands that deal with PostScript code
;; generation, that is, with `pr-ps-*' and `pr-despool-*' commands.  If
;; spooling is on, only `pr-despool-*' commands prompt for a file name and save
;; the PostScript code spooled in this file.
;;
;; `printing' has also a special way to handle some major mode through
;; `*-mode*' commands.  So it's possible to customize a major mode printing,
;; it's only needed to declared the customization in `pr-mode-alist' (see
;; section Options) and invoke some of `*-mode*' commands.  An example for
;; major mode usage is when you're using gnus (or mh, or rmail, etc.) and
;; you're in the *Summary* buffer, if you forget to switch to the *Article*
;; buffer before printing, you'll get a nicely formatted list of article
;; subjects shows up at the printer.  With major mode printing you don't need
;; to switch from gnus *Summary* buffer first.
;;
;; Current global keyboard mapping for GNU Emacs is:
;;
;;    (global-set-key [print]   'pr-ps-fast-fire)
;;    (global-set-key [M-print] 'pr-ps-mode-using-ghostscript)
;;    (global-set-key [C-print] 'pr-txt-fast-fire)
;;
;; And for XEmacs is:
;;
;;    (global-set-key 'f22           'pr-ps-fast-fire)
;;    (global-set-key '(meta f22)    'pr-ps-mode-using-ghostscript)
;;    (global-set-key '(control f22) 'pr-txt-fast-fire)
;;
;; As a suggestion of global keyboard mapping for some `printing' commands:
;;
;;    (global-set-key "\C-cbp" 'pr-ps-buffer-print)
;;    (global-set-key "\C-cbx" 'pr-ps-buffer-preview)
;;    (global-set-key "\C-cbb" 'pr-ps-buffer-using-ghostscript)
;;    (global-set-key "\C-crp" 'pr-ps-region-print)
;;    (global-set-key "\C-crx" 'pr-ps-region-preview)
;;    (global-set-key "\C-crr" 'pr-ps-region-using-ghostscript)
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of `printing' options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `pr-txt-name'		Specify a printer for printing a text file.
;;
;; `pr-txt-printer-alist'	Specify an alist of all text printers.
;;
;; `pr-ps-name'			Specify a printer for printing a PostScript
;;				file.
;;
;; `pr-ps-printer-alist'	Specify an alist for all PostScript printers.
;;
;; `pr-temp-dir'		Specify a directory for temporary files during
;;				printing.
;;
;; `pr-ps-temp-file'		Specify PostScript temporary file name.
;;
;; `pr-gv-command'		Specify path and name of gsview program.
;;
;; `pr-gs-command'		Specify path and name of ghostscript program.
;;
;; `pr-gs-switches'		Specify ghostscript switches.
;;
;; `pr-gs-device'		Specify ghostscript device switch value.
;;
;; `pr-gs-resolution'		Specify ghostscript resolution switch value.
;;
;; `pr-print-using-ghostscript'	Non-nil means print using ghostscript.
;;
;; `pr-faces-p'			Non-nil means print with face attributes.
;;
;; `pr-spool-p'			Non-nil means spool printing in a buffer.
;;
;; `pr-file-landscape'		Non-nil means print PostScript file in
;;				landscape orientation.
;;
;; `pr-file-duplex'		Non-nil means print PostScript file in duplex
;;				mode.
;;
;; `pr-file-tumble'		Non-nil means print PostScript file in tumble
;;				mode.
;;
;; `pr-auto-region'		Non-nil means region is automagically detected.
;;
;; `pr-auto-mode'		Non-nil means major-mode printing is prefered
;;				over normal printing.
;;
;; `pr-mode-alist'		Specify an alist for a major-mode and printing
;;				function.
;;
;; `pr-ps-utility'		Specify PostScript utility processing.
;;
;; `pr-ps-utility-alist'	Specify an alist for PostScript utility
;;				processing.
;;
;; `pr-menu-lock'		Non-nil means menu is locked while selecting
;;				toggle options.
;;
;; `pr-menu-char-height'	Specify menu char height in pixels.
;;
;; `pr-menu-char-width'		Specify menu char width in pixels.
;;
;; `pr-setting-database'	Specify an alist for settings in general.
;;
;; `pr-visible-entry-list'	Specify a list of Printing menu visible
;;				entries.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq pr-faces-p t)
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET pr-faces-p RET t RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *PostScript* group,
;;	 expand *Printing* group
;;	 and then customize `printing' options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v pr-faces-p RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; e) or invoke:
;;
;;	 M-x pr-customize RET
;;
;;    and then customize `printing' options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; f) or use menu bar, for example:
;;	 click on menu-bar *Tools* option,
;;	 then click on *Printing*,
;;	 then click on *Customize*,
;;	 then click on *printing*
;;	 and then customize `printing' options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; Menu Layout
;; -----------
;;
;; The `printing' menu (Tools/Printing) has the following layout:
;;
;;        +------------------------------+       +-A--------+     +-B------+
;; I   1  |    PostScript Preview       >|-------|Buffer   >|-----|1-up    |
;;     2  |    PostScript Print         >|---- A |Region   >|-- B |2-up    |
;;     3  |    PostScript Printer: name >|---- C |Mode     >|-- B |4-up    |
;;        +------------------------------+       |File     >|-\   |Other...|
;; II  4  |    Printify                 >|-----\ |Despool...| |   +--------+
;;     5  |    Print                    >|---\ | +----------+ |
;;     6  |    Text Printer: name       >|-\ | | +------+     |
;;        +------------------------------+ | | \-|Buffer| /---/
;; III 7  |[ ] Landscape                 | | |   |Region| |
;;     8  |[ ] Print Header              | | |   +------+ | +-------------+
;;     9  |[ ] Print Header Frame        | | |   +------+ \-|    As Is... | Ia
;;     10 |[ ] Line Number               | | \---|Buffer|   +-------------+ Ib
;;     11 |[ ] Zebra Stripes             | |     |Region|   |    name    >|- C
;;     12 |[ ] Duplex                    | |     |Mode  |   +-------------+
;;     13 |[ ] Tumble                    | |     +------+   |    1-up...  | Ic
;;     14 |[ ] Upside-Down               | | +-C--------+   |    2-up...  |
;;        +------------------------------+ \-|( ) name A|   |    4-up...  |
;; IV  15 |[ ] Spool Buffer              |   |( ) name B|   |    Other... |
;;     16 |[ ] Print with-faces          |   |...       |   +-------------+
;;     17 |[ ] Print Using Ghostscript   |   |(*) name  |   |[ ] Landscape| Id
;;        +------------------------------+   |...       |   |[ ] Duplex   | Ie
;; V   18 |[ ] Auto Region               |   +----------+   |[ ] Tumble   | If
;;     19 |[ ] Auto Mode                 |                  +-------------+
;;     20 |[ ] Menu Lock                 |
;;        +------------------------------+     +-D------+
;; VI  21 |    Customize                >|-----|printing|
;;     22 |    Show Settings            >|-- D |ps-print|
;;     23 |    Help                      |     |lpr     |
;;        +------------------------------+     +--------+
;;
;; See `pr-visible-entry-list' for hiding some parts of the menu.
;;
;; The menu has the following sections:
;;
;; I. PostScript printing:
;;
;;    1. You can generate a PostScript file (if you type C-u before activating
;;	 menu) or PostScript temporary file for a buffer, a region or a major
;;	 mode, choosing 1-up, 2-up, 4-up or any other n-up printing; after file
;;	 generation, ghostview is activated using the file generated as
;;	 argument.  This option is disabled if spooling is on (option 15).
;;	 Also, if you already have a PostScript file you can preview it.
;;	 Instead of previewing each buffer, region or major mode at once, you
;;	 can save temporarily the PostScript code generated in a buffer and
;;	 preview it later.  The option `Despool...' despools the PostScript
;;	 spooling buffer in a temporary file and uses ghostview to preview it.
;;	 If you type C-u before choosing this option, the PostScript code
;;	 generated is saved in a file instead of saving in a temporary file.
;;	 To spool the PostScript code generated you need to turn on the option
;;	 15.  The option `Despool...' is enabled if spooling is on (option
;;	 15).
;;
;;	 NOTE 1: It's possible to customize a major mode printing, just declare
;;		 the customization in `pr-mode-alist' and invoke some of
;;		 `*-mode*' commands or select Mode option in Printing menu.  An
;;		 example for major mode usage is when you're using gnus (or mh,
;;		 or rmail, etc.) and you're in the *Summary* buffer, if you
;;		 forget to switch to the *Article* buffer before printing,
;;		 you'll get a nicely formatted list of article subjects shows
;;		 up at the printer.  With major mode printing you don't need to
;;		 switch from gnus *Summary* buffer first.
;;
;;	 NOTE 2: There are the following options for PostScript file
;;		 processing:
;;		 Ia. Print the file *as is*, that is, send it directly to
;;		     PostScript printer.
;;		 Ib. PostScript utility processing selection.
;;		     See `pr-ps-utility-alist' and `pr-setting-database' for
;;		     documentation.
;;		 Ic. Do n-up processing before printing.
;;		 Id. Toggle on/off landscape for PostScript file processing.
;;		 Ie. Toggle on/off duplex for PostScript file processing.
;;		 If. Toggle on/off tumble for PostScript file processing.
;;
;;	 NOTE 3: Don't forget to download and install the utilities declared on
;;		 `pr-ps-utility-alist'.
;;
;;    2. Operate the same way as option 1, but it sends directly the PostScript
;;	 code (or put in a file, if you've typed C-u) or it uses ghostscript to
;;	 print the PostScript file generated.  It depends on option 17, if it's
;;	 turned on, it uses ghostscript; otherwise, it sends directly to
;;	 printer.  If spooling is on (option 15), the PostScript code is saved
;;	 temporarily in a buffer instead of printing it or saving it in a file.
;;	 Also, if you already have a PostScript file you can print it.  Instead
;;	 of printing each buffer, region or major mode at once, you can save
;;	 temporarily the PostScript code generated in a buffer and print it
;;	 later.  The option `Despool...' despools the PostScript spooling
;;	 buffer directly on a printer.  If you type C-u before choosing this
;;	 option, the PostScript code generated is saved in a file instead of
;;	 sending to printer.  To spool the PostScript code generated you need
;;	 to turn on the option 15.  This option is enabled if spooling is on
;;	 (option 15).  See also the NOTE 1, NOTE 2 and NOTE 3 on option 1.
;;
;;    3. You can select a new PostScript printer to send PostScript code
;;	 generated.  For selection it's used all PostScript printers defined
;;	 in `pr-ps-printer-alist' variable (see it for documentation).
;;	 See also `pr-setting-database'.
;;
;; II. Text printing:
;;
;;    4. If you have control characters (character code from \000 to \037) in a
;;	 buffer and you want to print them in a text printer, select this
;;	 option.  All control characters in your buffer or region will be
;;	 replaced by a printable representation.  The printable representations
;;	 use ^ (for ASCII control characters) or hex.  The characters tab,
;;	 linefeed, space, return and formfeed are not affected.  You don't need
;;	 to select this option if you use any option of section I, the
;;	 PostScript engine treats control characters properly.
;;
;;    5. If you want to print a buffer, region or major mode in a text printer,
;;	 select this option.  See also the NOTE 1 on option 1.
;;
;;    6. You can select a new text printer to send text generated.  For
;;	 selection it's used all text printers defined in
;;	 `pr-txt-printer-alist' variable (see it for documentation).
;;	 See also `pr-setting-database'.
;;
;; III. PostScript page toggle options:
;;
;;    7. If you want a PostScript landscape printing, turn on this option.
;;
;;    8. If you want to have a header in each page in your PostScript code,
;;	 turn on this option.
;;
;;    9. If you want to draw a gaudy frame around the header, turn on this
;;	 option.  This option is enabled if print header is on (option 8).
;;
;;    10. If you want that the line number is printed in your PostScript code,
;;	  turn on this option.
;;
;;    11. If you want background zebra stripes in your PostScript code, turn on
;;	  this option.
;;
;;    12. If you want a duplex printing and your PostScript printer has this
;;	  feature, turn on this option.
;;
;;    13. If you turned on duplex printing, you can choose if you want to have
;;	  a printing suitable for binding on the left or right (tumble off), or
;;	  to have a printing suitable for binding at top or bottom (tumble on).
;;	  This option is enabled if duplex is on (option 12).
;;
;;    14. If you want a PostScript upside-down printing, turn on this option.
;;
;; IV. PostScript processing toggle options:
;;
;;    15. If you want to spool the PostScript code generated, turn on this
;;	  option.  To spool the PostScript code generated use option 2.  You
;;	  can despool later by choosing option 1 or 2, sub-option `Despool...'.
;;
;;    16. If you use colors in your buffers and want to see these colors on
;;	  your PostScript code generated, turn on this option.  If you have a
;;	  black/white PostScript printer, these colors are displayed in gray
;;	  scale by PostScript printer interpreter.
;;
;;    17. If you don't have a PostScript printer to send PostScript files, turn
;;	  on this option.  When this option is on, the ghostscript is used to
;;	  print PostScript files.  In a Unix system, if ghostscript is set as a
;;	  PostScript filter, you don't need to turn on this option.
;;
;; V. Printing customization:
;;
;;    18. If you want that region is automagically detected, turn on this
;;	  option.  Note that this will only work if you're using transient mark
;;	  mode.  When this option is on, the `*-buffer*' commands will behave
;;	  like `*-region*' commands, that is, `*-buffer*' commands will print
;;	  only the region marked instead of all buffer.
;;
;;    19. Turn this option on if you want that when current major-mode is
;;	  declared in `pr-mode-alist', the `*-buffer*' and `*-region*' commands
;;	  behave like `*-mode*' commands.
;;
;;    20. If you want that Printing menu stays poped up while you are setting
;;	  toggle options, turn on this option.  The variables
;;	  `pr-menu-char-height' and `pr-menu-char-width' are used to guess the
;;	  menu position, so don't forget to adjust these variables if menu
;;	  position is not ok.
;;
;; VI. Customization:
;;
;;    21. Besides all options in section III, IV and V, you can customize much
;;	  more PostScript options in `ps-print' option.  Or you can customize
;;	  some `lpr' options for text printing.  Or customize `printing'
;;	  options.
;;
;;    22. Show current settings for `printing', `ps-print' or `lpr'.
;;
;;    23. Quick help for printing menu layout.
;;
;;
;; Option Settings
;; ---------------
;;
;; Below it's shown only the main options that affect all `printing' package.
;; Check all the settings below *BEFORE* running `printing' commands.
;;
;; * Example of setting for Unix system:
;;
;;    (require 'printing)
;;    (setq pr-txt-name      'prt_06a)
;;    (setq pr-txt-printer-alist
;;          '((prt_06a "lpr" nil "prt_06a")
;;            (prt_07c "lpr" nil "prt_07c")
;;            ))
;;    (setq pr-ps-name       'lps_06b)
;;    (setq pr-ps-printer-alist
;;          '((lps_06b "lpr" nil "-P " "lps_06b")
;;            (lps_07c "lpr" nil "-P " "lps_07c")
;;            ))
;;    (setq pr-temp-dir      "/tmp/")
;;    (setq pr-gv-command    "gv")
;;    (setq pr-gs-command    "gs")
;;    (setq pr-gs-switches '("-q -dNOPAUSE -I/usr/share/ghostscript/5.10"))
;;    (setq pr-gs-device     "uniprint")
;;    (setq pr-gs-resolution 300)
;;    (setq pr-ps-utility    'mpage)
;;    (setq pr-ps-utility-alist
;;	    '((mpage "mpage"    "-b%s" "-%d" "-l" "-t" "-T" ">" nil)
;;	      (psnup "psnup -q" "-P%s" "-%d" "-l" nil  nil  " " nil
;;		     (:inherits-from . no-duplex))
;;	      ))
;;    (setq pr-setting-database
;;	    '((no-duplex
;;	       nil nil nil
;;	       (pr-file-duplex . nil)
;;	       (pr-file-tumble . nil))
;;	      ))
;;    (pr-update-menus t)		; update now printer and utility menus
;;
;; * Example of setting for Windows system:
;;
;;    (require 'printing)
;;    (setq pr-txt-name      'prt_06a)
;;    (setq pr-txt-printer-alist
;;          '((prt_06a  "print"     nil "/D:\\\\printers\\prt_06a")
;;            (prt_07c  "print"     nil "/D:\\\\printers\\prt_07c")
;;            (standard "redpr.exe" nil "")
;;            ))
;;    (setq pr-ps-name       'lps_06b)
;;    (setq pr-ps-printer-alist
;;          '((lps_06b  "print"     nil "/D:" "\\\\printers\\lps_06b")
;;            (lps_07c  "print"     nil nil   "/D:\\\\printers\\lps_07c")
;;            (standard "redpr.exe" nil nil   "")
;;            ))
;;    (setq pr-temp-dir      "C:/WINDOWS/TEMP/")
;;    (setq pr-gv-command    "c:/gs/gsview/gsview32.exe")
;;    (setq pr-gs-command    "c:/gs/gswin32.exe")
;;    (setq pr-gs-switches '("-q -dNOPAUSE -Ic:/gs/gs5.50;c:/gs/gs5.50/fonts"))
;;    (setq pr-gs-device     "mswinpr2")
;;    (setq pr-gs-resolution 300)
;;    (setq pr-ps-utility    'psnup)
;;    (setq pr-ps-utility-alist
;;	    '((psnup "c:/psutils/psnup -q" "-P%s" "-%d" "-l" nil  nil  " " nil
;;		     (:inherits-from . no-duplex))
;;	      ))
;;    (setq pr-setting-database
;;	    '((no-duplex
;;	       nil nil nil
;;	       (pr-file-duplex . nil)
;;	       (pr-file-tumble . nil))
;;	      ))
;;    (pr-update-menus t)		; update now printer and utility menus
;;
;; NOTE: Don't forget to download and install the utilities declared on
;;	 `pr-ps-utility-alist'.
;;
;;
;; Utilities
;; ---------
;;
;; `printing' package has the following utilities:
;;
;;    `pr-setup'	Return the current `printing' setup.
;;
;;    `lpr-setup'	Return the current `lpr' setup.
;;
;;    `pr-update-menus'	Update utility, PostScript and text printer menus.
;;
;; Below are some URL where you can find good utilities.
;;
;; * For `printing' package:
;;
;;    printing	`http://www.cpqd.com.br/~vinicius/emacs/printing.el.gz'
;;    ps-print	`http://www.cpqd.com.br/~vinicius/emacs/ps-print.tar.gz'
;;
;; * For Unix system:
;;
;;    gs, gv		`http://www.cs.wisc.edu/~ghost/'
;;    enscript		`http://people.ssh.fi/mtr/genscript/'
;;    psnup		`http://www.dcs.ed.ac.uk/home/ajcd/psutils/index.html'
;;    mpage		`http://www.mesa.nl/pub/mpage/'
;;
;; * For Windows system:
;;
;;    gswin32, gsview32	`http://www.cs.wisc.edu/~ghost/'
;;    enscript		`http://people.ssh.fi/mtr/genscript/'
;;    psnup		`http://www.dcs.ed.ac.uk/home/ajcd/psutils/index.html'
;;    redmon		`http://www.cs.wisc.edu/~ghost/redmon/'
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Fred Labrosse <f.labrosse@maths.bath.ac.uk> for XEmacs tests.
;;
;; Thanks to Klaus Berndl <klaus.berndl@sdm.de> for invaluable help/debugging
;; and for suggestions:
;;    - ghostscript parameters for `pr-ps-printer-alist'.
;;    - default printer name.
;;    - completion functions.
;;    - automagic region detection.
;;    - menu entry hiding.
;;    - fast fire PostScript printing command.
;;
;; Thanks to Kim F. Storm <storm@filanet.dk> for beta-test and for suggestions:
;;    - PostScritpt Print and PostScript Print Preview merge.
;;    - Tools/Printing menu.
;;    - replace *-using-preview by *-using-ghostscript.
;;    - printer selection.
;;    - extra parameters for `pr-ps-printer-alist'.
;;
;; Thanks to:
;;    Frederic Corne <frederic.corne@erli.fr>	print-nt.el
;;    Tom Vogels <tov@ece.cmu.edu>		mh-e-init.el
;;    Matthew O. Persico <mpersico@erols.com>	win32-ps-print.el
;; And to all people who contributed with them.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'lpr)
(require 'ps-print)


(and (string< ps-print-version "5.2")
     (error "`printing' requires `ps-print' package version 5.2 or later."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To avoid compilation gripes


(eval-when-compile
  (require 'rmail)
  (require 'mh-e)
  (require 'gnus))

;; function definition
(mapcar #'(lambda (sym)
	    (or (fboundp sym)
		(defalias sym 'ignore)))
	'(;; GNU Emacs
	  force-mode-line-update
	  frame-char-height
	  frame-char-width
	  mouse-pixel-position
	  ;; XEmacs
	  add-submenu
	  event-function
	  event-object
	  find-menu-item
	  font-height
	  font-width
	  get-popup-menu-response
	  make-event
	  misc-user-event-p
	  region-active-p
	  relabel-menu-item
	  set-menubar-dirty-flag
	  event-x-pixel
	  event-y-pixel))

;; variable definition
;; GNU Emacs
(defvar mark-active nil)
;; XEmacs
(defvar current-menubar     nil)
(defvar current-mouse-event nil)
(defvar zmacs-region-stays  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To avoid loading or compilation gripes (real definitions are forward)


(defun pr-do-update-menus (&optional force))
(defun pr-menu-set-utility-title (value &optional item entry index))
(defun pr-menu-set-ps-title (value &optional item entry index))
(defun pr-menu-set-txt-title (value &optional item entry index))
(defun pr-menu-alist (alist var-sym fun menu-path modified-sym force name
			    entry index))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Functions (I)


(defun pr-dosify-path (path)
  "Replace unix-style directory separator character with dos/windows one."
  (interactive "sPath: ")
  (if ps-windows-system
      (subst-char-in-string ?/ ?\\ path)
    path))


(defun pr-unixify-path (path)
  "Replace dos-style directory separator character with unix one."
  (interactive "sPath: ")
  (if ps-windows-system
      (subst-char-in-string ?\\ ?/ path)
    path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printer & Utility Selection (I)


;; The definition is here to avoid multidefinition message when compiling.
(defun pr-update-menus (&optional force)
  "Update utility, PostScript and text printer menus.

If FORCE is non-nil, update menus doesn't matter if `pr-ps-printer-alist',
`pr-txt-printer-alist' or `pr-ps-utility-alist' were modified or not;
otherwise, update PostScript printer menu iff `pr-ps-printer-menu-modified' is
non-nil, update text printer menu iff `pr-txt-printer-menu-modified' is
non-nil, and update PostScript File menus iff `pr-ps-utility-menu-modified' is
non-nil."
  (interactive)
  (pr-do-update-menus force))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Functions


(defun pr-alist-custom-set (symbol value)
  "Set the value of custom variables for printer & utility selection."
  (set symbol value)
  (pr-update-menus t))


(defun pr-ps-utility-custom-set (symbol value)
  "Update utility menu entry."
  (set symbol value)
  (pr-menu-set-utility-title value))


(defun pr-ps-name-custom-set (symbol value)
  "Update `PostScript Printer:' menu entry."
  (set symbol value)
  (pr-menu-set-ps-title value))


(defun pr-txt-name-custom-set (symbol value)
  "Update `Text Printer:' menu entry."
  (set symbol value)
  (pr-menu-set-txt-title value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Interface


(defgroup printing nil
  "Printing Utilities group"
  :tag "Printing Utilities"
  :link '(emacs-library-link :tag "Source Lisp File" "printing.el")
  :prefix "pr-"
  :group 'wp
  :group 'postscript)


(defcustom pr-txt-name 'default
  "*Specify a printer for printing a text file.

The printer name symbol should be defined on `pr-txt-printer-alist' (see it for
documentation).

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update text printer menu."
  :type 'symbol
  :set 'pr-txt-name-custom-set
  :group 'printing)


(defcustom pr-txt-printer-alist
  (list (list 'default
	      (if ps-windows-system
		  "print"
		lpr-command)
	      nil
	      (cond ((boundp 'printer-name) printer-name)
		    (ps-windows-system "PRN:")
		    (t nil)
		    )))
  ;; Examples:
  ;; * On Unix system:
  ;;    '((prt_06a "lpr" nil "prt_06a")
  ;;      (prt_07c "lpr" nil "prt_07c")
  ;;      )
  ;; * On Windows system:
  ;;    '((prt_06a  "print"     nil "/D:\\\\printers\\prt_06a")
  ;;      (prt_07c  "print"     nil "/D:\\\\printers\\prt_07c")
  ;;      (standard "redpr.exe" nil "")
  ;;      )
  "*Specify an alist of all text printers (text printer database).

The alist element has the form:

   (SYMBOL COMMAND SWITCHES NAME)

Where:

SYMBOL		It's a symbol to identify a text printer.  It's for
		`pr-txt-name' variable setting and for menu selection.
		Examples:
			'prt_06a
			'my_printer

COMMAND		Name of program for printing a text file.  On MS-DOS and
		MS-Windows systems, if the value is an empty string then Emacs
		will write directly to the printer port named by NAME (see text
		below).  The programs `print' and `nprint' (the standard print
		programs on Windows NT and Novell Netware respectively) are
		handled specially, using NAME as the destination for output;
		any other program is treated like `lpr' except that an explicit
		filename is given as the last argument.
		If COMMAND is \"\", it's used the default printing program:
		`print' for Windows system, `lp' for lp system and `lpr' for
		all other systems.
		Examples:
			\"print\"
			\"lpr\"
			\"lp\"

SWITCHES	List of sexp's to pass as extra options for text printer
		program.  It is recommended to set NAME (see text below)
		instead of including an explicit switch on this list.
		Example:
		   . for lpr
			'(\"-#3\" \"-l\")
			nil

NAME		A string that specifies a text printer name.
		On Unix-like systems, a string value should be a name
		understood by lpr's -P option (or lp's -d option).
		On MS-DOS and MS-Windows systems, it is the name of a printer
		device or port.  Typical non-default settings would be \"LPT1\"
		to \"LPT3\" for parallel printers, or \"COM1\" to \"COM4\" or
		\"AUX\" for serial printers, or \"//hostname/printer\" (or
		\"/D://hostname/printer\")for a shared network printer.  You
		can also set it to a name of a file, in which case the output
		gets appended to that file.  If you want to discard the printed
		output, set this to \"NUL\".
		Examples:
		   . for print.exe
			\"/D://host/share-name\"
			\"/D:\\\\host\\share-name\"
			\"LPT1:\"

		   . for lpr or lp
			\"share-name\"

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update text printer menu."
  :type '(repeat :tag "Text Printer Alist"
		 (list :tag "Text Printer"
		       (symbol :tag "Printer Symbol Name")
		       (string :tag "Printer Command")
		       (repeat :tag "Printer Switches"
			       (sexp :tag "Switch"))
		       (string :tag "Printer Name")))
  :set 'pr-alist-custom-set
  :group 'printing)


(defcustom pr-ps-name 'default
  "*Specify a printer for printing a PostScript file.

This printer name symbol should be defined on `pr-ps-printer-alist' (see it for
documentation).

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update PostScript printer menu."
  :type 'symbol
  :set 'pr-ps-name-custom-set
  :group 'printing)


(defcustom pr-ps-printer-alist
  (let ((printer (or (getenv "PRINTER") (getenv "LPDEST") ps-printer-name)))
    (list
     (cond (ps-windows-system
	    (list 'default "print"     nil nil  printer))
	   (ps-lp-system
	    (list 'default lpr-command nil "-d " printer))
	   (t
	    (list 'default lpr-command nil "-P " printer))
	   )))
  ;; Examples:
  ;; * On Unix system:
  ;;    '((lps_06b "lpr" nil "-P " "lps_06b")
  ;;      (lps_07c "lpr" nil "-P " "lps_07c")
  ;;      )
  ;; * On Windows system:
  ;;    '((lps_06b  "print"     nil "/D:" "\\\\printers\\lps_06b")
  ;;      (lps_07c  "print"     nil nil   "/D:\\\\printers\\lps_07c")
  ;;      (standard "redpr.exe" nil nil   "")
  ;;      )
  "*Specify an alist for all PostScript printers (PostScript printer database).

The alist element has the form:

   (SYMBOL COMMAND SWITCHES PRINTER-SWITCH NAME DEFAULT...)

Where:

SYMBOL		It's a symbol to identify a PostScript printer.  It's for
		`pr-ps-name' variable setting and for menu selection.
		Examples:
			'prt_06a
			'my_printer

COMMAND		Name of program for printing a PostScript file.  On MS-DOS and
		MS-Windows systems, if the value is an empty string then Emacs
		will write directly to the printer port named by NAME (see text
		below).  The programs `print' and `nprint' (the standard print
		programs on Windows NT and Novell Netware respectively) are
		handled specially, using NAME as the destination for output;
		any other program is treated like `lpr' except that an explicit
		filename is given as the last argument.
		If COMMAND is \"\", it's used the default printing program:
		`print' for Windows system, `lp' for lp system and `lpr' for
		all other systems.
		Examples:
			\"print\"
			\"lpr\"
			\"lp\"
			\"cp\"

SWITCHES	List of sexp's to pass as extra options for PostScript printer
		program.  It is recommended to set NAME (see text below)
		instead of including an explicit switch on this list.
		Example:
		   . for lpr
			'(\"-#3\" \"-l\")
			nil

PRINTER-SWITCH	A string that specifies PostScript printer name switch.  If
		it's necessary to have a space between PRINTER-SWITCH and NAME,
		it should be inserted at the end of PRINTER-SWITCH string.
		If PRINTER-SWITCH is \"\", it's used the default printer name
		switch: `/D:' for Windows system, `-d' for lp system and `-P'
		for all other systems.
		Examples:
		   . for lpr
			\"-P \"

		   . for lp
			\"-d \"

		   . for print.exe
			\"/D:\"

NAME		A string that specifies a PostScript printer name.
		On Unix-like systems, a string value should be a name
		understood by lpr's -P option (or lp's -d option).
		On MS-DOS and MS-Windows systems, it is the name of a printer
		device or port.  Typical non-default settings would be \"LPT1\"
		to \"LPT3\" for parallel printers, or \"COM1\" to \"COM4\" or
		\"AUX\" for serial printers, or \"//hostname/printer\" (or
		\"/D://hostname/printer\")for a shared network printer.  You
		can also set it to a name of a file, in which case the output
		gets appended to that file.  If you want to discard the printed
		output, set this to \"NUL\".
		Examples:
		   . for cp.exe
			\"//host/share-name\"
			\"\\\\host\\share-name\"

		   . for print.exe
			\"/D://host/share-name\"
			\"/D:\\\\host\\share-name\"
			\"\\\\host\\share-name\"
			\"LPT1:\"

		   . for lpr or lp
			\"share-name\"

DEFAULT		It's a way to set default values when this entry is selected.
		It's a cons like:

		   (VARIABLE . VALUE)

		That associates VARIABLE with VALUE.  when this entry is
		selected, it's executed the following command:

		   (set VARIABLE (eval VALUE))

		Note that VALUE can be any valid lisp expression.  So, don't
		forget to quote symbols and constant lists.
		If VARIABLE is the special keyword `:inherits-from', VALUE must
		be a symbol name setting defined in `pr-setting-database' from
		which the current setting inherits the context.  Take care with
		circular inheritance.
		Examples:
			'(ps-landscape-mode . nil)
			'(ps-spool-duplex . t)
			'(pr-gs-device . (my-gs-device t))

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update PostScript printer menu."
  :type '(repeat
	  :tag "PostScript Printer Alist"
	  (list
	   :tag "PostScript Printer"
	   (symbol :tag "PS Printer Symbol Name")
	   (string :tag "PS Printer Command")
	   (repeat :tag "PS Printer Switches"
		   (sexp :tag "Switch"))
	   (choice :menu-tag "PS Printer Name Switch"
		   :tag "PS Printer Name Switch"
		   (const :tag "None" nil)
		   string)
	   (string :tag "PS Printer Name")
	   (repeat
	    :inline t
	    (cons
	     :tag "Default Value"
	     (choice
	      :menu-tag "Variable"
	      :tag "Variable"
	      (const :tag "Landscape"              ps-landscape-mode)
	      (const :tag "Print Header"           ps-print-header)
	      (const :tag "Print Header Frame"     ps-print-header-frame)
	      (const :tag "Line Number"            ps-line-number)
	      (const :tag "Zebra Stripes"          ps-zebra-stripes)
	      (const :tag "Duplex"                 ps-spool-duplex)
	      (const :tag "Tumble"                 ps-spool-tumble)
	      (const :tag "Upside-Down"            ps-print-upside-down)
	      (const :tag "PS File Landscape"      pr-file-landscape)
	      (const :tag "PS File Duplex"         pr-file-duplex)
	      (const :tag "PS File Tumble"         pr-file-tumble)
	      (const :tag "Auto Region"            pr-auto-region)
	      (const :tag "Auto Mode"              pr-auto-mode)
	      (const :tag "Ghostscript Device"     pr-gs-device)
	      (const :tag "Ghostscript Resolution" pr-gs-resolution)
	      (const :tag ":inherits-from"         :inherits-from)
	      (variable :tag "Other"))
	     (sexp :tag "Value")))
	   ))
  :set 'pr-alist-custom-set
  :group 'printing)


(defcustom pr-temp-dir
  (let ((tmp (or (getenv "TMP") (getenv "TEMP"))))
    (if ps-windows-system
	(pr-dosify-path (concat (or tmp "c:") "/"))
      (or tmp "/tmp/")))
  "*Specify a directory for temporary files during printing."
  :type '(directory :tag "Temporary Directory")
  :group 'printing)


(defcustom pr-ps-temp-file "prspool.ps"
  "*Specify PostScript temporary file name."
  :type '(file :tag "PostScript Temporary File Name")
  :group 'printing)


(defcustom pr-gv-command
  (if ps-windows-system
      "c:/gs/gsview/gsview32.exe"
    "gv")
  "*Specify path and name of gsview program."
  :type '(string :tag "Ghostview Program")
  :group 'printing)


(defcustom pr-gs-command
  (if ps-windows-system
      "c:/gs/gswin32.exe"
    "gs")
  "*Specify path and name of ghostscript program."
  :type '(string :tag "Ghostscript Program")
  :group 'printing)


(defcustom pr-gs-switches
  (if ps-windows-system
      '("-q -dNOPAUSE -Ic:/gs/gs5.50;c:/gs/gs5.50/fonts")
    '("-q -dNOPAUSE -I/usr/share/ghostscript/5.10"))
  "*Specify ghostscript switches.  See the documentation on GS for more info.

It's a list of strings, where each string is one or more ghostscript switches.

A note on the gs switches:

-q					quiet
-dNOPAUSE				don't wait for user intervention
-Ic:/gs/gs5.50;c:/gs/gs5.50/fonts	the directories needed for gs
-c quit					it's added at the end to terminate gs

See ghostscript documentation for more information."
  :type '(repeat (string :tag "Ghostscript Switch"))
  :group 'printing)


(defcustom pr-gs-device
  (if ps-windows-system
      "mswinpr2"
    "uniprint")
  "*Specify ghostscript device switch value (-sDEVICE=).

A note on the gs switches:

-sDEVICE=djet500	the printer - works with HP DeskJet 540

See the documentation on GS for more info.
See also `pr-ps-printer-alist'."
  :type '(string :tag "Ghostscript Device")
  :group 'printing)


(defcustom pr-gs-resolution 300
  "*Specify ghostscript resolution switch value (-r).

A note on the gs switches:

-r300	resolution 300x300

See the documentation on GS for more info.
See also `pr-ps-printer-alist'."
  :type '(integer :tag "Ghostscript Resolution")
  :group 'printing)


(defcustom pr-print-using-ghostscript nil
  "*Non-nil means print using ghostscript.

This is useful if you don't have a PostScript printer, so you could use the
ghostscript to print a PostScript file.

In a Unix system, if ghostscript is set as a PostScript filter, this variable
should be nil."
  :type 'boolean
  :group 'printing)


(defcustom pr-faces-p nil
  "*Non-nil means print with face attributes."
  :type 'boolean
  :group 'printing)


(defcustom pr-spool-p nil
  "*Non-nil means spool printing in a buffer."
  :type 'boolean
  :group 'printing)


(defcustom pr-file-landscape nil
  "*Non-nil means print PostScript file in landscape orientation."
  :type 'boolean
  :group 'printing)


(defcustom pr-file-duplex nil
  "*Non-nil means print PostScript file in duplex mode."
  :type 'boolean
  :group 'printing)


(defcustom pr-file-tumble nil
  "*Non-nil means print PostScript file in tumble mode.

If tumble is off, produces a printing suitable for binding on the left or
right.
If tumble is on, produces a printing suitable for binding at the top or
bottom."
  :type 'boolean
  :group 'printing)


(defcustom pr-auto-region t
  "*Non-nil means region is automagically detected.

Note that this will only work if you're using transient mark mode.

When this variable is non-nil, the `*-buffer*' commands will behave like
`*-region*' commands, that is, `*-buffer*' commands will print only the region
marked instead of all buffer."
  :type 'boolean
  :group 'printing)


(defcustom pr-auto-mode t
  "*Non-nil means major-mode printing is prefered over normal printing.

That is, if current major-mode is declared in `pr-mode-alist', the `*-buffer*'
and `*-region*' commands will behave like `*-mode*' commands; otherwise,
`*-buffer*' commands will print the current buffer and `*-region*' commands
will print the current region."
  :type 'boolean
  :group 'printing)


(defcustom pr-mode-alist
  '((mh-folder-mode			; mh summary buffer
     pr-mh-lpr-1  pr-mh-print-1
     2
     (ps-article-author ps-article-subject)
     ("/pagenumberstring load" pr-article-date)
     nil
     )
    (mh-letter-mode			; mh letter buffer
     pr-mh-lpr-2  pr-mh-print-2
     2
     (ps-article-author ps-article-subject)
     ("/pagenumberstring load" pr-article-date)
     nil
     )
    (rmail-summary-mode			; rmail summary buffer
     pr-rmail-lpr pr-rmail-print
     3
     (ps-article-subject ps-article-author buffer-name)
     nil
     nil
     )
    (rmail-mode				; rmail buffer
     pr-rmail-lpr pr-rmail-print
     3
     (ps-article-subject ps-article-author buffer-name)
     nil
     nil
     )
    (gnus-summary-mode			; gnus summary buffer
     pr-gnus-lpr  pr-gnus-print
     3
     (ps-article-subject ps-article-author gnus-newsgroup-name)
     nil
     nil
     )
    (gnus-article-mode			; gnus article buffer
     pr-gnus-lpr  pr-gnus-print
     3
     (ps-article-subject ps-article-author gnus-newsgroup-name)
     nil
     nil
     )
    (Info-mode				; Info buffer
     pr-mode-lpr  pr-mode-print
     2
     (ps-info-node ps-info-file)
     nil
     nil
     )
    )
  "*Specify an alist for a major-mode and printing functions.

To customize a major mode printing, just declare the customization in
`pr-mode-alist' and invoke some of `*-mode*' commands.  An example for major
mode usage is when you're using gnus (or mh, or rmail, etc.) and you're in the
*Summary* buffer, if you forget to switch to the *Article* buffer before
printing, you'll get a nicely formatted list of article subjects shows up at
the printer.  With major mode printing you don't need to switch from gnus
*Summary* buffer first.

The elements have the following form:

   (MAJOR-MODE
    LPR-PRINT PS-PRINT
    HEADER-LINES
    LEFT-HEADER
    RIGHT-HEADER
    KILL-LOCAL-VARIABLE
    DEFAULT...)

Where:

MAJOR-MODE	It's the major mode symbol.

LPR-PRINT	It's a symbol function for text printing.  It's invoked with
		one argument:
		(HEADER-LINES  LEFT-HEADER  RIGHT-HEADER DEFAULT...).

		Usually LPR-PRINT function prepares the environment or buffer
		and then call the function `pr-mode-lpr' which it's used to
		process the buffer and send it to text printer.

		The `pr-mode-lpr' definition is:

		(pr-mode-lpr HEADER-LIST &optional FROM TO)

		Where HEADER-LIST is like the argument passed to LPR-PRINT.
		FROM and TO are the beginning and end markers, respectively,
		for a region.  If FROM is nil, it's used (point-min); if TO is
		nil, it's used (point-max).

PS-PRINT	It's a symbol function for PostScript printing.  It's invoked
		with 3 arguments: n-up printing, file name and the list:
		(HEADER-LINES  LEFT-HEADER  RIGHT-HEADER DEFAULT...).

		Usually PS-PRINT function prepares the environment or buffer
		and then call the function `pr-mode-print' which it's used to
		process the buffer and send it to PostScript printer.

		The `pr-mode-print' definition is:

		(pr-mode-print N-UP FILENAME HEADER-LIST &optional FROM TO)

		Where N-UP, FILENAME and HEADER-LIST are like the arguments
		passed to PS-PRINT.  FROM and TO are the beginning and end
		markers, respectively, for a region.  If TO is nil, it's used
		(point-max).

HEADER-LINES	It's the number of header lines; if is nil, it uses
		`ps-header-lines' value.

LEFT-HEADER	It's the left header part, it's a list of string, variable
		symbol or function symbol (with no argument); if is nil, it
		uses `ps-left-header' value.

RIGHT-HEADER	It's the right header part, it's a list of string, variable
		symbol or function symbol (with no argument); if is nil, it
		uses `ps-right-header' value.

KILL-LOCAL-VARIABLE
		Non-nil means to kill all buffer local variable declared in
		DEFAULT (see below).

DEFAULT		It's a way to set default values when this entry is selected.
		It's a cons like:

		   (VARIABLE-SYM . VALUE)

		That associates VARIABLE-SYM with VALUE.  when this entry is
		selected, it's executed the following command:

		   (set (make-local-variable VARIABLE-SYM) (eval VALUE))

		Note that VALUE can be any valid lisp expression.  So, don't
		forget to quote symbols and constant lists.
		If VARIABLE is the special keyword `:inherits-from', VALUE must
		be a symbol name setting defined in `pr-setting-database' from
		which the current setting inherits the context.  Take care with
		circular inheritance.
		Examples:
			'(ps-landscape-mode . nil)
			'(ps-spool-duplex . t)
			'(pr-gs-device . (my-gs-device t))"
  :type '(repeat
	  (list
	   :tag "Major Mode Alist Element"
	   (symbol :tag "Major Mode")
	   (function :tag "Text Printing Function")
	   (function :tag "PS Printing Function")
	   (choice :menu-tag "Number of Header Lines"
		   :tag "Number of Header Lines"
		   (integer :tag "Number")
		   (const :tag "Default Number" nil))
	   (repeat :tag "Left Header List"
		   (choice string symbol))
	   (repeat :tag "Right Header List"
		   (choice string symbol))
	   (boolean :tag "Kill Local Variable At End")
	   (repeat
	    :inline t
	    (cons
	     :tag "Default Value"
	     (choice
	      :menu-tag "Variable"
	      :tag "Variable"
	      (const :tag "Landscape"              ps-landscape-mode)
	      (const :tag "Print Header"           ps-print-header)
	      (const :tag "Print Header Frame"     ps-print-header-frame)
	      (const :tag "Line Number"            ps-line-number)
	      (const :tag "Zebra Stripes"          ps-zebra-stripes)
	      (const :tag "Duplex"                 ps-spool-duplex)
	      (const :tag "Tumble"                 ps-spool-tumble)
	      (const :tag "Upside-Down"            ps-print-upside-down)
	      (const :tag "PS File Landscape"      pr-file-landscape)
	      (const :tag "PS File Duplex"         pr-file-duplex)
	      (const :tag "PS File Tumble"         pr-file-tumble)
	      (const :tag "Auto Region"            pr-auto-region)
	      (const :tag "Auto Mode"              pr-auto-mode)
	      (const :tag "Ghostscript Device"     pr-gs-device)
	      (const :tag "Ghostscript Resolution" pr-gs-resolution)
	      (const :tag ":inherits-from"         :inherits-from)
	      (variable :tag "Other"))
	     (sexp :tag "Value")))
	   ))
  :group 'printing)


(defcustom pr-ps-utility 'mpage
  "*Specify PostScript utility symbol.

This utility symbol should be defined on `pr-ps-utility-alist' (see it for
documentation).

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update PostScript utility menu.

NOTE: Don't forget to download and install the utilities declared on
      `pr-ps-utility-alist'."
  :type '(symbol :tag "PS File Utility")
  :set 'pr-ps-utility-custom-set
  :group 'printing)


(defcustom pr-ps-utility-alist
  '((mpage "mpage"    "-b%s" "-%d" "-l" "-t" "-T" ">" nil)
    (psnup "psnup -q" "-P%s" "-%d" "-l" nil  nil  " " nil
	   (:inherits-from . no-duplex))
    )
  ;; Examples:
  ;; * On Unix system:
  ;;    '((mpage "mpage"    "-b%s" "-%d" "-l" "-t" "-T" ">" nil)
  ;;      (psnup "psnup -q" "-P%s" "-%d" "-l" nil  nil  " " nil
  ;;             (pr-file-duplex . nil) (pr-file-tumble . nil))
  ;;      )
  ;; * On Windows system:
  ;;    '((psnup "c:/psutils/psnup -q" "-P%s" "-%d" "-l" nil  nil  " " nil
  ;;             (pr-file-duplex . nil) (pr-file-tumble . nil))
  ;;      )
  "*Specify an alist for PostScript utility processing (PS utility database).

The alist element has the form:

   (SYMBOL UTILITY PAPERSIZE N-UP LANDSCAPE DUPLEX TUMBLE OUTPUT SWITCHES
	   DEFAULT...)

Where:

SYMBOL		It's a symbol to identify a PostScript utility.  It's for
		`pr-ps-utility' variable setting and for menu selection.
		Examples:
			'mpage
			'psnup

UTILITY		Name of utility for processing a PostScript file.
		Examples:
		    . for Unix system:
			\"mpage\"
			\"psnup -q\"

		    . for Windows system:
			\"c:/psutils/psnup -q\"

PAPERSIZE	It's a format string to specify paper size switch.
		Example:
		    . for mpage
			\"-b%s\"

N-UP		It's a format string to specify n-up switch.
		Example:
		    . for psnup
			\"-%d\"

LANDSCAPE	It's a string to specify landscape switch.  If the utility
		doesn't have landscape switch, set to nil.
		Example:
		    . for psnup
			\"-l\"

DUPLEX		It's a string to specify duplex switch.  If the utility doesn't
		have duplex switch, set to nil.
		Example:
		    . for psnup
			nil

TUMBLE		It's a string to specify tumble switch.  If the utility doesn't
		have tumble switch, set to nil.
		Example:
		    . for psnup
			nil

OUTPUT		It's a string to specify how to generate an output file.  Some
		utilities accept an output file option, but some others need
		output redirection or some other way to specify an output file.
		Example:
		    . for psnup
			\" \" ; psnup ... input output

		    . for mpage
			\">\" ; mpage ... input > output

SWITCHES	List of sexp's to pass as extra options for PostScript utility
		program.
		Example:
		    . for psnup
			'(\"-q\")
			nil

DEFAULT		It's a way to set default values when this entry is selected.
		It's a cons like:

		   (VARIABLE . VALUE)

		That associates VARIABLE with VALUE.  when this entry is
		selected, it's executed the following command:

		   (set VARIABLE (eval VALUE))

		Note that VALUE can be any valid lisp expression.  So, don't
		forget to quote symbols and constant lists.
		If VARIABLE is the special keyword `:inherits-from', VALUE must
		be a symbol name setting defined in `pr-setting-database' from
		which the current setting inherits the context.  Take care with
		circular inheritance.
		Examples:
			'(pr-file-landscape . nil)
			'(pr-file-duplex . t)
			'(pr-gs-device . (my-gs-device t))

This variable should be modified by customization engine.  If this variable is
modified by other means (for example, a lisp function), use `pr-update-menus'
function (see it for documentation) to update PostScript utility menu.

NOTE: Don't forget to download and install the utilities declared on
      `pr-ps-utility-alist'."
  :type '(repeat
	  :tag "PS File Command Alist"
	  (list :tag "PS File Command"
		(symbol :tag "PS File Command Symbol")
		(string :tag "PS File Command Name")
		(choice :menu-tag "PS File Paper Size"
			:tag "PS File Paper Size"
			(const :tag "No Paper Size" nil)
			(string :tag "Paper Size Format"))
		(choice :menu-tag "PS File N-Up"
			:tag "PS File N-Up"
			(const :tag "No N-Up" nil)
			(string :tag "N-Up Format"))
		(choice :menu-tag "PS File Landscape"
			:tag "PS File Landscape"
			(const :tag "No Landscape" nil)
			(string :tag "Landscape Switch"))
		(choice :menu-tag "PS File Duplex"
			:tag "PS File Duplex"
			(const :tag "No Duplex" nil)
			(string :tag "Duplex Switch"))
		(choice :menu-tag "PS File Tumble"
			:tag "PS File Tumble"
			(const :tag "No Tumble" nil)
			(string :tag "Tumble Switch"))
		(string :tag "Output Separator")
		(repeat :tag "Utility Switches"
			(sexp :tag "Switch"))
		(repeat
		 :inline t
		 (cons
		  :tag "Default Value"
		  (choice
		   :menu-tag "Variable"
		   :tag "Variable"
		   (const :tag "PS File Landscape"      pr-file-landscape)
		   (const :tag "PS File Duplex"         pr-file-duplex)
		   (const :tag "PS File Tumble"         pr-file-tumble)
		   (const :tag "Ghostscript Device"     pr-gs-device)
		   (const :tag "Ghostscript Resolution" pr-gs-resolution)
		   (const :tag ":inherits-from"         :inherits-from)
		   (variable :tag "Other"))
		  (sexp :tag "Value")))
		))
  :set 'pr-alist-custom-set
  :group 'printing)


(defcustom pr-menu-lock t
  "*Non-nil means menu is locked while selecting toggle options.

See also `pr-menu-char-height' and `pr-menu-char-width'."
  :type 'boolean
  :group 'printing)


(defcustom pr-menu-char-height
  (cond ((eq ps-print-emacs-type 'emacs) ; GNU Emacs
	 (frame-char-height))
	((eq ps-print-emacs-type 'xemacs) ; XEmacs
	 (font-height (face-font 'default))))
  "*Specify menu char height in pixels.

This variable is used to guess which vertical position should be locked the
menu, so don't forget to adjust it if menu position is not ok.

See also `pr-menu-lock' and `pr-menu-char-width'."
  :type 'integer
  :group 'printing)


(defcustom pr-menu-char-width
  (cond ((eq ps-print-emacs-type 'emacs) ; GNU Emacs
	 (frame-char-width))
	((eq ps-print-emacs-type 'xemacs) ; XEmacs
	 (font-width (face-font 'default))))
  "*Specify menu char width in pixels.

This variable is used to guess which horizontal position should be locked the
menu, so don't forget to adjust it if menu position is not ok.

See also `pr-menu-lock' and `pr-menu-char-height'."
  :type 'integer
  :group 'printing)


(defcustom pr-setting-database
  '((no-duplex				; setting symbol name
     nil nil nil			; inherits  local  kill-local
     (pr-file-duplex . nil)		; settings
     (pr-file-tumble . nil))
    )
  "*Specify an alist for settings in general.

The elements have the following form:

   (SYMBOL INHERITS LOCAL KILL-LOCAL SETTING...)

Where:

SYMBOL		It's a symbol to identify the setting group.

INHERITS	Specify the inheritance for SYMBOL group.  It's a symbol name
		setting from which the current setting inherits the context.
		If INHERITS is nil, means that there is no inheritance.
		This is a simple inheritance mechanism.

		Let's see an example to illustrate the inheritance mechanism:

		(setq pr-setting-database
		      '((no-duplex	; setting symbol name
			 nil		; inherits
			 nil nil	; local  kill-local
			 (pr-file-duplex . nil) ; settings
			 (pr-file-tumble . nil)
			 )
			(no-duplex-and-landscape ; setting symbol name
			 no-duplex	; inherits
			 nil nil	; local  kill-local
			 (pr-file-landscape . nil) ; settings
			 )))

		The example above has two setting groups: no-duplex and
		no-duplex-and-landscape.  When setting no-duplex is activated
		through `:inherits-from' (see `pr-ps-utility', `pr-mode-alist'
		and `pr-ps-printer-alist'), the variables pr-file-duplex and
		pr-file-tumble are both set to nil.

		Now when setting no-duplex-and-landscape is activated through
		`:inherits-from', the variable pr-file-landscape is set to nil
		and also the settings for no-duplex are done, because
		no-duplex-and-landscape inherits settings from no-duplex.

		Take care with circular inheritance.  It's an error if circular
		inheritance happens.

LOCAL		Non-nil means that all settings for SYMBOL group will be
		declared local buffer.

KILL-LOCAL	Non-nil means that all settings for SYMBOL group will be
		killed at end.  It has effect only when LOCAL is non-nil.

SETTING		It's a cons like:

		   (VARIABLE . VALUE)

		That associates VARIABLE with VALUE.  when this entry is
		selected, it's executed the following command:

		  * If LOCAL is non-nil:
		   (set (make-local-variable VARIABLE) (eval VALUE))

		  * If LOCAL is nil:
		   (set VARIABLE (eval VALUE))

		Note that VALUE can be any valid lisp expression.  So, don't
		forget to quote symbols and constant lists.
		This setting is ignored if VARIABLE is equal to keyword
		`:inherits-from'.
		Examples:
			'(ps-landscape-mode . nil)
			'(ps-spool-duplex . t)
			'(pr-gs-device . (my-gs-device t))"
  :type '(repeat
	  (list
	   :tag "Setting Alist Element"
	   (symbol :tag "Setting Name")
	   (choice :menu-tag "Inheritance" :tag "Inheritance"
		   (const :tag "No Inheritance" nil)
		   (symbol :tag "Inherits From"))
	   (boolean :tag "Local Buffer Setting")
	   (boolean :tag "Kill Local Variable At End")
	   (repeat
	    :inline t
	    (cons
	     :tag "Setting"
	     (choice
	      :menu-tag "Variable"
	      :tag "Variable"
	      (const :tag "Landscape"              ps-landscape-mode)
	      (const :tag "Print Header"           ps-print-header)
	      (const :tag "Print Header Frame"     ps-print-header-frame)
	      (const :tag "Line Number"            ps-line-number)
	      (const :tag "Zebra Stripes"          ps-zebra-stripes)
	      (const :tag "Duplex"                 ps-spool-duplex)
	      (const :tag "Tumble"                 ps-spool-tumble)
	      (const :tag "Upside-Down"            ps-print-upside-down)
	      (const :tag "PS File Landscape"      pr-file-landscape)
	      (const :tag "PS File Duplex"         pr-file-duplex)
	      (const :tag "PS File Tumble"         pr-file-tumble)
	      (const :tag "Auto Region"            pr-auto-region)
	      (const :tag "Auto Mode"              pr-auto-mode)
	      (const :tag "Ghostscript Device"     pr-gs-device)
	      (const :tag "Ghostscript Resolution" pr-gs-resolution)
	      (variable :tag "Other"))
	     (sexp :tag "Value")))
	   ))
  :group 'printing)


(defcustom pr-visible-entry-list
  '(postscript text postscript-options postscript-process printing help)
  "*Specify a list of Printing menu visible entries.

Valid values with the corresponding menu parts are:

			      +------------------------------+
   `postscript'		      |    PostScript Preview       >|
			      |    PostScript Print         >|
			      |    PostScript Printer: name >|
			      +------------------------------+
   `text'		      |    Printify                 >|
			      |    Print                    >|
			      |    Text Printer: name       >|
			      +------------------------------+
   `postscript-options'	      |[ ] Landscape                 |
			      |[ ] Print Header              |
			      |[ ] Print Header Frame        |
			      |[ ] Line Number               |
			      |[ ] Zebra Stripes             |
			      |[ ] Duplex                    |
			      |[ ] Tumble                    |
			      |[ ] Upside-Down               |
			      +------------------------------+
   `postscript-process'	      |[ ] Spool Buffer              |
			      |[ ] Print with-faces          |
			      |[ ] Print Using Ghostscript   |
			      +------------------------------+
   `printing'		      |[ ] Auto Region               |
			      |[ ] Auto Mode                 |
			      |[ ] Menu Lock                 |
			      +------------------------------+
   `help'		      |    Customize                >|
			      |    Show Settings            >|
			      |    Help                      |
			      +------------------------------+

Any other value is ignored."
  :type '(repeat :tag "Menu Visible Part"
		 (choice :menu-tag "Menu Part"
			 :tag "Menu Part"
			 (const postscript)
			 (const text)
			 (const postscript-options)
			 (const postscript-process)
			 (const printing)
			 (const help)))
  :group 'printing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables


(defvar pr-txt-command nil
  "Name of program for printing a text file.
See `pr-txt-printer-alist'.")


(defvar pr-txt-switches nil
  "List of sexp's to pass as extra options for text printer program.
See `pr-txt-printer-alist'.")


(defvar pr-txt-printer nil
  "Specify text printer name.
See `pr-txt-printer-alist'.")


(defvar pr-ps-command nil
  "Name of program for printing a PostScript file.
See `pr-ps-printer-alist'.")


(defvar pr-ps-switches nil
  "List of sexp's to pass as extra options for PostScript printer program.
See `pr-ps-printer-alist'.")


(defvar pr-ps-printer-switch nil
  "Specify PostScript printer name switch.
See `pr-ps-printer-alist'.")


(defvar pr-ps-printer nil
  "Specify PostScript printer name.
See `pr-ps-printer-alist'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys & Menus


(cond
 ((eq ps-print-emacs-type 'emacs)
  ;; GNU Emacs
  (defun pr-mark-active-p ()
    mark-active)

  ;; GNU Emacs
  (defsubst pr-region-active-p ()
    (and pr-auto-region transient-mark-mode mark-active)))

 ((eq ps-print-emacs-type 'xemacs)
  ;; XEmacs
  (defalias 'pr-mark-active-p 'region-active-p)

  ;; XEmacs
  (defsubst pr-region-active-p ()
    (and pr-auto-region (not zmacs-region-stays) (region-active-p)))))


(defsubst pr-visible-p (key)
  (memq key pr-visible-entry-list))


(defsubst pr-mode-alist-p ()
  (cdr (assq major-mode pr-mode-alist)))


(defsubst pr-auto-mode-p ()
  (and pr-auto-mode (pr-mode-alist-p)))


(defsubst pr-using-ghostscript-p ()
  (and pr-print-using-ghostscript (not pr-spool-p)))


(defconst pr-menu-spec
  '(
    ("PostScript Preview" :visible (pr-visible-p 'postscript)
     ("Buffer" :active (not pr-spool-p)
      ["1-up"     (pr-ps-buffer-preview 1   t) t]
      ["2-up"     (pr-ps-buffer-preview 2   t) t]
      ["4-up"     (pr-ps-buffer-preview 4   t) t]
      ["Other..." (pr-ps-buffer-preview nil t)
       :keys "\\[pr-ps-buffer-preview]"])
     ("Region" :active (and (not pr-spool-p) (pr-mark-active-p))
      ["1-up"     (pr-ps-region-preview 1   t) t]
      ["2-up"     (pr-ps-region-preview 2   t) t]
      ["4-up"     (pr-ps-region-preview 4   t) t]
      ["Other..." (pr-ps-region-preview nil t)
       :keys "\\[pr-ps-region-preview]"])
     ("Mode" :active (and (not pr-spool-p) (pr-mode-alist-p))
      ["1-up"     (pr-ps-mode-preview 1   t) t]
      ["2-up"     (pr-ps-mode-preview 2   t) t]
      ["4-up"     (pr-ps-mode-preview 4   t) t]
      ["Other..." (pr-ps-mode-preview nil t)
       :keys "\\[pr-ps-mode-preview]"])
     ("File"
      ["As Is..."  (call-interactively 'pr-ps-file-preview)
       :keys "\\[pr-ps-file-preview]"]
      "--"
      ["PostScript Utility" pr-update-menus pr-ps-utility-alist]
      "--"
      ["1-up..."   (pr-ps-file-up-preview 1   t t) pr-ps-utility-alist]
      ["2-up..."   (pr-ps-file-up-preview 2   t t) pr-ps-utility-alist]
      ["4-up..."   (pr-ps-file-up-preview 4   t t) pr-ps-utility-alist]
      ["Other..."  (pr-ps-file-up-preview nil t t)
       :keys "\\[pr-ps-file-up-preview]" :active pr-ps-utility-alist]
      "--"
      ["Landscape" pr-toggle-file-landscape
       :style toggle :selected pr-file-landscape
       :active pr-ps-utility-alist]
      ["Duplex"    pr-toggle-file-duplex
       :style toggle :selected pr-file-duplex
       :active pr-ps-utility-alist]
      ["Tumble"    pr-toggle-file-tumble
       :style toggle :selected pr-file-tumble
       :active (and pr-file-duplex pr-ps-utility-alist)])
     ["Despool..." (call-interactively 'pr-despool-preview)
      :active pr-spool-p :keys "\\[pr-despool-preview]"])
    ("PostScript Print" :visible (pr-visible-p 'postscript)
     ("Buffer"
      ["1-up"     (pr-ps-buffer-ps-print 1   t) t]
      ["2-up"     (pr-ps-buffer-ps-print 2   t) t]
      ["4-up"     (pr-ps-buffer-ps-print 4   t) t]
      ["Other..." (pr-ps-buffer-ps-print nil t)
       :keys "\\[pr-ps-buffer-ps-print]"])
     ("Region" :active (pr-mark-active-p)
      ["1-up"     (pr-ps-region-ps-print 1   t) t]
      ["2-up"     (pr-ps-region-ps-print 2   t) t]
      ["4-up"     (pr-ps-region-ps-print 4   t) t]
      ["Other..." (pr-ps-region-ps-print nil t)
       :keys "\\[pr-ps-region-ps-print]"])
     ("Mode" :active (pr-mode-alist-p)
      ["1-up"     (pr-ps-mode-ps-print 1   t) t]
      ["2-up"     (pr-ps-mode-ps-print 2   t) t]
      ["4-up"     (pr-ps-mode-ps-print 4   t) t]
      ["Other..." (pr-ps-mode-ps-print nil t)
       :keys "\\[pr-ps-mode-ps-print]"])
     ("File"
      ["As Is..."  (call-interactively 'pr-ps-file-ps-print)
       :keys "\\[pr-ps-file-ps-print]"]
      "--"
      ["PostScript Utility" pr-update-menus pr-ps-utility-alist]
      "--"
      ["1-up..."   (pr-ps-file-up-ps-print 1   t t) pr-ps-utility-alist]
      ["2-up..."   (pr-ps-file-up-ps-print 2   t t) pr-ps-utility-alist]
      ["4-up..."   (pr-ps-file-up-ps-print 4   t t) pr-ps-utility-alist]
      ["Other..."  (pr-ps-file-up-ps-print nil t t)
       :keys "\\[pr-ps-file-up-ps-print]" :active pr-ps-utility-alist]
      "--"
      ["Landscape" pr-toggle-file-landscape
       :style toggle :selected pr-file-landscape
       :active pr-ps-utility-alist]
      ["Duplex"    pr-toggle-file-duplex
       :style toggle :selected pr-file-duplex
       :active pr-ps-utility-alist]
      ["Tumble"    pr-toggle-file-tumble
       :style toggle :selected pr-file-tumble
       :active (and pr-file-duplex pr-ps-utility-alist)])
     ["Despool..." (call-interactively 'pr-despool-ps-print)
      :active pr-spool-p :keys "\\[pr-despool-ps-print]"])
     ["PostScript Printers" pr-update-menus
      :active pr-ps-printer-alist :included (pr-visible-p 'postscript)]
    "--"
    ("Printify" :visible (pr-visible-p 'text)
     ["Buffer" pr-printify-buffer t]
     ["Region" pr-printify-region (pr-mark-active-p)])
    ("Print" :visible (pr-visible-p 'text)
     ["Buffer" pr-txt-buffer t]
     ["Region" pr-txt-region (pr-mark-active-p)]
     ["Mode"   pr-txt-mode   (pr-mode-alist-p)])
    ["Text Printers" pr-update-menus
     :active pr-txt-printer-alist :included (pr-visible-p 'text)]
    "--"
    ["Landscape"               pr-toggle-landscape
     :style toggle :selected ps-landscape-mode
     :included (pr-visible-p 'postscript-options)]
    ["Print Header"            pr-toggle-header
     :style toggle :selected ps-print-header
     :included (pr-visible-p 'postscript-options)]
    ["Print Header Frame"      pr-toggle-header-frame
     :style toggle :selected ps-print-header-frame :active ps-print-header
     :included (pr-visible-p 'postscript-options)]
    ["Line Number"             pr-toggle-line
     :style toggle :selected ps-line-number
     :included (pr-visible-p 'postscript-options)]
    ["Zebra Stripes"           pr-toggle-zebra
     :style toggle :selected ps-zebra-stripes
     :included (pr-visible-p 'postscript-options)]
    ["Duplex"                  pr-toggle-duplex
     :style toggle :selected ps-spool-duplex
     :included (pr-visible-p 'postscript-options)]
    ["Tumble"                  pr-toggle-tumble
     :style toggle :selected ps-spool-tumble :active ps-spool-duplex
     :included (pr-visible-p 'postscript-options)]
    ["Upside-Down"             pr-toggle-upside-down
     :style toggle :selected ps-print-upside-down
     :included (pr-visible-p 'postscript-options)]
    "--"
    ["Spool Buffer"            pr-toggle-spool
     :style toggle :selected pr-spool-p
     :included (pr-visible-p 'postscript-process)]
    ["Print with-faces"        pr-toggle-faces
     :style toggle :selected pr-faces-p
     :included (pr-visible-p 'postscript-process)]
    ["Print Using Ghostscript" pr-toggle-ghostscript
     :style toggle :selected pr-print-using-ghostscript
     :included (pr-visible-p 'postscript-process)]
    "--"
    ["Auto Region" pr-toggle-region
     :style toggle :selected pr-auto-region :included (pr-visible-p 'printing)]
    ["Auto Mode"   pr-toggle-mode
     :style toggle :selected pr-auto-mode :included (pr-visible-p 'printing)]
    ["Menu Lock"   pr-toggle-lock
     :style toggle :selected pr-menu-lock :included (pr-visible-p 'printing)]
    "--"
    ("Customize" :visible (pr-visible-p 'help)
     ["printing" pr-customize       t]
     ["ps-print" ps-print-customize t]
     ["lpr"      lpr-customize      t])
    ("Show Settings" :visible (pr-visible-p 'help)
     ["printing" pr-show-pr-setup  t]
     ["ps-print" pr-show-ps-setup  t]
     ["lpr"      pr-show-lpr-setup t])
    ["Help" pr-help :active t :included (pr-visible-p 'help)]
    ))


;; Menu mapping: unfortunately XEmacs doesn't support :active for submenus,
;; only for items.
(defun pr-xemacs-menu-map (menu)
  (while menu
    (let ((item (car menu)))
      (cond ((symbolp item)		; menu keyword
	     ;; use :included instead of :active or :visible
	     (and (memq item '(:active :visible))
		  (setcar menu :included))
	     (setq menu (cdr menu)))
	    ((listp item)		; submenu
	     (pr-xemacs-menu-map item))
	    ))
    (setq menu (cdr menu))))


(defmacro pr-xemacs-global-menubar (&rest body)
  `(save-excursion
     (let ((temp (get-buffer-create (make-temp-name " *Temp"))))
       ;; be sure to access global menubar
       (set-buffer temp)
       ,@body
       (kill-buffer temp))))


(cond ((eq ps-print-emacs-type 'emacs)	; GNU Emacs
       ;; Menu binding
       (require 'easymenu)
	;; Replace existing "print" item by "Printing" item.
	;; If you're changing this file, you'll load it a second,
	;; third... time, but "print" item exists only in the first load.
       (defvar pr-menu-print-item "print")
       (easy-menu-change '("tools") "Printing" pr-menu-spec pr-menu-print-item)
       (when pr-menu-print-item
	 (easy-menu-remove-item nil '("tools") pr-menu-print-item)
	 (setq pr-menu-print-item nil))

       ;; Key binding
       (global-set-key [print]   'pr-ps-fast-fire)
       (global-set-key [M-print] 'pr-ps-mode-using-ghostscript)
       (global-set-key [C-print] 'pr-txt-fast-fire))


      ((eq ps-print-emacs-type 'xemacs)	; XEmacs
       ;; Menu mapping
       (pr-xemacs-menu-map pr-menu-spec)
       ;; Menu binding
       (pr-xemacs-global-menubar
	(add-submenu nil (cons "Printing" pr-menu-spec) "Apps"))

       ;; Key binding
       (global-set-key 'f22           'pr-ps-fast-fire)
       (global-set-key '(meta f22)    'pr-ps-mode-using-ghostscript)
       (global-set-key '(control f22) 'pr-txt-fast-fire)))


;;; You can also use something like:
;;;(global-set-key "\C-cbp" 'pr-ps-buffer-print)
;;;(global-set-key "\C-cbx" 'pr-ps-buffer-preview)
;;;(global-set-key "\C-cbb" 'pr-ps-buffer-using-ghostscript)
;;;(global-set-key "\C-crp" 'pr-ps-region-print)
;;;(global-set-key "\C-crx" 'pr-ps-region-preview)
;;;(global-set-key "\C-crr" 'pr-ps-region-using-ghostscript)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help Message


(defconst pr-help-message
  (concat "printing.el version " pr-version
	  "\n\n
Menu Layout
-----------

The `printing' menu (Tools/Printing) has the following layout:

       +------------------------------+       +-A--------+     +-B------+
I   1  |    PostScript Preview       >|-------|Buffer   >|-----|1-up    |
    2  |    PostScript Print         >|---- A |Region   >|-- B |2-up    |
    3  |    PostScript Printer: name >|---- C |Mode     >|-- B |4-up    |
       +------------------------------+       |File     >|-\\   |Other...|
II  4  |    Printify                 >|-----\\ |Despool...| |   +--------+
    5  |    Print                    >|---\\ | +----------+ |
    6  |    Text Printer: name       >|-\\ | | +------+     |
       +------------------------------+ | | \\-|Buffer| /---/
III 7  |[ ] Landscape                 | | |   |Region| |
    8  |[ ] Print Header              | | |   +------+ | +-------------+
    9  |[ ] Print Header Frame        | | |   +------+ \\-|    As Is... | Ia
    10 |[ ] Line Number               | | \\---|Buffer|   +-------------+ Ib
    11 |[ ] Zebra Stripes             | |     |Region|   |    name    >|-- C
    12 |[ ] Duplex                    | |     |Mode  |   +-------------+
    13 |[ ] Tumble                    | |     +------+   |    1-up...  | Ic
    14 |[ ] Upside-Down               | | +-C--------+   |    2-up...  |
       +------------------------------+ \\-|( ) name A|   |    4-up...  |
IV  15 |[ ] Spool Buffer              |   |( ) name B|   |    Other... |
    16 |[ ] Print with-faces          |   |...       |   +-------------+
    17 |[ ] Print Using Ghostscript   |   |(*) name  |   |[ ] Landscape| Id
       +------------------------------+   |...       |   |[ ] Duplex   | Ie
V   18 |[ ] Auto Region               |   +----------+   |[ ] Tumble   | If
    19 |[ ] Auto Mode                 |                  +-------------+
    20 |[ ] Menu Lock                 |
       +------------------------------+     +-D------+
VI  21 |    Customize                >|-----|printing|
    22 |    Show Settings            >|-- D |ps-print|
    23 |    Help                      |     |lpr     |
       +------------------------------+     +--------+

See `pr-visible-entry-list' for hiding some parts of the menu.

The menu has the following sections:

I. PostScript printing:

   1. You can generate a PostScript file (if you type C-u before activating
      menu) or PostScript temporary file for a buffer, a region or a major
      mode, choosing 1-up, 2-up, 4-up or any other n-up printing; after file
      generation, ghostview is activated using the file generated as
      argument.  This option is disabled if spooling is on (option 15).
      Also, if you already have a PostScript file you can preview it.
      Instead of previewing each buffer, region or major mode at once, you
      can save temporarily the PostScript code generated in a buffer and
      preview it later.  The option `Despool...' despools the PostScript
      spooling buffer in a temporary file and uses ghostview to preview it.
      If you type C-u before choosing this option, the PostScript code
      generated is saved in a file instead of saving in a temporary file.  To
      spool the PostScript code generated you need to turn on the option 15.
      The option `Despool...' is enabled if spooling is on (option 15).

      NOTE 1: It's possible to customize a major mode printing, just declare
	      the customization in `pr-mode-alist' and invoke some of
	      `*-mode*' commands or select Mode option in Printing menu.  An
	      example for major mode usage is when you're using gnus (or mh,
	      or rmail, etc.) and you're in the *Summary* buffer, if you
	      forget to switch to the *Article* buffer before printing,
	      you'll get a nicely formatted list of article subjects shows
	      up at the printer.  With major mode printing you don't need to
	      switch from gnus *Summary* buffer first.

      NOTE 2: There are the following options for PostScript file processing:
	      Ia. Print the file *as is*, that is, send it directly to
		  PostScript printer.
	      Ib. PostScript utility processing selection.
		  See `pr-ps-utility-alist' and `pr-setting-database' for
		  documentation.
	      Ic. Do n-up processing before printing.
	      Id. Toggle on/off landscape for PostScript file processing.
	      Ie. Toggle on/off duplex for PostScript file processing.
	      If. Toggle on/off tumble for PostScript file processing.

      NOTE 3: Don't forget to download and install the utilities declared on
	      `pr-ps-utility-alist'.

   2. Operate the same way as option 1, but it sends directly the PostScript
      code (or put in a file, if you've typed C-u) or it uses ghostscript to
      print the PostScript file generated.  It depends on option 17, if it's
      turned on, it uses ghostscript; otherwise, it sends directly to
      printer.  If spooling is on (option 15), the PostScript code is saved
      temporarily in a buffer instead of printing it or saving it in a file.
      Also, if you already have a PostScript file you can print it.
      Instead of printing each buffer, region or major mode at once, you can
      save temporarily the PostScript code generated in a buffer and print it
      later.  The option `Despool...' despools the PostScript spooling buffer
      directly on a printer.  If you type C-u before choosing this option,
      the PostScript code generated is saved in a file instead of sending to
      printer.  To spool the PostScript code generated you need to turn on
      the option 15.  This option is enabled if spooling is on (option 15).
      See also the NOTE 1, NOTE 2 and NOTE 3 on option 1.

   3. You can select a new PostScript printer to send PostScript code
      generated.  For selection it's used all PostScript printers defined
      in `pr-ps-printer-alist' variable (see it for documentation).
      See also `pr-setting-database'.

II. Text printing:

   4. If you have control characters (character code from \\000 to \\037) in a
      buffer and you want to print them in a text printer, select this
      option.  All control characters in your buffer or region will be
      replaced by a printable representation.  The printable representations
      use ^ (for ASCII control characters) or hex.  The characters tab,
      linefeed, space, return and formfeed are not affected.
      You don't need to select this option if you use any option of section
      I, the PostScript engine treats control characters properly.

   5. If you want to print a buffer, region or major mode in a text printer,
      select this option.  See also the NOTE 1 on option 1.

   6. You can select a new text printer to send text generated.  For
      selection it's used all text printers defined in `pr-txt-printer-alist'
      variable (see it for documentation).
      See also `pr-setting-database'.

III. PostScript page toggle options:

   7. If you want a PostScript landscape printing, turn on this option.

   8. If you want to have a header in each page in your PostScript code,
      turn on this option.

   9. If you want to draw a gaudy frame around the header, turn on this
      option.  This option is enabled if print header is on (option 8).

   10. If you want that the line number is printed in your PostScript code,
       turn on this option.

   11. If you want background zebra stripes in your PostScript code, turn on
       this option.

   12. If you want a duplex printing and your PostScript printer has this
       feature, turn on this option.

   13. If you turned on duplex printing, you can choose if you want to have a
       printing suitable for binding on the left or right (tumble off), or to
       have a printing suitable for binding at top or bottom (tumble on).
       This option is enabled if duplex is on (option 12).

   14. If you want a PostScript upside-down printing, turn on this option.

IV. PostScript processing toggle options:

   15. If you want to spool the PostScript code generated, turn on this
       option.  To spool the PostScript code generated use option 2.  You can
       despool later by choosing option 1 or 2, sub-option `Despool...'.

   16. If you use colors in your buffers and want to see these colors on your
       PostScript code generated, turn on this option.  If you have a
       black/white PostScript printer, these colors are displayed in gray
       scale by PostScript printer interpreter.

   17. If you don't have a PostScript printer to send PostScript files, turn
       on this option.  When this option is on, the ghostscript is used to
       print PostScript files.  In a Unix system, if ghostscript is set as a
       PostScript filter, you don't need to turn on this option.

V. Printing customization:

   18. If you want that region is automagically detected, turn on this
       option.  Note that this will only work if you're using transient mark
       mode.  When this option is on, the `*-buffer*' commands will behave
       like `*-region*' commands, that is, `*-buffer*' commands will print
       only the region marked instead of all buffer.

   19. Turn this option on if you want that when current major-mode is
       declared in `pr-mode-alist', the `*-buffer*' and `*-region*' commands
       behave like `*-mode*' commands.

   20. If you want that Printing menu stays poped while you are setting
       toggle options, turn on this option.  The variables
       `pr-menu-char-height' and `pr-menu-char-width' are used to guess the
       menu position, so don't forget to adjust these variables if menu
       position is not ok.

VI. Customization:

   21. Besides all options in section III, IV and V, you can customize much
       more PostScript options in `ps-print' option.  Or you can customize
       some `lpr' options for text printing.  Or customize `printing'
       options.

   22. Show current settings for `printing', `ps-print' or `lpr'.

   23. Quick help for printing menu layout.
")
  "Printing help message.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands


;;;###autoload
(defun pr-ps-buffer-preview (n-up &optional filename)
  "Preview buffer using ghostview.

Interactively, the command prompts for N-UP printing number and, when you use a
prefix argument (C-u), the command prompts the user for a file name, and saves
the PostScript image in that file instead of saving it in a temporary file.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument FILENAME is treated as follows: if it's nil, save the image in a
temporary file.  If FILENAME is a string, save the PostScript image in a file
with that name.  If FILENAME is t, prompts for a file name."
  (interactive (list (pr-interactive-n-up
		      (if (pr-auto-mode-p)
			  "PS preview mode"
			(pr-region-active-string "PS preview")))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (if (pr-auto-mode-p)
      (pr-ps-mode-preview n-up filename)
    (pr-ps-preview (pr-region-active-symbol) n-up filename
		   (pr-region-active-string "PS preview"))))


;;;###autoload
(defun pr-ps-buffer-using-ghostscript (n-up &optional filename)
  "Print buffer using PostScript through ghostscript.

Interactively, the command prompts for N-UP printing number and, when you use a
prefix argument (C-u), the command prompts the user for a file name, and saves
the PostScript image in that file instead of sending it to the printer.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument FILENAME is treated as follows: if it's nil, send the image to the
printer.  If FILENAME is a string, save the PostScript image in a file with
that name.  If FILENAME is t, prompts for a file name."
  (interactive (list (pr-interactive-n-up
		      (if (pr-auto-mode-p)
			  "PS print GS mode"
			(pr-region-active-string "PS print GS")))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (if (pr-auto-mode-p)
      (pr-ps-mode-using-ghostscript n-up filename)
    (pr-ps-using-ghostscript (pr-region-active-symbol) n-up filename
			     (pr-region-active-string "PS print GS"))))


;;;###autoload
(defun pr-ps-buffer-print (n-up &optional filename)
  "Print buffer using PostScript printer.

Interactively, the command prompts for N-UP printing number and, when you use a
prefix argument (C-u), the command prompts the user for a file name, and saves
the PostScript image in that file instead of sending it to the printer.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument FILENAME is treated as follows: if it's nil, send the image to the
printer.  If FILENAME is a string, save the PostScript image in a file with
that name.  If FILENAME is t, prompts for a file name."
  (interactive (list (pr-interactive-n-up
		      (if (pr-auto-mode-p)
			  "PS print mode"
			(pr-region-active-string "PS print")))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (if (pr-auto-mode-p)
      (pr-ps-mode-print n-up filename)
    (pr-ps-print (pr-region-active-symbol) n-up filename
		 (pr-region-active-string "PS print"))))


;;;###autoload
(defun pr-ps-buffer-ps-print (n-up &optional filename)
  "Print buffer using PostScript printer or through ghostscript.

It depends on `pr-print-using-ghostscript'.

Interactively, the command prompts for N-UP printing number and, when you use a
prefix argument (C-u), the command prompts the user for a file name, and saves
the PostScript image in that file instead of sending it to the printer.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument FILENAME is treated as follows: if it's nil, send the image to the
printer.  If FILENAME is a string, save the PostScript image in a file with
that name.  If FILENAME is t, prompts for a file name."
  (interactive (list (pr-interactive-n-up
		      (let ((str (if (pr-using-ghostscript-p)
				     "PS print GS"
				   "PS print")))
			(if (pr-auto-mode-p)
			    (concat str " mode")
			  (pr-region-active-string str))))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (cond ((pr-auto-mode-p)
	 (pr-ps-mode-ps-print n-up filename))
	((pr-using-ghostscript-p)
	 (pr-ps-using-ghostscript (pr-region-active-symbol) n-up filename
				  (pr-region-active-string "PS print GS")))
	(t
	 (pr-ps-print (pr-region-active-symbol) n-up filename
		      (pr-region-active-string "PS print")))))


;;;###autoload
(defun pr-ps-region-preview (n-up &optional filename)
  "Preview region using ghostview.

See also `pr-ps-buffer-preview'."
  (interactive (list (pr-interactive-n-up
		      (if (pr-auto-mode-p)
			  "PS preview mode"
			"PS preview region"))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (if (pr-auto-mode-p)
      (let ((pr-auto-region t))
	(pr-ps-mode-preview n-up filename))
    (pr-ps-preview 'region n-up filename "PS preview region")))


;;;###autoload
(defun pr-ps-region-using-ghostscript (n-up &optional filename)
  "Print region using PostScript through ghostscript.

See also `pr-ps-buffer-using-ghostscript'."
  (interactive (list (pr-interactive-n-up
		      (if (pr-auto-mode-p)
			  "PS print GS mode"
			"PS print GS region"))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (if (pr-auto-mode-p)
      (let ((pr-auto-region t))
	(pr-ps-mode-using-ghostscript n-up filename))
    (pr-ps-using-ghostscript 'region n-up filename "PS print GS region")))


;;;###autoload
(defun pr-ps-region-print (n-up &optional filename)
  "Print region using PostScript printer.

See also `pr-ps-buffer-print'."
  (interactive (list (pr-interactive-n-up
		      (if (pr-auto-mode-p)
			  "PS print mode"
			"PS print region"))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (if (pr-auto-mode-p)
      (let ((pr-auto-region t))
	(pr-ps-mode-print n-up filename))
    (pr-ps-print 'region n-up filename "PS print region")))


;;;###autoload
(defun pr-ps-region-ps-print (n-up &optional filename)
  "Print region using PostScript printer or through ghostscript.

See also `pr-ps-buffer-ps-print'."
  (interactive (list (pr-interactive-n-up
		      (concat (if (pr-using-ghostscript-p)
				  "PS print GS "
				"PS print ")
			      (if (pr-auto-mode-p)
				  "mode"
				"region")))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (cond ((pr-auto-mode-p)
	 (let ((pr-auto-region t))
	   (pr-ps-mode-ps-print n-up filename)))
	((pr-using-ghostscript-p)
	 (pr-ps-using-ghostscript 'region n-up filename "PS print GS region"))
	(t
	 (pr-ps-print 'region n-up filename "PS print region"))))


;;;###autoload
(defun pr-ps-mode-preview (n-up &optional filename)
  "Preview major mode using ghostview.

See also `pr-ps-buffer-preview'."
  (interactive (list (pr-interactive-n-up "PS preview mode")
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (pr-set-n-up-and-filename 'n-up 'filename "PS preview mode")
  (let ((file (pr-ps-file filename)))
    (and (pr-ps-mode n-up file)
	 (not pr-spool-p)
	 (pr-ps-file-preview file))))


;;;###autoload
(defun pr-ps-mode-using-ghostscript (n-up &optional filename)
  "Print major mode using PostScript through ghostscript.

See also `pr-ps-buffer-using-ghostscript'."
  (interactive (list (pr-interactive-n-up "PS print GS mode")
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (pr-set-n-up-and-filename 'n-up 'filename "PS print GS mode")
  (let ((file (pr-ps-file filename)))
    (when (and (pr-ps-mode n-up file)
	       (not pr-spool-p))
      (pr-ps-file-using-ghostscript file)
      (or filename (delete-file file)))))


;;;###autoload
(defun pr-ps-mode-print (n-up &optional filename)
  "Print major mode using PostScript printer.

See also `pr-ps-buffer-print'."
  (interactive (list (pr-interactive-n-up "PS print mode")
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (pr-set-n-up-and-filename 'n-up 'filename "PS print mode")
  (pr-ps-mode n-up filename))


;;;###autoload
(defun pr-ps-mode-ps-print (n-up &optional filename)
  "Print major mode using PostScript or through ghostscript.

See also `pr-ps-buffer-ps-print'."
  (interactive (list (pr-interactive-n-up
		      (if (pr-using-ghostscript-p)
			  "PS print GS mode"
			"PS print mode"))
		     (and (not pr-spool-p)
			  (ps-print-preprint current-prefix-arg))))
  (if (pr-using-ghostscript-p)
      (pr-ps-mode-using-ghostscript n-up filename)
    (pr-ps-mode-print n-up filename)))


;;;###autoload
(defun pr-printify-buffer ()
  "Replace nonprinting characters in buffer with printable representations.
The printable representations use ^ (for ASCII control characters) or hex.
The characters tab, linefeed, space, return and formfeed are not affected."
  (interactive "*")
  (if (pr-region-active-p)
      (pr-printify-region)
    (printify-region (point-min) (point-max))))


;;;###autoload
(defun pr-printify-region ()
  "Replace nonprinting characters in region with printable representations.
The printable representations use ^ (for ASCII control characters) or hex.
The characters tab, linefeed, space, return and formfeed are not affected."
  (interactive "*")
  (printify-region (point) (mark)))


;;;###autoload
(defun pr-txt-buffer ()
  "Print buffer using text printer."
  (interactive)
  (cond ((pr-auto-mode-p)
	 (pr-txt-mode))
	((pr-region-active-p)
	 (pr-txt-region))
	(t
	 (pr-txt-print (point-min) (point-max)))))


;;;###autoload
(defun pr-txt-region ()
  "Print region using text printer."
  (interactive)
  (if (pr-auto-mode-p)
      (let ((pr-auto-region t))
	(pr-txt-mode))
    (pr-txt-print (point) (mark))))


;;;###autoload
(defun pr-txt-mode ()
  "Print major mode using text printer."
  (interactive)
  (let ((args (pr-mode-alist-p)))
    (if args
	(funcall (car args) (nthcdr 2 args))
      (ding)
      (message "`%s' major mode not declared." major-mode))))


;;;###autoload
(defun pr-despool-preview (&optional filename)
  "Preview spooled PostScript.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of saving it in a temporary file.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
save the image in a temporary file.  If FILENAME is a string, save the
PostScript image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (let ((file (pr-ps-file filename)))
    (when (stringp file)
      (pr-despool-print file)
      (pr-ps-file-preview file))))


;;;###autoload
(defun pr-despool-using-ghostscript (&optional filename)
  "Print spooled PostScript using ghostscript.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
send the image to the printer.  If FILENAME is a string, save the PostScript
image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (let ((file (pr-ps-file filename)))
    (when (stringp file)
      (pr-despool-print file)
      (pr-ps-file-using-ghostscript file)
      (or filename (delete-file file)))))


;;;###autoload
(defun pr-despool-print (&optional filename)
  "Send the spooled PostScript to the printer.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
send the image to the printer.  If FILENAME is a string, save the PostScript
image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (let ((ps-lpr-command         pr-ps-command)
	(ps-lpr-switches        pr-ps-switches)
	(ps-printer-name-option pr-ps-printer-switch)
	(ps-printer-name        pr-ps-printer))
    (ps-despool filename)))


;;;###autoload
(defun pr-despool-ps-print (&optional filename)
  "Send the spooled PostScript to the printer or use ghostscript to print it.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a file name, and saves the spooled PostScript image in that file
instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it is nil,
send the image to the printer.  If FILENAME is a string, save the PostScript
image in a file with that name."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (if pr-print-using-ghostscript
      (pr-despool-using-ghostscript filename)
    (pr-despool-print filename)))


;;;###autoload
(defun pr-ps-file-preview (filename)
  "Preview PostScript file FILENAME."
  (interactive (list (pr-ps-infile-preprint "Preview ")))
  (and (stringp filename) (file-exists-p filename)
       (start-process-shell-command "PREVIEW" "*Messages*"
				    pr-gv-command filename)))


;;;###autoload
(defun pr-ps-file-up-preview (n-up ifilename &optional ofilename)
  "Preview PostScript file FILENAME."
  (interactive (list (pr-interactive-n-up "PS preview")
		     (pr-ps-infile-preprint "PS preview ")
		     (ps-print-preprint current-prefix-arg)))
  (let ((outfile (pr-ps-utility-args 'n-up 'ifilename 'ofilename
				     "PS preview ")))
    (pr-ps-utility-process n-up ifilename outfile)
    (pr-ps-file-preview outfile)))


;;;###autoload
(defun pr-ps-file-using-ghostscript (filename)
  "Print PostScript file FILENAME using ghostscript."
  (interactive (list (pr-ps-infile-preprint "Print preview ")))
  (and (stringp filename) (file-exists-p filename)
       (let* ((file (pr-expand-file-name filename))
	      (tempfile (pr-dosify-path (make-temp-name file))))
	 ;; gs use
	 (shell-command
	  (concat pr-gs-command
		  " -sDEVICE=" pr-gs-device
		  " -r" (int-to-string pr-gs-resolution)
		  " " (pr-switches-string pr-gs-switches "pr-gs-switches")
		  " -sOutputFile=" tempfile " " file " -c quit"))
	 ;; printing
	 (pr-ps-file-print tempfile)
	 ;; deleting
	 (delete-file tempfile))))


;;;###autoload
(defun pr-ps-file-print (filename)
  "Print PostScript file FILENAME."
  (interactive (list (pr-ps-infile-preprint "Print ")))
  (and (stringp filename) (file-exists-p filename)
       ;; printing
       (let ((file (pr-expand-file-name filename)))
	 (shell-command
	  (concat pr-ps-command " "
		  (pr-switches-string pr-ps-switches "pr-gs-switches") " "
		  (if (string-match "cp" pr-ps-command)
		      ;; for "cp" (cmd in out)
		      (concat "\"" file "\" "
			      pr-ps-printer-switch pr-ps-printer)
		    ;; else, for others (cmd out in)
		    (concat pr-ps-printer-switch pr-ps-printer
			    " \"" file "\"")))))))


;;;###autoload
(defun pr-ps-file-ps-print (filename)
  "Send PostScript file FILENAME to printer or use ghostscript to print it."
  (interactive (list (pr-ps-infile-preprint
		      (if pr-print-using-ghostscript
			  "Print preview "
			"Print "))))
  (if pr-print-using-ghostscript
      (pr-ps-file-using-ghostscript filename)
    (pr-ps-file-print filename)))


;;;###autoload
(defun pr-ps-file-up-ps-print (n-up ifilename &optional ofilename)
  "Process a PostScript file IFILENAME and send it to printer.

Interactively, the command prompts for N-UP printing number, for an input
PostScript file IFILENAME and, when you use a prefix argument (C-u), the
command prompts the user for an output PostScript file name OFILENAME, and
saves the PostScript image in that file instead of sending it to the printer.

Noninteractively, if N-UP is nil, prompts for N-UP printing number.  The
argument IFILENAME is treated as follows: if it's t, prompts for an input
PostScript file name; otherwise, it *must* be a string that it's an input
PostScript file name.  The argument OFILENAME is treated as follows: if it's
nil, send the image to the printer.  If OFILENAME is a string, save the
PostScript image in a file with that name.  If OFILENAME is t, prompts for a
file name."
  (interactive (list (pr-interactive-n-up
		      (if pr-print-using-ghostscript
			  "PS print GS"
			"PS print"))
		     (pr-ps-infile-preprint
		      (if pr-print-using-ghostscript
			  "PS print GS "
			"PS print "))
		     (ps-print-preprint current-prefix-arg)))
  (let ((outfile (pr-ps-utility-args 'n-up 'ifilename 'ofilename
				     (if pr-print-using-ghostscript
					 "PS print GS "
				       "PS print "))))
    (pr-ps-utility-process n-up ifilename outfile)
    (unless ofilename
      (pr-ps-file-ps-print outfile)
      (delete-file outfile))))


;;;###autoload
(defun pr-toggle-file-duplex ()
  "Toggle duplex for PostScript file."
  (interactive)
  (pr-toggle 'pr-file-duplex "PS file duplex" nil 7 5 nil
	     '("PostScript Print" "File")))


;;;###autoload
(defun pr-toggle-file-tumble ()
  "Toggle tumble for PostScript file.

If tumble is off, produces a printing suitable for binding on the left or
right.
If tumble is on, produces a printing suitable for binding at the top or
bottom."
  (interactive)
  (pr-toggle 'pr-file-tumble "PS file tumble" nil 8 5 nil
	     '("PostScript Print" "File")))


;;;###autoload
(defun pr-toggle-file-landscape ()
  "Toggle landscape for PostScript file."
  (interactive)
  (pr-toggle 'pr-file-landscape "PS file landscape" nil 6 5 nil
	     '("PostScript Print" "File")))


;;;###autoload
(defun pr-toggle-ghostscript ()
  "Toggle printing using ghostscript."
  (interactive)
  (pr-toggle 'pr-print-using-ghostscript "Printing using ghostscript"
	     'postscript-process 2 12 'toggle))


;;;###autoload
(defun pr-toggle-faces ()
  "Toggle printing with faces."
  (interactive)
  (pr-toggle 'pr-faces-p "Printing with-faces"
	     'postscript-process 1 12 'toggle))


;;;###autoload
(defun pr-toggle-spool ()
  "Toggle spooling."
  (interactive)
  (pr-toggle 'pr-spool-p "Spooling printing"
	     'postscript-process 0 12 'toggle))


;;;###autoload
(defun pr-toggle-duplex ()
  "Toggle duplex."
  (interactive)
  (pr-toggle 'ps-spool-duplex "Printing duplex"
	     'postcsript-options 5 12 'toggle))


;;;###autoload
(defun pr-toggle-tumble ()
  "Toggle tumble.

If tumble is off, produces a printing suitable for binding on the left or
right.
If tumble is on, produces a printing suitable for binding at the top or
bottom."
  (interactive)
  (pr-toggle 'ps-spool-tumble "Tumble"
	     'postscript-options 6 12 'toggle))


;;;###autoload
(defun pr-toggle-landscape ()
  "Toggle landscape."
  (interactive)
  (pr-toggle 'ps-landscape-mode "Landscape"
	     'postscript-options 0 12 'toggle))


;;;###autoload
(defun pr-toggle-upside-down ()
  "Toggle upside-down."
  (interactive)
  (pr-toggle 'ps-print-upside-down "Upside-Down"
	     'postscript-options 7 12 'toggle))


;;;###autoload
(defun pr-toggle-line ()
  "Toggle line number."
  (interactive)
  (pr-toggle 'ps-line-number "Line number"
	     'postscript-options 3 12 'toggle))


;;;###autoload
(defun pr-toggle-zebra ()
  "Toggle zebra stripes."
  (interactive)
  (pr-toggle 'ps-zebra-stripes "Zebra stripe"
	     'postscript-options 4 12 'toggle))


;;;###autoload
(defun pr-toggle-header ()
  "Toggle printing header."
  (interactive)
  (pr-toggle 'ps-print-header "Print header"
	     'postscript-options 1 12 'toggle))


;;;###autoload
(defun pr-toggle-header-frame ()
  "Toggle printing header frame."
  (interactive)
  (pr-toggle 'ps-print-header-frame "Print header frame"
	     'postscript-options 2 12 'toggle))


;;;###autoload
(defun pr-toggle-lock ()
  "Toggle menu lock."
  (interactive)
  (pr-toggle 'pr-menu-lock "Menu lock"
	     'printing 2 12 'toggle))


;;;###autoload
(defun pr-toggle-region ()
  "Toggle auto region."
  (interactive)
  (pr-toggle 'pr-auto-region "Auto region"
	     'printing 0 12 'toggle))


;;;###autoload
(defun pr-toggle-mode ()
  "Toggle auto mode."
  (interactive)
  (pr-toggle 'pr-auto-mode "Auto mode"
	     'printing 1 12 'toggle))


;;;###autoload
(defun pr-customize ()
  "Customization of `printing' group."
  (interactive)
  (customize-group 'printing))


;;;###autoload
(defun lpr-customize ()
  "Customization of `lpr' group."
  (interactive)
  (customize-group 'lpr))


;;;###autoload
(defun pr-help ()
  "Help for printing package."
  (interactive)
  (pr-show-setup pr-help-message "*Printing Help*"))


;;;###autoload
(defun pr-ps-name ()
  "Select interactively a PostScript printer."
  (interactive)
  (pr-menu-set-ps-title
   (pr-complete-alist "PostScript printer" pr-ps-printer-alist pr-ps-name)))


;;;###autoload
(defun pr-txt-name ()
  "Select interactively a text printer."
  (interactive)
  (pr-menu-set-txt-title
   (pr-complete-alist "Text printer" pr-txt-printer-alist pr-txt-name)))


;;;###autoload
(defun pr-ps-utility ()
  "Select interactively a PostScript utility."
  (interactive)
  (pr-menu-set-utility-title
   (pr-complete-alist "Postscript utility" pr-ps-utility-alist pr-ps-utility)))


;;;###autoload
(defun pr-show-ps-setup ()
  "Show current ps-print settings."
  (interactive)
  (pr-show-setup (ps-setup) "*PS Setup*"))


;;;###autoload
(defun pr-show-pr-setup ()
  "Show current printing settings."
  (interactive)
  (pr-show-setup (pr-setup) "*PR Setup*"))


;;;###autoload
(defun pr-show-lpr-setup ()
  "Show current lpr settings."
  (interactive)
  (pr-show-setup (lpr-setup) "*LPR Setup*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fast Commands


;;;###autoload
(defun pr-ps-fast-fire (n-up &optional select)
  "Fast fire function for PostScript printing.

If a region is active, the region will be printed instead of the whole buffer.
Also if the current major-mode is defined in `pr-mode-alist', the settings in
`pr-mode-alist' will be used, that is, the current buffer or region will be
printed using `pr-ps-mode-ps-print'.


Interactively, you have the following situations:

   M-x pr-ps-fast-fire RET
      The command prompts the user for a N-UP value and printing will
      immediatelly be done using the current active printer.

   C-u   M-x pr-ps-fast-fire RET
   C-u 0 M-x pr-ps-fast-fire RET
      The command prompts the user for a N-UP value and also for a current
      PostScript printer, then printing will immediatelly be done using the new
      current active printer.

   C-u 1 M-x pr-ps-fast-fire RET
      The command prompts the user for a N-UP value and also for a file name,
      and saves the PostScript image in that file instead of sending it to the
      printer.

   C-u 2 M-x pr-ps-fast-fire RET
      The command prompts the user for a N-UP value, then for a current
      PostScript printer and, finally, for a file name.  Then change the active
      printer to that choosen by user and saves the PostScript image in
      that file instead of sending it to the printer.


Noninteractively, the argument N-UP should be a positive integer greater than
zero and the argument SELECT is treated as follows:

   If it's nil, send the image to the printer.

   If it's a list or an integer lesser or equal to zero, the command prompts
   the user for a current PostScript printer, then printing will immediatelly
   be done using the new current active printer.

   If it's an integer equal to 1, the command prompts the user for a file name
   and saves the PostScript image in that file instead of sending it to the
   printer.

   If it's an integer greater or equal to 2, the command prompts the user for a
   current PostScript printer and for a file name.  Then change the active
   printer to that choosen by user and saves the PostScript image in that file
   instead of sending it to the printer.

   If it's a symbol which it's defined in `pr-ps-printer-alist', it's the new
   active printer and printing will immediatelly be done using the new active
   printer.

   Otherwise, send the image to the printer.


Note that this command always behaves as if `pr-auto-region' and `pr-auto-mode'
are both set to t."
  (interactive (list (pr-interactive-n-up
		      (if (pr-using-ghostscript-p)
			  "PS print GS fast"
			"PS print fast"))
		     current-prefix-arg))
  (let ((pr-auto-region t)
	(pr-auto-mode   t)
	filename)
    (cond ((null select))
	  ((listp select)
	   (pr-ps-name))
	  ((and (symbolp select)
		(assq select pr-ps-printer-alist))
	   (pr-menu-set-ps-title select))
	  ((integerp select)
	   (and (/= select 1)
		(pr-ps-name))
	   (and (>= select 1) (not pr-spool-p)
		(setq filename (pr-ps-outfile-preprint
				(if pr-print-using-ghostscript
				    "GS Fast "
				  "Fast "))))))
    (pr-ps-buffer-ps-print
     (cond ((not (integerp n-up))
	    (error "n-up must be an integer greater than zero."))
	   ((<= n-up 0) 1)
	   ((> n-up 100) 100)
	   (t n-up))
     filename)))


;;;###autoload
(defun pr-txt-fast-fire (&optional select-printer)
  "Fast fire function for text printing.

If a region is active, the region will be printed instead of the whole buffer.
Also if the current major-mode is defined in `pr-mode-alist', the settings in
`pr-mode-alist' will be used, that is, the current buffer or region will be
printed using `pr-txt-mode'.

Interactively, when you use a prefix argument (C-u), the command prompts the
user for a new active text printer.

Noninteractively, the argument SELECT-PRINTER is treated as follows:

   If it's nil, the printing is sent to the current active text printer.

   If it's a symbol which it's defined in `pr-txt-printer-alist', it's the new
   active printer and printing will immediatelly be done using the new active
   printer.

   If it's non-nil, the command prompts the user for a new active text printer.

Note that this command always behaves as if `pr-auto-region' and `pr-auto-mode'
are both set to t."
  (interactive (list current-prefix-arg))
  (cond ((null select-printer))
	((and (symbolp select-printer)
	      (assq select-printer pr-txt-printer-alist))
	 (pr-menu-set-txt-title select-printer))
	(t
	 (pr-txt-name)))
  (let ((pr-auto-region t)
	(pr-auto-mode   t))
    (pr-txt-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities


(defun pr-setup ()
  "Return the current `printing' setup.

This is *not* an interactive command.
One way to see `printing' setup is to switch to a *Scratch* buffer and type:

   M-: (insert (pr-setup)) RET

Or choose the menu option Printing/Show Settings/printing."
  (format
   "
;;; printing.el version %s

\(setq pr-txt-name          %s
      pr-txt-printer-alist %s

      pr-ps-name          %s
      pr-ps-printer-alist %s

      pr-temp-dir     %S
      pr-ps-temp-file %S

      pr-gv-command    %S
      pr-gs-command    %S
      pr-gs-switches   %s
      pr-gs-device     %S
      pr-gs-resolution %S

      pr-print-using-ghostscript %S
      pr-faces-p                 %S
      pr-spool-p                 %S
      pr-file-landscape          %S
      pr-file-duplex             %S
      pr-file-tumble             %S
      pr-auto-region             %S
      pr-auto-mode               %S

      pr-ps-utility       %s
      pr-ps-utility-alist %s

      pr-mode-alist %s

      pr-menu-lock        %S
      pr-menu-char-height %S
      pr-menu-char-width  %S

      pr-setting-database %s

      pr-visible-entry-list %s)

;;; printing.el - end of settings
"
   pr-version
   (ps-print-quote pr-txt-name)
   (ps-print-quote pr-txt-printer-alist)
   (ps-print-quote pr-ps-name)
   (ps-print-quote pr-ps-printer-alist)
   pr-temp-dir
   pr-ps-temp-file
   pr-gv-command
   pr-gs-command
   (ps-print-quote pr-gs-switches)
   pr-gs-device
   pr-gs-resolution
   pr-print-using-ghostscript
   pr-faces-p
   pr-spool-p
   pr-file-landscape
   pr-file-duplex
   pr-file-tumble
   pr-auto-region
   pr-auto-mode
   (ps-print-quote pr-ps-utility)
   (ps-print-quote pr-ps-utility-alist)
   (ps-print-quote pr-mode-alist)
   pr-menu-lock
   pr-menu-char-height
   pr-menu-char-width
   (ps-print-quote pr-setting-database)
   (ps-print-quote pr-visible-entry-list)))


(defun lpr-setup ()
  "Return the current `lpr' setup.

This is *not* an interactive command.
One way to see `lpr' setup is to switch to a *Scratch* buffer and type:

   M-: (insert (lpr-setup)) RET

Or choose the menu option Printing/Show Settings/lpr."
  (format
   "
;;; lpr.el settings

\(setq printer-name             %S
      lpr-switches             %s
      lpr-add-switches         %S
      lpr-command              %S
      lpr-headers-switches     %s
      print-region-function    %s
      lpr-page-header-program  %S
      lpr-page-header-switches %s)

;;; lpr.el - end of settings
"
   printer-name
   (ps-print-quote lpr-switches)
   lpr-add-switches
   lpr-command
   (ps-print-quote lpr-headers-switches)
   (ps-print-quote print-region-function)
   lpr-page-header-program
   (ps-print-quote lpr-page-header-switches)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mh-e (adapted from mh-e-init.el -- Tom Vogels <tov@ece.cmu.edu>)


(defun pr-article-date ()
  "Find the date of an article or mail message in current buffer.
Return only the dayname, if present, weekday, month, and year."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
	 "^Date:[ \t]+\\(\\([A-Za-z]+, \\)?[0-9]+ [A-Za-z]+ [0-9]+\\)" nil t)
	(buffer-substring (match-beginning 1) (match-end 1))
      (time-stamp-yyyy/mm/dd))))


(defun pr-mh-current-message ()
  "Go to mh-inbox current message."
  (let ((msg (or (mh-get-msg-num nil) 0)))
    (mh-show)
    (set-buffer mh-show-buffer)
    (goto-char (point-min))
    (mh-start-of-uncleaned-message)
    (message "Printing message %d" msg)))


(defun pr-mh-print-1 (n-up filename header-list)
  "Print mh-inbox current message in PostScript."
  (save-excursion
    (save-window-excursion
      (pr-mh-current-message)
      (pr-mode-print n-up filename header-list (point)))))


(defun pr-mh-lpr-1 (header-list)
  "Print mh-inbox current message in text printer."
  (save-excursion
    (save-window-excursion
      (pr-mh-current-message)
      (pr-mode-lpr header-list (point)))))


(defalias 'pr-mh-print-2 'pr-mode-print)


(defalias 'pr-mh-lpr-2 'pr-mode-lpr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rmail (hacked from ps-print.el)


(defun pr-rmail-lpr (header-list)
  "Print RMAIL current message in text printer."
  (pr-lpr-message-from-summary header-list 'rmail-summary-buffer rmail-buffer))


(defun pr-rmail-print (n-up filename header-list)
  "Print RMAIL current message in PostScript."
  (pr-ps-message-from-summary n-up filename header-list
			      'rmail-summary-buffer rmail-buffer))


(defun pr-ps-message-from-summary (n-up filename header-list
					summary-buffer summary-default)
  "Print current message in PostScript."
  (let ((buf (or (and (boundp summary-buffer)
		      (symbol-value summary-buffer))
		 summary-default)))
    (and (get-buffer buf)
	 (save-excursion
	   (set-buffer buf)
	   (pr-mode-print n-up filename header-list)))))


(defun pr-lpr-message-from-summary (header-list summary-buffer summary-default)
  "Print current message in text printer."
  (let ((buf (or (and (boundp summary-buffer)
		      (symbol-value summary-buffer))
		 summary-default)))
    (and (get-buffer buf)
	 (save-excursion
	   (set-buffer buf)
	   (pr-mode-lpr header-list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnus (hacked from ps-print.el)


(defun pr-gnus-print (n-up filename header-list)
  "Print *Article* current message in PostScript."
  (pr-ps-message-from-summary n-up filename header-list
			      'gnus-article-buffer "*Article*"))


(defun pr-gnus-lpr (header-list)
  "Print *Article* current message in text printer."
  (pr-lpr-message-from-summary header-list 'gnus-article-buffer "*Article*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Functions


(defun pr-ps-mode (n-up filename)
  "If current major mode is declared, print it in PostScript."
  (let ((args (pr-mode-alist-p)))
    (if args
	(let ((fun (cdr args)))
	  (funcall (car fun) n-up filename (cdr fun))
	  t)
      (ding)
      (message "`%s' major mode not declared." major-mode)
      nil)))


(defmacro pr-local-variable (header-list &rest body)
  `(save-excursion
     (let ((ps-header-lines (or (nth 0 ,header-list) ps-header-lines))
	   (ps-left-header  (or (nth 1 ,header-list) ps-left-header))
	   (ps-right-header (or (nth 2 ,header-list) ps-right-header))
	   ps-razzle-dazzle)
       (let ((local-var-list (pr-eval-local-alist (nthcdr 4 ,header-list))))
	 ,@body
	 (and (nth 3 ,header-list)
	      (pr-kill-local-variable local-var-list))))))


(defun pr-mode-print (n-up filename header-list &optional from to)
  "Print current major mode in PostScript."
  (pr-local-variable
   header-list
   (let ((file (pr-ps-file filename))
	 (start (cond (from)
		      ((pr-region-active-p) (region-beginning))
		      (t nil)
		      )))
     (pr-text2ps (pr-region-active-symbol start) n-up file start
		 (cond (to)
		       ((pr-region-active-p) (region-end))
		       (from (point-max))
		       ))
     (unless (or pr-spool-p filename)
       (pr-ps-file-print file)
       (delete-file file)))))


(defun pr-mode-lpr (header-list &optional from to)
  "Print current major mode in text printer."
  (pr-local-variable
   header-list
   (pr-txt-print (cond (from)
		       ((pr-region-active-p) (region-beginning))
		       (t (point-min)))
		 (cond (to)
		       ((pr-region-active-p) (region-end))
		       (t (point-max))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu Lock


(defconst pr-menu-entry-alist
  '((postscript         . 3)
    (text               . 3)
    (postscript-options . 8)
    (postscript-process . 3)
    (printing           . 3)
    (help               . 3)
    )
  "Alist that associates menu part with number of items per part.

It's used by `pr-menu-index'.

Each element has the form:

   (MENU-PART . NUMBER-OF-ITEMS)

See `pr-visible-entry-alist'.")


(defun pr-menu-index (entry index)
  (let ((base-list
	 (cond ((eq entry 'text)
		'(postscript))
	       ((eq entry 'postscript-options)
		'(postscript text))
	       ((eq entry 'postscript-process)
		'(postscript text postscript-options))
	       ((eq entry 'printing)
		'(postscript text postscript-options postscript-process))
	       (t
		nil)
	       ))
	key)
    (while base-list
      (setq key       (car base-list)
	    base-list (cdr base-list))
      (and (pr-visible-p key)
	   (setq index (+ index
			  (cdr (assq key pr-menu-entry-alist)))))))
  (+ index 2))


(cond
 ((eq ps-print-emacs-type 'xemacs)
  ;; XEmacs
  (defun pr-menu-position (entry index horizontal)
    (make-event
     'button-release
     (list 'button 1
	   'x (- (event-x-pixel current-mouse-event) ; X
		 (* horizontal pr-menu-char-width))
	   'y (- (event-y-pixel current-mouse-event) ; Y
		 (* (pr-menu-index entry index) pr-menu-char-height)))))
  )
 (ps-windows-system
  ;; GNU Emacs for Windows 95/98/NT
  (defun pr-menu-position (entry index horizontal)
    (let ((pos (cdr (mouse-pixel-position))))
      (list
       (list (car pos)			; X
	     (- (cdr pos)		; Y
		(* (pr-menu-index entry index) pr-menu-char-height)))
       (selected-frame))))		; frame
  )
 (t
  ;; GNU Emacs
  (defun pr-menu-position (entry index horizontal)
    (let ((pos (cdr (mouse-pixel-position))))
      (list
       (list (- (car pos)		; X
		(* horizontal pr-menu-char-width))
	     (- (cdr pos)		; Y
		(* (pr-menu-index entry index) pr-menu-char-height)))
       (selected-frame))))		; frame
  ))


(defvar pr-menu-position nil)
(defvar pr-menu-state nil)


(cond
 ((eq ps-print-emacs-type 'emacs)
  ;; GNU Emacs
  (defun pr-menu-lock (entry index horizontal state path)
    (when (and (not (interactive-p)) pr-menu-lock)
      (or (and pr-menu-position (eq state pr-menu-state))
	  (setq pr-menu-position (pr-menu-position entry index horizontal)
		pr-menu-state    state))
      (let* ((menu   (pr-menu-lookup path))
	     (result (x-popup-menu pr-menu-position menu)))
	(and result
	     (let ((command (lookup-key menu (vconcat result))))
	       (if (fboundp command)
		   (funcall command)
		 (eval command)))))
      (setq pr-menu-position nil)))

  ;; GNU Emacs
  (defun pr-menu-lookup (path)
    (let ((ipath [menu-bar tools Printing]))
      (lookup-key global-map
		  (if path
		      (vconcat ipath
			       (mapcar 'pr-get-symbol
				       (if (listp path)
					   path
					 (list path))))
		    ipath)))))


 ((eq ps-print-emacs-type 'xemacs)
  ;; XEmacs
  (defun pr-menu-lock (entry index horizontal state path)
    (when (and (not (interactive-p)) pr-menu-lock)
      (or (and pr-menu-position (eq state pr-menu-state))
	  (setq pr-menu-position (pr-menu-position entry index horizontal)
		pr-menu-state    state))
      (let* ((menu   (pr-menu-lookup path))
	     (result (get-popup-menu-response menu pr-menu-position)))
	(and (misc-user-event-p result)
	     (funcall (event-function result) (event-object result))))
      (setq pr-menu-position nil)))

  ;; XEmacs
  (defun pr-menu-lookup (path)
    (car (find-menu-item current-menubar (cons "Printing" path))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printer & Utility Selection (II)


(defvar pr-ps-printer-menu-modified  t
  "Non-nil means `pr-ps-printer-alist' was modified and need to update menu.")
(defvar pr-txt-printer-menu-modified t
  "Non-nil means `pr-txt-printer-alist' was modified and need to update menu.")
(defvar pr-ps-utility-menu-modified t
  "Non-nil means `pr-ps-utility-alist' was modified and need to update menu.")


(defun pr-menu-create (name alist var-sym fun entry index)
  (cons name
	(mapcar
	 #'(lambda (elt)
	     (let ((sym (car elt)))
	       (vector
		(symbol-name sym)
		(list fun (list 'quote sym) nil (list 'quote entry) index)
		:style 'radio
		:selected (list 'eq var-sym (list 'quote sym)))))
	 alist)))


(cond
 ((eq ps-print-emacs-type 'emacs)
  ;; GNU Emacs
  (defalias 'pr-update-mode-line 'force-mode-line-update)

  ;; GNU Emacs
  (defun pr-do-update-menus (&optional force)
    (pr-menu-alist pr-ps-printer-alist
		   'pr-ps-name
		   'pr-menu-set-ps-title
		   "PostScript Printers"
		   'pr-ps-printer-menu-modified
		   force
		   "PostScript Printers"
		   'postscript 2)
    (pr-menu-alist pr-txt-printer-alist
		   'pr-txt-name
		   'pr-menu-set-txt-title
		   "Text Printers"
		   'pr-txt-printer-menu-modified
		   force
		   "Text Printers"
		   'text 2)
    (let ((save-var pr-ps-utility-menu-modified))
      (pr-menu-alist pr-ps-utility-alist
		     'pr-ps-utility
		     'pr-menu-set-utility-title
		     '("PostScript Print"   "File" "PostScript Utility")
		     'save-var
		     force
		     "PostScript Utility"
		     nil 1))
    (pr-menu-alist pr-ps-utility-alist
		   'pr-ps-utility
		   'pr-menu-set-utility-title
		   '("PostScript Preview" "File" "PostScript Utility")
		   'pr-ps-utility-menu-modified
		   force
		   "PostScript Utility"
		   nil 1))

  ;; GNU Emacs
  (defvar pr-temp-menu nil)

  ;; GNU Emacs
  (defun pr-menu-alist (alist var-sym fun menu-path modified-sym force name
			      entry index)
    (when (and alist (or force (symbol-value modified-sym)))
      (easy-menu-define pr-temp-menu nil ""
			(pr-menu-create name alist var-sym fun entry index))
      (let ((item (pr-menu-get-item menu-path)))
	(and item
	     (let* ((binding     (nthcdr 3 item))
		    (key-binding (cdr binding)))
	       (setcar binding pr-temp-menu)
	       (and key-binding (listp (car key-binding))
		    (setcdr binding (cdr key-binding))) ; skip KEY-BINDING
	       (funcall fun (symbol-value var-sym) item))))
      (set modified-sym nil)))

  ;; GNU Emacs
  (defun pr-menu-set-ps-title (value &optional item entry index)
    (pr-menu-set-item-name (or item
			       (pr-menu-get-item "PostScript Printers"))
			   (format "PostScript Printer: %s" value))
    (pr-ps-set-printer value)
    (and index
	 (pr-menu-lock entry index 12 'toggle nil)))

  ;; GNU Emacs
  (defun pr-menu-set-txt-title (value &optional item entry index)
    (pr-menu-set-item-name (or item
			       (pr-menu-get-item "Text Printers"))
			   (format "Text Printer: %s" value))
    (pr-txt-set-printer value)
    (and index
	 (pr-menu-lock entry index 12 'toggle nil)))

  ;; GNU Emacs
  (defun pr-menu-set-utility-title (value &optional item entry index)
    (let ((name (symbol-name value)))
      (if item
	  (pr-menu-set-item-name item name)
	(pr-menu-set-item-name
	 (pr-menu-get-item
	  '("PostScript Print"   "File" "PostScript Utility"))
	 name)
	(pr-menu-set-item-name
	 (pr-menu-get-item
	  '("PostScript Preview" "File" "PostScript Utility"))
	 name)))
    (pr-ps-set-utility value)
    (and index
	 (pr-menu-lock entry index 5 nil '("PostScript Print" "File")))))


 ((eq ps-print-emacs-type 'xemacs)
  ;; XEmacs
  (defalias 'pr-update-mode-line 'set-menubar-dirty-flag)

  ;; XEmacs
  (defvar pr-ps-name-old    "PostScript Printers")
  (defvar pr-txt-name-old   "Text Printers")
  (defvar pr-ps-utility-old "PostScript Utility")

  ;; XEmacs
  (defun pr-do-update-menus (&optional force)
    (pr-menu-alist pr-ps-printer-alist
		   'pr-ps-name
		   'pr-menu-set-ps-title
		   '("Printing")
		   'pr-ps-printer-menu-modified
		   force
		   pr-ps-name-old
		   'postscript 2)
    (pr-menu-alist pr-txt-printer-alist
		   'pr-txt-name
		   'pr-menu-set-txt-title
		   '("Printing")
		   'pr-txt-printer-menu-modified
		   force
		   pr-txt-name-old
		   'text 2)
    (let ((save-var pr-ps-utility-menu-modified))
      (pr-menu-alist pr-ps-utility-alist
		     'pr-ps-utility
		     'pr-menu-set-utility-title
		     '("Printing" "PostScript Print"   "File")
		     'save-var
		     force
		     pr-ps-utility-old
		     nil 1))
    (pr-menu-alist pr-ps-utility-alist
		   'pr-ps-utility
		   'pr-menu-set-utility-title
		   '("Printing" "PostScript Preview" "File")
		   'pr-ps-utility-menu-modified
		   force
		   pr-ps-utility-old
		   nil 1))

  ;; XEmacs
  (defun pr-menu-alist (alist var-sym fun menu-path modified-sym force name
			      entry index)
    (when (and alist (or force (symbol-value modified-sym)))
      (pr-xemacs-global-menubar
       (add-submenu menu-path
		    (pr-menu-create name alist var-sym fun entry index)))
      (funcall fun (symbol-value var-sym))
      (set modified-sym nil)))

  ;; XEmacs
  (defun pr-menu-set-ps-title (value &optional item entry index)
    (pr-xemacs-global-menubar
     (let ((newname (format "PostScript Printer: %s" value)))
       (relabel-menu-item
	(list "Printing" pr-ps-name-old)
	newname)
       (setq pr-ps-name-old newname)))
    (pr-ps-set-printer value)
    (and index
	 (pr-menu-lock entry index 12 'toggle nil)))

  ;; XEmacs
  (defun pr-menu-set-txt-title (value &optional item entry index)
    (pr-xemacs-global-menubar
     (let ((newname (format "Text Printer: %s" value)))
       (relabel-menu-item
	(list "Printing" pr-txt-name-old)
	newname)
       (setq pr-txt-name-old newname)))
    (pr-txt-set-printer value)
    (and index
	 (pr-menu-lock entry index 12 'toggle nil)))

  ;; XEmacs
  (defun pr-menu-set-utility-title (value &optional item entry index)
    (pr-xemacs-global-menubar
     (let ((newname (format "%s" value)))
       (relabel-menu-item
	(list "Printing" "PostScript Print" "File" pr-ps-utility-old)
	newname)
       (relabel-menu-item
	(list "Printing" "PostScript Preview" "File" pr-ps-utility-old)
	newname)
       (setq pr-ps-utility-old newname)))
    (pr-ps-set-utility value)
    (and index
	 (pr-menu-lock entry index 5 nil '("PostScript Print" "File"))))))


(defun pr-menu-set-item-name (item name)
  (and item
       (setcar (nthcdr 2 item) name)))	; ITEM-NAME


(defun pr-menu-get-item (name-list)
  ;; NAME-LIST is a string or a list of strings.
  (let ((ipath [menu-bar tools Printing])
	(len   (and (listp name-list) (length name-list))))
    (and len (= len 1)
	 (setq name-list (car name-list)))
    (cond
     ((null name-list)
      ;; nil
      nil)
     ((listp name-list)
      ;; list and (length list) > 1
      (let* ((copy (copy-sequence name-list))
	     (name (pr-get-symbol (nth (1- len) copy)))
	     (path (progn
		     (setcdr (nthcdr (- len 2) copy) nil)
		     copy))
	     (menu (lookup-key
		    global-map
		    (if path
			(vconcat ipath
				 (mapcar 'pr-get-symbol path))
		      ipath))))
	(assq name (nthcdr 2 menu))))
     (t
      ;; string
      (let ((name (pr-get-symbol name-list))
	    (menu (lookup-key global-map ipath)))
	(assq name (nthcdr 2 menu)))))))


(defun pr-get-symbol (name)
  (or (intern-soft name)
      (make-symbol name)))


(defun pr-ps-set-utility (value)
  (let ((item (cdr (assq value pr-ps-utility-alist))))
    (or item
	(error
	 "Invalid PostScript utility name `%s' for variable `pr-ps-utility'."
	 value))
    (setq pr-ps-utility value)
    (pr-eval-alist (nthcdr 8 item)))
  (pr-update-mode-line))


(defun pr-ps-set-printer (value)
  (let ((ps (cdr (assq value pr-ps-printer-alist))))
    (or ps
	(error
	 "Invalid PostScript printer name `%s' for variable `pr-ps-name'."
	 value))
    (setq pr-ps-name           value
	  pr-ps-command        (pr-dosify-path (nth 0 ps))
	  pr-ps-switches       (nth 1 ps)
	  pr-ps-printer-switch (nth 2 ps)
	  pr-ps-printer        (pr-dosify-path (nth 3 ps)))
    (and (string-equal pr-ps-command "")
	 (setq pr-ps-command
	       (cond (ps-windows-system "print")
		     (ps-lp-system      "lp")
		     (t                 "lpr")
		     )))
    (and (string-equal pr-ps-printer-switch "")
	 (setq pr-ps-printer-switch
	       (cond (ps-windows-system "/D:")
		     (ps-lp-system      "-d")
		     (t                 "-P")
		     )))
    (pr-eval-alist (nthcdr 4 ps)))
  (pr-update-mode-line))


(defun pr-txt-set-printer (value)
  (let ((txt (cdr (assq value pr-txt-printer-alist))))
    (or txt
	(error "Invalid text printer name `%s' for variable `pr-txt-name'."
	       value))
    (setq pr-txt-name     value
	  pr-txt-command  (pr-dosify-path (nth 0 txt))
	  pr-txt-switches (nth 1 txt)
	  pr-txt-printer  (pr-dosify-path (nth 2 txt))))
  (and (string-equal pr-txt-command "")
       (setq pr-txt-command
	     (cond (ps-windows-system "print")
		   (ps-lp-system      "lp")
		   (t                 "lpr")
		   )))
  (pr-update-mode-line))


(defun pr-eval-alist (alist)
  (mapcar #'(lambda (option)
	      (let ((var-sym (car option))
		    (value   (cdr option)))
		(if (eq var-sym :inherits-from)
		    (pr-eval-setting-alist value 'global)
		  (set var-sym (eval value)))))
	  alist))


(defun pr-eval-local-alist (alist)
  (let (local-list)
    (mapcar #'(lambda (option)
		(let ((var-sym (car option))
		      (value   (cdr option)))
		  (setq local-list
			(if (eq var-sym :inherits-from)
			    (nconc (pr-eval-setting-alist value) local-list)
			  (set (make-local-variable var-sym) (eval value))
			  (cons var-sym local-list)))))
	    alist)
    local-list))


(defun pr-eval-setting-alist (key &optional global old)
  (let ((setting (cdr (assq key pr-setting-database))))
    (and setting
	 (let ((inherits (nth 0 setting))
	       (local    (nth 1 setting))
	       (kill     (nth 2 setting))
	       local-list)
	   (and local global
		(progn
		  (ding)
		  (message "There are local buffer settings for `%S'." key)
		  (setq global nil)))
	   (and inherits
		(if (memq inherits old)
		    (error "Circular inheritance for `%S'." inherits)
		  (setq local-list
			(pr-eval-setting-alist inherits global
					       (cons inherits old)))))
	   (mapcar
	    (cond ((not local)		; global settings
		   #'(lambda (option)
		       (let ((var-sym (car option)))
			 (or (eq var-sym :inherits-from)
			     (set var-sym (eval (cdr option)))))))
		  (kill			; local settings with killing
		   #'(lambda (option)
		       (let ((var-sym (car option)))
			 (unless (eq var-sym :inherits-from)
			   (setq local-list (cons var-sym local-list))
			   (set (make-local-variable var-sym)
				(eval (cdr option)))))))
		  (t			; local settings without killing
		   #'(lambda (option)
		       (let ((var-sym (car option)))
			 (or (eq var-sym :inherits-from)
			     (set (make-local-variable var-sym)
				  (eval (cdr option))))))))
	    (nthcdr 3 setting))
	   local-list))))


(defun pr-kill-local-variable (local-var-list)
  (mapcar 'kill-local-variable local-var-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Functions (II)


(defun pr-region-active-symbol (&optional region-p)
  (if (or region-p (pr-region-active-p))
      'region
    'buffer))


(defun pr-region-active-string (prefix)
  (concat prefix
	  (if (pr-region-active-p)
	      " region"
	    " buffer")))


(defun pr-show-setup (settings buffer-name)
  (with-output-to-temp-buffer buffer-name
    (princ settings)
    (print-help-return-message)))


(defun pr-complete-alist (prompt alist default)
  (let ((collection (mapcar #'(lambda (elt)
				(setq elt (car elt))
				(cons (symbol-name elt) elt))
			    alist)))
    (cdr (assoc (completing-read (concat prompt ": ")
				 collection nil t
				 (symbol-name default) nil
				 (symbol-name default))
		collection))))


(defun pr-expand-file-name (filename)
  (pr-dosify-path (expand-file-name filename)))


(defun pr-ps-outfile-preprint (&optional mess)
  (let* ((prompt (format "%soutput PostScript file name: " (or mess "")))
	 (res    (read-file-name prompt default-directory "" nil)))
    (while (cond ((not (file-writable-p res))
		  (ding)
		  (setq prompt "is unwritable"))
		 ((file-directory-p res)
		  (ding)
		  (setq prompt "is a directory"))
		 ((file-exists-p res)
		  (ding)
		  (setq prompt "exists")
		  (not (y-or-n-p (format "File `%s' exists; overwrite? "
					 res))))
		 (t nil))
      (setq res (read-file-name
		 (format "File %s; PostScript file: " prompt)
		 (file-name-directory res) nil nil
		 (file-name-nondirectory res))))
    (pr-expand-file-name res)))


(defun pr-ps-infile-preprint (&optional mess)
  (let* ((prompt (format "%sinput PostScript file name: " (or mess "")))
	 (res    (read-file-name prompt default-directory "" nil)))
    (while (cond ((not (file-exists-p res))
		  (ding)
		  (setq prompt "doesn't exist"))
		 ((not (file-readable-p res))
		  (ding)
		  (setq prompt "is unreadable"))
		 ((file-directory-p res)
		  (ding)
		  (setq prompt "is a directory"))
		 (t nil))
      (setq res (read-file-name
		 (format "File %s; PostScript file: " prompt)
		 (file-name-directory res) nil nil
		 (file-name-nondirectory res))))
    (pr-expand-file-name res)))


(defun pr-toggle (var-sym mess entry index horizontal state &optional path)
  (set var-sym (not (symbol-value var-sym)))
  (message "%s is %s" mess (if (symbol-value var-sym) "on" "off"))
  (pr-menu-lock entry index horizontal state path))


(defun pr-ps-utility-args (n-up-sym infile-sym outfile-sym prompt)
  ;; n-up
  (or (symbol-value n-up-sym)
      (set n-up-sym (pr-interactive-n-up prompt)))
  (and (eq (symbol-value infile-sym) t)
       (set infile-sym (and (not (interactive-p))
			    (pr-ps-infile-preprint prompt))))
  ;; input file
  (or (symbol-value infile-sym)
      (error "%s: input PostScript file name is missing" prompt))
  (set infile-sym (pr-dosify-path (symbol-value infile-sym)))
  ;; output file
  (and (eq (symbol-value outfile-sym) t)
       (set outfile-sym (and (not (interactive-p))
			     current-prefix-arg
			     (pr-ps-outfile-preprint prompt))))
  (and (symbol-value outfile-sym)
       (set outfile-sym (pr-dosify-path (symbol-value outfile-sym))))
  (pr-ps-file (symbol-value outfile-sym)))


(defun pr-ps-utility-process (n-up infile outfile)
  (let (item)
    (and (stringp infile) (file-exists-p infile)
	 (setq item (cdr (assq pr-ps-utility pr-ps-utility-alist)))
	 (shell-command
	  (concat (nth 0 item) " "
		  (pr-switches-string (nth 7 item) "pr-ps-utility-alist entry")
		  " "
		  (and (nth 1 item)
		       (format (nth 1 item) ps-paper-type))
		  " " (format (nth 2 item) n-up) " "
		  (and pr-file-landscape (nth 3 item)) " "
		  (and pr-file-duplex    (nth 4 item)) " "
		  (and pr-file-tumble    (nth 5 item))
		  " \"" (pr-expand-file-name infile) "\" "
		  (nth 6 item)
		  " \"" (pr-expand-file-name outfile) "\"")))))


(defun pr-txt-print (from to)
  (let ((lpr-command  pr-txt-command)
	(lpr-switches (pr-switches pr-txt-switches "pr-txt-switches"))
	(printer-name pr-txt-printer))
    (lpr-region from to)))


(defun pr-switches-string (switches mess)
  (mapconcat 'identity (pr-switches switches mess) " "))


(defun pr-switches (switches mess)
  (or (listp switches)
      (error "%S should have a list of strings." mess))
  (ps-flatten-list			; dynamic evaluation
   (mapcar 'ps-eval-switch switches)))


(defun pr-ps-preview (kind n-up filename mess)
  (pr-set-n-up-and-filename 'n-up 'filename mess)
  (let ((file (pr-ps-file filename)))
    (pr-text2ps kind n-up file)
    (or pr-spool-p (pr-ps-file-preview file))))


(defun pr-ps-using-ghostscript (kind n-up filename mess)
  (pr-set-n-up-and-filename 'n-up 'filename mess)
  (let ((file (pr-ps-file filename)))
    (pr-text2ps kind n-up file)
    (unless (or pr-spool-p filename)
      (pr-ps-file-using-ghostscript file)
      (delete-file file))))


(defun pr-ps-print (kind n-up filename mess)
  (pr-set-n-up-and-filename 'n-up 'filename mess)
  (let ((file (pr-ps-file filename)))
    (pr-text2ps kind n-up file)
    (unless (or pr-spool-p filename)
      (pr-ps-file-print file)
      (delete-file file))))


(defun pr-ps-file (&optional filename)
  (pr-dosify-path (or filename
		      (concat pr-temp-dir pr-ps-temp-file))))


(defun pr-interactive-n-up (mess)
  (or (stringp mess) (setq mess "*"))
  (save-match-data
    (let* ((fmt-prompt "%s[%s] N-up printing: (default 1) ")
	   (prompt "")
	   (str (read-string (format fmt-prompt prompt mess) "1" nil "1"))
	   int)
      (while (if (string-match "^\\s *[0-9]+$" str)
		 (setq int (string-to-int str)
		       prompt (cond ((< int 1)   "Integer below 1; ")
				    ((> int 100) "Integer above 100; ")
				    (t           nil)))
	       (setq prompt "Invalid integer syntax; "))
	(ding)
	(setq str (read-string (format fmt-prompt prompt mess) str nil "1")))
      int)))


(defun pr-set-n-up-and-filename (n-up-sym filename-sym mess)
  ;; n-up
  (or (symbol-value n-up-sym)
      (set n-up-sym (pr-interactive-n-up mess)))
  ;; output file
  (and (not pr-spool-p)
       (eq (symbol-value filename-sym) t)
       (set filename-sym (and (not (interactive-p))
			      current-prefix-arg
			      (ps-print-preprint current-prefix-arg))))
  (and (symbol-value filename-sym)
       (set filename-sym (pr-dosify-path (symbol-value filename-sym)))))


(defun pr-text2ps (kind n-up filename &optional from to)
  (let ((ps-n-up-printing n-up)
	ps-spool-config)
    (and (not pr-spool-p) (stringp filename) (file-exists-p filename)
	 (delete-file filename))
    (cond (pr-faces-p
	   (cond (pr-spool-p
		  ;; pr-faces-p and pr-spool-p
		  ;; here FILENAME arg is ignored
		  (cond ((eq kind 'buffer)
			 (ps-spool-buffer-with-faces))
			((eq kind 'region)
			 (ps-spool-region-with-faces (or from (point))
						     (or to (mark))))
			))
		  ;; pr-faces-p and not pr-spool-p
		 ((eq kind 'buffer)
		  (ps-print-buffer-with-faces filename))
		 ((eq kind 'region)
		  (ps-print-region-with-faces (or from (point))
					      (or to (mark)) filename))
		 ))
	  (pr-spool-p
	   ;; not pr-faces-p and pr-spool-p
	   ;; here FILENAME arg is ignored
	   (cond ((eq kind 'buffer)
		  (ps-spool-buffer))
		 ((eq kind 'region)
		  (ps-spool-region (or from (point)) (or to (mark))))
		 ))
	  ;; not pr-faces-p and not pr-spool-p
	  ((eq kind 'buffer)
	   (ps-print-buffer filename))
	  ((eq kind 'region)
	   (ps-print-region (or from (point)) (or to (mark)) filename))
	  )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(pr-update-menus t)


(provide 'printing)


;;; printing.el ends here
