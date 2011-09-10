;;; fuse-toolbar.el --- toolbar support for FUSE

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  16 February 1998
;; Version:  0.5.10
;; Keywords:  feff, uwxafs, input files, atoms, autobk, feffit

;; $Id: fuse-toolbar.el,v 1.2 1998/03/14 22:45:59 bruce Exp $

;; Copyright (C) 1998 Bruce Ravel

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;; Everyone is granted permission to copy, modify and redistribute this
;; and related files provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;;; Commentary:
;; this is just a bunch of variable definitions

;;; Change-log:

;;; Code:

(require 'xpm)

(defvar Atoms-toolbar-eval-buffer-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *buffer[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        8            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #dcdcdc\",
\"b c #bebebe s backgroundToolBarColor\",
\"c c #2f4f4f\",
\"d c #ff0000\",
\"e c #ffd700\",
\"f c #708090\",
/* pixels */
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbb..bbbbbbbbbb......bbbbbbbbbbbbbb\",
\"bbbbbbbb..bbbbbbbbb..b..bbbbbbbbbbbbbbbb\",
\"bbbbbbb....bb..b.........b...bb.b.bbbbbb\",
\"bbbbbbb..b..b..b..b..b..b..b..b...bbbbbb\",
\"bbbbbbb.bb....bb..b..b..b.....b..bbbbbbb\",
\"bbbbbb..bb.b..b..b..b..b..bbbb..bbbbbbbb\",
\"bbbbbb..b..b..b..b..b..b..b..b..bbbbbbbb\",
\"bbbbbb....bbb..b.b..b..bb...bb..bbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbb..bbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbb....bbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbb.a..a.bbbbbbbbbb...b.....bbbb\",
\"bbbbbbbbbbb.a..a.bbbbbbbbbb...b.....bbbb\",
\"bbbbbbbbbb.aa..aa.bbbbbbbbb.........bbbb\",
\"bbbbbbbbbb.aa..aa.bbbbbbbbbb..aaaa.bbbbb\",
\"bbbbbbbbbb.aaaaaa.bbbbbbbbbb..aaaa.bbbbb\",
\"bbbb.......aa..aa............aaaaa.bbbbb\",
\"bbbb.eeeeee.a..a.eeeeeeee.aaaaaaaa.bbbbb\",
\"bbbb.eeeeee.aaaa.eeeeeeee.a..aaaaa.bbbbb\",
\"bbbb.eeeeee..aaaa.eeeeeee.aaaaaaaa.bbbbb\",
\"bbbb........a.aaa............aaaaa.bbbbb\",
\"bbbbbbbbbb.aa..aaa.bbbbbbbbb.......bbbbb\",
\"bbbbbbbbb.aa.bb.aaa.bbbbbbbbb.aaaa.bbbbb\",
\"bbbbbbbb.aa.bbbb.aa.bbbbbbbbb.aaa..bbbbb\",
\"bbbbbfbf.dd.fbfb.dd.fbfbfbfb.a.a..bbbbbb\",
\"bbbbbfff.dd.ffff.dd.fffffff.a.a..bbbbbbb\",
\"bbbbbbbb.dd.bbbb.dd.bbbbbb.a.a..bbbbbbbb\",
\"bbbbbbbb.dd.bbbb.dd.bbbb......bbfbffbbbb\",
\"bbbbbbbb.dd.bbbb.dd.bbbbbbbbbbffffffbbbb\",
\"bbbbbbbbb.d.bbbb.d.bbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbb..fbfb..fbfbfbfbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbfffffffbffffffbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"};"))
  "XPM format image used for the \"____\" button")


(defvar Atoms-toolbar-eval-line-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *line[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        8            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #dcdcdc\",
\"b c #bebebe s backgroundToolBarColor\",
\"c c #2f4f4f\",
\"d c #ff0000\",
\"e c #ffd700\",
\"f c #708090\",
/* pixels */
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbb..b..bbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbb..bbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbb..b..b....bbb...bbbbbbbbbbbbbbb\",
\"bbbbbbbbb..b..b..b..b..b..bbbbbbbbbbbbbb\",
\"bbbbbbbbb..b..b.bb..b.....bbbbbbbbbbbbbb\",
\"bbbbbbbb..b..b..b..b..bbbbbbbbbbbbbbbbbb\",
\"bbbbbbbb..b..b..b..b..b..bbbbbbbbbbbbbbb\",
\"bbbbbbbb..b..b..b..bb...bbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbb..bbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbb....bbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbb.a..a.bbbbbbbbbb...b.....bbbb\",
\"bbbbbbbbbbb.a..a.bbbbbbbbbb...b.....bbbb\",
\"bbbbbbbbbb.aa..aa.bbbbbbbbb.........bbbb\",
\"bbbbbbbbbb.aa..aa.bbbbbbbbbb..aaaa.bbbbb\",
\"bbbbbbbbbb.aaaaaa.bbbbbbbbbb..aaaa.bbbbb\",
\"bbbb.......aa..aa............aaaaa.bbbbb\",
\"bbbb.eeeeee.a..a.eeeeeeee.aaaaaaaa.bbbbb\",
\"bbbb.eeeeee.aaaa.eeeeeeee.a..aaaaa.bbbbb\",
\"bbbb.eeeeee..aaaa.eeeeeee.aaaaaaaa.bbbbb\",
\"bbbb........a.aaa............aaaaa.bbbbb\",
\"bbbbbbbbbb.aa..aaa.bbbbbbbbb.......bbbbb\",
\"bbbbbbbbb.aa.bb.aaa.bbbbbbbbb.aaaa.bbbbb\",
\"bbbbbbbb.aa.bbbb.aa.bbbbbbbbb.aaa..bbbbb\",
\"bbbbbfbf.dd.fbfb.dd.fbfbfbfb.a.a..bbbbbb\",
\"bbbbbfff.dd.ffff.dd.fffffff.a.a..bbbbbbb\",
\"bbbbbbbb.dd.bbbb.dd.bbbbbb.a.a..bbbbbbbb\",
\"bbbbbbbb.dd.bbbb.dd.bbbb......bbfbffbbbb\",
\"bbbbbbbb.dd.bbbb.dd.bbbbbbbbbbffffffbbbb\",
\"bbbbbbbbb.d.bbbb.d.bbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbb..fbfb..fbfbfbfbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbfffffffbffffffbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",
\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"};"))
  "XPM format image used for the \"____\" button")


(defvar Autobk-toolbar-all-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Autobk-toolbar-all[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        7            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
\"e c #000000\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaa..a.aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaa..a.aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa...aa..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa.aa..a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa....a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..a..a..a..aaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..a..a..a..aaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa..a....a..aaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Autobk-toolbar-chi-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *chi[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaa..aaaa.aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaa..aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa...a....aa..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..a.a..a..a..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..aaaa.aa..a..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..aaa..a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..a....a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa...a..a..a..aaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Autobk-toolbar-xmu-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *xmu[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..aa...a..a..aa..a..aaaaaaaa\",
\"aaaaaaaaaaaaa....a..a..a..a..a..aaaaaaaa\",
\"aaaaaaaaaaaaaa..aa..a..a....aa..aaaaaaaa\",
\"aaaaaaaaaaaaa...a..a..a..a..a..aaaaaaaaa\",
\"aaaaaaaaaaaa..a....a..a..a..a..aaaaaaaaa\",
\"aaaaaaaaaaaa.aa....a..a..aa..a.aaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Diffkk-toolbar-fp-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * Diffkk_toolbar_fp_up_xpm[] = {
\"40 40 13 1\",
\" 	c #E79DE79DFFFF\",
\".	c #28A24D344924\",
\"X	c #BEFBBEFBBEFB s backgroundToolBarColor\",
\"o	c #000000000000\",
\"O	c #965896589658\",
\"+	c #104014511040\",
\"@	c #38E33CF338E3\",
\"#	c #514455555144\",
\"$	c #28A228A228A2\",
\"%	c #69A669A669A6\",
\"&	c #79E77DF779E7\",
\"*	c #00000000FFFF\",
\"=	c #FFFF61854103\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXO+@Xo@XXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXX#$XO%&XXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXooXXXXX&oo&XXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXX+%XXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXo&XXXXXXXXXXXXXX*XXXXXXXX\",
\"XXXXXXXXoXXXXX&@XXXXXXXXXXXXXX*XXXXXXXXX\",
\"XXXXXXXooXXXXX#%XXXXXXXXXXXXX*XXXXXXXXXX\",
\"XXXXXXXXoXXXXX%OXXXXXXXXXXXX*XXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXX=XXXX*XX*XXXXXXXXXXXX\",
\"XXXXXXXXo=XXX*XXX==X==X*X**XXXXX==XXXXXX\",
\"XXXXXXXooX==*X**=XXXXX==XXXXXX==XXXXXXXX\",
\"XXXXXXXXoXX*=X==*XXXX*XX==XX==XXXXXXXXXX\",
\"XXXXXXXXoX*XX=XXX**X*XXXXX==XXXXXXXXXXXX\",
\"XXXXXXXXo*XXXXXXXXX*XXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXooXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXooooooooooooooooooooooooooooXXXXXX\",
\"XXXXXXXXoXXXXoXXXXoXXXXoXXXXoXXXXXXXXXXX\",
\"XXXXXXXXoXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"};"))
  "XPM format image used for the \"____\" button")


(defvar Diffkk-toolbar-fpp-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char * Diffkk_toolbar_fpp_up_xpm[] = {
\"40 40 14 1\",
\" 	c #E79DE79DFFFF\",
\".	c #28A24D344924\",
\"X	c #BEFBBEFBBEFB s backgroundToolBarColor\",
\"o	c #965896589658\",
\"O	c #104014511040\",
\"+	c #38E33CF338E3\",
\"@	c #000000000000\",
\"#	c #79E77DF779E7\",
\"$	c #514455555144\",
\"%	c #28A228A228A2\",
\"&	c #69A669A669A6\",
\"*	c #AEBAAAAAAEBA\",
\"=	c #00000000FFFF\",
\"-	c #FFFF61854103\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXoO+X@##XXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXX$%X#&$*XXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXX#@@#XXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXX@@XXXXXXXO&XXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXX@#XXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXX#+XXXXXXXXXXXXXX=XXXXXXXX\",
\"XXXXXXXX@XXXXXX$&XXXXXXXXXXXXX=XXXXXXXXX\",
\"XXXXXXX@@XXXXXX&oXXXXXXXXXXXX=XXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXX=XXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXX-XXXX=XX=XXXXXXXXXXXX\",
\"XXXXXXXX@-XXX=XXX--X--X=X==XXXXX--XXXXXX\",
\"XXXXXXX@@X--=X==-XXXXX--XXXXXX--XXXXXXXX\",
\"XXXXXXXX@XX=-X--=XXXX=XX--XX--XXXXXXXXXX\",
\"XXXXXXXX@X=XX-XXX==X=XXXXX--XXXXXXXXXXXX\",
\"XXXXXXXX@=XXXXXXXXX=XXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXX@@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXX@@@@@@@@@@@@@@@@@@@@@@@@@@@@XXXXXX\",
\"XXXXXXXX@XXXX@XXXX@XXXX@XXXX@XXXXXXXXXXX\",
\"XXXXXXXX@XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\",
\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"};"))
  "XPM format image used for the \"____\" button")


(defvar Feff-toolbar-chi-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *chi[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaa..aaaa.aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaa..aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa...a....aa..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..a.a..a..a..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..aaaa.aa..a..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..aaa..a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..a....a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa...a..a..a..aaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Feff-toolbar-dos-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Feff-toolbar-dos[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa.....aaaa....aaaa....aaaaaaa\",
\"aaaaaaaaaaaa..aa..aa..aa..aa..aa.aaaaaaa\",
\"aaaaaaaaaaa..aaa..a..aaa..aa..aaaaaaaaaa\",
\"aaaaaaaaaaa..aaa..a..aaa..aa...aaaaaaaaa\",
\"aaaaaaaaaaa..aa..a..aaa..aaaa...aaaaaaaa\",
\"aaaaaaaaaa..aaa..a..aaa..a..aa..aaaaaaaa\",
\"aaaaaaaaaa..aa..aa..aa..aa..aa..aaaaaaaa\",
\"aaaaaaaaaa.....aaaa....aaaa....aaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Feff-toolbar-xmu-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *xmu[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa..aa...a..a..aa..a..aaaaaaaa\",
\"aaaaaaaaaaaaa....a..a..a..a..a..aaaaaaaa\",
\"aaaaaaaaaaaaaa..aa..a..a....aa..aaaaaaaa\",
\"aaaaaaaaaaaaa...a..a..a..a..a..aaaaaaaaa\",
\"aaaaaaaaaaaa..a....a..a..a..a..aaaaaaaaa\",
\"aaaaaaaaaaaa.aa....a..a..aa..a.aaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


;;============================================================

(defvar Feffit-toolbar-k-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Feffit-toolbar-k[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa...a.aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaa.aa.aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa.a.aaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa.aa.aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa..aa..aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Feffit-toolbar-log-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Feffit-toolbar-log[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        8            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff00ff\",
\"d c #ff0000\",
\"e c #ffffff\",
\"f c #ffff00\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaa.....aaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaa.ddddd.aaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.ddddddd.aaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.ddddddc.aaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.dcceecd.aaaaa..aaaaaaaaaaaaaaaa\",
\"aaaaaaaaa.ddeed.aaaa..fe.aaaaaaaaaaaaaaa\",
\"aaaaaaaaaa.....aaa..efef.aaaaaaaaaaaaaaa\",
\"aaaaaaaaa..ddd....fefefef.aaaaaaaaaaaaaa\",
\"aaaaaaaa.d.dde.d.fefefefe.aaaaaaaaaaaaaa\",
\"aaaaaaa.dd.ddc.dc.fefefefe.aaaaaaaaaaaaa\",
\"aaaaaaa.ddd...ddc.efefefef.aaaaaaaaaaaaa\",
\"aaaaaaa.dcdddddcd.fefefefef.aaaaaaaaaaaa\",
\"aaaaaa..ddcceecdd.efefefefe.aaaaaaaaaaaa\",
\"aaaa..a..ddddddd.efefefefefe.aaaaaaaaaaa\",
\"aaaa.aaa........efefefefefef.aaaaaaaaaaa\",
\"aaaaa.a.a.a..efefefefefaaefef.aaaaaaaaaa\",
\"aaaaa.aa.a.a.fe..fefeaafefefe.aaaaaaaaaa\",
\"aaaaaa.aa.a.aef..afaaefefefefe.aaaaaaaaa\",
\"aaaaaa.aaa.aef..ea...aef....ef.aaaaaaaaa\",
\"aaaaaaa.aefefe..a..e..f..e..fef.aaaaaaaa\",
\"aaaaaaa.aaefea....ef..a.ea..efef.aaaaaaa\",
\"aaaaaaaa.afef..e..fa....fe.eaafef...aaaa\",
\"aaaaaaaa.aafe..f..a..f..e..aefefefe.aaaa\",
\"aaaaaaaaa.aef..af...faf....efefefe.aaaaa\",
\"aaaaaaaaa.aaefefefefefaa..afefefe.aaaaaa\",
\"aaaaaaaaaa.afefefefeaa...efefef..aaaaaaa\",
\"aaaaaaaaaa..afefefefefefefefe..aaaaaaaaa\",
\"aaaaaaaaaaa....efefefefefef..aaaaaaaaaaa\",
\"aaaaaaaaaaaa.......fefefe..aaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..fee..fef..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa...f.....aaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa.....aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Feffit-toolbar-prm-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Feffit-toolbar-prm[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        8            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff00ff\",
\"d c #ff0000\",
\"e c #ffffff\",
\"f c #ffff00\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaa.....aaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaa.ddddd.aaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.ddddddd.aaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.ddddddc.aaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.dcceecd.aaaaa..aaaaaaaaaaaaaaaa\",
\"aaaaaaaaa.ddeed.aaaa..fe.aaaaaaaaaaaaaaa\",
\"aaaaaaaaaa.....aaa..efef.aaaaaaaaaaaaaaa\",
\"aaaaaaaaa..ddd....fefefef.aaaaaaaaaaaaaa\",
\"aaaaaaaa.d.dde.d.fefefefe.aaaaaaaaaaaaaa\",
\"aaaaaaa.dd.ddc.dc.fefefefe.aaaaaaaaaaaaa\",
\"aaaaaaa.ddd...ddc.efefefef.aaaaaaaaaaaaa\",
\"aaaaaaa.dcdddddcd.fefefefef.aaaaaaaaaaaa\",
\"aaaaaa..ddcceecdd.efefefefe.aaaaaaaaaaaa\",
\"aaaa..a..ddddddd.efefefefefe.aaaaaaaaaaa\",
\"aaaa.aaa........efefefefefef.aaaaaaaaaaa\",
\"aaaaa.a.a.a..efefefefefaaefef.aaaaaaaaaa\",
\"aaaaa.aa.a.a.fefefefeaafefefe.aaaaaaaaaa\",
\"aaaaaa.aa.a.aefefafaaefefefefe.aaaaaaaaa\",
\"aaaaaa.aaa.a....ea.f...f..a..f.aaaaaaaaa\",
\"aaaaaaa.aefe..f..e...e..a..e..f.aaaaaaaa\",
\"aaaaaaa.aaef.aa..a..ea..e..f..ef.aaaaaaa\",
\"aaaaaaaa.af..ef.f..aa..e..f..afef...aaaa\",
\"aaaaaaaa.aa..f..a..fe..f..a..fefefe.aaaa\",
\"aaaaaaaaa.a....af..af..e..f..efefe.aaaaa\",
\"aaaaaaaaa.a.efefefefefaaefafefefe.aaaaaa\",
\"aaaaaaaaaa..fefefefeaafefefefef..aaaaaaa\",
\"aaaaaaaaaa..afefefefefefefefe..aaaaaaaaa\",
\"aaaaaaaaaaa....efefefefefef..aaaaaaaaaaa\",
\"aaaaaaaaaaaa.......fefefe..aaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..fee..fef..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa...f.....aaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa.....aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Feffit-toolbar-q-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Feffit-toolbar-q[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaa..aaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa..aa.aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa..aaaa.aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa.aaaaa.aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa.aaaaa.aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa.aaa.aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Feffit-toolbar-r-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Feffit-toolbar-r[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa....aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaa.aa.aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaa.a..aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa.aa.aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa.aa.aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa...aa..aaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Normal-toolbar-all-nor-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Normal-toolbar-all-nor[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaa..a.aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaa..a.aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa...aa..a..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa.aa..a..a..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa....a..a..aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa..a..a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa..a..a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaa..a....a..aaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaa....aaa...aa.a...a..a..aaaaa\",
\"aaaaaaaaaaaa..a..a..a..a...a..a..a..aaaa\",
\"aaaaaaaaaaaa.aa....aa..a..aa..a..a..aaaa\",
\"aaaaaaaaaaa..a..a..aa....aa..a..a..aaaaa\",
\"aaaaaaaaaaa..a..a..a..a..aa..a..a..aaaaa\",
\"aaaaaaaaaaa..a..aa...aa..aa..a..a..aaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Normal-toolbar-all-xmu-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Normal-toolbar-all-xmu[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaa..a.aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaa..a.aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa...aa..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa.aa..a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa....a..a..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..a..a..a..aaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaa..a..a..a..aaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaa..a....a..aaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaa..aa...a..a..aa..a..aaaaaaaaaa\",
\"aaaaaaaaaaa....a..a..a..a..a..aaaaaaaaaa\",
\"aaaaaaaaaaaa..aa..a..a....aa..aaaaaaaaaa\",
\"aaaaaaaaaaa...a..a..a..a..a..aaaaaaaaaaa\",
\"aaaaaaaaaa..a....a..a..a..a..aaaaaaaaaaa\",
\"aaaaaaaaaa.aa....a..a..aa..a.aaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Normal-toolbar-this-nor-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Normal-toolbar-this-nor[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaa....aaa...aa.a...a..a..aaaaaaa\",
\"aaaaaaaaaa..a..a..a..a...a..a..a..aaaaaa\",
\"aaaaaaaaaa.aa....aa..a..aa..a..a..aaaaaa\",
\"aaaaaaaaa..a..a..aa....aa..a..a..aaaaaaa\",
\"aaaaaaaaa..a..a..a..a..aa..a..a..aaaaaaa\",
\"aaaaaaaaa..a..aa...aa..aa..a..a..aaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Normal-toolbar-this-xmu-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *Normal-toolbar-this-xmu[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaa..aa...a..a..aa..a..aaaaaaaaa\",
\"aaaaaaaaaaaa....a..a..a..a..a..aaaaaaaaa\",
\"aaaaaaaaaaaaa..aa..a..a....aa..aaaaaaaaa\",
\"aaaaaaaaaaaa...a..a..a..a..a..aaaaaaaaaa\",
\"aaaaaaaaaaa..a....a..a..a..a..aaaaaaaaaa\",
\"aaaaaaaaaaa.aa....a..a..aa..a.aaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar Phit-toolbar-plot-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *stats2[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff6347\",
\"d c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaadaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaadaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaadaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaadaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaacaaaadaadaaaaaaaaaaaa\",
\"aaaaaaaa.caaadaaaccaccadaddaaaaaccaaaaaa\",
\"aaaaaaa..accdaddcaaaaaccaaaaaaccaaaaaaaa\",
\"aaaaaaaa.aadcaccdaaaadaaccaaccaaaaaaaaaa\",
\"aaaaaaaa.adaacaaaddadaaaaaccaaaaaaaaaaaa\",
\"aaaaaaaa.daaaaaaaaadaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaa..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa............................aaaaaa\",
\"aaaaaaaa.aaaa.aaaa.aaaa.aaaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-bug-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *spiderbut[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        5            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff0000\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.aaaaaaa\",
\"aaaaaaaaaa.aaaaaaaaaaaaaaaaaaaaa.aaaaaaa\",
\"aaaaaaaaaa.aaaaaaaaaaaaaaaaaaaa..aaaaaaa\",
\"aaaaaaaaaa.aaaaaaaaaaaaaaaaaaa..aaaaaaaa\",
\"aaaaaaaaaa.aaaaaaaaaaaaaaaaaa..aaaaaaaaa\",
\"aaaaaaaaaa.aaaaaaaaaaaaaaaaaa.aaaaaaaaaa\",
\"aaaaaaaaaa..aaaaaa.a.aaaaaaa..aaaaa.aaaa\",
\"aaaaa..aaaa..aaaaa...aaaaaa..aaaa...aaaa\",
\"aaaaaa..aaaa..aaa.....aaaa..aaaa..aaaaaa\",
\"aaaaaaa..aaaa..aaa...aaaaa.aaaa..aaaaaaa\",
\"aaaaaaaa...aaa..aaaaaaaaa..aaa..aaaaaaaa\",
\"aaaaaaaaaa.aaaa..a...a....aa...aaaaaaaaa\",
\"aaaaaaaaaa...aaaa.....aaaaa..aaaaaaaaaaa\",
\"aaaaaaaaaaaa....a.....a.....aaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa.....aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaa......a...a.......aaaaaaaaaaa\",
\"aaaaaaaaaa..aaa.aaaaaaa..aaa...aaaaaaaaa\",
\"aaaaaaaaa..aa..aa.....aa..aaaa...aaaaaaa\",
\"aaaaaaaa..aa..aa.......aa.aaaaaa..aaaaaa\",
\"aaaaaa...aa..aa..ccccc..a..aaaaaa.aaaaaa\",
\"aaaaaa.aaa..aa...ccccc...a..aaaaaaaaaaaa\",
\"aaaaaaaaa..aaa....ccc....aaa.aaaaaaaaaaa\",
\"aaaaaaaa..aaaa.....c.....aaa.aaaaaaaaaaa\",
\"aaaaaaaa.aaaaa.....c.....aaa.aaaaaaaaaaa\",
\"aaaaaaa..aaaaa....ccc....aaa..aaaaaaaaaa\",
\"aaaaaaa.aaaaaaa..ccccc..aaaaa.aaaaaaaaaa\",
\"aaaaaa..aaaaaaaa.ccccc.aaaaaa..aaaaaaaaa\",
\"aaaaaa.aaaaaaaaaa.....aaaaaaaa.aaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.aaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-document-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *book_index[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        7            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff0000\",
\"d c #ffffff\",
\"e c #708090\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa.........bbeaaaebb..........aaaaaa\",
\"aaaaaa.ddddddddaaebebeaaddddddddd.aaaaaa\",
\"aaaa...dab.bddeebadbdaeebedeeeeed...aaaa\",
\"aaaa.c.dbaddddebeedbdeebeedebebed.c.aaaa\",
\"aaaa.c.d.de.edeebeabdbbeeddebbbed.c.aaaa\",
\"aaaa.c.dbad.ddebeadbdeeebeddeeeed.c.aaaa\",
\"aaaa.c.dab..ddeeeedbdebeeedebebed.c.aaaa\",
\"aaaa.c.dddddddeebeabdebebedeebedd.c.aaaa\",
\"aaaa.c.debebedebeedbdbebeedbeeeeb.c.aaaa\",
\"aaaa.c.debeeedeeeaabdaaddddebedbb.c.aaaa\",
\"aaaa.c.deebeddbebedbdbaa.adeeedeb.c.aaaa\",
\"aaaa.c.ddeebedeeebaba.dd.dddeeedd.c.aaaa\",
\"aaaa.c.debeebdbeeedbd....ddeebeed.c.aaaa\",
\"aaaa.c.deebeedeebadbd.dd.ddeeeedd.c.aaaa\",
\"aaaa.c.dbbebddeeeeabd.aa.adebebbd.c.aaaa\",
\"aaaa.c.deeeeedeebeabaedddddeeeedd.c.aaaa\",
\"aaaa.c.dbebbbdebeadbdaeeeedebeeed.c.aaaa\",
\"aaaa.c.deeebddeeebdbdeebeedeebeed.c.aaaa\",
\"aaaa.c.debeeedebeeabdebebedebeebd.c.aaaa\",
\"aaaa.c.deebbedeeeedbdeeeeddeeeeed.c.aaaa\",
\"aaaa.c.dddddddddaadbdaddddddddddd.c.aaaa\",
\"aaaa.c..........beabaeb...........c.aaaa\",
\"aaaa.c.bbbbbbbbbb.bbbbbbbbbbbbbbb.c.aaaa\",
\"aaaa.c.bbbbbbbbbb..e.bbbbbbbbbbbb.c.aaaa\",
\"aaaa.c.bbbbbbbbbb.b.bbbbbbbbbbbbb.c.aaaa\",
\"aaaa.c............e.e.............c.aaaa\",
\"aaaa.cccccccccccc.a.a.ccccccccccccc.aaaa\",
\"aaaa................................aaaa\",
\"aaaaaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-exit-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *sun2[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        5            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ffff00\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaacaaaaaaacaaaaaacaaaaaaaaaaaa\",
\"aaaaaacaaaaaacaaaaaacaaaaacaaaaaacaaaaaa\",
\"aaaaaaacaaaaacaaaaaacaaaacaaaaaacaaaaaaa\",
\"aaaaaaaacaaaaacaaaaacaaaacaaaaacaaaaaaaa\",
\"aaaaaaaaacaaaaacaaaaaaaacaaaaacaaaaaaaaa\",
\"aaaaaaaaaacaaaaaaccccccaaaaaacaaaaaaaaaa\",
\"aaaaaaaaaaacaaaccccccccccaaacaaaaaaaaaaa\",
\"aaaaaaaaaaaaaccccccccccccccaaaaaaacaaaaa\",
\"aaaaaccaaaaaccccccccccccccccaaaaccaaaaaa\",
\"aaaaaaaccaaaccccccccccccccccaaacaaaaaaaa\",
\"aaaaaaaaacaccccccccccccccccccacaaaaaaaaa\",
\"aaaaaaaaaaaccccccccccccccccccaaaaaaaaaaa\",
\"aaaaaaaaaac........cc........caaaaaaaaaa\",
\"aaaaaaaaaacc................ccaaaaaaaaaa\",
\"aaaaaaaaaacc.......cc.......ccaaaaaaaaaa\",
\"aaaaaccccacc......cccc......ccacccaaaaaa\",
\"aaaaaaaaaaccc....cccccc....cccaaaaaaaaaa\",
\"aaaaaaaaaaccccccccccccccccccccaaaaaaaaaa\",
\"aaaaaaaaaaaccccccccccccccccccaaaaaaaaaaa\",
\"aaaaaaaaaaaccccccccccccccccccaaaaaaaaaaa\",
\"aaaaaaaaccaacccc.cccccc.ccccaaccaaaaaaaa\",
\"aaaaaaccaaaaccccc......cccccaaaaccaaaaaa\",
\"aaaaacaaaaaaaccccc....cccccaaaaaaacaaaaa\",
\"aaaaaaaaaaacaaacccccccccccaacaaaaaaaaaaa\",
\"aaaaaaaaaacaaaaaacccccccaaaaacaaaaaaaaaa\",
\"aaaaaaaaacaaaaacaaaaaaaaacaaaacaaaaaaaaa\",
\"aaaaaaaacaaaaacaaaaacaaaacaaaaacaaaaaaaa\",
\"aaaaaaacaaaaaacaaaaacaaaaacaaaaacaaaaaaa\",
\"aaaaaacaaaaaacaaaaaacaaaaacaaaaaacaaaaaa\",
\"aaaaaaaaaaaaacaaaaaacaaaaaacaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")





(defvar fuse-toolbar-helper-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *help_btn[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        3            1\",
/* colors */
\"a c #bebebe s backgroundToolBarColor\",
\"b c #000000\",
\"c c #ff0000\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbbbaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaabbbccccccccbbbaaaaaaaaaaaaa\",
\"aaaaaaaaaaabbccccccccccccccbbaaaaaaaaaaa\",
\"aaaaaaaaaabccccccccccccccccccbaaaaaaaaaa\",
\"aaaaaaaaabccccccccccccccccccccbaaaaaaaaa\",
\"aaaaaaaabcccccccbbbbbbbbcccccccbaaaaaaaa\",
\"aaaaaaaabccccbbbaaaaaaaabbbccccbaaaaaaaa\",
\"aaaaaaabccccbaaaaaaaaaaaaaabccccbaaaaaaa\",
\"aaaaaaabcccbaaaaaaaaaaaaaaaabcccbaaaaaaa\",
\"aaaaaaabcccbaaaaaaaaaaaaaaaabcccbaaaaaaa\",
\"aaaaaaabcccbaaaaaaaaaaaaaaaabcccbaaaaaaa\",
\"aaaaaaabbbbbaaaaaaaaaaaaaaabccccbaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaabbbccccbaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbbbcccccccbaaaaaaaa\",
\"aaaaaaaaaaaaaaaabcccccccccccccbaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccccccccbaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccccccccbbaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbbbbbbaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabccccbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaabbbbbbaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"help\" button")


(defvar fuse-toolbar-log-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *notebut[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        8            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff00ff\",
\"d c #ff0000\",
\"e c #ffffff\",
\"f c #ffff00\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaa.....aaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaa.ddddd.aaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.ddddddd.aaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.ddddddc.aaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaa.dcceecd.aaaaa..aaaaaaaaaaaaaaaa\",
\"aaaaaaaaa.ddeed.aaaa..fe.aaaaaaaaaaaaaaa\",
\"aaaaaaaaaa.....aaa..efef.aaaaaaaaaaaaaaa\",
\"aaaaaaaaa..ddd....fefefef.aaaaaaaaaaaaaa\",
\"aaaaaaaa.d.dde.d.fefefefe.aaaaaaaaaaaaaa\",
\"aaaaaaa.dd.ddc.dc.fefefefe.aaaaaaaaaaaaa\",
\"aaaaaaa.ddd...ddc.efefefef.aaaaaaaaaaaaa\",
\"aaaaaaa.dcdddddcd.fefefefef.aaaaaaaaaaaa\",
\"aaaaaa..ddcceecdd.efefefefe.aaaaaaaaaaaa\",
\"aaaa..a..ddddddd.efefefefefe.aaaaaaaaaaa\",
\"aaaa.aaa........efefefefefef.aaaaaaaaaaa\",
\"aaaaa.a.a.a..efefefefefaaefef.aaaaaaaaaa\",
\"aaaaa.aa.a.a.fefefefeaafefefe.aaaaaaaaaa\",
\"aaaaaa.aa.a.aefefafaaefefefefe.aaaaaaaaa\",
\"aaaaaa.aaa.aefefeaafeaefeaafef.aaaaaaaaa\",
\"aaaaaaa.aefefefaaefefefaaefefef.aaaaaaaa\",
\"aaaaaaa.aaefeaafaaefeaafeaefefef.aaaaaaa\",
\"aaaaaaaa.afefefefefaaefefefeaafef...aaaa\",
\"aaaaaaaa.aafefefaaafefefefaaefefefe.aaaa\",
\"aaaaaaaaa.aefeaafefafafeaafefefefe.aaaaa\",
\"aaaaaaaaa.aaefefefefefaaefafefefe.aaaaaa\",
\"aaaaaaaaaa.afefefefeaafefefefef..aaaaaaa\",
\"aaaaaaaaaa..afefefefefefefefe..aaaaaaaaa\",
\"aaaaaaaaaaa....efefefefefef..aaaaaaaaaaa\",
\"aaaaaaaaaaaa.......fefefe..aaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa..fee..fef..aaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaa...f.....aaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa.....aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")



(defvar fuse-toolbar-new-file-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *comment3[] = {
/* width height num_colors chars_per_pixel */
\"    40    40       14            1\",
/* colors */
\". c #000000\",
\"# c #d2b48c\",
\"a c #f5deb3\",
\"b c #e6e6fa\",
\"c c #b22222\",
\"d c #cd853f\",
\"e c #bebebe s backgroundToolBarColor\",
\"f c #a0522d\",
\"g c #2f4f4f\",
\"h c #ff6347\",
\"i c #ffd700\",
\"j c #708090\",
\"k c #fffacd\",
\"l c #ffa500\",
/* pixels */
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeee.eeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeee.k.eeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeee.kkk.eeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeee.kkkkk.eeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeee.kkkkkkk.eeeeeeeaa#eeeeee\",
\"eeeeeeeeeeeeee.kkkk.kkkk.eeeeeaa###eeeee\",
\"eeeeeeeeeeeee.kkkkekkkkkk.eee.a##hh#eeee\",
\"eeeeeeeeeeee.kkkkjkkk.kkkk.e.l.#hhhheeee\",
\"eeeeeeeeeee.kkkk.kkkekkkkkkai.l.hhhfeeee\",
\"eeeeeeeeee.kkkkjkkk.kkk.kkaiii.l.hfeeeee\",
\"eeeeeeeee.kkkk.kkkjkkk.kkaiiill.f.eeeeee\",
\"eeeeeeee.kkkkkkkkkkkkjkkaiiilllc.eeeeeee\",
\"eeeeeee.kkkk.kkkjkkkkkkaiiilllcceeeeeeee\",
\"eeeeee.kkkkekkk.kkk.kkaiiilllcck.eeeeeee\",
\"eeeee.kkkkjkkkkkkkekkaiiilllcckkk.eeeeee\",
\"eeee.kkkk.kkk.kkkekkllcilllcckkkkk.eeeee\",
\"eeeee.kkkkkkekkk.kkklllcllcckkkkkkk.eeee\",
\"eeeeee.kkkkjkkkjkkkk..ldccckkkkkkk.eeeee\",
\"eeeeeee.kkkkkkjkkkkk...ffckkkkkkk.eeeeee\",
\"eeeeeeee.kkkk.kkkkkk...ffkkkkkkk.eeeeeee\",
\"eeeeeeeee.kkkkkkkkkkkkkkkkkkkkk.eeeeeeee\",
\"eeeeeeeeee.kkkkkkkkkkkkkkkkkkk.eeeeeeeee\",
\"eeeeeeeeeee.kkkkkkkkkkkkkkkkk.eeeeeeeeee\",
\"eeeeeeeeeeee.kkkkkkkkkkkkkkk.eeeeeeeeeee\",
\"eeeeeeeeeeeee.kkkkkkkkkkkkk.eeeeeeeeeeee\",
\"eeeeeeeeeeeeee.kkkkkkkkkkk.eeeeeeeeeeeee\",
\"eeeeeeeeeeeeeee.kkkkkkkkk.eeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeee.kkkkkkk.eeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeee.kkkkk.eeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeee.kkk.eeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeee.k.eeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeee.eeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-program-document-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *library4[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        8            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff0000\",
\"d c #2e8b57\",
\"e c #ffff00\",
\"f c #0000ff\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa.....aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaa.fff.aaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa.....fff.....aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa.ccc.fff..ee.aaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaa.ccc.fff..ee.aaaaaaaaaaaaaa\",
\"aaaaaaaa......ccc.fff..ee.aaaaaaaaaaaaaa\",
\"aaaaaaaa.fff..ccc.fff..ee.aaaaaaaaaaaaaa\",
\"aaaaaaaa.fff..ccc.fff..ee......aaaaaaaaa\",
\"aaaaaaaa.fff..ccc.fff..ee..ddd.aaaaaaaaa\",
\"aaaaaaaa.fff..ccc.fff..ee..ddd.aaaaaaaaa\",
\"aaaaaaaa.fff..ccc.fff..ee..ddd.aaaaaaaaa\",
\"aaaaaaa..fff..ccc.fff..ee..ddd..aaaaaaaa\",
\"aaaaaaa..fff..ccc.fff..ee..ddd..aaaaaaaa\",
\"aaaaaaa..fff..ccc.fff..ee..ddd..aaaaaaaa\",
\"aaaaaaa..fff..ccc.fff..ee..ddd..aaaaaaaa\",
\"aaaaaaa..fff..ccc.fff..ee..ddd..aaaaaaaa\",
\"aaaaaaa..fff..ccc.fff..ee..ddd..aaaaaaaa\",
\"aaaaaaa..fff..ccc.fff..ee..ddd..aaaaaaaa\",
\"aaaaa....fff..ccc.fff..ee..ddd....aaaaaa\",
\"aaaaa..............................aaaaa\",
\"aaaaa.ceccccecccceccccecccceccccec.aaaaa\",
\"aaaaa..............................aaaaa\",
\"aaaaaaaaa.e.aaaaaaaaaaaaaaa.e.aaaaaaaaaa\",
\"aaaaaaaaa.c.aaaaaaaaaaaaaaa.c.aaaaaaaaaa\",
\"aaaaaaaaa.c.aaaaaaaaaaaaaaa.c.aaaaaaaaaa\",
\"aaaaaaaaa...aaaaaaaaaaaaaaa...aaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-quit-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *suntanbut[] = {
/* width height num_colors chars_per_pixel */
\"    40    40       18            1\",
/* colors */
\". c #000000\",
\"# c #32cd32\",
\"a c #1e90ff\",
\"b c #e6e6fa\",
\"c c #dcdcdc\",
\"d c #b22222\",
\"e c #cd853f\",
\"f c #87ceeb\",
\"g c #bebebe s backgroundToolBarColor\",
\"h c #a0522d\",
\"i c #2f4f4f\",
\"j c #2e8b57\",
\"k c #ffffff\",
\"l c #ffff00\",
\"m c #ffd700\",
\"n c #708090\",
\"o c #ffa500\",
\"p c #0000ff\",
/* pixels */
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggakkfagggggggggggggggggggggggg\",
\"ggggggggaakkkfaakakggggggggggggggggggggg\",
\"ggggggaafkkkaaakkaakkggggggggggggggggggg\",
\"ggggaackkkaaaakkkkaakkgggggggggggggggggg\",
\"ggggppppppaffbkkkkaaabkggggggggggggggggg\",
\"ggggppppppppppiikkfaaakkgggggggggggggggg\",
\"ggggppppppppppippppppfkkkggggggggggggggg\",
\"ggggppppppppppippppppppfkkgggggggggggggg\",
\"ggggggpppppppipppppppppppfgggggggggggggg\",
\"ggggggggggpppippppppppppppgggggggggggggg\",
\"gggggggggggggigggpppppppgggggggggggggggg\",
\"gggggggggggggigggggggggggggggggggggggggg\",
\"ggggggggggggiggggggggggggggggggggggggggg\",
\"ggggggggggggigj#...ggggggggggggggggggggg\",
\"ggggggggggggij#..j##j#gggggggggggggggggg\",
\"gggggggggggijj######jigggggggggggggggggg\",
\"gggggggggelj#j#i###ji#iggggggggggggggggg\",
\"ggggggggghj#mmj#iiiggi#ggggggggggggggggg\",
\"gggggggggg########igi#iggggggggggggggggg\",
\"ggggggggggioo.###ddddiggggj####gggg#gggg\",
\"ggggggggggihlm.i###dddd####j...j#gg#gggg\",
\"ggggggggggighl.ij###d###i.##ooeiij##gggg\",
\"gggggggggigggem..i####..ml.i#mj.lo..gggg\",
\"gggggggggiggghlloi..ioooonemi#..lmoogggg\",
\"gggggggggignnnhehoooog.ggggooi.illllgggg\",
\"ggggnnnninnnnn..nnnnnn.nnnnnolollllmgggg\",
\"ggggnnnninnnnn.nnnnnnnnnnnnnnmllohhngggg\",
\"ggggnnnnnnnnnnnnnnnnnnnnnnnnnn.nnnnngggg\",
\"ggggnnnnnnnnnnnnnnnnnnnnnnnnnn.nnnnngggg\",
\"gggggggggnnnnggggnnnnnnnnnnnnnnnnngggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\",
\"gggggggggggggggggggggggggggggggggggggggg\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-return-to-fuse-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *homebut2[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        9            1\",
/* colors */
\". c #000000\",
\"# c #e6e6fa\",
\"a c #dcdcdc\",
\"b c #87ceeb\",
\"c c #bebebe s backgroundToolBarColor\",
\"d c #2f4f4f\",
\"e c #ff0000\",
\"f c #fffacd\",
\"g c #0000ff\",
/* pixels */
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"ccccccccccccccccccc..ccccccccccccccccccc\",
\"cccccccccccccccccc.ee.cccccccccccccccccc\",
\"ccccccccccccccccc.eeee.ccccccccccccccccc\",
\"cccccccccc....cc.eeeeee.cccccccccccccccc\",
\"cccccccccc.ee.c.eeeeeeee.ccccccccccccccc\",
\"cccccccccc.ee..eeeeeeeeee.cccccccccccccc\",
\"cccccccccc.ee.eeeeeeeeeeee.ccccccccccccc\",
\"cccccccccc.eeeeeeeeeeeeeeee.cccccccccccc\",
\"cccccccccc.eeeeeeeeeeeeeeeee.ccccccccccc\",
\"cccccccccc.eeeeeeeeeeeeeeeeee.cccccccccc\",
\"ccccccccc.eeeeeeeeeeeeeeeeeeee.ccccccccc\",
\"cccccccc.eeeeeeeeeeeeeeeeeeeeee.cccccccc\",
\"ccccccc.eeeeeeeeeeeeeeeeeeeeeeee.ccccccc\",
\"cccccc............................cccccc\",
\"ccccccccc.ffffffffffffffffffff.ccccccccc\",
\"ccccccccc.ffffffffffffffffffff.ccccccccc\",
\"ccccccccc.ffffffffffffffffffff.ccccccccc\",
\"ccccccccc.fff......ffgggggggff.ccccccccc\",
\"ccccccccc.fff.gggg.ffgbbbbbgff.ccccccccc\",
\"ccccccccc.fff.gggg.ffgbbbbbgff.ccccccccc\",
\"ccccccccc.fff.gggg.ffgbbbbbgff.ccccccccc\",
\"ccccccccc.fff.ggaa.ffgbbbbbgff.ccccccccc\",
\"ccccccccc.fff.gggg.ffgggggggff.ccccccccc\",
\"ccccccccc.fff.gggg.fffffffffff.ccccccccc\",
\"ccccccccc.fff.gggg.fffffffffff.ccccccccc\",
\"ccccccccc.fff.gggg.fffffffffff.ccccccccc\",
\"ccccccccc.fff.gggg.fffffffffff.ccccccccc\",
\"ccccccccc......................ccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\",
\"cccccccccccccccccccccccccccccccccccccccc\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-run-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *exclaimbut[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        7            1\",
/* colors */
\". c #e6e6fa\",
\"# c #b22222\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff0000\",
\"d c #ffffff\",
\"e c #708090\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaccccaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaccccccccaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaccddcccccceaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaacddccccccceeaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaccdcccccccc#eaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaacddcccccccc#eeaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaacdccccccccc#eeaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaacdccccccccc#eeaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaccccccccccc#eeaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaccccccccccc#eeaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaacccccccccc#eeaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaacccccccccc#eeaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaccccccccc#eeeaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaccccccccc#eeaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaccccccccc#eeaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaacccccccc#eeeaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaccccccc#eeaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaacccccc#eeeaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaacccccc#eeaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaacccc#eeeaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaacccc#eeaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaa###eeeaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaeeeaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaacccccaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaacddccc#eaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaacddccccc#eaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaacdcccccc#eeaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaacccccccc#eeaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaacccccccc#eeaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaa#ccccc#eeeaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaa#####eeeaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaeeeeeaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-template-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *comment3[] = {
/* width height num_colors chars_per_pixel */
\"    40    40       14            1\",
/* colors */
\". c #000000\",
\"# c #d2b48c\",
\"a c #f5deb3\",
\"b c #e6e6fa\",
\"c c #b22222\",
\"d c #cd853f\",
\"e c #bebebe s backgroundToolBarColor\",
\"f c #a0522d\",
\"g c #2f4f4f\",
\"h c #ff6347\",
\"i c #ffd700\",
\"j c #708090\",
\"k c #fffacd\",
\"l c #ffa500\",
/* pixels */
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeee.eeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeee.k.eeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeee.kkk.eeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeee.kkkkk.eeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeee.kkkkkkk.eeeeeeeaa#eeeeee\",
\"eeeeeeeeeeeeee.kkkk.kkkk.eeeeeaa###eeeee\",
\"eeeeeeeeeeeee.kkkkekkkkkk.eee.a##hh#eeee\",
\"eeeeeeeeeeee.kkkkjkkk.kkkk.e.l.#hhhheeee\",
\"eeeeeeeeeee.kkkk.kkkekkkkkkai.l.hhhfeeee\",
\"eeeeeeeeee.kkkkjkkk.kkk.kkaiii.l.hfeeeee\",
\"eeeeeeeee.kkkk.kkkjkkk.kkaiiill.f.eeeeee\",
\"eeeeeeee.kkkkkkkkkkkkjkkaiiilllc.eeeeeee\",
\"eeeeeee.kkkk.kkkjkkkkkkaiiilllcceeeeeeee\",
\"eeeeee.kkkkekkk.kkk.kkaiiilllcck.eeeeeee\",
\"eeeee.kkkkjkkkkkkkekkaiiilllcckkk.eeeeee\",
\"eeee.kkkk.kkk.kkkekkllcilllcckkkkk.eeeee\",
\"eeeee.kkkkkkekkk.kkklllcllcckkkkkkk.eeee\",
\"eeeeee.kkkkjkkkjkkkk..ldccckkkkkkk.eeeee\",
\"eeeeeee.kkkkkkjkkkkk...ffckkkkkkk.eeeeee\",
\"eeeeeeee.kkkk.kkkkkk...ffkkkkkkk.eeeeeee\",
\"eeeeeeeee.kkkkkkkkkkkkkkkkkkkkk.eeeeeeee\",
\"eeeeeeeeee.kkkkkkkkkkkkkkkkkkk.eeeeeeeee\",
\"eeeeeeeeeee.kkkkkkkkkkkkkkkkk.eeeeeeeeee\",
\"eeeeeeeeeeee.kkkkkkkkkkkkkkk.eeeeeeeeeee\",
\"eeeeeeeeeeeee.kkkkkkkkkkkkk.eeeeeeeeeeee\",
\"eeeeeeeeeeeeee.kkkkkkkkkkk.eeeeeeeeeeeee\",
\"eeeeeeeeeeeeeee.kkkkkkkkk.eeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeee.kkkkkkk.eeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeee.kkkkk.eeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeee.kkk.eeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeee.k.eeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeee.eeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\",
\"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee\"};"))
  "XPM format image used for the \"____\" button")


(defvar fuse-toolbar-tutorial-xpm
  (if (featurep 'xpm)
      (toolbar-make-button-list
       "/* XPM */
static char *book_index[] = {
/* width height num_colors chars_per_pixel */
\"    40    40        6            1\",
/* colors */
\". c #000000\",
\"a c #bebebe s backgroundToolBarColor\",
\"b c #2f4f4f\",
\"c c #ff0000\",
\"d c #ffffff\",
\"e c #708090\",
/* pixels */
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaa.........bbeaaaebb..........aaaaaa\",
\"aaaaaa.ddddddddaaebebeaaddddddddd.aaaaaa\",
\"aaaa...dab.bddeebadbdaeebedeeeeed...aaaa\",
\"aaaa.c.dbaddddebeedbdeebeedebebed.c.aaaa\",
\"aaaa.c.d.de.edeebeabdbbeeddebbbed.c.aaaa\",
\"aaaa.c.dbad.ddebeadbdeeebeddeeeed.c.aaaa\",
\"aaaa.c.dab..ddeeeedbdebeeedebebed.c.aaaa\",
\"aaaa.c.dddddddeebeabdebebedeebedd.c.aaaa\",
\"aaaa.c.debebedebeedbdbebeedbeeeeb.c.aaaa\",
\"aaaa.c.debeeedeeeaabdaaddddebedbb.c.aaaa\",
\"aaaa.c.deebeddbebedbdbaa.adeeedeb.c.aaaa\",
\"aaaa.c.ddeebedeeebaba.dd.dddeeedd.c.aaaa\",
\"aaaa.c.debeebdbeeedbd....ddeebeed.c.aaaa\",
\"aaaa.c.deebeedeebadbd.dd.ddeeeedd.c.aaaa\",
\"aaaa.c.dbbebddeeeeabd.aa.adebebbd.c.aaaa\",
\"aaaa.c.deeeeedeebeabaedddddeeeedd.c.aaaa\",
\"aaaa.c.dbebbbdebeadbdaeeeedebeeed.c.aaaa\",
\"aaaa.c.deeebddeeebdbdeebeedeebeed.c.aaaa\",
\"aaaa.c.debeeedebeeabdebebedebeebd.c.aaaa\",
\"aaaa.c.deebbedeeeedbdeeeeddeeeeed.c.aaaa\",
\"aaaa.c.dddddddddaadbdaddddddddddd.c.aaaa\",
\"aaaa.c..........beabaeb...........c.aaaa\",
\"aaaa.c.bbbbbbbbbb.bbbbbbbbbbbbbbb.c.aaaa\",
\"aaaa.c.bbbbbbbbbb..e.bbbbbbbbbbbb.c.aaaa\",
\"aaaa.c.bbbbbbbbbb.b.bbbbbbbbbbbbb.c.aaaa\",
\"aaaa.c............e.e.............c.aaaa\",
\"aaaa.cccccccccccc.a.a.ccccccccccccc.aaaa\",
\"aaaa................................aaaa\",
\"aaaaaaaaaaaaaaaaaa...aaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\",
\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"};"))
  "XPM format image used for the \"____\" button")


;;; That's it! ----------------------------------------------------------------

;; any final chores before leaving
(provide 'fuse-icons)

;;;============================================================================
;;;
;;; fuse-icons.el end here
