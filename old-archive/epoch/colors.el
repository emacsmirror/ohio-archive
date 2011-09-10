; From: kinstrey@wsqtba.crd.ge.com
; Subject: colors.el
; Date: 19 Apr 91 20:04:48 GMT
; Organization: GNUs Not Usenet
; 
;;; Copyright (C) 1990  General Electric Company.
;;; Written by Michael A. Kinstrey, for the
;;; DICE (DARPA Initiative in Concurrent Engineering) project.
;;;
;;; This file is for use with GNU Emacs 18.55 or later, 
;;; or Epoch, a modified version of GNU Emacs.
;;; Requires Epoch 3.2 or later. 
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY. No author or distributor accepts
;;; responsibility to anyone for the consequences of using this code
;;; or for whether it serves any particular purpose or works at all,
;;; unless explicitly stated in a written agreement. Refer to the
;;; GNU Emacs General Public License for full details.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; this code, but only under the conditions described in the
;;; GNU Emacs General Public License.
;;; Among other things, the copyright notice and this notice 
;;; must be preserved on all copies.

;; The colors in this file are the "standard" colors available in X
;; simply refer to the required color by its uppercase name
;; (BTW, these are only available for use under epoch,
;;       since the get-color routine is epoch-specific)
(provide 'colors)

(if running-epoch
  (progn
    (defconst RED   (get-color "red"))
    (defconst GREEN (get-color "green"))
    (defconst BLACK (get-color "black"))
    (defconst GREY  (get-color "grey"))
    (defconst GRAY  (get-color "gray"))
    (defconst YELLOW (get-color "yellow"))
    (defconst BLUE  (get-color "blue"))
    (defconst WHITE (get-color "white"))
    (defconst WHEAT (get-color "wheat"))
    (defconst AQUAMARINE (get-color "aquamarine"))
    (defconst MEDIUM_AQUAMARINE (get-color "medium aquamarine"))
    (defconst CADET_BLUE (get-color "cadet blue"))
    (defconst CORNFLOWER_BLUE (get-color "cornflower blue"))
    (defconst DARK_SLATE_BLUE (get-color "dark slate blue"))
    (defconst LIGHT_BLUE (get-color "light blue"))
    (defconst LIGHT_STEEL_BLUE (get-color "light steel blue"))
    (defconst MEDIUM_BLUE (get-color "medium blue"))
    (defconst MEDIUM_SLATE_BLUE (get-color "medium slate blue"))
    (defconst MIDNIGHT_BLUE (get-color "midnight blue"))
    (defconst NAVY_BLUE (get-color "navy blue"))
    (defconst NAVY (get-color "navy"))
    (defconst SKY_BLUE (get-color "sky blue"))
    (defconst SLATE_BLUE (get-color "slate blue"))
    (defconst STEEL_BLUE (get-color "steel blue"))
    (defconst CORAL (get-color "coral"))
    (defconst CYAN (get-color "cyan"))
    (defconst FIREBRICK (get-color "firebrick"))
    (defconst BROWN (get-color "brown"))
    (defconst GOLD (get-color "gold"))
    (defconst GOLDENROD (get-color "goldenrod"))
    (defconst MEDIUM_GOLDENROD (get-color "medium goldenrod"))
    (defconst GREEN (get-color "green"))
    (defconst DARK_GREEN (get-color "dark green"))
    (defconst DARK_OLIVE_GREEN (get-color "dark olive green"))
    (defconst FOREST_GREEN (get-color "forest green"))
    (defconst LIME_GREEN (get-color "lime green"))
    (defconst MEDIUM_FOREST_GREEN (get-color "medium forest green"))
    (defconst MEDIUM_SEA_GREEN (get-color "medium sea green"))
    (defconst MEDIUM_SPRING_GREEN (get-color "medium spring green"))
    (defconst PALE_GREEN (get-color "pale green"))
    (defconst SEA_GREEN (get-color "sea green"))
    (defconst SPRING_GREEN (get-color "spring green"))
    (defconst YELLOW_GREEN (get-color "yellow green"))
    (defconst DARK_SLATE_GREEN (get-color "dark slate green"))
    (defconst DARK_SLATE_GRAY (get-color "dark slate gray"))
    (defconst DARK_SLATE_GREY (get-color "dark slate grey"))
    (defconst DIM_GREY (get-color "dim grey"))
    (defconst DIM_GRAY (get-color "dim gray"))
    (defconst LIGHT_GREY (get-color "light grey"))
    (defconst LIGHT_GRAY (get-color "light gray"))
    (defconst KHAKI (get-color "khaki"))
    (defconst MAGENTA (get-color "magenta"))
    (defconst MAROON (get-color "maroon"))
    (defconst ORANGE (get-color "orange"))
    (defconst ORCHID (get-color "orchid"))
    (defconst DARK_ORCHID (get-color "dark orchid"))
    (defconst MEDIUM_ORCHID (get-color "medium orchid"))
    (defconst PINK (get-color "pink"))
    (defconst PLUM (get-color "plum"))
    (defconst INDIAN_RED (get-color "indian red"))
    (defconst MEDIUM_VIOLET_RED (get-color "medium violet red"))
    (defconst ORANGE_RED (get-color "orange red"))
    (defconst VIOLET_RED (get-color "violet red"))
    (defconst SALMON (get-color "salmon"))
    (defconst SIENNA (get-color "sienna"))
    (defconst TAN (get-color "tan"))
    (defconst THISTLE (get-color "thistle"))
    (defconst TURQUOISE (get-color "turquoise"))
    (defconst DARK_TURQUOISE (get-color "dark turquoise"))
    (defconst MEDIUM_TURQUOISE (get-color "medium turquoise"))
    (defconst VIOLET (get-color "violet"))
    (defconst BLUE_VIOLET (get-color "blue violet"))
    (defconst GREEN_YELLOW (get-color "green yellow"))
  )
)

