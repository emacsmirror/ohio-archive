;;; zenirc-make --- a hack to set the load path for byte-compiling

;; Copyright (C) 1995, 1996 Per Persson

;; Author: Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, make
;; Created: 1995-06-26

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(setq load-path (cons "." load-path))

;;; zenirc-make.el ends here