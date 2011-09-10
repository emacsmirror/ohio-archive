### 00readme.txt --- commenting, file headers, templates

## Author: Christoph Wedler <wedler@fmi.uni-passau.de>
## Version: $Id: 00readme.txt,v 2.4 1999/01/25 11:15:47 wedler Exp d029492 $


This is the readme file of package template (version 2.5)
    http://www.fmi.uni-passau.de/~wedler/template/

The source of the newest version can be found at
    http://www.fmi.uni-passau.de/~wedler/template/template.tar.gz


Overview
========

Supports writing standardized comments, (auto-)updating parts of the buffer,
e.g., time stamps or file names in headers, using templates when creating a
new file and creating templates.  A template is a file with normal text,
pre-defined "expansion forms" like inserting (parts of) the file name,
date/time, user name etc (see below for more), setting point and register
positions, and "expansion forms" which are defined by elisp sexps at the end
of the template.  Some examples and default templates are distributed with
this package.


Files in the distribution
=========================

00readme.txt		this file
RCS-log			change log file
lisp/			lisp files, put these into your `load-path'
  ChangeLog		detailed change log file for lisp files since v2.1
  template.el		the package itself

examples/		templates which explain the functions in template.el
  00readme.txt				explanation
  DEFAULT.tpl				a default template file
  TEMPLATE.txt.tpl, text1.txt		template with instance
  TEMPLATE.cc.tpl, main.cc		ditto
  exercise.tex.tpl, exercise2.tex	ditto

templates/		templates for your ~/.templates/
  00readme.txt.tpl 	for readme files
  DEFAULT.tpl		you should always have this file
  Makefile.tpl		for makefiles
  TEMPLATE.c.tpl	for C files
  TEMPLATE.cls.tpl      for LaTeX classes (in your $TEXINPUTS)
  TEMPLATE.el.tpl       for emacs-lisp files
  TEMPLATE.h.tpl	for C header files
  TEMPLATE.html.tpl	for html files
  TEMPLATE.java.tpl	for java files
  TEMPLATE.sty.tpl      for LaTeX packages (in your $TEXINPUTS)
  TEMPLATE.tex.tpl	for La(TeX) files

### 00readme.txt ends here
