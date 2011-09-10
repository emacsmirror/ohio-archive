### 00readme.txt --- templates which explain the functions in template.el

## Author: Christoph Wedler <wedler@fmi.uni-passau.de>
## Version: $Id: 00readme.txt,v 2.0 1997/03/10 08:41:18 wedler Exp wedler $


TEMPLATE.txt.tpl	template which shows the use of:
			 * the pre-defined expansions
			 * per-template defined expansion
  text1.txt		an instance of it
TEMPLATE.cc.tpl		template which shows the use of:
			 * commands for commenting if `comment-start' is a
			   string with length > 1
			 * auto numbering if numbering is present
  main.cc		an instance of it
exercise.tex.tpl	template which shows the use of:
			 * commands for commenting if `comment-start' is a
			   string with length = 1
			 * auto numbering when "raw file name" is the same
			   is the raw file name of the template
  exercise2.tex		an instance of it

###  Explanation: TEMPLATE.txt.tpl - text1.txt ===============================

(1) Compare "TEMPLATE.txt.tpl" with "text1.txt" and try to understand what the
    all the (>>>X<<<) stand for.  Type M-x describe-variable for the user
    options mentioned in the text.  If you do not know emacs-lisp, you do not
    have to understand the section "Insecure..." and the per-template expansion
    forms after "This message...".

(2) Try to use "C-x t" (`template-new-file').  Enter "mytext.txt" as the
    filename (in the current directory examples/).  This function should
    suggest to use TEMPLATE.txt.tpl as the template file.  You should be asked

	Have you checked the template functions?

    because the template contains insecure per-template definitions for the
    expansion.  Answer with "y" (check this for yourself!).  Enter some text
    each time you're asked.  Compare the resulting file with "text1.txt": you
    should have an empty file name number.

(3) Try the file name "222.txt".  "222" is file name number, because a file
    name number requires a non-empty, non-digit prefix.

###  Explanation: TEMPLATE.cc.tpl - main.cc ==================================

(4) Use "C-x t" with "main.cc" (file that is distributed with this read-me
    file).  You should be asked

	File DIR/main.cc exists.  Delete contents?

    Answer "yes" and type "y" 9 times.  You should get the same contents as the
    original (if your indentation in c++-mode is the same).

(5) Create a file "main1.cc".  Try to create it another time, i.e., "main1.cc"
    has been saved or is still a living buffer.  This time, a unique file name
    "main2.cc" is computed since the file name contained a file name number.

###  Explanation: exercise.tex.tpl - exercise2.tex ===========================

(6) Use "C-x t" with "exercise1.tex".  You should get the same contents as the
    original (if your indentation in latex-mode is the same).  You should not
    be asked

	Have you checked the template functions?

(7) Call "C-x t" with file name "exercise" (via "e" + TAB!).  This command
    should suggest to create a file "exercise3.tex" using the template
    "exercise.tex.tpl".  By default, if a template with the same raw file name
    can be found, the extension is not that important and auto numbering is
    provided even if the given file name does not contain a number.
