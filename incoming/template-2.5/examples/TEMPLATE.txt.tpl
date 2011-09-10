### TEMPLATE.txt.tpl --- example template, shows expansion forms

## The file name is changed anyway:
## `template-new-file' calls `template-update-header'
## You are using Emacs (>>>emacs<<<)

###  Predefined expansions: ==================================================

point:			(>>>POINT<<<)
mark:			(>>>MARK<<<)Type C-x C-x to see the mark.
                with XEmacs, the region between point and mark is highlighted
directory:		(>>>DIR<<<)
file name:		(>>>FILE<<<)
raw file name:		(>>>FILE_RAW<<<)
number in name:		(>>>FILE_NUM<<<)
extension without dot:	(>>>FILE_EXT<<<)
file name w/o ext:	(>>>FILE_SANS<<<)
upcase name w/o ext:    (>>>FILE_UPCASE<<<)
date/time:		(>>>DATE<<<)
		using template-time-format = "(>>>time_format<<<)"
year:			(>>>YEAR<<<)
ISO 8601 date:  	(>>>ISO_DATE<<<)
UTC date/time for VC:  	(>>>VC_DATE<<<)
initial comment:	(>>>COMMENT<<<)
..without asking again: (>>>COMMENT<<<)
author:			(>>>AUTHOR<<<)
user name:		(>>>USER_NAME<<<)
login name:		(>>>LOGIN_NAME<<<)
host address:		(>>>HOST_ADDR<<<)
literal environment:	(>>>LITERAL<<<)(>>>DATE<<<) format(>>>/LITERAL<<<)
		Used to insert a text which should not be expanded even if it
		contains strings matched by `template-expansion-regexp'.
zero form:      Used to insert a single expansion form literally, e.g.,
		(>>>(>>>ZERO_FORM<<<)ZERO_FORM<<<).

See user option `template-default-expansion-alist'.  You may also use expansion
forms with one-letter keys, see `template-key-alias-alist'.

###  Default expansions: =====================================================

default prompt:		(>>>default_prompt<<<)
..without asking again: (>>>default_prompt<<<)
register pos:		(>>>1<<<)Type "C-x r j 1" to jump to the `T'

See user options `template-expansion-regexp' and `template-register-regexp'.

###  Secure per-template defined expansions: =================================

* some commands:	(>>>time<<<) (>>>single<<<)
user defined prompt:	(>>>user_prompt<<<)
..without asking again: (>>>user_prompt<<<)
another prompt:		(>>>again_prompt<<<)
..with asking again:    (>>>again_prompt<<<)
recursive prompt:	(>>>recursive_prompt<<<)
choice:			(>>>choice<<<)
..same choice again:    (>>>choice<<<)
register pos:		(>>>reg_pos<<<)Type "C-x r j r" to jump to the `T'
register text:  registers at key ?a, ?b and ?c contain text, e.g., type
		"C-x r i a" to insert contents of register ?a.  The text in
		registers b and ?c is shown with the temporary message at
		point.  The temporary message for ?c includes a comment.

See user option `template-definition-start'.

###  Insecure per-template defined expansion: ================================

deletion:	987654321(>>>delete3<<<)
template-file:	(>>>file<<<)
pre-expansion:	"(>>>var1<<<)"
post-expansion: cannot be shown here, but type M-: var1 RETURN

See user option `template-definition-start'.

###  The last line
>>>TEMPLATE-DEFINITION-SECTION<<<
"Buffer shown as modified since there was user input."

("user_prompt" "Insert here and at a second place: ")
("again_prompt" "Insert here (asked twice): " "again_pre: " " :again_post" "again_default" t)
("recursive_prompt" "Insert here (empty = stop): " nil "(>>>_recursive_prompt<<<)" "none" expand)
("_recursive_prompt" "Insert here (empty = stop): " ", " "(>>>_recursive_prompt<<<)" "." expand)
("choice" "Do you want yes? " ("yes" . "Yes, I want \"yes\".") ("no" . "No, not in (>>>FILE<<<)"))

(?a . "Text in register ?a.")
(?b "Text in register ?b.")
(?c "Text in register ?c." "Comment for register ?c")

"This message disappears with the first user event or after 10 minutes."

("emacs" . emacs-version)
("time_format" . template-time-format)
("time" template-insert-time "%a %d %b %Y, %I:%M %p")
("single" template-single-comment . 1)
("reg_pos" . ?r)
("delete3" backward-delete-char . 3)
("file" (insert (prin1-to-string template-file)))
("var1" . var1)

nil
(setq var1 "Value before expansion")
nil
(setq var1 "Value after expansion")
