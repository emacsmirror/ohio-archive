;; perl-descr.el -- one-line information on a perl symbol
;; SCCS Status     : @(#)@ perl-descr.el	1.8
;; Author          : Johan Vromans
;; Created On      : Thu Jun 28 17:06:26 1990
;; Last Modified By: Dave Brennan
;; Last Modified On: Thu Aug 26 19:11:50 1993
;; Update Count    : 111
;; Status          : Released
;;
;; Purpose of this package:
;;
;;   This file defines the function 'describe-perl-symbol, which
;;   displays a one-line information on a perl symbol.
;;   
;;   Based on 'describe-lisp-symbol' and others.
;;   Hacked for Perl by Johan Vromans <jv@mh.nl>
;;
;; Installation instructions
;;
;;   Byte-compile and place perl-descr.elc in your emacs lisp library.
;;
;; Usage instructions:
;;
;;   Add to your .emacs:
;;
;;     (autoload 'describe-perl-symbol "perl-descr"
;;               "One-line information on a perl symbol" t)
;;     (autoload 'switch-to-perl-doc-buffer "perl-descr"
;;               "One-line information on a perl symbol" t)
;;
;;   Bind it to your favorite key, e.g. (from .emacs):
;;
;;     (setq perl-mode-hook 
;;           (function (lambda ()
;;              (local-set-key "\eOQ" 'describe-perl-symbol)
;;		(local-set-key "\eOD" 'switch-to-perl-doc-buffer))))
;;
;; Known bugs:
;;
;;    The information is a bit terse.
;;    The function does a good job at finding the current perl symbol
;;    around point, but perl has a VERY weird syntax...
;;
;; LCD Archive Entry:
;; describe-perl-symbol|Johan Vromans|jv@mh.nl|
;; One-line information on a perl symbol.|
;; 22-Aug-1993|1.9|~/misc/perl-descr.el.Z|

(defvar perl-doc-buffer "*perl-doc*"
  "Where the documentation can be found.")

(defun perl-symbol-at-point ()
  "Get the closest Perl symbol to point, but don't change your
position. Has a preference for looking backward when not
directly on a symbol."

  (let ((perl-wordchars "a-zA-Z0-9_") start end symbol)
	      
    (save-excursion

      ;; first see if you're just past a symbol
      (if (eobp)
	  (if (not (bobp))
	      (backward-char 1))
	(if (looking-at "[] \t\n[{}()]")
	    (progn
	      (skip-chars-backward " \n\t\r({[]})")
	      (if (not (bobp))
		  (backward-char 1)))))

      (if (looking-at (concat "[$%@]?[" perl-wordchars "]"))
	  (progn
	    (skip-chars-backward perl-wordchars)
	    (setq start (point))
	    ; Get identifier. Include leading $ % @ to find things like
	    ; @ARGV and %ENV .
	    (if (string-match "[$%@]" (char-to-string (preceding-char)))
		(setq start (1- start))
	      (forward-char 1))
	    (skip-chars-forward perl-wordchars))

	;; else a symbol?
	  (progn
	    (setq start (point))
	    (if (looking-at "[$@][^ \n\t]") ; special variable
		(forward-char 1)
	      (if (string-match "[$@]" (char-to-string (preceding-char)))
		  (setq start (1- start))))
	    (forward-char 1)))
      (buffer-substring start (point)))))

(defun describe-perl-symbol (symbol)
  "Display the documentation of SYMBOL, a Perl operator."
  (interactive
    (let ((fn (perl-symbol-at-point))
	  (enable-recursive-minibuffers t)
	  val args-file regexp)
      (setq val (read-from-minibuffer
		  (if fn
		      (format "Symbol (default %s): " fn)
		    "Symbol: ")))
      (if (string= val "")
	  (setq val fn))
      (setq regexp (concat "^" (regexp-quote val) "\\([ \t([/]\\|$\\)"))

      ;; get the buffer with the documentation text
      (switch-to-perl-doc-buffer)

      ;; lookup in the doc
      (goto-char (point-min))
      (let ((case-fold-search nil))
	(list 
	 (if (re-search-forward regexp (point-max) t)
	     (save-excursion
	       (beginning-of-line 1)
	       (let ((lnstart (point)))
		 (end-of-line)
		 (message "%s" (buffer-substring lnstart (point)))))
	   (error (format "No definition for %s" val))))))))

(defun switch-to-perl-doc-buffer ()
  "go to the perl documentation buffer and insert the documentation"
  (interactive)
  (let ((buf (get-buffer-create perl-doc-buffer)))
    (if (interactive-p)
	(switch-to-buffer-other-window buf)
      (set-buffer buf))
    (if (= (buffer-size) 0)
	(progn
	  (insert
	   "# @(#)@ perl-descr.el 1.8 - describe-perl-symbol [Perl 4.036]
!	Logical negation.	!= Numeric inequality.	!~ Negated pattern match or substitution.
!=	Numeric inequality.
!~	Search pattern, substitution, or translation (negated).
$!	If used in a numeric context, yields the current value of errno. If used in a string context, yields the corresponding error string.
$\"	The separator which joins elements of arrays interpolated in strings.
$#	The output format for printed numbers. Initial value is %.20g.
$$	The process number of the perl running this script. Altered (in the child process) by fork().
$%	The current page number of the currently selected output channel.
$&	The string matched by the last pattern match.
$'	The string following what was matched by the last pattern match.
$(	The real gid of this process.
$)	The effective gid of this process.
$*	Set to 1 to do multiline matching within a string, 0 to assume strings contain a single line. Default is 0.
$+	The last bracket matched by the last search pattern.
$,	The output field separator for the print operator.
$-	The number of lines left on the page.
$.	The current input line number of the last filehandle that was read.
$/	The input record separator, newline by default.
$0	The name of the file containing the perl script being executed. May be set
$1..	Contains the subpattern from the corresponding set of parentheses in the last pattern matched.
$:	The set of characters after which a string may be broken to fill continuation fields (starting with ^) in a format.
$;	The subscript separator for multi-dimensional array emulation. Default is \"\\034\".
$<	The real uid of this process.
$=	The page length of the current output channel. Default is 60 lines.
$>	The effective uid of this process.
$?	The status returned by the last backtick (``) command, pipe close or system operator.
$@	The perl error message from the last eval or do @var{EXPR} command.
$ARGV	The name of the current file used with <> .
$[	The index of the first element in an array, and of the first character in a substring. Default is 0.
$\\	The output record separator for the print operator.
$]	The perl version string as displayed with perl -v.
$^	The name of the current top-of-page format. See also $^D, $^F, $^I, $^P, $^T, $^W, $^X
$^D	The value of the perl debug (-D) flags.
$^F	The highest system file descriptor, ordinarily 2.
$^I	The value of the in-place edit extension (perl -i option).
$^P	Internal debugging flag.
$^T	The time the script was started. Used by -A/-M/-C file tests.
$^W	True if warnings are requested (perl -w flag).
$^X	The name under which perl was invoked (argv[0]).
$_	The default input and pattern-searching space.
$`	The string preceding what was matched by the last pattern match.
$|	If set to nonzero, forces a flush after every write or print on the currently selected output channel. Default is 0. The following variables are always local to the current block:
$~	The name of the current report format.
%	Modulo division.	%= Modulo division assignment.
%=	Modulo division assignment.
%ENV	Contains the current environment.
%INC	List of files that have been require-d or do-ne.
%SIG	Used to set signal handlers for various signals.
&	Bitwise and.	&& Logical and.
&&	Logical and.	&&= Logical and assignment.
&&=	Logical and assignment.
&=	Bitwise and assignment.
*	Multiplication.	** Exponentiation,
**	Exponentiation.
*NAME	Refers to all objects represented by NAME. *NAM1 = *NAM2 makes NAM1 a reference to NAM2.
+	Addition.	++ Auto-increment	+= Addition assignment.
++	Auto-increment (magical on strings).
+=	Addition assignment.
,	Comma operator.
-	Subtraction.	-- Auto-decrement.	-= Subtraction assignment.
--	Auto-decrement.
-=	Subtraction assignment.
-A	Access time in days since script started.
-B	File is a non-text (binary) file.
-C	Inode change time in days since script started.
-M	Age in days since script started.
-O	File is owned by real uid.
-R	File is readable by real uid.
-S	File is a socket .
-T	File is a text file.
-W	File is writable by real uid.
-X	File is executable by real uid.
-b	File is a block special file.
-c	File is a character special file.
-d	File is a directory.
-e	File exists .
-f	File is a plain file.
-g	File has setgid bit set.
-k	File has sticky bit set.
-l	File is a symbolic link.
-o	File is owned by effective uid.
-p	File is a named pipe (FIFO).
-r	File is readable by effective uid.
-s	File has non-zero size.
-t	Tests if filehandle (STDIN by default) is opened to a tty.
-u	File has setuid bit set.
-w	File is writable by effective uid.
-x	File is executable by effective uid.
-z	File has zero size.
.	Concatenate strings.	.= Concatenate assignment.	.. Alternation, also range operator.
..	Alternation, also range operator.
.=	Concatenate assignment strings
/	Division.	/= Division assignment.	/PATTERN/io	Pattern match
/=	Division assignment.
/PATTERN/io	Pattern match.
<	Numeric less than.	<< Bitwise shift left.
<<	Bitwise shift left.
<=	Numeric less than or equal to.	<=> Numeric compare.
<=>	Numeric compare.
=	Assignment.	== Numeric equality.	=~ Search pattern, substitution, or translation.
==	Numeric equality.
=~	Search pattern, substitution, or translation
>	Numeric greater than.	>= Numeric greater than or equal to.
>=	Numeric greater than or equal to.	>> Bitwise shift right.
>>	Bitwise shift right.	>>= Bitwise shift right assignment.
>>=	Bitwise shift right assignment.
? :	Alternation (if-then-else) operator.	?PATTERN? Backwards pattern match.
?PATTERN?	Backwards pattern match.
@ARGV	Contains the command line arguments for the script (not including the command name). See $0 for the command name.
@INC	Contains the list of places to look for perl scripts to be evaluated by the do EXPR command.
@_	Parameter array for subroutines. Also used by split if not in array context.
\\0	Octal char, e.g. \\033.
\\E	Case modification terminator. See \\L and \\U .
\\L	Lowercase until \\E .
\\U	Upcase until \\E .
\\a	Alarm character (octal 007).
\\b	Backspace character (octal 010).
\\c	Control character, e.g. \\c[ .
\\e	Escape character (octal 033).
\\f	Formfeed character (octal 014).
\\l	Lowercase of next character. See also \\L and \\u,
\\n	Newline character (octal 012).
\\r	Return character (octal 015).
\\t	Tab character (octal 011).
\\u	Upcase  of next character. See also \\U and \\l,
\\x	Hex character, e.g. \\x1b.
^	Bitwise exclusive or.
__END__	End of program source.
__FILE__	Current (source) filename.
__LINE__	Current line in current source.
accept(NEWSOCKET,GENERICSOCKET)
alarm(SECONDS)
atan2(X,Y)
bind(SOCKET,NAME)
binmode(FILEHANDLE)
caller[(LEVEL)]
chdir(EXPR)
chmod(LIST)
chop[(LIST|VAR)]
chown(LIST)
chroot(FILENAME)
close(FILEHANDLE)
closedir(DIRHANDLE)
cmp	String compare.
connect(SOCKET,NAME)
cos(EXPR)
crypt(PLAINTEXT,SALT)
dbmclose(ASSOC_ARRAY)
dbmopen(ASSOC,DBNAME,MODE)
defined(EXPR)
delete($ASSOC{KEY})
die(LIST)
do { ... }|SUBR while|until EXPR	executes at least once
do(EXPR|SUBR([LIST]))
dump LABEL
each(ASSOC_ARRAY)
endgrent
endhostent
endnetent
endprotoent
endpwent
endservent
eof[([FILEHANDLE])]
eq	String equality.
eval(EXPR) or eval { BLOCK }
exec(LIST)
exit(EXPR)
exp(EXPR)
fcntl(FILEHANDLE,FUNCTION,SCALAR)
fileno(FILEHANDLE)
flock(FILEHANDLE,OPERATION)
for (EXPR;EXPR;EXPR) { ... }
foreach [VAR] (@ARRAY) { ... }
fork
ge	String greater than or equal.
getc[(FILEHANDLE)]
getgrent
getgrgid(GID)
getgrnam(NAME)
gethostbyaddr(ADDR,ADDRTYPE)
gethostbyname(NAME)
gethostent
getlogin
getnetbyaddr(ADDR,ADDRTYPE)
getnetbyname(NAME)
getnetent
getpeername(SOCKET)
getpgrp(PID)
getppid
getpriority(WHICH,WHO)
getprotobyname(NAME)
getprotobynumber(NUMBER)
getprotoent
getpwent
getpwnam(NAME)
getpwuid(UID)
getservbyname(NAME,PROTO)
getservbyport(PORT,PROTO)
getservent
getsockname(SOCKET)
getsockopt(SOCKET,LEVEL,OPTNAME)
gmtime(EXPR)
goto LABEL
grep(EXPR,LIST)
gt	String greater than.
hex(EXPR)
if (EXPR) { ... } [ elsif (EXPR) { ... } ... ] [ else { ... } ] or EXPR if EXPR
index(STR,SUBSTR[,OFFSET])
int(EXPR)
ioctl(FILEHANDLE,FUNCTION,SCALAR)
join(EXPR,LIST)
keys(ASSOC_ARRAY)
kill(LIST)
last [LABEL]
le	String less than or equal.
length(EXPR)
link(OLDFILE,NEWFILE)
listen(SOCKET,QUEUESIZE)
local(LIST)
localtime(EXPR)
log(EXPR)
lstat(EXPR|FILEHANDLE|VAR)
lt	String less than.
m/PATTERN/iog
mkdir(FILENAME,MODE)
msgctl(ID,CMD,ARG)
msgget(KEY,FLAGS)
msgrcv(ID,VAR,SIZE,TYPE.FLAGS)
msgsnd(ID,MSG,FLAGS)
ne	String inequality.
next [LABEL]
oct(EXPR)
open(FILEHANDLE[,EXPR])
opendir(DIRHANDLE,EXPR)
ord(EXPR)
pack(TEMPLATE,LIST)
package	Introduces package context.
pipe(READHANDLE,WRITEHANDLE)
pop(ARRAY)
print [FILEHANDLE] [(LIST)]
printf [FILEHANDLE] (FORMAT,LIST)
push(ARRAY,LIST)
q/STRING/	Synonym for 'STRING'
qq/STRING/	Synonym for \"STRING\"
qx/STRING/	Synonym for `STRING`
rand[(EXPR)]
read(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
readdir(DIRHANDLE)
readlink(EXPR)
recv(SOCKET,SCALAR,LEN,FLAGS)
redo [LABEL]
rename(OLDNAME,NEWNAME)
require [FILENAME]
reset[(EXPR)]
return(LIST)
reverse(LIST)
rewinddir(DIRHANDLE)
rindex(STR,SUBSTR[,OFFSET])
rmdir(FILENAME)
s/PATTERN/REPLACEMENT/gieo
scalar(EXPR)
seek(FILEHANDLE,POSITION,WHENCE)
seekdir(DIRHANDLE,POS)
select(FILEHANDLE | RBITS,WBITS,EBITS,TIMEOUT)
semctl(ID,SEMNUM,CMD,ARG)
semget(KEY,NSEMS,SIZE,FLAGS)
semop(KEY,...)
send(SOCKET,MSG,FLAGS[,TO])
setgrent
sethostent(STAYOPEN)
setnetent(STAYOPEN)
setpgrp(PID,PGRP)
setpriority(WHICH,WHO,PRIORITY)
setprotoent(STAYOPEN)
setpwent
setservent(STAYOPEN)
setsockopt(SOCKET,LEVEL,OPTNAME,OPTVAL)
shift[(ARRAY)]
shmctl(ID,CMD,ARG)
shmget(KEY,SIZE,FLAGS)
shmread(ID,VAR,POS,SIZE)
shmwrite(ID,STRING,POS,SIZE)
shutdown(SOCKET,HOW)
sin(EXPR)
sleep[(EXPR)]
socket(SOCKET,DOMAIN,TYPE,PROTOCOL)
socketpair(SOCKET1,SOCKET2,DOMAIN,TYPE,PROTOCOL)
sort [SUBROUTINE] (LIST)
splice(ARRAY,OFFSET[,LENGTH[,LIST]])
split[(/PATTERN/[,EXPR[,LIMIT]])]
sprintf(FORMAT,LIST)
sqrt(EXPR)
srand(EXPR)
stat(EXPR|FILEHANDLE|VAR)
study[(SCALAR)]
substr(EXPR,OFFSET[,LEN])
symlink(OLDFILE,NEWFILE)
syscall(LIST)
sysread(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
system(LIST)
syswrite(FILEHANDLE,SCALAR,LENGTH[,OFFSET])
tell[(FILEHANDLE)]
telldir(DIRHANDLE)
time
times
tr/SEARCHLIST/REPLACEMENTLIST/cds
truncate(FILE|EXPR,LENGTH)
umask[(EXPR)]
undef[(EXPR)]
unless (EXPR) { ... } [ else { ... } ] or EXPR unless EXPR
unlink(LIST)
unpack(TEMPLATE,EXPR)
unshift(ARRAY,LIST)
until (EXPR) { ... } or EXPR until EXPR
utime(LIST)
values(ASSOC_ARRAY)
vec(EXPR,OFFSET,BITS)
wait
waitpid(PID,FLAGS)
wantarray
warn(LIST)
while  (EXPR) { ... } or EXPR while EXPR
write[(EXPR|FILEHANDLE)]
x	Repeat string or array.	x= Repetition assignment.
x=	Repetition assignment.
y/SEARCHLIST/REPLACEMENTLIST/
|	Bitwise or.	||	Logical or.
||	Logical or.
~	Unary bitwise complement.
")
	  (setq buffer-read-only t)))))


