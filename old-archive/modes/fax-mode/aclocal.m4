AC_DEFUN(RS_PATH_EMACS_LIBEXEC,[
AC_MSG_CHECKING(for Emacs' executable directory)
AC_CACHE_VAL(rs_cv_path_emacs_libexec,[
echo "(setq d exec-directory)
(if (string-match \"/\\\\'\" d)
    (setq d (substring d 0 (match-beginning 0))))
(princ d)" >./conftest.el
d=`emacs -batch -l ./conftest.el 2>/dev/null`
if test $? -eq 0 -a -d $d
then
  rs_cv_path_emacs_libexec=$d
else
  rs_cv_path_emacs_libexec="not found"
  for d in /usr/local/libexec /usr/local/lib /usr/libexec /usr/lib
  do
    if test "$rs_cv_path_emacs_libexec" != "not found"
    then
      break
    fi
    d=`find $d/emacs -perm +111 -name emacsserver 2>/dev/null | tail -n 1`
    if test $? -eq 0
    then
      d=`dirname $d`
      if test -d $d
      then
        rs_cv_path_emacs_libexec=$d
      fi
    fi
  done
fi
rm -f conftest.el])
AC_MSG_RESULT($rs_cv_path_emacs_libexec)
if test "$rs_cv_path_emacs_libexec" != "not found"
then
  EMACS_LIBEXECDIR=$rs_cv_path_emacs_libexec
  AC_SUBST(EMACS_LIBEXECDIR)
fi
AC_PROVIDE([$0])
])

AC_DEFUN(RS_PATH_EMACS_LOCALLISP,[
AC_MSG_CHECKING(for Emacs' local lisp directory)
changequote(<<,>>)dnl
AC_CACHE_VAL(rs_cv_path_emacs_locallisp,<<
echo "(setq d data-directory)
(if (string-match \"/\\\\'\" d)
    (setq d (substring d 0 (match-beginning 0))))
(if (string-match \"/[0-9]+\\\\.[0-9]+/etc\\\\'\" d)
    (setq d (substring d 0 (match-beginning 0)))
  (setq d (concat d \"/../..\")))
(princ d)" >./conftest.el
d=`emacs -batch -l ./conftest.el 2>/dev/null`
if test $? -eq 0 -a -d $d
then
  rs_cv_path_emacs_locallisp=$d/site-lisp
else
  rs_cv_path_emacs_locallisp="not found"
  for d in /usr/local/share /usr/share /usr/local/lib /usr/lib
  do
    if test "$rs_cv_path_emacs_locallisp" != "not found"
    then
      break
    fi
    if test -d $d/emacs/site-lisp
    then
      rs_cv_path_emacs_locallisp=$d/emacs/site-lisp
    fi
  done
fi
rm -f conftest.el>>)
changequote([,])dnl
AC_MSG_RESULT($rs_cv_path_emacs_locallisp)
if test "$rs_cv_path_emacs_locallisp" != "not found"
then
  EMACS_LOCALLISPDIR=$rs_cv_path_emacs_locallisp
  AC_SUBST(EMACS_LOCALLISPDIR)
fi
AC_PROVIDE([$0])
])
