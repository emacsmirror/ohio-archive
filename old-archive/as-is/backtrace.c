From: mct@philabs.Philips.Com (Mark C. Tucker)
Subject: A Slightly Better *Backtrace*
Date: 19 Nov 87 13:25:02 GMT
Organization: Philips Laboratories, Briarcliff Manor, NY


	One small problem with the emacs debugger is that it prints out
			(if ...)
and you're not really sure which if it is.

	The changes described below make a small improvement.  Instead of just
printing "...", it 'grinds' out the actual parameters to a depth of 2.
[Because the *Backtrace* window has disabled line wrapping, it doesn't really matter if there is to much information on the line.]
* (let? ((my-indent (+++))) (if (and see-hard-blanks (+++ +++ +++)) (progn (++

	It does it by using a grind_depth variable which is tested by the
basic 'print' function.

	In addition, it distinguishes normal functions that evaluate their arguments from those "things" like 'if' that are special forms.
Therefore, it prints a question mark [eg, (let? ...)].
	?		when arguments are evaluated
	+		when argumnts are not evaluated


	Below are the changes to the sources.


	Enjoy,

	-- Mark Tucker

	mct@philabs
==============================================

This function is from EMACS 18.49

DEFUN ("backtrace", Fbacktrace, Sbacktrace, 0, 0, "",
  "Print a trace of Lisp function calls currently active.\n\
Output stream used is value of standard-output.")
  ()
{
  register struct backtrace *backlist = backtrace_list;
  register int i;
  register Lisp_Object tail;
  Lisp_Object tem;

  while (backlist)
    {
      write_string (backlist->debug_on_exit ? "* " : "  ", 2);
      if (backlist->nargs == UNEVALLED)
        write_string ("(", -1);
      tem = *backlist->function;
      Fprin1 (tem, Qnil);	/* This can QUIT */
      if (backlist->nargs == UNEVALLED)
	{
#ifdef NEVERDEF
	 /* mct: original stuff */
	  if (backlist->evalargs)
	    write_string (" ...computing arguments...", -1);
	  else
	    write_string (" ...", -1);
#else
	{extern int grind_depth; /* mct */
	 grind_depth = 2;
	 if (backlist->evalargs)
	   write_string ("? ", -1);
	 else
	   write_string ("+ ", -1);

	 for (tail = *backlist->args, i = 0; !NULL (tail); tail = Fcdr (tail), i++)
	   {
	     if (i) write_string (" ", -1);
	     Fprin1 (Fcar (tail), Qnil);
	   }
	 grind_depth = -1;
       }
#endif
	}
      else if (backlist->nargs == MANY)
	{
	  write_string ("(", -1);
	  for (tail = *backlist->args, i = 0; !NULL (tail); tail = Fcdr (tail), i++)
	    {
	      if (i) write_string (" ", -1);
	      Fprin1 (Fcar (tail), Qnil);
	    }
	}
      else
	{
	  write_string ("(", -1);
	  for (i = 0; i < backlist->nargs; i++)
	    {
	      if (i) write_string (" ", -1);
	      Fprin1 (backlist->args[i], Qnil);
	    }
	}
      write_string (")\n", -1);
      backlist = backlist->next;
    }
  return Qnil;
}


int grind_depth; = -1;		/* mct *//* initialize it in print_syms! */
static void
print (obj, printcharfun, escapeflag)
#ifndef RTPC_REGISTER_BUG
     register Lisp_Object obj;
#else
     Lisp_Object obj;
#endif
     register Lisp_Object printcharfun;
     int escapeflag;
{
  char buf[30];

  QUIT;

/*#ifdef NEVERDEF*/

  if( (grind_depth != -1)  && print_depth > grind_depth ) /* mct */
      {
      strout ("+++", -1, printcharfun );
      return;
      }
/*#endif*/

  print_depth++;
  if (print_depth > 200)
    error ("Apparently circular structure being printed");
#ifdef MAX_PRINT_CHARS
  if (max_print && print_chars > max_print)
    {
      PRINTCHAR ('\n');
      print_chars = 0;
    }
#endif /* MAX_PRINT_CHARS */

  switch (XTYPE (obj))
    {
    default: ...
    case Lisp_Int: ...
    case Lisp_String: ...
    case Lisp_Symbol: ...
    case Lisp_Cons: ...
    case Lisp_Vector: ...
    case Lisp_Buffer: ...
    case Lisp_Process: ...
    case Lisp_Window: ...
    case Lisp_Window_Configuration: ...
    case Lisp_Marker: ...
    case Lisp_Subr: ...
    }

  print_depth--;
}

==============================================
-- 
Mark Tucker				(914) 945-6361
Philips Laboratories			philabs.philips.com!mct@seismo.CSS.GOV
Briarcliff Manor, NY 10510		{seismo,decvax,ihnp4}!philabs!mct
