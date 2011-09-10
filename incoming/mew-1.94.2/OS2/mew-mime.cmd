/* ----------------------------------------------------------------- *
 *      mew-mime.cmd  --- external method for OS/2 Mew
 *
 *        Copyright (C) 1996,1997,1998  OKUNISHI Fujikazu
 *        Copyright (C) 1997,1998       KONDO Hiroyasu
 *
 * Author: OKUNISHI Fujikazu <fuji0924@mbox.kyoto-inet.or.jp>
 *         KONDO Hiroyasu    <hirokon@homi.toyota.aichi.jp>
 *
 * Version:
 *     $Id: mew-mime.cmd,v 1.2 1998/01/29 14:09:17 kazu Exp $
 * Requirement:
 *     VREXX2.ZIP : Visual REXX for Presentation Manager(IBMEWS,Freeware)
 * ----------------------------------------------------------------- */

  Trace Off; '@echo off'; vrexx=1
  If RxFuncQuery('SysLoadFuncs') Then Do
	Call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
	Call SysLoadFuncs
  End
  If RxFuncQuery('VInit') Then rb=RxFuncAdd('VInit', 'VREXX', 'VINIT')
  If VInit()=='ERROR' Then Do; Call VExit; vrexx=0; End

  Signal on failure name QUIT
  Signal on halt name QUIT
  Signal on syntax name QUIT

  Parse Source . . this .
  cmdPath = Left(this, Lastpos('\',this) -1)
  this = Filespec('Name',this)
  this = Left(this, Length(this) -4)

/* ----------------------------------------------------------------- *
 *  parameter check		"mew-mime {C-T:} {target}"
 * ----------------------------------------------------------------- */
  Parse Arg ct target opt .
   If target=='' Then Do
     Call EMSG(0 'no parameters.')
     Call VMSG(1 'usage:' this '{C-T:} {targetfile}')
   End
   ct=Translate(ct)
   target=Translate(target,'\','/')
   If Pos('\',target) ==0 Then target='.\' || target

/* ----------------------------------------------------------------- *
 *  initialize
 * ----------------------------------------------------------------- */
  env = 'OS2ENVIRONMENT'
  home = Translate(Value('HOME',,env),'\','/')

  tmpdir = Left(target, Lastpos('\',target) -1)
  savedir=Value('MEW_SAVE_DIR',,env)
   If savedir=='' Then savedir=Value('TMP',,env)
   If savedir=='' Then savedir=Value('TEMP',,env)
   If savedir=='' Then savedir='.'
  tmp_fn = SysTempFileName('\mew-????')
  initfile = home'\.'this'rc'
  logfile = home'\'this'.log'
  If Stream(initfile,'C','Query Exist')=''
    Then initfile=cmdPath||inifspec
  If Stream(initfile,'C','Query Exist')='' Then Do
    Call VMSG(0 initfile 'not exists.')
    Signal SAVE
  End

/* ----------------------------------------------------------------- *
 *  get progname from initfile
 * ----------------------------------------------------------------- */
  /* SysFileSearch cannot understand EOL=LF (T_T) */
   Do While Lines(initfile)
     def = Translate(Linein(initfile),'',D2C(9))  /*TAB=>[white space]*/
     If Translate(Word(def,1)) = ct Then Do
       Parse Var def mtype suffix progname args
       If ct == Translate(mtype) Then Signal EXEC
     End
   End

/* ----------------------------------------------------------------- *
 *  undefined C-T: ==> treated as `Application/Octet-stream'
 * ----------------------------------------------------------------- */
SAVE:
	Call EMSG(0 'undefined C-T: in' initfile)
	save_fn = savedir || tmp_fn || '.bin'
	If vrexx Then Do
	  Call VDialogPos 10, 50
	  button = VFileBox(this': Save as', save_fn, 'file')
	  If button=='OK' Then Do
	    save_fn = file.vstring
	    'copy' target save_fn '1>nul'
	    If rc<>0 Then Call VMSG(1 'cannot save as' save_fn)
	             Else Call VMSG(1 'saved as' save_fn)
	  End
	  Else Call VMSG(1 'Aborted!!')
	End
	Else Do  /*w/o VREXX support*/
	  'copy' target save_fn '1>nul'
	  If rc<>0 Then Call FMSG(1 'cannot save as' save_fn)
	           Else Call FMSG(1 'saved as' save_fn)
	End
Signal QUIT


/* ----------------------------------------------------------------- *
 *  exec extprog
 * ----------------------------------------------------------------- */
EXEC:
	Call LineOut(initfile)  /*close*/
	/* -------------------------------------------------------- *
	 *  CTE: x-uue	  `begin 644 foo.gif'
	 * -------------------------------------------------------- */
	head = Linein(target)
	Call Lineout target  /*close*/
	If Translate(Word(head,1))='BEGIN' & Datatype(Word(head,2),'W')=1 Then Do
	  uufname = Word(head,3)
	  'call uumerge -d' savedir target
	  If rc<>0 Then Call VMSG(1 'uumerge error!')
	           Else fname=savedir'\'uufname
	End
	Else Do
	  tmp_fn = tmpdir ||tmp_fn || '.' || suffix
	  'copy' target tmp_fn '>nul'
/*
 *	  If Pos('TEXT/',ct)>0 Then Do
 *	    cocoexe = SysSearchPath('PATH','coco.exe')
 *	    If Pos('ROT13',ct)>0
 *	      Then  cocoexe '*sjis* <' target '|nkf -rs>' tmp_fn '2>nul'
 *	      Else  cocoexe '*sjis* <' target '>' tmp_fn '2>nul'
 *	  End
 *	  Else 'copy' target tmp_fn '>nul'
 */
	End

	args = Space(args)
	progname = Translate(progname,'\','/')
	/* ----------------------------------------- *
	 *  Netscape/nsclient
	 *     "d:" => "/d^|"  or "/d%7C"
	 * ----------------------------------------- */
	prog=Translate(progname)
	If Pos('NETSCAPE',prog)>0|Pos('NSCLIENT',prog)>0 Then
	  tmp_fn='file:///'|| Left(tmp_fn,1) ||"^|"|| Delstr(tmp_fn,1,2)
	'start' progname args tmp_fn
	/*'detach' progname args tmp_fn*/
Signal QUIT

/* ----------------------------------------------------------------- *
 *  error messages
 * ----------------------------------------------------------------- */
EMSG: Procedure
	Parse Arg bye msg
	Call LineOut STDERR,msg
	If bye Then Exit 999
Return

/* ----------------------------------------------------------------- *
 *  error dialog  (with VREXX.DLL)
 * ----------------------------------------------------------------- */
VMSG: Procedure  Expose this vrexx
	Parse Arg bye msg
	If vrexx Then Do
	  mbox.0=1; mbox.1=msg
	  Call VmsgBox this, mbox, 1
	End
	If bye Then Signal QUIT
Return

/* ----------------------------------------------------------------- *
 *  error log  (without VREXX support)
 * ----------------------------------------------------------------- */
FMSG: Procedure  Expose logfile
	Parse Arg bye msg
	Call Lineout logfile, Date() Time() '09'x msg
	If bye Then Exit 999
Return

/* ----------------------------------------------------------------- *
 *  exit
 * ----------------------------------------------------------------- */
QUIT:
	If vrexx Then Call VExit
Exit

/* end of mew-mime.cmd */
