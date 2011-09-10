/*
 *              configure.cmd for Mew
 *
 *       Copyleft:-) 1997 OKUNISHI Fujikazu
 *
 *       This [OS/2] REXX procedure conforms
 *      GNU GENERAL PUBLIC LICENSE Version 2.
 *
 * Created: Jun 22,1997
 * Revised: Sep 16,1997
 * Keywords: Mew, OS/2, rexx, configure, install
 *
 * special thanks to:
 *   SASAKI Osamu    <s-osamu@ppp.bekkoame.or.jp>
 *   OHMORI Norihito <ohmori@p.chiba-u.ac.jp>
 */

  Trace Off

  Call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
  Call SysLoadFuncs
  Parse Arg Argv.1 Argv.2 Argv.3 .

/* set variable */
  Parse Source . . thiscmd
   thiscmd = Filespec('Name',thiscmd)  /* OS/2 REXX */
  DefaultPerl5 = 'perl5'
  CacheFile    = '.\os2config.cache'

/* ----------------------------------------------------------------- *
 *            cachefile exist ?
 * ----------------------------------------------------------------- */
  If Stream(CacheFile,'c','query exist') <> ''
    Then Do
         Say 'use sed script' CacheFile
         Signal CREATE
    End
  Say 'NOT exist' CacheFile

/* ------------------------------------------------------------------ *
 *            Argment item syntax check   CASE: no cachefile
 * ------------------------------------------------------------------ */
  ConfVal.1.0='--autoconf';  ConfVal.1.1=''
  ConfVal.2.0='--perl5';  ConfVal.2.1=''
  ConfVal.3.0="--help"

  Do lp = 1 To 2
    If Argv.lp = "" Then Leave
    Do lp2 = 1 To 4
      If lp2 = 4 Then
        Do
          Say Argv.lp "???"
          Signal USAGE
        End
      match = Compare(ConfVal.lp2.0, Argv.lp)
      If match = 0 Then
        Do
          If lp2 = 3 Then Signal USAGE  /*14*/
          ConfVal.lp2.1="yes"
          Leave
        End
      If (match > Length(ConfVal.lp2.0)) & (Substr(Argv.lp, match,1) = '=')
        Then Do
          ConfVal.lp2.1=Substr(Argv.lp, match+1)
          Leave
        End
    End
  End
  AutoConf = Translate(ConfVal.1.1)
    If AutoConf <> 'YES' Then AUTOCONF = 'NO'
  Perl5Prog = ConfVal.2.1
    If Translate(Perl5Prog)='YES'|Perl5prog =''
      Then Perl5Prog=''
      Else Signal GENERATE

/* ----------------------------------------------------------------- *
 *            get perl5 program name w/ fullpath
 * ----------------------------------------------------------------- */
  If AUTOCONF='YES'
    Then Perl5Prog = SysSearchPath('PATH',DefaultPerl5 || '.exe')
    Else Do  /* AUTOCONF=NO */
     Do Forever
      Say "Perl5 program name ? (default =" DefaultPerl5 ")"
      Parse Pull Perl5Path .
      If Perl5Path = "" Then
        Do
          Perl5Path = DefaultPerl5
          Cursor = SysCurPos( Word( SysCurPos( ), 1 ) -1, 0 )
          Say Perl5Path
        End
      ChkExt = Perl5Path
      Parse Upper Var ChkExt ChkExt
      If Pos( ".EXE", ChkExt ) = 0 Then Perl5Path = Insert(Perl5Path, ".exe")
      Drop ChkExt
      Perl5Prog = SysSearchPath("PATH", Perl5Path)
      If Perl5Prog = ""
       Then
        Do
          Say "Perl program `"Perl5Path"' Not Found."
          Signal PROG_END
        End
       Else
        Do
          Say "Perl5 executable `"Perl5Prog"'. Found."
          Say "Use this program ? Yes or No ( default = Yes )"
          Pull Anser .
          If (Anser = "") | (Anser = "YES" ) Then
            Do
              Cursor = SysCurPos( Word( SysCurPos( ), 1 ) -1, 0 )
              Say "Yes"
              Leave
            End
        End
     End
    End /* AUTOCONF=YES */
    Perl5Prog = Translate(Perl5Prog,Xrange('a','z'),Xrange('A','Z'))
    Perl5Prog = Translate(Perl5Prog,'/','\')

 /* generate sed script file */
GENERATE:
  Call Lineout STDERR,'AutoConf='autoconf', Perl5Prog='perl5prog
  SedScr = 's|@PERL@|' || Perl5Prog || '|'
  Call LineOut CacheFile, SedScr
  Call LineOut CacheFile

/* ----------------------------------------------------------------- *
 *  search `*.in' files, then create
 * ----------------------------------------------------------------- */
CREATE:
  Call SysFileTree '*.in', 'src', 'FO'
  If src.0 <> '0' Then
    Do
      Say 'Please Wait...'
      Do i=1 To src.0
        /* delete '.in' strings */
        target = Reverse(Delstr(Reverse(src.i),1,3))
        If Filespec('Name',src.i) <> 'configure.in' &,
           Filespec('Name',src.i) <> 'Makefile.in'
          Then
           Do
             Say 'creating' target
             '@sed -f' CacheFile src.i '>' target
             Say 'Done.'
           End
      End
  End
  Exit

/* ----------------------------------------------------------------- *
 *	Display usage message
 * ----------------------------------------------------------------- */
USAGE:
  Say 'Usage:' thiscmd 'CONFIGURATION [-OPTION[=VALUE] ...]'
  Say
  Say 'Set configration and installation parameters for IM.'
  Say 'CONFIGURATION specifies the operating system to build for.'
  Say
  Say ConfVal.1.0||'		skip manual configuration'
  Say ConfVal.2.0||'			set Perl5 executable w/ fullpath'
  Say
  Say 'for example:'
  Say '	' thiscmd ConfVal.1.0
  Say '	' thiscmd ConfVal.2.0 || '=d:/usr/local/bin/perl(.exe)'


PROG_END:
Exit

/* End of procedure */
