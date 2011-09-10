      program phit
c----------------------------------------------------------------------
c  version 0.13    27 December, 1995
c----------------------------------------------------------------------
c            written by      Bruce Ravel
c                            Department of Physics
c                            Box 351560
c                            University of Washington
c                            Seattle, WA   USA 98195
c
c                 phone      (206) 543-0435 
c                 e-mail     ravel@u.washington.edu
c
c----------------------------------------------------------------------
c      description of the code:
c
c---------------------------------------------------------------------
c  version history:
c    0.01-0.04:  in construction, reads input, fits a lineshape, 
c                writes output, set math expressions, simple statistics,
c                lots of lineshapes (2/95)
c    0.05: beta release, fits, full encoding functionality, calculates 
c          full statistics (2/95)
c    0.06: fixed reporting of correlations (5/95) normalize gaussians 
c          and lorentzians to unit area, improve parsing of sets and 
c          functions, nptx=250 nsetx=300 (6/95)
c    0.07: check explicit and implicit dependence on xaxis of all set 
c          values, allow user to specify # of redecodings with loop
c          keyword (dangerous!), resid keyword & output, set sigres
c          to standard deviation of residuals assuring unit chi-square
c          (6/95)
c    0.08: added peak position function, improved handling of sets and 
c          guesses in nofit option (6/95)
c    0.09: ordering of set values to assure proper single pass evaluation
c          and minimize calls to decod, jconst=2000 in encod/decod, 
c          improved data file headers, avoid i/o filename collisions, 
c          include files (6/95)
c    0.10: propagate error bars into set values, new inout (7/95)
c    0.11: recognize npoints for fit output (7/95)
c          allow user-specified numerical functions, must be on same grid
c          as data and be in 2 column ascii files and write=fit (8/95)
c    0.12: fix min and max in encod package and fixed number of iterations 
c          bug in phset. (11/95)
c    0.13: fixed some bugs in phline (11/95) include file message, echo
c          keyword with @, g format specifier in phlog, nonnegative
c          gauss and lor widths, parse all set values on first 
c          iteration (12/95)
c---------------------------------------------------------------------
c  in the stack (in no particular order) :
c    improve input lineshapes (interpolated)
c    more evaluated lineshapes
c    check dependence of functions on guesses to flag unused guess values
c       the deal here is a guess value used in a set value that is not used
c       in a function (aargh!)
c---------------------------------------------------------------------

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      character*75 messg, reason, how, line
 4000 format('PHIT ',a5,51x,'by Bruce Ravel')
 4010 format(75('='))
 4030 format('Phit --- Fitting ',i4,' data points with ',i2,
     $            ' variables.')
 4035 format('         Using ',i2,' set expressions and ',i2,
     $            ' functions.')
 4036 format('         Using ',i2,' set expressions and ',i2,
     $            ' function.')
 4037 format('         Using ',i2,' set expression and ',i2,
     $            ' functions.')
 4038 format('         Using ',i2,' set expression and ',i2,
     $            ' function.')
 4040 format('         Using ',i2,' input lineshapes.')
 4041 format('         Using ',i2,' input lineshape.')

c---------------------------------------------------------------------
c  this should match the version number in the third line of this file
      versn = '0.13 '
c---------------------------------------------------------------------
      vaxflg  = .false.
c VAX USERS:  change the line above to .true.
c VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX 
c---------------------------------------------------------------------
c  run time messages: lines of '=' and version number
      write(messg,4010)
      call messag(messg)
      write(messg,4000)versn
      call messag(messg)
      write(messg,4010)
      call messag(messg)

c-----------------------------------------------------------------
c  initialize all variables in common
      call phinit

c-----------------------------------------------------------------
c  parse input file
      call messag(' ')
      call messag('Phit --- Parsing input file.')
      call phinpt
      if (lend) then
          call messag('         Input file empty.  Phit is stopping.')
          goto 99
      endif
      if (imess.eq.99) then
          call dump
          goto 99
      endif

c-----------------------------------------------------------------
c  read data file
      if (data.ne.nullwd) call phdata

c-----------------------------------------------------------------
c  read lineshapes
      if (nline.gt.0) call phline
      
c-----------------------------------------------------------------
c  encode all math expressions, order set expressions for one-pass
c  evaluation, check sets for dependence on x-axis
      call messag('Phit --- Encoding math expressions.')
      call messag('         Evaluating functional dependences '//
     $            'of set values.')
      call phset
      call phenco

c-----------------------------------------------------------------
c  dry-run complete
      call messag(' ')
      write(messg,4030)nfit, nguess
      call messag(messg)
      if ((nset.ne.1).and.(nfunr.ne.1)) then
          write(messg,4035)nset, nfunr
          call messag(messg)
      elseif ((nset.ne.1).and.(nfunr.eq.1)) then
          write(messg,4036)nset, nfunr
          call messag(messg)
      elseif ((nset.eq.1).and.(nfunr.ne.1)) then
          write(messg,4037)nset, nfunr
          call messag(messg)
      else
          write(messg,4038)nset, nfunr
          call messag(messg)
      endif
      if (nline.gt.1) then
          write(messg,4040)nline
          call messag(messg)
      elseif (nline.eq.1) then
          write(messg,4041)nline
          call messag(messg)
      endif
      if (ldry) then
          call messag(' ')
          call messag('         Dry run complete.  Phit is stopping.')
          goto 99
      endif

c-----------------------------------------------------------------
c  fitting time
      if (.not.nofit) then
          call phfit
          call messag('              Fitting completed.')
          call messag(' ')
          if (noerr) then
              call messag('Phit --- Skipping error analysis,'//
     $                    ' as requested.')
              goto 80
          endif
          if (lsig) then
              how = 'error bars supplied with data.'
          elseif (sigdat.gt.epsi) then
              how = 'constant sigma supplied in input file.'
          else
              how = 'standard deviation of the residuals.'
          endif
          ii = istrln(how)
          call messag('Phit --- Estimating uncertainties using '//
     $                how(:ii))
          if (nfit.le.nguess) then
              line = '* * * WARNING'
              call messag(' ')
              call messag(line)
              line = '      You have over-specified your data set.'
              call messag(line)
              line = '      Reduced chi-square cannot be calculated.'
              call messag(line)
              line = '      Error bars will be estimated but not '//
     $                    'scaled.'
              call messag(line)
              call messag(' ')
          endif
          call pherr
      else
          reason = '"nofit" keyword was found'
          if (inofit.eq.1) reason = 'no guessed values were found'
          if (inofit.eq.2) reason = 'no input data were specified'
          ii = istrln(reason)
          call messag('Phit --- Not fitting, '//reason(:ii)//
     $                ' in input file.')
          call messag(' ')
          call phnoft
      endif

c-----------------------------------------------------------------
c  write results of fit to log file, write data
 80   continue 

c  get final evaluation of sets and functions, propagate error bars
      call messag('Phit --- Evaluating function at best-fit values.')
      call pheval
      if ((.not.nofit).and.(.not.noerr).and.(nfit.gt.nguess)) then
          call messag('Phit --- Propagating error bars into set '//
     $                'values.')
          if (ierbar.eq.0) call phprop
      endif

      ii = istrln(logout)
      call messag('Phit --- Writing log file: "'//logout(:ii)//'"')
      call phlog
      if ((nosum).and.(.not.lall)) then
          call messag('Phit --- Not writing output data files,'//
     $                ' as requested.')
      else
          call messag('Phit --- Writing output files.')
          call phout
      endif
      call messag(' ')
      call messag('Phit is finished.')

 99   continue 
      call messag(' ')
      stop
c  end main program phit
      end
      subroutine phinit

c  initialize every variable held in the common blocks

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


c-------- integers --------------------------------------------------
      ntit   = 0
      nfun   = 0
      nfunr  = 0
      nguess = 0
      nset   = 0
      imess  = -1
      nkey   = 0
      ndoc   = 0
      ndata  = 0
      inofit = -1
      nptout = -1
      ixr    = 0
      iter   = 0

      do 20 i=1,ncodex
        do 10 j=1,nfunx
          icode(i,j) = 0
 10     continue 
        do 15 j=1,nsetx
          icdset(i,j) = 0
 15     continue 
 20   continue 

      do 30 i=1,nvarx
        ipoint(i) = 0
 30   continue 

      do 40 i=1,nguesx
        ierflg(i) = 0
 40   continue 

      do 50 i=0,nlinex
c        ixcol(i)  = 1
c        iycol(i)  = 2
        lindat(i) = nullwd
        lname(i)  = nullwd
        do 45 j=1,ncolx
          label(j,i) = '&'
 45     continue 
 50   continue 

c-------- logicals --------------------------------------------------
      lall   = .false.
      lend   = .false.
      ldry   = .false.
      nofit  = .false.
      lsig   = .false.
      nosum  = .false.
      noerr  = .false.
      lresid = .false.
      do 90 i=1,nsetx
        ldepx(i)  = .false.
        ldepg(i)  = .false.
        iptset(i) = i
 90   continue 

c-------- characters ------------------------------------------------
      data   = nullwd
      outfil = nullwd
      xaxis  = 'x'
      frmin  = 'ascii'
      frmout = 'ascii'
      ftype  = 'xmu'
      skey   = ' '
      wrtout = 'full'
      logout = 'phit.log'
      ignore = '#'

      do 110 i=1,ntitx
        title(i) = ' '
 110  continue 

      do 120 i=1,nguesx
        gname(i) = ' '
 120  continue 

      do 130 i=1,nsetx
        sname(i) = ' '
 130  continue 

      do 140 i=1,nfunx
        functn(i) = nullwd
        id(i)     = nullwd
 140  continue 

      do 150 i=1,nvarx
        vnames(i) = ' '
 150  continue 

      do 160 i=1,ndocx
        doc(i) = ' '
 160  continue 

      do 170 i=1,nsetx
        set(i) = nullwd
 170  continue 

c-------- floating point numbers ------------------------------------

      chisqr = zero
      sigres = one
      sigdat = zero
      cormin = 0.15

c-------- floating point arrays  ------------------------------------
      do 210 i=1,ndatx
        xdata(i) = 0.e0
        ydata(i) = 0.e0
        xout(i)  = 0.e0
        yout(i)  = 0.e0
        dummy(i) = 0.e0
        fvect(i) = 0.e0
        diff(i)  = 0.e0
 210   continue 

      do 220 i=1,nrangx
        range(i) = valnul - 1
 220  continue 

      do 230 i=1,nguesx
        guess(i) = zero
        delta(i) = 0.e0
        do 225 j=1,nguesx
          correl(i,j) = 0.e0
 225    continue 
 230  continue 

      do 240 i=1,nsetx
        setval(i) = zero
        delset(i) = zero
 240  continue 

      do 260 i=1,nptx
        do 250 j=1,nvarx
          values(i,j) = zero
 250    continue 
        defval(i) = zero
 260  continue 

      do 270 i=1,nconx
        consts(i) = zero
 270  continue 

      return 
c  end subroutine phinit
      end
      subroutine phinpt

c---------------------------------------------------------------------
c  parse the input file by searching for keywords and assigning values 
c  to the parameters associated with the keywords.  also perform some 
c  error checks on the input parameters. 
c---------------------------------------------------------------------

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      parameter (nwdx=20, nincx=4)
c  nincx: max depth of nested include files

      logical      there, inpnul, lstop, noout, lech(5)
      character*2  test
      character*6  cnum
      character*72 words(nwdx), incfil(nincx), echstr
      character*80 string, titln*72, temp
      real         value
      integer      incunt(nincx)
c  incfil, incunt: names and unit numbers of active include files

 1410 format(bn,f10.0)
 1440 format(a)
 1450 format(a,i2)
 4000 format(i2) 
 4010 format(i6)

c---------------------------------------------------------------------
c  initialize some things used only in this routine
      iline  = 0
      ninc   = 0
      do 10 i=1,nincx
        incfil(i) = ' '
        incunt(i) = 0
 10   continue 
      do 15 i=1,5
        lech(i) = .false.
 15   continue 
      inpnul = .true.
      noout  = .false.
      test   = 'ab'
c  the value of the variable test must be in the same case as the 
c  keyword names in the long block of elseif's

c---------------------------------------------------------------------
c  open phit.inp, look for upper and lower case file names 
      fname = 'phit.inp'
      call lower(fname)
      inquire(file=fname,exist=there)
      if (.not.there) then
          call upper(fname)
          inquire(file=fname,exist=there)
      endif
      if (.not.there) then
          call messag('         ERROR! Input file for PHIT is '//
     $                'not found.')
          call messag('Phit is stopping.')
          call messag(' ')
          stop
      endif
      open(unit=1,file=fname,status='old')
      inpunt = 1

c---------------------------------------------------------------------
c  begin reading the input file, check if line is blank or comment 
c  beginning the parsing, inpunt is unit of presently read file --
c  phit.inp is 1, include files are numbered from 10
 101  continue
      read (inpunt,1440,end=191)string
c      print*,'reading from unit #',inpunt
      iline=iline+1
      call untab(string)

 120  continue
      nwds = nwdx
      do 122 iw=1,nwds
        words(iw)=' '
 122  continue
      call triml(string)
c                                           - denotes end of job
      if  (string(1:1).eq.'-') goto 191
c                                           skip a comment line
      if  ((string(1:1).eq.'!').or.(string(1:1).eq.'*')
     $ .or.(string(1:1).eq.'%').or.(string(1:1).eq.'#')) goto 101
c                                           skip a blank line
      if  (string.eq.' ') goto 101
c                                           begin reading line
      i=1
      call bwords(string,nwds,words)
      inpnul = .false.

c  ******************** input file parsing ************************* 
c  read a word, identify it, assign the value from the following word(s)
c  increment i and come back.  i points to position in string, when i
c  exceeds nwds go read a new line.
130   continue
      call case(test,words(i))
c                                           skip a blank line
      if     (words(i).eq.' ') then
          goto 101
c                                           ignore everything after !,%
c                                           need to handle * separately
      elseif ((words(i)(1:1).eq.'!').or.(words(i)(1:1).eq.'%')
     $                .or.(words(i)(1:1).eq.'#')) then
          goto 101

c     ********************************************************************
c     ************************************* read header information

c                                           title and comment are synonyms
      elseif ((words(i).eq.'comment').or.(words(i).eq.'title')) then
          if (ntit.lt.ntitx) then
              call gettit(words(i), string, titln, ntit, .false.)
              title(ntit) = titln
          endif
          goto 101

c     ********************************************************************
c     ************************************* file names and formats

c                                           data file
      elseif ((words(i).eq.'data').or.(words(i).eq.'infile').or.
     $                (words(i).eq.'read')) then
          data = words(i+1)
          i=i+2
      elseif (words(i).eq.'skey') then
          skey = words(i+1)
          i=i+2
      elseif (words(i).eq.'nkey') then
          call getint(words(i),words(i+1),nkey)
          i=i+2
c                                           output file name
      elseif (words(i)(1:3).eq.'out') then
          outfil = words(i+1)
          i=i+2
c                                           log file name
      elseif (words(i)(1:3).eq.'log') then
          logout = words(i+1)
          i=i+2
c                                           include file
c     --- record name, assign a unit number, open the file, begin reading 
c         after present line, return to previous input file when done
      elseif (words(i).eq.'include') then
          if (ninc.eq.nincx) goto 670
          do 20 nnn=1,ninc
            if (words(i+1).eq.incfil(nnn)) goto 671
 20       continue 
          ninc = ninc+1
          incfil(ninc) = words(i+1)
          incunt(ninc) = nxtunt(10)
          open(unit=incunt(ninc), file=incfil(ninc), status='old')
          ii=istrln(incfil(ninc))
          call messag('          include > reading file: "'//
     $                incfil(ninc)(:ii)//'"')
          inpunt = incunt(ninc)
          i=i+2
c                                           echo back value of keyword
      elseif (words(i).eq.'echo') then
          call echo(words(i+1), echstr, ies, lech)
          if (ies.gt.0) call messag('  echo  > '//echstr(:ies))
          i=i+2

c                                           format, default is ascii
      elseif (words(i).eq.'format') then
          call case(test, words(i+1))
          if (words(i+1)(:2).eq.'uw') then
              frmin  = 'uwxafs'
              frmout = 'uwxafs'
          endif
          i=i+2
      elseif (words(i).eq.'formin') then
          if (words(i+1)(:2).eq.'uw') then
              frmin  = 'uwxafs'
          endif
          i=i+2
      elseif (words(i).eq.'formout') then
          if (words(i+1)(:2).eq.'uw') then
              frmout = 'uwxafs'
          endif
          i=i+2
c                                           mark comment line in data
      elseif (words(i)(1:3).eq.'ign') then
          ignore = words(i+1)
          i=i+2

c     ********************************************************************
c     ************************************* error analysis keywords

c                                           input single sigma value, number
      elseif (words(i).eq.'sigdat') then
          call getrea(words(i), words(i+1), value)
          sigdat = dble(value)
          i=i+2
c                                           input sigma array, logical
      elseif (words(i).eq.'sigma') then
          lsig = .true.
          i=i+1
c                                           input minimum correlation
      elseif (words(i)(1:3).eq.'cor') then
          call getrea(words(i), words(i+1), cormin)
          i=i+2

c     ********************************************************************
c     ************************************* control of x-axis

c                                           name of xaxis in math expres.
      elseif (words(i).eq.'xaxis') then
          xaxis = words(i+1)
          call case(test, xaxis)
          i=i+2
c                                           output to fit or full x-range
      elseif (words(i)(1:3).eq.'wri') then
          call case(test,words(i+1))
          if (words(i+1)(1:3).eq.'fit') wrtout='fit'
          i=i+2
c                                           data boundries
c                                           observe nrangx!!
      elseif ((words(i).eq.'x1').or.(words(i).eq.'xmin')) then
          call getrea(words(i), words(i+1), value)
          range(1) = dble(value)
          i=i+2
      elseif (words(i).eq.'x2') then
          call getrea(words(i), words(i+1), value)
          range(2) = dble(value)
          i=i+2
      elseif (words(i).eq.'x3') then
          call getrea(words(i), words(i+1), value)
          range(3) = dble(value)
          i=i+2
      elseif (words(i).eq.'x4') then
          call getrea(words(i), words(i+1), value)
          range(4) = dble(value)
          i=i+2
      elseif (words(i).eq.'x5') then
          call getrea(words(i), words(i+1), value)
          range(5) = dble(value)
          i=i+2
      elseif (words(i).eq.'x6') then
          call getrea(words(i), words(i+1), value)
          range(6) = dble(value)
          i=i+2
      elseif (words(i).eq.'x7') then
          call getrea(words(i), words(i+1), value)
          range(7) = dble(value)
          i=i+2
      elseif (words(i).eq.'x8') then
          call getrea(words(i), words(i+1), value)
          range(8) = dble(value)
          i=i+2
      elseif (words(i).eq.'x9') then
          call getrea(words(i), words(i+1), value)
          range(9) = dble(value)
          i=i+2
      elseif ((words(i).eq.'x10').or.(words(i).eq.'xmax')) then
          call getrea(words(i), words(i+1), value)
          range(10) = dble(value)
          i=i+2
c                                           no. data points in nofit output
      elseif (words(i)(1:2).eq.'np') then
          call getint(words(i), words(i+1), nptout)
          i=i+2

c     ********************************************************************
c     ************************************* run control

c                                           end keyword, 
      elseif (words(i).eq.'end') then
          goto 193
c                                           write out indiv. files
      elseif (words(i)(1:3).eq.'all') then
          lall = .true.
          i=i+1
c                                           don't write out sum of functions
      elseif (words(i).eq.'nosum') then
          nosum = .true.
          i=i+1
c                                           write out nothing at all
      elseif (words(i).eq.'noout') then
          lall  = .false.
          nosum = .true.
          i=i+1
c                                           write out array of residuals
      elseif (words(i)(1:5).eq.'resid') then
          lresid = .true.
          i=i+1
c                                           don't perform error analysis
      elseif (words(i).eq.'noerr') then
          noerr = .true.
          i=i+1
c                                           dry-run, "spell check" input file
      elseif ((words(i).eq.'norun').or.(words(i)(1:3).eq.'dry'))
     $                then
          ldry = .true.
          i=i+1
c                                           just construct sum of functions
      elseif (words(i).eq.'nofit') then
          nofit = .true.
          i=i+1

c     ********************************************************************
c     ************************************* diagnostic functions

c                                           print diagnostic messages
      elseif (words(i).eq.'message') then
          call getint(words(i),words(i+1), imess)
          i=i+2

c     ********************************************************************
c     ************************************* numerical lineshapes

c                                           filename of lineshape
      elseif (words(i)(1:4).eq.'line') then
          if (i.ne.1) goto 666
          nline = nline + 1
          if (nline.gt.nlinex) goto 672
          lindat(nline) = words(2)
          do 140 iw=3,nwds
c           --- end of line comment
            if ( (words(iw)(1:1).eq.'!').or.(words(iw)(1:1).eq.'#').or.
     $                  (words(iw)(1:1).eq.'%') ) then
                goto 101
c           --- skip this column
            elseif (words(iw).eq.'&') then
                goto 140
c           --- read word as a label
            else
                label(iw-2,nline) = words(iw)
            endif
 140      continue 
          goto 101

c     ********************************************************************
c     ************************************* read guesses and sets

      elseif (words(i).eq.'guess') then
          if (i.ne.1) goto 666
          nguess = nguess+1
          if (nguess.gt.nguesx) goto 667
          gname(nguess) = words(i+1)
          call case(test, gname(nguess))
          call getrea('the guess value '//words(i+1),words(i+2), value)
          guess(nguess) = dble(value)
          goto 101
      elseif (words(i).eq.'set') then
          if (i.ne.1) goto 666
          nset = nset+1
          if (nset.gt.nsetx) goto 668
          sname(nset) = words(i+1)
          call case(test, sname(nset))
c         --- read rest of line into set
          i1   = istrln(words(i))
          temp = string(i1+1:)
          call triml(temp)
          i2   = istrln(words(i+1))
          string = temp(i2+1:)
          call triml(string)
c         --- remove end of line comment
          ib = index(string, '!')
          if (ib.eq.0) ib=75
          ip = index(string, '%')
          if (ip.eq.0) ip=75
          ih = index(string, '#')
          if (ih.eq.0) ih=75
          ii = min(ib, ip, ih)
          set(nset) = string(:ii-1)
c         --- remove leading equal or comma
          call triml(set(nset))
          if ((set(nset)(1:1).eq.',').or.(set(nset)(1:1).eq.'=')) then
              ii = istrln(set(nset))
              temp = set(nset)(2:ii)
              call triml(temp)
              set(nset) = temp
          endif
          goto 101

c     ********************************************************************
c     ************************************* read function and id
          
      elseif (words(i)(1:3).eq.'fun') then
          if (i.ne.1) goto 666
          call getint(words(i),words(i+1), nfun)
          if (nfun.gt.nfunx) goto 669
          nfunr = nfunr+1
c         --- read rest of line into functn
          i1   = istrln(words(i))
          temp = string(i1+1:)
          call triml(temp)
          i2   = istrln(words(i+1))
          string = temp(i2+1:)
          call triml(string)
c         --- remove end of line comment
          ib = index(string, '!')
          if (ib.eq.0) ib=75
          ip = index(string, '%')
          if (ip.eq.0) ip=75
          ih = index(string, '#')
          if (ih.eq.0) ih=75
          ii = min(ib, ip, ih)
          functn(nfun) = string(:ii-1)
c         --- remove leading equal or comma
          call triml(functn(nfun))
          if ((functn(nfun)(1:1).eq.',').or.
     $                (functn(nfun)(1:1).eq.'=')) then
              ii = istrln(functn(nfun))
              temp = functn(nfun)(2:ii)
              call triml(temp)
              functn(nfun) = temp
          endif
          goto 101

      elseif (words(i).eq.'id') then
          if (i.ne.1) goto 666
          call getint(words(i),words(i+1), nfun)
          if (nfun.gt.nfunx) goto 669
          i1   = istrln(words(i))
          temp = string(i1+1:)
          call triml(temp)
          i2   = istrln(words(i+1))
          string = temp(i2+1:)
          call triml(string)
          id(nfun) = string
          goto 101
          

      else
          ii = istrln(words(i))
          call messag('"'//words(i)(:ii)//'" is not a recognized '//
     $                'keyword.')
          i=i+1
      endif

c     ---if read entire line then read next line (101),
c        else read next word in line (130)
      if (i.gt.nwds) goto 101
      goto 130

c     --- done reading lines, if presently reading an include file, 
c         need to continue parsing, else go on
191   continue
      if (inpunt.ne.1) then
c         --- remove most recent include file from the list, deincrement ninc
          close(inpunt)
          incunt(ninc) = 0
          incfil(ninc) = ' '
          ninc = ninc-1
c         --- read from previous include file or go back to phit.inp
          if (ninc.gt.0) then
              inpunt = incunt(ninc)
          else
              inpunt = 1
          endif
          goto 101
      endif
c     --- jump here from end keyword
 193  continue 

      if (inpnul) then
          lend=.true.
          goto 500
      endif

c-----------------------------------------------------------------------
c  do a few things before leaving this routine
c-----------------------------------------------------------------------

c  echo back values for line, guess, set, function, id as requested
      if (lech(1)) then
          do 300 i=1,nline
            i1 = istrln(lname(i))
            i2 = istrln(lindat(i))
            write(echstr,400)i,lname(i)(:i1),lindat(i)(:i2)
 400        format('   echo  > line #',i2, ' "',a, '" read from "',a)
            ies = istrln(echstr)
            call messag(echstr(:ies))
 300      continue 
      endif
      if (lech(2)) then
          do 310 i=1,nguess
            i1 = istrln(gname(i))
            write(echstr,410)i, gname(i)(:i1), guess(i)
 410        format('  echo  > guess value #',i2,': ',a,' = ',g12.5)
            ies = istrln(echstr)
            call messag(echstr(:ies))
 310      continue 
      endif
      if (lech(3)) then
          do 320 i=1,nset
            i1 = istrln(sname(i))
            i2 = istrln(set(i))
            write(echstr,420)i, sname(i)(:i1), set(i)(:i2)
 420        format('  echo  > set value #',i2,': ',a,' = ',a)
            ies = istrln(echstr)
            call messag(echstr(:ies))
 320      continue 
      endif
      if (lech(4)) then
          do 330 i=1,nfunx
            if (functn(i).ne.nullwd) then
                ii = istrln(functn(i))
                write(echstr,430)i,functn(i)(:ii)
 430            format('  echo  > function #',i2,': ',a)
                ies = istrln(echstr)
                call messag(echstr(:ies))
            endif
 330      continue 
      endif
      if (lech(5)) then
          do 340 i=1,nfunx
            if (functn(i).ne.nullwd) then
                ii = istrln(id(i))
                write(echstr,440)i,id(i)(:ii)
 440            format('  echo  > id line #',i2,': ',a)
                ies = istrln(echstr)
                call messag(echstr(:ies))
            endif
 340      continue 
      endif

      call messag(' ')
c  set total number of variables
      nvar = nset+nguess

      if (nfun.le.0) then
          call messag('     * * ERROR! Your input file contains '//
     $                'no functions.')
          call messag('Phit is stopping.')
          stop
      endif

c  I/O format
      call triml(frmin)
      call triml(frmout)
      call case(test, frmin)
      call case(test, frmout)

c  n/skey required for uwxafs input
      if (frmin.eq.'uwxafs') then
          if ((nkey.le.0).and.(skey.eq.' ')) then
              call messag('     * * ERROR!  Either the skey or '//
     $                    'nkey must be specified for UWXAFS input.')
              call messag('Phit is stopping.')
              stop
          endif
      endif

c  make sure cormin is between 0 and 1
      if (cormin.gt.one)  then
          call messag('     * * NOTICE!  Cormin larger '//
     $                'than 1, reset to 1.')
          cormin = one
      endif
      if (cormin.lt.zero) then 
          call messag('     * * NOTICE!  Cormin smaller '//
     $                'than 0, reset to 0.')
          cormin = zero
      endif

c  sanity check sigdat, it must be positive
      sigdat = abs(sigdat)

c  nofitting option:
c     --- nofit keyword found...
      if (nofit) inofit=0
c     --- or no guess values found... 
      if (nguess.eq.0) then
          inofit = 1
          nofit  = .true.
      endif
c     --- or no data specified
      if (data.eq.nullwd) then
          inofit = 2
          nofit  = .true.
      endif
      if ((nofit).and.(nptout.le.0)) then
          nptout = 100
          call messag('     * * NOTICE!  Npoints set to 100.')
      endif

c  redundancy check xaxis, sets, and guesses
      lstop = .false.
      do 220 i=1,nset
        if (xaxis.eq.sname(i)) then
            ii=istrln(xaxis)
            call messag('     * * ERROR! "'//xaxis(:ii)//'" is the '//
     $                  'xaxis name and a set value.')
            lstop = .true.
        endif
 220  continue 
      do 230 i=1,nguess
        if (xaxis.eq.gname(i)) then
            ii=istrln(xaxis)
            call messag('     * * ERROR! "'//xaxis(:ii)//'" is the '//
     $                  'xaxis name and a guess value.')
            lstop = .true.
        endif
 230  continue 
      do 250 i=1,nset
        do 240 j=1,nguess
          if (sname(i).eq.gname(j)) then
              ii=istrln(sname(i))
              call messag('     * * ERROR! "'//sname(i)(:ii)//
     $                    '" is both a set and a guess value.')
              lstop = .true.
          endif
 240    continue 
        do 245 j=i+1,nset
          if (sname(i).eq.sname(j)) then
              ii=istrln(sname(i))
              call messag('     * * ERROR! "'//sname(i)(:ii)//
     $                    '" is declared twice as a set value.')
              lstop = .true.
          endif
 245    continue 
 250  continue 
      do 270 i=1,nguess
        do 260 j=i+1,nguess
          if (gname(i).eq.gname(j)) then
              ii=istrln(gname(i))
              call messag('     * * ERROR! "'//gname(i)(:ii)//
     $                    '" is declared twice as a guess value.')
              lstop = .true.
          endif
 260    continue 
 270  continue 

      if (lstop) then
          call messag('Phit cannot continue with ambiguous '//
     $                'variable names.')
          call messag('Phit is stopping.')
          stop
      endif

c  obvious filename conflicts, do not allow overwriting of data, input,
c  or log file here or in phout
      if (logout.eq.outfil) then
          call messag('     * * ERROR! The log file and the '//
     $                'output file cannot have the same name.')
          lstop = .true.
      endif
      if (logout.eq.fname) then
          call messag('     * * ERROR! The log file and the '//
     $                'input file cannot have the same name.')
          lstop = .true.
      endif
      if (logout.eq.data) then
          call messag('     * * ERROR! The log file and the '//
     $                'data file cannot have the same name.')
          lstop = .true.
      endif
      if (outfil.eq.fname) then
          call messag('     * * ERROR! The output file and the '//
     $                'input file cannot have the same name.')
          lstop = .true.
      endif
      if (outfil.eq.data) then
          call messag('     * * ERROR! The output file and the '//
     $                'data file cannot have the same name.')
          lstop = .true.
      endif
      if (lstop) then
          call messag('         Phit is stopping')
          stop
      endif
     
 500  continue
      return

c  error reading function, id, guess, or set
 666  continue
      write(cnum,4010)iline
      call messag(' ')
      call messag('* * * ERROR!')
      call messag('Error at line '//cnum//' of input file.')
      call messag('The keywords function, id, guess, and set must '//
     $            'be the first words')
      call messag('on the line and must be the only keyword on '//
     $            'that line.')
      call messag('To avoid trouble, Phit is stopping now.')
      call messag('Please correct your input file and run Phit again.')
      call messag(' ')
      stop

 667  continue 
      write(cnum,4010)ngeusx
      call triml(cnum)
      ii = istrln(cnum)
      call messag(' ')
      call messag('* * * ERROR!')
      call messag('You have exceeded the hard wired limit of '//
     $            cnum(:ii)//' guess values.')
      call messag('Phit is stopping.')
      call messag(' ')
      stop

 668  continue 
      write(cnum,4010)nsetx
      call triml(cnum)
      ii = istrln(cnum)
      call messag(' ')
      call messag('* * * ERROR!')
      call messag('You have exceeded the hard wired limit of '//
     $            cnum(:ii)//' set math expressions.')
      call messag('Phit is stopping.')
      call messag(' ')
      stop

 669  continue 
      write(cnum,4010)nfunx
      call triml(cnum)
      ii = istrln(cnum)
      call messag(' ')
      call messag('* * * ERROR!')
      call messag('There is a hard wired limit of '//
     $            cnum(:ii)//' functions and '//cnum(:ii)//' ids.')
      call messag('The function or id index cannot exceed '//
     $            cnum(:ii)//'.')
      call messag('Phit is stopping.')
      call messag(' ')
      stop

 670  continue 
      write(cnum,4010)nincx
      call triml(cnum)
      ii = istrln(cnum)
      call messag(' ')
      call messag('* * * ERROR!')
      call messag('You have exceeded the hard wired limit of '
     $            //cnum(:ii)//' include files.')
      call messag('Phit is stopping.')
      call messag(' ')
      stop

 671  continue 
      ii = istrln(words(i+1))
      call messag(' ')
      call messag('* * * ERROR!')
      call messag('You have called include file "'//
     $            words(i+1)(:ii)//'" recursively.')
      call messag('This is not allowed in Phit.')
      call messag('Phit is stopping.')
      call messag(' ')
      stop

 672  continue 
      write(cnum,4010)nlinex
      call triml(cnum)
      ii = istrln(cnum)
      call messag(' ')
      call messag('* * * ERROR!')
      call messag('There is a hard wired limit of '//
     $            cnum(:ii)//' lineshapes.')
      call messag('Phit is stopping.')
      call messag(' ')
      stop

c end subroutine phinpt
      end


      subroutine dump

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      print*,'ntit, nguess, nset'
      print*,ntit, nguess, nset
      print*,' '

      do 10 i=1,ntit
        ii=istrln(title(i))
        print*,'title(',i,') > ',title(i)(:ii)
 10   continue 
      
      print*,' '

      ii=istrln(data)
      print*,'data file:      ',data(:ii)
      ii=istrln(outfil)
      print*,'output file:     ',outfil(:ii)
      print*,'x-axis name:     ',xaxis
      print*,'input format:    ',frmin
      print*,'output format:   ',frmout
      print*,'logical all:     ',lall
      print*,'logical dry:     ',ldry
      print*,'data range:      ',range
      print*,'sigdat:          ',sigdat
      print*,'min. correlation:',cormin

      print*,' '

      do 30 i=1,nguess
        print*,'guess: ', gname(i), guess(i)
 30   continue 
      do 40 i=1,nset
        print*,'set:   ', sname(i), set(i)
 40   continue 

      print*,' '
      
      do 50 i=1,nline
        ii = istrln(lindat(i))
        print*,'line:  ', lname(i), lindat(i)(:ii)
 50   continue 

      print*,' '

      do 60 i=1,nfunx
        if (functn(i).ne.nullwd) then
            ii = istrln(functn(i))
            print*,'function #',i,'  ',functn(i)(:ii)
        endif
        if (id(i).ne.nullwd) then
            ii = istrln(id(i))
            print*,'id #',i,'        ',id(i)(:ii)
        endif
 60   continue 

      return 
c  end subroutine dump
      end
      subroutine phenco

c  encode the math expressions for the set values and the functions.
c  perform various sanity checks on the parameter names

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      logical      lstop
      character*3  cnum

 4000 format(i3)

      lstop = .false.

c-------------------------------------------------------------------------
c  encode up all the functions, vnames will contain all variable names 
c  used in the functions
c  icode contains the encoded math expressions for the functions
      if (imess.eq.50) print*,'        (In subroutine phenco)'
      do 100 i=1,nfunx
        if (functn(i).ne.nullwd) then
            if (imess.eq.50) print*,'        encoding function # ',i
            call encod(functn(i), vnames, nvarx, consts, nconx,
     $                  icode(1,i), ncodex, ierr)
            if (ierr.ne.0) then
                write(cnum,4000)i
                call messag('   * * Error in function #'//cnum)
                call messag('Phit is stopping')
                stop
            endif
        endif
 100  continue 
      
c-------------------------------------------------------------------------
c  encode up all the set values, use the same vnames and consts arrays
c  icdset contains the encoded math expressions for the set values
      do 110 i=1,nset
        if (imess.eq.50) print*,'        encoding set value # ',i
        call encod(set(i), vnames, nvarx, consts, nconx,
     $              icdset(1,i), ncodex, ierr)
        if (ierr.ne.0) then
            ii = istrln(set(i))
            call messag('   * * Error in set value "'//set(i)(:ii)//'"')
            call messag('Phit is stopping')
            stop
        endif
 110  continue 

c-------------------------------------------------------------------------
c  check to see that all variables in vnames are in s/gnames 
c  search vnames until a blank is found
      nvf = 0
      do 200 i=1,nvarx
        if (vnames(i).eq.' ') goto 210
        nvf = nvf+1

c       search guess value names
        do 170 jg=1,nguess
          if (gname(jg).eq.vnames(i)) goto 200
 170    continue 
        
c       search set value names
        do 180 js=1,nset
          if (sname(js).eq.vnames(i)) goto 200
 180    continue 

c       search lineshape names
        do 190 jl=1,nline
          if (lname(jl).eq.vnames(i)) goto 200
 190    continue 

c       check xaxis name
        if (xaxis.eq.vnames(i)) goto 200

c       get here only if vnames(i) is not in sname, gname or xaxis
        ii = istrln(vnames(i))
        call messag('     * * ERROR! "'//vnames(i)(:ii)//'" is not '//
     $              'a defined variable.')
        lstop = .true.

 200  continue 
 210  continue 
      if (imess.eq.50)
     $     print*,'      number of encoded variables found = ',nvf

c-------------------------------------------------------------------------
c  check that all set, guess and xaxis names are in vnames
c  ipoint is an array of pointers mapping the values of set, guess, and 
c  the present value of xaxis to the appropriate places in the array values.
c  note that vname(i) maps to values(1,i).
c  ipoint will be used in evaluating the function to minimize.
c  ipoint:
c     1 -> nset:                 pointers to set values in array values
c     nset + 1 -> nset + nguess: pointers to guess values in array values
c     nset + nguess + 1:         pointer to xaxis value in array values
c     nset + nguess + 1 -> ... + nlines:  pointer to lineshape value in array

c     check that each set value is in vnames
      do 330 i=1,nset
        do 320 j=1,nvf
          if (sname(i).eq.vnames(j)) then
              ipoint(i) = j
              goto 330
          endif
 320    continue 
        ii=istrln(sname(i))
        call messag('     * * WARNING!  Set value "'//sname(i)(:ii)//
     $              '" was not found in any math expression.')
 330  continue 

c     check that each guess value is in vnames
      do 360 i=1,nguess
        do 350 j=1,nvf
          if (gname(i).eq.vnames(j)) then
              ipoint(i+nset) = j
              goto 360
          endif
 350    continue 
        ii=istrln(gname(i))
        call messag('     * * ERROR!  Free variable "'//gname(i)(:ii)//
     $              '" was not found in any math expression.')
        lstop = .true.
 360  continue 

c     check that xaxis is in vnames
      do 370 j=1,nvf
        if (xaxis.eq.vnames(j)) then
            ipoint(nvar+1) = j
            goto 375
        endif
 370  continue 
      ii = istrln(xaxis)
      call messag('     * * WARNING!  No function depends on "'//
     $            xaxis(:ii)//'".')
 375  continue 
      
c     check that each lineshape is in vnames
      do 390 i=1,nline
        do 380 j=1,nvf
          if (lname(i).eq.vnames(j)) then
              ipoint(i+nvar+1) = j
              goto 390
          endif
 380    continue 
        ii=istrln(lname(i))
        call messag('     * * WARNING!  Lineshape "'//lname(i)(:ii)//
     $              '" was not found in any math expression.')
 390  continue 

c--------------------------------------------------------------------------
c  check to see that no set math expressions are recursive
      do 450 i=1,nset
        do 410 j=1,ncodex
          if (icdset(j,i).eq.0) goto 450
          if (icdset(j,i).eq.ipoint(i)) then
              ii = istrln(sname(i))
              call messag('     * * ERROR!  Set value "'//
     $                    sname(i)(:ii)//'" depends on itself.')
              call messag('                 Recursive definitions '//
     $                    'are not allowed.')
              lstop = .true.
              goto 450
          endif
 410    continue 
 450  continue 

c--------------------------------------------------------------------------
c  variable names have been checked both ways, the pointer array is loaded,
c  we are ready to go IF no fatal error have been found
      if (lstop) then
          call messag(' ')
          call messag('At least one fatal encoding error was found '//
     $                'in your functions.')
          call messag('Phit is stopping.')
          stop
      endif

c--------------------------------------------------------------------------
c  check set expressions for dependence on x
c  this is a bit tricky,  the second if in the 510 loop searches each
c  set expression for explicit dependence on x
c  then we need to search for implicit x dependence.  if a = b + c and
c  c = 3*x, then c will be flagged for explicit dependence in the first 
c  part of the 510 loop.  On the next 
c  time through the 530 loop, the dependence of a on c will be flagged in 
c  the 500 loop.  Then both a and c will be flagged as having x dependence.
c  This whole process has to be repeated nset times to assure proper flagging
c  for the most perverse possible case.
      do 530 loop=1,nset
        do 520 i=1,nset
          do 510 j=1,ncodex
            if (icdset(j,i).eq.0) goto 520
            if (loop.eq.1) then
                do 500 k=nvar+1, nvar+1+nline
                  if (icdset(j,i).eq.ipoint(k)) then
                      ldepx(i) = .true.
                      goto 520
                  endif
 500            continue 
            else
                do 505 k=1,nset
                  if (icdset(j,i).eq.ipoint(k)) then
                      if (ldepx(k)) ldepx(i) = .true.
                  endif
 505            continue 
            endif
 510      continue 
 520    continue 
 530  continue 

      if (imess.eq.55) then
          print*,' '
          print*,'dependence on xaxis'
          print*,'set value --- ldepx(i)'
          do 540 i=1,nset
            ii = istrln(sname(i))
            print*,sname(i)(:ii),ldepx(i)
 540      continue 
          print*,' '
      endif

c  now check set values for dependence on the guess values
      do 640 loop=1,nset
        do 630 i=1,nset
          do 620 j=1,ncodex
            if (icdset(j,i).eq.0) goto 630
            if (loop.eq.1) then
                do 600 k=nset+1,nset+nguess
                  if (icdset(j,i).eq.ipoint(k)) then
                      ldepg(i) = .true.
                      goto 630
                  endif
 600            continue 
            else
                do 610 k=1,nset
                  if (icdset(j,i).eq.ipoint(k)) then
                      if (ldepg(k)) ldepg(i) = .true.
                  endif
 610            continue 
            endif
 620      continue 
 630    continue 
 640  continue 

      if (imess.eq.55) then
          print*,' '
          print*,'dependence on guess values'
          print*,'set value --- ldepg(i)'
          do 650 i=1,nset
            ii = istrln(sname(i))
            print*,sname(i)(:ii),ldepg(i)
 650      continue 
          print*,' '
      endif

c-------------------------------------------------------------------
c  now i want to make a pointer array containing information on the 
c  most efficient way to evaluate the sets in phunct.  at this point 
c  the sets are ordered for one pass evaluation and I have information
c  on which sets depend on the x-axis and which depend on guess values.
c  i only want to evaluate the sets that are independent of x-axis and
c  guess values once at the beginning of the problem.  i only want to 
c  evaluate those that depend on guesses and not x-axis at the first 
c  x-axis point on each evaluation of the fitting function.  the first
c  nindep values of istord point to the set expression indeces of those
c  set values that only need to be evaluated once.  the next ndepg point 
c  to those that need to be evaluated once per evaluation.  the last 
c  elements point to those that depend on x.  by evaluating in the order
c  indicated in istord, one pass evaluation is still assured and I will
c  call decod a minimum number of times.  now wasn't that crystal clear?
      ip = 0
      do 700 i=1,nset
        if ((.not.ldepx(i)).and.(.not.ldepg(i))) then
            ip = ip+1
            istord(ip) = i
        endif
 700  continue 
      nindep = ip
      do 710 i=1,nset
        if (ldepg(i).and.(.not.ldepx(i))) then
            ip = ip+1
            istord(ip) = i
        endif
 710  continue 
      ndepg = ip
      do 720 i=1,nset
        if (ldepx(i)) then
            ip = ip+1
            istord(ip) = i
        endif
 720  continue 
      if (ip.ne.nset) then
          call messag('Incorrect counting in Phenco for istord.')
          call messag('How can this happen????')
          call messag('Contact Bruce.')
          stop
      elseif (imess.eq.55) then
          print*,'order of evaluation:'
          print*,'nindep, ndepg: ',nindep, ndepg
          print*,'i, sname, set, ldepx, ldepg, istord'
          do 730 i=1,nset
            ii = istrln(sname(i))
            ij = istrln(set(i))
            print*,i,' ',sname(i)(:ii),' ',set(i)(:ij),
     $                  ldepx(i),ldepg(i),istord(i)
 730      continue 
      endif

c-----------------------------------------------------------------------------
c  diagnostic for viewing encoded names and set/guess/xaxis names and pointers
      if (imess.eq.50) then
          print*,'*** vnames ***'
          do 900 i=1,nvf
            print*,i,'  ',vnames(i)
 900      continue 
          print*,'*** sets, guesses, and pointers ***'
          do 910 i=1,nset
            print*,i,'  ',sname(i),ipoint(i)
 910      continue 
          do 920 i=1,nguess
            j=i+nset
            print*,j,'  ',gname(i),ipoint(j)
 920      continue 
          print*,'   ',xaxis,ipoint(nvar+1)
      endif
c-----------------------------------------------------------------------------

      return 
c  end subroutine phenco
      end
      subroutine phdata

c  read an input data file as (X,Y) data.  if a fitting range is specified
c  copy the write the data into work arrays only over the specified range(s)

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      parameter (small=1.d-10)
      logical     there, lstop
      dimension   xx(nrangx), nx(nrangx)
      character*3 cnum

 400  format(i3)

      lstop = .false.

      inquire(file=data, exist=there)
      if (.not.there) then
          ii = istrln(data)
          call messag('Input file "'//data(:ii)//'" not found.')
          call messag('Phit is stopping.')
          stop
      endif

c--------------------------------------------------------------------
c  read data
      ndata = ndatx
      ndoc  = ndocx
      ii = istrln(data)
      if (frmin(1:2).eq.'uw') then
          write(cnum,400)nkey
          call messag('Phit --- Reading data file: "'//data(:ii)//
     $                '", nkey '//cnum)
      else
          call messag('Phit --- Reading data file: "'//data(:ii)//
     $                '"')
      endif

      if (imess.eq.60) then
          print*,'in subroutine phdata: '
          print*,' '
          print*,'ftype >',ftype,'< frmin >',frmin,'<'
          print*,'in >',data,'<'
          print*,'vaxflg ',vaxflg,' skey >',skey,'< nkey ',nkey
      endif

      call inpdat(ftype, frmin, data, vaxflg, skey, nkey, 
     $       ndoc, doc, ndata, xdata, ydata, zdata, yampl, yphas)

      if (imess.eq.60) then
          print*,'ndata=',ndata
          print*,'first three data points:'
          print*,xdata(1),ydata(1),zdata(1)
          print*,xdata(2),ydata(2),zdata(2)
          print*,xdata(3),ydata(3),zdata(3)
      endif 

c--------------------------------------------------------------------
c  remove data outside of data range(s), write fitting range(s) to 
c  xfit and yfit
      ixr=0
      do 10 i=1,nrangx
        xx(i) = 0.d0
        nx(i) = 0
        if (range(i).ge.valnul) then
            ixr     = ixr+1
            xx(ixr) = range(i)
        endif
 10   continue 

      if (ixr.eq.0) then
          ixr = 2
          nx(1) = 1
          xx(1) = dble(xdata(1))
          nx(2) = ndata
          xx(2) = dble(xdata(ndata))
          goto 70
      endif

c     sort them
      call piksrt(ixr,xx)

      do 20 i=1,ixr
        nx(i) = nofx(sngl(xx(i)), xdata, ndata)
 20   continue 
      if (mod(ixr,2).ne.0) then
          call messag('         Odd number of fitting range points.'//
     $                '  Adding last data point.')
          ixr     = ixr+1
          nx(ixr) = ndata
          xx(ixr) = dble(xdata(ndata))
      endif

 70   continue 
      nfit = 0
      do 100 i=1,ixr/2
        do 80 j=nx(2*i-1), nx(2*i)
          nfit       = nfit+1
          xfit(nfit) = dble(xdata(j))
          yfit(nfit) = dble(ydata(j))
          sigma(nfit)= dble(zdata(j))
          if ((lsig).and.(sigma(nfit).lt.(small*ydata(j)))) then
              write(cnum,400)j
              call messag('         The error bar at data point '
     $                    //cnum//' is small')
              call messag('         enough to give computational '//
     $                    'error.')
              call messag('         That error bar has been reset '//
     $                    'to 10^-10 * the data at that point.')
              sigma(nfit) = small * ydata(j)
c              lstop = .true.
          endif
 80     continue 
 100  continue 

      xmin = big
      xmax = zero
      do 110 i=1,ixr
        range(i) = xx(i)
        xmin = min(xmin,range(i))
        xmax = max(xmax,range(i))
 110  continue 
      do 120 i=ixr+1,nrangx
        range(i) = valnul-1.d0
 120  continue 

      if (imess.eq.60) then
          print*,'number of input data point =    ',ndata
          print*,'number of data points in fit =  ',nfit
          print*,'number of fit range boundries = ',ixr
          print*,'fit range values:'
          print*,xx
      endif

      if (lstop) then
          call messag(' ')
          call messag('         ERROR!  Zero values for input '//
     $                'error bars were found.')
          call messag('                 Non-linear least square '//
     $                'fitting cannot proceed.')
          call messag('Phit is stopping.')
          stop
      endif

      return
c  end subroutine phdata
      end

c========================================================================

      subroutine piksrt(n,arr)
c     an N^2 sorting routine appropriate for sorting the fitting range 
c     values  --  from Numerical Recipes ch.8.1
c     replace this if nrangx is ever set greater than 20!
      implicit double precision (a-h,o-z)
      integer n
c      real arr(n)
      double precision arr(n)
      do 12 j=2,n
        a = arr(j)
        do 11 i=j-1,1,-1
          if (arr(i).le.a) goto 10
          arr(i+1) = arr(i)
 11     continue 
        i=0
 10     continue 
        arr(i+1) = a
 12   continue 
      return 
c  end subroutine piksrt
      end
      subroutine phunct(kfit, kvar, xguess, fvec, iflag)

c-----------------------------------------------------------------------
c  evaluation of sum of functions for least-squares minimization
c-----------------------------------------------------------------------
c  Be very careful about single/double precision!!!

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      integer kfit, kvar, iflag
      real    xguess(kvar), fvec(kfit)
      double precision sumfun

c  assign guess values to their places in values
c  at each xaxis point assign xaxis value to values
c  at each xaxis point re-decode set expressions which depend on xaxis.

c  iter counts the number of time this subroutine has been called
c  used to avoid redecoding set expressions that do not depend on the 
c  guess values
      iter = iter+1

      do 50 i=1,kvar
        j = nset+i
        if (ipoint(j).ne.0) then
            values(1,ipoint(j)) = dble(xguess(i))
        else
c           unused guess values should have already tilted the code
c           back in phenco.  this is a paranoia condition ;->
            call messag('How did you get here??? (phunc)')
            call messag('An unassigned guess value was found.')
            call messag('If you are reading this, contact Bruce.')
            stop
        endif
 50   continue 

c     loop of xaxis points
      do 300 ix=1,kfit
c       assign present xaxis point to appropriate place in values
        if (ipoint(nvar+1).ne.0) then
            values(1,ipoint(nvar+1)) = xfit(ix)
        endif
        do 70 jj=1,nline
          if (ipoint(nvar+1+jj).ne.0) then
              values(1,ipoint(nvar+1+jj)) = dble(yline(ix,jj))
          endif
 70     continue 

c       --- first time through, evaluate all of the sets, after that
c           evaluate all sets that depend on x or a guess value at first 
c           xaxis point, and only those that depend on x on subsequent
c           xaxis points
c           istart is starting element of istord pointer array 
        istart = ndepg+1
        if (ix.eq.1) then
            istart = nindep+1
        endif
        if (iter.eq.1) then
            istart = 1
        endif

        do 80 i=istart,nset
c         --- see comments about istord in phenco for why this is 
c             an efficient way to evaluate the sets
          iii = istord(i)
          if (ipoint(iii).ne.0) then
              np = nptx
              call decod(icdset(1,iii), ncodex, consts, values, 
     $                    nvpts, nptx, nvarx, defval, np, outval)
              setval(iii) = outval(1)
              values(1,ipoint(iii)) = setval(iii)
          endif
 80     continue 

c       --- decode and sum functions using updated sets and xaxis
        sumfun = zero
        do 250 ifun=1,nfunx
          if (functn(ifun).eq.nullwd) goto 250

          np = nptx
          call decod(icode(1,ifun), ncodex, consts, values, nvpts,
     $                nptx, nvarx, defval, np, outval)

          if (imess.eq.71) then
              print*,'decoded value of function # ',ifun,' = ',
     $                    outval(1)
          endif
          
          sumfun = sumfun + outval(1)
 250    continue 

c       --- chi squared *not* reduced chi-square
        diff(ix) = real( yfit(ix) - sumfun ) 
        if (lsig) then
            anorm = sigma(ix)
        elseif (sigdat.gt.epsi) then
            anorm = sigdat
        else
            anorm = sigres
        endif
        fvec(ix) = diff(ix) / real(anorm)
 300  continue 

      return 
c  end subroutine phunct
      end
      subroutine phfit

c-----------------------------------------------------------------------
c  call lmdif1 to perform minimization on function calculated in phunct
c-----------------------------------------------------------------------

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

      parameter (lwa = ndatx*nvarx + 5*nvarx + ndatx)
      character*5 cnum

      real    toler, wa(lwa), fvec(ndatx)
      integer iwa(nvarx)
      real    xguess(nguesx)
      external phunct

 400  format(i5)

c  might want to think harder about toler
      toler = 1e-5
c  setup for lmdif
      kvar  = nguess
      kfit  = nfit
      do 10 i=1,nguess
        xguess(i) = real(guess(i))
 10   continue 

      call lmdif1(phunct, kfit, kvar, xguess, fvec, toler,
     $            lminfo, iwa, wa, lwa)

      if (lminfo.eq.4) then
          call messag('     * * * WARNING! * * *')
          call messag('     The function is orthogonal to the '//
     $                'jacobian matrix within machine precision.')
          call messag('     This probably means that one of your '//
     $                'guess values has no effect on the fit.')
          call messag(' ')
      elseif (lminfo.eq.5) then
          call messag('     * * * WARNING! * * *')
          write(cnum, 400)200*(kvar+1)
          call messag('     You have exceeded '//cnum//
     $                ' function calls.')
          call messag(' ')
      endif

c  get sigma as the standard deviation of the residuals
c  (only valid if sigdat not provided AND sigma array not provided), 
      ave = zero
      do 120 i=1,kfit
        ave = ave + dble(fvec(i))
 120  continue 
      ave = ave / kfit

      sigres = zero
      do 125 i=1,kfit
        sigres = sigres + dble(fvec(i))**2
 125  continue 
      sigres = sqrt( sigres/kfit - ave**2 )

c     --- another pass through phunct to get fvec scaled for certain
      call phunct(kfit, kvar, xguess, fvec, iflag)
      do 130 i=1,kfit
        chisqr  = chisqr + dble(fvec(i))**2
        fvect(i) = fvec(i)
 130  continue 

      if (imess.eq.40) then
          print*,'(in phfit)'
          do 150 i=1,nguess
            print*,gname(i),'=',xguess(i)
 150      continue 
          print*,'chisqr=',chisqr
          print*,'sigres=',sigres
      endif

c  save best fit values 
      do 200 i=1,nguess
        final(i) = dble(xguess(i))
 200  continue 

      return 
c  end subroutine phfit
      end
      subroutine phout

c  write out the sum of functions and, optionally, the individual functions
c  write to the fitted or the full grid as specified by keyword "write"

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

      character*72 root
      character*10 sk
      character*3  ext, cnum, badnam*6
      real         funout(ndatx)
      logical      there

 4000 format(i3.3)
 4010 format(i3)

c  skip writing sum of functions if nosum = T
      if (nosum) then
          call messag('         Not writing sum of functions,'//
     $                ' as requested.')
          goto 30
      endif

c  determine output file name
c  strpth returns the input file name with the path and extension removed
      if (outfil.eq.nullwd) then
          call strpth(data, root, vaxflg)
          ii = istrln(root)
          if (.not.nofit) then
              outfil = root(:ii)//'.fit'
          else
              outfil = root(:ii)//'.nofit'
          endif
      endif

c     --- avoid filename collisions
      if ((outfil.eq.fname).or.(outfil.eq.logout).or.
     $            (outfil.eq.data)) then
          badnam = 'log'
          ii = 3
          if (outfil.eq.data) then
              badnam = 'data'
              ii = 4
          elseif (outfil.eq.fname) then
              badnam = 'input'
              ii = 6
          endif
          call messag('     * * Warning! The sum of functions file '//
     $                'has the same name as the')
          call messag('         '//badnam(:ii)//' file.')
          call strpth(outfil, root, vaxflg)
          ii = istrln(root)
          root = root(:ii)//'_'
          if (.not.nofit) then
              outfil = root(:ii+1)//'.fit'
          else
              outfil = root(:ii+1)//'.nofit'
          endif
          ii = istrln(outfil)
          call messag('         Outfile renamed to '//
     $                outfil(:ii))
      endif

c--------------------------------------------------------------------
c  prep outdat then write the full sum of functions
      ftype  = 'xmu'
      if (frmout.eq.'ascii') ftype = '2col'
      sk     = ' '
      nk     = 0
      nd     = ndata
      if (wrtout.eq.'fit') then
          nd = nfit
      elseif (nptout.ne.-1) then
          nd = nptout
      endif
      iexist = 0
c     concatinate titles and docs
      mdoc = min(ndocx-ntit, ndoc)
      do 10 i=mdoc,1,-1
        doc(i+ntit) = doc(i)
 10   continue 
      do 20 i=1,ntit
        doc(i) = title(i)
 20   continue 
      ndoc = mdoc + ntit 
      lstdoc = min(ndoc, ndocx)
      doc(lstdoc) = 'This file generated by Phit, version '//versn

      ii = istrln(outfil)
      call messag('              Writing sum of functions to "'//
     $            outfil(:ii)//'"')
      there = .false.
      inquire(file=outfil, exist=there)
      if ((there).and.(.not.vaxflg))
     $            call messag('              Overwriting "'//
     $                        outfil(:ii)//'"')
      call outdat(ftype, frmout, outfil, vaxflg, sk, nk,
     $    ndoc, doc, nd, xout, yout, dummy, dummy, dummy, iexist)

      if ((lresid).and.(.not.nofit).and.(.not.nosum)) then
          sk     = ' '
          nk     = 0
          nd     = ndata
          if (wrtout.eq.'fit') nd = nfit
          iexist = 0

          if (frmout.eq.'ascii') then
c             --- individual function file called outfil.res
              call strpex(outfil, root)
              ii = istrln(root)
              outfil = root(:ii)//'.res'
c             --- avoid filename collisions
              if ((outfil.eq.fname).or.(outfil.eq.logout).or.
     $                    (outfil.eq.data)) then
                  badnam = 'log'
                  ii = 3
                  if (outfil.eq.data) then
                      badnam = 'data'
                      ii = 4
                  elseif (outfil.eq.fname) then
                      badnam = 'input'
                      ii = 6
                  endif
                  call messag('     * * Warning! The residual file '//
     $                        'has the same name as the '//badnam(:ii)//
     $                        ' file')
                  call strpth(outfil, root, vaxflg)
                  ii = istrln(root)
                  root = root(:ii)//'_'
                  outfil = root(:ii+1)//'.fit'
                  ii = istrln(outfil)
                  call messag('         Outfile renamed to '//
     $                        outfil(:ii))
              endif
          endif

          do 25 i=1,nd
            if (nd.eq.ndata) then
                diff(i) = ydata(i) - yout(i)
            else
                diff(i) = real(yfit(i)) - yout(i)
            endif
 25       continue 

          ii = istrln(outfil)
          call messag('              Writing residuals to "'//
     $                outfil(:ii)//'"')
          there = .false.
          inquire(file=outfil, exist=there)
          if ((there).and.(.not.vaxflg))
     $                call messag('              Overwriting "'//
     $                outfil(:ii)//'"')
          call outdat(ftype, frmout, outfil, vaxflg, sk, nk,
     $                ndoc, doc, nd, xout, diff, dummy, dummy,
     $                dummy, iexist)
      endif
      
 30   continue 
c  write out individual functions if requested
      if (.not.lall) goto 999

c     prep document array for functions output
      ndoc = min(ndocx-2, ndoc)
      do 100 i=ndoc,1,-1
        doc(i+2) = doc(i)
 100  continue 
      ndoc = ndoc+2
      if (ndoc.eq.ndocx) then
          doc(ndoc) = 'This file generated by Phit, version '//versn
      endif

      do 200 ifun=1,nfunx
        if (functn(ifun).eq.nullwd) goto 200

c       loop through x-axis values, construct function
        nn = nfit
        if (wrtout.eq.'full') nn = ndata
        do 150 ix=1,nn
          if (wrtout.eq.'fit') then
              values(1,ipoint(nvar+1)) = xfit(ix)
              xout(ix) = real(xfit(ix))
          else
              values(1,ipoint(nvar+1)) = dble(xdata(ix))
              xout(ix) = xdata(ix)
          endif

c         --- decode set values for present xaxis value, 
          do 130 i=1,nset
            if ( (.not.ldepx(i)).and.(ix.gt.1) ) then
                goto 130
            else
                if (ipoint(i).ne.0) then
                    np = nptx
                    call decod(icdset(1,i), ncodex, consts, values, 
     $                          nvpts, nptx, nvarx, defval, np, outval)
                    setval(i) = outval(1)
                    values(1,ipoint(i)) = setval(i)
                endif
            endif
 130      continue 

c         --- decode function with updated decoding of set
          np = nptx
          call decod(icode(1,ifun), ncodex, consts, values, nvpts,
     $                nptx, nvarx, defval, np, outval)
          funout(ix) = real(outval(1))
 150    continue 

c       prep outdat and write individual function
        do 110 i=1,ndatx
          dummy(i) = 0.e0
 110    continue 
        ftype = 'xmu'
        sk = ' '
        nk = 0
        if (frmout.eq.'ascii') then
c           individual function file called outfil.###
            call strpex(outfil, root)
            write(ext,4000)ifun
            ii = istrln(root)
            outfil = root(:ii)//'.'//ext
c           --- avoid filename collisions
            if ((outfil.eq.fname).or.(outfil.eq.logout).or.
     $                  (outfil.eq.data)) then
                badnam = 'log'
                ii = 3
                if (outfil.eq.data) then
                    badnam = 'data'
                    ii = 4
                elseif (outfil.eq.fname) then
                    badnam = 'input'
                    ii = 6
                endif
                call messag('     * * Warning! An individual '//
     $                      'function file has the same name')
                call messag('         as the '//badnam(:ii)//' file.')
                call strpth(outfil, root, vaxflg)
                ii = istrln(root)
                root = root(:ii)//'_'
                outfil = root(:ii+1)//'.'//ext
                ii = istrln(outfil)
                call messag('         Outfile renamed to '//
     $                      outfil(:ii))
            endif
        endif


        doc(1) = id(ifun)
        doc(2) = ' > '//functn(ifun)
        write(cnum,4010)ifun
        ii = istrln(outfil)
        call messag('              Writing function '//cnum//
     $              ' to "'//outfil(:ii)//'"')
        there = .false.
        inquire(file=outfil, exist=there)
        if ((there).and.(.not.vaxflg))
     $              call messag('              Overwriting "'//
     $                          outfil(:ii)//'"')
        call outdat(ftype, frmout, outfil, vaxflg, sk, nk,
     $              ndoc, doc, nn, xout, funout,
     $              dummy, dummy, dummy, iexist)

 200  continue 

 999  continue 

      return 
c  end subroutine phout
      end

c==================================================================

      subroutine strpth(fname, root, vaxflg)
      
c  input:
c     fname: input file name
c     vaxflg:  false if not a vax
c  output:
c     root:  fname with path and extension stripped away
c            the path is everything before the last "/" in fname 
c            ("]" for vax).  the extension is everything after the last dot

      character*(*) fname, root
      character*1   delim, dot
      logical       vaxflg

      idelim = 0
      idot   = -1
      dot    = '.'
      delim  = '/'
      if (vaxflg) delim = ']'

c  find end of path in input file name
      ii = istrln(fname)
      do 10 i=ii,1,-1
        if (fname(i:i).eq.delim) idelim = i
        if (fname(i:i).eq.dot)   idot   = i
 10   continue 
 20   continue 
      if (idot.eq.-1) idot = ii+1

      root = fname(idelim+1: idot-1)

      return 
c  end subroutine strpth
      end

c==================================================================

      subroutine strpex(fname, root)
      
c  input:
c     fname: input file name
c  output:
c     root:  fname with extension stripped away
c            the extension is everything after the last dot

      character*(*) fname, root
      character*1   dot

      idot   = -1
      dot    = '.'

c  find end of path in input file name
      ii = istrln(fname)
      do 10 i=ii,1,-1
        if (fname(i:i).eq.dot) idot = i
 10   continue 
 20   continue 
      if (idot.eq.-1) idot = ii+1

      root = fname(:idot-1)

      return 
c  end subroutine strpex
      end
      subroutine pheval

c-----------------------------------------------------------------------
c  final evaluation of sum of functions
c-----------------------------------------------------------------------
      
c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

      logical ugrid

      ugrid = .false.

c  assign final guess values to their places in values
c  at each xaxis point assign it to values
c  decode those set expressions at each xaxis point which depend on 
c  xaxis.  decode the rest at first xaxis value only.  do not use
c  istord since this might be a nofit

      do 10 i=1,nguess
        j = nset+i
        if (ipoint(j).ne.0) then
            values(1,ipoint(j)) = final(i)
        else
c           unused guess values should have already tilted the code
c           back in phenco.  this is a paranoia condition ;->
            call messag('How did you get here??? (pheval)')
            call messag('An unassigned final value was found.')
            call messag('If you are reading this, contact Bruce.')
            stop
        endif
 10   continue 

c     loop of xaxis points      
      if ( (nptout.ne.-1).and.(nptout.ne.nn) ) then
          nn     = nptout
          ugrid  = .true.
          deltax = (xmax-xmin)/(nptout-1)
      elseif (wrtout.eq.'fit') then
          nn = nfit
      elseif (wrtout.eq.'full') then
          nn = ndata
      endif

      do 100 ix=1,nn
        if (ugrid) then
            xout(ix) = real( (ix-1)*deltax + xmin )
            if (ipoint(nvar+1).ne.0)
     $                  values(1,ipoint(nvar+1)) = dble(xout(ix))
        elseif (wrtout.eq.'fit') then
            xout(ix) = real(xfit(ix))
            if (ipoint(nvar+1).ne.0)
     $                  values(1,ipoint(nvar+1)) = xfit(ix)
        elseif (wrtout.eq.'full') then
            xout(ix) = xdata(ix)
            if (ipoint(nvar+1).ne.0)
     $                  values(1,ipoint(nvar+1)) = dble(xdata(ix))
        endif

        do 30 jj=1,nline
          if (ipoint(nvar+1+jj).ne.0) then
              values(1,ipoint(nvar+1+jj)) = dble(yline(ix,jj))
c          else
c             unused xaxis value should have already tilted the code
c             back in phenco.  this is a paranoia condition ;->
c              call messag('How did you get here??? (pheval)')
c              call messag('The lineshape value was unassigned.')
c              call messag('If you are reading this, contact Bruce.')
c              stop
          endif
 30     continue 

        do 50 i=1,nset
          if ( (.not.ldepx(i)).and.(ix.gt.1) ) then
              goto 50
          else
              if (ipoint(i).ne.0) then
                  np = nptx
                  call decod(icdset(1,i), ncodex, consts, values, 
     $                        nvpts, nptx, nvarx, defval, np, outval)
                  if ((imess.eq.70).and.(ix.eq.1)) then
                      ii = istrln(sname(i))
                      print*,'at xaxis = ',
     $                            real(values(1,ipoint(nvar+1))),' ',
     $                       sname(i)(:ii),' evaluates to ', outval(1)
                  endif
                  setval(i) = outval(1)
                  values(1,ipoint(i)) = setval(i)
              endif
          endif
 50     continue 

        sumfun = 0
        do 70 ifun=1,nfunx
          if (functn(ifun).eq.nullwd) goto 70
          if ((imess.eq.70).and.(ix.eq.1)) call evmess(ifun)
          np = nptx
          call decod(icode(1,ifun), ncodex, consts, values, nvpts,
     $                nptx, nvarx, defval, np, outval)
          if ((imess.eq.70).and.(ix.eq.1)) then
              print*,'at xaxis = ',real(values(1,ipoint(nvar+1))),
     $                    ' function ',ifun,' evaluates to ', outval(1)
          endif
          sumfun = sumfun + outval(1)
 70     continue 

        yout(ix) = real(sumfun)

 100  continue 


      return 
c  end subroutine pheval
      end

c========================================================================

      subroutine evmess(if)

c  write some diagnostic messages if imess=70

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=



 40   format(i2,2x,a10,2x,f12.6,2x,i2,2x,f12.6)

c     only get here if imess=70
      print*,'(in pheval) function # ',if,':'
      print*,functn(if)
      print*,'*** vnames ***'
      do 510 i=1,nvf
        print*,i,'  ',vnames(i)
 510  continue 
      print*,'*** sets, guesses, and pointers ***'
      do 520 i=1,nset
        if (ipoint(i).ne.0)
     $              write(*,40)i,sname(i),real(setval(i)),ipoint(i),
     $              real(values(1,ipoint(i)))
 520  continue 
      do 530 i=1,nguess
        j=i+nset
        write(*,40)j,gname(i),real(guess(i)),ipoint(j),
     $              real(values(1,ipoint(j)))
 530  continue 
      print*,'   ',xaxis,ipoint(nvar+1)

      return 
c  end subroutine evmess
      end
      subroutine phnoft

c  prepare for phout in the case that no fit is requested and no data file
c  is provided

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      if (inofit.lt.2) goto 25

c  determine data range
      xmin = big
      xmax = valnul
      ixr  = 0
      do 10 i=1,nrangx
        if (range(i).gt.valnul) then
            ixr = ixr+1
            xmin = min(xmin, range(i))
            xmax = max(xmax, range(i))
        endif
 10   continue 

c  tilt if only one data range value is specified
      if ((data.eq.nullwd).and.(ixr.le.1)) then
          call messag('     * * ERROR! You have specified no '//
     $                'data file and zero or one x-range values.')
          call messag('                This is not enough '//
     $                'information to write out your function.')
          call messag('                Phit is stopping.')
          stop
      endif

      if (ixr.le.1) then
          xmin = xdata(1)
          xmax = xdata(ndata)
      endif
      if (nptout.eq.-1) nptout = 100

c  construct an even grid using the largest and smallest of range and nptout
      deltax = (xmax-xmin)/(nptout-1)
      do 20 i=1,nptout
        xfit(i) = (i-1)*deltax + xmin 
 20   continue 
      wrtout = 'fit'
c      ndata = nptout
      nfit  = nptout

 25   continue 
c  transfer any guess values so that pheval will work correctly
      do 30 i=1,nguess
        final(i) = guess(i)
 30   continue 

c  assign a name to the output file if neither data nor outfil specified
      if (outfil.ne.nullwd) goto 99
      if (data.eq.nullwd) outfil = 'phit.out'

 99   continue 
      return 
c  end subroutine phnoft
      end
      subroutine phlog

c  write a log file for a run of phit

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      character*7 cnum*3
      character*6 ordinl(nrangx/2), creal
      parameter(ncorrx=nguesx*(nguesx-1)/2, small = 1.d-3) 
      character*72 corrwd(ncorrx), reason, chiwd*25
      real         chinu
      logical      there

      data (ordinl(i), i=1,5) /'First ', 'Second', 'Third ',
     $            'Fourth', 'Fifth '/
c%%%        data ordinl(6)  /'Sixth '/
c%%%        data ordinl(7)  /'Seventh'/
c%%%        data ordinl(8)  /'Eighth'/
c%%%        data ordinl(9)  /'Ninth '/
c%%%        data ordinl(10) /'Tenth '/

 400  format(1x,a)
 405  format(1x)
 410  format(i3)
 415  format(5x,a20,5x,a)
 417  format(5x,a20,5x,g12.7)
 420  format(1x,'|',3x,a20,3(3x,g13.7))
 425  format(1x,75('-'))
 430  format(1x,75('='))
 435  format(3x,'>',2x,a)
 440  format(5x,a,i4)
 445  format(5x,a,g12.7)
 450  format(f6.3)
 455  format(5x,a6,' fitting range:',3x,g11.5,' to ',g11.5)
 467  format(5x,a20,5x,g12.7,1x,'+/-',1x,g12.7)
 470  format(5x,'"',a,'" read from: ',a)
 4000 format(' PHIT ',a5,51x,'by Bruce Ravel')

      ii    = istrln(logout)
      there = .false.
      inquire(file=logout, exist=there)
      if (there) call messag('              Overwriting  "'//
     $                       logout(:ii)//'"')

      if (.not.vaxflg) then
          open(unit=2, file=logout, status= 'unknown')
      else 
          open(unit=2, file=logout, status= 'new')
      endif

c --- program name and version number and my name
      write(2,430)
      write(2,4000)versn
      write(2,430)
      write(2,405)

c --- input data file
      ii=istrln(data)
      if (nofit) goto 10
      if (frmin.eq.'ascii') then 
          write(2,400)'Data read from: "'//data(:ii)//'"'
      else
          write(cnum,410)nkey
          call triml(cnum)
          write(2,400)'Data read from: "'//data(:ii)//'" nkey = '//cnum
      endif
      write(2,405)
 10   continue 

c --- title lines from input file
      write(2,400)'Titles:'
      do 20 i=1,ntit
        write(2,435)title(i)
 20   continue 
      write(2,405)

c --- no fit message
      if (nofit) then
          reason = 'the "nofit" keyword was found'
          if (inofit.eq.1) reason = 'no guessed values were found'
          if (inofit.eq.2) reason = 'no input data were specified'
          ii = istrln(reason)
          write(2,400)'No fit performed because '//reason(:ii)//
     $                ' in input file.'
      endif
      write(2,405)

c --- write statistics
      write(2,430)
      write(2,405)
c --- no fit
      if (nofit) then
          write(2,440)'Number of data points in output:             '
     $            ,nfit
          write(2,405)
          goto 40
      endif
c --- number of points and variables
      write(2,440)'Number of data points in fit:                    '
     $            ,nfit
      write(2,440)'Number of variables:                             '
     $            ,nguess
      write(2,440)'Number of unused data points:                    '
     $            ,nfit-nguess
      if (noerr) goto 40
c --- sigmas
      if (lsig) then
          write(2,400)'    Sigma array provided with data.'
      elseif (sigdat.gt.epsi) then
          write(2,445)'Sigma provided by user:'//
     $                '                       ',real(sigdat)
          sig = sigdat
      else
          write(2,445)'Sigma = standard deviation of residuals:'//
     $                '      ',real(sigres)
          sig = sigres
      endif
c --- chi squared
      if (nfit.le.nguess) then
          write(2,400)' '
          write(2,400)'*** You had 0 unused data points.  '//
     $                'Chi-square and the r-factor are unreliable.'
          write(2,400)' '
          dnu = 1.d0
          chiwd = 'Chi squared:             '
      else
          dnu = dble(nfit-nguess)
          chiwd = 'Reduced chi squared:    '
      endif
      chinu = real(chisqr/dnu)
      write(2,445)chiwd//'                     ',real(chinu)
c --- R factor
      write(2,445)'R factor:                '//
     $            '                     ',rfac
      write(2,405)

c --- problem in lmdif message
      if (ierbar.ne.0) then
          write(2,400)'    WARNING!  Error bars were not '//
     $                'determined.'
          write(2,400)'              Either the curvature '//
     $                'matrix was singular or one of your'
          write(2,400)'              variables '//
     $                'did not affect the fit.'
          write(2,400)'              The problem variable '//
     $                'might have been one of the following:'
          do 30 i=1,nguess
            ii = istrln(gname(i))
            if (ierflg(i).eq.1) write(2,400)'               '//
     $                  '     '//gname(i)(:ii)
 30       continue 
      endif
 40   continue 

c --- guess values names, initial guesses, best fit values, error bars
      write(2,430)
      write(2,405)
      write(2,400)'Guess values:'
      write(2,405)
      write(2,400)'|   name                  '//
     $            'initial guess   best fit value     uncertainty'
      write(2,425)
      do 50 i=1,nguess
        if (nfit.le.nguess) then
            d = delta(i)
        else
            d = delta(i) * sqrt(chinu)
        endif
        write(2,420)gname(i), real(guess(i)), real(final(i)), d
c        print*,delta(i), chinu
 50   continue 
      write(2,425)
      write(2,405)

c --- list of correlations
      if ((.not.nofit).and.(ierbar.eq.0).and.(nguess.gt.1)
     $                .and.(.not.noerr)) then
          write(creal,450)cormin
          call triml(creal)
          ii=istrln(creal)
          call getcor(corrwd, ncorr)
          if (ncorr.eq.0) then
              write(2,430)
              write(2,405)
              write(2,400)'    All '//'correletions are between '/
     $                    /creal(:ii)//' and -'//creal(:ii)
              write(2,405)
              goto 70
          endif
          write(2,400)'Correlations between guess values:'
          write(2,405)
          write(2,400)'    variable 1             variable 2'//
     $                '             correlation'
          write(2,425)
          do 60 i=ncorr,1,-1
            write(2,400)corrwd(i)
 60       continue 
          write(2,425)
          ncx = nguess*(nguess-1)/2
          if (ncorr.lt.ncx) write(2,400)'    All other '//
     $                'correletions are between '//creal(:ii)//
     $                ' and -'//creal(:ii)
          write(2,405)
      endif
 70   continue 

c --- xaxis label, fitting ranges, and lineshapes
      write(2,430)
      write(2,405)
      ii=istrln(xaxis)
      write(2,400)'The x-axis label is: '//xaxis(:ii)
      write(2,405)
      do 80 i=1,ixr/2
        if (range(i*2).lt.valnul) goto 90
        write(2,455)ordinl(i),range(2*i-1),range(2*i)
 80   continue 
 90   continue 
      write(2,405)
      ii=istrln(wrtout)
      write(2,400)'Functions written to '//wrtout(:ii)//' data range.'
      write(2,405)
      if (nline.gt.0) then
          write(2,400)'Lineshapes:'
          do 95 i=1,nline
            i1 = istrln(lname(i))
            i2 = istrln(lindat(i))
            write(2,470)lname(i)(:i1),lindat(i)(:i2)
 95       continue 
          write(2,405)
      endif

c --- set values labels and math expressions or values
c     values will be written if the set value does not depend on xaxis
c     math expression will be written if it does
c     set values + propagated error bars will be written for those
c     that depend on guess values
      write(2,430)
      write(2,405)
      write(2,400)'Set value math expressions:'
      write(2,405)
      write(2,400)'    name                        '//
     $            'value'
      write(2,425)
      do 100 i=1,nset
c       ---depends on x then write math expression
        if (ldepx(iptset(i))) then
            ii = istrln(set(iptset(i)))
            write(2,415)sname(iptset(i)), set(iptset(i))(:ii)
c       ---depends on guess value then write value and error bar
        elseif ( (ldepg(iptset(i))).and.(.not.noerr) ) then
            write(2,467)sname(iptset(i)), setval(iptset(i)),
     $                  delset(iptset(i))
c       ---independent of x and guesses, just write value
        else
            write(2,417)sname(iptset(i)), setval(iptset(i))
        endif
 100  continue 
      write(2,425)
      write(2,405)

c --- functions and id lines
      write(2,430)
      write(2,405)
      do 110 i=1,nfunx
        if (functn(i).ne.nullwd) then
            write(cnum,410)i
            if (id(i).ne.nullwd) then 
                ii = istrln(id(i))
                write(2,400)'Function '//cnum//' >  '//id(i)(:ii)
            else
                write(2,400)'Function '//cnum
            endif
            ii = istrln(functn(i))
            write(2,400)'      '//functn(i)(:ii)
            write(2,405)
        endif
 110  continue 

      close(2)
      return 
c  end subroutine phlog
      end

c=======================================================================

      subroutine getcor(corrwd, ncorr)

c  read correl matrix for correlations, sort them by absolute values, 
c  discard those smaller than cormin

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      parameter(ncorrx=nguesx*(nguesx-1)/2) 
      character*72 corrwd(ncorrx)
      real         corr(ncorrx)
      integer      iparit(ncorrx), ipair(ncorrx, 2), index(ncorrx)

 400  format(4x,a,3x,a,3x,'=',3x,f6.3)

c --- initialize local variables
      do 5 i=1,ncorrx
        corrwd(i)  = nullwd
        corr(i)    = 0.e0
        iparit(i)  = 0
        ipair(i,1) = 0
        ipair(i,2) = 0
        index(i)   = 0
 5    continue 

c --- find all correlations larger than cormin
      ncorr = 0
      do 20 i=2,nguess
        do 10 j=1,i-1
          if (abs(correl(i,j)).gt.cormin) then
              ncorr = ncorr+1
              corr(ncorr)    = abs(correl(i,j))
              iparit(ncorr)  = nint( abs(correl(i,j)) / correl(i,j))
              ipair(ncorr,1) = i
              ipair(ncorr,2) = j
          endif
 10     continue 
 20   continue 

c --- sort them by absolute value
      if (ncorr.gt.0) then 
          call sortir(ncorr, index, corr) 

c ------- save sorted correlations as a character string for log file
          do 30 i=1,ncorr
            write(corrwd(i),400)gname(ipair(index(i),1)),
     $                  gname(ipair(index(i),2)),
     $                  corr(index(i))*iparit(index(i))
 30       continue 
      endif

      return 
c  end subroutine getcor
      end 

c=======================================================================

      subroutine sortir (n, index, r)

c     I took this from Feff -- Bruce

c     SORT by rearranges Indices, keys are Real numbers
c     Heap sort, following algorithm in Knuth using r as key
c     Knuth, The Art of Computer Programming,
c     Vol 3 / Sorting and Searching, pp 146-7
c     Array r is not modified, instead array index is returned
c     ordered so that r(index(1)) is smallest, etc.
c     rr is temporary r storage (Knuth's R), irr is index of stored r

      dimension r(n), index(n)

c     Initialize index array
      do 10  i = 1, n
        index(i) = i
 10   continue
c     only 1 element is already sorted
      if (n .eq. 1)  goto 90

c     H1: initialize
      l  = n/2 + 1
      ir = n

c     H2: Decrease l or ir
 20   continue
      if (l .gt. 1)  then
          l   = l-1
          irr = index(l)
          rr  = r(irr)
      else
          irr = index(ir)
          rr  = r(irr)
          index(ir) = index(1)
          ir  = ir-1
          if (ir .eq. 1) then
              index(1) = irr
              goto 90
          endif
      endif

c     H3: Prepare for sift-up
      j = l

c     H4: Advance downward
 40   continue
      i = j
      j = 2 * j
      if (j .eq. ir)  goto 60
      if (j .gt. ir)  goto 80

c     H5: Find larger son of i
      if (r(index(j)) .lt. r(index(j+1)))  j = j+1

c     H6: Son larger than rr?
 60   continue
      if (rr .ge. r(index(j)))  goto 80

c     H7: Move son up
      index(i) = index(j)
      goto 40

c     H8: Store rr in it's proper place
 80   continue
      index(i) = irr
      goto 20

 90   continue 
      return
      end
      subroutine pherr

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


      external phunct

      real ftemp(ndatx), fjac(ndatx,nguesx), alpha(nguesx,nguesx)
      real xguess(nguesx)

c  initialize arrays and prep for fiterr
      do 10 i=1,nguesx
        xguess(i) = real(final(i))
        do 5 j=1,nguesx
          alpha(i,j) = 0.e0
 5      continue 
 10   continue 
      do 20 i=1,ndatx
        ftemp(i) = 0.e0
        do 15 j=1,nguesx
          fjac(i,j) = 0.e0
 15     continue 
 20   continue 

c  compute R-factor = sum of square of residual / sum of square of data
      rfac   = 0.e0
      sumdat = zero
      do 30 i =1,nfit
        rfac   = rfac   + diff(i)**2
        sumdat = sumdat + yfit(i)**2
 30   continue 
      rfac = rfac / real(sumdat)

      if (imess.eq.80) then
          print*,'(in pherr)'
          print*,'nfit=',nfit,'    nguess=',nguess
          print*,'lsig? ',lsig
          print*,'chisqr=',real(chisqr)
      endif

      iounit = 6
      istep  = 2
      iprint = 0
      if (imess.eq.80) iprint = 2

      if (imess.eq.80) print*,'calling fiterr'
      call fiterr(phunct, nfit, nguess, ndatx, nguesx, fvect, ftemp,
     $            fjac, alpha, iprint, iounit, istep, xguess, delta,
     $            correl, ierbar, ierflg)
      if (imess.eq.80) print*,'done with fiterr'

      if (ierbar.ne.0) then
          call messag('         WARNING!  Error bars cannot be '//
     $                'determined.')
          call messag('                   Either the curvature '//
     $                'matrix is singular or')
          call messag('                   one of your variables '//
     $                'does not affect to fit.')
          call messag('                   The problem variable '//
     $                'might be one of the following:')
          do 100 i=1,nguess
            ii = istrln(gname(i))
            if (ierflg(i).eq.1) call messag('                   '//
     $                  '     '//gname(i)(:ii))
 100      continue 
      endif

      return 
c  end subroutine pherr
      end

      subroutine phset

c---------------------------------------------------------------------------
c  reorder the set expressions to assure proper one pass evaluation.  
c  properly ordered set expressions only depend on previously evaluated
c  set expressions, guesses, x-axis, or numbers
c---------------------------------------------------------------------------
c  here's the algorithm:
c    1. treat the array of set expressions as a heap.  elements low in 
c       the heap depend on elements above.  no element is to depend
c       on an element below
c    2. the itop pointer point to the highest uncheck heap element
c    3. break the itop_th element into words where the delimiters
c       are spaces, tabs, or math elements: + - * / , ( ) ^ 
c    4. check to see if a word matches an element below in the heap.
c       if it does, swap those two elements
c    5. do this nset times
c
c  this will assure that the set expressions are ordered for one pass
c  evaluation
c
c  this will break for circular references between sets (a=b, b=a+1), 
c  but so will the fit
c---------------------------------------------------------------------------

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

      parameter(nwdx=36)
      character*74 tmpexp, work
      character*20 osname(nsetx), words(nwdx), tmpnam

      do 10 i=1,nsetx
        osname(i) = sname(i)
        iptset(i) = 0
 10   continue 

      if (imess.eq.57) then
          print*,'i, set name,  set expression'
          do 20 i=1,nset
            i1=istrln(sname(i))
            i2=istrln(set(i))
            print*,i,'  ',sname(i)(:i1),'  ',set(i)(:i2)
 20       continue 
      endif

      itop  = 1
      nwds  = nwdx
 130  continue 
      if (itop.le.nset) then
          do 100 j=1,nwds
            words(j) = ' '
 100      continue 
          work = set(itop)
          call unblnk(work)
          nwds = nwdx
          call bset(work, nwds, words)
          itarg = itop
c     --- find deepest element in heap that is part of this set expression
          do 120 j=1,nwds
            do 110 k=itop,nset
              if (words(j).eq.sname(k)) itarg=max(itarg,k)
 110        continue 
 120      continue 
c     --- present set expression is ok where it is, increment itop
          if (itarg.le.itop) then
              itop = itop+1
              goto 130
          endif
c     --- swap present sname/set with the deepest one it depends on
          tmpnam       = sname(itop)
          tmpexp       = set(itop)
          sname(itop)  = sname(itarg)
          set(itop)    = set(itarg)
          sname(itarg) = tmpnam
          set(itarg)   = tmpexp
          goto 130
      endif
c  save pointers between original and sorted positions for the 
c  sake of the log file
      do 510 i=1,nset
        do 500 j=1,nset
          if (osname(i).eq.sname(j)) iptset(i)=j
 500    continue 
 510  continue 

      if (imess.eq.57) then
          print*,' '
          print*,'i, set name,  set expression,  pointer'
          do 520 i=1,nset
            i1=istrln(sname(i))
            i2=istrln(set(i))
            print*,i,'  ',sname(i)(:i1),'  ',set(i)(:i2),' ',iptset(i)
 520      continue 
      endif

      return 
c  end subroutine phset
      end
      subroutine bset (s, nwords, words)
c
c     breaks math expressions into words.  
c     words are defined as all characters in between delimiters and
c     represent set or guess values, xaxis labels, functions, or numbers
c     words are seperated by one of the following:
c         (   )   ,   +   -   /   *   ^
c     and may be of 0 length
c
c     the open parenthesis is taken as part of a function name
c
c     args        i/o      description
c     ----        ---      -----------
c     s            i       char*(*)  string to be broken up
c     nwords      i/o      input:  maximum number of words to get
c                          output: number of words found
c     words(nwords) o      char*(*) words(nwords)
c                          contains words found.  words(j), where j is
c                          greater then nwords found, are undefined on
c                          output.
c
c      adapted from bwords written by:  steven zabinsky, september 1984
c
c-- no floating point numbers in this routine.
      implicit integer (a-z)
      character*(*) s, words(nwords)
      character     blank, comma, oparen, cparen, plus, minus,
     $              mult, div, caret
      parameter (blank = ' ', comma = ',',  oparen = '(', cparen = ')',
     $            plus = '+', minus = '-',  mult   = '*',
     $             div = '/', caret = '^' )
 
c-- betw    .true. if between words
c   comfnd  .true. if between words and a comma or equal has
c                                         already been found
      logical betw, comfnd
c-- maximum number of words allowed
      wordsx = nwords
 
c-- slen is last non-blank character in string
      slen = istrln (s)
 
c-- all blank string is special case
      if (slen .eq. 0)  then
         nwords = 0
         return
      endif
 
c-- begc is beginning character of a word
      begc = 1
      nwords = 0
      betw   = .true.
      comfnd = .true.
      do 10  i = 1, slen
         if (s(i:i) .eq. blank)  then
            if (.not. betw)  then
               nwords = nwords + 1
               words (nwords) = s (begc : i-1)
               betw = .true.
               comfnd = .false.
            endif
         elseif ((s(i:i).eq.comma).or.(s(i:i).eq.oparen)
     $                  .or.(s(i:i).eq.cparen).or.(s(i:i).eq.plus)
     $                  .or.(s(i:i).eq.minus) .or.(s(i:i).eq.mult)
     $                  .or.(s(i:i).eq.div)   .or.(s(i:i).eq.caret))
     $                  then
            if (.not. betw)  then
               nwords = nwords + 1
               words (nwords) = s(begc : i-1)
               if (s(i:i).eq.oparen) words (nwords) = s(begc : i)
               betw = .true.
            elseif (comfnd)  then
               nwords = nwords + 1
               words (nwords) = blank
            endif
            comfnd = .true.
         else
            if (betw)  then
               betw = .false.
               begc = i
            endif
         endif
         if (nwords .ge. wordsx)  return
   10 continue
c 
      if (.not. betw  .and.  nwords .lt. wordsx)  then
         nwords = nwords + 1
         words (nwords) = s (begc :slen)
      endif
      return
c end subroutine bset 
      end
      subroutine phprop

c--------------------------------------------------------------------
c  propagate error bars in guess values into set values
c--------------------------------------------------------------------

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

      dimension dsdg(nguesx, nsetx)
c  dsdg stores the change in set values with change in guess values

c     ---initialize dsdg
      do 20 j=1,nsetx
        do 10 i=1,nguesx      
          dsdg(i,j) = 0.d0
 10     continue 
 20   continue 

c     ---evaluate values array for decoding with final guess values
      do 110 i=1,nguess
        j = nset+i
        if (ipoint(j).ne.0) then
            values(1,ipoint(j)) = final(i)
        else
c           unused guess values should have already tilted the code
c           back in phenco.  this is a paranoia condition ;->
            call messag('How did you get here??? (phprop)')
            call messag('An unassigned final value was found.')
            call messag('If you are reading this, contact Bruce.')
            stop
        endif
 110  continue 

c     ---evaluate values of sets as each guess varies
      do 150 ig=1,nguess
c       ---change each final value by size of error bar
        values(1, ipoint(nset+ig)) = final(ig) + delta(ig)
        
c       ---decode each of the sets, calculate change in set divided by 
c          error bar in guess
        do 120 is=1,nset
          if (ldepx(is)) goto 120
          if (ldepg(is)) then
              np = nptx
              call decod(icdset(1,is), ncodex, consts, values, 
     $                    nvpts, nptx, nvarx, defval, np, outval)
              dsdg(ig,is) = (outval(1) - setval(is)) / delta(ig)
              values(1,ipoint(is)) = outval(1)
          endif
 120    continue 

c       ---reset present final in values
        values(1, ipoint(nset+ig)) = final(ig) 
 150  continue 

c     ---evaluate error bars in sets
      do 330 is=1,nset
c       ---do not evaluate for x-dependent or guess-independent sets
        if (ldepx(is)) goto 330
        if (ldepg(is)) then
            delset(is) = 0.d0
            do 320 jg=1,nguess
              do 310 kg=1,jg
                delset(is) = dble(correl(jg,kg)*delta(jg)*delta(kg)) *
     $                       dsdg(jg,is) * dsdg(kg,is) + delset(is)
 310          continue 
 320        continue 
            delset(is) = sqrt(delset(is))
        endif
 330  continue 

c     ---values array needs to be reset with final values for phout
      do 420 is=1,nset
        values(1,ipoint(is)) = setval(is)
 420  continue 

      return
c  end subroutine phprop
      end

      subroutine phline

c  read in a lineshape from a specified file.  determine which columns 
c  to read as x and y.  interpolate this onto the grid of the data to 
c  be fit.  for now this only works for aligned, column data.

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

      parameter (nwdx=10)
      real         xcol(ndatx), ycol(ndatx), x
      character*80 line
      character*20 words(nwdx)
      character*2  ci
      logical      isdata, there, lstop
      external     isdata

 400  format(a)
 410  format(i2)

      call messag('Phit --- Reading and interpolating lineshapes.')
      lstop = .false.


c  --- process each of the lineshapes
      do 100 i=1,nline
        if (imess.eq.30) then
            write(ci,410)i
            call messag('checking lineshape '//ci)
        endif

c       --- decide which columns to read as data
        ixcol = 0
        iycol = 0
        do 10 j=1,ncolx
          if (label(j,i).eq.'&') then
              goto 10
          elseif (label(j,i).eq.xaxis) then
              ixcol = j
          else
              iycol = j
              lname(i) = label(j,i)
          endif
 10     continue 
        ncol = max(ixcol, iycol)
        if (ixcol.eq.0) then
            ii = istrln(lindat(i))
            call messag('* * * ERROR! The column for the x-axis was '//
     $                  'not specified for file: ')
            call messag('      '//lindat(i)(:ii))
            lstop = .true.
            goto 130
        endif
        if (iycol.eq.0) then
            ii = istrln(lindat(i))
            call messag('* * * ERROR! The column for the y-axis was '//
     $                  'not specified for file: ')
            call messag('      '//lindat(i)(:ii))
            lstop = .true.
            goto 130
        endif

c     --- error check the lineshape names
        do 110 j=1,nguess
          if (gname(j).eq.lname(i)) then
              ii = istrln(lname(i))
              call messag('     * * ERROR! "'//lname(i)(:ii)//'" is a '
     $                    //'lineshape and a guess value.')
              lstop = .true.
          endif
 110    continue 
        do 120 j=1,nset
          if (sname(j).eq.lname(i)) then
              ii = istrln(lname(i))
              call messag('     * * ERROR! "'//lname(i)(:ii)//'" is a '
     $                    //'lineshape and a set value.')
              lstop = .true.
          endif
 120    continue 
        if (xaxis.eq.lname(i)) then
            ii = istrln(lname(i))
            call messag('     * * ERROR! "'//lname(i)(:ii)//'" is a '
     $                  //'lineshape and the x-axis label.')
            lstop = .true.
        endif
 130    continue 
        if (lstop) then
            call messag('Phit cannot continue with ambiguous '//
     $                  'variable names.')
            call messag('Phit is stopping.')
            stop
        endif


c       --- make sure the file is there, then open it
        inquire(file=lindat(i), exist=there)
        if (.not.there) then
            ii = istrln(lindat(i))
            call messag('* * * ERROR!')
            call messag('      '//lindat(i)(:ii)//' was not found.')
            call messag('      Phit stopping.')
            stop
        endif
        iunit = nxtunt(17)
        open(unit=iunit, file=lindat(i), status = 'old')

c       --- read data from the file
        ii = istrln(lindat(i))
        call messag('         Reading lineshape from: "'//
     $              lindat(i)(:ii)//'"')
        nd = 0
 20     continue 
        read(iunit, 400, end=40)line
        call triml(line)
        call untab(line)
        nwds = nwdx
        do 30 j=1,nwdx
          words(j) = ' '
 30     continue 
        call bwords(line, nwds, words)
        if ( (line(1:1).eq.'#').or.(line(1:1).eq.ignore)) then
            goto 20
        elseif (.not.isdata(ncol, nwdx, nwds, words)) then
            goto 20
        else
            nd = nd + 1
            call getrea( 'x-axis data', words(ixcol), xcol(nd) )
            call getrea( 'y-axis data', words(iycol), ycol(nd) )
        endif
        goto 20

c       --- interpolate onto grid of data to be fit
 40     continue 
c        nterms = 3
c        istart = 1
        nt     = 3
        do 90 j=1,nfit
          x   = real(xfit(j))
          nx0 = nofx(x, xcol, nd)
          if (nx0.eq.1) nx0=2
          call polint(xcol(nx0-1),ycol(nx0-1),nt,x,yline(j,i),dy)
 90     continue 

 100  continue 


      return 
c  end subroutine phline
      end
      subroutine polint(xa, ya, n, x, y, dy)

      integer n, nmax
      real    dy, x, y, xa(n), ya(n)
      parameter (nmax=10)
      integer i, m, ns
      real    den, dif, dift, ho, hp, w, c(nmax), d(nmax)

      ns = 1
      dif = abs(x-xa(1))

      do 11 i=1,n
        dift = abs(x-xa(i))
        if (dift.lt.dif) then
            ns = i
            dif = dift
        endif
        c(i) = ya(i)
        d(i) = ya(i)
 11   continue 
      y  = ya(ns)
      ns = ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho = xa(i) - x
          hp = xa(i+m) - x
          w = c(i+1) - d(i)
          den = ho-hp
          if (den.eq.0.) print*,'failure in polint'
          den = w/den
          d(i) = hp*den
          c(i) = ho*den
 12     continue 
        if (2*ns.lt.n-m) then
            dy = c(ns+1)
        else
            dy = d(ns)
            ns = ns-1
        endif
        y = y+dy
 13   continue 
      return 
c  end subroutine polint
      end
      subroutine echo(word, echstr, ies, lech)

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

      parameter(nval=27, nlog=9, nspec=11, ntot=nval+nlog+nspec)
      character*10 keywd(ntot)
      character*72 word, echstr, temp, vals(nval)
      logical      logval(nlog), lech(5)
      data (keywd(i), i=1,nval)/ 'data', 'infile', 'skey', 'nkey',
     $            'outfile', 'logfile', 'formin', 'formout',
     $            'ignore', 'sigdat', 'cormin',
     $            'xaxis', 'write', 'x1', 'xmin', 'x2',
     $            'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9',
     $            'x10', 'xmax', 'np', 'message'/
      data (keywd(i), i=nval+1,nval+nlog)/ 'all', 'nosum',
     $            'noout', 'residual', 'noerr', 'norun', 'dryrun',
     $            'nofit', 'sigma'/
      data (keywd(i), i=nval+nlog+1,ntot)/'comment', 'title',
     $            'format', 'line', 'guess', 'set', 'function', 'id', 
     $            'include', 'echo', 'end' /
 

      ies = 0
      call triml(word)
      if (word(1:1).ne.'@') then
          ii = istrln(word)
          echstr = '"'//word(:ii)//'"'
          ies = istrln(echstr)

      else
          ikey = 0
          temp = word(2:)
          ii = istrln(temp)
          if (temp(1:3).eq.'fun') temp='function'
          do 10 i=1,ntot
            if (temp.eq.keywd(i)) ikey=i
 10       continue 

          if (ikey.eq.0) then
              echstr = '"'//temp(:ii)//' is not a recognized keyword'
              ies = istrln(echstr)
              
c                                         numerical values
          elseif (ikey.le.nval) then
              call echval(vals)
              call triml(vals(ikey))
              echstr = '"'//temp(:ii)//'" = '//vals(ikey)
              ies = istrln(echstr)

c                                         logical values
          elseif (ikey.le.(nval+nlog)) then
              logval(1) = lall
              logval(2) = nosum
              logval(3) = (.not.lall).and.(nosum)
              logval(4) = lresid
              logval(5) = noerr
              logval(6) = ldry
              logval(7) = ldry
              logval(8) = nofit
              logval(9) = lsig
              if (logval(ikey-nval)) then
                  echstr = '"'//temp(:ii)//'" is set to '//
     $                    'true.'
              else
                  echstr = '"'//temp(:ii)//'" is set to '//
     $                    'false.'
              endif
              ies = istrln(echstr)

c                                         comment and title
          elseif (ikey.le.(nval+nlog+2)) then
              echstr = temp(:ii)//
     $                    ' lines have been written to the screen.'
              ies = istrln(echstr)

c                                         format
          elseif (ikey.eq.(nval+nlog+3)) then
              iin=istrln(frmin)
              iout=istrln(frmout)
              echstr = 'Format: on input is '//frmin(:iin)//
     $                    ' and on output is '//frmout(:iout)
              ies = istrln(echstr)

c                                         line, guess, set, function, id
          elseif (ikey.le.(nval+nlog+8)) then
              iech = ikey - (nval+nlog+3)
              lech(iech) = .true.

c                                         include, echo, end
          else
              echstr = '"'//temp(:ii)//'" controls reading of input '//
     $                    'file and has no value to echo.'
              ies = istrln(echstr)
          endif

      endif

      return 
c  end subroutine echo
      end
      subroutine echval(vals)

c      include 'phinc.h'
c-----------------------------------------------------------------------
c  copyright 1995 University of Washington        written by Bruce Ravel
c-----------------------------------------------------------------------
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= header for program PHIT =+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c  include file for Phit
      implicit double precision (a-h,o-z)
      implicit integer (i-n)
c     logicals often begin with (l), but not implicitly

c------------------------------------------------------------------
c-----parameters---------------------------------------------------

      parameter (ndatx=2**12, nrangx=10, ndocx=19, nlinex=10)
      parameter (ntitx=19, nfunx=75, nconx=300, nptx=250)
      parameter (nguesx=50, nsetx=150, nvarx=nguesx+nsetx+1+nlinex)
      parameter (ncodex=nconx+nvarx+10)
      parameter (iloopx=3, ncolx = 10)

c  ndatx:  max number of input data points
c  nrangx: max number of delimiters in input data
c  ndocx:  max number of documents in i/o data
c  nlinex: max number of input lineshapes
c  ntitx:  max number of title lines
c  nfunx:  max number of input functions
c  nconx:  max number of constants to be encoded
c  nptx:   max size of encoded vectors, for now I am using scalars (=ndatx?)
c          how big should this really be?
c  nguesx: max number of guessed values
c  nsetx:  max number of set values
c  nvarx:  max number of encoded variable names
c  ncodex: max number of encoded objects
c  iloopx: max number of re-decoding loops
c------------------------------------------------------------------

      character*100 doc(ndocx)
      character*74  functn(nfunx), set(nsetx)
      character*72  title(ntitx), id(nfunx), data, outfil, logout,
     $              lindat(0:nlinex)
      character*20  gname(nguesx), sname(nsetx), vnames(nvarx), xaxis,
     $              label(ncolx, 0:nlinex), lname(0:nlinex)
      character*10  frmin, frmout, skey, ftype, wrtout, fname
      character*5   versn
      character*3   nullwd
      character*1   ignore

c  I/O arrays, must be real
      real xdata(ndatx), ydata(ndatx), zdata(ndatx),
     $     yampl(ndatx), yphas(ndatx), 
     $     xout(ndatx),  yout(ndatx), dummy(ndatx)
      real xline(ndatx,nlinex), yline(ndatx,nlinex)

c  statistics arrays
      real cormin, fvect(ndatx), delta(nguesx), correl(nguesx, nguesx)
      real rfac, diff(ndatx)

c  initialization parameters
      parameter (zero=0.d0, one=1.d0, epsi=1.d-10)
      parameter (nullwd='?#@')
      parameter (valnul=-999999.d0)
      parameter (big=1.d20)

      logical lend, lall, ldry, vaxflg, nofit, lsig, nosum, noerr
      logical ldepx(nsetx), ldepg(nsetx), lresid
c  ldepx(i) is true if set value sname(i) depends on x-axis explicitly
c  or implicitly
c  ldepg(i) is true if set value sname(i) depends on any guess value
c  explicitly of implicitly

c------------------------------------------------------------------
c-----common blocks------------------------------------------------

      save /words/
      common /words/ title, data, outfil, xaxis, frmin, frmout,
     $               gname, sname, vnames, functn, id, versn,
     $               ftype, skey, doc, wrtout, logout, set, fname,
     $               lindat, label, ignore, lname
      
      save /data/
      common /data/ xdata, ydata, zdata, yampl, yphas, xline, yline

      save /out/
      common /out/ xout, yout, dummy

      save /stats/
      common /stats/ cormin, rfac, fvect, delta, correl,
     $               diff

      save /fit/
      common /fit/ chisqr, sigres, sigdat, xfit(ndatx),
     $             yfit(ndatx), sigma(ndatx)

      save /info/
      common /info/ range(nrangx), guess(nguesx), setval(nsetx),
     $              consts(nconx), values(nptx, nvarx), final(nguesx),
     $              delset(nsetx), xmin, xmax

      save /logic/
      common /logic/ lend, lall, ldry, vaxflg, nofit, lsig,
     $               nosum, noerr, ldepx, ldepg, lresid

      save /ints/
      common /ints/ nfun, nfunr, nguess, nset, nvar, nline, nvf, imess, 
     $              ntit, nkey, ndoc, ndata, nfit, inofit, nptout,
     $              ierbar, ixr,
     $              icode(ncodex,nfunx), icdset(ncodex,nsetx),
     $              icdxax(ncodex,nlinex),
     $              nvpts(nptx), ipoint(nvarx), ierflg(nguesx),
     $              iloops, iter, iptset(nsetx), nlnpts(0:nlinex)

      save /order/
      common /order/ nindep, ndepg, istord(nsetx)

      save /decdp/
      common /decdp/ defval(nptx), outval(nptx) 

c ------- explanations of integer flags:

c iptset:
c      iptset(i) points to position of set values and names as read
c      in the input file.  want to write sets to log file in that order
c      rather than the order found after phset

c ierbar:
c      0:  no problems encountered in fiterr
c      1:  something went wrong in fiterr
c      ierflg(i) = 1 if guess(i) is problematic, = 0 if ok

c inofit:
c      0:  nofit keyword found
c      1:  no guess values
c      2:  no input data

c imess:
c     -1:  initial state
c     40:  write diagnostic messages in phfit
c     50:  write diagnostic messages in phenco
c     55:  write ldepx diagnostic in phenco
c     57:  write phset diagnostic
c     60:  write diagnostic messages in phdata
c     70:  write function evaluations in pheval -- not so spewy
c     71:  write function evaluations in phunct -- very spewy
c     80:  write diagnostics in pherr & fiterr
c     99:  run subroutine dump to check input data

c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+= end of header for program PHIT  =+=+=+=+=+=+=+=+=+=+=
c=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=

      parameter(nval=27)
      character*72 vals(nval)

c       data (keywd, i=1,nval)/ 'data', 'infile', 'skey', 'nkey',
c      $            'outfile', 'logfile', 'formin', 'formout',
c      $            'ignore', 'sigdat', 'cormin',
c      $            'xaxis', 'write', 'x1', 'xmin', 'x2',
c      $            'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9',
c      $            'x10', 'xmax', 'np', 'message'/

 400  format(bn,i10)
 410  format(bn,g15.5)

      vals(1) = data
      vals(2) = data
      vals(3) = skey
      write(vals(4), 400)nkey
      vals(5) = outfil
      vals(6) = logout
      vals(7) = frmin
      vals(8) = frmout
      vals(9) = ignore
      write(vals(10),410)sigdat
      write(vals(11),410)cormin
      vals(12) = xaxis
      vals(13) = wrtout
      write(vals(14),410)range(1)
      write(vals(15),410)range(1)
      write(vals(16),410)range(2)
      write(vals(17),410)range(3)
      write(vals(18),410)range(4)
      write(vals(19),410)range(5)
      write(vals(20),410)range(6)
      write(vals(21),410)range(7)
      write(vals(22),410)range(8)
      write(vals(23),410)range(8)
      write(vals(24),410)range(10)
      write(vals(25),410)range(10)
      write(vals(26), 400)np
      write(vals(27), 400)imess

      return 
c  end subroutine echval
      end
      subroutine getrea(keywrd,string,value)
      
      character*(*) keywrd, string
      character*72  messg
      integer       j, k, id, ie
      real          value
      logical       isnum

 400  format(bn,f13.0)
 410  format(bn,e19.13)
 420  format(bn,d19.13)

      if (isnum(string)) then
          call lower(string)
          ie = index(string, 'e')
          id = index(string, 'd')
          if ((id.eq.0).and.(ie.eq.0)) then
              read(string,400)value
          elseif (ie.ne.0) then
              read(string,410)value
          elseif (id.ne.0) then
              read(string,420)value
          endif
      else
          j = istrln(string)
          k = istrln(keywrd)
          messg = 'Error reading "'//string(:j)//'" as the value for "'
     $                //keywrd(:k)//'"'
          call messag(messg)
          messg = '"'//string(:j)//'" must be a real number.'
          call messag(messg)
          stop
      endif

      return
c  end subroutine getrea
      end

c**********************************************************************

      subroutine getint(keywrd,string,ivalue)
      
      character*(*) keywrd, string
      character*72  messg
      integer       ivalue, j, k

 400  format(bn,i10)

      read(string,400,iostat=ierr)ivalue
      if (ierr.ne.0) then
          j = istrln(string)
          k = istrln(keywrd)
          messg = 'Error reading '//string(:j)//' as the value for '//
     $                keywrd(:k)
          call messag(messg)
          messg = string(:j)//' must be an integer.'
          call messag(messg)
          stop
      endif

      return
c  end subroutine getint
      end

c**********************************************************************

      subroutine getlog(keywrd,string,lvalue)
      
      character*(*) keywrd, string
      character*72  test*2
      logical       lvalue

      test   = 'ab'
      lvalue = .false.
      call triml(string)
      call case(test,string)
      if ( (string(:1).eq.'t') .or. (string(:1).eq.'y')
     $                         .or. (string(:2).eq.'on') )
     $            lvalue=.true. 

      return
c  end subroutine getlog
      end

c**********************************************************************

      subroutine gettit(keywrd,string,title,ntit,stdout)
      
      character*(*) keywrd, string, title
      character*72  messg, toss
      integer       ntit, i, j
      logical       stdout

      ntit  = ntit + 1
      toss  = string
      call case(keywrd,toss)
      i     = index(toss, keywrd)
      j     = istrln(keywrd)
      title = string(i+j+1:70)
      call triml(title)
      if ( (title(:1).eq.'=') .or. (title(:1).eq.',') ) then
          toss  = title(2:)
          title = toss
          call triml(title)
      endif
      if (.not.stdout) then
          messg = '  title > '//title
          call messag(messg)
      endif
      
      return
c  end subroutine gettit
      end

      subroutine case(test,word)
c  returns *word* in the same case as *test*
c  note that this is just the reverse of smcase !
      character*(*) test, word
      call smcase (word, test)
      return
c  end subroutine case
      end


      function  nofx(x,array,npts)

      implicit integer(i-n)
      implicit real(a-h,o-z)
c      implicit double precision(a-h,o-z)

c
c   function nofx
c
c   purpose
c     given a value x and an array of values, find the index
c     corresponding to the array element closest to x
c
c   usage
c     n = nofx(x,array,npts)
c
c   parameters
c     x     - a given value
c     array - array of values, assumed to be stored in order of
c             increasing value
c     npts  - number of elements in array
c
c   subroutines and function subprograms required
c     none
c
c   written  8/11/81 by j.m. tranquada
c
      dimension  array(npts)
      imin = 1
      imax = npts
      inc = ( imax - imin ) / 2
   10 continue
      it  = imin + inc
      xit = array(it)
      if ( x .lt. xit ) then
         imax = it
      else if ( x .gt. xit ) then
         imin = it
      else
         nofx = it
         return
      endif
      inc = ( imax - imin ) / 2
      if ( inc .gt. 0 ) go to 10
      xave = ( array(imin) + array(imin+1) ) / 2.
      if ( x .lt. xave ) then
         nofx = imin
      else
         nofx = imin + 1
      endif
      return
c end function nofx
      end


       subroutine openfl(iunit, file, status, iexist, ierr)
c  
c  open a file, 
c   if unit <= 0, the first unused unit number greater than 7 will 
c                be assigned.
c   if status = 'old', the existence of the file is checked.
c   if the file does not exist iexist is set to -1
c   if the file does exist, iexist = iunit.
c   if any errors are encountered, ierr is set to -1.
c
c   note: iunit, iexist, and ierr may be overwritten by this routine
       character*(*)  file, status, stat*10
       integer        iunit, iexist, ierr, iulow
       logical        opend, exist
       data  iulow /7/
c
c make sure there is a unit number
       ierr   = -3
       iexist = 0
       if (iunit.le.0) then
          iunit  = iulow
 10       continue
          inquire(unit=iunit, opened = opend)
          if (opend) then
             if (iunit.gt.20) return
             iunit = iunit + 1
             go to 10
          endif
       end if
c
c if status = 'old', check that the file name exists
       ierr = -2
       stat =  status                          
       call smcase(stat,'a')
       if (stat.eq.'old') then
          iexist = -1
          inquire(file=file, exist = exist)
          if (.not.exist) return
          iexist = iunit
       end if
c 
c open the file
       ierr = -1
       open(unit=iunit, file=file, status=status, err=100)
       ierr = 0
 100   continue
       return
c end  subroutine openfl
       end

      subroutine pijump (ph, old)
c
c     removes jumps of 2*pi in phases
c     ph = current value of phase (may be modified on output, but
c          only by multiples of 2*pi)
c     old = previous value of phase
 
ccc         implicit double precision (a-h, o-z)
      parameter (pi = 3.14159 26535 89793 23846 26433)
      parameter (twopi = 2 * pi)
      dimension xph(3)
 
      xph(1) = ph - old
      jump =  (abs(xph(1))+ pi) / twopi
      xph(2) = xph(1) - jump*twopi
      xph(3) = xph(1) + jump*twopi
 
      xphmin = min (abs(xph(1)), abs(xph(2)), abs(xph(3)))
      do 10  i = 1, 3
         if (abs (xphmin - abs(xph(i))) .le. 0.01)  isave = i
 10   continue
 
      ph = old + xph(isave)
 
      return
c end subroutine pijump
      end



c%%%        integer function nbrstr(string)
c%%%  c
c%%%  c  find a number in a string
c%%%  c  given a string that is known to begin with a digit or sign.
c%%%  c  return the position of the end of the number.
c%%%  c  nbrstr : position of end of number
c%%%        integer   istrln, i, ilen, iback
c%%%        character*(*)  string 
c%%%        character*1    digit*10, plus, minus, d, e, decml, s, sp
c%%%        logical     lexp, ldecml
c%%%        data digit  /'1234567890'/
c%%%        data minus,plus,d,e,decml /'-','+','d','e','.'/
c%%%  c------
c%%%        ldecml = .false.
c%%%        lexp   = .false.
c%%%        ilen   = istrln(string)
c%%%        nbrstr = ilen
c%%%        if (ilen.gt.1) then
c%%%           iback  = 1
c%%%  c find end of number :  digits are always ok.
c%%%  c stop at second d, e, decml, or sign that's not preceded by (d,e)
c%%%           do 200 i = 2, ilen
c%%%              sp = string(i-1:i-1)
c%%%              s  = string(i:i)
c%%%              if (index(digit,s).eq.0) then 
c%%%                 if ( ( (s.ne.plus).and.(s.ne.minus).and.(s.ne.d)
c%%%       $                 .and.(s.ne.e).and.(s.ne.decml))
c%%%       $          .or.( lexp.and.((s.eq.d).or.(s.eq.e)))
c%%%       $          .or.( ldecml.and.(s.eq.decml)) 
c%%%       $          .or.( ( (s.eq.plus).or.(s.eq.minus)).and.
c%%%       $                (sp.ne.d).and.(sp.ne.e)) )     go to 210
c%%%                 lexp   = lexp.or.(s.eq.d).or.(s.eq.e)
c%%%                 ldecml = ldecml.or.(s.eq.decml)
c%%%              end if
c%%%   200     continue 
c%%%           iback = 0
c%%%   210     continue 
c%%%           nbrstr = i - 1 - iback 
c%%%        end if
c%%%        return
c%%%  c  end function nbrstr
c%%%        end
c%%%        subroutine unblnk (string)
c%%%  c
c%%%  c remove blanks from a string
c%%%        integer        i, ilen, j
c%%%        character*(*)  string, str*256
c%%%        ilen = min(256, max(1, istrln(string)))
c%%%        j   = 0
c%%%        str = ' '
c%%%        do 10 i = 1, ilen
c%%%           if (string(i:i).ne.' ') then
c%%%              j = j+1
c%%%              str(j:j) = string(i:i)
c%%%           end if
c%%%   10   continue 
c%%%        string = ' '
c%%%        string = str(1:j)
c%%%        return
c%%%  c end subroutine unblnk
c%%%        end

       subroutine sgetvl(word, xreal, ierr)
c  reads real number "xreal" from input character string "word".
c  returns xreal = 0. and (ierr.ne.0)  if word cannot be a number
      character*(*) word
      real          xreal
      integer       ierr
      logical       number, isnum
      external      isnum
c
      xreal   = 0.
      ierr    = -999
      number  = isnum(word)
      if (number) then
          ierr = 0
          read ( word, '(bn,f30.0)', iostat = ierr)  xreal
      end if    
      return
c end subroutine sgetvl
      end
c---------------------------------------------------------------- 
c%%%        subroutine uneql (string)
c%%%  c replace commas or equal signs with blanks 
c%%%        integer        i, ilen
c%%%        character*(*)  string, equal*1
c%%%        data   equal / '=' /
c%%%        ilen = max(1, istrln(string))
c%%%        do 10 i=1,ilen
c%%%          if (string(i:i).eq.equal) string(i:i) = ' '
c%%%   10   continue 
c%%%        return
c%%%  c end subroutine uneql
c%%%        end
c---------------------------------------------------------------- 
      integer function nxtunt(iunit)

c  this function returns the value of the next unopened unit number equal
c  to or larger than iunit.  it will return neither unit numbers 0, 5, 
c  or 6 nor a negative unit number 

      logical open

      iun = iunit
      if (iun.le.0) iun = 1
 10   continue 
      if ((iun.eq.5).or.(iun.eq.6)) then
          iun = 7
          goto 10
      endif
      inquire (unit=iun, opened=open)
      if (open) then
          iun = iun + 1
          goto 10
      endif
      
      nxtunt = iun
      return 
c  end integer function nxtunt
      end

       logical function isdat(string)
c  tests if string contains numerical data 
c    returns true if the first (up to eight) words in string can
c    all be numbers. requires at least two words, and tests only 
c    the first eight columns
       integer nwords, mwords, i 
       parameter (mwords = 8)
       character*(30)  string*(*), words(mwords), line*(256)
       logical isnum
       external isnum
c       
       isdat = .false.
       do 10 i = 1, mwords 
          words(i) = 'no'
 10    continue
c
       nwords = mwords
       line   = string
       call triml(line)
       call untab(line)
       call bwords(line, nwords, words)
       if (nwords.ge.2) then
          isdat = .true.
          do 50 i = 1, nwords
             if (.not. ( isnum( words(i) ) ) ) isdat = .false.
 50       continue
       end if
c
       return
       end


       subroutine testrf(flnam, irecl, flform, ier)
c
c   test whether a data file can be interpreted as  uwxafs binary 
c   data file or  ascii column data file.
c
c   uwxafs binary files use direct access binary files 
c   with word size irecl, which is a machine dependent parameter
c
c ier = -1 : file not found
c ier = -2 : broken uwxafs file?
c ier = -3 : not uwxafs file, but can't find data. 
c ier = -4 : looks like ascii, saw line  of minus signs, 
c             but 2nd following line doesn't have data
c
c   copright 1994 university of washington   matt newville
c -----------------------------------------------------
      integer   i, irecl, iunit
      parameter (mwords = 3)
      character*(*) flnam, flform, line*80
      integer*2    indx(4)
      logical    exist, opend, isdat, prevdt, lisdat
      external  isdat
c -----------------------------------------------------
      flform = 'none'
      ier    = -1
      iunit  = 7
 10   continue
      inquire(unit=iunit, opened = opend)
      if (opend) then 
         if (iunit.gt.20) return
         iunit = iunit + 1   
         go to 10
      endif
      inquire(file = flnam, exist = exist)
      if (.not.exist) return
      ier    = -2
c -----------------------------------------------------
c try reading file as a uwxafs binary file
c     which have patriotic magic numbers embedded in them
      indx(3) = 0
      indx(4) = 0
      open(iunit, file= flnam, recl = irecl, err = 20, 
     $      access = 'direct', status = 'old' )
 20   continue
      read(iunit, rec=1, err = 25) (indx(i), i=1,4)
 25   continue
      if ((indx(3).eq.1776).and.(indx(4).eq.704)) then
         flform = 'uwxafs'
         ier  = 0
         go to 900
      end if
c -----------------------------------------------------
c try to read file as ascii data file
      close(iunit)
      open(iunit, file=flnam, status='old')
      prevdt = .false.
 200  continue
         ier  = -3
         read(iunit, '(a)', end = 900, err = 900) line
         call triml (line)
         if (line(3:6) .eq. '----') then 
            ier = -4
            read(iunit, '(a)', end = 900, err = 900) line
            read(iunit, '(a)', end = 900, err = 900) line
            lisdat = isdat(line)
            if (lisdat ) then
               flform = 'ascii'
               ier = 0
            end if
            go to 900
         end if
c if two lines in a row have all words being numbers, it is  data
         lisdat = isdat(line)
         if (lisdat.and.prevdt)  then 
            flform = 'ascii'
            ier = 0
            go to 900
         end if
         prevdt = lisdat
         go to 200
c---------------------
 900  continue
      close(iunit)
      return
c end subroutine testrf
      end
c----------------------------------------------------------------------
c          input/output routines for data files
c               for the uwxafs programs
c
c   input/output routines for data files for the uwxafs programs
c
c   copyright 1992  university of washington, seattle, washington
c   written by      matthew newville
c                   department of physics, fm-15
c                   university of washington
c                   seattle, wa   usa 98195
c   phone           (206) 543-0435
c   e-mail          newville@u.washington.edu
c
c  these routines are the basic input/output routines for getting
c  numerical and document data from files into the uwxafs programs.
c  there are currently two data formats supported:
c
c 1. 'uw' :  a binary file format known as the uwxafs file handling
c            routines. this is very efficient way to store data, and
c            can store several (191) data sets in a single file. the
c            drawback is that the files are not extremely portable.
c
c 2. 'asc':  these are column files in a format that is fairly easy
c            for anything to deal with. the files have several lines
c            of documents. if the first character of the document is
c            '#' this character will be removed. after the documents
c            is a line with minus signs for characters(3:6), then an
c            ignored line (for column labels), and then the data. up
c            to five columns are used. the expected order is:
c                  x, real(y), imag(y), ampl(y), phase(y).
c            if any column representing y is zero, the appropriate
c            value will be calculated and returned. the files in this
c            format hold only one data set, and use more memory than
c            the uwxafs files, but are portable and convenient.
c
c  other file types can be added without too much difficulty.
c  the routines listed here are:
c      inpdat : retrieve data and documents from a file
c      inpcol : retrieve data and documents from an ascii file
c      inpuwx : retrieve data and documents from a uwxafs file
c      outdat : write data and documents to a file
c      outcol : write data and documents to an ascii file
c      outuwx : write data and documents to a uwxafs file
c
c  note: the fortran input/output unit number 11 is used for all
c        unit numbers in these routines. conflicts between these
c        routines will not happen, but conflicts may arise if
c        unit = 11 indicates an open file in a calling subprogram.
c----------------------------------------------------------------------
       subroutine inpdat(filtyp, format, filnam, vax, skey, nkey, 
     $       ndoc, doc, ndata, xdata, yreal, yimag, yampl, yphas )
c
c   copyright 1992  university of washington :          matt newville
c
c    retrieve data and documents from a file acording
c    to the format specified by 'format'.
c inputs:
c   filtyp    file type to open. if may be ' '
c   format    file format (uwxafs, ascii, column)
c   filnam    file name
c   vax       logical flag for being on a vax machine (binary file)
c   skey      symbolic key for record in uwxafs file
c   ndata     maximum number of elements in data arrays
c   nkey      numeric key for record in uwxafs file
c   ndoc      maximum number of document lines to get
c               note:   ndoc cannot be less than or equal to zero!
c outputs:
c   skey      symbolic key of record in uwxafs file
c   ndoc      number of document lines returned
c   doc       array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c---------------------------------------------------------------------
       character*(*)  filtyp, format, skey, filnam, doc(*)
       character*10   type, symkey, form, formin, errmsg*80
       dimension      xdata(*), yreal(*), yimag(*)
       dimension      yampl(*), yphas(*)
       logical        vax 
       integer        irecl, ndatmx, ndocmx
       data  irecl, ndocmx, ndatmx  / 512 , 19, 4096/
c---------------------------------------------------------------------
c some initializations
       if (vax) irecl = 128   
       type = filtyp
       call triml(type)
       call upper(type)
       symkey = skey
       call triml(symkey)
       call upper(symkey)
c       
c determine format of the input file
       formin = format
       call triml(formin)
       call smcase(formin, 'a')
       call testrf(filnam, irecl, form, ier)
       call smcase(form, 'a')
       if (ier.eq.-1) then 
          call messag('  inpdat error: file not found  ')
       elseif (ier.eq.-2) then 
          call messag('  inpdat error: unknown file format = '//formin)
       elseif (ier.eq.-3) then 
          call messag('  inpdat error: poorly formatted ascii data?  ')
       elseif (ier.eq.-4) then 
          call messag('  inpdat error: no data in ascii format file? ')
       end if
       if (ier.ne.0) then 
          errmsg =    '    for file ' // filnam
          ilen   = istrln(errmsg)
          call messag( errmsg(1:ilen) )
          stop
       endif
       if ((formin.ne.' ').and.(formin(1:2).ne.form(1:2))) then
          call messag('  inpdat warning: the requested format was'//
     $         ' incorrect!')
          call messag('  form    = '//form(1:5)  )
          call messag('  formin  = '//formin(1:5)  )
       end if
c  now call the appropriate routine to get the data,
c  according to the format.
       ndata = max(1, min(ndata, ndatmx) )
       ndoc  = max(1, min(ndoc , ndocmx) )
       if (form(1:2).eq.'uw') then
          ndoc = ndocmx
          call inpuwx(type, filnam, skey, nkey, irecl, ndoc, doc,
     $         ndata, xdata, yreal, yimag, yampl, yphas )
       elseif ((form(1:2).eq.'co').or.(form(1:2).eq.'as')) then
          call inpcol(filnam, ndoc, doc,
     $         ndata, xdata, yreal, yimag, yampl, yphas )
          skey   = 'ascii'
          call upper(skey)
       else
          call messag('  inpdat error: unknown file format = '// form)
          ilen   = min(54, max(1, istrln(filnam)))
          errmsg = '                for file ' // filnam(1:ilen)
          call messag( errmsg(1:ilen+26) )
          stop
       end if
       filtyp = type
       format = form
c     
       return
c end subroutine inpdat
       end
       subroutine inpcol(filnam, ndoc, doc, ndata,
     $                   xdata, yreal, yimag, yampl, yphas)
c
c   copyright 1992  university of washington :       matt newville
c
c   open and get all information from a column file. document
c   lines are read until a line of '----', then a label line is
c   skipped and the column data are read in.  the data is read 
c   and stored in the following order:
c                xdata  yreal  yimag  yampl  yphas
c inputs:
c   filnam    file name containing data
c   ndoc      maximum number of document lines to get
c   ndata     maximum number of elements in data arrays
c outputs:
c   ndoc      number of document lines returned
c   doc       array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c---------------------------------------------------------------------
       integer   ilen , istrln, j, i, mxword, ndoc, ndata, iounit
       integer   iexist, ierr, nwords, idoc, id
       real      zero , small
       parameter( zero = 0.e0 , small = 1.e-6, mxword = 5)
       real      xdata(*), yreal(*), yimag(*), yampl(*), yphas(*)
       real      xinp(mxword)
       logical   isdat
       character*(*) filnam, doc(*)
       character*30  words(mxword), line*100, status*10, file*60
       external      istrln, isdat
c--------------------------------------------------------------------- 
 10    format(a)
       file = filnam
       ilen = istrln(file)
       if (ilen.le.0)  then
           call messag( ' inpcol:  no file name given')
           stop
       end if
c  initialize buffers
       do 80 j = 1, ndoc
          doc(j) = ' '
  80   continue
       do 100 i = 1, mxword
          words(i) = '0. '
          xinp(i)  = zero
 100   continue
       do 120 j = 1, ndata
          xdata(j) = zero
          yreal(j) = zero
          yimag(j) = zero
          yampl(j) = zero
          yphas(j) = zero
 120   continue
c  open data file
      iounit = 7
      status ='old'
      call openfl(iounit, filnam, status, iexist, ierr)
      if ((iexist.lt.0).or.(ierr.lt.0)) go to 900
c
c  get documents from header: up to ndoc
c       read file header, save as document lines,
c       remove leading '#' and '%' both of which are
c       known to be extraneous comment characters.
       nwords = 5
       idoc = 0
       id   = 1
 200   continue
          read(iounit, 10, end = 950, err = 960) line
          call triml (line)
c  if line is '----', read one more line, go read numerical data
          if (line(3:6) .eq. '----')  then
             read(iounit, 10, end = 950, err = 960) line
             goto 400
          end if
c  remove leading '#' or '%' from line 
          if ( (line(1:1).eq.'#').or.(line(1:1).eq.'%') ) then
             line(1:1) = ' '
             call triml(line)
c  if the line is all numbers, then this is data!
          elseif (isdat(line)) then
             goto 410
          end if
c  save line in doc if there's room
          if ((idoc .lt. ndoc) .and. (istrln(line).gt.0) ) then
             idoc = idoc + 1
             doc(idoc) = line
          endif
          goto 200
c     
c  read numerical data
 400   continue
          nwords = 5
          read(iounit, 10, end = 600, err = 980) line
 410      continue 
          call untab(line)
          call bwords(line,nwords,words)
          if (nwords.le.1) goto 600 
          do 450 i = 1, nwords
c >>> changed for phit
              call sgetvl(words(i), xinp(i), ierr) 
c <<<
              if (ierr.ne.0) goto 600
 450      continue
          xdata(id) = xinp(1)
          yreal(id) = xinp(2)
          yimag(id) = xinp(3)
          yampl(id) = xinp(4)
          yphas(id) = xinp(5)
          if (id.ge.ndata) go to 610
          if ( id.ge.2 ) then
             if (abs(xdata(id) - xdata(id-1)).ge.small) id = id + 1
          else
             id = id + 1
          end if
          goto 400
 600   continue
       id    = id - 1
       if (id.lt.1) go to 950
 610   continue
       ndata = id 
       if (idoc.le.0) then
          ndoc =  1
          doc(1) = 'inpdat: no document line found'
       end if
c  make sure that all columns are filled:
c   if yampl and yphas are both zero, compute them from yreal, yimag
c   if yreal and yimag are both zero, compute them from yampl, yphas
       do 800 i = 1, ndata
          if ( ( (yampl(i).eq.zero).and.(yphas(i).eq.zero) ) .and.
     $         ( (yreal(i).ne.zero).or. (yimag(i).ne.zero) ) ) then
            yampl(i) = sqrt( yreal(i)**2 + yimag(i)**2 )
            yphas(i) = atan2( yimag(i), yreal(i) )
             if (i.gt.1) call pijump( yphas(i), yphas(i-1) )
 
          elseif ( (yreal(i).eq.zero).and.(yimag(i).eq.zero)
     $        .and.(yampl(i).ne.zero)   ) then
            yreal(i) = yampl(i) * cos ( yphas(i) )
            yimag(i) = yampl(i) * sin ( yphas(i) )
 
          end if
 800   continue
c  close data file and return
       close(iounit)
       return
c error handling
c  open file - error
 900   continue
         call messag(' inpcol: error opening file '//file(1:ilen) )
         go to 990
c  end or error at reading documents 
 950   continue
 960   continue
         call messag( ' inpcol: error reading file '//file(1:ilen) )
         call messag('         during reading of documents.') 
         go to 990
c  error at reading numerical data
 980   continue
         call messag( ' inpcol: error reading file '//file(1:ilen) )
         call messag('         during reading of numerical data.')

 990     continue 
         close(iounit)
         stop
c end error handling
c end subroutine inpcol
       end
       subroutine inpuwx(ftypin, filein, skey, nkey, irecl, ndoc, 
     $           documt, ndata, xdata, yreal, yimag, yampl, yphas )
c
c   copyright 1992  university of washington :          matt newville
c
c     open and get all information from a uwxafs file
c
c inputs:
c   ftypin   file type to open, checked for compatibility, may be ' '
c   filein   file name containing data
c   skey     symbolic key for record in data file (only one of these)
c   nkey     numeric key for record in data file  (two is needed    )
c   ndoc     maximum number of document lines to get
c outputs:
c   skey      symbolic key of record in uwxafs file
c   ndoc      number of document lines returned
c   docu      array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c
c notes:
c  1   the full 'noabort' error checking is done for the calls to
c      the uwxafs routines, which means that marginally useful
c      error messages will be given when one of the uwxafs
c      filehandling routines dies.
c
c  2    currently, the following file types are supported:
c           xmu,  chi,  rsp,  env,  rspep, rip
c
c  3    uwxafs file handling routines only do single precision.
c       this routine can be made implicit double precision if the
c       array buffer is maintained as single precision:
c           implicit double precision(a-h,o-z)
c           real           buffer(maxpts)
c---------------------------------------------------------------------
       parameter( maxpts = 2048, zero = 0. )
       character*(*)  ftypin, skey, filein, documt(*)
       character*10   type, ftype, safefl*8, abrtfl*8
       character*70   filnam, messg
       dimension      xdata(*), yreal(*), yimag(*)
       dimension      yampl(*), yphas(*)
       real           buffer(maxpts)
c---------------------------------------------------------------------
c initialize
 10    format(a)
 20    format(2x,2a)
 30    format(2x,a,i3)
       safefl = ' '
       abrtfl = 'noabort'
       call upper(abrtfl)
 
       ftype = ftypin
       filnam= filein
 
       call upper(skey)
       call triml(skey)
       call triml(ftype)
       call triml(filnam)
       ilen = max(1, istrln(filnam))
c note: uwxafs requires ftype to be upper case.
       call upper (ftype)
        do 100 i = 1,ndata
            xdata(i)  = zero
            yreal(i)  = zero
            yimag(i)  = zero
            yampl(i)  = zero
            yphas(i)  = zero
100    continue
       do 110 i = 1, maxpts
            buffer(i) = zero
110    continue
c  call uwxafs file handling routines:
c : open data file
       iounit = 11
       call openrf(iounit, filnam, abrtfl, safefl, ftype, irecl, ier)
       if (ier.ne.0) then
                messg = 'inpuwx: error opening file '
           call messag(messg//filnam(:ilen))
                write (messg, '(9x,a,i4)') 'openrf error code ',ier
           call messag(messg)
           stop
       end if
c : check file type
       call gftype(iounit, type, ier)
       if (ier.ne.0) then
                messg = 'inpuwx: error getting file type for '
           call messag(messg//filnam(:ilen))
                write (messg, '(9x,a,i4)') 'gftype error code ',ier
           call messag(messg)
           stop
       end if
       call upper(type)
 
       if (ftype.eq.' ') then
           ftype = type
       elseif (ftype.ne.type) then
                messg = 'inpuwx: incorrect file type for '
           call messag(messg//filnam(:ilen))
                messg = '     file type for this file is '
           call messag(messg//type)
                messg = '     file type requested was '
           call messag(messg//ftype)
           stop
       endif
       ftypin = ftype
 
c : find out how many records there are in the file
       call gnie (iounit, nie, ier)
       if (nie.le.0) then
               messg = 'inpuwx:  no data records in '
          call messag(messg//filnam(:ilen) )
          stop
       end if
c : get skey if it wasn't given as input
       if (skey.eq.' ') then
           call gskey(iounit, nkey, skey, ier)
           if (ier.ne.0) then
                  messg = 'inpuwx: error getting skey for '
             call messag(messg//filnam(:ilen))
                  write (messg, '(9x,a,i4)') 'gskey error code ',ier
             call messag(messg)
             stop
           end if
           if (skey.eq.' ') then
             write (messg, '(1x,2a,i4)') 'inpuwx: found no skey ',
     $                                  'for nkey =',nkey
             call messag(messg)
             call messag('        in file = '//filnam(:ilen))
             stop
           end if
       end if
 
c : get nkey if it wasn't given as input
       if (nkey.eq.0) then
           call gnkey(iounit, skey, nkey, ier)
           if (ier.ne.0) then
                  messg = 'inpuwx: error getting nkey for '
             call messag(messg//filnam(:ilen))
                  write (messg, '(9x,a,i4)') 'gnkey error code ',ier
             call messag(messg)
             stop
           end if
       end if
c 
c : get documents : up to ndoc
c   first check how many document lines there are
       call gdlen(iounit, nkey, ndocln, ier)
       if (ier.ne.0) then
               messg = 'inpuwx: error getting document length for '
          call messag(messg//filnam(:ilen))
               write (messg, '(9x,a,i4)') 'gdlen error code ',ier
          call messag(messg)
          stop
       end if
       if (ndoc.gt.ndocln) ndoc = ndocln
c   then get the documents
       call getdoc(iounit, documt, ndoc, skey, nkey, ndsent, ier)
       if (ier.eq.6) then
               messg = 'inpuwx error: reading file '
          call messag(messg//filnam(:ilen) )
               messg = '  no skey or nkey given to specify record, '
          call messag(messg)
               messg = '  or an incorrect skey or nkey given '
          call messag(messg)
          stop
       elseif (ier.ne.0) then
               messg = 'inpuwx: error getting documents for '
          call messag(messg//filnam(:ilen))
               write (messg, '(9x,a,i4)') 'getdoc error code ',ier
          call messag(messg)
          stop
       end if
       ndoc = ndsent
 
c : get data
       call getrec(iounit, buffer, maxpts, skey, nkey, nbuff, ier)
       if (ier.ne.0) then
               messg = 'inpuwx: error getting data for '
          call messag(messg//filnam(:ilen))
               write (messg, '(9x,a,i4)') 'getrec error code ',ier
          call messag(messg)
          stop
       end if
 
c : close file
       call closrf(iounit,ier)
       if (ier.ne.0) then
               messg = 'inpuwx: error closing data file '
          call messag(messg//filnam(:ilen))
               write (messg, '(9x,a,i4)') 'closrf error code ',ier
          call messag(messg)
          stop
       end if
c-----------------------------------------------------------------
c finished with uwxafs routines, so now sort the data into
c xdata, re(y), imag(y), ampl(y), phase(y) according to file type
c
c convert ftype to the case of this routine.
c   'case' controls the the case of this routine
       call smcase (ftype, 'case')
c- xmu: nbuff energy, then nbuff y-values
       if (ftype.eq.'xmu') then
            ndata   = nbuff/2
            do 400 i = 1, ndata
               xdata(i) = buffer(i)
               yreal(i) = buffer(ndata + i)
               yampl(i) = yreal(i)
400         continue
c-  chi: xmin, deltax, chi(kmin + i*deltak)
       elseif (ftype.eq.'chi') then
            ndata   = nbuff - 2
            do 500 i = 1, ndata
               xdata(i) = buffer(1) + (i-1)*buffer(2)
               yreal(i) = buffer(2 + i)
               yampl(i) = yreal(i)
500         continue
c-  env,rspep: kmin, deltak, phase, amplitude pairs (kmin + i*deltak)
       elseif ( (ftype.eq.'env').or.(ftype.eq.'rspep')  ) then
            ndata   = (nbuff - 1) / 2
            do 600 i = 1, ndata
               xdata(i) = buffer(1) +(i-1)*buffer(2)
               yphas(i) = buffer(2*i+1)
               yampl(i) = buffer(2*i+2)
               yreal(i) = yampl(i) * cos ( yphas(i) )
               yimag(i) = yampl(i) * sin ( yphas(i) )
600         continue
c  rsp, rip: kmin, deltak, real, imaginary pairs (kmin + i*deltak)
       elseif ( (ftype.eq.'rsp').or.(ftype.eq.'rip')  ) then
            ndata   = (nbuff - 1) / 2
            do 700 i = 1, ndata
               xdata(i) = buffer(1) +(i-1)*buffer(2)
               yreal(i) = buffer(2*i+1)
               yimag(i) = buffer(2*i+2)
               yampl(i) = sqrt( yreal(i)**2 + yimag(i)**2 )
               yphas(i) = atan2( yimag(i), yreal(i) )
                 if (i.gt.1) call pijump( yphas(i), yphas(i-1) )
700         continue
       else
                  messg = 'inpuwx: unrecognized file type for '
             call messag(messg//filnam(:ilen))
                  messg = '        file type for this file is '
             call messag(messg//ftype)
             stop
       end if
       return
c end subroutine inpuwx
       end
       subroutine outdat(filtyp, format, filnam, vax, skey, nkey,
     $    ndoc, doc, ndata, xdata, yreal, yimag, yampl, yphas, iexist)
c
c   copyright 1992  university of washington :          matt newville
c
c    write data and documents to a file acording to the
c    format specified
c inputs:
c   filtyp    file type to open, may be ' '.
c   format    file format (uwxafs, ascii, column)
c   filnam    file name
c   vax       logical flag for being on a vax machine (binary file)
c   ndoc      number of document lines returned
c   doc       array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c   iexist    flag for whether to write redundant data to uwxafs file
c             iexist = 1 : do not write redundant data
c             iexist = 0 : do write redundant data
c outputs:
c   skey      symbolic key for record in uwxafs file
c   nkey      numeric key for record in uwxafs file
c   ndoc      number of document lines written
c
c---------------------------------------------------------------------
       character*(*)  filtyp, format, filnam, skey, doc(*)
       character*30   type, form
       dimension      xdata(*), yreal(*), yimag(*)
       dimension      yampl(*), yphas(*)
       logical        vax 
       integer        irecl
c---------------------------------------------------------------------
       irecl  = 512
       if (vax) irecl = 128   
c
       idoc = ndoc
       skey = ' '
       form = format
       type = filtyp
       call upper(type)
       call triml(type)
       call triml(form)
c convert form to the case of this routine.
c   'case' controls the the case of this routine
       call smcase (form, 'case')
c
       if (form(1:2).eq.'uw') then
             if ( (idoc.le.0).or.(idoc.gt.19) )  idoc = 19
             call outuwx(type, filnam, skey, nkey, irecl, idoc, doc,
     $           ndata, xdata, yreal, yimag, yampl, yphas, iexist)
       elseif ( (form(1:3).eq.'col').or.(form(1:3).eq.'asc') ) then
              call outcol(type, filnam, idoc, doc,
     $           ndata, xdata, yreal, yimag, yampl, yphas)
              skey = 'ascii'
             call upper(skey)
       else
         call messag('outdat: unknown file format = '//form)
         stop
       end if
c
       return
c end subroutine outdat
       end
       subroutine outcol(filtyp, filnam, ndoc, doc, ndata,
     $                  xdata, yreal, yimag, yampl, yphas)
c
c   copyright 1992  university of washington :          matt newville
c
c  open and write all information to a column file. document lines are
c  written, followed by a line of '----', then a label line, and then
c  the data are written.  the file type tells what to use for the label
c  and how many columns to write. it may be left blank.
c
c inputs:
c   filtyp    file type to write (may be ' ' : used for label only)
c   filnam    file name to write (' ' and '*' mean write to unit 6)
c   ndoc      maximum number of document lines to write
c   documt    array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c outputs:
c   ndoc      number of document lines written
c
c---------------------------------------------------------------------
       parameter(zero = 0.000000)
       character*(*)  filtyp, filnam, doc(*)
       character*80   filout, errmsg, type*10, status*10
       dimension      xdata(*), yreal(*), yimag(*)
       dimension      yampl(*), yphas(*)
c---------------------------------------------------------------------
       type   = filtyp
       call triml(type)
c convert type to the case of this routine.
       call smcase(type, 'a')
       filout = filnam
       call triml(filout)
       if (ndata.le.0) ndata = 2
 25    format(2a)
 26    format(a,5x,a)
c open data file
c     if file name is ' ' or '*', write to standard output (unit 6)
       iounit = 6
       if ((filout.ne.' ').and.(filout.ne.'*')) then
          iounit = 0
          status ='unknown'
          call openfl(iounit, filout, status, iexist, ierr)
          if ((ierr.lt.0).or.(iexist.lt.0)) go to 990
       endif
c     
c     write documents to header
       jdoc  = 0
       mxl   = 76
       mxlp1 = mxl + 1
       do 200, idoc = 1, ndoc
          call triml(doc(idoc))
          ilen = istrln(doc(idoc))
          if (ilen.ge.1) then
             jdoc = jdoc + 1
             if (ilen.gt.mxl) then
                write(iounit, 25) '# ', doc(idoc)(1:mxl)
                write(iounit, 26) '# ', doc(idoc)(mxlp1:ilen)
             else
                write(iounit, 25) '# ', doc(idoc)(1:ilen)
             end if
          end if
 200   continue
       ndoc = jdoc
       write(iounit, 25) '#----------------------------------',
     $                   '-----------------------------------'
c  write line of minus signs and the label for columns
       if (type.eq.'xmu') then
          write(iounit, 25) '#     energy          xmu       ', ' '
c >>> changed for phit
       elseif (type.eq.'2col') then
          write(iounit, 25) '#     x               y(x)       ', ' '
c <<<
       elseif (type.eq.'chi') then
          write(iounit, 25) '#     k              chi(k)     ', ' '
       elseif (type.eq.'env') then
          write(iounit, 25) '#     k          real(chi(k))   ',
     $         'imag(chi(k))   ampl(chi(k))   phase(chi(k))'
       elseif ( (type.eq.'rsp').or.(type.eq.'rspep') ) then
          write(iounit, 25) '#     r          real(chi(r))   ',
     $         'imag(chi(r))   ampl(chi(r))   phase(chi(r))'
       else
          write(iounit, 25) '#     x          real(y(x))     ',
     $         'imag(y(x))     ampl(y(x))     phase(y(x))  '
       end if
c
c  make sure that all of re(y), im(y), amp(y), and phase(y) are known
       do 300 i = 1, ndata
          if ( ( (yampl(i).eq.zero).and.(yphas(i).eq.zero) ) .and.
     $         ( (yreal(i).ne.zero).or. (yimag(i).ne.zero) ) ) then
             yampl(i) = sqrt( yreal(i)**2 + yimag(i)**2 )
             yphas(i) = atan2( yimag(i), yreal(i) )
             if (i.gt.1) call pijump( yphas(i), yphas(i-1) )
          elseif ( (yreal(i).eq.zero).and.(yimag(i).eq.zero)
     $                               .and.(yampl(i).ne.zero) ) then
             yreal(i) = yampl(i) * cos ( yphas(i) )
             yimag(i) = yampl(i) * sin ( yphas(i) )
          end if
 300   continue
c
c  write out data : some file types only write out a few columns
 380   format(2x,e13.7,3x,e13.7)
 390   format(2x,e13.7,2x,e13.7,2x,e13.7,2x,e13.7,2x,e13.7)
       if ( (type.eq.'xmu').or.(type.eq.'chi').or.
     $             (type.eq.'2col') ) then
          do 400 i = 1, ndata
             write(iounit, 380) xdata(i), yreal(i)
 400      continue
       else
          do 450 i = 1, ndata
             write(iounit, 390) xdata(i), yreal(i), yimag(i),
     $                          yampl(i), yphas(i)
 450      continue
       end if
c
c  close data file and return
       close(iounit)
       return
 990   continue
       ilen   = max(1, istrln(filnam))
       errmsg = 'outcol: error opening file '//filnam(:ilen)
       imsg   = istrln(errmsg)
       call messag(errmsg(:imsg))
       stop
c end subroutine outcol
       end

       subroutine outuwx(ftypin, filein, skey, nkey, irecl, ndoc, doc,
     $           ndata, xdata, yreal, yimag, yampl, yphas, iexist)
c
c     write out data and documents to a uwxafs file
c
c inputs:
c   ftypin    file type to write to, may be ' ' if filnam exists.
c   filein    file name to write to
c   skey      symbolic key of record in uwxafs file
c   ndoc      number of document lines returned
c   doc       array of document lines
c   ndata     number of elements in data arrays
c   xdata     array of x values of data
c   yreal     array of real part of y data values
c   yimag     array of imaginary part of y data values
c   yampl     array of amplitude part of y data values
c   yphas     array of phase part of y data values
c   iexist    flag for whether to write redundant data to file
c               iexist = 1 : do not write redundant data
c               iexist = 0 : do write redundant data
c
c   copyright 1992  university of washington :          matt newville
c-----------------------------------------------------------------------
       parameter(maxpts = 2048, maxdoc = 19, zero = 0.)
       character*(*)  filein, ftypin, doc(*), skey
       character*10   skyout, ftype, type, filnam*70, messg*70
       character*100  docout(maxdoc), abrtfl*8, safefl*8
       dimension      xdata(*), yreal(*), yimag(*)
       dimension      yampl(*), yphas(*)
       real           buffer(maxpts)
c-----------------------------------------------------------------------
c initialize
 10    format(a)
       safefl = ' '
       abrtfl = 'noabort'
       call upper(abrtfl)
       skyout = ' '
       type   = ' '
       filnam = filein
       call triml(filnam)
       ilen   = max(1, istrln(filnam))
       ftype  = ftypin
       call upper(ftype)
c >>> new for phit
       if (ftype.eq.'2COL') ftype = 'XMU'
c <<<
       do 60 i = 1, maxdoc
          docout(i) = ' '
 60    continue
c output documents
       idoc = 0
 80    continue
          idoc  = idoc + 1
          if ((idoc.ge.maxdoc).or.(idoc.gt.ndoc)) then
             idoc  = idoc - 1
             go to 100
          end if
          docout(idoc) = doc(idoc)
          call triml(docout(idoc))
          go to 80
100    continue
ccccc       ndoc = idoc
c  open data file to check file type
       iounit = 11
       call openrf(iounit, filnam, abrtfl, safefl, ftype, irecl, ier)
       if (ier.ne.0) then
               messg = 'outuwx: error opening file '//filnam(:ilen)
               imsg  = max(1, istrln(messg))
          call messag(messg(:imsg))
               write(messg, '(9x,a,i3)' ) 'openrf error code ',ier
          call messag(messg)
          stop
       end if
 
c  check file type
       call gftype(iounit, type, ier)
       call upper(type)
c  if file type was not given, close and the re-open data file
c           with file type just found, so we can write to file
       if (ftype.eq.' ')  then
           ftype = type
           call closrf(iounit,ier)
           call openrf(iounit, filnam, abrtfl, safefl, ftype,irecl,ier)
c  if file type was given but it was wrong, stop
       elseif (ftype.ne.type) then
                 messg = 'outuwx: incorrect file type for '
            call messag(messg//filnam(:ilen))
                 messg = '        file type for this file is '
            call messag(messg//type)
                 messg = '        file type requested was '
            call messag(messg//ftype)
            stop
       endif
c
c  make sure that all of re(y), im(y), amp(y), and phase(y) are known
       do 300 i = 1, ndata
          if ( ( (yampl(i).eq.zero).and.(yphas(i).eq.zero) ) .and.
     $         ( (yreal(i).ne.zero).or. (yimag(i).ne.zero) ) ) then
            yampl(i) = sqrt( yreal(i)**2 + yimag(i)**2 )
            yphas(i) = atan2( yimag(i), yreal(i) )
             if (i.gt.1) call pijump( yphas(i), yphas(i-1) )
 
          elseif ( (yreal(i).eq.zero).and.(yimag(i).eq.zero)
     $        .and.(yampl(i).ne.zero)   ) then
            yreal(i) = yampl(i) * cos ( yphas(i) )
            yimag(i) = yampl(i) * sin ( yphas(i) )
 
          end if
300    continue
c
c  put data into a single buffer according to data type
c convert ftype to the case of this routine.
c   'case' controls the the case of this routine
       call smcase(ftype, 'case')
c  usually buffer(1) and buffer(2) are xdata(1) and xdata(2) -xdata(1)
       buffer(1) = xdata(1)
       buffer(2) = xdata(2) - xdata(1)
c   xmu: nbuff energy, then nbuff y-values
       if (ftype.eq.'xmu') then
            nbuff    = 2*ndata
            do 400 i = 1, ndata
               buffer(i)         = xdata(i)
               buffer(ndata + i) = yreal(i)
400         continue
c   chi: kmin, deltak, chi(kmin + i*deltak)
       elseif (ftype.eq.'chi') then
            nbuff     = ndata + 2
            do 500 i  = 1, ndata
               buffer(2 + i)     = yreal(i)
500         continue
c   env: kmin, deltak, phase, amplitude pairs (kmin + i*deltak)
       elseif ( (ftype.eq.'env').or.(ftype.eq.'rspep') ) then
            nbuff     = 2* (ndata + 1)
            do 600 i  = 1, ndata
               buffer(2*i+1)     = yphas(i)
               buffer(2*i+2)     = yampl(i)
600         continue
c   rsp: kmin, deltak, real, imaginary pairs (kmin + i*deltak)
       elseif ( (ftype.eq.'rsp').or.(ftype.eq.'rip') ) then
            nbuff     = 2* (ndata + 1)
            do 700 i = 1, ndata
               buffer(2*i+1)     = yreal(i)
               buffer(2*i+2)     = yimag(i)
700         continue
c   other data types not yet supported
       else
            call messag('outuwx: not able to decipher ftype ='//ftype)
            stop
       end if
c 
c  generate skyout for data with hash
       call hash(buffer, nbuff, docout, idoc, skyout)
 
c  check if this record is already in the file,
c    and decide whether or not to write data and
c    documentation for the record to the file
 
       call gnkey(iounit, skyout, nkey, ier)
       if ( (iexist.eq.1).and.(nkey.ne.0) ) then
          skey = ' '
       else
          call putrec(iounit, buffer, nbuff, skyout, 0, ier)
          call putdoc(iounit, docout, idoc,  skyout, 0, ier)
          skey = skyout
       end if
 
       ftypin = ftype
c  close file and leave
       call closrf(iounit, ierr)
       return
c end subroutine outuwx
       end
      subroutine cabort(messg, abortf)
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c  conditional abort;  if abortf is .true.
c
      character*(*) messg
      logical abortf
      call messag(messg)
      if (abortf)  then
         call messag('* uwxafs data file handling abort *')
         stop 
      endif
      return
c end subroutine cabort
      end
      subroutine getdoc(iounit,doc,nl,skey,nkey,ntl,ier)
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c      get documentation lines from data file
c
c      input parameters
c        iounit : i/o unit number (integer,input)
c        doc    : document array (character,in-out)
c        nl     : no of lines to get (integer,input)
c        skey   : symbolic key of record (character,input)
c        nkey   : numeric key of record (integer,input)
c        ntc    : number of lines actually got(integer,output)
c        ier    : error code (integer,output)
c               1 - unit not declared
c               2 - nl negative or zero
c               3 - nkey .gt. indxl .or. nkey .lt. 1
c               4 - nkey is not on file
c               5 - skey and nkey don't match
c               6 - skey is not on file
c
c       if skey is blank, nkey is used. if nkey is 0, skey is used.
c       if both are given, they should match.
c
c       if doc is equal to 'doc' then data are transferred
c       to internal buffer.  nl is ignored in this case.
c       see routines getline, addline and prntdoc for
c         the manipulation of this buffer
c
      implicit integer(a-z)
      parameter (indxl = 191, nu = 2 )
c
      character*80   fname(nu), doctmp*100
      character*2048 cindx(nu)
      integer*2      indx(4,0:indxl,nu)
      logical        abortf, safe, rewrt, modify(nu)
c
      common /uwdata/ unit(nu),modify,abortf,safe,rewrt,nldoc,indx
      common /uwdocs/ cindx,fname
c
      save /uwdata/,/uwdocs/
c
      parameter (maxl=20, maxchr=100 )
      character*(maxchr) dbuf(maxl)
      common /uwdbuf/ dbuf
      save   /uwdbuf/
c
      character*(*) skey,doc(*)
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('getdoc: unit not declared',abortf)
        ier=1
        return
      endif
c
c convert form to the case of this routine.
c   'case' controls the the case of this routine
      doctmp = doc(1)
      call smcase(doctmp, 'case')
c
      if ((nl.le.0).and.(doctmp.ne.'doc')) then
        call cabort('getdoc: no document lines to get',abortf)
        ier=2
        return
      endif
c
      if(nkey.ne.0) then
        if(nkey.lt.0.or.nkey.gt.indxl) then
          call cabort('getdoc: nkey out of bounds',abortf)
          ier=3
          return
        elseif(indx(1,nkey,u).eq.0) then
          call cabort('getdoc: nkey does not exist',abortf)
          ier=4
          return
        elseif(skey.ne.' '.and.
     $      skey.ne.cindx(u)(nkey*10+1:nkey*10+10)) then
          call cabort('getdoc: skey mismatch',abortf)
          ier=5
          return
        endif
        iord=nkey
c      skey is not given
      else
        call gnkey(iounit,skey,iord,ier)
        if(iord.eq.0) then
          call cabort('getdoc: skey not found',abortf)
          ier=6
          return
        endif
      endif
c
      pru=indx(3,iord,u)
c
c      to document buffer
      if (doctmp.eq.'doc') then
        nldoc=indx(4,iord,u)
        nblk=(nldoc+4)/5
        ntl=nldoc
        do 10 i=1,nblk
          read(iounit,rec=pru+i-1) (dbuf(j),j=i*5-4,i*5)
   10   continue
c      to doc
      else
        ntl=indx(4,iord,u)
        ntl=min(ntl,nl)
        nblk=(ntl+4)/5
        do 20 i=1,nblk
          lblk=min(i*5,ntl)
          read(iounit,rec=pru+i-1) (doc(j),j=i*5-4,lblk)
   20   continue
      endif
      ier=0
      return
c end subroutine getdoc
      end
      subroutine getrec(iounit,array,nw,skey,nkey,ntw,ier)
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c        get data record from a data file
c          iounit : i/o unit number (integer,input)
c          array  : array to receive data (any type,output)
c          nw     : maximum no of words to receive(integer,input)
c          skey   : symbolic key of data record to get(character,input)
c          nkey   : numeric key of data record to get(integer,input)
c          ntw    : actual no of words received(integer,output)
c          ier    : error code(integer,output)
c            1 - unit not declared
c            2 - nw .lt. 0
c            3 - nkey .gt. indxl .or. .lt. 0
c            4 - nkey not on file
c            5 - two keys do not match
c            6 - skey not on file
c          either nkey(with skey=' ') or skey(with nkey=0) may be
c          given.  if both are given, they should match.
c
      implicit integer(a-z)
c
      parameter (indxl=191, nu=2, iword=128 )
c
      character*(*) skey
      character*80 fname(nu)
      character*2048 cindx(nu)
      integer*2 indx(4,0:indxl,nu)
      logical abortf,safe,rewrt,modify(nu)
c
      common /uwdata/ unit(nu),modify,abortf,safe,rewrt,nldoc,indx
      common /uwdocs/ cindx,fname
c
      save /uwdata/,/uwdocs/
c
      real array(nw)
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('getrec: iounit not declared',abortf)
        ier=1
        return
      endif
c
      if(nw.le.0) then
        call cabort('getrec: no data to get',abortf)
        ier=2
        return
      endif
c
      last=indx(2,0,u)
      if(nkey.ne.0) then
        if(nkey.lt.0.or.nkey.gt.indxl) then
          call cabort('getrec: nkey out of bounds',abortf)
          ier=3
          return
        elseif(nkey.gt.last) then
          call cabort('getrec: nkey does not exist',abortf)
          ier=4
          return
        elseif((skey.ne.' ').and.(skey.ne.cindx(u)(nkey*10+1:
     $      nkey*10+10))) then
          call cabort('getrec: skey mismatch',abortf)
          ier=5
          return
        endif
      iord=nkey
c
c        skey is given
c
      else
        call gnkey(iounit,skey,iord,iii)
        if(iord.eq.0) then
          call cabort('getrec: skey not found',abortf)
          ier=6
          return
        endif
      endif
c
      pru=indx(1,iord,u)
      ntw=indx(2,iord,u)
      if(ntw.gt.nw) then
        ntw=nw
        call messag('getrec: field shorter than data')
      endif
c
c        iword is no of words in a block
      nblk  = ( ntw + iword - 1) / iword
      do 10 i = 1, nblk
        l = min(i*iword, ntw)
        read(iounit,rec=pru+i-1)(array(j),j=(i-1)*iword+1,l)
   10 continue
      ier=0
      return
c end subroutine getrec
      end
      subroutine gunit(iounit,u)
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c        match unit no with iounit no
c          iounit : fortran i/o unit no(integer,input)
c          u      : corresponding index(1,2.. upto nu, in order of
c                   call to openrf routine)(integer,output)
c            relation   unit(u)=iounit
      implicit integer(a-z)
c
      parameter (indxl=191, nu=2 )
c
      character*80 fname(nu)
      character*2048 cindx(nu)
      integer*2 indx(4,0:indxl,nu)
      logical abortf,safe,rewrt,modify(nu)
c
      common /uwdata/ unit(nu),modify,abortf,safe,rewrt,nldoc,indx
      common /uwdocs/ cindx,fname
c
      save /uwdata/,/uwdocs/
c
      u=0
      do 10 n=1,nu
          if(unit(n).eq.iounit)  then
              u = n
              go to 20
          end if
   10 continue
   20 continue
      return
c end subroutine gunit
      end
      subroutine hash(array, narray, doc, ndoc, skey)
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c  generate 5 character string composed of alphanumerics
c
c   since this routine is not called too often, execution speed
c   is not as important as getting a reasonably pseudo-random skey.
c ***   note:  random numbers done as in numerical recipes
c-----------------------------------------------------------------------
       parameter(maxpts = 4096)
       parameter(imod =  6655, imul = 936, iadd = 1399)
       parameter(jmod = 14406, jmul = 967, jadd = 3041)
       parameter(kmod =  7875, kmul = 211, kadd = 1663)
       parameter(lmod =  6075, lmul = 106, ladd = 1283)
       parameter(ihalf= 3333, imost= 5555, jhalf= 7201, jthrd= 4821)
c
       double precision work(maxpts), sum(4)
       character*(*)    doc(*)
       character*(5)    skey
       real             array(*)
       integer          ikey(5), iran(131)
       logical          loop
c
c initialize
       nwork  = 2*narray
       skey   = 'skey0'
       if (narray.le.0)   return  
       ichr0  = ichar('0')
       ichra  = ichar('a')
       do 15 i = 1, 5
           ikey(i) = 0
 15    continue
       do 18 i = 1, 4
           sum(i)  = 0.0
 18    continue
 
c get measure of the magnitude of the data two different ways
c        ihalf ~= imod/2 , so that roughly half the values
c                          are used to make the partial sum
       aver  = 0.1
       part  = 0.2
       do 30 i = 1, narray
          aver  = abs( array(i) / narray ) + aver
          irndm = mod(imul*(i + narray)  + iadd, imod)
          if ( (i.gt.2) .and. (irndm.gt.ihalf) ) then
             part = abs( array(i) / narray ) + part
             if (irndm.gt.imost) then
                 part = abs( array(i-2) / narray ) + part
              else
                 part = abs( array(i-1) / narray ) + part
              end if
          end if
30     continue
 
c create work array such that all values of work are of the order 1.
c  - most values are scaled to one of the two different measures of
c    magnitude from above.
c  - a few values get scaled to be on the order of 1 without reference
c    to these values (should prevent against two arrays differing only
c    by a constant factor from having the same skey)
c  - couldn't resist the golden mean and fine structure constant.
 
       if (abs(aver).le.0.001) aver = 0.01
       if (abs(part).le.0.001) part = 0.01
       do 150 i = 1, narray
          loop           = .true.
          work(i)        = abs( array(i) / aver )
          work(i+narray) = abs( array(i) / part )
          j              = i*kmul + kadd + narray
          jrndm          = mod(jmul*j  + jadd, jmod)
          if ( jrndm.lt.jthrd ) then
              work(i) = work(i) * 3.14159265
          elseif ( jrndm.gt.jhalf ) then
              j              =  mod(i*j + jrndm, narray - 1 ) + 1
              work(i+narray) = abs( (array(i)+array(j)) / 2.71828183)
100           continue
                if(loop.and.(work(i+narray).le.(0.0072974))) then
                   work(i+narray) = work(i+narray) * 1.618033989
                   loop = .false.
                   go to 100
                elseif(loop.and.(work(i+narray).ge.( 137.036) )) then
                   work(i+narray) = work(i+narray) * 0.618033989
                   loop = .false.
                   go to 100
                endif
          end if
150     continue
c 
c generate iran: a list of 131 pseudo-random integers that
c                do not depend on the data at all
       do 200 i = 1, 131
            j       = i*imul + iadd
            jtmp    = mod(jmul*j    + jadd, jmod) + 1
            ktmp    = mod(kmul*jtmp + kadd, kmod) + 3
            iran(i) = mod(lmul*ktmp + ladd, lmod) + 7
200    continue
c 
c collect 4 different sums from the work array:
c   each value in the work array is multiplied by a pseudo-randomly
c   selected integer between 20 and 120, and 4 different sums are made.
c   since each value in work() is on the order of 1, each of the sums
c   should be of the order 100*narray. with narray being something
c   between 100 and 1000, each of the different sums will be a random
c   number on the order of 50 000. this value mod 36 should be a good
c   random number.
c
       do 500 i = 1, nwork
c       get some random numbers, and make them bigger than 50
          i1 = mod (i * imul + iadd, imod ) + 53
          i2 = mod (i * jmul + jadd, jmod ) + 67
          i3 = mod (i * kmul + kadd, kmod ) + 31
          i4 = mod (i * lmul + ladd, lmod ) + 79
 
c       use these to make random numbers between [1, 130 ]
          j1 = mod( jmul*i1 + kadd, 109) + 3
          j2 = mod( kmul*i2 + ladd, 119) + 5
          j3 = mod( lmul*i3 + iadd, 111) + 7
          j4 = mod( imul*i4 + jadd, 123) + 1
 
c       use these for the iran array of random numbers to get a set
c                                   of numbers between [20 and 150]
          k1 = mod( jmul*( i4 + iran(j1)) + kadd,  73 ) + 43
          k2 = mod( kmul*( i2 + iran(j2)) + ladd, 111 ) + 37
          k3 = mod( lmul*( i1 + iran(j3)) + iadd,  91 ) + 29
          k4 = mod( imul*( i3 + iran(j4)) + jadd, 121 ) + 19
 
c       do "randomly weighted" sum of work array
          sum(1) = sum(1) + work(i) * k1
          sum(2) = sum(2) + work(i) * k2
          sum(3) = sum(3) + work(i) * k3
          sum(4) = sum(4) + work(i) * k4
500    continue
 
c turn the sums to integers between 1 and 36 for ikey(1) - ikey(4)
       do 900 i = 1, 4
 880      continue 
          if (abs(sum(i)).ge.100 000 000) then
              sum(i) = sum(i) / 1.618033989
              go to 880
          end if
          isum  = int( sum(i) )
          ikey(i) = mod(isum, 36)
 900   continue
 
c ikey(5) : sum from document array
       isum = 0
       im   = mod(iran(16) * ndoc + iran(61), 353)  + 27
       ia   = mod(iran(77) * ndoc + iran(52), 347)  + 19
 
       do 2000 i = 1, ndoc
          call triml( doc(i) )
          jlen = max(1, istrln( doc(i)))
          do 1800 j = 1, jlen
             kseed = mod( (j + 2*i) * imul  + jadd , 127) + 1
             k     = mod( iran(kseed) * im  + ia   ,  13) + 3
             isum  = isum + k * ichar( doc(i)(j:j) )
1800      continue
2000   continue
       ikey(5) = mod(isum, 36)
c
c map integers 1 to 36 to numerals and letters
c   ascii assumed but not required. the numerals must be
c   ordered 0 - 9 and the letters must be ordered a - z.
       do 4000 i = 1, 5
         if (ikey(i).le.9) then
            ikey(i) = ikey(i) + ichr0
         else
            ikey(i) = ikey(i) - 10 + ichra
          end if
4000   continue
 
c write skey from ikey
       do 5000 i = 1, 5
          skey(i:i) = char( ikey(i) )
5000   continue
       call upper(skey)
       return
c end subroutine hash
       end
       subroutine openrf(iounit,lfn,aflag,sflag,ftype,irecl,ier)
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c    openrf uses direct access binary files with word size  irecl.
c    irecl = 128 on vax, 512 otherwise?
c
c      structure of random data file
c
c       block size=512 byte=128 word
c
c       block 1-3 : indx(4,0:indxl,u)
c       block 4-7 : cindx(u)*2048
c
c       first data block is 8
c
c       indx(1,0,u) address of eof (non exisiting)
c       indx(2,0,u)=no of entries
c       indx(3,0,u)=1776  for identification
c       indx(4,0,u)=704   same purpose
c
c       indx(1,n,u)=address of data n
c       indx(2,n,u)=no of words for data n
c       indx(3,n,u)=address of doc n
c       indx(4,n,u)=no of lines for doc n
c
c       cindx(u)(1:10)=ftype
c       cindx(u)(n*10+1:n*10+10)=skey for nkey n
c---------------------------
      implicit integer(a-z)
      parameter (indxl = 191, nu = 2)
      character*(*) ftype,lfn,aflag,sflag
      character*80 fname(nu),fn, aflg*10, sflg*10
      character*2048 cindx(nu)
      integer*2 indx(4,0:indxl,nu)
      logical abortf,safe,rewrt,exist
      logical clor, modify(nu) 
c
      common /uwdata/ unit(nu),modify,abortf,safe,rewrt,nldoc,indx
      common /uwdocs/ cindx,fname
c
      save /uwdata/,/uwdocs/
c
ccc      data unit,nldoc/nu*0,0/
ccc      data abortf,safe/.true.,.false./
c
        clor =.false.
c   find out the case of this routine.
c   'case' controls the the case of this routine
       sflg  = sflag 
       aflg  = aflag 
       call smcase(aflg, 'case')
       call smcase(sflg, 'case')
c
      abortf = (aflg.ne.'noabort') 
      safe   = safe.or.(sflg.eq.'safe')
c
c      if lfn is blank, lfn=for0un where un is fortran i/o no
      if(lfn.eq.' ') then
        fn = 'for0'
        write(fn(5:6),1,err=9999)iounit
    1   format(i2.2)
      else
        fn=lfn
      endif
      inquire (file=fn,exist=exist)
c
      call gunit(iounit,u)
      if(u.eq.0) then
c      assign iounit no to unit(n), a table
        do 100 n=1,nu
          if(unit(n).eq.0) then
            unit(n)=iounit
            u=n
            fname(u)=fn
            go to 110
          endif
 100     continue
c
        call cabort('openrf: max no of files exceeded',abortf)
        ier=1
        return
 110    continue
c
      else
        call messag('openrf: unit reopened')
      endif
c
      if(ftype.eq.' ') then
c      no modify permit
        modify(u)=.true.
      else
        modify(u)=.false.
      endif
c
      if(exist) then
c          file exists
        if(.not.modify(u)) then
c          can modify the file
          open(iounit, file=fn, recl=irecl, access='direct',
     $         status='old', iostat=iosb, err=9999)
        else
c          cannot modify the file
          open(iounit, file=fn, recl=irecl, access='direct',
     $                            status='old')
cccccc        $         ,readonly)
        endif
c          read in existing index
        do 10 i=1,3
          read(iounit,rec=i)((indx(k,l,u),k=1,4),l=i*64-64,i*64-1)
   10   continue
        do 20 i=1,4
          read(iounit,rec=i+3)cindx(u)(i*512-511:i*512)
   20   continue
c
        if(indx(3,0,u).ne.1776 .or. indx(4,0,u).ne.704) then
          call cabort('openrf: wrong file',abortf)
          ier=4
          return
        endif
        ier=0
c
        if(ftype.ne.' '.and.ftype.ne.cindx(u)(1:10))then
          call cabort('openrf: wrong file type',abortf)
          ier=3
          return
        endif
        return
c
c          new file
c
      else
        if(ftype.eq.' ') then
          call cabort('openrf: ftype needed for file creation',abortf)
          ier=5
          return
        endif
c          create a new file
        open(iounit,file=fn,recl=irecl,access='direct',
     x       status='new')
        cindx(u)=ftype
c          index initialization
        do 30 i=1,indxl
           do 30 j=1,4
              indx(j,i,u)=0
   30   continue
        indx(1,0,u)=8
        indx(2,0,u)=0
        indx(3,0,u)=1776
        indx(4,0,u)=704
        ier=0
        go to 99
      endif
c
      entry wrindx(iounit)
c          write out index
      call gunit(iounit,u)
   99 continue
      do 101 i=1,3
        write(iounit,rec=i,err=9999)
     $        ((indx(j,k,u),j=1,4),k=i*64-64,i*64-1)
  101 continue
      do 111 i=1,4
        write(iounit,rec=i+3,err=9999)cindx(u)(i*512-511:i*512)
  111 continue
        if(clor) go to 77
 9998 return
 9999 ier=iosb
      go to 9998
c
      entry closrf(iounit,ier)
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('openrf: unit not declared',abortf)
        ier=1
        return
      endif
c         reset i/o unit table
      unit(u)=0
      ier=0
c            if modified, rewrite index
         clor=.false.
      if(modify(u)) then
         clor=.true.
         go to 99
      endif
 77   continue
      close(iounit)
      clor=.false.
      return
c
      entry rwrtrf(iounit,ier)
c          rewrite interlock.  safeguard.
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('openrf: unit not declared',abortf)
        ier=1
        return
      endif
c
      rewrt=.true.
      return
c end subroutine openrf
      end
      subroutine putdoc(iounit,doc,nl,skey,nkey,ier)
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c        put documentation lines to a random access data file
c          iounit  : fortran i/o unit no(integer,input)
c          doc     : document lines(character,input)
c          nl      : no of lines to be written(integer,input)
c          skey    : symbolic key associated with the record(char,in)
c          nkey    : numeric key associated with the record(integer,in)
c          ier     : error code (integer,output)
c            1 - unit not declared
c            2 - nl .le. 0
c            3 -
c            4 - skey not given
c            5 - rewrite interlock not cleared
c            6 - skey not found
c          if doc='doc' , internal buffer is used
c          symbolic key must be given
c          for new record, nkey should be zero.
c
      implicit integer(a-z)
c
      parameter (indxl=191, nu=2 )
c
      character*(*) skey
      character*80 fname(nu), doctmp*100
      character*2048 cindx(nu)
      integer*2 indx(4,0:indxl,nu)
      logical abortf,safe,rewrt,modify(nu)
c
      common /uwdata/ unit(nu),modify,abortf,safe,rewrt,nldoc,indx
      common /uwdocs/ cindx,fname
c
      save /uwdata/,/uwdocs/
c
      parameter(maxl=20, maxchr=100 )
      character*(maxchr) dbuf(maxl)
      common /uwdbuf/ dbuf
      save /uwdbuf/
c
      character*(*) doc(*)
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('putdoc: unit not declared',abortf)
        ier=1
        return
      endif
c
c convert form to the case of this routine.
c   'case' controls the the case of this routine
      doctmp = doc(1)
      call smcase(doctmp, 'case')
c
      if ((nl.le.0).and.(doctmp.ne.'doc')) then
        call cabort('putdoc: no documents to write',abortf)
        ier=2
        return
      endif
c
      if(skey.eq.' ') then
        call cabort('putdoc: skey must be given',abortf)
        ier=4
        return
      endif
c
      last=indx(2,0,u)
c
      if(nkey.ne.0) then
c          existing record
        if(.not.rewrt) then
          call cabort('putdoc: rewrite interlock not cleared',abortf)
          ier=5
          return
        endif
        rewrt=.false.
        iord=nkey
      else
c        new record
        call gnkey(iounit,skey,iord,ier)
        if(iord.eq.0) then
          call cabort('putdoc: skey not found',abortf)
          ier=6
          return
        endif
      endif
c
c        write at the end, always
      pru=indx(1,0,u)
      if (doctmp.eq.'doc') then
c        write internal buffer
        indx(4,iord,u)=nldoc
        nblk=(nldoc+4)/5
        do 10 i=1,nblk
            lblk=min(i*5,nldoc)
            write(iounit, rec=pru+i-1) (dbuf(j),j=i*5-4,lblk)
   10   continue
      else
c        write doc
        indx(4,iord,u)=nl
        nblk=(nl+4)/5
        do 20 i=1,nblk
            lblk=min(i*5,nl)
            write(iounit, rec=pru+i-1)(doc(j),j=i*5-4,lblk)
   20   continue
      endif
c        adjust index
      indx(3,iord,u)=pru
      indx(1,0,u)=pru+nblk
c        if (safe) write out new index
      if(safe) then
        call wrindx(iounit)
      else
c        otherwise, mark it
c        new index will be written when closrf is called
        modify(u)=.true.
      endif
      ier=0
      return
c end subroutine putdoc
      end
      subroutine putrec(iounit,array,nw,skey,nkey,ier)
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c        put data record to a random access data file
c
c         iounit : i/o unit no(integer,input)
c         array  : data to be put(any type,input)
c         nw     : no of words in array(integer,input)
c         skey   : symbolic key for the record(character,input)
c         nkey   : numeric key for the record(integer,input)
c         ier    : error code(integer,output)
c           1 - unit not declared
c           2 - nw .lt. 0
c           3 - file protection violated
c           4 - skey is blank
c           5 - rewrite interlock not cleared
c           6 - record length is different for rewrite
c           7 - nkey does not exist
c           8 - index full
c
c         skey must be given always and nkey should be zero for new record
c         non zero nkey means rewrite. - nw should be equal to current one
c
      implicit integer(a-z)
c
      parameter (indxl=191, nu=2, iword = 128 )
c
      character*(*) skey
      character*80 fname(nu),ctmp
      character*2048 cindx(nu)
      integer*2 indx(4,0:indxl,nu)
      logical abortf,safe,rewrt,modify(nu)
c
      common /uwdata/ unit(nu),modify,abortf,safe,rewrt,nldoc,indx
      common /uwdocs/ cindx,fname
c
      save /uwdata/,/uwdocs/
c
      real array(nw)
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('putrec: unit not declared',abortf)
        ier=1
        return
      endif
c
      if(nw.lt.0) then
        call cabort('putrec:  no data to write',abortf)
        ier=2
        return
      endif
c
      if(modify(u)) then
        call cabort('putrec: '//fname(u)//
     $                    ' has no write permission',abortf)
        ier=3
        return
      endif
c
      if(skey.eq.' ') then
        call cabort('putrec:  skey not given',abortf)
        ier=4
        return
      endif
c
      last=indx(2,0,u)
      if(nkey.ne.0) then
c        rewrite
        if(.not.rewrt) then
          call cabort('putrec:  rewrite interlock not cleared',abortf)
          ier=5
          return
        endif
        rewrt=.false.
c          rewrite
        if(nkey.gt.last) then
          call cabort('putrec: old nkey does not exist',abortf)
          ier=7
          return
        endif
        if(nw.ne.indx(2,nkey,u)) then
          call cabort('putrec: old/new record length dont match',abortf)
          ier=6
          return
        endif
        pru=indx(1,nkey,u)
c
c        new record
      else
        if(last.ge.indxl) then
          call cabort('putrec: index full',abortf)
          ier=8
          return
        endif
        pru=indx(1,0,u)
      endif
c
      nblk  = (nw + iword - 1)/iword
      do 10 i = 1 , nblk
        l = min(i*iword, nw)
        write(iounit, rec=i+pru-1)(array(j),j=(i-1)*iword+1,l)
   10 continue
c
c          new index for rewrite
      if(nkey.ne.0) then
        call messag('putrec:  symbolic key overwritten'//
     $              cindx(u)(nkey*10+1:nkey*10+10))
        cindx(u)(nkey*10+1:nkey*10+10)=skey
      else
c          new index for new record
        cindx(u)(last*10+11:last*10+20)=skey
        indx(1,last+1,u)=indx(1,0,u)
        indx(2,last+1,u)=nw
      endif
c          new no of entries and eof block no
      indx(1,0,u)=indx(1,0,u)+nblk
      indx(2,0,u)=last+1
      ier=0
c      check duplicate key
      ctmp=skey
      ntmp=nkey
  100 continue
      do 500 n=1,last
      if(ctmp.ne.cindx(u)(n*10+1:n*10+10)) go to 500
      if(n.ne.ntmp) go to 510
  500 continue
c         no duplicate key
      go to 600
c      put an asterisk, if one is not there already
  510 continue
      do 520 i=6,10
        if(cindx(u)(n*10+i:n*10+i).eq.'*') go to 520
        cindx(u)(n*10+i:n*10+i)='*'
c      check again if new skey is duplicate
        ctmp=cindx(u)(n*10+1:n*10+10)
        ntmp=n
        go to 100
  520 continue
      call cabort('putrec:  same skey occured five times',abortf)
  600 continue
      if(safe) then
        call wrindx(iounit)
      else
        modify(u)=.true.
      endif
      return
c end subroutine putrec
      end
      subroutine rfmisc
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c  miscellaneous routines for handling uwexafs files.  most of 
c  the entries here are to find out what's inside a file.
c        copyright university of washington 1981
c
      implicit integer(a-z)
c
      parameter (indxl=191, nu=2)
c
      character*(*) skey,ftype,lfn
      character*80 fname(nu)
      character*2048 cindx(nu)
      integer*2 indx(4,0:indxl,nu)
      logical abortf,safe,rewrt,modify(nu)
c
      common /uwdata/ unit(nu),modify,abortf,safe,rewrt,nldoc,indx
      common /uwdocs/ cindx,fname
c
      save /uwdata/,/uwdocs/
c
      entry gskey(iounit,nkey,skey,ier)
c
c          get symbolic key from a numeric key
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('gskey: unit not declared',abortf)
        ier=1
        return
      endif
      if(nkey.lt.0.or.nkey.gt.indxl) then
        call cabort('gskey:  nkey out of range',abortf)
        ier=2
        return
      endif
c          if nkey does not exist, skey=' '
      skey=cindx(u)(nkey*10+1:nkey*10+10)
      ier=0
      return
c
      entry gnkey(iounit,skey,nkey,ier)
c
c          get a numeric key from a symbolic key
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('gnkey:  unit not declared',abortf)
        ier=1
        return
      endif
c
      last=indx(2,0,u)
      do 300 n=1,last
        if(skey.eq.cindx(u)(n*10+1:n*10+10)) go to 310
  300 continue
c        skey not found
      nkey=0
      return
c
  310 continue
      nkey=n
      ier=0
      return
c
      entry gftype(iounit,ftype,ier)
c
c      get file-type from a i/o unit no
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('gftype:  iounit not declared',abortf)
        ier=1
        return
      endif
c
      ftype=cindx(u)(1:10)
      ier=0
      return
c
      entry glfn(iounit,lfn,ier)
c
c        get filename from   i/o unit no
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('gfln: iounit not declared',abortf)
        ier=1
        return
      endif
c
      lfn=fname(u)
      ier=0
      return
c
      entry gflen(iounit,flen,ier)
c
c         get the length of a file from i/o unit no
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('gflen: iounit not declared',abortf)
        ier=1
        return
      endif
c
      flen=indx(1,0,u)-1
      return
c
      entry gnie(iounit,nie,ier)
c
c          get number of entries from i/o unit no
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('gnie: iounit not declared ',abortf)
        ier=1
        return
      endif
c
      nie=indx(2,0,u)
      ier=0
      return
c
      entry grlen(iounit,nkey,rlen,ier)
c
c         get the length of a data record (in words) from a numeric key
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('grlen: iounit not declared ',abortf)
        ier=1
        return
      endif
c
      if(nkey.lt.0.or.nkey.gt.indxl) then
        call cabort('grlen: nkey out of range',abortf)
        ier=2
        return
      endif
c
      rlen=indx(2,nkey,u)
      ier=0
      return
c
      entry gdlen(iounit,nkey,dlen,ier)
c
c       get the length of document (in lines) from a numeric key
c
      call gunit(iounit,u)
      if(u.eq.0) then
        call cabort('gdlen: unit not declared',abortf)
        ier=1
        return
      endif
c
      if(nkey.lt.0.or.nkey.gt.indxl) then
        call cabort('gdlen: nkey out of range',abortf)
        ier=2
        return
      endif
c
      dlen=indx(4,nkey,u)
      ier=0
      return
c
c end subroutine rfmisc
      end
      block data uwbdat
c
c        copyright university of washington 1981
c        part of uwxafs binary (rdf) file handling 
c
c    block data statements for uwxafs 
c
      implicit integer(a-z)
      parameter (indxl = 191, nu = 2)
      integer*2 indx(4,0:indxl,nu)
      logical abortf,safe,rewrt
      logical modify(nu)
c
      common /uwdata/ unit(nu),modify,abortf,safe,rewrt,nldoc,indx
      save /uwdata/
      data unit,nldoc/nu*0,0/
      data abortf,safe/.true.,.false./
c end block data
      end          
      real function enorm(n,x)
      integer n
      real x(n)
c     **********
c
c     function enorm
c
c     given an n-vector x, this function calculates the
c     euclidean norm of x.
c
c     the euclidean norm is computed by accumulating the sum of
c     squares in three different sums. the sums of squares for the
c     small and large components are scaled so that no overflows
c     occur. non-destructive underflows are permitted. underflows
c     and overflows do not occur in the computation of the unscaled
c     sum of squares for the intermediate components.
c     the definitions of small, intermediate and large components
c     depend on two constants, rdwarf and rgiant. the main
c     restrictions on these constants are that rdwarf**2 not
c     underflow and rgiant**2 not overflow. the constants
c     given here are suitable for every known computer.
c
c     the function statement is
c
c       real function enorm(n,x)
c
c     where
c
c       n is a positive integer input variable.
c
c       x is an input array of length n.
c
c     subprograms called
c
c       fortran-supplied ... abs,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i
      real agiant,floatn,one,rdwarf,rgiant,s1,s2,s3,xabs,x1max,x3max,
     *     zero
      data one,zero,rdwarf,rgiant /1.0e0,0.0e0,3.834e-20,1.304e19/
      s1 = zero
      s2 = zero
      s3 = zero
      x1max = zero
      x3max = zero
      floatn = n
      agiant = rgiant/floatn
      do 90 i = 1, n
         xabs = abs(x(i))
         if (xabs .gt. rdwarf .and. xabs .lt. agiant) go to 70
            if (xabs .le. rdwarf) go to 30
c
c              sum for large components.
c
               if (xabs .le. x1max) go to 10
                  s1 = one + s1*(x1max/xabs)**2
                  x1max = xabs
                  go to 20
   10          continue
                  s1 = s1 + (xabs/x1max)**2
   20          continue
               go to 60
   30       continue
c
c              sum for small components.
c
               if (xabs .le. x3max) go to 40
                  s3 = one + s3*(x3max/xabs)**2
                  x3max = xabs
                  go to 50
   40          continue
                  if (xabs .ne. zero) s3 = s3 + (xabs/x3max)**2
   50          continue
   60       continue
            go to 80
   70    continue
c
c           sum for intermediate components.
c
            s2 = s2 + xabs**2
   80    continue
   90    continue
c
c     calculation of norm.
c
      if (s1 .eq. zero) go to 100
         enorm = x1max*sqrt(s1+(s2/x1max)/x1max)
         go to 130
  100 continue
         if (s2 .eq. zero) go to 110
            if (s2 .ge. x3max)
     *         enorm = sqrt(s2*(one+(x3max/s2)*(x3max*s3)))
            if (s2 .lt. x3max)
     *         enorm = sqrt(x3max*((s2/x3max)+(x3max*s3)))
            go to 120
  110    continue
            enorm = x3max*sqrt(s3)
  120    continue
  130 continue
      return
c
c     last card of function enorm.
c
      end
      subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
      integer m,n,ldfjac,iflag
      real epsfcn
      real x(n),fvec(m),fjac(ldfjac,n),wa(m)
      external fcn
c     **********
c
c     subroutine fdjac2
c
c     this subroutine computes a forward-difference approximation
c     to the m by n jacobian matrix associated with a specified
c     problem of m functions in n variables.
c
c     the subroutine statement is
c
c       subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
c
c         subroutine fcn(m,n,x,fvec,iflag)
c         integer m,n,iflag
c         real x(n),fvec(m)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ----------
c         return
c         end
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of fdjac2.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an input array of length n.
c
c       fvec is an input array of length m which must contain the
c         functions evaluated at x.
c
c       fjac is an output m by n array which contains the
c         approximation to the jacobian matrix evaluated at x.
c
c       ldfjac is a positive integer input variable not less than m
c         which specifies the leading dimension of the array fjac.
c
c       iflag is an integer variable which can be used to terminate
c         the execution of fdjac2. see description of fcn.
c
c       epsfcn is an input variable used in determining a suitable
c         step length for the forward-difference approximation. this
c         approximation assumes that the relative errors in the
c         functions are of the order of epsfcn. if epsfcn is less
c         than the machine precision, it is assumed that the relative
c         errors in the functions are of the order of the machine
c         precision.
c
c       wa is a work array of length m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... spmpar
c
c       fortran-supplied ... abs,amax1,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j
      real eps,epsmch,h,temp,zero
      real spmpar
      data zero /0.0e0/
c
c     epsmch is the machine precision.
c
      epsmch = spmpar(1)
c
      eps = sqrt(amax1(epsfcn,epsmch))
      do 20 j = 1, n
         temp = x(j)
         h = eps*abs(temp)
         if (h .eq. zero) h = eps
         x(j) = temp + h
         call fcn(m,n,x,wa,iflag)
         if (iflag .lt. 0) go to 30
         x(j) = temp
         do 10 i = 1, m
            fjac(i,j) = (wa(i) - fvec(i))/h
   10       continue
   20    continue
   30 continue
      return
c
c     last card of subroutine fdjac2.
c
      end
      subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
     *                 diag,mode,factor,nprint,info,nfev,fjac,ldfjac,
     *                 ipvt,qtf,wa1,wa2,wa3,wa4)
c
      integer m,n,maxfev,mode,nprint,info,nfev,ldfjac
      integer ipvt(n)
      real ftol,xtol,gtol,epsfcn,factor
      real x(n),fvec(m),diag(n),fjac(ldfjac,n),qtf(n),wa1(n),wa2(n),
     *     wa3(n),wa4(m)
      external fcn
c     **********
c
c     subroutine lmdif
c
c     the purpose of lmdif is to minimize the sum of the squares of
c     m nonlinear functions in n variables by a modification of
c     the levenberg-marquardt algorithm. the user must provide a
c     subroutine which calculates the functions. the jacobian is
c     then calculated by a forward-difference approximation.
c
c     the subroutine statement is
c
c       subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
c                        diag,mode,factor,nprint,info,nfev,fjac,
c                        ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
c
c         subroutine fcn(m,n,x,fvec,iflag)
c         integer m,n,iflag
c         real x(n),fvec(m)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ----------
c         return
c         end
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of lmdif.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.
c
c       fvec is an output array of length m which contains
c         the functions evaluated at the output x.
c
c       ftol is a nonnegative input variable. termination
c         occurs when both the actual and predicted relative
c         reductions in the sum of squares are at most ftol.
c         therefore, ftol measures the relative error desired
c         in the sum of squares.
c
c       xtol is a nonnegative input variable. termination
c         occurs when the relative error between two consecutive
c         iterates is at most xtol. therefore, xtol measures the
c         relative error desired in the approximate solution.
c
c       gtol is a nonnegative input variable. termination
c         occurs when the cosine of the angle between fvec and
c         any column of the jacobian is at most gtol in absolute
c         value. therefore, gtol measures the orthogonality
c         desired between the function vector and the columns
c         of the jacobian.
c
c       maxfev is a positive integer input variable. termination
c         occurs when the number of calls to fcn is at least
c         maxfev by the end of an iteration.
c
c       epsfcn is an input variable used in determining a suitable
c         step length for the forward-difference approximation. this
c         approximation assumes that the relative errors in the
c         functions are of the order of epsfcn. if epsfcn is less
c         than the machine precision, it is assumed that the relative
c         errors in the functions are of the order of the machine
c         precision.
c
c       diag is an array of length n. if mode = 1 (see
c         below), diag is internally set. if mode = 2, diag
c         must contain positive entries that serve as
c         multiplicative scale factors for the variables.
c
c       mode is an integer input variable. if mode = 1, the
c         variables will be scaled internally. if mode = 2,
c         the scaling is specified by the input diag. other
c         values of mode are equivalent to mode = 1.
c
c       factor is a positive input variable used in determining the
c         initial step bound. this bound is set to the product of
c         factor and the euclidean norm of diag*x if nonzero, or else
c         to factor itself. in most cases factor should lie in the
c         interval (.1,100.). 100. is a generally recommended value.
c
c       nprint is an integer input variable that enables controlled
c         printing of iterates if it is positive. in this case,
c         fcn is called with iflag = 0 at the beginning of the first
c         iteration and every nprint iterations thereafter and
c         immediately prior to return, with x and fvec available
c         for printing. if nprint is not positive, no special calls
c         of fcn with iflag = 0 are made.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0  improper input parameters.
c
c         info = 1  both actual and predicted relative reductions
c                   in the sum of squares are at most ftol.
c
c         info = 2  relative error between two consecutive iterates
c                   is at most xtol.
c
c         info = 3  conditions for info = 1 and info = 2 both hold.
c
c         info = 4  the cosine of the angle between fvec and any
c                   column of the jacobian is at most gtol in
c                   absolute value.
c
c         info = 5  number of calls to fcn has reached or
c                   exceeded maxfev.
c
c         info = 6  ftol is too small. no further reduction in
c                   the sum of squares is possible.
c
c         info = 7  xtol is too small. no further improvement in
c                   the approximate solution x is possible.
c
c         info = 8  gtol is too small. fvec is orthogonal to the
c                   columns of the jacobian to machine precision.
c
c       nfev is an integer output variable set to the number of
c         calls to fcn.
c
c       fjac is an output m by n array. the upper n by n submatrix
c         of fjac contains an upper triangular matrix r with
c         diagonal elements of nonincreasing magnitude such that
c
c                t     t           t
c               p *(jac *jac)*p = r *r,
c
c         where p is a permutation matrix and jac is the final
c         calculated jacobian. column j of p is column ipvt(j)
c         (see below) of the identity matrix. the lower trapezoidal
c         part of fjac contains information generated during
c         the computation of r.
c
c       ldfjac is a positive integer input variable not less than m
c         which specifies the leading dimension of the array fjac.
c
c       ipvt is an integer output array of length n. ipvt
c         defines a permutation matrix p such that jac*p = q*r,
c         where jac is the final calculated jacobian, q is
c         orthogonal (not stored), and r is upper triangular
c         with diagonal elements of nonincreasing magnitude.
c         column j of p is column ipvt(j) of the identity matrix.
c
c       qtf is an output array of length n which contains
c         the first n elements of the vector (q transpose)*fvec.
c
c       wa1, wa2, and wa3 are work arrays of length n.
c
c       wa4 is a work array of length m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... spmpar,enorm,fdjac2,lmpar,qrfac
c
c       fortran-supplied ... abs,amax1,amin1,sqrt,mod
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,iflag,iter,j,l
      real actred,delta,dirder,epsmch,fnorm,fnorm1,gnorm,one,par,
     *     pnorm,prered,p1,p5,p25,p75,p0001,ratio,sum,temp,temp1,
     *     temp2,xnorm,zero
      real spmpar,enorm
      data one,p1,p5,p25,p75,p0001,zero
     *     /1.0e0,1.0e-1,5.0e-1,2.5e-1,7.5e-1,1.0e-4,0.0e0/
c
c     epsmch is the machine precision.
c
      epsmch = spmpar(1)
c
      info = 0
      iflag = 0
      nfev = 0
c
c     check the input parameters for errors.
c
      if (n .le. 0 .or. m .lt. n .or. ldfjac .lt. m
     *    .or. ftol .lt. zero .or. xtol .lt. zero .or. gtol .lt. zero
     *    .or. maxfev .le. 0 .or. factor .le. zero) go to 300
      if (mode .ne. 2) go to 20
      do 10 j = 1, n
         if (diag(j) .le. zero) go to 300
   10    continue
   20 continue
c
c     evaluate the function at the starting point
c     and calculate its norm.
c
      iflag = 1
      call fcn(m,n,x,fvec,iflag)
      nfev = 1
      if (iflag .lt. 0) go to 300
      fnorm = enorm(m,fvec)
c
c     initialize levenberg-marquardt parameter and iteration counter.
c
      par = zero
      iter = 1
c
c     beginning of the outer loop.
c
   30 continue
c
c     print message to let user know that routine is running
c                    added by matt newville july 1992
c
         if ( mod(iter,25) .eq. 0) then
           call messag('                    fitting ...')
         end if
c
c        calculate the jacobian matrix.
c
         iflag = 2
         call fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa4)
         nfev = nfev + n
         if (iflag .lt. 0) go to 300
c
c        if requested, call fcn to enable printing of iterates.
c
         if (nprint .le. 0) go to 40
         iflag = 0
         if (mod(iter-1,nprint) .eq. 0) call fcn(m,n,x,fvec,iflag)
         if (iflag .lt. 0) go to 300
   40    continue
c
c        compute the qr factorization of the jacobian.
c
         call qrfac(m,n,fjac,ldfjac,.true.,ipvt,n,wa1,wa2,wa3)
c
c        on the first iteration and if mode is 1, scale according
c        to the norms of the columns of the initial jacobian.
c
         if (iter .ne. 1) go to 80
         if (mode .eq. 2) go to 60
         do 50 j = 1, n
            diag(j) = wa2(j)
            if (wa2(j) .eq. zero) diag(j) = one
   50       continue
   60    continue
c
c        on the first iteration, calculate the norm of the scaled x
c        and initialize the step bound delta.
c
         do 70 j = 1, n
            wa3(j) = diag(j)*x(j)
   70       continue
         xnorm = enorm(n,wa3)
         delta = factor*xnorm
         if (delta .eq. zero) delta = factor
   80    continue
c
c        form (q transpose)*fvec and store the first n components in
c        qtf.
c
         do 90 i = 1, m
            wa4(i) = fvec(i)
   90       continue
         do 130 j = 1, n
            if (fjac(j,j) .eq. zero) go to 120
            sum = zero
            do 100 i = j, m
               sum = sum + fjac(i,j)*wa4(i)
  100          continue
            temp = -sum/fjac(j,j)
            do 110 i = j, m
               wa4(i) = wa4(i) + fjac(i,j)*temp
  110          continue
  120       continue
            fjac(j,j) = wa1(j)
            qtf(j) = wa4(j)
  130       continue
c
c        compute the norm of the scaled gradient.
c
         gnorm = zero
         if (fnorm .eq. zero) go to 170
         do 160 j = 1, n
            l = ipvt(j)
            if (wa2(l) .eq. zero) go to 150
            sum = zero
            do 140 i = 1, j
               sum = sum + fjac(i,j)*(qtf(i)/fnorm)
  140          continue
            gnorm = amax1(gnorm,abs(sum/wa2(l)))
  150       continue
  160       continue
  170    continue
c
c        test for convergence of the gradient norm.
c
         if (gnorm .le. gtol) info = 4
         if (info .ne. 0) go to 300
c
c        rescale if necessary.
c
         if (mode .eq. 2) go to 190
         do 180 j = 1, n
            diag(j) = amax1(diag(j),wa2(j))
  180       continue
  190    continue
c
c        beginning of the inner loop.
c
  200    continue
c
c           determine the levenberg-marquardt parameter.
c
            call lmpar(n,fjac,ldfjac,ipvt,diag,qtf,delta,par,wa1,wa2,
     *                 wa3,wa4)
c
c           store the direction p and x + p. calculate the norm of p.
c
            do 210 j = 1, n
               wa1(j) = -wa1(j)
               wa2(j) = x(j) + wa1(j)
               wa3(j) = diag(j)*wa1(j)
  210          continue
            pnorm = enorm(n,wa3)
c
c           on the first iteration, adjust the initial step bound.
c
            if (iter .eq. 1) delta = amin1(delta,pnorm)
c
c           evaluate the function at x + p and calculate its norm.
c
            iflag = 1
            call fcn(m,n,wa2,wa4,iflag)
            nfev = nfev + 1
            if (iflag .lt. 0) go to 300
            fnorm1 = enorm(m,wa4)
c
c           compute the scaled actual reduction.
c
            actred = -one
            if (p1*fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2
c
c           compute the scaled predicted reduction and
c           the scaled directional derivative.
c
            do 230 j = 1, n
               wa3(j) = zero
               l = ipvt(j)
               temp = wa1(l)
               do 220 i = 1, j
                  wa3(i) = wa3(i) + fjac(i,j)*temp
  220             continue
  230          continue
            temp1 = enorm(n,wa3)/fnorm
            temp2 = (sqrt(par)*pnorm)/fnorm
            prered = temp1**2 + temp2**2/p5
            dirder = -(temp1**2 + temp2**2)
c
c           compute the ratio of the actual to the predicted
c           reduction.
c
            ratio = zero
            if (prered .ne. zero) ratio = actred/prered
c
c           update the step bound.
c
            if (ratio .gt. p25) go to 240
               if (actred .ge. zero) temp = p5
               if (actred .lt. zero)
     *            temp = p5*dirder/(dirder + p5*actred)
               if (p1*fnorm1 .ge. fnorm .or. temp .lt. p1) temp = p1
               delta = temp*amin1(delta,pnorm/p1)
               par = par/temp
               go to 260
  240       continue
               if (par .ne. zero .and. ratio .lt. p75) go to 250
               delta = pnorm/p5
               par = p5*par
  250          continue
  260       continue
c
c           test for successful iteration.
c
            if (ratio .lt. p0001) go to 290
c
c           successful iteration. update x, fvec, and their norms.
c
            do 270 j = 1, n
               x(j) = wa2(j)
               wa2(j) = diag(j)*x(j)
  270          continue
            do 280 i = 1, m
               fvec(i) = wa4(i)
  280          continue
            xnorm = enorm(n,wa2)
            fnorm = fnorm1
            iter = iter + 1
  290       continue
c
c           tests for convergence.
c
            if (abs(actred) .le. ftol .and. prered .le. ftol
     *          .and. p5*ratio .le. one) info = 1
            if (delta .le. xtol*xnorm) info = 2
            if (abs(actred) .le. ftol .and. prered .le. ftol
     *          .and. p5*ratio .le. one .and. info .eq. 2) info = 3
            if (info .ne. 0) go to 300
c
c           tests for termination and stringent tolerances.
c
            if (nfev .ge. maxfev) info = 5
            if (abs(actred) .le. epsmch .and. prered .le. epsmch
     *          .and. p5*ratio .le. one) info = 6
            if (delta .le. epsmch*xnorm) info = 7
            if (gnorm .le. epsmch) info = 8
            if (info .ne. 0) go to 300
c
c           end of the inner loop. repeat if iteration unsuccessful.
c
            if (ratio .lt. p0001) go to 200
c
c        end of the outer loop.
c
         go to 30
  300 continue
c
c     termination, either normal or user imposed.
c
      if (iflag .lt. 0) info = iflag
      iflag = 0
      if (nprint .gt. 0) call fcn(m,n,x,fvec,iflag)
      return
c
c     last card of subroutine lmdif.
c
      end
      subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa)
c
c  single precision levenberg-marquardt non-linear least square fitting
c  routine with finite difference approximation to the jacobian.
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
      integer m,n,info,lwa
      integer iwa(n)
      real tol
      real x(n),fvec(m),wa(lwa)
      external fcn
c     **********
c
c     subroutine lmdif1
c
c     the purpose of lmdif1 is to minimize the sum of the squares of
c     m nonlinear functions in n variables by a modification of the
c     levenberg-marquardt algorithm. this is done by using the more
c     general least-squares solver lmdif. the user must provide a
c     subroutine which calculates the functions. the jacobian is
c     then calculated by a forward-difference approximation.
c
c     the subroutine statement is
c
c       subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa)
c
c     where
c
c       fcn is the name of the user-supplied subroutine which
c         calculates the functions. fcn must be declared
c         in an external statement in the user calling
c         program, and should be written as follows.
c
c         subroutine fcn(m,n,x,fvec,iflag)
c         integer m,n,iflag
c         real x(n),fvec(m)
c         ----------
c         calculate the functions at x and
c         return this vector in fvec.
c         ----------
c         return
c         end
c
c         the value of iflag should not be changed by fcn unless
c         the user wants to terminate execution of lmdif1.
c         in this case set iflag to a negative integer.
c
c       m is a positive integer input variable set to the number
c         of functions.
c
c       n is a positive integer input variable set to the number
c         of variables. n must not exceed m.
c
c       x is an array of length n. on input x must contain
c         an initial estimate of the solution vector. on output x
c         contains the final estimate of the solution vector.
c
c       fvec is an output array of length m which contains
c         the functions evaluated at the output x.
c
c       tol is a nonnegative input variable. termination occurs
c         when the algorithm estimates either that the relative
c         error in the sum of squares is at most tol or that
c         the relative error between x and the solution is at
c         most tol.
c
c       info is an integer output variable. if the user has
c         terminated execution, info is set to the (negative)
c         value of iflag. see description of fcn. otherwise,
c         info is set as follows.
c
c         info = 0  improper input parameters.
c
c         info = 1  algorithm estimates that the relative error
c                   in the sum of squares is at most tol.
c
c         info = 2  algorithm estimates that the relative error
c                   between x and the solution is at most tol.
c
c         info = 3  conditions for info = 1 and info = 2 both hold.
c
c         info = 4  fvec is orthogonal to the columns of the
c                   jacobian to machine precision.
c
c         info = 5  number of calls to fcn has reached or
c                   exceeded 200*(n+1).
c
c         info = 6  tol is too small. no further reduction in
c                   the sum of squares is possible.
c
c         info = 7  tol is too small. no further improvement in
c                   the approximate solution x is possible.
c
c       iwa is an integer work array of length n.
c
c       wa is a work array of length lwa.
c
c       lwa is a positive integer input variable not less than
c         m*n+5*n+m.
c
c     subprograms called
c
c       user-supplied ...... fcn
c
c       minpack-supplied ... lmdif
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer maxfev,mode,mp5n,nfev,nprint
      real epsfcn,factor,ftol,gtol,xtol,zero
      data factor,zero /1.0e2,0.0e0/
      info = 0
c
c     check the input parameters for errors.
c
      if (n .le. 0 .or. m .lt. n .or. tol .lt. zero
     *    .or. lwa .lt. m*n + 5*n + m) go to 10
c
c     call lmdif.
c
      maxfev = 200*(n + 1)
      ftol = tol
      xtol = tol
      gtol = zero
      epsfcn = zero
      mode = 1
      nprint = 0
      mp5n = m + 5*n
      call lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,wa(1),
     *           mode,factor,nprint,info,nfev,wa(mp5n+1),m,iwa,
     *           wa(n+1),wa(2*n+1),wa(3*n+1),wa(4*n+1),wa(5*n+1))
      if (info .eq. 8) info = 4
   10 continue
      return
c
c     last card of subroutine lmdif1.
c
      end
      subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,wa1,
     *                 wa2)
      integer n,ldr
      integer ipvt(n)
      real delta,par
      real r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa1(n),wa2(n)
c     **********
c
c     subroutine lmpar
c
c     given an m by n matrix a, an n by n nonsingular diagonal
c     matrix d, an m-vector b, and a positive number delta,
c     the problem is to determine a value for the parameter
c     par such that if x solves the system
c
c           a*x = b ,     sqrt(par)*d*x = 0 ,
c
c     in the least squares sense, and dxnorm is the euclidean
c     norm of d*x, then either par is zero and
c
c           (dxnorm-delta) .le. 0.1*delta ,
c
c     or par is positive and
c
c           abs(dxnorm-delta) .le. 0.1*delta .
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then lmpar expects
c     the full upper triangle of r, the permutation matrix p,
c     and the first n components of (q transpose)*b. on output
c     lmpar also provides an upper triangular matrix s such that
c
c            t   t                   t
c           p *(a *a + par*d*d)*p = s *s .
c
c     s is employed within lmpar and may be of separate interest.
c
c     only a few iterations are generally needed for convergence
c     of the algorithm. if, however, the limit of 10 iterations
c     is reached, then the output par will contain the best
c     value obtained so far.
c
c     the subroutine statement is
c
c       subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,
c                        wa1,wa2)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle
c         must contain the full upper triangle of the matrix r.
c         on output the full upper triangle is unaltered, and the
c         strict lower triangle contains the strict upper triangle
c         (transposed) of the upper triangular matrix s.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       delta is a positive input variable which specifies an upper
c         bound on the euclidean norm of d*x.
c
c       par is a nonnegative variable. on input par contains an
c         initial estimate of the levenberg-marquardt parameter.
c         on output par contains the final estimate.
c
c       x is an output array of length n which contains the least
c         squares solution of the system a*x = b, sqrt(par)*d*x = 0,
c         for the output par.
c
c       sdiag is an output array of length n which contains the
c         diagonal elements of the upper triangular matrix s.
c
c       wa1 and wa2 are work arrays of length n.
c
c     subprograms called
c
c       minpack-supplied ... spmpar,enorm,qrsolv
c
c       fortran-supplied ... abs,amax1,amin1,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,iter,j,jm1,jp1,k,l,nsing
      real dxnorm,dwarf,fp,gnorm,parc,parl,paru,p1,p001,sum,temp,zero
      real spmpar,enorm
      data p1,p001,zero /1.0e-1,1.0e-3,0.0e0/
c
c     dwarf is the smallest positive magnitude.
c
      dwarf = spmpar(2)
c
c     compute and store in x the gauss-newton direction. if the
c     jacobian is rank-deficient, obtain a least squares solution.
c
      nsing = n
      do 10 j = 1, n
         wa1(j) = qtb(j)
         if (r(j,j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa1(j) = zero
   10    continue
      if (nsing .lt. 1) go to 50
      do 40 k = 1, nsing
         j = nsing - k + 1
         wa1(j) = wa1(j)/r(j,j)
         temp = wa1(j)
         jm1 = j - 1
         if (jm1 .lt. 1) go to 30
         do 20 i = 1, jm1
            wa1(i) = wa1(i) - r(i,j)*temp
   20       continue
   30    continue
   40    continue
   50 continue
      do 60 j = 1, n
         l = ipvt(j)
         x(l) = wa1(j)
   60    continue
c
c     initialize the iteration counter.
c     evaluate the function at the origin, and test
c     for acceptance of the gauss-newton direction.
c
      iter = 0
      do 70 j = 1, n
         wa2(j) = diag(j)*x(j)
   70    continue
      dxnorm = enorm(n,wa2)
      fp = dxnorm - delta
      if (fp .le. p1*delta) go to 220
c
c     if the jacobian is not rank deficient, the newton
c     step provides a lower bound, parl, for the zero of
c     the function. otherwise set this bound to zero.
c
      parl = zero
      if (nsing .lt. n) go to 120
      do 80 j = 1, n
         l = ipvt(j)
         wa1(j) = diag(l)*(wa2(l)/dxnorm)
   80    continue
      do 110 j = 1, n
         sum = zero
         jm1 = j - 1
         if (jm1 .lt. 1) go to 100
         do 90 i = 1, jm1
            sum = sum + r(i,j)*wa1(i)
   90       continue
  100    continue
         wa1(j) = (wa1(j) - sum)/r(j,j)
  110    continue
      temp = enorm(n,wa1)
      parl = ((fp/delta)/temp)/temp
  120 continue
c
c     calculate an upper bound, paru, for the zero of the function.
c
      do 140 j = 1, n
         sum = zero
         do 130 i = 1, j
            sum = sum + r(i,j)*qtb(i)
  130       continue
         l = ipvt(j)
         wa1(j) = sum/diag(l)
  140    continue
      gnorm = enorm(n,wa1)
      paru = gnorm/delta
      if (paru .eq. zero) paru = dwarf/amin1(delta,p1)
c
c     if the input par lies outside of the interval (parl,paru),
c     set par to the closer endpoint.
c
      par = amax1(par,parl)
      par = amin1(par,paru)
      if (par .eq. zero) par = gnorm/dxnorm
c
c     beginning of an iteration.
c
  150 continue
         iter = iter + 1
c
c        evaluate the function at the current value of par.
c
         if (par .eq. zero) par = amax1(dwarf,p001*paru)
         temp = sqrt(par)
         do 160 j = 1, n
            wa1(j) = temp*diag(j)
  160       continue
         call qrsolv(n,r,ldr,ipvt,wa1,qtb,x,sdiag,wa2)
         do 170 j = 1, n
            wa2(j) = diag(j)*x(j)
  170       continue
         dxnorm = enorm(n,wa2)
         temp = fp
         fp = dxnorm - delta
c
c        if the function is small enough, accept the current value
c        of par. also test for the exceptional cases where parl
c        is zero or the number of iterations has reached 10.
c
         if (abs(fp) .le. p1*delta
     *       .or. parl .eq. zero .and. fp .le. temp
     *            .and. temp .lt. zero .or. iter .eq. 10) go to 220
c
c        compute the newton correction.
c
         do 180 j = 1, n
            l = ipvt(j)
            wa1(j) = diag(l)*(wa2(l)/dxnorm)
  180       continue
         do 210 j = 1, n
            wa1(j) = wa1(j)/sdiag(j)
            temp = wa1(j)
            jp1 = j + 1
            if (n .lt. jp1) go to 200
            do 190 i = jp1, n
               wa1(i) = wa1(i) - r(i,j)*temp
  190          continue
  200       continue
  210       continue
         temp = enorm(n,wa1)
         parc = ((fp/delta)/temp)/temp
c
c        depending on the sign of the function, update parl or paru.
c
         if (fp .gt. zero) parl = amax1(parl,par)
         if (fp .lt. zero) paru = amin1(paru,par)
c
c        compute an improved estimate for par.
c
         par = amax1(parl,par+parc)
c
c        end of an iteration.
c
         go to 150
  220 continue
c
c     termination.
c
      if (iter .eq. 0) par = zero
      return
c
c     last card of subroutine lmpar.
c
      end
      subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
      integer m,n,lda,lipvt
      integer ipvt(lipvt)
      logical pivot
      real a(lda,n),rdiag(n),acnorm(n),wa(n)
c     **********
c
c     subroutine qrfac
c
c     this subroutine uses householder transformations with column
c     pivoting (optional) to compute a qr factorization of the
c     m by n matrix a. that is, qrfac determines an orthogonal
c     matrix q, a permutation matrix p, and an upper trapezoidal
c     matrix r with diagonal elements of nonincreasing magnitude,
c     such that a*p = q*r. the householder transformation for
c     column k, k = 1,2,...,min(m,n), is of the form
c
c                           t
c           i - (1/u(k))*u*u
c
c     where u has zeros in the first k-1 positions. the form of
c     this transformation and the method of pivoting first
c     appeared in the corresponding linpack subroutine.
c
c     the subroutine statement is
c
c       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
c
c     where
c
c       m is a positive integer input variable set to the number
c         of rows of a.
c
c       n is a positive integer input variable set to the number
c         of columns of a.
c
c       a is an m by n array. on input a contains the matrix for
c         which the qr factorization is to be computed. on output
c         the strict upper trapezoidal part of a contains the strict
c         upper trapezoidal part of r, and the lower trapezoidal
c         part of a contains a factored form of q (the non-trivial
c         elements of the u vectors described above).
c
c       lda is a positive integer input variable not less than m
c         which specifies the leading dimension of the array a.
c
c       pivot is a logical input variable. if pivot is set true,
c         then column pivoting is enforced. if pivot is set false,
c         then no column pivoting is done.
c
c       ipvt is an integer output array of length lipvt. ipvt
c         defines the permutation matrix p such that a*p = q*r.
c         column j of p is column ipvt(j) of the identity matrix.
c         if pivot is false, ipvt is not referenced.
c
c       lipvt is a positive integer input variable. if pivot is false,
c         then lipvt may be as small as 1. if pivot is true, then
c         lipvt must be at least n.
c
c       rdiag is an output array of length n which contains the
c         diagonal elements of r.
c
c       acnorm is an output array of length n which contains the
c         norms of the corresponding columns of the input matrix a.
c         if this information is not needed, then acnorm can coincide
c         with rdiag.
c
c       wa is a work array of length n. if pivot is false, then wa
c         can coincide with rdiag.
c
c     subprograms called
c
c       minpack-supplied ... spmpar,enorm
c
c       fortran-supplied ... amax1,sqrt,min0
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j,jp1,k,kmax,minmn
      real ajnorm,epsmch,one,p05,sum,temp,zero
      real spmpar,enorm
      data one,p05,zero /1.0e0,5.0e-2,0.0e0/
c
c     epsmch is the machine precision.
c
      epsmch = spmpar(1)
c
c     compute the initial column norms and initialize several arrays.
c
      do 10 j = 1, n
         acnorm(j) = enorm(m,a(1,j))
         rdiag(j) = acnorm(j)
         wa(j) = rdiag(j)
         if (pivot) ipvt(j) = j
   10    continue
c
c     reduce a to r with householder transformations.
c
      minmn = min0(m,n)
      do 110 j = 1, minmn
         if (.not.pivot) go to 40
c
c        bring the column of largest norm into the pivot position.
c
         kmax = j
         do 20 k = j, n
            if (rdiag(k) .gt. rdiag(kmax)) kmax = k
   20       continue
         if (kmax .eq. j) go to 40
         do 30 i = 1, m
            temp = a(i,j)
            a(i,j) = a(i,kmax)
            a(i,kmax) = temp
   30       continue
         rdiag(kmax) = rdiag(j)
         wa(kmax) = wa(j)
         k = ipvt(j)
         ipvt(j) = ipvt(kmax)
         ipvt(kmax) = k
   40    continue
c
c        compute the householder transformation to reduce the
c        j-th column of a to a multiple of the j-th unit vector.
c
         ajnorm = enorm(m-j+1,a(j,j))
         if (ajnorm .eq. zero) go to 100
         if (a(j,j) .lt. zero) ajnorm = -ajnorm
         do 50 i = j, m
            a(i,j) = a(i,j)/ajnorm
   50       continue
         a(j,j) = a(j,j) + one
c
c        apply the transformation to the remaining columns
c        and update the norms.
c
         jp1 = j + 1
         if (n .lt. jp1) go to 100
         do 90 k = jp1, n
            sum = zero
            do 60 i = j, m
               sum = sum + a(i,j)*a(i,k)
   60          continue
            temp = sum/a(j,j)
            do 70 i = j, m
               a(i,k) = a(i,k) - temp*a(i,j)
   70          continue
            if (.not.pivot .or. rdiag(k) .eq. zero) go to 80
            temp = a(j,k)/rdiag(k)
            rdiag(k) = rdiag(k)*sqrt(amax1(zero,one-temp**2))
            if (p05*(rdiag(k)/wa(k))**2 .gt. epsmch) go to 80
            rdiag(k) = enorm(m-j,a(jp1,k))
            wa(k) = rdiag(k)
   80       continue
   90       continue
  100    continue
         rdiag(j) = -ajnorm
  110    continue
      return
c
c     last card of subroutine qrfac.
c
      end
      subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
      integer n,ldr
      integer ipvt(n)
      real r(ldr,n),diag(n),qtb(n),x(n),sdiag(n),wa(n)
c     **********
c
c     subroutine qrsolv
c
c     given an m by n matrix a, an n by n diagonal matrix d,
c     and an m-vector b, the problem is to determine an x which
c     solves the system
c
c           a*x = b ,     d*x = 0 ,
c
c     in the least squares sense.
c
c     this subroutine completes the solution of the problem
c     if it is provided with the necessary information from the
c     qr factorization, with column pivoting, of a. that is, if
c     a*p = q*r, where p is a permutation matrix, q has orthogonal
c     columns, and r is an upper triangular matrix with diagonal
c     elements of nonincreasing magnitude, then qrsolv expects
c     the full upper triangle of r, the permutation matrix p,
c     and the first n components of (q transpose)*b. the system
c     a*x = b, d*x = 0, is then equivalent to
c
c                  t       t
c           r*z = q *b ,  p *d*p*z = 0 ,
c
c     where x = p*z. if this system does not have full rank,
c     then a least squares solution is obtained. on output qrsolv
c     also provides an upper triangular matrix s such that
c
c            t   t               t
c           p *(a *a + d*d)*p = s *s .
c
c     s is computed within qrsolv and may be of separate interest.
c
c     the subroutine statement is
c
c       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
c
c     where
c
c       n is a positive integer input variable set to the order of r.
c
c       r is an n by n array. on input the full upper triangle
c         must contain the full upper triangle of the matrix r.
c         on output the full upper triangle is unaltered, and the
c         strict lower triangle contains the strict upper triangle
c         (transposed) of the upper triangular matrix s.
c
c       ldr is a positive integer input variable not less than n
c         which specifies the leading dimension of the array r.
c
c       ipvt is an integer input array of length n which defines the
c         permutation matrix p such that a*p = q*r. column j of p
c         is column ipvt(j) of the identity matrix.
c
c       diag is an input array of length n which must contain the
c         diagonal elements of the matrix d.
c
c       qtb is an input array of length n which must contain the first
c         n elements of the vector (q transpose)*b.
c
c       x is an output array of length n which contains the least
c         squares solution of the system a*x = b, d*x = 0.
c
c       sdiag is an output array of length n which contains the
c         diagonal elements of the upper triangular matrix s.
c
c       wa is a work array of length n.
c
c     subprograms called
c
c       fortran-supplied ... abs,sqrt
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c     **********
      integer i,j,jp1,k,kp1,l,nsing
      real cos,cotan,p5,p25,qtbpj,sin,sum,tan,temp,zero
      data p5,p25,zero /5.0e-1,2.5e-1,0.0e0/
c
c     copy r and (q transpose)*b to preserve input and initialize s.
c     in particular, save the diagonal elements of r in x.
c
      do 20 j = 1, n
         do 10 i = j, n
            r(i,j) = r(j,i)
   10       continue
         x(j) = r(j,j)
         wa(j) = qtb(j)
   20    continue
c
c     eliminate the diagonal matrix d using a givens rotation.
c
      do 100 j = 1, n
c
c        prepare the row of d to be eliminated, locating the
c        diagonal element using p from the qr factorization.
c
         l = ipvt(j)
         if (diag(l) .eq. zero) go to 90
         do 30 k = j, n
            sdiag(k) = zero
   30       continue
         sdiag(j) = diag(l)
c
c        the transformations to eliminate the row of d
c        modify only a single element of (q transpose)*b
c        beyond the first n, which is initially zero.
c
         qtbpj = zero
         do 80 k = j, n
c
c           determine a givens rotation which eliminates the
c           appropriate element in the current row of d.
c
            if (sdiag(k) .eq. zero) go to 70
            if (abs(r(k,k)) .ge. abs(sdiag(k))) go to 40
               cotan = r(k,k)/sdiag(k)
               sin = p5/sqrt(p25+p25*cotan**2)
               cos = sin*cotan
               go to 50
   40       continue
               tan = sdiag(k)/r(k,k)
               cos = p5/sqrt(p25+p25*tan**2)
               sin = cos*tan
   50       continue
c
c           compute the modified diagonal element of r and
c           the modified element of ((q transpose)*b,0).
c
            r(k,k) = cos*r(k,k) + sin*sdiag(k)
            temp = cos*wa(k) + sin*qtbpj
            qtbpj = -sin*wa(k) + cos*qtbpj
            wa(k) = temp
c
c           accumulate the tranformation in the row of s.
c
            kp1 = k + 1
            if (n .lt. kp1) go to 70
            do 60 i = kp1, n
               temp = cos*r(i,k) + sin*sdiag(i)
               sdiag(i) = -sin*r(i,k) + cos*sdiag(i)
               r(i,k) = temp
   60          continue
   70       continue
   80       continue
   90    continue
c
c        store the diagonal element of s and restore
c        the corresponding diagonal element of r.
c
         sdiag(j) = r(j,j)
         r(j,j) = x(j)
  100    continue
c
c     solve the triangular system for z. if the system is
c     singular, then obtain a least squares solution.
c
      nsing = n
      do 110 j = 1, n
         if (sdiag(j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa(j) = zero
  110    continue
      if (nsing .lt. 1) go to 150
      do 140 k = 1, nsing
         j = nsing - k + 1
         sum = zero
         jp1 = j + 1
         if (nsing .lt. jp1) go to 130
         do 120 i = jp1, nsing
            sum = sum + r(i,j)*wa(i)
  120       continue
  130    continue
         wa(j) = (wa(j) - sum)/sdiag(j)
  140    continue
  150 continue
c
c     permute the components of z back to components of x.
c
      do 160 j = 1, n
         l = ipvt(j)
         x(l) = wa(j)
  160    continue
      return
c
c     last card of subroutine qrsolv.
c
      end
      real function spmpar(i)
      integer i
      real rmach(3)
ccc-      real function spmpar(i)
ccc-      integer i
c     **********
c
c     function spmpar
c
c***************************************************************
cc     rewritten to eliminate machine dependence of precision 
cc     so as to give the same precision for all machines. for 
cc     feffit, epsilon is set to 1.e-06, the minimum number is 
cc     1.e-30, and the maximum is 1.e+30.
cc     to restore the orginal version, uncomment all lines 
cc     beginning with  "ccc-", and comment out all other lines.
cc                             matt newville     oct 1992
c***************************************************************
c
c     this function provides single precision machine parameters
c     when the appropriate set of data statements is activated (by
c     removing the c from column 1) and all other data statements are
c     rendered inactive. most of the parameter values were obtained
c     from the corresponding bell laboratories port library function.
c
c     the function statement is
c
c       real function spmpar(i)
c
c     where
c
c       i is an integer input variable set to 1, 2, or 3 which
c         selects the desired machine parameter. if the machine has
c         t base b digits and its smallest and largest exponents are
c         emin and emax, respectively, then these parameters are
c
c         spmpar(1) = b**(1 - t), the machine precision,
c
c         spmpar(2) = b**(emin - 1), the smallest magnitude,
c
c         spmpar(3) = b**emax*(1 - b**(-t)), the largest magnitude.
c
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c
c
c
c     **********
ccc-       integer mcheps(2)
ccc-       integer minmag(2)
ccc-       integer maxmag(2)
ccc-       real rmach(3)
ccc-       equivalence (rmach(1),mcheps(1))
ccc-       equivalence (rmach(2),minmag(1))
ccc-       equivalence (rmach(3),maxmag(1))
c
c     machine constants for the ibm 360/370 series,
c     the amdahl 470/v6, the icl 2900, the itel as/6,
c     the xerox sigma 5/7/9 and the sel systems 85/86.
c
c     data rmach(1) / z3c100000 /
c     data rmach(2) / z00100000 /
c     data rmach(3) / z7fffffff /
c
c     machine constants for the honeywell 600/6000 series.
c
c     data rmach(1) / o716400000000 /
c     data rmach(2) / o402400000000 /
c     data rmach(3) / o376777777777 /
c
c     machine constants for the cdc 6000/7000 series.
c
c     data rmach(1) / 16414000000000000000b /
c     data rmach(2) / 00014000000000000000b /
c     data rmach(3) / 37767777777777777777b /
c
c     machine constants for the pdp-10 (ka or ki processor).
c
c     data rmach(1) / "147400000000 /
c     data rmach(2) / "000400000000 /
c     data rmach(3) / "377777777777 /
c
c     machine constants for the pdp-11.
c
c     data mcheps(1),mcheps(2) / 13568,     0 /
c     data minmag(1),minmag(2) /   128,     0 /
c     data maxmag(1),maxmag(2) / 32767,    -1 /
c
c     machine constants for the burroughs 5700/6700/7700 systems.
c
c     data rmach(1) / o1301000000000000 /
c     data rmach(2) / o1771000000000000 /
c     data rmach(3) / o0777777777777777 /
c
c     machine constants for the burroughs 1700 system.
c
c     data rmach(1) / z4ea800000 /
c     data rmach(2) / z400800000 /
c     data rmach(3) / z5ffffffff /
c
c     machine constants for the univac 1100 series.
c
c     data rmach(1) / o147400000000 /
c     data rmach(2) / o000400000000 /
c     data rmach(3) / o377777777777 /
c
c     machine constants for the data general eclipse s/200.
c
c     note - it may be appropriate to include the following card -
c     static rmach(3)
c
c     data minmag/20k,0/,maxmag/77777k,177777k/
c     data mcheps/36020k,0/
c
c     machine constants for the harris 220.
c
c     data mcheps(1),mcheps(2) / '20000000, '00000353 /
c     data minmag(1),minmag(2) / '20000000, '00000201 /
c     data maxmag(1),maxmag(2) / '37777777, '00000177 /
c
c     machine constants for the cray-1.
c
c     data rmach(1) / 0377224000000000000000b /
c     data rmach(2) / 0200034000000000000000b /
c     data rmach(3) / 0577777777777777777776b /
c
c     machine constants for the prime 400.
c
c     data mcheps(1) / :10000000153 /
c     data minmag(1) / :10000000000 /
c     data maxmag(1) / :17777777777 /
c
c     machine constants for the vax-11.
c
ccc-       data mcheps(1) /  13568 /
ccc-       data minmag(1) /    128 /
ccc-       data maxmag(1) / -32769 /
       data rmach(1), rmach(2), rmach(3) /1.e-06,1.e-30,1.e+30 /
       spmpar = rmach(i)
       return
c
ccc-       spmpar = rmach(i)
ccc-       return
c
c     last card of function spmpar.
c
ccc-       end
c end function spmpar
       end
       subroutine fiterr(fcn,nfit,nvar,mfit,mvar,fbest,ftemp,fjac,
     $      alpha,iprint,iounit,istep,x,delta,correl,ierror,iflag)
c 
c     error analysis for a fit using the minpack routines 
c
c     given a subroutine, *fcn*, to generate a fitting function
c     with *nfit* evaluations from a set of *nvar* variables, 
c     with best-fit values *x* and residuals *fbest* determined, 
c     this will return the uncertainties in *x* to *delta*, and 
c     the correlations between the variables in *correl*.
c
c     copyright 1994 university of washington         matt newville
c
c  arguments:
c     fcn     name of subroutine to generate fitting function,    [in]
c             with call statement as for minpack routines :  
c                   call fcn(nfit,nvar,x,f,ier)
c     nfit    number of function evaluations for call to fcn      [in]
c     nvar    number of variables                                 [in]
c     mfit    dimension of arrays for function evaluations        [in]
c     mvar    dimension of arrays for variables                   [in]
c     fbest   array of fit residual for best fit         (mfit)   [in]
c     ftemp   array of fit residuals for constructing    (mfit) [work]
c             jacobian. on output, this is equal to fbest.
c     fjac    array of finite difference jacobian   (mfit,mvar) [work]
c     alpha   curvature and covariance matrix       (mvar,mvar) [work]
c     iprint  integer print flag for debug messages               [in]  
c     iounit  open fortran unit for debug messages                [in] 
c     istep   maximum number of loops in error evaluation         [in]
c     x       array of best fit values for variables     (mvar)   [in]  
c     delta   array of uncertainties for the variables   (mvar)  [out]
c     correl  array of two-variable correlations    (mvar,mvar)  [out]  
c     ierror  integer flag that is non-zero if error bars        [out]
c             cannot be estimated because the curvature 
c             matrix cannot be inverted, so that one or 
c             more of the variables do not affect the fit.
c     iflag   integer array whose elements are 1 if the   (mvar) [out]
c             corresponding variable is suspected of 
c             causing the failure of the inversion of the
c             curvature matrix. these may be null variables.
c
c  required external subprograms:
c     fcn,  gaussj (both subroutines)
c
c     the algorithm here is to construct and invert the nvar x nvar  
c     curvature matrix  alpha, whose elements are found by summing
c     over the elements of the jacobian matrix, fjac:
c        fjac(i,j) = dfvect(i) / dx(j)   (i = 1, nfit; j = 1, nvar)
c     where fvect is the residual array for the fit and dx is a small 
c     change in one variable away from the best-fit solution. then
c        alpha(j,k) = alpha(k,j) 
c                   = sum_(i=1)^nfit (fjac(i,j) * fjac(i,k))
c
c     the inverse of alpha gives the curvature matrix, whose diagonal 
c     elements are used as the uncertainties and whose off-diagonal
c     elements give the correlations between variables. 
c
c--------------------------------------------------------------------
       parameter (mdata = 20, maxpts = 2048)
c       parameter(maxfit = mdata*maxpts )
       integer  mfit,mvar,nfit,nvar,i,k,j,iloop,istep, istepx
       integer  iflag(mvar), ierror, iprint, iounit, ier
c       real     fbest(maxfit), ftemp(maxfit), fjac(maxfit,mvar)
       real     fbest(mfit), ftemp(mfit), fjac(mfit,mvar)
       real     x(mvar), correl(mvar,mvar), alpha(mvar,mvar)
       real     delta(mvar), delx, delxi, sum, tempx
       real     eps, epsdef, epsmin, tiny, small,zero,one,two
       parameter ( epsdef = 1.0e-2, epsmin = 1.0e-6, tiny = 1.0e-12)
       parameter ( small  = 1.0e-12)
       parameter ( zero   = 0.e0  , one    = 1.e0 , two  = 2.e0  )
       external  fcn, gaussj
c 
       if (iprint.ge.2)  write (iounit,*) '>>>> fiterr start'
       istepx= max(1, istep)
       ier   = 0
       ierror= 0
       iloop = 0
 10    continue 
       iloop = iloop  + 1
c 
c     construct jacobian using the best possible guess for the 
c     relative error in each variable to evaluate the derivatives.  
c     if not available, use 1% of the value for the variable.  
c
       do 50 j = 1, nvar
          tempx  = x(j)
          if ((iloop .gt. 1) .and. (abs(tempx) .gt. tiny)) then
             eps = max( delta(j) / ( two*abs(tempx)) ,  epsmin )
          else
             eps = epsdef
          end if
          delx  = eps * abs(tempx)
          if (delx .eq. zero) delx = eps
          delxi =  one  / delx 
          x(j)  = tempx + delx
          call fcn(nfit, nvar, x, ftemp, ier)
          if (ier .lt. 0) go to 65
          do 30 i = 1, nfit
             fjac(i,j) = ( fbest(i) - ftemp(i)) * delxi
 30       continue
          x(j)  = tempx
 50    continue
 65    continue
c
c   re-evaluate best-fit to restore any common block stuff
c
       call fcn(nfit,nvar,x,ftemp,ier)
c     
c     collect the symmetric curvature matrix, store in alpha 
c
       if (iprint.ge.2)  then
          write (iounit,*) '   curvature matrix:'
          write (iounit,*) '     j , k , alpha(j,k)'
       end if
       do 180 j = 1, nvar
          do 160 k = 1, j
             sum  = zero
             do 140 i = 1, nfit
                sum = sum  + fjac(i,j) * fjac(i,k)
 140         continue
             alpha(j,k) = sum
             if (k.ne.j) alpha(k,j) = sum
             if (iprint.ge.2)  write (iounit,*) j, k, alpha(j,k)
 160      continue
 180   continue
c     
c     invert alpha to give the covariance matrix.  gaussj does
c     gauss-jordan elimination which will die if the matrix is
c     singular. although more efficient versions of this method
c     exist, in the event of a singular matrix, this one will
c     preserve the original matrix and set ier to 1.
c     
       ier   = 0
       call gaussj(alpha,nvar,mvar,ier)
c      
c     if alpha could not be inverted, flag those variables with 
c     small diagonal components of alpha - these are the likely 
c     null variables that caused the matrix inversion to fail.
c
       if (ier.ge.1) then
          ierror = 1
          if (iprint.ge.2) write(iounit,*)
     $             '!!!inversion failed!!!     i, alpha(i,i) '
          do 250 i = 1, nvar
             iflag(i) = 0
             if (iprint.ge.2)  write(iounit,*) i, alpha(i,i)
             if (abs(alpha(i,i)).le. small)  iflag(i) = 1
 250      continue
          return
       end if
c     
c     alpha now contains the covariance matrix, and is easily
c     converted into delta, the uncertainty for each variable,
c     and correl, the two-variable correlation matrix.
c     
       if (iprint.ge.2)  then
          write (iounit,*) ' fiterr done with loop = ',iloop
          write (iounit,*) '     j , delta(j)'
       end if
       do 360 i = 1, nvar
          delta(i) = max(tiny, sqrt( abs( alpha(i,i)) ))
          if (iprint.ge.2) write (iounit,*) i, delta(i)
          do 330 j = 1, i 
             correl(j,i) =  alpha(j,i) / (delta(i) * delta(j))
             correl(i,j) = correl(j,i) 
 330      continue
 360   continue
c     
c     try it a second time with better estimates for the values
c     of deltax for the derivatives to get the jacobian matrix.
c     
       if ( iloop .lt. istepx ) go to 10
c     
c     finished 
c
       if (iprint.ge.2)  write (iounit,*) '>>>> fiterr done'
       return
c     end routine fiterr
       end
       subroutine gaussj(a, n, ma, ier) 
c
c     gauss-jordan elimination to invert a matrix. 
c          based on a routine in 'numerical recipes' by
c          press, flannery, teukolsky, and vetterling.
c  inputs :
c    a     inverted matrix if ier = 0    
c          input matrix    if ier = 1
c    ier   error code, set to 1 if matrix cannot be inverted
c
c                                              matt newville
c--------------------------------------------------------------
       parameter (nmax = 100)
       parameter (zero = 0., one = 1.)
       dimension a(ma, ma) , asav(nmax, nmax)
       dimension ipiv(nmax), indxr(nmax),indxc(nmax) 
c     
       ier = 0
       if ( (n.gt.nmax) .or. (n.gt.ma)) then
          call messag('  gaussj error: matrix too big ')
          ier  = 1
          return
       end if
c  initialize ipiv, and keep a spare version 
c  of the input matrix around just in case
       do 30 i = 1, n
          ipiv(i) = 0 
          do 20 j = 1, n
             asav(j,i) = a(j,i)
 20       continue    
 30    continue
c     
c  main loop over the columns to be reduced
c
       do 300 i = 1, n 
          big = zero
c                                   search for a pivot element
          do 120 j = 1, n 
             if (ipiv(j).ne.1) then 
                do 100 k = 1, n 
                   if (ipiv(k).eq.0) then 
                      if ( abs(a(j,k)) .ge. big) then 
                         big  = abs(a(j,k)) 
                         irow = j 
                         icol = k 
                      endif 
                   elseif (ipiv(k).gt.1) then 
                      ier = 1
                      go to 500
                   endif 
 100            continue 
             endif 
 120      continue 
          ipiv(icol) = ipiv(icol) + 1  
c                                    a pivot has been found
          if (irow.ne.icol) then 
             do 160 l = 1, n 
                dum        = a(irow, l) 
                a(irow, l) = a(icol, l) 
                a(icol, l) = dum 
 160         continue 
          endif 
c     divide the pivot row by the pivot element
          indxr(i) = irow 
          indxc(i) = icol 
          if (a(icol, icol).eq.zero) then
             ier = 1
             go to 500
          end if 
          pivinv       = one / a(icol, icol) 
          a(icol,icol) = one 
          do 200 l = 1, n 
             a(icol, l) = a(icol, l) * pivinv 
 200      continue 
c     reduce the rows except for the pivot one
          do 250 ll = 1, n 
             if (ll.ne.icol) then 
                dum        = a(ll, icol) 
                a(ll,icol) = zero 
                do 220 l = 1, n 
                   a(ll,l) = a(ll,l) - a(icol,l) * dum 
 220            continue 
             endif 
 250      continue 
 300   continue 
c
c   unscramble the solution, by interchanging column pairs
c   in the reverse order that the permutation was done
c
       do 400 i = n, 1, -1 
          if (indxr(i) .ne. indxc(i)) then 
             do 350 j = 1, n 
                dum           = a(j,indxr(i)) 
                a(j,indxr(i)) = a(j,indxc(i)) 
                a(j,indxc(i)) = dum 
 350         continue 
          endif 
 400   continue 
c     
c     if any errors happened, restore original matrix
c     
 500   continue
       if (ier.ne.0) then 
          do 540 i = 1, n
             do 520 j = 1, n
                a(j,i) = asav(j,i)
 520         continue    
 540      continue
       end if
c     
       return 
c  end subroutine gaussj
       end 
      logical function isdata(nicol, nwdx, nwds, words)

      character*(*) words(nwdx)
      logical isnum
      external isnum

      n = min(nicol, nwds)
      do 10 i=1,n
        if (.not.isnum(words(i))) then
            isdata = .false.
            return 
        endif
 10   continue 
      isdata = .true.

      return 
c  end logical function isdata
      end
        subroutine intmem(x,y,npts,nterms,istart,xin,yout)
c
c       interpolation routine from bevington's book -- with "memory"
c
c     istart is the value of i1 that intmem "remembers" from the last time
c     it was called.  this speeds up multiple calls. ilast should be set
c     to 1 before the first call of interp in a sequence of calls
c     this *will* break if you do not call intmem for increasing values
c     of xin 
c                                       --br.  11/24/93
c
c        real deltax,delta,a,prod,sum
        dimension x(*),y(*)
        dimension delta(10),a(10)
c
c        search for  appropriate value of x(1)
c
C%%%  11      do 19 i=1,npts
 11     do 19 i=istart,npts
        if(xin-x(i)) 13,17,19
13      i1=i-nterms/2
        if(i1) 15,15,21
15      i1=1
        go to 21
17      yout=y(i)
18      go to 61
19      continue
        i1=npts-nterms+1
21      i2=i1+nterms-1
        if(npts-i2) 23,31,31
23      i2=npts
        i1=i2-nterms+1
25      if(i1) 26,26,31
26      i1=1
27      nterms=i2-i1+1
c
c        evaluate deviations delta
c
31      denom=x(i1+1)-x(i1)
        deltax=(xin-x(i1))/denom
        do 35 i=1,nterms
        ix=i1+i-1
35      delta(i)=(x(ix)-x(i1))/denom
c
c        accumulate coefficients a
c
40      a(1)=y(i1)
41      do 50 k=2,nterms
        prod=1.
        sum=0.
        imax=k-1
        ixmax=i1+imax
        do 49 i=1,imax
        j=k-i
        prod=prod*(delta(k)-delta(j))
49      sum=sum-a(j)/prod
50      a(k)=sum+y(ixmax)/prod
c
c        accumulate sum of expansion
c
51      sum=a(1)
        do 57 j=2,nterms
        prod=1.
        imax=j-1
        do 56 i=1,imax
56      prod=prod*(deltax-delta(i))
57      sum=sum+a(j)*prod
60      yout=sum
        istart = i1
61      return
c end subroutine interp
        end

       subroutine decod(icode, micode, consts, values,  nvpts,
     $                  mvpts, mvects, defval,  nvout, outval)
c
c  copyright 1993  university of washington      matt newville
c
c   this decodes the icode array, inserting the values in consts,
c   and values when necessary, performing one- and two-component 
c   math operations (as well as any specially defined functions)
c   and returns the calulated parameter to outval.
c   the default value, defval, will be returned if icode is empty.
c
c  input:
c    icode     integer array containing coded math expression
c    micode    dimension of icode
c    consts    real array of the constant numerical values
c    values    real array of vector values (mvpts,mvects)
c    nvpts     number of points in each values array  (mvects)
c    mvpts     max no of array elements in values and nvpts
c    mvects    max no of vectors (number of values)
c    defval    default value for the parameter        (mvpts)
c    nvout     number of points in defval
c  output:
c    nvout     number of points in outval
c    outval    output value for the parameter         (mvpts)
c  note:
c    mvpts cannot exceed maxpts
c---------------------------------------------------------------------
       implicit double precision(a-h,o-z)
       integer    micode, mvpts, mvects, nvout
       integer    mstack, jconst, maxpts, jindex
       parameter  ( maxpts = 2048, mstack = 40)
       parameter  ( jconst = 2000, jindex = 2*jconst -1 )
       integer    icode(micode), nvpts(mvpts)
       double precision       consts(*), values(mvpts, mvects)
       double precision       defval(mvpts), outval(mvpts)
       double precision       x(maxpts, mstack), zero, one
       parameter  ( zero = 0., one = 1.)
       integer    nx(mstack)
       integer    ierr, iplace, istack, ic, i, j

c      include 'ints.h'
       integer   iexp, ilog, isqrt, isin, icos, itan, iabs, ineg
       integer   iasin, iacos, iatan, isinh, icosh, itanh
c -- new in phit
       integer   icot, isec, icsc, iacot, iasec, iacsc
       integer   icoth, isech, icsch, iacoth, iasech, iacsch
       integer   ilog10, ifact, iint, inint
c -- new in phit
       integer   iadd, isub, imul, idiv, iy2x
       integer   jadd, jsub, jmin, jmax, jeins
       integer   jstep, jgauss, jlor, jpeak
       parameter(iexp  = -10, ilog  = -11, isqrt = -12, isin  = -13,
     $           icos  = -14, itan  = -15, iasin = -16, iacos = -17,
     $           iatan = -18, iabs  = -19, ineg  = -20,
     $           isinh = -23, icosh = -24,
     $           itanh = -25, icoth = -26, isech = -27, icsch = -28,
     $           icot  = -29, isec  = -30, icsc  = -31,
     $           iacot = -32, iasec = -33, iacsc = -34,
     $           iacoth= -35, iasech= -36, iacsch= -37,
     $           ilog10= -38, ifact = -39, iint  = -40, inint = -41,
     $           iadd  = -50, isub  = -51, imul  = -52,
     $           idiv  = -53, iy2x  = -54, jadd  =-111, jsub  =-112,
     $           jmin  =-55,  jmax  = -56,
     $           jeins =-121, jstep =-122, jgauss=-123, jlor  =-124,
     $           jpeak =-125)



c---------------------------------------------------------------------
c-- return default if the icode is empty
       if (mvpts.gt.maxpts) then
          ierr = 5
          go to 1000
       endif
       if (nvout.le.0) nvout = 1
       do 20 i = 1, nvout 
          outval(i) = defval(i)
 20    continue
       if (icode(1).eq.0)  goto 9999
c-- initialize stack
       do 50 i = 1, mstack
          nx(i)  = 0
          x(1,i) = zero
          x(2,i) = zero
  50   continue
c-- interpret next object, do operations, and manage stack
c                  hold place in icode array with iplace
       ierr   = 0
       iplace = 0
       istack = 0
 100   continue
          iplace = iplace + 1
          if (iplace.gt.micode)   ierr = 2
          ic = icode(iplace)
          if (ic.eq.0) then
             goto 9999
c-- if number, then push everything in stack
          elseif (ic.ge.1) then
             istack = istack + 1
             if (istack.ge.mstack)     ierr = 1
             do 200 i = istack, 2, -1
                nx(i) = nx(i-1)
                do 180 j = 1, nx(i) 
                   x(j,i)  = x(j,i-1)
 180            continue
 200         continue
             if (ic.le.jconst)  then
                nx(1)   = max(1, min(maxpts, nvpts(ic)))
                do 300 j = 1, nx(1) 
                   x(j,1)  = values(j,ic)
 300            continue
             elseif (ic.eq.jindex) then
                nx(1) = maxpts
                do 320 j = 1, maxpts
                   x(j,1) = j * one 
 320            continue 
             elseif (ic.gt.jconst) then
                x(1,1)  = consts(ic - jconst)
                nx(1)   = 1
             endif   
c-- one-component math: overwrite x(1)  
c                       don't change the rest of the stack or nx(1)
          elseif ( (ic.le.-10).and.(ic.ge.-49) ) then
             call f1mth(x(1,1),nx(1),ic,ierr)
c-- two-component math: overwrite x(1), drop x(2),  
c                       drop stack by 1, update nx(1)
c                                       /--- made gt -100 by BR
          elseif ( (ic.le.-50).and.(ic.gt.-100) ) then
             call f2mth( x(1,1), nx(1), x(1,2), nx(2), ic, ierr)
             call stack(x,maxpts,mstack,nx,istack,1)
c-- special math operations:
c -- new for phit
          elseif ( (ic.eq.jeins) ) then
             call eins(x(1,1), nx(1),x(1,2),nx(2),x(1,3),nx(3),ierr)
             call stack(x,maxpts,mstack,nx,istack,2)
          elseif ( (ic.eq.jstep) ) then
              call step(x(1,1), nx(1), x(1,2), nx(2), ierr)
              call stack(x,maxpts,mstack,nx,istack,1)
          elseif ( (ic.eq.jgauss) ) then
              call gauss(x(1,1),nx(1),x(1,2),nx(2),x(1,3),nx(3),ierr)
              call stack(x,maxpts,mstack,nx,istack,2)
          elseif ( (ic.eq.jlor) ) then
              call lor(x(1,1),nx(1),x(1,2),nx(2),x(1,3),nx(3),ierr)
              call stack(x,maxpts,mstack,nx,istack,2)
          elseif ( (ic.eq.jpeak) ) then
              call peak(x(1,1), nx(1),x(1,2),nx(2),x(1,3),nx(3),
     $                  x(1,4), nx(4),x(1,5),nx(5),x(1,6),nx(6),
     $                  x(1,7), nx(7),x(1,8),nx(8),x(1,9),nx(9),
     $                  x(1,10),nx(10),ierr)
              call stack(x,maxpts,mstack,nx,istack,9)
c -- new for phit
c-- one-component vector operations: max, min value of an array,
c         sum or product of all elements in a single array
          elseif (ic .le.-200) then
             call v1mth(x(1,1),nx(1),ic,ierr)
          else
             ierr = 3
          end if
c-- done : if no errors, then update param and go to next object
          nvout = nx(1)       
          do 800 i = 1, nx(1)
             outval(i) = x(i,1)
 800      continue
          if (ierr.ne.0) go to 1000
       go to 100
c--  error handling
1000   continue
       if (ierr.eq.ilog) then
         call messag( '   math error: log(x) must have x > 0 ')
       elseif (ierr.eq.isqrt) then
         call messag( '   math error: sqrt(x) cannot have x < 0')
       elseif (ierr.eq.iasin) then
         call messag( '   math error: asin(x) must have (-1 < x < 1)')
       elseif (ierr.eq.iacos) then
         call messag( '   math error: acos(x) must have (-1 < x < 1)')
c -- new for phit
       elseif (ierr.eq.icoth) then
         call messag( '   math error: coth(x) is singular at 0')
       elseif (ierr.eq.icsch) then
         call messag( '   math error: csch(x) is singular at 0')
       elseif (ierr.eq.icot) then
         call messag( '   math error: cot(x) is singular at integer '//
     $                 'multiples of pi/2')
       elseif (ierr.eq.isec) then
         call messag( '   math error: sec(x) is singular at odd '//
     $                 'integer multiples of pi/2')
       elseif (ierr.eq.icsc) then
         call messag( '   math error: csc(x) is singular at integer '//
     $                 'multiples of pi')
       elseif (ierr.eq.ilog10) then
         call messag( '   math error: log10(x) must have x > 0 ')
       elseif (ierr.eq.ifact) then
         call messag( '   math error: fact(x) floating overflow '//
     $                 'for requested x')
c -- new for phit
       elseif (ierr.eq.idiv) then
         call messag( '   math error: divide by 0 ')
       elseif (ierr.eq.iy2x) then
         call messag( '   math error: invalid exponentiation ')
       elseif (ierr.eq.1) then
         call messag( '   param died: exceeded stack size! call matt')
       elseif (ierr.eq.2) then
         call messag( '   param died: too many objects!  call matt')
       elseif (ierr.eq.3) then
         call messag( '   param died: unknown operation!  call matt')
       elseif (ierr.eq.5) then
         call messag( '   param died: mvpts .gt. maxpts!  call matt')
       else
         call messag( '   param died: impossible!   call matt')
       end if
       if (ierr.gt.0) then
           j = 0
1500       continue
              j =  j + 1
              if (icode(j).ne.0)  then
                 print*, icode(j)
                 go to 1500
              end if
              stop
       end if
       go to 100

 9999  continue 
       return
c end subroutine decod
       end
c-------------------------------------------------------------------
       integer function nptstk( n1, n2 )
c  determine number of vector points resulting from a 
c  two-component math operation:
c     n1 = min(n1, n2) unless either n1 or n2 = 1, which
c     means one of the components is a constant, and 
c     should be applied to all elements of vector 2
c
       integer n1, n2
       nptstk = min ( n1, n2 )
       if ( (n1.le.1).or.(n2.le.1) ) nptstk = max ( n1, n2 )
       return
c  end function nptstk
       end
c-------------------------------------------------------------------
       subroutine f1mth( x, nx, iop, ierr)
c
c  copyright 1993  university of washington      matt newville
c
c one component math. if any tests are failed, x is returned.
c iop is an integer indication of which operation to perform.
c
       implicit double precision(a-h,o-z)
       double precision        x(*), xexp, zero, one, fifty, big
       integer     iop, ierr, nx, i

c      include 'ints.h'
       integer   iexp, ilog, isqrt, isin, icos, itan, iabs, ineg
       integer   iasin, iacos, iatan, isinh, icosh, itanh
c -- new in phit
       integer   icot, isec, icsc, iacot, iasec, iacsc
       integer   icoth, isech, icsch, iacoth, iasech, iacsch
       integer   ilog10, ifact, iint, inint
c -- new in phit
       integer   iadd, isub, imul, idiv, iy2x
       integer   jadd, jsub, jmin, jmax, jeins
       integer   jstep, jgauss, jlor, jpeak
       parameter(iexp  = -10, ilog  = -11, isqrt = -12, isin  = -13,
     $           icos  = -14, itan  = -15, iasin = -16, iacos = -17,
     $           iatan = -18, iabs  = -19, ineg  = -20,
     $           isinh = -23, icosh = -24,
     $           itanh = -25, icoth = -26, isech = -27, icsch = -28,
     $           icot  = -29, isec  = -30, icsc  = -31,
     $           iacot = -32, iasec = -33, iacsc = -34,
     $           iacoth= -35, iasech= -36, iacsch= -37,
     $           ilog10= -38, ifact = -39, iint  = -40, inint = -41,
     $           iadd  = -50, isub  = -51, imul  = -52,
     $           idiv  = -53, iy2x  = -54, jadd  =-111, jsub  =-112,
     $           jmin  =-55,  jmax  = -56,
     $           jeins =-121, jstep =-122, jgauss=-123, jlor  =-124,
     $           jpeak =-125)


       parameter (  zero = 0., one = 1., fifty = 50. )
       parameter (  big = 1.d30, tiny = 1.d-12 )

       ierr  = 0
       if (iop.eq.iexp) then
          do 20 i = 1, nx
             xexp  = max(-fifty, min(x(i), fifty) )
             x(i)  = exp(xexp)
 20       continue 
       elseif (iop.eq.ilog) then
          do 40 i = 1, nx
             if  ( x(i).gt.zero ) then
                x(i) = log(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 40       continue 
       elseif (iop.eq.isqrt) then
          do 60 i = 1, nx
             if  ( x(i).ge.zero ) then
                x(i) = sqrt(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 60       continue 
       elseif (iop.eq.iabs)  then
          do  80 i = 1, nx
             x(i) = abs(x(i))
 80       continue 
       elseif (iop.eq.ineg)  then
          do 100 i = 1, nx
             x(i) = - x(i)
 100      continue 
       elseif (iop.eq.isin)  then
          do 120 i = 1, nx
             x(i) = sin(x(i))
 120      continue 
       elseif (iop.eq.icos)  then
          do 140 i = 1, nx
             x(i) = cos(x(i))
 140      continue 
       elseif (iop.eq.itan)  then
          do 160 i = 1, nx
             x(i) = tan(x(i))
 160      continue 
       elseif (iop.eq.iasin) then
          do 180 i = 1, nx
             if (abs(x(i)).le.one) then
                x(i) = asin(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 180      continue 
       elseif (iop.eq.iacos) then
          do 200 i = 1, nx
             if (abs(x(i)).le.one) then
                x(i) = acos(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 200      continue 
       elseif (iop.eq.iatan) then
          do 220 i = 1, nx
             x(i) = atan(x(i))
 220      continue 
       elseif (iop.eq.itanh) then
          do 240 i = 1, nx
             x(i) = tanh(x(i))
 240      continue 
       elseif (iop.eq.icosh) then
          do 260 i = 1, nx
             xexp  = max(-fifty, min(x(i), fifty) )
             x(i) = cosh(xexp)
 260      continue 
       elseif (iop.eq.isinh) then
          do 280 i = 1, nx
             xexp  = max(-fifty, min(x(i), fifty) )
             x(i) = sinh(xexp)
 280      continue 
c --- new for phit
       elseif (iop.eq.icoth) then
          do 290 i = 1, nx
             x(i) = tanh(x(i))
             if (abs(x(i)).lt.tiny) then
                 x(i) = sign(big, x(i))
                 ierr = iop
             else
                 x(i) = 1 / x(i)
             endif
 290      continue 
       elseif (iop.eq.isech) then
          do 295 i = 1, nx
             xexp = max(-fifty, min(x(i), fifty) )
             x(i) = cosh(xexp)
             x(i) = 1 / x(i)
 295       continue 
       elseif (iop.eq.icsch) then
          do 300 i = 1, nx
             xexp = max(-fifty, min(x(i), fifty) )
             x(i) = sinh(xexp)
             if (abs(x(i)).lt.tiny) then
                 x(i) = sign(big, x(i))
                 ierr = iop
             else
                 x(i) = 1 / x(i)
             endif
 300      continue 
       elseif (iop.eq.icot) then
          do 310 i = 1, nx
            if (abs(cos(x(i))).lt.tiny) then
                x(i) = sign(big, x(i))
                ierr = iop
            else
                x(i) = tan(x(i))
            endif
            if (abs(x(i)).lt.tiny) then
                x(i) = sign(big, x(i))
                ierr = iop
            else
                x(i) = 1 / x(i)
            endif
 310      continue 
       elseif (iop.eq.isec) then
          do 320 i = 1, nx
             x(i) = cos(x(i))
             if (abs(x(i)).lt.tiny) then
                 x(i) = sign(big, x(i))
                 ierr = iop
             else
                 x(i) = 1 / x(i)
             endif
 320      continue 
       elseif (iop.eq.icsc) then
          do 330 i = 1, nx
             x(i) = sin(x(i))
             if (abs(x(i)).lt.tiny) then
                 x(i) = sign(big, x(i))
                 ierr = iop
             else
                 x(i) = 1 / x(i)
             endif
 330      continue 
       elseif (iop.eq.ilog10) then
          do 340 i = 1, nx
             if  ( x(i).gt.zero ) then
                x(i) = log10(x(i))
             else
                ierr  = iop
                x(i) = zero
             end if
 340      continue 
       elseif (iop.eq.iint) then
          do 350 i = 1, nx
             x(i) = dble(int(x(i)))
 350      continue 
       elseif (iop.eq.inint) then
          do 360 i = 1, nx
             x(i) = dble(nint(x(i)))
 360      continue 
       elseif (iop.eq.ifact) then
          do 370 i = 1, nx
            if (x(i).gt.big) then
                ierr = iop
                x(i) = 0
            else
                nnn  = nint(x(i))
                x(i) = 1
                do 365 j = 1, nnn
                  x(i) = x(i) * dble(j)
 365            continue 
            endif
 370      continue 
c --- new for phit
       end if
       return
c end subroutine f1mth
       end
c-------------------------------------------------------------------
       subroutine f2mth( x, nx, y, ny, iop, ierr)
c
c  copyright 1993  university of washington      matt newville
c
c two component math: x is overwritten by operator(x,y).
c if ( (negative number)**(fraction) ) is requested, x is unchanged.
c
c iop is an integer indication of which operation to perform.
c if iop = 0, then x is returned.
c
       implicit double precision(a-h,o-z)
       integer  maxpts, ix, iy, i
       parameter (maxpts = 2048)
       double precision      x(*), y(*), xout(maxpts)
       double precision      zero, one, fifty, xtmp, test
       integer   nx, ny, iop, ierr, nx1, ny1, nptstk
       integer   iadd, isub, imul, idiv, iy2x, newx
       integer   jadd, jsub, jmin, jmax
       parameter (iadd = -50, isub = -51, imul  = -52)
       parameter (jadd =-111, jsub =-112, jmin =-55, jmax =-56)
       parameter (idiv = -53, iy2x  = -54 )
       parameter (zero = 0., one = 1., fifty = 50. * one )
       external   nptstk
       ierr = 0
c decide number of points to write out 
c     npstk = smaller of nx1, ny1 unless either of them is 1, 
c             in which case it is the larger of the two. 
c             (n = 1 implies a scalar )
       nx1  = nx
       ny1  = ny
       nx   = nptstk (nx1, ny1)
       if ((iop.eq.iadd).or.(iop.eq.jadd)) then
          do 20 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = y(iy) + x(ix)
 20       continue 
       elseif ((iop.eq.isub).or.(iop.eq.jsub)) then
          do 40 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = y(iy) - x(ix)
 40       continue 
       elseif (iop.eq.imul) then
          do 60 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = y(iy) * x(ix)
 60       continue 
       elseif (iop.eq.idiv) then
          do 80 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             if (x(ix).eq.zero)  then
                xout(i) = zero
                ierr  = iop
             else
                xout(i) = y(iy) / x(ix)
             end if
 80       continue 
       elseif (iop.eq.iy2x) then
          do 100 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             newx = int( x(ix) )
             xtmp = float(newx)
             if (x(ix).eq.zero)  then
                xout(i) = one
             elseif ( (y(iy).eq.zero).and.(x(ix).gt.zero) ) then
                xout(i) = zero
             elseif (y(iy).gt.zero)  then
                test  = x(ix) * log(y(iy))
                if (test.gt.fifty) then
                   xout(i) = exp(fifty)
                elseif (test.lt.(-fifty)) then
                   xout(i) = exp(-fifty)
                else
                   xout(i) = y(iy)**x(ix)
                end if
             elseif ( (y(iy).lt.zero).and.(xtmp.eq.x(ix)) ) then
                test  = xtmp * log(-y(iy))
                if (test.gt.fifty) then
                   xout(i) = exp(fifty)
                elseif (test.lt.(-fifty)) then
                   xout(i) = exp(-fifty)
                else
                   xout(i) = y(iy)**newx
                end if
             else
                ierr  = iop
                xout(i) = zero
             end if
 100      continue 
       elseif (iop.eq.jmin) then
          do 120 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = min( y(iy) , x(ix) )
 120      continue 
       elseif (iop.eq.jmax) then
          do 140 i = 1, nx
             ix = min(nx1, i)
             iy = min(ny1, i)
             xout(i) = max( y(iy) , x(ix) )
 140      continue 
       end if
c now overwrite x
       do 300 i = 1, nx
          x(i) = xout(i)
 300   continue 
       return
c end subroutine f2mth
       end
c-------------------------------------------------------------------
       subroutine v1mth( x, nx, iop, ierr)
c
c  copyright 1994  university of washington      matt newville
c
c one component math on a vector. 
c if any tests are failed, x is returned.
c iop is an integer indication of which operation to perform.
c
       implicit double precision(a-h,o-z)
       double precision        x(*), zero, one, xtmp
       integer     iop, ierr, nx, i
       integer    jvmax, jvmin, jvsum, jvprod, jvnpts
       parameter (jvmax  =-251,  jvmin  =-252, jvnpts = -253,
     $            jvsum  =-255,  jvprod =-256 )
       parameter (  zero = 0., one = 1.)
       ierr  = 0
       if (nx.le.0) nx = 1
       if (iop.eq.jvnpts) then
          xtmp = nx * one
       elseif (iop.eq.jvmax) then
          xtmp  = x(1)
          do 20 i = 1, nx
             xtmp = max(xtmp, x(i))
 20       continue 
       elseif (iop.eq.jvmin)  then
          xtmp  = x(1)
          do 40 i = 1, nx
             xtmp = min(xtmp, x(i))
 40       continue 
       elseif (iop.eq.jvsum)  then
          xtmp  = zero
          do 60 i = 1, nx
             xtmp = xtmp + x(i)
 60       continue 
       elseif (iop.eq.jvprod)  then
          xtmp  = one
          do 80 i = 1, nx
             xtmp = xtmp * x(i)
 80       continue 
       end if
c final array
       do 200 i = 2, nx
          x(i) = zero
 200   continue 
       x(1) = xtmp
       nx   = 1
       return
c end subroutine v1mth
       end
c-------------------------------------------------------------------
       subroutine stack(x,mpts,mstack,npts,istack,ipop)
c
c   drop the stack held in x by idrop. (ipop = 1 for 2 component math)
c   copyright 1993  university of washington      matt newville
c   
       implicit double precision(a-h,o-z)
       integer  mpts, mstack, istack, ipop, i, j, imax
       integer  npts(mstack)
       double precision     x(mpts, mstack), zero
       parameter ( zero = 0.)
c
       istack = istack - ipop
       imax   = max(1, min(mstack, istack + ipop))
       do 100 i = 2, imax
          if (i .gt. istack) then
             npts(i) = 0
             x(1,i)  = zero
          else
             npts(i) = max(1, min(mpts,npts(i+ipop)))
             do 50 j = 1, npts(i)
                x(j, i)  = x(j, i + ipop)
  50         continue
          end if
 100   continue
       return
c  end subroutine stack
       end
c-------------------------------------------------------------------
       subroutine enchk(iparen, strout, itemp, ieqn, ierr)
c
c   some syntax checking for encod routine
c
       integer       maxlen, ileft, iright, icomma
c       integer       jmin, jmax, jdebye, jeins
       parameter ( maxlen = 100)
       parameter ( ileft =  -6, iright =  -7, icomma = -8   )
c       parameter (jmin =-115, jmax  =-116, jdebye =-120, jeins =-121)
       character*70  strout, errmsg, strtmp
       integer       itemp(maxlen), ieqn, ilen, istrln, iparen
       integer       ierr, i, ibfr, iaft, it, ii
       integer       j, jt, jstack, jcomma
       logical       strok
c      include 'ints.h'
       integer   iexp, ilog, isqrt, isin, icos, itan, iabs, ineg
       integer   iasin, iacos, iatan, isinh, icosh, itanh
c -- new in phit
       integer   icot, isec, icsc, iacot, iasec, iacsc
       integer   icoth, isech, icsch, iacoth, iasech, iacsch
       integer   ilog10, ifact, iint, inint
c -- new in phit
       integer   iadd, isub, imul, idiv, iy2x
       integer   jadd, jsub, jmin, jmax, jeins
       integer   jstep, jgauss, jlor, jpeak
       parameter(iexp  = -10, ilog  = -11, isqrt = -12, isin  = -13,
     $           icos  = -14, itan  = -15, iasin = -16, iacos = -17,
     $           iatan = -18, iabs  = -19, ineg  = -20,
     $           isinh = -23, icosh = -24,
     $           itanh = -25, icoth = -26, isech = -27, icsch = -28,
     $           icot  = -29, isec  = -30, icsc  = -31,
     $           iacot = -32, iasec = -33, iacsc = -34,
     $           iacoth= -35, iasech= -36, iacsch= -37,
     $           ilog10= -38, ifact = -39, iint  = -40, inint = -41,
     $           iadd  = -50, isub  = -51, imul  = -52,
     $           idiv  = -53, iy2x  = -54, jadd  =-111, jsub  =-112,
     $           jmin  =-55,  jmax  = -56,
     $           jeins =-121, jstep =-122, jgauss=-123, jlor  =-124,
     $           jpeak =-125)


       external      istrln

       ilen = istrln(strout)
       if (iparen.ne.0) then
          call messag( ' math encoding error:  syntax error ')
          call messag( '     '//strout(1:ilen) )
          call messag( '  parentheses are not matched ')
          ierr = 7
          return
       end if
cc       print*, ' enchk #1  ieqn = ', ieqn
c--
c-- check that one component math functions are followed by "("
c-- and that parentheses are not left hanging
       iparen = 0
       do 4400 i = 1, ieqn + 2
          if (i.eq.1) then
             ibfr = ileft
          else
             ibfr = itemp(i-1)
          end if
          iaft = itemp(i+1)
          it   = itemp(i)
          if (it.eq.ileft)    iparen = iparen + 1
          if (it.eq.iright)   iparen = iparen - 1
c--   check that iparen is never negative (that is left parens
c     before right parens)
          if (     (iparen.lt.0)
     $         .or.( (it.eq.ileft).and.(ibfr.gt. 0)     )
     $         .or.( (it.eq.ileft).and.(iaft.eq.iright) )
     $         .or.( (it.eq.iright).and.(iaft.eq.ileft) )
     $         .or.( (it.eq.iright).and.(iaft.gt. 0)    ) ) then
             call messag( ' math encoding error:  syntax error')
             call messag( '     '//strout(1:ilen) )
             call messag( '  parentheses not used properly')
             ierr = 7
             return
          end if
c--   check that "(," and ",)" are not in string
          if ( (it.eq.icomma).and.((iaft.eq.iright).or.
     $         (ibfr.eq.ileft).or.(ibfr.eq.icomma)) ) then
             call messag( ' math encoding error:  syntax error ')
             call messag( '     '//strout(1:ilen) )
             errmsg = ' ",,", "(," and ",)" are not allowed '
             ii = istrln(errmsg)
             call messag( '  '//errmsg(1:ii) )
             ierr = 3
             return
          end if
c--   check that one-component math operators are followed by "("
          if ( (it.le.-10).and.(it.ge.-49) ) then
             if (iaft.ne.ileft) then
                call messag( ' math encoding error:  syntax error')
                call messag( '     '//strout(1:ilen) )
                errmsg = 'one-component math function must be '//
     $               'followed by "("'
                ii = istrln(errmsg)
                call messag('  '// errmsg(1:ii) )
                ierr = 5
                return
             end if
             if ( (i.gt.1).and.(ibfr.ge.-49).and.(ibfr.le.-1)
     $            .and.(ibfr.ne.ileft).and.(ibfr.ne.icomma) )  then
                call messag(' math encoding error:  syntax error ')
                call messag( '     '//strout(1:ilen) )
                errmsg = 'a number is preceded by a '//
     $               'one-component math function without using "(" '
                ii = istrln(errmsg)
                call messag('  '// errmsg(1:ii) )
                ierr = 5
                return
             end if
          end if
c--   look for a real number preceded or followed by a
c     either a real number or a variable
          if ( ((it.ge.300).and.(it.le.600)).and.
     $         ((iaft.ge.1).or.(ibfr.ge.1)) ) then
             call messag(' math encoding error:  syntax error ')
             call messag( '     '//strout(1:ilen) )
             if (iaft.ge.0) then
                errmsg = 'a real number is followed by '//
     $               'a real number, variable or fixed value'
             else
                errmsg = 'a real number is preceded by '//
     $               'a real number, variable or fixed value'
             end if
             ii = istrln(errmsg)
             call messag('  '// errmsg(1:ii) )
             ierr = 3
          end if
          if ((it.ge.1).and.(iaft.ge.-49).and.(iaft.le.-10)) then
             call messag(' math encoding error:  syntax error')
             call messag( '     '//strout(1:ilen) )
             errmsg = 'a number is followed by a '//
     $            'one-component math function '
             ii = istrln(errmsg)
             call messag('  '// errmsg(1:ii) )
             ierr = 5
             return
          end if
c     
c  the sepcial functions eins, step, gauss, lor, etc, require 
c  a certain number of commas (usually 1 or 2)
             if (it.le.-100)  then
                jstack = 1
                jcomma = 0
                do  4200 j = i+2, ieqn
                   jt   = itemp(j)
ccc                   print*, ' jt = ', jt
                   if (jt.eq.ileft ) jstack = jstack + 1
                   if (jt.eq.iright) jstack = jstack - 1
                   if ((jstack.eq.1).and.(jt.eq.icomma))
     $                  jcomma = jcomma + 1
                   strok = .true.
                   if (jstack.eq.0) then 
ccc                      print*, ' jstack = 0 , jcomma = ', jcomma
                      strok = .false.
                      if ((it.eq.jeins).and.(jcomma.ne.2)) then
                         errmsg = ' the function "eins" '//
     $                        'requires 3 arguments and 2 commas'
                         strtmp = ' the proper syntax is: '//
     $                        ' "eins(temp, theta, mass)" '
                      elseif ((it.eq.jgauss).and.(jcomma.ne.2)) then
                         errmsg = ' the function "gauss" '//
     $                        'requires 3 arguments and 2 commas'
                         strtmp = ' the proper syntax is: '//
     $                        ' "gauss(x, center, width)" '
                      elseif ((it.eq.jlor).and.(jcomma.ne.2)) then
                         errmsg = ' the function "lor" '//
     $                        'requires 3 arguments and 2 commas'
                         strtmp = ' the proper syntax is: '//
     $                        ' "lor(x, center, width)" '
                      elseif ((it.eq.jstep).and.(jcomma.ne.1)) then
                         errmsg = ' the function "step" '//
     $                        'requires 2 arguments and 1 comma'
                         strtmp = ' the proper syntax is: '//
     $                        ' "step(x,x0)" '
                      elseif ((it.eq.jmin).and.(jcomma.ne.1)) then
                         errmsg = ' the function "min" '//
     $                        'requires 2 arguments and 1 comma'
                         strtmp = ' the proper syntax is: '//
     $                        ' "min(x,y)" '
                      elseif ((it.eq.jmax).and.(jcomma.ne.1)) then
                         errmsg = ' the function "max" '//
     $                        'requires 2 arguments and 1 comma'
                         strtmp = ' the proper syntax is: "max(x,y)" '
                      else
                         strok = .true.
                      endif   
                      if (.not.strok) then 
                         call messag(
     $                        ' math encoding error:  syntax error')
                         call messag( '     '//strout(1:ilen))
                         ii = max(1, istrln(errmsg))
                         call messag('  '// errmsg(1:ii))
                         ii = max(1, istrln(strtmp))
                         call messag('  '// strtmp(1:ii))
                         ierr = 5
                         return
                      end if
                   endif
 4200           continue
             end if
 4400     continue
       return
       end
        subroutine encod(string,vnames,nv,consts,nc,icode,ni,ierr)
c
c   copyright 1993  university of washington      matt newville
c
c   this encodes the integer array 'icode' from an equation in the
c   character string 'string'. the companion function *decod* will
c   decode this integer array, returning the proper number.
c   decod is called by:
c             decod(icode, consts, values, defval)
c   the values of 'values' should correspond to the variables named
c   in 'vnames'.  encod and decod are designed for many repeated
c   evaluations. the encoding is slow and slightly redundant and the
c   decoding is as efficient as possible. icode is a small number of
c   integers representing the rpn notation for the math expression,
c   with special integer values specifing all operations and values.
c
c   the character string contains a fortran-like math expression.
c   variables can be used. their names will be held in the character
c   array 'vnames', and their numerical values will be held in the
c   real array 'values'. variables do not need to be explicitly
c   declared before encoding. if a variable is found that has not
c   already been identified, it will be added to the list. the link
c   between variable name and value, and the actual values used are
c   expected to be managed by the routine(s) calling encod and decod.
c
c  input:
c    string  character string containing fortran-like math expression
c    vnames  character array containing variable names
c    nv      dimension of array vnames          (maximum = 500)
c    consts  real array of numerical constants in math expressions
c    nc      dimension of array consts
c    ni      dimension of icode consts          (maximum = 100)
c  output:
c    string  math expression as to be evaluated (with parens added)
c    vnames  character array containing variable names
c    consts  real array of numerical constants in math expressions
c    icode   integer array containing code for the math expression
c    ierr    error/warning code - routine will not stop !
c           -2     a new variable was added to the list
c           -1     string empty
c            0     no errors or warnings messages at all
c            1     too many constants
c            2     incorrect dimension of vnames
c            3     improper/ambiguous arithmetic
c            5     one-component math syntax error
c            7     parentheses syntax error
c                             ( unmatched or in improper place)
c            9     too many objects in math expresion
c----------------------------------------------------------------------
c    the real array consts contains all the real numbers used as
c    constants. the first 10 values of consts are set aside for
c    "common" real values: 0, 1, 2, pi, etc. these ten constants
c    can be used for accessing internal parameters. the calling
c    routine can associate any values it likes with the first ten
c    numbers, and by rewriting some of this routine, names can be
c    associated with the values. 'pi', and 'reff' are handled in
c    this way. (though it's not expected that anyone will want to
c    overwrite the value of pi, the same is not true for reff).
c
c----------coding parameters for the math operations------------------
c   icode value             meaning
c  -299 to -100    special functions { add(x,y), eins(temp,theta,rmass) }
c   -99 to  -50    two-element math operations (x+y, x**y)
c   -49 to  -10    one-element math operations (1/x, sin(x), et c.)
c    -9 to   -6    control operations (open and close parens, comma)
c    -5 or   -1    not possible! (useful for  overwriting/disabling)
c             0    null string
c     1 to 2000    variables corresponding to vnames strings
c  2001 to    ?    numbers in fixval corresponding to fixnam strings
c---------------------------------------------------------------------
c passed variables
       implicit double precision(a-h, o-z)
       integer            nv, ni, nc, ierr, icode(ni)
       character*(*)      string, vnames(nv)
       double precision   consts(nc), pi, one, zero, xreal
       integer    maxlen, jconst, ileft, iright, icomma
       parameter(pi = 3.141592653589793, one = 1., zero = 0.)
       parameter(maxlen = 100, jconst =2000 )
       parameter(ileft =  -6, iright =  -7, icomma = -8   )
c      include 'ints.h'
       integer   iexp, ilog, isqrt, isin, icos, itan, iabs, ineg
       integer   iasin, iacos, iatan, isinh, icosh, itanh
c -- new in phit
       integer   icot, isec, icsc, iacot, iasec, iacsc
       integer   icoth, isech, icsch, iacoth, iasech, iacsch
       integer   ilog10, ifact, iint, inint
c -- new in phit
       integer   iadd, isub, imul, idiv, iy2x
       integer   jadd, jsub, jmin, jmax, jeins
       integer   jstep, jgauss, jlor, jpeak
       parameter(iexp  = -10, ilog  = -11, isqrt = -12, isin  = -13,
     $           icos  = -14, itan  = -15, iasin = -16, iacos = -17,
     $           iatan = -18, iabs  = -19, ineg  = -20,
     $           isinh = -23, icosh = -24,
     $           itanh = -25, icoth = -26, isech = -27, icsch = -28,
     $           icot  = -29, isec  = -30, icsc  = -31,
     $           iacot = -32, iasec = -33, iacsc = -34,
     $           iacoth= -35, iasech= -36, iacsch= -37,
     $           ilog10= -38, ifact = -39, iint  = -40, inint = -41,
     $           iadd  = -50, isub  = -51, imul  = -52,
     $           idiv  = -53, iy2x  = -54, jadd  =-111, jsub  =-112,
     $           jmin  =-55,  jmax  = -56,
     $           jeins =-121, jstep =-122, jgauss=-123, jlor  =-124,
     $           jpeak =-125)


c## for vector math:
       integer   jvmax, jvmin, jvsum, jvprod, jvnpts
       parameter(jvmax =-251, jvmin  =-252, jvnpts = -253,
     $           jvsum =-255, jvprod =-256 )
c##
c internal variables
       character*70 str, strtmp, errmsg, strout, strnum*30
       character*1  str1, straft, strbfr, number*12, opera*9
       logical      found
       integer      itemp(maxlen), ntemp, ilen, istr, ieqn
       integer      iparen, iexcla, iperct, ieolc, isave
       integer      ibfr, iaft, ibefr, iaftr, ivarln, istrln, i, ii
       external     istrln
       data number,opera /'1234567890 .' , '+-*/^(), '/
c-----------------------------------------------------------------------
c  initial error checking of input dimensions
       if (nv.ge.jconst) then
          call messag(' math encoding error:  bad dimension')
          write (errmsg,'(4x,a,i4,a)') ' more than', jconst,
     $                       ' variables are requested '
          ii  = istrln(errmsg)
          call messag('  '// errmsg(1:ii) )
          ierr = 2
          return
       end if
c     initialization
       ierr   = 0
       strtmp = ' '
       str    = ' '
       str1   = ' '
       strnum = ' '
       straft = ' '
       strbfr = ' '
       found  = .false.
c     remove interior blanks from string
       strtmp = string
       call triml(strtmp)
       call unblnk(strtmp)
c     remove end-of-line comments
c     '!' and '%' signify end of line comments
       iexcla = index(strtmp,'!')
       if (iexcla.eq.0) iexcla = 70
       iperct = index(strtmp,'%')
       if (iperct.eq.0) iperct = 70
       ieolc  = min(iperct,iexcla)
       if (ieolc.eq.1) strtmp = ' '
       if ( (ieolc.ge.2).and.(ieolc.le.70) ) then
          str        = strtmp(:ieolc-1)
          str(ieolc:) = ' '
          strtmp     = str
       end if
       ilen   = istrln(strtmp)
c     
c     if string is blank, return
       if ( (strtmp.eq.' ').or.(ilen.le.0).or.(ieolc.eq.1) ) then
          icode(1) = 0
          ierr     = -1
          return
       end if
c     
c     convert string to the case of this routine :
c     the variable 'case' controls the case of the routine, so it
c     must be the same case as the strings tested for in strtmp.
       call smcase(strtmp, 'case')
       do 40 i = 1, nv
          call smcase(vnames(i), 'case')
 40    continue
c     
c     initialized integer arrys to 0
       do 50 i = 1, ni
          icode(i) = 0
 50    continue
       do 60 i=1,maxlen
          itemp(i)  = 0
 60    continue
c     
c  set the first values in consts:
c  be careful when changing consts(1) from zero, because the rest
c  of the first ten constants are set to zero also, even though a
c  calling routine may want to overwrite some of them !!!
       consts(1) = zero
       consts(2) = one
       consts(3) = pi
       consts(4) = zero
       consts(5) = zero
c     initialization done.
c-------------------------------------------------------------------
c   now start dealing with strtmp as a math expression
c   fix multiple "unitary" operations: ++, -- -> +; -+, +- -> -
       call unblnk(strtmp)
       ilen  = istrln(strtmp)
       do 120 i = 1, ilen-1
          str1   = strtmp(i:i)
          straft = strtmp(i+1:i+1)
          if    ( ( (str1.eq.'-').and.(straft.eq.'-') ).or.
     $            ( (str1.eq.'+').and.(straft.eq.'+') ) ) then
             strtmp(i:i+1) = ' +'
          elseif( ( (str1.eq.'+').and.(straft.eq.'-') ).or.
     $            ( (str1.eq.'-').and.(straft.eq.'+') ) ) then
             strtmp(i:i+1) = ' -'
          end if
 120   continue
       call unblnk(strtmp)
       ilen  = istrln(strtmp)
c   insert parens to ensure normal math precedence.
c   note that this is not entirely necessary, but it is convenient
c   to rewrite the string in the way it is intended to be evaluated.
       call parens(strtmp)
       call triml(strtmp)
       string = strtmp
       ilen   = istrln(string)
       strout = string
       strtmp = ' '
c-----------------------------------------------------------------------
c   with the string well behaved (parens inserted so there are no
c   ambiguities in the math), let's dechiper it and encode icode
c   decipher string into integers
       isave  = 0
       istr   = 0
       ieqn   = 0
       iparen = 0
c     advance string postion, check for end of string
 300   continue
       ieqn = ieqn + 1
       istr = istr + 1
       ibfr = istr - 1
       iaft = istr + 1
       if (ibfr.lt.1) ibfr = 1
       if (istr.gt.ilen) go to 4000
       str1   = string(istr:istr)
       strbfr = string(ibfr:ibfr)
       straft = string(iaft:iaft)
c--   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
c     parse string
c#constants:  real number : find length of real number, read it,
c     store it, and advance string to end of number
       if (index(number,str1).ne.0) then
          isave  = istr + nbrstr(string(istr:ilen)) 
          strnum = string(istr:isave)
          call getval(strnum, xreal, ierr)
          if (ierr.ne.0) then
             call messag( ' encod error: trying to read a '//
     $                    ' real number from the string')
             errmsg = string(istr:isave)
             il     = istrln(errmsg)
             call messag('     '//errmsg(1:il) )
             call messag('  this will be set to zero ?' )
          endif   
c   check if constant is already stored. don't start adding more
c   until consts(10), to preserve the stored internal constants
cc          print*, xreal
          do 450 i = 1, nc
             if (xreal.eq.consts(i)) then
                itemp(ieqn)  = jconst + i
                go to 500
             elseif ((consts(i).eq.0).and.(i.ge.10)) then
                itemp(ieqn)  = jconst + i
                consts(i) = xreal
                go to 500
             end if
 450      continue
 500      continue
c   error : too many constants!
cc          print*, ' umm,'
          if (i.ge.nc) then
             call messag( ' encod error:   too many constants !')
             write (errmsg,'(14x,2a,i4,a)') 'the current',
     $            ' limit is ', nc,' unique numbers.'
             call messag( errmsg )
             ierr = 1
             return
          end if
          istr = isave
c#internally stored constants
c     'pi' followed by an operation (or a blank or paren)
c     means use the constant stored in adress #(jconst + 3).
       elseif( (string(istr:istr+1).eq.'pi').and.
     $         (index(opera,string(istr+2:istr+2)).ne.0)  ) then
          itemp(ieqn)  = jconst + 3
          istr         = istr + 1
c     
c     'reff' followed by an operation (or a blank or paren)
c     means use the constant stored in adress #(jconst + 4).
c     (this is useful for feffit)
       elseif( (string(istr:istr+3).eq.'reff').and.
     $         (index(opera,string(istr+4:istr+4)).ne.0)  ) then
          itemp(ieqn)  = jconst + 4
          istr         = istr + 3
c     
c     'i' followed by an operation (or a blank or paren)
c     means use the constant stored in adress #(2*jconst - 1).
c     (this is useful for datman)
       elseif( (string(istr:istr+1).eq.'_i').and.
     $         (index(opera,string(istr+2:istr+2)).ne.0)  ) then
          itemp(ieqn)  = 2*jconst - 1 
          istr         = istr + 1
c     
c# end constants
c# math operations
c     parens and comma
       elseif (str1.eq.'(') then
          itemp(ieqn)  = ileft
          iparen = iparen + 1
       elseif (str1.eq.')') then
          itemp(ieqn)  = iright
          iparen = iparen - 1
       elseif (str1.eq.',') then
          itemp(ieqn)  = icomma
c     two component math
       elseif ((str1.eq.'+').or.(str1.eq.'-')) then
          if ( (straft.eq.')') ) then
             call messag(' math encoding error:  syntax error '//
     $            'in the expression:' )
             call messag( '     '//strout(1:ilen) )
             call messag('  "+)"  and "-)" are not correct syntax.')
             ierr = 3
             return
          else
             if (str1.eq.'+') itemp(ieqn) = iadd
             if (str1.eq.'-') itemp(ieqn) = isub
          end if
       elseif ((str1.eq.'/').or.(str1.eq.'*')) then
          ibefr = index('(+-/*^,',strbfr)
          iaftr = index(')*/^,',straft)
          if ( (istr.eq.1).or.(istr.eq.ilen).or.
     $         (iaftr.ne.0).or.(ibefr.ne.0) ) then
             call messag(' math encoding error:  syntax error '//
     $                   'in the expression:' )
             call messag( '     '//strout(1:ilen) )
             if (ibefr.ne.0) then
                errmsg = '  "/" or "*" preceded by one of "+-/*^,("'
             elseif (iaftr.ne.0) then
                errmsg = '  "/" or "*" followed by one of "/*^,)"'
             elseif (istr.eq.1) then
                errmsg = '  "/" or "*" occurs first'
             elseif (istr.eq.ilen) then
                errmsg = '  "/" or "*" occurs last'
             end if
             ii  = istrln(errmsg)
             call messag('    '//errmsg(1:ii) )
             ierr = 3
             return
          else
             if (str1.eq.'*') itemp(ieqn) = imul
             if (str1.eq.'/') itemp(ieqn) = idiv
          end if
       elseif (str1.eq.'^') then
          ibefr = index('(+-/*^,',strbfr)
          iaftr = index(')*/^,',straft)
          if ( (istr.eq.1).or.(istr.eq.ilen).or.
     $         (iaftr.ne.0).or.(ibefr.ne.0) ) then
             call messag(' math encoding error:  syntax error '//
     $            'in the expression:' )
             call messag( '     '//strout(1:ilen) )
             if (ibefr.ne.0) then
                errmsg = '  "^" preceded by one of "+-/*^,("'
             elseif (iaftr.ne.0) then
                errmsg = '  "^" followed by one of "/*^,)"'
             elseif (istr.eq.1) then
                errmsg = '  "^" occurs first'
             elseif (istr.eq.ilen) then
                errmsg = '  "^" occurs last'
             end if
             ii  = istrln(errmsg)
             call messag('    '//errmsg(1:ii) )
             ierr = 3
             return
          else
             itemp(ieqn) = iy2x
          end if
c     
c     special math functions:
       elseif (string(istr :istr+3).eq.'add(') then
          itemp(ieqn)  = jadd
          istr = istr + 2
       elseif (string(istr :istr+3).eq.'sub(') then
          itemp(ieqn)  = jsub
          istr = istr + 2
       elseif (string(istr :istr+3).eq.'min(') then
          itemp(ieqn)  = jmin
          istr = istr + 2
       elseif (string(istr :istr+3).eq.'max(') then
          itemp(ieqn)  = jmax
          istr = istr + 2
       elseif (string(istr :istr+4).eq.'eins(')  then
          itemp(ieqn)  = jeins
          istr = istr + 3
c --- new for phit
       elseif (string(istr :istr+4).eq.'step(')  then
          itemp(ieqn)  = jstep
          istr = istr + 3
       elseif (string(istr :istr+5).eq.'gauss(')  then
          itemp(ieqn)  = jgauss
          istr = istr + 4
       elseif (string(istr :istr+3).eq.'lor(')  then
          itemp(ieqn)  = jlor
          istr = istr + 2
       elseif (string(istr :istr+4).eq.'peak(')  then
          itemp(ieqn)  = jpeak
          istr = istr + 3

c --- new for phit
c##   special functions for arrays or vectors
       elseif (string(istr :istr+4).eq.'npts(') then
          itemp(ieqn)  = jvnpts
          istr = istr + 3
       elseif (string(istr :istr+4).eq.'vmax(') then
          itemp(ieqn)  = jvmax
          istr = istr + 3
       elseif (string(istr :istr+4).eq.'vmin(') then
          itemp(ieqn)  = jvmin
          istr = istr + 3
       elseif (string(istr :istr+4).eq.'vsum(') then
          itemp(ieqn)  = jvsum
          istr = istr + 3
       elseif (string(istr :istr+5).eq.'vprod(') then
          itemp(ieqn)  = jvprod
          istr = istr + 4
c##   end  special finctions for arrays 
c     
c     one component math :
c     the operator must be followed by '(', or the expression will
c     be thought to be a variable name: exp2 is a variable !
       elseif (string(istr  :istr+3).eq.'exp(') then
          itemp(ieqn)  = iexp
          istr = istr + 2
       elseif (string(istr :istr+3).eq.'log(') then
          itemp(ieqn)  = ilog
          istr = istr + 2
       elseif (string(istr  :istr+2).eq.'ln(') then
          itemp(ieqn)  = ilog
          istr = istr + 1
       elseif (string(istr  :istr+4).eq.'sqrt(') then
          itemp(ieqn)  = isqrt
          istr = istr + 3
       elseif (string(istr :istr+3).eq.'abs(') then
          itemp(ieqn)  = iabs
          istr = istr + 2
       elseif (string(istr :istr+3).eq.'sin(') then
          itemp(ieqn)  = isin
          istr = istr + 2
       elseif (string(istr :istr+3).eq.'cos(') then
          itemp(ieqn)  = icos
          istr = istr + 2
       elseif (string(istr :istr+3).eq.'tan(') then
          itemp(ieqn)  = itan
          istr = istr + 2
       elseif (string(istr  :istr+4).eq.'asin(') then
          itemp(ieqn)  = iasin
          istr = istr + 3
       elseif (string(istr  :istr+4).eq.'acos(') then
          itemp(ieqn)  = iacos
          istr = istr + 3
       elseif (string(istr  :istr+4).eq.'atan(') then
          itemp(ieqn)  = iatan
          istr = istr + 3
       elseif (string(istr  :istr+4).eq.'sinh(') then
          itemp(ieqn)  = isinh
          istr = istr + 3
       elseif (string(istr  :istr+4).eq.'cosh(') then
          itemp(ieqn)  = icosh
          istr = istr + 3
       elseif (string(istr  :istr+4).eq.'tanh(') then
          itemp(ieqn)  = itanh
          istr = istr + 3
c --- new for phit
       elseif (string(istr  :istr+4).eq.'coth(') then
          itemp(ieqn)  = icoth
          istr = istr + 3
       elseif (string(istr  :istr+4).eq.'sech(') then
          itemp(ieqn)  = isech
          istr = istr + 3
       elseif (string(istr  :istr+4).eq.'csch(') then
          itemp(ieqn)  = icsch
          istr = istr + 3
       elseif (string(istr  :istr+3).eq.'cot(') then
          itemp(ieqn)  = icot
          istr = istr + 2
       elseif (string(istr  :istr+3).eq.'sec(') then
          itemp(ieqn)  = isec
          istr = istr + 2
       elseif (string(istr  :istr+3).eq.'csc(') then
          itemp(ieqn)  = icsc
          istr = istr + 2
       elseif (string(istr  :istr+5).eq.'log10(') then
          itemp(ieqn)  = ilog10
          istr = istr + 4
       elseif (string(istr  :istr+3).eq.'int(') then
          itemp(ieqn)  = iint
          istr = istr + 2
       elseif (string(istr  :istr+4).eq.'nint(') then
          itemp(ieqn)  = inint
          istr = istr + 3
       elseif (string(istr  :istr+4).eq.'fact(') then
          itemp(ieqn)  = ifact
          istr = istr + 3
c --- new for phit
c     
c# end math operations
c# variables
c     end with blank or math symbol from character string opera
       else
          do 750 i = istr, 70
             str1 = string(i:i)
             if (index(opera,str1).ne.0) go to 760
 750      continue
 760      continue
c     find which variable it is:
          ivarln = i - 1
          found = .false.
          if (ivarln.le.istr) ivarln = istr
          do 800 i = 1, nv
             if ( string(istr:ivarln).eq.vnames(i) ) then
                found = .true.
                itemp(ieqn)  = i
                istr = ivarln
                go to 810
             end if
 800      continue
 810      continue
c     if it isn't already in vnames, put it in first available slot
          if (.not.found) then
             do 830 i = 1, nv
                if ( vnames(i).eq.' ' ) then
                   vnames(i) = string(istr:ivarln)
                   itemp(ieqn)  = i
                   found = .true.
                   istr  = ivarln
                   go to 840
                end if
 830         continue
 840         continue
          end if
c     if found is still false, then vnames is full.
c     this is then a good time to hurl a warning message.
          if (.not.found) then
             call messag( ' encod error: formula encoding error!')
             call messag( '              too many variables ')
             write (errmsg,'(14x,2a,i4,a)') 'the current',
     $            ' limit is ', nv,' unique variables.'
             call messag( errmsg )
             ierr = 2
             return
          end if
c     
c# end parsing and encoding, go back to line 300 for more
       end if
       go to 300
c--   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
c--   check that the number of parentheses is correct
 4000  continue
       call enchk(iparen, strout, itemp, ieqn, ierr)
       if (ierr.gt.0) return
c----------------------------------------------------------------------
c     rewrite itemp to reverse polish notation
c     (this allows easier decoding: see h-p calculator manuals on rpn.)
c     then load up icode, and we're all done.
       call engrpn(itemp)
       ntemp  = min (ni, maxlen)
       if (itemp(ntemp).ne.0) then
          call messag(' math encoding error:  too many objects!')
          write (errmsg,'(1x,a,i4,a)') 'more than', ntemp,
     $         ' objects in this expression : '
          ii  = istrln(errmsg)
          call messag('     '//errmsg(1:ii) )
          call messag('     '//strout(1:ilen) )
          ierr = 9
       end if
       do 5000 i = 1, ntemp
          icode(i) = itemp(i)
 5000  continue
       return
c     end subroutine encod
       end
c---------------------------------------------------------------
c----------------------------------------------------------------
        subroutine engrpn(icode)
c
c  copyright 1993  university of washington      matt newville
c
c     convert english encoded math code to reverse polish code
c
c     this seems to work fairly well  as long as the input icode
c     has already  had parentheses included.
c
c strategy:
c    first assign class of operation to each argument in icode.
c    then try to convert all unary minus signs to their correct
c    one-component operator.  next, two component function are
c    put after their two arguments. one component operators are
c    then put after thier argument. finally, all parenthese and
c    commas are dropped.
c---------------------------------------------------------------------
       integer   ileft, iright, icomma, ineg, iy2x
       integer   iadd, isub, imul, idiv, maxlen
       parameter (ileft =  -6, iright =  -7, icomma =  -8,
     $            ineg = -20,  iy2x = -54,
     $      iadd = -50, isub = -51, imul   = -52, idiv   = -53)
       parameter(maxlen = 100)
       integer   icode(maxlen),  itemp(maxlen) , idone(maxlen)
       integer   iclass(maxlen), icltmp(maxlen), idtemp(maxlen)
       integer   iclo(6), i, ic, ichi, icn, id, j, j0, k, ksave
       integer   ibfr, istack
       logical   opera
c---------------------------------------------------------------------
c-- initialize itemp, and assign class to objects and operators
c
       do 10 j = 1, maxlen
          i        = icode(j)
          if (i.eq.0)                        iclass(j) = 0
          if (i.gt.0)                        iclass(j) = 1
          if ((i.le.-10).and.(i.ge.-49))     iclass(j) = 2
          if ((i.eq.iadd) .or.(i.le.isub))   iclass(j) = 3
          if ((i.eq.imul) .or.(i.le.idiv))   iclass(j) = 4
          if (i.eq.iy2x)                     iclass(j) = 5
          if (i.le.-100)                     iclass(j) = 6
          if (i.eq.ileft)                    iclass(j) = 7
          if (i.eq.iright)                   iclass(j) = 8
          if (i.eq.icomma)                   iclass(j) = 9
          itemp(j)  = icode(j)
          icltmp(j) = iclass(j)
 10    continue
c---------------------------------------------------------------------
c-- convert unary minus and plus signs to unitary operators
c-- plus signs are easy: simply remove the plus sign.
c-- minus signs are hard: find next operator on this level,
c-- and convert  "- x1" to  "neg ( x1 )", which will then be
c-- converted down below to "x1 neg".
       do 500 j0 = 1, maxlen
          j = j0
 100      continue
          i  = itemp(j)
          ic = icltmp(j)
          if(ic.eq.0) go to 510
          ibfr = 0
          if (j.gt.1)      ibfr = iclass(j-1)
c--   unary plus sign
          if ( ((j.eq.1).or.(ibfr.eq.7).or.(ibfr.eq.4).or.(ibfr.eq.5)
     $             .or.(ibfr.eq.9))    .and.(i.eq.iadd)  )  then
             do 120  k = j, maxlen-1
                icode(k)  = itemp(k+1)
                iclass(k) = icltmp(k+1)
 120         continue
             icode(maxlen)  = 0
             iclass(maxlen) = 0
             do 130  k = j, maxlen
                  itemp(k)  = icode(k)
                  icltmp(k) = iclass(k)
 130           continue
               go to 100
c--   unary minus sign
c--   change minus sign to unary operator
c--   if next object is (, then we're done.
c--   otherwise, ... - x ... -> ... neg ( x ) ...
            elseif ( ((j.eq.1).or.(ibfr.eq.7).or.(ibfr.eq.9).or.
     $           (ibfr.eq.4).or.(ibfr.eq.5)).and.(i.eq.isub)  )  then
c  replace '-' with 'neg'
               icode(j)  = ineg
               iclass(j) = 2
c neg number : find next +-,) or end of line, and insert parentheses.
               if (iclass(j+1).eq.1) then
                  icn   =  icltmp(j+1)
                  opera = (icn.eq.9).or.(icn.eq.8).or.(icn.eq.0)
     $                .or.(icn.eq.3)
                  if (.not.opera) then
                     istack = 0
                     k      = j
 140                 continue
                     k = k + 1
                     if (k.ge.maxlen) go to 150
                     icn   =  icltmp(k)
                     opera =  (icn.eq.9).or.(icn.eq.8).or.(icn.eq.0)
     $                    .or.(icn.eq.3)
                     if ( (istack.eq.0) .and. (opera) ) go to 150
                     if (icn.eq.7) istack = istack + 1
                     if (icn.eq.8) istack = istack - 1
                     go to 140
 150                 continue
                     ksave = k -1
c  insert left paren
                     icode(j+1)  = ileft
                     iclass(j+1) = 7
c  bump everything after left paren up by 1
                     do 170  k = j+2, ksave + 1
                        icode(k)  = itemp(k-1)
                        iclass(k) = icltmp(k-1)
 170                 continue
c  insert right paren
                     icode(ksave+2)  = iright
                     iclass(ksave+2) = 8
c  bump everything after right paren up by 2
                     do 180  k = ksave+3, maxlen-2
                        icode(k)  = itemp(k-2)
                        iclass(k) = icltmp(k-2)
 180                 continue
                  end if
c
c neg unary operator : need to find end of argument of operator.
c       then change '-' -> neg and insert parens.
               elseif (((iclass(j+1).eq.2).or.(iclass(j+1).eq.6))
     $                 .and.(iclass(j+2).eq.7) ) then
c  find end of argument
                  istack = 1
                  do 200 k = j+3, maxlen
                     if (icltmp(k).eq.7) istack = istack + 1
                     if (icltmp(k).eq.8) istack = istack - 1
                     if (istack.eq.0) go to 220
 200              continue
 220              continue
                  ksave = k
c  insert left paren
                  icode(j+1)  = ileft
                  iclass(j+1) = 7
c  bump everything after left paren up by 1
                  do 250  k = j+2, ksave + 1
                     icode(k)  = itemp(k-1)
                     iclass(k) = icltmp(k-1)
 250              continue
c     insert right paren
                  icode(ksave+2)  = iright
                  iclass(ksave+2) = 8
c  bump everything after right paren up by 2
                  do 280  k = ksave+3, maxlen-2
                     icode(k)  = itemp(k-2)
                     iclass(k) = icltmp(k-2)
 280              continue
               end if
               do 380 k = 1, maxlen
                    itemp(k) = icode(k)
                    icltmp(k) = iclass(k)
 380           continue
               j = j - 1
               if (j.eq.0) j = 1
               go to 100
         end if
c reset itemp and icltmp and go back to beginning
 500  continue
 510  continue
c
      do 600 i = 1, maxlen
         icode(i) = itemp(i)
         iclass(i) = icltmp(i)
 600   continue
c---------------------------------------------------------------------
c-- convert class 5 operators (^ only):
c   x1 ^ x2  -> x1 x2 ^
c    if operator is '^', and is not already followed by ',)+-*/^',
c    then find next place with stack=0 (that is on the current level),
c    that contains an ',)+-*/^'
       ichi = 5
       iclo(1) =  9
       iclo(2) =  8
       iclo(3) =  0
       iclo(4) =  3
       iclo(5) =  4
       iclo(6) =  5
       call class(icode, iclass, ichi, iclo)
c---------------------------------------------------------------------
c-- convert class 4 operators (* and / only):
c   x1 * x2  -> x1 x2 *
c    if operator is '*/', and is not already followed by ',)+-*/',
c    then find next place with stack=0 (that is on the current level),
c    that contains an ',)+-*/'
c  undo iclo(6) = '^' to a repeat of iclo(3) = 0
       ichi    =  4
       iclo(6) =  0
       call class(icode, iclass, ichi, iclo)
c---------------------------------------------------------------------
c-- convert class 3 operators (+ and - only):
c   x1 + x2  -> x1 x2 +
c    if operator is '+-', and is not already followed by ',)+-',
c    then find next place with stack=0 (that is on the current level),
c    that contains an ',)+-'
c  undo iclo(5) = '*/' to a repeat of iclo(3) = 0
       ichi    =  3
       iclo(5) =  0
       call class(icode, iclass, ichi, iclo)
       do 900 i = 1, maxlen
          itemp(i) = icode(i)
          icltmp(i) = iclass(i)
 900   continue
c
c---------------------------------------------------------------------
c-- convert class 2 and class 6 operators.
c   all unary operators and special functions have the syntax:
c        f(x1, x2, x3, ...) -> (x1, x2, x3, ...) f
       do 6900 j = 1, maxlen
          idone(j) = 0
          idtemp(j) = 0
 6900  continue
       do 8000 j0 = 1, maxlen - 1
          j  =  j0
 7000     continue
          i  =  itemp(j)
          ic =  icltmp(j)
          id =  idtemp(j)
          if(ic.eq.0) go to 8010
          if ( (id.eq.0).and.
     $         ( (ic.eq.2).or.(ic.eq.6)).and.(iclass(j+1).eq.7) ) then
             istack = 1
             do 7200 k = j+2, maxlen
                if (icltmp(k).eq.7) istack = istack + 1
                if (icltmp(k).eq.8) istack = istack - 1
                if (istack.eq.0) go to 7300
 7200        continue
 7300        continue
             ksave = k
             icode(ksave)   = itemp(j)
             iclass(ksave)  = icltmp(j)
             idone(ksave)   = 1
             do 7500  k = j, ksave-1
                icode(k)  = itemp(k+1)
                iclass(k) = icltmp(k+1)
                idone(k)  = idtemp(k+1)
 7500        continue
c     reset itemp and start over again at the same place
             icode(maxlen-1) = 0
             icode(maxlen)   = 0
             idone(maxlen-1) = 0
             idone(maxlen)   = 0
             do 7800 k = 1, maxlen
                itemp(k)  = icode(k)
                icltmp(k) = iclass(k)
                idtemp(k) = idone(k)
 7800        continue
             go to 7000
          end if
 8000  continue
 8010  continue
c---------------------------------------------------------------------
c-- finally, remove all parentheses and commas for icode
       j = 0
       k = 0
       do 8900 i = 1, maxlen
          itemp(i)  = icode(i)
          icltmp(i) = iclass(i)
          icode(i)  = 0
          iclass(i) = 0
 8900  continue
 9000  continue
       j = j + 1
       if (j.gt.maxlen) go to 9100
       ic = icltmp(j)
       if (ic.eq.0) go to 9100
       if ( (ic.ne.7).and.(ic.ne.8).and.(ic.ne.9) ) then
          k = k + 1
          icode(k)  = itemp(j)
          iclass(k) = icltmp(j)
       end if
       go to 9000
 9100  continue
       return
c     end subroutine engrpn
       end
c------------------------------------------------------------
        subroutine class(icode, iclass, ichi, iclo)
c
c    copyright 1993  university of washington   matt newville
c
c    this is called by engrpn. operators are moved around
c    to convert english math to reverse polish.
c    if operator is of class icin, and is not already followed by
c    an operator with class in iclo,  then find next place with
c    stack=0 (that is on the current level), that contains an
c    operator with class in iclo
c---------------------------------------------------------------------
       integer i, maxlen, j0, ksave, ic, icn, j, k, istack
       parameter(maxlen = 100 )
       integer icode(maxlen),  iclass(maxlen), ichi , iclo(6)
       integer itemp(maxlen),  icltmp(maxlen)
       logical opera
       do 100 i = 1, maxlen
          itemp(i)  = icode(i)
          icltmp(i) = iclass(i)
 100   continue
       do 2000 j0 = 1, maxlen - 1
          j  =  j0
 500      continue
          ic =  icltmp(j)
          if (ic.eq.0) go to 2010
          if (ic.eq.ichi) then
             icn   =  icltmp(j+1)
             opera = .false.
             do 550 i = 1, 6
                if  ( icn.eq.iclo(i) ) opera = .true.
 550         continue
             if (.not.opera) then
                istack = 0
                k      = j
 600            continue
                   k = k + 1
                   if (k.ge.maxlen) go to 700
                   icn   =  icltmp(k)
                   opera = .false.
                   do 650 i = 1, 6
                      if  ( icn.eq.iclo(i) ) opera = .true.
 650               continue
                   if ( (istack.eq.0) .and. (opera) ) go to 700
                   if (icn.eq.7) istack = istack + 1
                   if (icn.eq.8) istack = istack - 1
                   go to 600
 700            continue
                ksave = k -1
                icode(ksave)   = itemp(j)
                iclass(ksave)  = icltmp(j)
                do 1000  k = j, ksave-1
                   icode(k)  = itemp(k+1)
                   iclass(k) = icltmp(k+1)
 1000           continue
c     reset itemp and start over again at the same place
                icode(maxlen-1) = 0
                icode(maxlen)   = 0
                do 1200 k = 1, maxlen
                   itemp(k) = icode(k)
                   icltmp(k) = iclass(k)
 1200           continue
                go to 500
             end if
          end if
 2000  continue
 2010  continue
c     finish it up
       do 3000 i = 1, maxlen
          icode(i) = itemp(i)
          iclass(i) = icltmp(i)
 3000  continue
c     
       return
c end subroutine class
       end
c-------------------------------------------------------------------
       subroutine getval(word, xreal, ierr)
c  reads real number "xreal" from input character string "word".
c  returns xreal = 0. and (ierr.ne.0)  if word cannot be a number
      implicit double precision(a-h,o-z)
      character*(*) word
      double precision          xreal
      integer       ierr
      logical       number, isnum
      external      isnum
c
      xreal   = 0.
      ierr    = -999
      number  = isnum(word)
      if (number) then
          ierr = 0
          read ( word, '(bn,f30.0)', iostat = ierr)  xreal
      end if    
      return
c end subroutine getval
      end
c----------------------------------------------------------------
      subroutine gtint(word, integr, ierr)
c  reads integer "integr" from input character string "word".
c  returns integr = 0 and (ierr.ne.0)  if word cannot be a number
      character*(*) word
      double precision          xreal
      integer       integr, ierr
      integr = 0
      call getval(word, xreal, ierr)
      if (ierr.eq.0) integr = int(xreal)
      return
c end subroutine gtint
      end
c---------------------------------------------------------------- 
      function istrln (string)
c
c  returns index of last non-blank character.  
c  returns zero if string is null or all blank.
      character*(*)  string
c-- if null string or blank string, return length zero.
      istrln = 0
      if (string(1:1).eq.char(0))  return
      if (string.eq.' ')  return
c 
c-- find rightmost non-blank character.
      ilen = len (string)
      do 20  i = ilen, 1, -1
         if (string (i:i) .ne. ' ')  goto 30
   20 continue
   30 istrln = i

      return
c end function istrln 
      end
c---------------------------------------------------------------- 
      subroutine triml (string)
c removes leading blanks.
      character*(*)  string
c
      jlen = istrln(string)
c-- all blank and null strings are special cases.
      if (jlen .eq. 0)  return
c-- find first non-blank char
      do 10  i = 1, jlen
         if (string (i:i) .ne. ' ')  goto 20
  10  continue
  20  continue
c-- if i is greater than jlen, no non-blanks were found.
      if (i .gt. jlen)  return
c-- remove the leading blanks.
      string = string(i:)
      return
c end subroutine triml 
      end
c---------------------------------------------------------------- 
      subroutine untab (string)
c replace tabs with blanks :    tab is ascii dependent
      integer        itab , i, ilen
      parameter      (itab = 9)
      character*(*)  string, tab*1
      tab  = char(itab)
      ilen = max(1, istrln(string))
 10   continue 
        i = index(string(:ilen), tab ) 
        if (i .ne. 0) then
            string(i:i) = ' '
            go to 10
        end if
      return
c end subroutine untab
      end
c---------------------------------------------------------------- 
      subroutine upper (str)
c  changes a-z to upper case.  ascii specific
c-   for ascii:  ichar(upper case 'a') =  65
c-               ichar(lower case 'a') =  97
      character*(*)  str
      integer iupa, iloa, iloz, idif
      data    iupa, iloa / 65, 97/
      idif = iloa - iupa 
      iloz = iloa + 25
      jlen = max(1, istrln (str) )
      do 10  i = 1, jlen
         ic = ichar (str(i:i))
         if ((ic.ge.iloa).and.(ic.le.iloz))  str(i:i) = char(ic-idif)
   10 continue
      return
c end subroutine upper
      end
c---------------------------------------------------------------- 
      subroutine lower (str)
c  changes a-z to lower case.  ascii specific
c-   for ascii:  ichar(upper case 'a') =  65
c-               ichar(lower case 'a') =  97
      character*(*)  str
      integer iupa, iloa, iupz, idif
      data    iupa, iloa / 65, 97/
      idif = iloa - iupa 
      iupz = iupa + 25
      jlen = max(1, istrln (str) )
      do 10  i = 1, jlen
         ic = ichar (str(i:i))
         if ((ic.ge.iupa).and.(ic.le.iupz)) str(i:i) = char(ic+idif)
   10 continue
      return
c end subroutine lower
      end
c---------------------------------------------------------------- 
      subroutine smcase (str, contrl)
c  convert case of string *str*to be the same case 
c  as the first letter of string *contrl* 
c  if contrl(1:1) is not a letter, *str* will be made lower case.
      character*(*) str, contrl, s1*1, t1*1
      s1 = contrl(1:1)
      t1 = s1
      call lower(t1)
      if (t1.eq.s1)  call lower(str) 
      if (t1.ne.s1)  call upper(str) 
      return
c end subroutine smcase
      end
c---------------------------------------------------------------- 
       logical function isnum (string)
c  returns true if string can be a number, else returns false 
c  recognizes e and d exponentials, but is not foolproof
c  to be a number, a string must contain:
c     - only characters in  'de.+-, 1234567890' (case is checked)
c     - no more than one 'd' or 'e' 
c     - no more than one '.'
       character*(*)  string, str*70, number*20
       integer        iexp, idec, i, ilen, ier, j, istrln
       double precision           x
       external       istrln
c  note:  layout of *number* is important: don't change this!!
       data           number   /'de.+-, 1234567890'/
c-
       isnum = .false. 
       iexp  = 0
       idec  = 0
       str   = string
       ilen  = max(1, istrln (str) )
       call smcase(str, number )
       do 100  i = 1, ilen
          j = index(number,str(i:i) )
          if (j.le.0)               go to 200
          if((j.eq.1).or.(j.eq.2))  iexp = iexp + 1
          if (j.eq.3)               idec = idec + 1
 100   continue
c  every character in the string was found in  *number*
c  so the string probably is a number
       isnum = .true.
c  but let's do a few more tests: 
c    number of exponential and decimal markers       
       if (iexp.ge.2) isnum = .false.
       if (idec.ge.2) isnum = .false.
c    read with iostat (this may report an error, but not always)
       read(str,150,iostat=ier)  x
 150   format (bn,f70.0)
       if (ier.ne.0)  isnum = .false.
c  all tests done
 200   continue
       return
c  end logical function isnum
       end
c---------------------------------------------------------------- 
      subroutine bwords (s, nwords, words)
c
c     breaks string into words.  words are seperated by one or more
c     blanks, or a comma or equal sign and zero or more blanks.
c
c     args        i/o      description
c     ----        ---      -----------
c     s            i       char*(*)  string to be broken up
c     nwords      i/o      input:  maximum number of words to get
c                          output: number of words found
c     words(nwords) o      char*(*) words(nwords)
c                          contains words found.  words(j), where j is
c                          greater then nwords found, are undefined on
c                          output.
c
c      written by:  steven zabinsky, september 1984
c
c**************************  deo soli gloria  **************************
c-- no floating point numbers in this routine.
      implicit integer (a-z)
      character*(*) s, words(nwords)
      character blank, comma, equal
      parameter (blank = ' ', comma = ',', equal = '=')
 
c-- betw    .true. if between words
c   comfnd  .true. if between words and a comma or equal has
c                                         already been found
      logical betw, comfnd
c-- maximum number of words allowed
      wordsx = nwords
 
c-- slen is last non-blank character in string
      slen = istrln (s)
 
c-- all blank string is special case
      if (slen .eq. 0)  then
         nwords = 0
         return
      endif
 
c-- begc is beginning character of a word
      begc = 1
      nwords = 0
      betw   = .true.
      comfnd = .true.
      do 10  i = 1, slen
         if (s(i:i) .eq. blank)  then
            if (.not. betw)  then
               nwords = nwords + 1
               words (nwords) = s (begc : i-1)
               betw = .true.
               comfnd = .false.
            endif
         elseif ((s(i:i).eq.comma).or.(s(i:i).eq.equal))  then
            if (.not. betw)  then
               nwords = nwords + 1
               words (nwords) = s(begc : i-1)
               betw = .true.
            elseif (comfnd)  then
               nwords = nwords + 1
               words (nwords) = blank
            endif
            comfnd = .true.
         else
            if (betw)  then
               betw = .false.
               begc = i
            endif
         endif
         if (nwords .ge. wordsx)  return
   10 continue
c 
      if (.not. betw  .and.  nwords .lt. wordsx)  then
         nwords = nwords + 1
         words (nwords) = s (begc :slen)
      endif
      return
c end subroutine bwords 
      end
c---------------------------------------------------------------- 
      subroutine strclp(str,str1,str2,strout)
c
c  a rather complex way of clipping a string: 
c      strout = the part of str that begins with str2.   
c  str1 and str2 are subsrtings of str, (str1 coming before str2),
c  and even if they are similar, strout begins with str2
c  for example:
c   1.  str =  "title title my title" with  str1 = str2 = "title"
c       gives strout = "title my title"
c   2.  str =  "id  1  1st path label" with str1 = "1", str2 = "1st"
c       gives strout = "1st path label"
c
      character*(*)  str, str1, str2, strout
      integer  i1, i2, ibeg, iend, istrln, ilen
      external istrln
      ilen   = len(strout)
      i1     = max(1, istrln(str1))
      i2     = max(1, istrln(str2))
      i1e    = index(str,str1(1:i1)) + i1
      ibeg   = index(str(i1e:),str2(1:i2) ) + i1e - 1
      iend   = min(ilen+ibeg, istrln(str) )                 
      strout = str(ibeg:iend)
      return
c end subroutine strclp
      end
c---------------------------------------------------------------- 
      subroutine messag(messg)
c  write message to  standard ouput 
      character*(*) messg
      write(*,10)   messg
 10   format(1x,a)
      return
c end subroutine messag
      end
c-----------------------------------------------------------------
       subroutine askstr(ask, str)
c
c      prompt for and return a characer string.
c      see also the routines askint, and askval.
c  inputs: 
c    ask   character string for prompt
c    str   default string to show in prompt
c  outputs:
c    str   string read in 
c  copyright 1993 university of washington     matt newville         
       character*(*) ask , str
       character*80  query , answer

       query = ask
       call triml(query)
       i = istrln(query)
       call triml(str)
       j = istrln(str)
       if (j.le.0) j = 1
       k = len(str)
       k = min(k,80) 
 10    format (a)
 30    format (a,' [', a, ']  >',$)
       write(*, 30) query(1:i), str(1:j)
       read (*, 10, err= 50) answer
       call triml (answer)
       if ( answer.ne.' ') str = answer(1:k)
c
 50    continue
       return
c  end subroutine askstr
       end
c---------------------------------------------------------
       subroutine askval(ask, val)
c
c      prompt for and return a real number.
c      see also askint, and askstr.
c  inputs: 
c    ask   character string for prompt
c    val   default real number to show in prompt
c  outputs:
c    val   real number read in 
c
c  copyright 1993 university of washington     matt newville         
c
       character*(*) ask, query*80, answer*30 
       double precision          val
c
       query = ask
       call triml(query)
       i = istrln(query)
 10    format (a)
 30    format( 2x,a,' [', f16.8, ']  >',$)
c
       write(*, 30) query(1:i), val
       read (*, 10) answer
       call triml (answer)
       if (answer.ne.' ')        call getval(answer, val, ierr)
       return
c  end subroutine askval 
       end
c---------------------------------------------------------------- 
       subroutine askint(ask, int)
c
c      prompt for and return a real number.
c      see also askint, and askstr.
c  inputs: 
c    ask   character string for prompt
c    int   default integer to show in prompt
c  outputs:
c    int   integer read in 
c
c  copyright 1993 university of washington     matt newville         
c
       character*(*) ask, query*80, answer*30 
       integer       int
c
       query = ask
       call triml(query)
       i = istrln(query)
 10    format (a)
 30    format( 2x,a,' [', i12, ']  >',$)
c
       write(*, 30) query(1:i), int
       read (*, 10)  answer
       call triml (answer)
       if (answer.ne.' ')        call gtint(answer, int, ierr)
       return
c  end subroutine askint
       end
c-----------------------------------------------------------------
      subroutine parse(string, nwords, words)
c
c  parse a string into substrings, or words
c  this is similar to bwords, but more restrictive:
c    words are substrings separated by commas or blanks, 
c    but must also have no unmatched parens, braces, 
c    square brackets, angle brackets, or double quotes.
c
c  for example :    {string 1} , a  "oh, yeah"   (and again) 
c  will give 4 words: words(1) = string 1 
c                     words(2) = a 
c                     words(3) = oh, yeah
c                     words(4) = and again
c  note that this is crude and that  ") v ("   
c  will return 1 word  =  ) v (   !
c
      logical       comfnd,  betw, okay
      integer       ibeg, iquote, iparen, isqr, ibrace, iangle
      integer       iwords, is, istrln, ii, i, ierr, nmax, nwords
      character*(*) string , words(nwords), str*100
      character*1   comma, blank, dquote, oparen, cparen, s
      character*1   osqr, csqr, obrace, cbrace, oangle, cangle
      external istrln
      data   comma, blank, dquote, oparen, cparen, osqr, csqr, 
     $       obrace, cbrace, oangle, cangle
     $  / ',', ' ', '"', '(', ')', '[', ']', '{', '}', '<', '>'/

c
      nmax   = nwords
      nwords = 0
      str    = string
      call triml(str)
      is    = istrln(str)
      if (is .eq. 0)   return
c
      ibeg   = 1
      iquote = 0
      iparen = 0
      isqr   = 0
      ibrace = 0
      iangle = 0
      iwords = 0
      comfnd = .true.
      betw   = .true.
      okay   = .true.
c
      do 100 i = 1, is
        s = str(i:i)
        if ((s.eq.dquote).and.(iquote.eq.0)) then
           iquote = 1
        elseif ((s.eq.dquote).and.(iquote.eq.1)) then
           iquote = 0
        elseif (s.eq.oparen) then
           iparen = iparen + 1
        elseif (s.eq.cparen) then
           iparen = iparen - 1
        elseif (s.eq.osqr)   then
           isqr   = isqr   + 1
        elseif (s.eq.csqr)   then
           isqr   = isqr   - 1
        elseif (s.eq.oangle) then
           iangle = iangle + 1
        elseif (s.eq.cangle) then
           iangle = iangle - 1
        elseif (s.eq.obrace) then
           ibrace = ibrace + 1
        elseif (s.eq.cbrace) then 
           ibrace = ibrace - 1
        end if
        okay = ( (iquote.eq.0).and.(iparen.eq.0).and.(isqr.eq.0)
     $      .and.(ibrace.eq.0).and.(iangle.eq.0) )
c  okay means that the delimiters have all cancelled out, 
c  so that the words can be broken up here
        if (okay) then
cc           print* ,' okay - s, i, ibeg, betw, comfnd = ',
cc     $                      s, i, ibeg, betw, comfnd
           if (s.eq.blank) then
              if (.not.betw)  then
                 nwords = nwords + 1
                 words (nwords) = str(ibeg:i-1)
                 betw = .true.
                 comfnd = .false.
              end if
           elseif (s.eq.comma)  then
              if (.not. betw)  then
                 nwords = nwords + 1
                 words (nwords) = str(ibeg : i-1)
                 betw = .true.
              elseif (comfnd)  then
                 nwords = nwords + 1
                 words (nwords) = blank
              endif
              comfnd = .true.
           elseif (betw)  then
              betw = .false.
              ibeg = i
           endif
        else
           if (betw)  then
              betw = .false.
              ibeg = i
           endif
        endif
        if (nwords .ge. nmax)  go to 200
 100  continue
c  get last word
      if ((.not.betw).and.(nwords.lt.nmax))  then
         nwords = nwords + 1
         words (nwords) = str(ibeg :is)
      endif
c at this point, the words may still have the delimiters
c around them, so strip them off:
 200  continue
      iwords = 0
      do 300 i = 1, nwords 
         str      = words(i) 
ccc         print*, ' str =   :',str(1:60)
         words(i) = ' '
         call triml(str)
         ii = istrln(str)
         if (ii.gt.0) then
            iwords = iwords + 1
ccc            print*, ' parse : subword =   :',str(1:ii),':'
            call getstr(str(1:ii), words(iwords), ierr)
ccc            print*, ' getstr gives =   :',words(iwords),':'
         endif
 300  continue
      nwords = iwords
      return
c end subroutine parse
      end
c---------------------------------------------------------------- 
      subroutine getstr(strinp, strout, ierr)
c
c  strips matched delimiters from string
c          ""   {}  ()  []  <>  
c 
      character*(*) strinp, strout, strtmp*80
      character*6   open, close
      data   open, close  / '"{([<' , '"})]>' /
c
      ierr   = 0
      strout = strinp
      strtmp = strout
      call  triml(strtmp)
      ilen   = istrln(strtmp)
      if (ilen .le. 0 ) return
      idel   = index(open, strtmp(1:1))
      if (idel .eq. 0 ) return
      istack = 0
      do 100 i = 2, ilen
        if (strtmp(i:i).eq.open(idel:idel)) then
            istack = istack + 1
        elseif (strtmp(i:i).eq.close(idel:idel)) then
            if (istack.eq.0) go to 110
            istack = istack - 1
        end if
 100  continue 
c  getting to here means the delimiters were not matched. 
c      ierr = 1
c      return
 110  continue 
      strout = strtmp(2: i-1)
c
      return
c end subroutine getstr
      end
c---------------------------------------------------------------- 
       subroutine getfln(strinp, str1, str2)
c  yet another "strip off the first word" subroutine!

       integer idelim, iclose, ii
       character*(*)  strinp, str1, str2,  errmsg*80
       character*5  open, close, clodel*1
       data open, close / '"{(<[',  '"})>]' /
c
       idelim = index(open,strinp(1:1))
       if (idelim.ne.0) then
          clodel = close(idelim:idelim) 
          iclose = index(strinp(2:), clodel)
          if (iclose.le.0) then
             call messag(' improper syntax - can not find closing'//
     $                   ' delimiter for file name in line:')
             errmsg = strinp
             ii     = istrln(errmsg)
             call messag(errmsg(1:ii))
             return
          end if
          str1 = strinp(2:iclose)
          str2 = strinp(iclose+2:)
       else
          str1 = ' '
          str2 = strinp
       end if
       return
c end  subroutine getfln
       end

       subroutine eins( x, nx, y, ny, z, nz, ierr)
c
c
c calculate debye waller in einstein model given
c   x = rmass
c   y = theta
c   z = temp
c
       implicit double precision(a-h,o-z)
       integer  maxpts, ix, iy, i
       parameter (maxpts = 2048)
       double precision      x(*), y(*), z(*), xout(maxpts)
       integer   nx, ny, nz,  ierr, nx1, ny1, nz1, nptstk
       external   nptstk
       ierr = 0
c decide number of points to write out 
c     nptstk = smaller of nx1, ny1 unless either of them is 1, 
c             in which case it is the larger of the two. 
c             (n = 1 implies a scalar )
       nx1  = nx
       ny1  = ny
       nz1  = nz
       nx   = nptstk (nx1, ny1)
       nx   = nptstk (nx,  nz1)
       do 20 i = 1, nx
          ix = min(nx1, i)
          iy = min(ny1, i)
          iz = min(nz1, i)
          xout(i) = einval(z(iz), y(iy), x(ix) )
 20    continue 
       
c now overwrite x
       do 300 i = 1, nx
          x(i) = xout(i)
 300   continue 
       return
c end subroutine eins
       end

       double precision function einval(t,theta,rmass)
c  
c  compute sigma squared from the eisntein model
c  input:
c    t       temperature          (K)
c    theta   einstein temperature (K)
c    rmass   reduced mass         (amu)
c  output:
c    einval  sigma squared        (angstroms squared)
       double precision   hbarc, boltz, amu2ev, small, big, two, factor
       double precision   t, theta, rmass, x
       parameter (hbarc  = 1973.270533d0,  boltz = 8.617385d-5    )
       parameter (amu2ev = 9.3149432d8                          )
       parameter (two    = 2.d0, small = 1.d-3, big = 1.d8        )
       parameter (factor = hbarc*hbarc/(two * boltz * amu2ev)   )
ccc
ccc      parameter (factor = 24.25423371)
ccc      1 amu =  amu2ev eV 
ccc      hbarc = 1973.270533 eV * AA
ccc      boltz = 1/11604.45  eV / K
ccc
       if (theta.lt.small)  theta = small
       if (rmass.lt.small)  rmass = small
       if (t.lt.small)      t     = small
       x  =  theta / (two * t)
       if (x.lt.small)      x     = small
       if (x.gt.big)        x     = big
       einval  = factor / ( rmass * theta * tanh(x))
       return
c  end function einval
       end


       subroutine step( x, nx, y, ny, ierr)

c calculate debye waller in einstein model given
c   x = site of step
c   y = x value

       implicit double precision(a-h,o-z)
       integer  maxpts, ix, iy, i
       parameter (maxpts = 2048)
       double precision      x(*), y(*), xout(maxpts)
       integer   nx, ny, ierr, nx1, ny1, nptstk
       external   nptstk
       ierr = 0
c decide number of points to write out 
c     nptstk = smaller of nx1, ny1 unless either of them is 1, 
c             in which case it is the larger of the two. 
c             (n = 1 implies a scalar )
       nx1  = nx
       ny1  = ny
       nx   = nptstk (nx1, ny1)
       do 20 i = 1, nx
          ix = min(nx1, i)
          iy = min(ny1, i)
          xout(i) = stepfn(y(iy), x(ix) )
 20    continue 
       
c now overwrite x
       do 300 i = 1, nx
          x(i) = xout(i)
 300   continue 
       return
c end subroutine step
       end

      double precision function stepfn(x, x0)

      double precision x, x0

      if (x.le.x0) then
          stepfn=0.d0
      else
          stepfn=1.d0
      endif
      return 
c  end function stepfn
      end

       subroutine gauss( x, nx, y, ny, z, nz, ierr)
c
c
c calculate a gaussian
c   x = width
c   y = center
c   z = x value
c
       implicit double precision(a-h,o-z)
       integer  maxpts, ix, iy, i
       parameter (maxpts = 2048)
       double precision      x(*), y(*), z(*), xout(maxpts)
       integer   nx, ny, nz,  ierr, nx1, ny1, nz1, nptstk
       external   nptstk
       ierr = 0
c decide number of points to write out 
c     nptstk = smaller of nx1, ny1 unless either of them is 1, 
c             in which case it is the larger of the two. 
c             (n = 1 implies a scalar )
       nx1  = nx
       ny1  = ny
       nz1  = nz
       nx   = nptstk (nx1, ny1)
       nx   = nptstk (nx,  nz1)
       do 20 i = 1, nx
          ix = min(nx1, i)
          iy = min(ny1, i)
          iz = min(nz1, i)
          xout(i) = gauval(z(iz), y(iy), x(ix) )
 20    continue 
       
c now overwrite x
       do 300 i = 1, nx
          x(i) = xout(i)
 300   continue 
       return
c end subroutine gauss
       end

      double precision function gauval(x, x0, w)

c  calculate a gaussian function
c    x:  x value
c    x0: center of gaussian     
c    w:  half-width of gaussian

      double precision x, x0, w, expon, tiny
      parameter(tiny=1.d-20)
      parameter(gnorm=1.7724 5385)
c  gaussian unit area normalization = sqrt(pi)

      w = abs(w)
      if ((abs(w).lt.tiny).or.(abs(x-x0).gt.(1/tiny))) then
          gauval = 0
      else
          expon  = ( (x-x0)/w )**2
          gauval = exp(-expon) / (gnorm*w)
      endif

      return 
c  end function gauval
      end
c----------------------------------------------------------------
       subroutine lor( x, nx, y, ny, z, nz, ierr)
c
c
c calculate a lorentzian
c   x = half-width
c   y = center
c   z = x axis
c
       implicit double precision(a-h,o-z)
       integer  maxpts, ix, iy, i
       parameter (maxpts = 2048)
       double precision      x(*), y(*), z(*), xout(maxpts)
       double precision lorval
       integer   nx, ny, nz,  ierr, nx1, ny1, nz1, nptstk
       external   nptstk
       ierr = 0
c decide number of points to write out 
c     nptstk = smaller of nx1, ny1 unless either of them is 1, 
c             in which case it is the larger of the two. 
c             (n = 1 implies a scalar )
       nx1  = nx
       ny1  = ny
       nz1  = nz
       nx   = nptstk (nx1, ny1)
       nx   = nptstk (nx,  nz1)
       do 20 i = 1, nx
          ix = min(nx1, i)
          iy = min(ny1, i)
          iz = min(nz1, i)
          xout(i) = lorval(z(iz), y(iy), x(ix) )
 20    continue 
       
c now overwrite x
       do 300 i = 1, nx
          x(i) = xout(i)
 300   continue 
       return
c end subroutine lor
       end

      double precision function lorval(x, x0, w)

c  calculate a lorentzian function
c    x:  x value
c    x0: center of lorentzian     
c    w:  half-width of lorentzian

      double precision x, x0, w, factor, tiny
      parameter(tiny=1.d-20)
      parameter(lnorm=2)
c  lorentzian unit area normalization = 2

      w = abs(w)
      if (abs(w).lt.tiny) then
          lorval = 0
      else
          factor = (x-x0)/w
          lorval = 1.d0 / (factor**2 + 1.d0)
          lorval = lorval / (lnorm*w)
      endif

      return 
c  end function lorval
      end
c----------------------------------------------------------------
      subroutine peak( xl, nxl, h, nh, k, nk, l, nl,
     $            a, na, b, nb, c, nc, al, nal, be, nbe,
     $            ga, nga, ierr)

c---calculate a diffraction peak:
c   xl       = x-ray wavelength
c   h,k,l    = miller indeces
c   a,b,c    = cell axes in angstroms
c   al,be,ga = cell angles in degrees ( with decimals not minutes )

      implicit double precision(a-h,o-z)
      integer  maxpts, ixl, ih, il, ik, ia, ib, ic, ial, ibe, iga, i
      parameter (maxpts = 2048)
      double precision xl(*), h(*), k(*), l(*),  xout(maxpts)
      double precision a(*),  b(*), c(*), al(*), be(*), ga(*)
      double precision peaval
      integer   nxl,  nh,  nk,  nl,  nal,  nbe,  nga
      integer   nxl1, nh1, nk1, nl1, nal1, nbe1, nga1
      integer   na,   nb,  nc,  na1, nb1,  nc1,  ierr, nptstk
      external  nptstk
      ierr = 0
c     decide number of points to write out 
c     nptstk = smaller of nx1, ny1 unless either of them is 1, 
c             in which case it is the larger of the two. 
c             (n = 1 implies a scalar )
      nxl1 = nxl
      nh1  = nh
      nk1  = nk
      nl1  = nl
      na1  = na
      nb1  = nb
      nc1  = nc
      nal1 = nal
      nbe1 = nbe
      nga1 = nga
      nxl  = nptstk (nxl1, nh1)
      nxl  = nptstk (nxl,  nk1)
      nxl  = nptstk (nxl,  nl1)
      nxl  = nptstk (nxl,  na1)
      nxl  = nptstk (nxl,  nb1)
      nxl  = nptstk (nxl,  nc1)
      nxl  = nptstk (nxl,  nal1)
      nxl  = nptstk (nxl,  nbe1)
      nxl  = nptstk (nxl,  nga1)
      do 20 i = 1, nxl
        ixl = min(nxl1,i)
        ih  = min(nh1, i)
        ik  = min(nk1, i)
        il  = min(nl1, i)
        ia  = min(na1, i)
        ib  = min(nb1, i)
        ic  = min(nc1, i)
        ial = min(nal1,i)
        ibe = min(nbe1,i)
        iga = min(nga1,i)
        xout(i) = peaval( ga(iga), be(ibe), al(ial), c(ic), b(ib),
     $              a(ia), l(il), k(ik), h(ih), xl(ixl) )
 20   continue 

c now overwrite x
      do 300 i = 1, nxl
        xl(i) = xout(i)
 300  continue
 
      return
c  end subroutine peak
      end

      double precision function peaval(xl, h, k, l, a, b, c, al, be, ga)

c  calculate a diffraction peak position
c   xl       = x-ray wavelength
c   h,k,l    = miller indeces
c   a,b,c    = cell axes in angstroms
c   al,be,ga = cell angles in degrees ( with decimals not minutes )

      implicit double precision (a-h,o-z)
      double precision xl, h, k, l, a, b, c, al, be, ga, alr, ber, gar
      double precision dfact, vfact, s11, s22, s33, s12, s13, s23
      double precision tiny, small, d2r, pi, sinth, volume
      parameter(tiny=1.d-20, small=1.d-7, pi=3.14159265358979323844)
      parameter(d2r=pi/180) 
      logical ortho

c  convert angles to radians
      alr = al*d2r
      ber = be*d2r
      gar = ga*d2r

      ortho = .true.
      test = abs(cos(alr)) + abs(cos(ber)) + abs(cos(gar))
      if (test.gt.small) ortho = .false.

      volume = a*b*c
      if (.not.ortho) then
          vfact  = 1 - cos(alr)**2 - cos(ber)**2 - cos(gar)**2 +
     $                2*cos(alr)*cos(ber)*cos(gar)
          volume = volume*sqrt(vfact)
      endif

      s11 = (b*c*sin(alr))**2
      s22 = (a*c*sin(ber))**2
      s33 = (a*b*sin(gar))**2
      s12 = 0.d0
      s23 = 0.d0
      s13 = 0.d0
      if (.not.ortho) then
          s12 = a * b * c * c * (cos(alr)*cos(ber) - cos(gar))
          s23 = a * b * c * a * (cos(ber)*cos(gar) - cos(alr))
          s13 = a * b * c * b * (cos(gar)*cos(alr) - cos(ber))
      endif

      dfact = s11*h**2 + s22*k**2 + s33*l**2 +
     $            2*(s12*h*k + s23*k*l + s13*h*l)

      if ( (volume.lt.tiny).or.(dfact.lt.tiny) ) then
          sinth = 1.d0
      else
          d = sqrt( volume**2 / dfact )
          sinth = xl / (2*d)
      endif
      if (abs(sinth).gt.1.d0) then
          peaval = pi/2
      else
          peaval = asin( sinth )
      endif

c  convert peaval to 2theta and degrees      
      peaval = 2*peaval/d2r

      return 
c  end function peaval
      end
      integer function nbrstr(string)
c
c  find a number in a string
c  given a string that is known to begin with a digit or sign.
c  return the position of the end of the number.
c  nbrstr : position of end of number
      integer   istrln, i, ilen, iback
      character*(*)  string 
      character*1    digit*10, plus, minus, d, e, decml, s, sp
      logical     lexp, ldecml
      data digit  /'1234567890'/
      data minus,plus,d,e,decml /'-','+','d','e','.'/
c------
      ldecml = .false.
      lexp   = .false.
      ilen   = istrln(string)
      nbrstr = ilen
      if (ilen.gt.1) then
         iback  = 1
c find end of number :  digits are always ok.
c stop at second d, e, decml, or sign that's not preceded by (d,e)
         do 200 i = 2, ilen
            sp = string(i-1:i-1)
            s  = string(i:i)
            if (index(digit,s).eq.0) then 
               if ( ( (s.ne.plus).and.(s.ne.minus).and.(s.ne.d)
     $                 .and.(s.ne.e).and.(s.ne.decml) )
     $          .or.( lexp.and.((s.eq.d).or.(s.eq.e)) )
     $          .or.( ldecml.and.(s.eq.decml) ) 
     $          .or.( ( (s.eq.plus).or.(s.eq.minus) ).and.
     $                (sp.ne.d).and.(sp.ne.e) ) )     go to 210
               lexp   = lexp.or.(s.eq.d).or.(s.eq.e)
               ldecml = ldecml.or.(s.eq.decml)
            end if
 200     continue 
         iback = 0
 210     continue 
         nbrstr = i - 1 - iback 
      end if
cc      print*, 'nbrstr: string = ' , string(1:ilen), nbrstr,'  '
cc     $                             string(1:nbrstr)
      return
c  end function nbrstr
      end

       subroutine parens(string)
c
c  copyright 1993  university of washington      matt newville
c  insert parentheses in a string for a fortran math expression
c  to give the normal math precedence :
c   ^   before   *,/,+,-          and  *,/    before   +,-
c  also:  ** is replaced by ^ 
c
c  this calls parins, which does the real work of inserting parens.
c--------------------------------------------------------------------
       character*(*) string,  strtmp*100
       integer       i, ilen, istrln
       external      istrln
c
c  first replace '**' with '^ '
       strtmp = string
       ilen = istrln(strtmp)
       do 10 i = 1, ilen-1
          if (strtmp(i:i+1).eq.'**') then
             strtmp(i:i+1) = '^ '
          end if
 10    continue
       call unblnk(strtmp)
       ilen = istrln(strtmp)
       if ((strtmp.ne.' ').and.(ilen.gt.0)) then 
c
c then put parentheses in to make sure that exponentiation is
c done before multiplication, division, addition, or subtraction.
          if (index(strtmp,'^').ne.0)
     $         call parins(strtmp,ilen,'^','*/+-')
c
c then put parentheses in to make sure that multiplication and 
c division are done before addition and subtraction.
          if ((index(strtmp,'*').ne.0).or.(index(strtmp,'/').ne.0))
     $         call parins(strtmp,ilen,'*/','+-')
c
c   put new string into output and return
       endif
       string = strtmp
       return
c end subroutine parens
       end
       subroutine parins(strin, ilen , sopt1, sopt2)
c
c  copyright 1993  university of washington      matt newville
c
c  insert parentheses in a string for a fortran math expression
c  to give the normal math precedence :
c          "sopt1" is more important than "sopt2"
c  this gets kind of ugly but appears to never fail. 
c--------------------------------------------------------------------
       integer     mstack,i, ilen, j, istart, istack, iopt, ioptst
       parameter ( mstack = 40 )
       character*(*) strin, sopt1, sopt2
       character*100  string, dummy, str1*1, operas*4, digits*10
       logical       paren(mstack)
       integer       idiff, jstk, i1, i2, io, ieon, nbrstr
       integer       iopen(mstack), iclose(mstack), istrln
       external      istrln, nbrstr
       data operas, digits / '*/+-', '0123456789'/

c insert a leading blank, initialize stack control and parentheses
       iopt = 0
       dummy = ' '
       dummy(2:ilen+1 ) = strin(1:ilen)
       string = dummy
       istart = 1
       istack = 1
       do 50 i = 1, mstack
          iopen(i)  = 1
          iclose(i) = ilen
          paren(i)  = .false.
 50    continue
 100   continue
       ilen = istrln(string) + 2
       ieon = istart - 1
       do 200 i = istart, ilen
c get current character
c check for exponentiation or parens, update stack index
c and insert parens if they aren't there already
c  note that numbers (found with nbrstr) are skipped over 
         str1 = string(i:i)
          if (i.le.ieon) go to 199
          if (index( digits, str1 ).ne.0 ) then
             ieon = i + nbrstr(string(i:))
          elseif (index(sopt1,str1).ne.0) then
             iopt = i
             paren(istack) = .true.
          elseif (str1.eq.'(') then
             istack = istack + 1
             if (istack.gt.mstack) istack = mstack
             iopen(istack) = i
          elseif (str1.eq.')') then
             istack = istack - 1
             if (istack.lt.1) istack = 1
          elseif (index(sopt2, str1 ).ne.0 ) then
             ioptst = i - iopt
             if ( paren(istack) ) then
                paren(istack) = .false.
c     normal case: find a far away operation
                if (ioptst.gt.1) then
                   istart = i + 2
                   io = iopen(istack)
                   idiff = i - io
                   if (idiff.gt.1) then
                      dummy = ' '
                      dummy = string(1:io)//'('//
     $                        string(io+1 :i-1)//')'//string(i:)
                      string = dummy
                   end if
c     non-normal case: operation immediately after '^'
                else
                   jstk = 0
                   do 170 j = i + 1, ilen - 2
                      str1 = string(j:j)
                      if (str1.eq.'(') then
                         jstk = jstk + 1
                      elseif (str1.eq.')') then
                         jstk = jstk - 1
                      elseif ( (jstk.eq.0) .and.
     $                        (index(operas,str1).ne.0) ) then
                         go to 180
                      end if
 170               continue
 180               continue
                   dummy = ' '
                   dummy = string(:i-1)//'('//string(i: j-1)
     $                  //')'//string(j:)
                   string = dummy
                end if
                go to 100
             else
                iopen(istack) = i
             end if
          end if
 199      continue 
 200   continue
c     if needed, insert a last set of parens at the end
       if ( paren(1).and.(iopen(1).ne.1) ) then
          i1 = iopen(istack)
          i2 = istrln(string) + 1
          dummy  = ' '
          dummy  = string(1:i1)//'('//
     $         string(i1+1:i2-1)//')'//string(i2:)
          string = dummy
       end if
       call triml(string)
       strin = string
       ilen  = istrln(string)
300    continue
       return
c end soubroutine parins       
       end
      subroutine unblnk (string)
c
c remove blanks from a string
      integer        i, ilen, j
      character*(*)  string, str*256
      ilen = min(256, max(1, istrln(string)))
      j   = 0
      str = ' '
      do 10 i = 1, ilen
         if (string(i:i).ne.' ') then
            j = j+1
            str(j:j) = string(i:i)
         end if
 10   continue 
      string = ' '
      string = str(1:j)
      return
c end subroutine unblnk
      end
