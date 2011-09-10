      program normal

c----------------------------------------------------------------------
c  version 0.93    11 November, 1995
c----------------------------------------------------------------------
c            written by      Bruce Ravel
c                            Department of Physics
c                            Box 351560
c                            University of Washington
c                            Seattle, WA   USA 98195
c                      
c                 phone      (206) 543-0435 
c                 e-mail     ravel@u.washington.edu
c----------------------------------------------------------------------
c      description of the code:
c 
c  Normal normalizes and aligns xmu scans.  The normalization is done 
c  subtracting a pre-edge line then extrapolating a post-edge line to
c  e0.  The e0 intercept is used for edge-step normalization.  Alignment
c  is done by varying an e0 shift, an edge step, and the slope of a 
c  post-edge line to best fit data to a standard data scan.  The 
c  derivative function is computed by Ridder's method of polynomial
c  extrapolation.
c----------------------------------------------------------------------
c       sample input file:
c 
c     e0 = 4979
c     format = uwxafs      out = ti.nor
c     derivative
c     files
c         ti.xmu  5	! standard scan
c         ti.xmu  12	another scan
c----------------------------------------------------------------------
c       version history
c
c  0.9  pre-release version
c  0.91 default emax for fit = 100
c  0.92 derivatives
c  0.93 fix files names
c----------------------------------------------------------------------

      parameter(nptx=2**11, nfilx=50, ndocx=25, iou=10,
     $            nwdx=5)
      parameter(epsi=1.e-5)
      character*5 vnum
      parameter(vnum='0.93')
      
      dimension     energy(nptx), xmuraw(nptx), xmuout(nptx)
      dimension     foo1(nptx), foo2(nptx), foo3(nptx)
      dimension     eref(nptx), xref(nptx), dydx(nptx)
      dimension     nkey(nfilx)
      logical       eefind, stfind, vaxflg, lalign, lderiv
      character*3   cnum, test
      character*10  filtyp, frmin, frmout, skey(nfilx), skeyo
      character*78  outfil, file(nfilx), line, of, string,
     $              commnt(nfilx)
      character*100 doc(ndocx)
      real          xmuint
      external      xmuint

      common /idata/ nref, nxmu, nmin, nmax
      save /idata/
      common /rdata/ e0, emin, emax, eref, xref, energy, xmuout
      save /rdata/

      character*40 comm

 400  format(a)
 410  format(1x,f13.6,3x,f13.6)
 420  format(75('='))
 430  format(i3)
 432  format(i3.3)
 440  format('Normal ',a5,' -- Normalize and align xmu data.',16x,
     $            'by Bruce Ravel')

      vaxflg   = .false.
c VAX USERS:  change the line above to .true.
c VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX VAX 
      test = 'abc'

      write(line,420)
      call messag(line)
      write(string,440)vnum
      call messag(string)
      call messag(line)

      call noinit(nkey, imess, predg1, predg2, enor1, enor2,
     $            foo1, foo2, foo3,
     $            of, file, skey, commnt, eefind, lalign, lderiv)

      call normin(nfile, imess, nkey, file, e0, 
     $            emax, enor1, enor2, predg1, predg2,
     $            frmin, frmout, of, skey, commnt, lalign, lderiv)

      do 50 i=1,nfile

c       --- read the data
        filtyp = ' '
        nxmu   = nptx
        ndoc   = ndocx
        call inpdat(filtyp, frmin, file(i), vaxflg, skey(i), nkey(i), 
     $       ndoc, doc, nxmu, energy, xmuraw, foo1, foo2, foo3 )
        call case(test,filtyp)
        if ((frmin(1:3).eq.'uwx').and.(filtyp.ne.'xmu')) then
            call messag(' *** ERROR:  uwxafs input file must be '//
     $                  'of type XMU.')
            call messag('             Skipping data set.')
            goto 50
        endif

c       --- run time message about input data file
        if (i.eq.1) then
            comm = '  Read standard from "'
        elseif (lalign) then
            comm = '  Aligning data from "'
        else
            comm = '  Normalizing data from "'
        endif
        ii = istrln(file(i))
        ic = istrln(comm)
        if (frmin(1:3).eq.'uwx') then
            write(cnum,430)nkey(i)
            call messag(comm(:ic)//file(i)(:ii)//'", nkey = '//cnum)
        else
            call messag(comm(:ic)//file(i)(:ii)//'"')
        endif

c       --- remove pre-edge and normalize.  use step as initial guess
c       --- in all other data
        step   = 0
        stfind = .true.
        slopre = 0
        bpre   = 0
        if (enor2.gt.energy(nxmu)-e0) enor2=energy(nxmu)-e0

        call preedg(eefind, e0, predg1, predg2, enor1, enor2,
     $              nxmu, energy, xmuraw, step, stfind, slopre,
     $              bpre, xmuout)
        if (imess.ne.0) then
            print*,'step, slopre, bpre'
            print*,step, slopre, bpre
        endif

        e0shft = 0.e0
        slope  = 0.e0
        if (i.eq.1) then
            do 20 j=1,nxmu
              nref      = nxmu
              eref(j)   = energy(j) 
              xref(j)   = xmuout(j) / step
              xmuout(j) = xmuout(j) / step
 20         continue 
        else
            emin = e0 - pre1
            if (lalign) call edgfit(e0shft, step, slope)
            if (step.lt.epsi) step = 1.e0
            do 30 j=1,nxmu
              energy(j) = energy(j) - e0shft
              factor = 0.e0
              if (energy(j).gt.e0) factor = slope*(energy(j)-e0)
              xmuout(j) = (xmuout(j) / step) + factor
 30         continue 
        endif

        if (of.ne.' ') then
            outfil = of
            if (frmout.eq.'ascii') then
                write(cnum,432)i
                call getnam(of,cnum,outfil)
            endif
        else
            if (frmout.eq.'ascii') then
                in = index(file(i), '.')
                is = istrln(file(i))
                if (in.ne.0) then
                    outfil = file(i)(:in-1)//'_norm'//file(i)(in:is)
                else
                    outfil = file(i)(:is)//'_norm'
                endif
            else
                call getnam(file(i),'nor',outfil)
            endif
        endif

        filtyp = 'xmu'
        nkeyo  = 0
        skeyo  = ' '
        iexist = 0
        call addoc(ndoc, doc, commnt(i), e0shft, step, slope)
        call outdat(filtyp, frmout, outfil, vaxflg, skeyo, nkeyo,
     $    ndoc, doc, nxmu, energy, xmuout, foo1, foo2, foo3, iexist)

        if (lderiv) then
            call messag('       and calculating derivative')
            h = 1.e0
            do 40 j=1,nxmu
              dydx(j) = dfridr(xmuint, energy(j), h, err)
 40         continue 
c             if (of.ne.' ') then
c                 outfil = of
c                 if (frmout.eq.'ascii') then
c                     write(cnum,432)i
c                     call getnam(of,cnum,outfil)
c                 endif
c             else
            if (frmout.eq.'ascii') then
                in = index(file(i), '.')
                is = istrln(file(i))
                if (in.ne.0) then
                    outfil = file(i)(:in-1)//'_der'//file(i)(in:is)
                else
                    outfil = file(i)(:is)//'_der'
                endif
            else
                call getnam(file(i),'der',outfil)
            endif
c            endif
            filtyp = 'xmu'
            nkeyo  = 0
            skeyo  = ' '
            iexist = 0
            call addocd(ndoc, doc)
            call outdat(filtyp, frmout, outfil, vaxflg, skeyo, nkeyo,
     $                  ndoc, doc, nxmu, energy, dydx, foo1, foo2,
     $                  foo3, iexist)
        endif

 50   continue 

      call messag(' ')
      if (frmout(1:3).eq.'uwx') then
          ii = istrln(outfil)
          call messag('  >>> Wrote uwxafs output to "'//
     $                outfil(:ii)//'"')
      else
          string = outfil
          call messag('  >>> Finished writing ascii output.')
      endif
      call messag(line)

      stop
c end main program normal
      end

      subroutine noinit(nkey, imess, predg1, predg2, enor1, enor2,
     $            foo1, foo2, foo3,
     $            of, file, skey, commnt, eefind, lalign, lderiv)

      parameter(nptx=2**11, nfilx=50)

      logical       eefind, lalign, lderiv
      character*10  skey(nfilx)
      character*78  file(nfilx), of, commnt(nfilx)
      integer       nkey(nfilx)

      dimension energy(nptx), xmuout(nptx)
      dimension eref(nptx), xref(nptx)
      dimension foo1(nptx), foo2(nptx), foo3(nptx)
      common /idata/ nref, nxmu, nmin, nmax
      common /rdata/ e0, emin, emax, eref, xref, energy, xmuout

      parameter(zero=0.e0, nulval=-99 999)

c --------- integers ----------------------------------------------
      imess  = 0
      nref   = 0
      do 10 i=1,nfilx
        nkey(i) = 0
 10   continue 

c --------- reals -------------------------------------------------
      e0     = real(nulval-1)
      emax   = real(nulval-1)
      predg1 = -200.e0
      predg2 = -50.e0
      enor1  = 100.e0
      enor2  = 500.e0
      do 20 i=1,nptx
        eref(i)   = zero
        xref(i)   = zero
        energy(i) = zero
        xmuout(i) = zero
        foo1(i)   = zero
        foo2(i)   = zero
        foo3(i)   = zero
 20   continue 

c --------- logicals ----------------------------------------------
      eefind = .false.
      lalign = .true.
      lderiv = .false.

c --------- characters --------------------------------------------
      of = ' '
      do 510 i=1,nfilx
        file(i)   = ' '
        skey(i)   = ' '
        commnt(i) = ' '
 510  continue 

      return 
c  end subroutine noinit
      end 
      subroutine normin(nfile, imess, nkey, file, e0, 
     $            emax, enor1, enor2, predg1, predg2,
     $            frmin, frmout, of, skey, commnt, lalign, lderiv)

c--------------------------------------------------------------
c  copyright 1993 university of washington         bruce ravel
c--------------------------------------------------------------
      implicit real(a-h,o-z)
      implicit integer(i-n)
c      implicit double precision (a-h,o-z)
c---------------------------------------------------------------------   
c  this parses the lines of the command file looking for keywords then
c  reads in the value for that keyword from the next word.  
c---------------------------------------------------------------------

      parameter(nwdx=20, nfilx=50)
      parameter(zero=0.e0, nulval=-99999)
      dimension     nkey(nfilx)
      character*2   test
      character*10  frmin, frmout, skey(nfilx)
      character*20  fname, errmes*60
      character*78  file(nfilx), string, messg, of, words(nwdx),
     $              commnt(nfilx), temp
      logical       lend, inpnul, there, lalign, lderiv

 1410 format(bn,f10.0)
 1440 format(a)

c---------------------------------------------------------------------
c  initialize some things used only in this routine
      iline  = 0
      inpnul = .true.
      test   = 'ab'
c  the value of the variable test must be in the same case as the 
c  keyword names in the long block of elseif's

c---------------------------------------------------------------------
c  open norm.inp
      fname = 'normal.inp'
      call upper(fname)
      inquire(file=fname,exist=there)
      if (.not.there) then
          call lower(fname)
          inquire(file=fname,exist=there)
      endif
      if (.not.there) then
          call messag(' ')
          call messag('* * * WARNING * * *')
          call messag('"normal.inp" not found.  Normal stopping.')
          call messag(' ')
          stop
      endif
      open(unit=1,file=fname,status='old')

c---------------------------------------------------------------------
c  begin reading the input file, words cleared each time
101   continue
      nwds = nwdx
      do 110 iw=1,nwds
        words(iw)=' '
110   continue
      read (1,1440,end=191)string
      call untab(string)

120   continue
      call triml(string)
c                                           - denotes end of job
      if  (string(1:1).eq.'-') goto 191
c                                           skip a comment line
      if  ((string(1:1).eq.'!').or.(string(1:1).eq.'*')
     $ .or.(string(1:1).eq.'%')) goto 101
c                                           skip a blank line
      if  (string.eq.' ') goto 101
c                                           begin reading line, i counts words
      i=1
      call bwords(string,nwds,words)
      inpnul = .false.
      iline  = iline+1

c************************ input file parsing ***************************
c  read a word, identify it, assign the value from the following word(s)
c  increment i and come back.  i points to position in string, when i
c  exceeds nwds go read a new line.
 130  continue
      call case(test,words(i))
c                                           skip a blank line
      if     (words(i).eq.' ') then
          goto 101
c                                           ignore everything after !,*,%
      elseif ((words(i)(1:1).eq.'!').or.(words(i)(1:1).eq.'*').or.
     $          (words(i)(1:1).eq.'%')) then
          goto 101
c                                           finish early
      elseif (words(i).eq.'end') then
          goto 191
c                                           only normalize
      elseif ((words(i).eq.'nofit').or.(words(i)(1:4).eq.'noal').or.
     $                (words(i)(1:4).eq.'norm')) then
          lalign = .false.
          i=i+1
c                                           calculate derivatives
      elseif (words(i)(1:3).eq.'der') then
          lderiv = .true.
          i=i+1
c                                           diagnostic messages
      elseif (words(i).eq.'message') then
          call getint(words(i), words(i+1), imess)
          i=i+2
c                                           e0
      elseif ((words(i).eq.'e0').or.(words(i).eq.'ee')) then
          call getrea(words(i), words(i+1), e0)
          i=i+2
c                                           pre-edge boundries
      elseif (words(i).eq.'pre1') then
          call getrea(words(i), words(i+1), predg1)
          i=i+2
      elseif (words(i).eq.'pre2') then
          call getrea(words(i), words(i+1), predg2)
          i=i+2
c                                           normalization boundries
      elseif (words(i).eq.'nor1') then
          call getrea(words(i), words(i+1), enor1)
          i=i+2
      elseif (words(i).eq.'nor2') then
          call getrea(words(i), words(i+1), enor2)
          i=i+2
c                                           max energy for fit
      elseif (words(i).eq.'emax') then
          call getrea(words(i), words(i+1), emax)
          i=i+2
c                                           format
      elseif (words(i).eq.'format') then
          frmin  = words(i+1)
          frmout = words(i+1)
          call triml(frmin)
          call case(test,frmin)
          call triml(frmout)
          call case(test,frmout)
          i=i+2
c                                           formin
      elseif (words(i).eq.'formin') then
          frmin = words(i+1)
          call triml(frmin)
          call case(test,frmin)
          i=i+2
c                                           formout
      elseif (words(i).eq.'formout') then
          frmout = words(i+1)
          call triml(frmout)
          call case(test,frmout)
          i=i+2
c                                           outfile
      elseif (words(i)(1:3).eq.'out') then
          of = words(i+1)
          i=i+2
c                                               read in data filenames
      elseif ((words(i)(1:3).eq.'fil').or.(words(i)(1:3).eq.'dat'))
     $                then
          nfile = 0
 140      continue
          read(1,1440,end=191)string
          call untab(string)
          call triml(string)
          nnw = nwdx
          do 150 ii=1,nwdx
            words(ii)=' '
 150      continue
          call bwords(string,nnw,words)
          if ((words(1)(:1).eq.'!').or.(words(1)(:1).eq.' ').or.
     $        (words(1)(:1).eq.'%').or.(words(1)(:1).eq.'*')) then
              goto 140
          elseif ((words(1)(1:3).eq.'---').or.(words(1).eq.'end')) then
              goto 191
          elseif ((words(1).eq.'e0').or.(words(1)(1:4).eq.'mess').or.
     $            (words(1).eq.'nofit').or.(words(1)(1:4).eq.'noal').or.
     $            (words(1).eq.'pre1').or.(words(1)(1:4).eq.'norm').or.
     $            (words(1).eq.'pre2').or.(words(1)(1:3).eq.'out').or.
     $            (words(1).eq.'nor1').or.(words(1)(1:3).eq.'fil').or.
     $            (words(1).eq.'nor2').or.(words(1).eq.'data').or.
     $            (words(1).eq.'emax').or.(words(1).eq.'format').or.
     $            (words(1).eq.'ee').or.
     $            (words(1).eq.'formin').or.(words(1).eq.'formout'))
     $                    then
              goto 130
          else
              nfile = nfile+1
              file(nfile) = words(1)
              call triml(file(nfile))

              nf = 2
              if (frmin(1:3).eq.'uwx') then 
                  nf = 3
                  if (istrln(words(2)).le.3) then
                      call getint('nkey',words(2),nkey(nfile))
                  else
                      skey(nfile) = words(2)
                  endif
              endif

c             --- individual file comments
              if ((nnw.ge.nf).and.(words(nf)(1:1).ne.'!').and.
     $                    (words(nf)(1:1).ne.'%').and.
     $                    (words(nf)(1:1).ne.'#').and.
     $                    (words(nf)(1:1).ne.'*')) then
                  commnt(nfile) = string
                  do 160 k=1,nf-1
                    kk   = istrln(words(k))
                    temp = commnt(nfile)(kk+1:)
                    commnt(nfile) = temp
                    call triml(commnt(nfile))
 160              continue 
              endif
              goto 140
          endif
      else
          iunk  = istrln(words(i))
          messg = words(i)(:iunk)//' is an unknown keyword.'
          call messag(messg)
          i=i+2
      endif
c     if read entire line then read next line else read next word in line
      if (i.ge.nwds) goto 101
      goto 130

c     done reading lines
191   continue
      if (inpnul) then
          lend=.true.
          goto 300
      endif

      if (e0.lt.nulval) then
          errmes = 'You must supply an e0 value.'
          goto 666
      endif
      if (nfile.eq.0) then
          errmes = 'You specified no input data.'
          goto 666
      endif

300   continue
      return

 666  continue 
      ii = istrln(errmes)
      call messag('* * * ERROR!  '//errmes(:ii))
      call messag('      Normal is stopping.')
      stop

c end subroutine normin
      end

      subroutine addoc(ndoc, doc, commnt, e0shft, step, slope)
      
      parameter(ndocx=25)
      character*78  commnt
      character*100 doc(ndocx)

      nnd = 1
      if (commnt.ne.' ') nnd = 2
      do 10 i=min(ndoc, ndocx-nnd), 1+nnd, -1
        doc(i+1) = doc(i)
 10   continue 

      if (commnt.ne.' ') then
          write(doc(2),400)commnt
 400      format('NORMAL: ',a)
      endif

      write(doc(1+nnd),410)e0shft, step, slope
 410  format('NORMAL: E0 shift = ',f7.2,' step and post-edge slope: ',
     $            f8.3,',',f9.5)

      return 
c  end subroutine addoc      
      end
      subroutine edgfit(e0shft, step, slope)

      parameter(nptx=2**11, epsi=0.01e0)
      parameter(valnul=-99 999.e0, elim=100.e0)
c  elim is default energy limit above edge for the fit
      
      dimension     energy(nptx), xmuout(nptx)
      dimension     eref(nptx), xref(nptx)
      common /idata/ nref, nxmu, nmin, nmax
      common /rdata/ e0, emin, emax, eref, xref, energy, xmuout

c  stuff for lmdif
      parameter(nguesx = 2)
      parameter (lwa = nptx*nguesx + 5*nguesx + nptx)
      character*5 cnum
      real    toler, wa(lwa), fvec(nptx)
      integer iwa(nguesx)
      real    xguess(nguesx)
      external edgfun

 400  format(i5)

c  if emax not set, set it to elim (a few tens of volts above the edge)
c  be careful not to exceed bounds of data set
      nmin = nofx(emin, eref, nref)
      if (emax.lt.valnul) emax = elim
      emx  = e0+emax
      if (emx.gt.eref(nref)) then
          nmax = min(nref, nxmu)
      else
          nmax = nofx(emx, eref, nref)
          nmax = min(nmax, nxmu)
      endif

c  setup for lmdif
c  xguess(1) is the e0 shift, xguess(2) is the step, 
c  xguess(3) is the post-edge slope
      toler = 1e-5
      kvar  = nguesx
      kfit  = nmax - nmin + 1
      xguess(1) = e0shft
      xguess(2) = step
c      xguess(3) = slope

      call lmdif1(edgfun, kfit, kvar, xguess, fvec, toler,
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

      e0shft = xguess(1)
      step   = xguess(2)
      slope  = 0 
c      slope  = xguess(3)

      return 
c  end subroutine edgfit
      end
      subroutine edgfun(kfit, kvar, xguess, fvec, iflag)

c-----------------------------------------------------------------------
c  evaluation of sum of functions for least-squares minimization
c-----------------------------------------------------------------------

      integer kfit, kvar, iflag
      real    xguess(kvar), fvec(kfit)

      parameter (nptx=2**11, epsi=1.e-3)
      dimension energy(nptx), xmuout(nptx)
      dimension eref(nptx), xref(nptx)
      common /idata/ nref, nxmu, nmin, nmax
      common /rdata/ e0, emin, emax, eref, xref, energy, xmuout

      nterms = 4
      do 50 i=1,kfit
        ii = i+nmin-1
        ee = eref(ii) + xguess(1)
        call interp(energy,xmuout,nxmu,nterms,ee,xx)
        factor  = 0.e0
c        if (ee.gt.e0) factor = xguess(3) * (ee-e0)
        func    = (xx / xguess(2)) + factor
        fvec(i) = xref(ii) - func
 50   continue 

      return 
c  end subroutine edgfun
      end
      function xmuint(x)

c  interpolate to find the value of xmu at any energy

      parameter(nptx=2**11)
      dimension     energy(nptx), xmuout(nptx)
      dimension     eref(nptx), xref(nptx)

      common /idata/ nref, nxmu, nmin, nmax
      save /idata/
      common /rdata/ e0, emin, emax, eref, xref, energy, xmuout
      save /rdata/
      
      nterms = 3
      call interp(energy,xmuout,nxmu,nterms,x,y)
      xmuint = y
      return 
c  end function xmuint
      end
      subroutine addocd(ndoc, doc)
      
      parameter(ndocx=25)
      character*100 doc(ndocx)

      do 10 i=min(ndoc, ndocx-1), 2, -1
        doc(i+1) = doc(i)
 10   continue 

      doc(2) = "derivative function"

      return 
c  end subroutine addocd
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
c $Id: misc.f,v 1.1.1.1 1997/04/27 20:18:02 ravel Exp $
c $Log: misc.f,v $
c Revision 1.1.1.1  1997/04/27 20:18:02  ravel
c Initial import of xanes sources, version 0.37
c
c Revision 1.1  1996/06/23 16:05:02  bruce
c Initial revision
c
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
      jlen = istrln(string)
c 
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
      string = string (i:)
      return
c end subroutine triml 
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
      subroutine bwrds2 (s, nwords, words)
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
       logical function isnum (string)
c  returns true if string can be a number, else returns false 
c  recognizes e and d exponentials, bit is not foolproof
c  to be a number, a string must contain:
c     - only characters in  'de.+-, 1234567890' (case is checked)
c     - no more than one 'd' or 'e' 
c     - no more than one '.'
       character*(*)  string, str*70, number*20
       integer        iexp, idec, i, ilen, ier, j, istrln
       real           x
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
      subroutine messag(messg)
c  write message to both standard ouput and to unit 2 
c  unit 2 must be opened already!
      character*(*) messg
      write(*,10)   messg
 10   format(1x,a)
      return
c end subroutine messag
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
      subroutine case(test,word)
c  returns *word* in the same case as *test*
c  note that this is just the reverse of smcase !
      character*(*) test, word
      call smcase (word, test)
      return
c  end subroutine case
      end
c---------------------------------------------------------------- 
      function  nofx(x,array,npts)

      implicit integer(i-n)
c      implicit real(a-h,o-z)
      implicit real(a-h,o-z)

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
c---------------------------------------------------------------
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
          messg = 'Error reading '//string(:j)//' as the value for '//
     $                keywrd(:k)
          call messag(messg)
          messg = string(:j)//' must be a real number.'
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

c=======================================================================
      subroutine z2s(isym,elem)
c---------------------------------------------------------------------------
c  copyright 1993 university of washington     matt newville and bruce ravel
c---------------------------------------------------------------------------

c     returns atomic symbol from z number:  default is '  '
      character*2 elem,symbol(103)
c
      data (symbol(i),i=1,103) /'h','he','li','be','b','c','n','o',
     $'f','ne','na','mg','al','si','p','s','cl','ar','k','ca','sc',
     $'ti','v','cr','mn','fe','co','ni','cu','zn','ga','ge','as','se',
     $'br','kr','rb','sr','y','zr','nb','mo','tc','ru','rh','pd','ag',
     $'cd','in','sn','sb','te','i','xe','cs','ba','la','ce','pr','nd',
     $'pm','sm','eu','gd','tb','dy','ho','er','tm','yb','lu','hf','ta',
     $'w','re','os','ir','pt','au','hg','tl','pb','bi','po','at','rn',
     $'fr','ra','ac','th','pa','u','np','pu','am','cm','bk','cf','es',
     $'fm','md','no','lr'/

      elem = '  '
      if ((isym.ge.1).and.(isym.le.103)) elem = symbol(isym)

      return
      end

      subroutine fixsym(sym)

c  returns a word with the first letter capitalized and the remaining
c  letters in lower case
c  this is very useful for writing out atomic symbols

      character*2 toss, sym*(*)

      toss = sym
      call upper(toss(1:1))
      call lower(toss(2:2))
      ii   = istrln(sym)
      if (ii.gt.2) then
          call lower(sym(3:ii))
          sym  = toss(1:1)//toss(2:2)//sym(3:ii)
      else
          sym  = toss(1:1)//toss(2:2)
      endif
      return 
c  end subroutine fixsym
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
        subroutine interp(x,y,npts,nterms,xin,yout)
c
c       interpolation routine from bevington's book
c
        double precision deltax,delta,a,prod,sum
        dimension x(*),y(*)
        dimension delta(10),a(10)
c
c        search for  appropriate value of x(1)
c
11      do 19 i=1,npts
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
61      return
c end subroutine interp
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
       double precision work(maxpts), sum(4), zero
       double precision pi, gold, goldm1, e1, aver, part
       parameter(zero = 0.d0, pi = 3.141592653589793d0 )
       parameter(e1= 2.7182818280)
       parameter(gold = 1.618033989d0, goldm1 = 0.618033989d0)
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
           sum(i)  = zero
 18    continue
 
c get measure of the magnitude of the data two different ways
c        ihalf ~= imod/2 , so that roughly half the values
c                          are used to make the partial sum
       aver  = 1.d-1
       part  = 2.d-1
       do 30 i = 1, narray
          aver  = abs( dble(array(i)) / narray ) + aver
          irndm = mod(imul*(i + narray)  + iadd, imod)
          if ( (i.gt.2) .and. (irndm.gt.ihalf) ) then
             part = abs( dble(array(i)) / narray ) + part
             if (irndm.gt.imost) then
                 part = abs( dble(array(i-2)) / narray ) + part
              else
                 part = abs( dble(array(i-1)) / narray ) + part
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
 
       if (abs(aver).le.1.d-3) aver = 1.d-2
       if (abs(part).le.1.d-3) part = 1.d-2
       do 150 i = 1, narray
          loop           = .true.
          work(i)        = abs( dble(array(i)) / aver )
          work(i+narray) = abs( dble(array(i)) / part )
          j              = i*kmul + kadd + narray
          jrndm          = mod(jmul*j  + jadd, jmod)
          if ( jrndm.lt.jthrd ) then
              work(i) = work(i) * pi
          elseif ( jrndm.gt.jhalf ) then
              j              =  mod(i*j + jrndm, narray - 1 ) + 1
              work(i+narray) = abs( dble(array(i)+array(j)) / e1)
100           continue
                if(loop.and.(work(i+narray).le.(0.0072974d0))) then
                   work(i+narray) = work(i+narray) * gold
                   loop = .false.
                   go to 100
                elseif(loop.and.(work(i+narray).ge.( 137.036d0))) then
                   work(i+narray) = work(i+narray) * goldm1
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
              sum(i) = sum(i) / gold
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
      function dfridr(func, x, h, err)

      integer  ntab
      real     dfridr, err, h, x, func, con, con2, big, safe
      parameter(con=1.4, con2=con*con, big=1.e30, ntab=10, safe=2.)
      external func
      integer  i, j
      real     errt, fac, hh, a(ntab,ntab)

      if (h.eq.0) then
          print*,'h must be non-zero in dfridr'
          stop
      endif

      hh = h
      a(1,1) = (func(x+hh) - func(x-hh)) / (2.0*hh)
      err = big
      do 120 i=2,ntab
        hh = hh/con
        a(1,i) = (func(x+hh) - func(x-hh)) / (2.0*hh)
        fac = con2
        do 110 j=2,i
          a(j,i) = (a(j-1,i)*fac - a(j-1,i-1)) / (fac-1.0)
          fac = con2*fac
          errt = max( abs(a(j,i)-a(j-1,i)), abs(a(j,i)-a(j-1,i-1)) )
          if (errt.lt.err) then
              err = errt
              dfridr = a(j,i)
          endif
 110    continue 
        if (abs(a(i,i)-a(i-1,i-1)).ge.safe*err) return 
 120  continue 

      return
c  end function dfridr
      end
      subroutine getnam(infile, ext, outfil)

c----------------------------------------------------------------------
c  create and output file name from an input file name and a choice
c  for output file extension.  
c
c  input:
c    infile:  input file name
c    ext:     output extension, doesn't need a dot
c  output:
c    outfil:  output file name
c----------------------------------------------------------------------

      character*(*) infile, ext, outfil

      idot = index(infile,'.')
      if (idot.eq.0) then
          ii = istrln(infile)
          outfil = infile(:ii)//'.'//ext
      else
          outfil = infile(:idot)//ext
      endif

      return
c  end subroutine getnam
      end
       subroutine preedg(eefind, ee, predg1, predg2, enor1, enor2,
     $    nxmu, energy, xmuraw, step, stfind, slopre, bpre, xmuout)
c
c     pre-edge subtraction and normalization of exafs data.
c
c inputs:
c   eefind   logical flag for picking e0 at the maximum derivative
c   ee       e0,  energy origin of data
c   predg1   region for picking pre-edge line
c   predg2   region for picking pre-edge line
c   enor1    region for picking normalization
c   enor2    region for picking normalization
c   nxmu     length of array energy, xmuraw, and xmuout
c   energy   array of energy points
c   xmuraw   array of raw absorption points
c   step     edge step for normalization (found if zero on input)
c   stfind   logical flag to tell whether to find edge step or not
c outputs:
c   ee       e0,  energy origin of data
c   predg1   region used for picking pre-edge line
c   predg2   region used for picking pre-edge line
c   enor1    region used for picking normalization
c   enor2    region used for picking normalization
c   step     edge step for normalization
c   slopre   slope of pre-edge line
c   bpre     intercept of pre-edge line (value at ee)
c   xmuout   array of unnormalized absorption points
c            after pre-edge line subtracted
c
c   requires subroutine polyft (and so also function nofx)
c
c   copyright 1992  university of washington :          matt newville
c-----------------------------------------------------------------
       logical     eefind, stfind, incre(4), incall, eestop
       dimension   energy(nxmu), xmuraw(nxmu), xmuout(nxmu), anorm(2)
c
c   if ee was not specified, it is found in the data from
c   these conditions:
c       1) ee is the third increasing point in a row.
c       2) ee has the largest derivative up to that point.
c   these conditions will almost always pick a point on the
c   absorption step, and will fail less often than simply
c   taking the point with maximum derivative.
c
       if (eefind) then
          do 100 j = 1, 4
             incre(j) = .false.
100       continue
          eestop = .false.
          dxde   = 0.
          dermax = 0.
          ntry   = int(nxmu/2) - 5
          nguess = int(ntry/3)
          ee = energy(nguess)
          dxde1 = ( xmuraw(5)-xmuraw(2) ) / ( energy(5)-energy(2) )
          do 150 i = 2, ntry
             deltax = xmuraw(i) - xmuraw(i-1)
             deltae = energy(i) - energy(i-1)
             if (deltae.ne.0) then
                    dxde = deltax/deltae
             else
                    dxde = 0.0
             end if
             if (dxde.gt.dxde1) then
                    incre(1) = .true.
             else
                    incre(1) = .false.
             end if
             incall = incre(4).and.incre(3).and.incre(2)
             if ( (dxde.gt.dermax).and.(.not.eestop).and.incall ) then
                ee = energy(i)
                dermax = dxde*(1.05)
             end if
             if (incre(4).and.(.not.incre(3)).and.(.not.incre(2))
     $           .and.(.not.incre(1)).and.(.not.eestop) ) then
                eestop = .true.
                go to 200
             end if
             do 130 j = 4, 2, -1
                   incre(j) = incre(j - 1)
130          continue
150       continue
       end if
200    continue
c
c  at this point, ee is known. so we go on
c  to do the pre-edge  and normalization
c
c  get pre-edge line (slope and intercept) by linear fit
       if ( (predg1.eq.0).and.(predg2.eq.0) )  then
             predg1 = - 200.0
             predg2 = - 50.0
       end if
 
       pre1 = ee + predg1
       pre2 = ee + predg2
       call polyft(pre1, pre2, energy, xmuraw, nxmu, 2, anorm)
       bpre   = anorm(1)
       slopre = anorm(2)
c
 
c  apply e0 shift, do pre-edge subtraction, and calculate
c  the energy relative to edge for easier normalization
       do 300 i = 1, nxmu
          xmuout(i)  = xmuraw(i) - bpre - slopre*energy(i)
          energy(i)  = energy(i) - ee
300    continue
 
c  normalization : make pre-edge 0.0 and post-edge 1.0
c    if step size wasn't given, get it by extracting to ee a
c    line that best fits the data on the range (ee+enor1,ee+enor2)
       if (stfind) then
          if ( (enor1.eq.0).and.(enor2.eq.0) )   then
                enor2 = 300.0
                enor1 = 100.0
          end if
          call polyft(enor1, enor2, energy, xmuout, nxmu, 2, anorm)
          step   = anorm(1)
       end if
       if (step.eq.0.) step = 1.0
c  put energy array back to original values, 
c  recover from bad pre-edge fits
       do 500 i = 1, nxmu
           energy(i)  = energy(i) + ee
c                            for "post-edge" pre-edge fit:
           if (predg1.gt.(0.0)) then
               xmuout(i) = 1.0 + xmuout(i)
           end if
500    continue
 
       return
c end subroutine preedg
       end
       subroutine polyft(xfit1,xfit2,xdata,ydata,ndata,nterms,aout)
c
c  get coefficients for polynomial fit :
c      ydata = aout(1) + aout(2)*xdata  + aout(3) *xdata^2 + ...
c  the fit is done between xdata = xfit1 and xfit2
c
c  inputs :
c    xfit1    lower bound of fitting range
c    xfit2    upper bound of fitting range
c    xdata    array of abscissa values for data
c    ydata    array of ordinate values for data
c    ndata    length of data arrays
c    nterms   number of terms in polynomial
c
c  outputs :
c    aout     coefficients of fitted polynomial to data
c
c   copyright 1992  university of washington :          matt newville
c
c  requires function nofx
c
c  see bevington pg 104 for expalanation of these variables
c
      implicit integer(i-n)
      implicit real(a-h,o-z)
c      implicit double precision(a-h,o-z)

       parameter (max= 5, max2m1 = 2*max-1, zero = 0.) 
       dimension         xdata(ndata), ydata(ndata), aout(nterms)
       double precision  sumx(max2m1), sumy(max)
       double precision  array(max,max), ain(max), delta, determ
       external          determ
c
c find points closest to endpoints of fitting range
       nfit1 = nofx(xfit1,xdata,ndata)
       nfit2 = nofx(xfit2,xdata,ndata)
       if (nfit1.gt.nfit2) then
            ntemp = nfit1
            nfit1 = nfit2
            nfit2 = ntemp
       elseif(nfit1.eq.nfit2) then
            go to 300
       end if
c
c   initialize internal arrays
       nmax   = 2 * nterms - 1
       do 100 i=1, nmax
          sumx(i) = zero
 100   continue
       do 110 i = 1, nterms
          ain(i) = zero
          sumy(i) = zero
          do 110 j = 1,  nterms
            array(i,j) = zero       
  110  continue
c
c  collect sums of data, sum of squares of data, etc.
       do 200 i = nfit1, nfit2 
          xi = xdata(i)
          yi = ydata(i)
          xterm = 1.0
          do 180 n=1, nmax
             sumx(n) = sumx(n) + xterm
             xterm   = xterm * xi
  180     continue
          yterm = yi
          do 190 n=1,nterms
             sumy(n) = sumy(n) + yterm
             yterm   = yterm * xi
  190     continue 
  200  continue
c
c construct matrices and evaluate coefficients
c
       do 210 j=1,nterms
         do 210 k=1,nterms
            array(j,k) = sumx(j + k - 1)
  210  continue 
       delta = determ(array,nterms,max)
       if (delta.ne.zero) then
           do 260 l=1,nterms
              do 250 j=1,nterms
                 do 240 k=1,nterms
                    array(j,k) = sumx(j+k-1)
  240            continue
                 array(j,l) = sumy(j)
  250         continue
              ain(l) = determ(array,nterms,max)/delta
  260      continue
       end if
  300  continue
       do 400 i = 1, nterms
          aout(i) = sngl(ain(i))
  400  continue
       return
c end  subroutine polyft
       end


      double precision function determ(array,nord,nrows)
c
c     calculate determinate of a square matrix
c        (from bevington "data reduction and error analysis
c         for the physical sciences" pg 294)
c     array: matrix to be analyzed
c     nterms: order of matrix
c     nrows:  first dimension of matrix in calling routine
c
      double precision array(nrows,nrows)
      determ = 1.
      do 150 k=1,nord

        if (array(k,k).ne.0) go to 130
        do 100 j=k,nord
          if (array(k,j).ne.0) go to 110
  100   continue
        determ = 0.
        go to 160
c
  110   do 120 i=k,nord
          save = array(i,j)
          array(i,j) = array(i,k)
  120   array(i,k) = save
        determ = -determ
c
c
  130   determ = determ*array(k,k)
        if (k.ge.nord) go to 150
        k1 = k+1
        do 140 i=k1,nord
          do 140 j=k1,nord
  140   array(i,j) = array(i,j)-array(i,k)*array(k,j)/array(k,k)
  150 continue
  160 return
c end double precision function determ 
      end
c                       minpack routines 
c  the following are routines from minpack to solve a non-linear 
c  least squares problem using the levenberg-marquardt method.
c  
c     argonne national laboratory. minpack project. march 1980.
c     burton s. garbow, kenneth e. hillstrom, jorge j. more
c
c  the routines were taken form the public domain service 
c  netlib using internet and the following commands:
c  mail
c  to  :  in%"netlibd@research.att.com" 
c  from:  newville@gibbs.phys.washington.edu
c  cc  :
c  subj:  send lmdif1 from sminpack
c----------------------------------------------------------------------
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
c----------------------------------------------------------------------
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
c----------------------------------------------------------------------
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
c----------------------------------------------------------------------
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
c----------------------------------------------------------------------
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
c----------------------------------------------------------------------
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
c----------------------------------------------------------------------
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
c---------------------------------------------------------------------- 
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
c--------------------------------------------------------------------
       subroutine gaussj(a, n, ma, ier) 
c
c          gauss-jordan elimination to invert a matrix. 
c          based on a routine in 'numerical recipes' by
c          press, flannery, teukolsky, and vetterling.
c  inputs :
c    a     matrix to invert
c    n     number of elements in a to use (i.e. that are'nt zero)
c    ma    dimension of array a  (current maximum is 200 x 200)
c  outputs:
c    a     inverted matrix if ier = 0    
c          input matrix    if ier = 1
c    ier   error code, set to 1 if matrix cannot be inverted
c
c                                              matt newville
c------------------------------------------------------------------
       parameter (nmax = 200)
       parameter (zero = 0.e0, one = 1.e0)
       dimension a(ma, ma) 
       dimension ipiv(nmax),indxr(nmax),indxc(nmax) 
       dimension asav(nmax, nmax)
c      
       ier = 0
       if (n.gt.nmax) then
          call messag('gaussj error: matrix too big, change size ')
          ier  = 1
          return
       end if
c  keep a spare version of the input matrix around just in case
       do 30 i = 1, n
          do 20 j = 1, n
             asav(i,j) = a(i, j)
  20      continue    
  30   continue
c   initialize ipiv
       do 60 j = 1,  n 
           ipiv(j) = 0 
  60   continue 
c
c    main loop over the columns to be reduced
c
       do 300 i = 1, n 
          big = zero
c                       outer loop of the search for a pivot element
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
c                       a pivot has been successfully found
          if (irow.ne.icol) then 
             do 160 l = 1, n 
                dum = a(irow, l) 
                a(irow, l) = a(icol, l) 
                a(icol, l) = dum 
 160         continue 
          endif 
c                       divide the pivot row by the pivot element, 
c                       located at irow, icol
          indxr(i) = irow 
          indxc(i) = icol 
          if (a(icol, icol).eq.zero) then
              ier = 1
              go to 500
          end if 
          pivinv = one / a(icol, icol) 
          a(icol,icol) = one 
          do 200 l = 1, n 
               a(icol, l) = a(icol, l) * pivinv 
 200      continue 
c                       reduce the rows except for the pivot one
         do 250 ll = 1, n 
             if (ll.ne.icol) then 
                 dum  = a(ll, icol) 
                 a(ll,icol) = zero 
                 do 220 l = 1, n 
                     a(ll,l) = a(ll,l) - a(icol,l) * dum 
 220             continue 
             endif 
 250      continue 
 300   continue 
c
c   unscramble the solution, by interchanging column pairs
c   in the reverse order that the permutation was done
c
       do 400 l = n, 1, -1 
          if (indxr(l) .ne. indxc(l)) then 
             do 350 k = 1, n 
                 dum = a(k,indxr(l)) 
                 a(k,indxr(l)) = a(k,indxc(l)) 
                 a(k,indxc(l)) = dum 
 350        continue 
          endif 
 400   continue 
c
c  if any errors happened, restore original matrix
c
 500   continue
       if (ier.ne.0) then 
           do 540 i = 1, n
              do 520 j = 1, n
                a(i,j) = asav(i, j)
 520          continue    
 540       continue
       end if
c
       return 
c  end subroutine gaussj
       end 

c---------------------------------------------------------------------
       real function sumsqr(array, narray)
c  returns sum of squares of an array with dimension narray
       dimension array(*)
       real        big
       parameter  (big = 1.e12)
       sumsqr = 0.
       do 50 i = 1, narray
           if (abs(array(i)).lt.big) then
              sumsqr = sumsqr + array(i)*array(i)
           else
              sumsqr = sumsqr + big*big
           end if
  50   continue
       return
c  end real function sumsqr
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
       data  iulow, iumax /7, 50/
c
c make sure there is a unit number
       ierr   = -3
       iexist = 0
       if (iunit.le.0) then
          iunit  = iulow
 10       continue
          inquire(unit=iunit, opened = opend)
          if (opend) then
             if (iunit.gt.iumax) then
                iunit = min(iumax,iunit)
                return
             end if
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
