       program lsfile
c
c a quick version of listdf : reads filename from standard input
c
       character*80  filnam, doc, errmsg
       character*10  skey, ftype, aflag, sflag, status
       integer       irecl, nie, iunit
       data     irecl, nie /512, 0 /
       data     filnam, doc, errmsg, skey, ftype
     $      /' ',   ' ', ' ', ' ', ' '/
       data     aflag, sflag, status
     $      /'noabort', 'safe', 'old'/
c     
c  read file name 
       read (*,'(a)') filnam
c     open file 
       call triml(filnam)
       call openfl(iunit, filnam, status, iexist, ierr)
       if (iexist.eq.-1)  then
          errmsg  = 'cannot find file   '//filnam
          ilen    = istrln(errmsg)
          call messag('#  '//errmsg(1:ilen))
          stop
       endif
       if (ierr.ne.0)   then
          errmsg  = 'error opening file   '//filnam
          ilen    = istrln(errmsg)
          call messag('#  '//errmsg(1:ilen))
          stop
       endif
       call openrf (iunit, filnam, aflag, sflag, ftype, irecl, ierr)
c     write out contents
       if (ierr.ne.0) then
          errmsg  = 'not a uwxafs file (?) '//filnam
          ilen    = istrln(errmsg)
          call messag('#  '//errmsg(1:ilen))
          stop
       endif
       call gnie(iunit,nie,ierr)
       call messag('# nkey  npts  ndoc  skey    document')
       do 60 i = 1, nie
          call grlen( iunit,   i,   nw, ierr)
          call gskey( iunit,   i, skey, ierr)
          call gdlen( iunit,   i,   nl, ierr)
          call getdoc(iunit, doc,    1, skey, i, ntl, ierr)
          write(errmsg,50) '#', i, nw, nl, skey, doc(1:50)
 50       format(a,i4,1x,2(i5),2x,a5,2x,a) 
          ilen = min(75, istrln(errmsg))
          call messag(errmsg(1:ilen))
 60    continue
       call closrf (iunit, ierr)
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
c-- find rightmost non-blank, non-null character.
      ilen = len (string)
      do 20  i = ilen, 1, -1
         if ((string (i:i) .ne. ' ') .and.
     $              (string (i:i) .ne. char(0)))  goto 30
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
      subroutine case(test,word)
c  returns *word* in the same case as *test*
c  note that this is just the reverse of smcase !
      character*(*) test, word
      call smcase (word, test)
      return
c  end subroutine case
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
c  write message to both standard ouput and to unit 2 
c  unit 2 must be opened already!
      character*(*) messg
      write(*,10)   messg
 10   format(1x,a)
      return
c end subroutine messag
      end
c---------------------------------------------------------------- 
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
