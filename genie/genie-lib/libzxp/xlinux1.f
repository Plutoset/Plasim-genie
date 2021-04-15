      subroutine getenvq(text,dir)
      character*(*) text,dir
      character*8 name
c
      integer ilen
      integer lnsig
c
      call getlog(name) 
      ilen=lnsig(name)
      if (text.eq.'HOME') dir='/home/'//name(1:ilen)//'/'
      if (text.eq.'COAST') dir='/home/swsvalde/qplot/'
      if (text.eq.'SCRIPT') dir='/home/swsvalde/qplot/'
c
      end
C
      subroutine xbeep
      end
c
      character*41 function fileattr(filename)
      character filename*(*),utfstr*41
      character*26 try
      integer*4 size
      integer ilen
c
      ilen=len(filename)
      call my_fileattr(filename(1:ilen),try,size)
      utfstr(1:24)=try(1:24)
      utfstr(25:31)=' Size= '
      write(utfstr(32:41),'(i10)')size
      fileattr=utfstr
c
      return
      end
c
      character*24 function fdate()
      character date1*24
      character try*26
c
      call my_date(try)
      date1(1:24)=try(1:24)
      fdate=date1
      end
c
      subroutine getlog(name)
      character name*8
      integer i
c
      do i=1,8
        name(i:i)=' '
      enddo
      call my_getlogg(name)
c
      end
C
      SUBROUTINE WININI (progid)
C
      character*20 progid
C      integer iargc,args
C      character*100 name,arg(3),progid
C
      COMMON/WINDOW/LWIN
      LOGICAL LWIN
C
C      args=iargc()
C      call getarg(0,name)
C      arg(1)=''
C      arg(2)=''
C      arg(3)=''
C      do i=1,args
C         call getarg(i,arg(i))
C      enddo
C      call xwish_init(name,arg(1),arg(2),arg(3),progid)
      if (progid(1:6).eq.'xqplot') then
         LWIN=.TRUE.
      else
         LWIN=.FALSE.
      endif
C
      RETURN
      END
c
      subroutine xtstpc(lpc)
c
c     This subroutine tells qplot6 if it is the pc version
c     Currently used to:
c          (a) tell qplot6 to pause before going on to next frame
c
      logical lpc
c
      lpc=.FALSE.
C
      return
      end
c
      SUBROUTINE XPAGES(IPAGE,ITEXT)
C
C     This subroutine has caused me alot of grief
C     It serves two roles. On the MS Dos/Windows PC it controls
C     whether you are in graphics or text modes. On X-based
C     machines it controls which of the active pages
C     that you are using (X knows them as 0 or 1 but this
C     sibroutine knows them as 1 and 2).
C
C     I've decided to seperate these two functions so :
C 
C          ITEXT= 1     for text mode
C          ITEXT= 0     for don't change mode
C          ITEXT=-1     for graphics mode
C
C     The default is for qplot to be in graphics mode
C     so changes to text mode only need to be considered
C     and graphics mode must always be reset at end of
C     routine.
C
C     ITEXT can be ignored on X-Based systems.
C     
C          IPAGE= 0     for no change
C          IPAGE=-1     for swap active/ pages
C          IPAGE= 1     for page 1
C          IPAGE= 2     for page 2
C          IPAGE= 3     for displaying page
C
      LOGICAL LWIN
      INTEGER IPAGE,IPAGE2,IPAGE3,ITEXT
      SAVE IPAGE2,IPAGE3
      DATA IPAGE2/0/,IPAGE3/0/
C
      CALL XTSTWN(LWIN)
C
      IF (ITEXT.EQ.1) THEN
      ELSE IF (ITEXT.EQ.0) THEN
      ELSE IF (ITEXT.EQ.-1) THEN
      ELSE
         print*,' This value of itext not valid ',ITEXT
      END IF
C
      IF (IPAGE.EQ.0) THEN
C         
      ELSE IF (IPAGE.EQ.-1) THEN
         IPAGE2=1-IPAGE2
         IF (.NOT.LWIN) THEN
            CALL X_UPDATEPAGE(IPAGE2)
            CALL X_DISPLAYPAGE(1-IPAGE2)
         END IF
         CALL X_CLEAR
         IPAGE3=0
      ELSE IF (IPAGE.EQ.3) THEN
         IF (.NOT.LWIN) CALL X_DISPLAYPAGE(IPAGE2)
         IF (IPAGE3.EQ.1) THEN
            IPAGE3=2
         ELSE
            IPAGE3=1
         END IF
      ELSE 
         print*,' this option not yet coded '
         stop 
c         IF (IPAGE3.EQ.1) THEN
c            IF (.NOT.LWIN) CALL X_DISPLAYPAGE(1-IPAGE2)
c            IPAGE3=0
c         ELSE IF (IPAGE3.EQ.2) THEN
c            IPAGE3=1
c         END IF
      END IF
C
      RETURN
C
      ENTRY XWHATUPG(IPAGE)
      IPAGE=IPAGE2
      RETURN
      END
C
      SUBROUTINE XOVER(IOVER,ISWAP)
      INTEGER IOVER
      INTEGER ISWAP
C
      IF (ISWAP.EQ.0) THEN
         IF (IOVER.EQ.1)THEN
            CALL DOVRON
         ELSEIF(IOVER.EQ.-1)THEN
            CALL DSAVOF
         ENDIF
      ELSE IF (ISWAP.EQ.1) THEN
         IF (IOVER.EQ.1)THEN
            CALL XCLEAR
            CALL DOVROF
            IOVER=-1
         ELSEIF(IOVER.EQ.-1)THEN
            CALL DSAVON
            CALL XCLEAR
         ENDIF
      END IF
C
      END
C
      SUBROUTINE CLRSCR
      END





