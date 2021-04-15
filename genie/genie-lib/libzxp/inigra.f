C
      SUBROUTINE INIGRA(ITYPE)
      INCLUDE 'f77pc.inc'
      include 'mappings.inc'
      include 'colours.inc'
C
      INTEGER NDISSET
      INTEGER ITYPE
      REAL UNICOL
      INTEGER IANGLE
      INTEGER IFN1
      INTEGER IFN2
      INTEGER IFN3
      INTEGER IEG
      INTEGER NX1
      INTEGER NY1
      REAL XTEMP
      INTEGER NXT
      INTEGER NYT
      INTEGER II
      INTEGER J
      INTEGER IDISSET
      INTEGER IT
      INTEGER NX
      INTEGER NY
      INTEGER NCOL1
      INTEGER INDEX
      REAL R
      REAL G
      REAL B
C
      INTEGER*2 COLARRAY1(3,0:255)
      INTEGER NEWCOLS(3,*)
C
      DATA NDISSET/0/
      SAVE NX1,NY1,NDISSET
C
      if (xinited.eq.1) then
         return
      endif
C
      ITYSCR=IABS(ITYPE)
      ISAVE=0
      LSAVE=.FALSE.
      UNICOL=-2
      ICOLPC=1
      IANGLE=0
      ISKIP=1
C
      XLHS=0.0
      XRHS=1.9/1.5
      YBOT=0.0
      YTOP=1.0
C
      IFN1=16
      IFN2=4
      IFN3=16
C
      IDASH=0
      IDASHT=0
      DASHT=0.0
      EPS=1.E-4
C
      IADAPT=5
c
      NSTEP_COL=5
c
      print*,' Adapter type ',iadapt
C
         print*,' For  CGA screen resolution ( 320x200) enter 2 '
         print*,' For  EGA screen resolution ( 640x350) enter 3 '
         print*,' For  VGA screen resolution ( 640x480) enter 4 '
         print*,' For SVGA screen resolution ( 800x600) enter 5 '
         print*,' For XVGA screen resolution (1024x768) enter 6 '
         print*,' Enter negative value if do not wish screen    '
         call flush(6)
         if (ndisset.eq.0) then
            read(5,*)ieg
         else
            ieg=ndisset
         end if
c
         if (ieg.eq.20) then
            print*,' For  CGA screen resolution ( 320x200) enter 2 '
            print*,' For  EGA screen resolution ( 640x350) enter 3 '
            print*,' For  VGA screen resolution ( 640x480) enter 4 '
            print*,' For SVGA screen resolution ( 800x600) enter 5 '
            print*,' For XVGA screen resolution (1024x768) enter 6 '
            print*,' Enter negative value if do not wish screen    '
            call flush(6)
            read(5,*)ieg
            print*,' Enter two font types '
            call flush(6)
            read(5,*)ifn1,ifn2
         end if
c
         if (ieg.ge.11.and.ieg.le.19) then
            nstep_col=1
            ieg=ieg-10
         else if (ieg.ge.-19.and.ieg.le.-11) then
            nstep_col=1
            ieg=ieg+10
         else if (ieg.eq.-21.or.ieg.eq.21.or.
     :            ieg.eq.-22.or.ieg.eq.22.or.
     :            ieg.eq.-23.or.ieg.eq.23) then
            nstep_col=1
         end if  
c
         if (IABS(ieg).eq.0) then
            xscrr=799.0
            yscrb=599.0
            oblate=0.933
            iega=5
            NX1=800
            NY1=600
         else if (IABS(ieg).eq.10) then
            xscrr=639.0
            yscrb=479.0
            oblate=0.89
            iega=3
            NX1=640
            NY1=480
         else if (IABS(ieg).eq.2) then
            xscrr=319.0
            yscrb=199.0
            oblate=0.89
            iega=1
            NX1=320
            NY1=200
         else if (IABS(ieg).eq.3) then
            xscrr=639.0
            yscrb=349.0
            oblate=0.89
            iega=2
            NX1=640
            NY1=350
         else if (IABS(ieg).eq.4) then
            xscrr=639.0
            yscrb=479.0
            oblate=0.89
            iega=3
            NX1=640
            NY1=480
         else if (IABS(ieg).eq.5) then
            xscrr=799.0
            yscrb=599.0
            oblate=0.933
            iega=5
            NX1=800
            NY1=600
         else if (IABS(ieg).eq.6) then
            xscrr=1023.0
            yscrb=767.0
            oblate=1000./1024.
            iega=6
            NX1=1024
            NY1=768
         else if (IABS(ieg).eq.7) then
            xscrr=1279.0
            yscrb=959.0
            oblate=0.89
            iega=7
            NX1=1280
            NY1=960
         else if (IABS(ieg).eq.8) then
            xscrr=639.0
            yscrb=479.0
            oblate=0.95
            iega=3
            NX1=640
            NY1=480
         else if (IABS(ieg).eq.9) then
            xscrr=319.0
            yscrb=199.0
            oblate=0.8
            iega=1
            NX1=320
            NY1=200
         else if (IABS(ieg).eq.22) then
            xscrr=479.0
            yscrb=359.0
            oblate=0.95
            iega=1
            NX1=480
            NY1=360
         else if (IABS(ieg).eq.21) then
            xscrr=319.0
            yscrb=239.0
            oblate=0.95
            iega=1
            NX1=320
            NY1=240
         else if (IABS(ieg).eq.23) then
            xscrr=800.0
            yscrb=800.0
            oblate=1.0
            iega=1
            NX1=800
            NY1=800
         end if
c
         IMON=0
         XTEMP=xscrr
         XSCRR=xscrr*OBLATE
         XSCRL=0.0
         YSCRT=0.0
         if (ieg.lt.0) then
            nxt=-nx1
            nyt=-ny1
            call X_DisplayInit(nxt,nyt,oblate,ifn1,ifn2,ifn3)
         else if (ieg.eq.0.or.ieg.eq.10) then
            oblate=-oblate
            call X_DisplayInit(nx1,ny1,oblate,ifn1,ifn2,ifn3)
            oblate=-oblate
         else
            call X_DisplayInit(nx1,ny1,oblate,ifn1,ifn2,ifn3)
         end if
C
         call colini
         do ii=0,NCOL-1
            do j=1,3
               colarray1(j,ii)=256*(1+colarray(j,ii))-1
            end do
         end do
C           call X_freecolours 
C           call X_setupcolours(1,NCOL,colarray1)
         call x_getcolours(ncol)
         call x_setcolours(colarray1)
C
         XTXTL=1
         XTXTR=80
         YTXTT=1
         YTXTB=30

         xinited=1
      IF (ITYPE.LT.0) ITYSCR=10 + ITYSCR
c
c     extra bit of code for extended use
c
      CALL X_CLEAR
C
      XTXTR=XTXTR*OBLATE
C
      DASHCR=YSCRB/XSCRR
C
      XRATIO=(XSCRR-XSCRL)/(XRHS-XLHS)
      YRATIO=(YSCRT-YSCRB)/(YTOP-YBOT)
C
      XRTTXT=(XTXTR-XTXTL)/(XSCRR-XSCRL)
      YRTTXT=(YTXTT-YTXTB)/(YSCRT-YSCRB)
C
      XSPACE1=XSCRL
      XSPACE2=XSCRR
C
      YSPACE1=YSCRB
      YSPACE2=YSCRT
C
      XTXACE1=XTXTL
      XTXACE2=XTXTR
C
      YTXACE1=YTXTB
      YTXACE2=YTXTT
      RETURN
C
      entry disset(idisset)
      ndisset=idisset
      return
C
      ENTRY XSCRPC(IT)
      IT=ITYSCR
      IF (ITYSCR.GT.10) IT=-(ITYSCR-10)
      RETURN
C
      entry xresol(nx,ny)
      nx=nx1
      ny=ny1
      return
C
      entry xresolr(nx,ny)
      nx=nint(xscrr)
      ny=nint(yscrb)
      return
C
      entry colini1(newcols,ncol1)
      call colini
C
      ncol1=ncol-noffset
      do ii=1,ncol1
         do j=1,3
            newcols(j,ii)=colarray(j,noffset+ii-1)
         end do
      end do
C
      do ii=0,NCOL-1
         do j=1,3
            colarray1(j,ii)=256*(1+colarray(j,ii))-1
         end do
      end do
C     call X_freecolours
C     call X_setupcolours(1,NCOL,colarray1)
      call x_setcolours(colarray1)
      return
C
      entry colset(newcols,ncol1)
         do ii=1,ncol1
            do j=1,3
               colarray(j,noffset+ii-1)=newcols(j,ii)
            end do
         end do
         ncol=noffset+ncol1
         do ii=0,NCOL-1
            do j=1,3
               colarray1(j,ii)=256*(1+colarray(j,ii))-1
            end do
         end do
C         call X_freecolours 
C         call X_setupcolours(1,NCOL,colarray1)
         call x_setcolours(colarray1)
      return
C
      ENTRY SPCOLSET(INDEX,R,G,B)
      COLARRAY(1,INDEX)=256*R
      COLARRAY(2,INDEX)=256*G
      COLARRAY(3,INDEX)=256*B
      do ii=0,NCOL-1
         do j=1,3
            colarray1(j,ii)=256*(1+colarray(j,ii))-1
         end do
      end do
C      call X_freecolours 
C     call X_setupcolours(1,NCOL,colarray1)
      call x_setcolours(colarray1)
      return
      end
