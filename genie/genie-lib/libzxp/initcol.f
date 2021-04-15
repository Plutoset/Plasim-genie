c
      subroutine initcol(colarray,NCOL,nsteps)
C
      INTEGER I
      INTEGER IOUT
      INTEGER II
      INTEGER NCOL
      INTEGER NCOL1
      INTEGER NSTEPS
      REAL RNCOL
      INTEGER IB1
      INTEGER IB2
      INTEGER IB3
      INTEGER IB4
      INTEGER IB5
      INTEGER NI1
      INTEGER NI2
      INTEGER NI3
C
      integer*2 colarray(3,0:*)
      LOGICAL LEXIST,LOPEN
C
      DO I=0,9
         INQUIRE(UNIT=90+I,OPENED=LOPEN)
         IF (.NOT.LOPEN) THEN
            IOUT=90+I
            GO TO 110
         END IF
         IF (I.EQ.9) THEN
            PRINT*,' Error in INITCOL: '
            STOP
         END IF
      END DO
 110  CONTINUE
C
      INQUIRE(FILE='cmap.dat',EXIST=LEXIST)
      IF(LEXIST)THEN
         print*,' cmap.dat exists. Reading colour map '
         OPEN(UNIT=IOUT,FILE='cmap.dat',STATUS='OLD',
     :          FORM='FORMATTED')
         do ii=1,4000
            read (iout,*,end=300,err=300)colarray(1,15+ii),
     :                    colarray(2,15+ii),
     :                    colarray(3,15+ii)
         end do
 300     ncol=15+ii
         print*,' There are a total number of colours of ',ncol
         close(unit=iout)
      ELSE
         ncol1=26*nsteps+1
         ncol=ncol1+16
         rncol=real(ncol1)
         ib1=nint(rncol)*0.28
         ib2=nint(rncol)*0.40
         ib3=nint(rncol)*0.50
         ib4=nint(rncol)*0.80
         ib5=nint(rncol)
      do i=1,ib5
         if (i.le.ib1) then
            ni1=221
            ni2=nint(221.*real(i-1)/real(ib1))
            ni3=0
         elseif (i.le.ib2) then
            ni1=nint(221.0*sqrt(1.-real(i-ib1)/real(ib2-ib1)))
            ni2=221
            ni3=0
         elseif (i.le.ib3) then
            ni1=0
            ni2=221
            ni3=nint(221.0*sqrt(real(i-ib2)/real(ib3-ib2)))
         elseif (i.le.ib4) then
            ni1=0
            ni2=nint(221.*sqrt(1.-real(i-ib3)/real(ib4-ib3)))
            ni3=221
         else
            ni1=nint(121.*sqrt(real(i-ib4)/real(ib5-ib4)))
            ni2=0
            ni3=221
         endif
         colarray(1,ncol-i)=ni1
         colarray(2,ncol-i)=ni2
         colarray(3,ncol-i)=ni3
      enddo
      endif
C
C     16 special colours.   Black=0, White=1
C                           Red=2, Blue=3, Green=4,
C                           Dark Grey=5, Medium Grey=6, Light Grey=7,
C                           Cyan=8, Lightred=9
C                           Black used for coastlines=10
C                           Black used for graticules=11
C                           Black used for outlines  =12
C
      colarray(1, 0)=0
      colarray(2, 0)=0
      colarray(3, 0)=1
      colarray(1, 1)=255
      colarray(2, 1)=255
      colarray(3, 1)=254
      colarray(1, 2)=254
      colarray(2, 2)=0
      colarray(3, 2)=0
      colarray(1, 3)=0
      colarray(2, 3)=254
      colarray(3, 3)=0
      colarray(1, 4)=0
      colarray(2, 4)=0
      colarray(3, 4)=254
      colarray(1, 5)=63
      colarray(2, 5)=63
      colarray(3, 5)=63
      colarray(1, 6)=127
      colarray(2, 6)=127
      colarray(3, 6)=127
      colarray(1, 7)=220
      colarray(2, 7)=220
      colarray(3, 7)=220
      colarray(1, 8)=0
      colarray(2, 8)=254
      colarray(3, 8)=254
      colarray(1, 9)=254
      colarray(2, 9)=127
      colarray(3, 9)=127
      colarray(1,10)=0
      colarray(2,10)=1
      colarray(3,10)=1
      colarray(1,11)=0
      colarray(2,11)=1
      colarray(3,11)=2
      colarray(1,12)=1
      colarray(2,12)=1
      colarray(3,12)=1
      colarray(1,13)=130
      colarray(2,13)=247
      colarray(3,13)=247
      colarray(1,14)=0
      colarray(2,14)=0
      colarray(3,14)=0
      colarray(1,15)=253
      colarray(2,15)=253
      colarray(3,15)=253
c
      return
      end
