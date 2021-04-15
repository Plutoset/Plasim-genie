C
      SUBROUTINE VWRITE(LVID,IGIF)
C
      include 'cframe.inc'
      include 'colours.inc'
C
      INTEGER NCOL1
      INTEGER IFILE_CON
      INTEGER IVERT
      INTEGER I
      INTEGER J
      INTEGER ILEN
      INTEGER LNSIG
      INTEGER ITOTAL
      INTEGER ISEC
      INTEGER ITIME
      INTEGER ITEMP
C
      LOGICAL LVID,LEXIST,LOPEN
      INTEGER IGIF
      INTEGER*4 FRAME( (nrasmx/2)*(nrasmy/2) )
      EQUIVALENCE(CFRAME(1),FRAME(1))
C
      PARAMETER (NCOL1=256)
      CHARACTER COLPSAR1(3,0:NCOL1-1)
      CHARACTER*13 FILENAME
      character*200 htmlfile,line1
      SAVE IFILE_CON
      DATA IFILE_CON/0/
      SAVE IVERT
      DATA IVERT/0/
C
      IF (LVID) THEN
         write(NUNITV) (frame(i),i=1,(nrasx/2)*(nrasy/2))
      END IF
C
      IF (IGIF.NE.0) THEN
         IF (GIFARRAY(1,0).EQ.-999) THEN 
            DO I=0,NCOL-1
               DO J=1,3
                  COLPSAR1(J,I)=CHAR(COLARRAY(J,I))
               END DO
            END DO
         ELSE
            DO I=0,NCOL-1
               DO J=1,3
                  COLPSAR1(J,I)=CHAR(GIFARRAY(J,I))
               END DO
            END DO
         END IF
         IF (NCOL.LT.256) THEN
            DO I=NCOL,NCOL1-1
               DO J=1,3
                  COLPSAR1(J,I)=CHAR(0)
               END DO
            END DO
         END IF
         filename='qpl12345.gif '
         write(filename(4:8),'(i5.5)')ifile_con
         ifile_con=ifile_con+1
         if (ivert.eq.1) then
            if (nrasx*nrasy.le.nrasmx*nrasmy/2) then
               call gifswap(cframe,cframe(nrasx*nrasy+1),nrasx,nrasy)
            endif
            call my_GIFMAKER(COLPSAR1,NCOL1,NRASY,NRASX,
     :                       CFRAME,FILENAME(1:12))
         else
            call my_GIFMAKER(COLPSAR1,NCOL1,NRASX,NRASY,
     :                       CFRAME,FILENAME(1:12))
         end if
c
         inquire(file='makehtml',exist=lexist) 
         if (lexist) then
            print*,' About to open makehtml file '  
            inquire(unit=99,opened=lopen)
            if (.not.lopen) then
               print*,' About to read makehtml file '  
               open(unit=99,file='makehtml')
               read(99,'(a)')htmlfile
               ilen=lnsig(htmlfile)
               read(99,*)itotal
               read(99,*)isec
               close(unit=99)
               print*,htmlfile(1:ilen)
               itime=itotal*isec + 20 - ifile_con*isec
               open(unit=99,file=htmlfile(1:ilen),
     :              status='unknown')
               do i=1,5
                  read(99,'(a)')line1
               end do
               print*,':',line1(1:7)
               if (line1(1:7).ne.'</html>'.and.
     :             line1(1:7).ne.'</HTML>') then
                  do i=1,8
                     read(99,*)
                  end do
                  write(99,*)' It will take about ',itime,
     :                       ' secs to create the images '
                  write(99,*)'</h3></body></html>'
                  write(6,*)' It will take about ',itime,
     :                       ' secs to create the images '
               end if
               close(unit=99)       
            else
               print*,' makehtml file channel already open '
               print*,' stopping program. try again        '
               stop 1 
            end if
         end if   
      END IF
      RETURN
C
      ENTRY VWRITE_SW(ITEMP)
C
      IVERT=ITEMP
C
      RETURN
      END
