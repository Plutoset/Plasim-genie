C
      SUBROUTINE GIFCAP(ividi,ipage)
C
      include 'colours.inc'
      LOGICAL LV
      INTEGER IVIDI(*)
C
      INTEGER IPAGE
      INTEGER NRASX
      INTEGER NRASY
      INTEGER NRASCL
      INTEGER IGIF
      INTEGER J
      INTEGER I1I
      INTEGER I1O
      INTEGER I
      INTEGER ITEMP
C
      CALL XCLEAR
      CALL XPAGES(ipage,0)
      CALL XCLEAR
      CALL VIDINI(3)
      CALL VIDRAS(NRASX,NRASY,NRASCL,LV,IGIF)
C
      call x_grabarea(0,0,nrasx-1,nrasy-1,ividi)
C
      DO J=1,NRASY/2
         I1I=(J-1)*NRASX
         I1O=(NRASY-J)*NRASX
         DO I=1,NRASX
            I1I=I1I+1
            I1O=I1O+1
            ITEMP=IVIDI(I1O)
            IVIDI(I1O)=IVIDI(I1I)
            IVIDI(I1I)=ITEMP
            IF (IVIDI(I1I).EQ.-1) IVIDI(I1I)=1
            IF (IVIDI(I1I).EQ.-2) IVIDI(I1I)=0
            IF (IVIDI(I1O).EQ.-1) IVIDI(I1O)=1
            IF (IVIDI(I1O).EQ.-2) IVIDI(I1O)=0
         END DO
      END DO
      DO J=1,3
         DO I=0,NCOL-1
            GIFARRAY(J,I)=COLARRAY(J,I)
         END DO
      END DO
      CALL VIDPIXI(IVIDI,0,NRASY,NRASX,NRASY,.TRUE.)
      CALL XPAGES(0,0)
      RETURN
      END
