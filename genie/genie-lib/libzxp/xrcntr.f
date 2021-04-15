      SUBROUTINE XRCNTR(ZG,Z,MD,MG,NG,CV)
C*     The final edition of the contouring routine
C*                                 12.6.1987
      INTEGER MD
      REAL CV
      INTEGER JG
      INTEGER NG
      INTEGER IDUB
      INTEGER J
      INTEGER MG
      INTEGER I
      REAL H1
      REAL ZG
      REAL H2
      REAL H3
      REAL H4
      REAL H5
C
      DIMENSION ZG(MD,*),Z(MD,*)
      COMPLEX   Z,B1,B2,ZA,ZB,Z1,Z2,Z3,Z4,D
            D(P1,P2,B1,B2   )=B1+(CV-P1)*(B2-B1)/(P2-P1)
      JG=ABS(NG)
      IDUB=0
      DO 1 J=1,MG-1
      DO 2 I=1,JG-1
      H1=ZG(J  ,I  )
      H2=ZG(J+1,I  )
      Z1= Z(J  ,I  )
      Z2= Z(J+1,I  )
      H3=ZG(J+1,I+1)
      H4=ZG(  J,I+1)
      Z3= Z(J+1,I+1)
      Z4= Z(  J,I+1)
      IF(H1-CV)11,20,20
   11 IF(H2-CV)12,14,14
   12 IF(H3-CV)13,15,15
   13 IF(H4-CV) 2,30,30
   14 IF(H3-CV)16,17,17
   15 IF(H4-CV)31,32,32
   16 IF(H4-CV)33,34,34
   17 IF(H4-CV)35,36,36
   20 IF(H2-CV)21,23,23
   21 IF(H3-CV)22,24,24
   22 IF(H4-CV)36,35,35
   23 IF(H3-CV)25,26,26
   24 IF(H4-CV)37,33,33
   25 IF(H4-CV)32,31,31
   26 IF(H4-CV)30, 2, 2
   30 ZA=D(H1,H4,Z1,Z4)
      ZB=D(H4,H3,Z4,Z3)
      GOTO 40
   31 ZA=D(H2,H3,Z2,Z3)
      ZB=D(H4,H3,Z4,Z3)
      GOTO 40
   32 ZA=D(H1,H4,Z1,Z4)
      ZB=D(H2,H3,Z2,Z3)
      GOTO 40
   33 ZA=D(H1,H2,Z1,Z2)
      ZB=D(H2,H3,Z2,Z3)
      IDUB=0
      GOTO 40
   34 IDUB=1
      H5=0.25*(H1+H2+H3+H4)
      IF(H5.GT.CV) IDUB=-1
      GOTO (30,31) 2-(1+IDUB)/2
   35 ZA=D(H1,H2,Z1,Z2)
      ZB=D(H4,H3,Z4,Z3)
      GOTO 40
   36 ZA=D(H1,H2,Z1,Z2)
      ZB=D(H1,H4,Z1,Z4)
      IDUB=0
      GOTO 40
   37 IDUB=-1
      H5=0.25*(H1+H2+H3+H4)
      IF(H5.GT.CV) IDUB=1
      GOTO (30,31) 2-(1+IDUB)/2
      GOTO 31
   40 CONTINUE
      IF(NG.GT.0) THEN
       CALL XPENUP(REAL(ZA),AIMAG(ZA))
       CALL XPENDN(REAL(ZB),AIMAG(ZB))
      ELSE
       ZB=ZA+0.7*(ZB-ZA)
       CALL XPENUP(REAL(ZA),AIMAG(ZA))
       CALL XPENDN(REAL(ZB),AIMAG(ZB))
      ENDIF
      IF(IDUB)36,2,33
    2 CONTINUE
    1 CONTINUE
      RETURN
      END
