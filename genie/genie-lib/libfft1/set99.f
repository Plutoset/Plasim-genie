*DECK SET99
C     SUBROUTINE 'SET99' - COMPUTES FACTORS OF N & TRIGONOMETRIC
C     FUNCTINS REQUIRED BY FFT991(F)
C
c     THIS ROUTINE CURRENTLY HAS BOUNDS ERRORS!!
c     but it works, if bounds checking is not on 
c     for the compilation of genie-lib.
c     I should change this really (djl).
c     The ifax(1) is a reference.  I think it should be ifax(10)
c
c
      SUBROUTINE SET99(TRIGS,IFAX,N)
      IMPLICIT NONE
      INTEGER N,LFAX,NIL,NHL,K,NU,IFAC,L,JFAX,NFAX,IFAX,I
      REAL    DEL,ANGLE,TRIGS
      DIMENSION TRIGS(N),IFAX(1),JFAX(10),LFAX(7)
      DATA LFAX/6,8,5,4,3,2,1/
C
      DEL=4.0*ASIN(1.0)/FLOAT(N)
      NIL=0
      NHL=(N/2)-1
      DO 10 K=NIL,NHL
      ANGLE=FLOAT(K)*DEL
      TRIGS(2*K+1)=COS(ANGLE)
      TRIGS(2*K+2)=SIN(ANGLE)
   10 CONTINUE
C
C     FIND FACTORS OF N (8,6,5,4,3,2; ONLY ONE 8 ALLOWED)
C     LOOK FOR SIXES FIRST, STORE FACTORS IN DESCENDING ORDER
      NU=N
      IFAC=6
      K=0
      L=1
   20 CONTINUE
      IF (MOD(NU,IFAC).NE.0) GO TO 30
      K=K+1
      JFAX(K)=IFAC
      IF (IFAC.NE.8) GO TO 25
      IF (K.EQ.1) GO TO 25
      JFAX(1)=8
      JFAX(K)=6
   25 CONTINUE
      NU=NU/IFAC
      IF (NU.EQ.1) GO TO 50
      IF (IFAC.NE.8) GO TO 20
   30 CONTINUE
      L=L+1
      IFAC=LFAX(L)
      IF (IFAC.GT.1) GO TO 20
C
      WRITE(6,40) N
   40 FORMAT(4H1N =,I4,27H - CONTAINS ILLEGAL FACTORS)
      RETURN
C
C     NOW REVERSE ORDER OF FACTORS
   50 CONTINUE
      NFAX=K
      IFAX(1)=NFAX
      DO 60 I=1,NFAX
      IFAX(NFAX+2-I)=JFAX(I)
   60 CONTINUE
      RETURN
      END
