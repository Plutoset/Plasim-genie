      SUBROUTINE INIGAU_lib(ALP,DALP,RLP,RDLP,CS,SI,AW,RSQ,ALAT,
     :           JG,MM,MOCT,NN,NWJ2)
      IMPLICIT NONE
      INTEGER JG,MM,MOCT,NN,NWJ2
      REAL ALP(NWJ2+NWJ2,JG),DALP(NWJ2+NWJ2,JG),RLP(NWJ2+NWJ2,JG),
     :    RDLP(NWJ2+NWJ2,JG)
      REAL CS(*),SI(*),AW(*),RSQ(*),ALAT(*)
C
C     SETS UP VARIABLES FOR GAUSSIAN LATITUDES AND THE
C        LEGENDRE FUNCTIONS.
C
      REAL PI,PI2
      PARAMETER(PI=3.14159265359,PI2=2.*PI)
      REAL SIT,WEIGHT,CSJ,RCSJ
      INTEGER JH,IP,JP,MP,N,MAX
C
      DO JH=1,JG
         CALL GWTLT1_lib(SIT,WEIGHT,JH,JG)
         CSJ=1.-SIT*SIT
         RCSJ=1./CSJ
         SI(JH)=SIT
         CS(JH)=    CSJ
         ALAT(JH)=ATAN(SIT*SQRT(RCSJ))*180./PI
         AW(JH)=WEIGHT*RCSJ*2.
         CALL LGNDRE_lib(ALP,DALP,SIT,CSJ,JH,MM,NN,NWJ2,JG,MOCT)
         IP=0
         DO MP=1,MM,MOCT
            N=MP-1
            MAX=MP+((NN-MP)/2)*2+1
            DO JP=MP,MAX
               IP=IP+1
               N=N+1
               RLP(IP,JH)=-RSQ(N)*ALP(IP,JH)
               RDLP(IP,JH)=-RSQ(N)*DALP(IP,JH)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
c
      SUBROUTINE INIGAU8(ALP,DALP,RLP,RDLP,CS,SI,AW,RSQ,ALAT,
     :           JG,MM,MOCT,NN,NWJ2)
      IMPLICIT NONE
      INTEGER JG,MM,MOCT,NN,NWJ2
      REAL*8 ALP(NWJ2+NWJ2,JG),DALP(NWJ2+NWJ2,JG),RLP(NWJ2+NWJ2,JG),
     :    RDLP(NWJ2+NWJ2,JG)
      REAL*8 CS(*),SI(*),AW(*),RSQ(*),ALAT(*)
C
C     SETS UP VARIABLES FOR GAUSSIAN LATITUDES AND THE
C        LEGENDRE FUNCTIONS.
C
      REAL*8 PI,PI2
      PARAMETER(PI=3.14159265359,PI2=2.*PI)
      REAL*8 SIT,WEIGHT,CSJ,RCSJ
      INTEGER JH,IP,JP,MP,N,MAX
C
      DO JH=1,JG
         CALL GWTLT18(SIT,WEIGHT,JH,JG)
         CSJ=1.-SIT*SIT
         RCSJ=1./CSJ
         SI(JH)=SIT
         CS(JH)=    CSJ
         ALAT(JH)=ATAN(SIT*SQRT(RCSJ))*180./PI
         AW(JH)=WEIGHT*RCSJ*2.
         CALL LGNDRE8(ALP,DALP,SIT,CSJ,JH,MM,NN,NWJ2,JG,MOCT)
         IP=0
         DO MP=1,MM,MOCT
            N=MP-1
            MAX=MP+((NN-MP)/2)*2+1
            DO JP=MP,MAX
               IP=IP+1
               N=N+1
               RLP(IP,JH)=-RSQ(N)*ALP(IP,JH)
               RDLP(IP,JH)=-RSQ(N)*DALP(IP,JH)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
