      SUBROUTINE INISPEC(GSG,Z,ALAT,MG,JG,NHEM,NN,MM,MOCT,NWJ2,IOPT)
      IMPLICIT NONE 
      INTEGER MG,JG,NHEM,NN,MM,MOCT,NWJ2
      INTEGER MGM,JGM,NHEMM,NNM,NWJ2M
      PARAMETER(MGM=320,JGM=80,NHEMM=2,NNM=106,NWJ2M=2862)
      REAL TRIG(MGM*2+15)
      INTEGER IFAX(1000),IOPT
      COMPLEX Z(*)
      REAL GSG(*)
      REAL SQ(NNM+1),RSQ(NNM+1)
      COMPLEX CMPA(((MGM+2)/2)*NHEMM)
      REAL POLY(NWJ2M*NHEMM)
      REAL ALP(2*NWJ2M*JGM),DALP(2*NWJ2M*JGM),
     :     RLP(2*NWJ2M*JGM),RDLP(2*NWJ2M*JGM)
      REAL CS(JGM),AW(JGM),SI(JGM),ALAT(*)
c
      IF (MG.GT.MGM.OR.JG.GT.JGM.OR.NHEM.GT.NHEMM.OR.
     :    NN.GT.NNM.OR.NWJ2.GT.NWJ2M) THEN
        PRINT*,' Maximum array dimensions too small '
        PRINT*,MGM,JGM,NHEMM,NNM,NWJ2M
        PRINT*,MG ,JG ,NHEM ,NN ,NWJ2
        STOP
      ENDIF
c
      CALL INISET_lib(SQ,RSQ,NN,MM,NWJ2,MOCT)
      CALL INIFFT(TRIG,IFAX,CMPA,MM,MOCT,NHEM,MG)
      CALL INIGAU_lib(ALP,DALP,RLP,RDLP,CS,SI,AW,RSQ,ALAT,
     :           JG,MM,MOCT,NN,NWJ2)
      CALL INIGS(Z,GSG,NN,MM,NHEM,MOCT,NWJ2,MG,JG,AW,CS,TRIG,
     :                 IFAX,ALP,DALP,RLP,RDLP,POLY,CMPA,IOPT)
c
      RETURN
      END
c
      SUBROUTINE INISPEC8(GSG,Z,ALAT,MG,JG,NHEM,NN,MM,MOCT,NWJ2,IOPT)
      IMPLICIT NONE
      INTEGER MG,JG,NHEM,NN,MM,MOCT,NWJ2
      INTEGER MGM,JGM,NHEMM,NNM,NWJ2M
      PARAMETER(MGM=320,JGM=80,NHEMM=2,NNM=106,NWJ2M=2862)
      REAL*8 TRIG(MGM*2+15)
      INTEGER IFAX(1000),IOPT
      COMPLEX*16 Z(*)
      REAL*8 GSG(*)
      REAL*8 SQ(NNM+1),RSQ(NNM+1)
      COMPLEX*16 CMPA(((MGM+2)/2)*NHEMM)
      REAL*8 POLY(NWJ2M*NHEMM)
      REAL*8 ALP(2*NWJ2M*JGM),DALP(2*NWJ2M*JGM),
     :     RLP(2*NWJ2M*JGM),RDLP(2*NWJ2M*JGM)
      REAL*8 CS(JGM),AW(JGM),SI(JGM),ALAT(*)
c
      IF (MG.GT.MGM.OR.JG.GT.JGM.OR.NHEM.GT.NHEMM.OR.
     :    NN.GT.NNM.OR.NWJ2.GT.NWJ2M) THEN
        PRINT*,' Maximum array dimensions too small '
        PRINT*,MGM,JGM,NHEMM,NNM,NWJ2M
        PRINT*,MG ,JG ,NHEM ,NN ,NWJ2
        STOP
      ENDIF
c
      CALL INISET8(SQ,RSQ,NN,MM,NWJ2,MOCT)
      CALL INIFFT8(TRIG,IFAX,CMPA,MM,MOCT,NHEM,MG)
      CALL INIGAU8(ALP,DALP,RLP,RDLP,CS,SI,AW,RSQ,ALAT,
     :           JG,MM,MOCT,NN,NWJ2)
      CALL INIGS8(Z,GSG,NN,MM,NHEM,MOCT,NWJ2,MG,JG,AW,CS,TRIG,
     :                 IFAX,ALP,DALP,RLP,RDLP,POLY,CMPA,IOPT)
c
      RETURN
      END
