*DECK TSTEP
C**********************************************************
C             SUBROUTINE TSTEP
C**********************************************************
      SUBROUTINE TSTEP

      IMPLICIT NONE

C
C     Takes an adiabatic timestep in spectral space
C     either a centred semi-implicit timestep for KOUNT>KITS
C     or an initial short timestep for KOUNT<KITS
C
C     A time filter used for centred timestep.
C
C
#include "param1.cmn"
      include 'param2.cmn'
      INTEGER NLT
      PARAMETER(NLT=NL+NL)
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'bats.cmn'
      include 'restor.cmn'
      INTEGER I1,IHEM,MP,IBM1,IN,K,L,I,KK,IL
      REAL    TBM1,TBME,TTR,DTR,TMIR,D1R,RCN,WA
      DIMENSION TBM1(NL2),D1(NL),WA(NL)
      INTEGER IWA(NL)
      DIMENSION TBME(NL,NL)
      EQUIVALENCE (TBM1(1),TBME(1,1))
      COMPLEX TMPA(NL),TMPB(NL)
      REAL RMPA(NLT),RMPB(NLT)
      EQUIVALENCE (TMPA(1),RMPA(1)),(TMPB(1),RMPB(1))
      DIMENSION TTR(IGP),DTR(IGP),TMIR(IGP),D1R(NLT)
      EQUIVALENCE (TTR(1),TT(1)),(DTR(1),DT(1)),(TMIR(1),TMI(1))
     +,(D1R(1),D1(1))
      COMPLEX ZPAV,TPAV,DPAV,DTI,SPAV,TRAPAV
      COMPLEX D1,SPPA,VPS,GSI1
C
      IF (KOUNT.GT.KITS) THEN
C
C        Ordinary centred semi-implicit timestep
C
         I1=0
         DO 800 IHEM=1,NHEM
            DO 4 MP=1,MFP,MOCT
               IBM1=NL2*(MP-2 +IHEM-1)
               DO 5 IN=MP,NFP,MH
                  I1=I1+1
                  IF (IBM1.NE.-NL2) THEN
                     SPPA=SPMI(I1)
                     VPS=VP(I1)
                     RCN=RSQ(IN+IHEM-1)
                     GSI1=GS(I1)
                     K=I1
                     CALL SGEMM('N','N',2,NL,NL,1.0,TTR(2*I1-1),IGO,
     +                          G,NL,0.0,RMPA,2)
                     CALL SGEMM('N','N',2,NL,NL,1.0,TMIR(2*I1-1),IGO,
     +                          G,NL,0.0,RMPB,2)
                     DO 12 L=1,NL
                        D1(L)=RCN*DMI(K)+DELT*(TMPB(L)+GSI1+T0(L)*SPPA
     1                       +RCN*DT(K)+DELT*(TMPA(L)-T0(L)*VPS))
                        K=K+IGA
   12                CONTINUE
                     K=I1
                     CALL SGEMM('N','N',2,NL,NL,1.0,D1R,2,
     +                          BM1(IBM1+1),NL,0.0,DTR(2*I1-1),IGO)
                     DO 14 L=1,NL
                        VP(I1)=VP(I1)+DT(K)*DSIGMA(L)
                        K=K+IGA
   14                CONTINUE
                     IBM1=IBM1+NL2+NL2
                     K=I1
                     CALL SGEMM('N','N',2,NL,NL,1.0,DTR(2*I1-1),IGO,
     +                          TAU,NL,0.0,RMPA,2)
                     DO 16 L=1,NL
                        TT(K)=TT(K)-TMPA(L)
                        K=K+IGA
   16                CONTINUE
                  ELSE
                     IBM1=NL2
                  ENDIF
    5          CONTINUE
    4       CONTINUE
            I1=NWJ2
  800    CONTINUE
         DO 40 I=1,IGB
            ZPAV=ZMI(I)
            ZMI(I)=PNU21*Z(I)+PNU*ZPAV
            Z(I)=ZPAV+DELT2*ZT(I)
            TPAV=TMI(I)
            TMI(I)=PNU21*T(I)+PNU*TPAV
            T(I)=TPAV+DELT2*TT(I)
            DPAV=DMI(I)
            DTI=DT(I)
            DMI(I)=PNU21*D(I)+PNU*DPAV
            D(I)=DTI+DTI-DPAV
   40    CONTINUE
         DO 141 KK=1,NTRAC
            DO 140 I=1,IGB
               TRAPAV=TRAMI(I,KK)
               TRAMI(I,KK)=PNU21*TRA(I,KK)+PNU*TRAPAV
               TRA(I,KK)=TRAPAV+DELT2*TRAT(I,KK)
  140       CONTINUE
  141    CONTINUE
         DO 41 I=1,IGA
            SPAV=SPMI(I)
            SPMI(I)=PNU21*SP(I)+PNU*SPAV
            SP(I)=SPAV-DELT2*VP(I)
   41    CONTINUE
         RETURN
      ELSE
C
C        Initial short timestep
C
         I1=0
         DO 820 IHEM=1,NHEM
            DO 8 MP=1,MFP,MOCT
               DO 9 IN=MP,NFP,MH
                  I1=I1+1
                  IF (I1+IHEM.NE.2) THEN
                     SPPA=SPMI(I1)
                     VPS=VP(I1)
                     RCN=RSQ(IN+IHEM-1)
                     GSI1=GS(I1)
                     DO 21 IL=1,NL2
                        TBM1(IL)=AQ(IL)
   21                CONTINUE
                     DO 30 L=1,NL
                        TBME(L,L)=TBME(L,L)+RCN
   30                CONTINUE
                     CALL MATINV(TBME,NL,NL,IWA,WA)
                     K=I1
                     CALL SGEMM('N','N',2,NL,NL,1.0,TTR(2*I1-1),IGO,
     +                          G,NL,0.0,RMPA,2)
                     CALL SGEMM('N','N',2,NL,NL,1.0,TMIR(2*I1-1),IGO,
     +                          G,NL,0.0,RMPB,2)
                     DO 22 L=1,NL
                        D1(L)=RCN*DMI(K)+DELT*(TMPB(L)+GSI1+T0(L)*SPPA+
     1                        RCN*DT(K)+DELT*(TMPA(L)-T0(L)*VPS))
                        K=K+IGA
   22                CONTINUE
                     K=I1
                     CALL SGEMM('N','N',2,NL,NL,1.0,D1R,2,
     +                          TBM1,NL,0.0,DTR(2*I1-1),IGO)
                     DO 24 L=1,NL
                        VP(I1)=VP(I1)+DT(K)*DSIGMA(L)
                        K=K+IGA
   24                CONTINUE
                     K=I1
                     CALL SGEMM('N','N',2,NL,NL,1.0,DTR(2*I1-1),IGO,
     +                          TAU,NL,0.0,RMPA,2)
                     DO 26 L=1,NL
                        TT(K)=TT(K)-TMPA(L)
                        K=K+IGA
   26                CONTINUE
                  ENDIF
    9          CONTINUE
    8       CONTINUE
            I1=NWJ2
  820    CONTINUE
         DO 42 I=1,IGB
            Z(I)=ZMI(I)+DELT2*ZT(I)
            T(I)=TMI(I)+DELT2*TT(I)
            D(I)=DT(I)+DT(I)-DMI(I)
   42    CONTINUE
         DO 143 KK=1,NTRAC
            DO 142 I=1,IGB
               TRA(I,KK)=TRAMI(I,KK)+DELT2*TRAT(I,KK)
  142       CONTINUE
  143    CONTINUE
         DO 43 I=1,IGA
            SP(I)=SPMI(I)-DELT2*VP(I)
   43    CONTINUE
         DO 29 L=1,NL2
            AQ(L)=AQ(L)*4.
   29    CONTINUE
         RETURN
      ENDIF
C
      END
