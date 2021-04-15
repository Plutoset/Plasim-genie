*DECK INISI
C***********************************************************
C                SUBROUTINE INISI
C***********************************************************
      SUBROUTINE INISI

      IMPLICIT NONE

C
C     Sets up arrays and variables for the vertical structure
C     and the semi-implicit scheme
C
#include "param1.cmn"
      include 'param2.cmn'
      INTEGER NLT
      PARAMETER(NLT=NL+NL)
      include 'blank.cmn'
      include 'bats.cmn'
      include 'outcon.cmn'
C
      INTEGER L,IG,LP,J,LIM,I,IC,JC,JCC,IT,M,IL,INS
      INTEGER IN,IM,N,IBM1
      REAL    TBM1,TBME,STP,P,T1,T2,S1,S2,T0M,T0P,ALJ
      REAL    TT01S2,SIG,TTKP,TTM,SIGM,RDSIGL,TM,TMM
      REAL    TTAU,FAC,HL,H,TAQ,CR,CI,RGVAL,DA,DB,DC
      REAL    DD,DELTSQ,RCN,WA,SFAC,T0L,DSIG


      integer maxlevs
      parameter(maxlevs=50)
      real sigmahtemp(maxlevs)

      DIMENSION H(NL),CR(NL),CI(NL),TBM1(NL2),WA(NL)
      INTEGER IWA(NL)
      DIMENSION TBME(NL,NL)
      EQUIVALENCE (TBM1(1),TBME(1,1))
C
  203 FORMAT(' STANDARD HEIGHTS IN KM')
  206 FORMAT(' GRAVITY WAVE SPEEDS IN M/SEC')
  208 FORMAT(' VALUES OF SIGMA AT HALF LEVELS')
  213 FORMAT(1X,8F8.4)
  214 FORMAT(' VALUES OF SIGMA AT FULL LEVELS')
  215 FORMAT(' BASIC STATE TEMPERATURES (NON-DIMENSIONAL)')
  216 FORMAT(1X,8F8.3)
  217 FORMAT(5X,8F8.3)
C
      STP=1.0/NL
      IF (.NOT. LSTRETCH) THEN
         DO 23 L=1,NLM
            SIGMAH(L)=L*STP
   23    CONTINUE
      ELSE
         P=0.0
         T1=(.9375/.94-1.25)/(.9375*(SQRT(.9375)-1.0))
         T2=4.0+T1
         DO 25 L=1,NLM
            P=P+STP
            SIGMAH(L)=P*(2.0-P)*(1.0+0.25*SIN(PI2*(P**.6)))/
     1                (5.0-T2*P+T1*(P**1.5))
   25    CONTINUE
      END IF

      IF (L22L.AND.NL.EQ.22) THEN
      SIGMAHtemp(1)=0.002
      SIGMAHtemp(2)=0.009
      SIGMAHtemp(3)=0.019
      SIGMAHtemp(4)=0.037
      SIGMAHtemp(5)=0.063       
      SIGMAHtemp(6)=0.100       
      SIGMAHtemp(7)=0.136       
      SIGMAHtemp(8)=0.172       
      SIGMAHtemp(9)=0.207              
      SIGMAHtemp(10)=0.242             
      SIGMAHtemp(11)=0.277             
      SIGMAHtemp(12)=0.316             
      SIGMAHtemp(13)=0.360             
      SIGMAHtemp(14)=0.411             
      SIGMAHtemp(15)=0.472             
      SIGMAHtemp(16)=0.544
      SIGMAHtemp(17)=0.624          
      SIGMAHtemp(18)=0.712          
      SIGMAHtemp(19)=0.800          
      SIGMAHtemp(20)=0.882          
      SIGMAHtemp(21)=0.951
      do l=1,nl-1
      sigmah(l)=sigmahtemp(l)
      enddo
      ENDIF
 
      S1=0.
      DO 60 L=1,NLM
         S2=SIGMAH(L)
         DSIGMA(L)=S2-S1
         SIGMA(L)=0.5*(S2+S1)
         RDSIG(L)=0.5/DSIGMA(L)
         S1=S2
   60 CONTINUE
      DSIGMA(NL)=1.-SIGMAH(NLM)
      RDSIG(NL)=0.5/DSIGMA(NL)
      SIGMA(NL)=0.5*(1.+SIGMAH(NLM))
C
C     This value, used in setting ALPHA(1), is irrelevant in the
C     angular momentum conserving ECMWF scheme
C
      S1=LOG(SIGMA(1)*SIGMA(1)/SIGMAH(1))
      IG=1
      T0M=T0(1)
      DO 61 L=1,NLM
         LP=L+1
         S2=LOG(SIGMAH(L))
         T0P=T0(LP)
         IG=IG+NL
         G(IG)=0.
         T01S2(L)=T0P-T0M
         ALPHA(L)=S2-S1
         TKP(L)=AKAP*T0M
         T0M=T0P
         S1=S2
   61 CONTINUE
      ALPHA(NL)=-S1
      TKP(NL)=AKAP*T0M
      G(1)=1.0
      DO 64 J=2,NL
         ALJ=ALPHA(J)
         IG=J
         LIM=J-1
         DO 62 I=1,LIM
            G(IG)=ALJ
            IG=IG+NL
   62    CONTINUE
         G(IG)=1.0-ALJ*SIGMAH(LIM)/DSIGMA(J)
         IF (J.LT.NL) THEN
            LIM=LIM+2
            DO 63 I=LIM,NL
               IG=IG+NL
               G(IG)=0.
   63       CONTINUE
         ENDIF
   64 CONTINUE
      IC=-1
      DO 50 I=1,NL
         IC=IC+1
         JC=IC*NLP
         JCC=JC-NLM
         DO 51 J=I,NL
            JC=JC+1
            JCC=JCC+NL
            C(JCC)=G(JC)*DSIGMA(I)/DSIGMA(J)
   51    CONTINUE
   50 CONTINUE
      TT01S2=T01S2(1)
      TAU(1)=0.5*TT01S2*(SIGMAH(1)-1.0)+TKP(1)*C(1)
      DO 65 L=2,NL
         TAU(L)=0.5*TT01S2*DSIGMA(L)
   65 CONTINUE
      SIG=SIGMAH(1)
      IT=NL
      DO 73 L=2,NL
         TTKP=TKP(L)
         TTM=TT01S2
         SIGM=SIG
         IF (L.LT.NL) THEN
            TT01S2=T01S2(L)
            SIG=SIGMAH(L)
         ENDIF
         RDSIGL=RDSIG(L)
         DO 72 M=1,NL
            IT=IT+1
            IF( M.LT.L) THEN
               TM=1.
               TMM=1.
            ELSEIF (M.EQ.L) THEN
               TM=1.
               TMM=0.
            ELSE
               TM=0.
               TMM=0.
            ENDIF
            TTAU=TTM*(SIGM-TMM)
            IF (L.LT.NL) TTAU=TTAU+TT01S2*(SIG-TM)
            TTAU=TTAU*RDSIGL*DSIGMA(M)
            IF (M.LE.L) TTAU=TTAU+TTKP*C(IT)
            TAU(IT)=TTAU
   72    CONTINUE
   73 CONTINUE
      FAC=0.001*CG/GA
      IL=0
      DO 78 L=1,NL
         HL=0.
         DO 77 M=1,NL
            IL=IL+1
            HL=HL+G(IL)*T0(M)
   77    CONTINUE
         H(L)=HL*FAC
   78 CONTINUE
      IL=0
      INS=1
      DO 81 L=1,NL
         DO 80 M=1,NL
            IN=INS
            IL=IL+1
            IM=M
            TAQ=T0(L)*DSIGMA(M)
            DO 79 N=1,NL
               TAQ=TAQ+G(IN)*TAU(IM)
               IN=IN+1
               IM=IM+NL
   79       CONTINUE
            AQ(IL)=TAQ
            TBM1(IL)=TAQ
   80    CONTINUE
         INS=INS+NL
   81 CONTINUE
      CALL QREIG(TBM1,NL,NL,NL,CR,CI)
      DO 82 L=1,NL
         CR(L)=CV*SQRT(CR(L))
   82 CONTINUE
C
C     Write out vertical information
C
      WRITE(6,208)
      WRITE(6,217)(SIGMAH(L),L=1,NLM)
      WRITE(6,214)
      WRITE(6,213)(SIGMA(L),L=1,NL)
      WRITE(6,215)
      WRITE(6,216)(T0(L),L=1,NL)
      WRITE(6,203)
      WRITE(6,216)(H(L),L=1,NL)
      WRITE(6,206)
      WRITE(6,216)(CR(L),L=1,NL)
      WRITE(6,*)
C
C     RGG matrix for vertical derivatives:
C     d()/dln(sigma) = (sigma)d()/d(sigma).
C
      IL=0
      DO 150 L=1,NL
         RGVAL=SIGMA(L)*RDSIG(L)
         DO 160 M=1,NL
            IL=IL+1
            RGG(IL)=0.
            IF (M.EQ.L-1) RGG(IL)=-RGVAL
            IF (M.EQ.L+1) RGG(IL)=RGVAL
  160    CONTINUE
  150 CONTINUE
      DA=SIGMA(1)
      DB=SIGMA(2)-DA
      DC=SIGMA(3)-SIGMA(2)
      DD=DC+DB
      RGG(1)=-DA*(DD+DB)/(DB*DD)
      RGG(2)=DA*DD/(DB*DC)
      RGG(3)=-DA*DB/(DC*DD)
      DA=SIGMA(NL)
      DD=SIGMA(NL-1)
      DB=DA-DD
      DC=DD-SIGMA(NL-2)
      DD=DC+DB
      RGG(NL2)=DA*(DD+DB)/(DB*DD)
      RGG(NL2-1)=-DA*DD/(DB*DC)
      RGG(NL2-2)=DA*DB/(DC*DD)
C
C     Setup arrays for semi-implicit scheme
C
      DELTSQ=DELT*DELT
      IBM1=0
      DO 11 IN=2,NNP
         RCN=RSQ(IN)
         IL=0
         DO 83 L=1,NL
            DO 84 M=1,NL
               IL=IL+1
               TBM1(IL)=AQ(IL)*DELTSQ
               IF(M.EQ.L)TBM1(IL)=TBM1(IL)+RCN
   84       CONTINUE
   83    CONTINUE
         CALL MATINV(TBME,NL,NL,IWA,WA)
         DO 85 L=1,NL2
            IBM1=IBM1+1
            BM1(IBM1)=TBM1(L)
   85    CONTINUE
   11 CONTINUE
      SFAC=0.5**KITS
      IF(LRSTRT.AND..NOT.LSHORT)SFAC=1.0
      DELT=DELT*SFAC
      DELT2=DELT+DELT
      DELTSQ=DELT*DELT
      DO 87 L=1,NL2
         AQ(L)=AQ(L)*DELTSQ
   87 CONTINUE
      TOUT1=0.
      TOUT2=0.
      DO 88 L=1,NL
         T0L=T0(L)
         DSIG=DSIGMA(L)*T0(L)
         TOUT1=TOUT1+DSIG
         TOUT2=TOUT2+DSIG*T0L
   88 CONTINUE
C
      END
