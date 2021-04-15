      SUBROUTINE INTER(GIN,GOUT,NLOUT,INTRL,FINTR)
c      SUBROUTINE INTER(GIN,GOUT,NLOUT,INTRL,FINTR,JOW,JXUP,JXDN)
C
C     Interpolate vertically from input array GIN(..,NL) to output
C     array GOUT(..,NLOUT), using pre-determined factors FINTR and
C     levels INTRL(..,L) and INTRL(..,L+1) in input data for level
C     L in output data.
C     If JOW=1, interpolated data is overwritten on first NLOUT levels
C     of input array.
C     Extrapolation beyond extreme model levels controlled by namelist
C     logicals LXTRP2 and LXTRP3 for LPC2 and LPC3 fields respectively.
C     If switched on, JXUP and JXDN define options used.  If switched
C     off, input options are overridden and values held constant beyond
C     extreme model levels (equivalent to JXUP=JXDN=0).
C     Current extrapolation options are:
C        Upward  : JXUP = 0  :  constant value above PSG(,1)
C                         1  :  same as interp - linear in ln(p)
C                         2  :  linear interp in p, to zero at p=0
C        Downward: JXDN = 0  :  constant value below PSG(,NL)
C                         1  :  same as interp - linear in ln(p)
C                         2  :  interp to surface omega, const below
C                         3  :  temperature: const lapse below PSG(,NL)
C                         4  :  Geopotential: based on constant lapse
C                         5  :  constant down to surface, zero below
C     NB.  Common COMGRM must not be called as it contains INTRL.
C     NB.  Common  GRIDP must not be called as it can contain GIN.
C
ccDJL*CALL PARAM1
ccDJL*CALL PARAM2
ccDJL*CALL COMPRL
ccDJL*CALL COMROG
ccDJL*CALL LEGAU
c+DJL
      implicit none
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'gridpp.cmn'

      INTEGER INTRL(IGC,NL)
      REAL GIN(IGC,NL),GOUT(IGC,NL),FINTR(IGC,NL)
c-DJL
ccDJL      INTEGER INTRL(IGC,NLOUT)
ccDJL      REAL GIN(IGC,NL),GOUT(IGC,NLOUT),FINTR(IGC,NLOUT)
ccDJL      LOGICAL LX
c+DJL
      integer nlout
c      integer JOW,JXUP,JXDN
c      integer ixup
c      integer ixdn
      integer ihem,iof,n,i,ii,lm

      nlout=nl

c-DJL

C                                                                               
ccDJL 6900 FORMAT(/' ***ABORT IN INTER: IXUP = ,'I3,' NOT ALLOWED')
ccDJL 6910 FORMAT(/' ***ABORT IN INTER: IXDN = ,'I3,' NOT ALLOWED')
C
C     Set extrapolation switches.  Override input options if no extrap.
C
ccDJL      LX=(LINTP2.AND.LXTRP2).OR.(LINTP3.AND.LXTRP3)
    
c      IXUP=JXUP
c      IXDN=JXDN
ccDJL      IF (.NOT.LX) THEN                                                         
ccDJL         IXUP=0                                                                 
ccDJL         IXDN=0                                                                 
ccDJL      ENDIF                                                                     
C                                                                               
ccDJL      IF (LX.AND.(IXUP.LT.0.OR.IXUP.GT.2)) THEN                                 
ccDJL         WRITE(6,6900) IXUP                                                     
ccDJL         CALL ABORT                                                             
ccDJL      ENDIF                                                                     
ccDJL      IF (LX.AND.(IXDN.LT.0.OR.IXDN.GT.5)) THEN                                 
ccDJL         WRITE(6,6910) IXDN                                                     
ccDJL         CALL ABORT                                                             
ccDJL      ENDIF                                                                     
C                                                                               
      DO 300 IHEM=1,NHEM
      IOF=(IHEM-1)*MGPP
C                                                                               
C     First perform interpolation and implied extrapolation using               
C     default factors.                                                          
C                                                                               
      DO 5 N=1,NLOUT
      DO 5 I=1,MG
      II=I+IOF
      LM=INTRL(II,N)
    5 GOUT(II,N)=GIN(II,LM)+FINTR(II,N)*(GIN(II,LM+1)-GIN(II,LM))
C                                                                               
C     Upward extrapolation.  Skip levels for which pressure level is            
C     wholly below top model level.                                             
C                                                                               
ccDJL      IF (IXUP.EQ.1) GOTO 100                                                   
ccDJL      DO 90 N=1,NLOUT                                                           
ccDJL      IF (PPR(N).GE.PSGMX(1,IHEM)) GOTO 90                                      
ccDJL      IF (IXUP.EQ.0) THEN                                                       
ccDJL         DO 10 I=1,MG                                                           
ccDJL         II=I+IOF                                                              
ccDJL         IF (PPR(N).LT.PSG(II,1)) GOUT(II,N)=GIN(II,1)                         
ccDJL   10    CONTINUE                                                              
ccDJL      ELSE IF (IXUP.EQ.2) THEN                                                 
ccDJL         DO 20 I=1,MG                                                          
ccDJL         II=I+IOF                                                              
ccDJL         IF (PPR(N).LT.PSG(II,1))  GOUT(II,N)=GIN(II,1)*PPR(N)/PSG(II,1)       
ccDJL   20    CONTINUE                                                              
ccDJL      ENDIF                                                                    
ccDJL   90 CONTINUE                                                                 
C                                                                              
C     Downward extrapolation.  Skip levels for which pressure level is         
C     wholly above bottom model level.                                         
C                                                                              
ccDJL  100 CONTINUE                                                                 
ccDJL      IF (IXDN.EQ.1) GOTO 200                                                  
ccDJL      DO 190 N=1,NLOUT                                                         
ccDJL      IF (PPR(N).LE.PSGMN(NL,IHEM)) GOTO 190                                   
ccDJL      IF (IXDN.EQ.0) THEN                                                      
ccDJL         DO 110 I=1,MG                                                         
ccDJL         II=I+IOF                                                              
ccDJL         IF (PPR(N).GT.PSG(II,NL)) GOUT(II,N)=GIN(II,NL)                       
ccDJL  110    CONTINUE                                                              
ccDJL      ELSE IF (IXDN.EQ.2) THEN                                                 
ccDJL         DO 120 I=1,MG                                                         
ccDJL         II=I+IOF                                                              
ccDJL         IF (PPR(N).GT.PSRF(II)) GOUT(II,N)=OMSRF(II)                          
ccDJL         IF (PPR(N).GT.PSG(II,NL).AND.PPR(N).LE.PSRF(II))                      
ccDJL     :      GOUT(II,N)=GIN(II,NL)+(OMSRF(II)-GIN(II,NL))*                      
ccDJL     :                 (PPR(N)-PSG(II,NL))/(PSRF(II)-PSG(II,NL))               
ccDJL  120    CONTINUE                                                              
ccDJL      ELSE IF (IXDN.EQ.3) THEN                                                 
ccDJL         DO 130 I=1,MG                                                         
ccDJL         II=I+IOF                                                              
ccDJL         IF (PPR(N).GT.PSG(II,NL))                                             
ccDJL     :      GOUT(II,N)=GIN(II,NL)*((PPR(N)/PSG(II,NL))**XLAPSE)                
ccDJL  130    CONTINUE                                                              
ccDJL      ELSE IF (IXDN.EQ.4) THEN                                                 
ccDJL         DO 140 I=1,MG                                                         
ccDJL         II=I+IOF                                                              
ccDJL         IF (PPR(N).GT.PSRF(II))                                               
ccDJL     :      GOUT(II,N)=GSG(II,JL)-(TSRF(II)/XLAPSE)*                           
ccDJL     :                            ((PPR(N)/PSRF(II))**XLAPSE-1.)               
ccDJL         IF (PPR(N).GT.PSG(II,NL).AND.PPR(N).LE.PSRF(II))                      
ccDJL     :      GOUT(II,N)=GIN(II,NL)+(GSG(II,JL)-GIN(II,NL))*                     
ccDJL     :               LOG(PPR(N)/PSG(II,NL))/LOG(PSRF(II)/PSG(II,NL))           
ccDJL  140    CONTINUE                                                              
ccDJL      ELSE IF (IXDN.EQ.5) THEN                                                 
ccDJL         DO 150 I=1,MG                                                         
ccDJL         II=I+IOF                                                              
ccDJL         IF (PPR(N).GT.PSRF(II)) GOUT(II,N)=0.                                 
ccDJL         IF (PPR(N).GT.PSG(II,NL).AND.PPR(N).LE.PSRF(II))                      
ccDJL     :      GOUT(II,N)=GIN(II,NL)                                              
ccDJL  150    CONTINUE                                                              
ccDJL      ENDIF                                                                    
ccDJL  190 CONTINUE                                                                 
C                                                                              
C     Copy to overwrite the input array if requested.                          
C                                                                              
ccDJL  200 CONTINUE                                                                 
ccDJL      IF (JOW.EQ.1) THEN                                                       
ccDJL         DO 210 N=1,NLOUT                                                      
ccDJL         DO 210 I=1,MG                                                         
ccDJL         II=I+IOF                                                             
ccDJL  210    GIN(II,N)=GOUT(II,N)                                                 
ccDJL      ENDIF                                                                   
C                                                                             
  300 CONTINUE
C                                                                             
      RETURN
      END
