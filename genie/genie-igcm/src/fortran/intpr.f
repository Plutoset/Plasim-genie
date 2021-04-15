      subroutine intpr

c     This subroutine interpolates fields on sigma levels, 
c     and interpolates them to fields on pressure levels.

c     It is almost copied straight from
c     /home/swsvalde/igcm/igcm3.1/diagn/igflux1.src
c     Any removals from this file are marked ccDJL
c     Any additions are enclosed by c+DJL and c-DJL

c+DJL
      implicit none
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'gridpp.cmn'
      integer ihem,iof,i,ii,lm,lmin,l,lp,nlpr,n

c-DJL

C     ****************************************************************  

ccDJL      SUBROUTINE INTPR
C
C     Interpolate from model levels to isobaric levels.
C     Can be called from either GRMULT or FLUX, depending on switches
C     LINTP2, LINTP3 in common COMPRL.
C     Extra fields are interpolated when called from GRMULT.
C     Reading model - sigma.
C     NB.  Arrays DUDLSG, DVDLSG in common COMGRM are used as workspace.
C
ccDJL*CALL PARAM1
ccDJL*CALL PARAM2
ccDJL*CALL COMGRM
ccDJL*CALL COMPRL
ccDJL*CALL GRIDP
     
c+DJL
c     PSG=sigma*plg in igflux1.src
c     I think that plg=spg!
c     We'll do the calculations in dimensionalised units.... 

      REAL FINTR(IGC,NL)

      integer INTRL(IGC,NL)
      real PSG(IGC,NL),PPR(NL)

      nlpr=nl
      DO L=1,NL
      ppr(l)=sigma(l)*p0
      DO II=1,IGC
      PSG(II,L)=SIGMA(L)*p0*(SPG(II)+1.0)
      enddo
      enddo
c     At the moment this is hard-wired to 7 levels.
c     It should be put somewhere into a namelist....
c     See also ini_netcdf.f for more hard-wiring!
      ppr(1)=5000.0
      ppr(2)=20000.0
      ppr(3)=30000.0
      ppr(4)=50000.0
      ppr(5)=70000.0
      ppr(6)=85000.0
      ppr(7)=100000.0
c-DJL




ccDJL EQUIVALENCE (FINTR(1,1),DUDLSG(1,1)),(WKINT(1,1),DVDLSG(1,1))
C
      DO 100 IHEM=1,NHEM
      IOF=(IHEM-1)*MGPP
C
C     First set up array INTRL containing model level directly above
C     required pressure level.
C
      DO 30 I=1,MG
      II=I+IOF
      LM=1
      DO 30 N=1,NLPR
      LMIN=LM+1

      DO 10 L=LMIN,NLM
      IF (PPR(N).LE.PSG(II,L)) GOTO 20
   10 LM=LM+1
   20 INTRL(II,N)=LM
   30 CONTINUE
C
C     Set up interpolation factors in array FINTR for linear
C     interpolation in ln(p).
C
      DO 40 N=1,NLPR
      DO 40 I=1,MG
      II=I+IOF
      LM=INTRL(II,N)
      LP=LM+1
   40 FINTR(II,N)=LOG(PPR(N)/PSG(II,LM))/LOG(PSG(II,LP)/PSG(II,LM))
C
  100 CONTINUE
C
C     Finally interpolate and extrapolate fields.
C     Extra fields required if called from GRMULT (LINTP2=.TRUE.).
C     Final two arguments are switches for upward and downward extrap
C     respectively.  See INTER for details.
C
c+DJL

      CALL INTER(UG,u_pg,NLPR,INTRL,FINTR)
      CALL INTER(VG,v_pg,NLPR,INTRL,FINTR)
      CALL INTER(TG,t_pg,NLPR,INTRL,FINTR)
      CALL INTER(trag(:,:,1),q_pg,NLPR,INTRL,FINTR)
      CALL INTER(GG,g_pg,NLPR,INTRL,FINTR)

c      CALL INTER(UG,u_pg,NLPR,INTRL,FINTR,0,1,1)
c      CALL INTER(VG,v_pg,NLPR,INTRL,FINTR,0,1,1)
c      CALL INTER(TG,t_pg,NLPR,INTRL,FINTR,0,1,1)
c      CALL INTER(trag(:,:,1),q_pg,NLPR,INTRL,FINTR,0,1,1)
c      CALL INTER(GG,g_pg,NLPR,INTRL,FINTR,0,1,1)

c-DJL
ccDJL      IF (LINTP2) THEN
ccDJL         CALL INTER(ZG ,WKINT,NLPR,INTRL,FINTR,1,1,0)
ccDJL         CALL INTER(DG ,WKINT,NLPR,INTRL,FINTR,1,0,0)
ccDJL         CALL INTER(GHG,WKINT,NLPR,INTRL,FINTR,1,1,4)
ccDJL      ENDIF
ccDJL      CALL INTER(OMG,WKINT,NLPR,INTRL,FINTR,1,2,2)
ccDJL      CALL INTER(UG ,WKINT,NLPR,INTRL,FINTR,1,1,0)
ccDJL      CALL INTER(VG ,WKINT,NLPR,INTRL,FINTR,1,1,0)
ccDJL      CALL INTER(TG ,WKINT,NLPR,INTRL,FINTR,1,0,3)
ccDJL      CALL INTER(QG ,WKINT,NLPR,INTRL,FINTR,1,0,0)
ccDJL      CALL INTER(HG ,WKINT,NLPR,INTRL,FINTR,1,0,5)
ccDJL      CALL INTER(FUG,WKINT,NLPR,INTRL,FINTR,1,0,5)
ccDJL      CALL INTER(FVG,WKINT,NLPR,INTRL,FINTR,1,0,5)
C           
      RETURN
      END


                     



