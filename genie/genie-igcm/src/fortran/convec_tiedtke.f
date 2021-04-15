      SUBROUTINE CONVEC_TIEDTKE(TTVDSC,QTVDSC)
      IMPLICIT NONE
C--       
C--   Purpose: This subroutine is a wrapper for the simplified SPEEDY 
C--            model (Molteni 2003) version of the Tiedtke(1993) 
C--            convection scheme within the IGCM3 module of the GENIE
C--            model.
C--   Method:  The subroutine calculates geopotenial (PHI) and dry 
C--            static energy (SE) as well as the saturated specific 
C--            humidity (QS) required by the convection scheme. 
C--            Increments of temperature, specific humidity and 
C--            convective precipitation are created and 
C--            non-dimensionalised to be consistent with the rest of 
C--            IGCM3. 
C--            NB: the convection scheme itself requires 
C--            dimensionalized quanities. 
C--            It also calls a vertical diffusion and shallow 
C--            convection scheme which is required for the deep 
C--            convection scheme to work properly.     
C--      
C--   David Cameron 23/12/2004 (dcam@ceh.ac.uk)
C--
C--  Cloud scheme implemented, based on the approach taken in the
C--  Betts-Miller scheme
C-- 
C--  John Hughes 27/11/2006 (J.K.Hughes@bristol.ac.uk)
C--      
#include "param1.cmn" 
      include 'param2.cmn'
      include 'gridpp.cmn'
      include 'blank.cmn'
      include 'bats.cmn'
      include 'orog_g.cmn'
      include 'physca.cmn'
      include 'legau.cmn'
      include 'cpiers.cmn'
      include 'igcm_cloud.cmn'


      REAL QG(IGC,NL),QTCR(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))
     *            ,(QTCR(1,1),VTRAG(1,1,1))


      INTEGER J,L
      INTEGER ITOP(IGC)         ! top of convection (layer index)
      INTEGER ICNV(IGC)
      REAL PQSAT
      REAL ESCON
      REAL CBMF(IGC)            ! cloud-base mass flux
      REAL PHI(IGC,NL)          ! geopotential
      REAL SE(IGC,NL)           ! dry static energy 
      REAL QS(IGC,NL)           ! local copy of saturation spec. hum.
      REAL TTVDSC(IGC,NL)       ! (OUT) temperature inc. from VDIFSC
      REAL QTVDSC(IGC,NL)       ! (OUT) spec. hum, inc. from VDIFSC
      REAL RRH(IGC,NL)          ! local copy of relative hum.

C
C Create geopotential and dry static energy
C


      DO J=1,IGC

        ESCON=1./PLG(J)
        TG(J,NL)=TG(J,NL)*CT
        QG(J,NL)=QG(J,NL)*CQ
        PHI(J,NL)=(GSG(J,JH)*GA)+XGEOP1(NL)*TG(J,NL)
        SE(J,NL)=CPD*TG(J,NL)+PHI(J,NL)

        DO L=NL-1,1,-1

          TG(J,L)=TG(J,L)*CT
          QG(J,L)=QG(J,L)*CQ
          PHI(J,L)=PHI(J,L+1)+XGEOP2(L+1)*TG(J,L+1)
     &             +XGEOP1(L)*TG(J,L)
          SE(J,L)=CPD*TG(J,L)+PHI(J,L)
          
        ENDDO
        
      ENDDO

C
C Create sat. spec. hum. and rel. hum.; force Q to be +ve
C

      DO J=1,IGC
        DO L=1,NL
          ESCON=1/PLG(J)          
C         QG(J,L)=MAX(QG(j,k),0.)
          QS(J,L)=(ESCON*PQSAT(TG(J,L)/CT)/SIGMA(L))*CQ
          RRH(J,L)=QG(J,L)/QS(J,L)
        ENDDO
      ENDDO
C
C Call deep convection scheme
C
      CALL CONVMF(PLG,SE,QG,QS,ITOP,CBMF,RRCR,TTCR,QTCR)
      
      DO J=1,IGC
        ICNV(J)=NL-ITOP(J)
      ENDDO
C
C Call shallow convection and vertical diffusion
C
      CALL VDIFSC (SE,RRH,QG,QS,PHI,ICNV,TTVDSC,QTVDSC)

C
C Convert fluxes to increments and non-dimensionalise for IGCM
C      
      DO J = 1,IGC

          ESCON=1./(PLG(J))
          RRCR(J)=RRCR(J)*(3600./1000.)


        DO L=1,NL
        
          TTCR(J,L)=(TTCR(J,L)*ESCON*GRDSCP(L))*(3600./CT)*(2./DELT2C)
          QTCR(J,L)=(QTCR(J,L)*ESCON*GRDSIG(L))*(3600./CQ)*(2./DELT2C)
          TTVDSC(J,L)=TTVDSC(J,L)*(3600./CT)*(2./DELT2C)
          QTVDSC(J,L)=QTVDSC(J,L)*(3600./CQ)*(2./DELT2C)
          TG(J,L)=TG(J,L)/CT 
          QG(J,L)=QG(J,L)/CQ

        ENDDO

        
      ENDDO

      

      RETURN
      END
      
      SUBROUTINE VDIFSC (SE,RH,QA,QSAT,PHI,ICNV,
     &                   TTENVD,QTENVD)      

      IMPLICIT NONE
C--
C--   SUBROUTINE VDIFSC (UA,VA,SE,RH,QA,QSAT,PHI,ICNV,
C--  &                   UTENVD,VTENVD,TTENVD,QTENVD)
C--
C--   Purpose: Compute tendencies of momentum, energy and moisture
C--            due to vertical diffusion and shallow convection
C--   Input:   UA     = u-wind                           (3-dim)
C--            VA     = v-wind                           (3-dim)
C--            SE     = dry static energy                (3-dim)
C--            RH     = relative humidity [0-1]          (3-dim)
C--            QA     = specific humidity [g/kg]         (3-dim)
C--            QSAT   = saturation sp. humidity [g/kg]   (3-dim)
C--            PHI    = geopotential                     (3-dim)
C--            ICNV   = index of deep convection         (2-dim)
C--   Output:  UTENVD = u-wind tendency                  (3-dim)
C--            VTENVD = v-wind tendency                  (3-dim)
C--            TTENVD = temperature tendency             (3-dim)
C--            QTENVD = sp. humidity tendency [g/(kg s)] (3-dim)
C--
C     Resolution parameters
C
C
C


#include "param1.cmn" 
      include 'param2.cmn' 
      INTEGER NLEV,NGP
      PARAMETER (NLEV=NL, NGP=IGC )
      include 'gridpp.cmn'
      include 'blank.cmn'
      include 'bats.cmn' 
      include 'physca.cmn'
      include 'legau.cmn'
      include 'convtie.cmn'
      include 'cpiers.cmn'
      include 'igcm_cloud.cmn'


      REAL SE(NGP,NLEV),
     &     RH(NGP,NLEV), QA(NGP,NLEV), QSAT(NGP,NLEV),
     &     PHI(NGP,NLEV)


      INTEGER J,K
      INTEGER ICNV(NGP)
      
      REAL ALHC

      REAL TTENVD(NGP,NLEV), QTENVD(NGP,NLEV)

      REAL RSIG(NLEV), RSIG1(NLEV)


      INTEGER NL1,K1
      REAL    CSHC,CVDI,FSHCQ,FSHCSE,FVDIQ,FVDISE,DRH0,FVDIQ2,DRH
      REAL    FCNV,FLUXSE,FLUXQ,SE0,DMSE

C--   1. Initalization

C     N.B. In this routine, fluxes of dry static energy and humidity
C          are scaled in such a way that:
C          d_T/dt = d_F'(SE)/d_sigma,  d_Q/dt = d_F'(Q)/d_sigma

      ALHC = 2501.
      
      NL1  = NLEV-1
      CSHC = DSIGMA(NLEV)/3600.
      CVDI = (SIGMAH(NL1)-SIGMAH(1))/((NL1-1)*3600.)

      FSHCQ  = CSHC/TRSHC
      FSHCSE = CSHC/(TRSHC*CPD)

      FVDIQ  = CVDI/TRVDI
      FVDISE = CVDI/(TRVDS*CPD)

      DO K=1,NL1
        RSIG(K) =1./DSIGMA(K)
        RSIG1(K)=1./(1.-SIGMAH(K))
      ENDDO
      RSIG(NLEV)=1./DSIGMA(NLEV)
   
      DO K=1,NLEV
        DO J=1,NGP
          TTENVD(J,K) = 0.
          QTENVD(J,K) = 0.
        ENDDO
      ENDDO


C--   2. Shallow convection

      DRH0   = RHGRAD*(SIGMA(NLEV)-SIGMA(NL1))
      FVDIQ2 = FVDIQ*SIGMAH(NL1)

      DO J=1,NGP

        DMSE = (SE(J,NLEV)-SE(J,NL1))+ALHC*(QA(J,NLEV)-QSAT(J,NL1))
        DRH  = RH(J,NLEV)-RH(J,NL1)
        FCNV = 1.

        IF (DMSE.GE.0.0) THEN

          IF (ICNV(J).GT.0) FCNV = REDSHC

          FLUXSE         = FCNV*FSHCSE*DMSE
          TTENVD(J,NL1)  = FLUXSE*RSIG(NL1)
          TTENVD(J,NLEV) =-FLUXSE*RSIG(NLEV)

          IF (DRH.GE.0.0) THEN
            FLUXQ          = FCNV*FSHCQ*QSAT(J,NLEV)*DRH
            QTENVD(J,NL1)  = FLUXQ*RSIG(NL1) 
            QTENVD(J,NLEV) =-FLUXQ*RSIG(NLEV)
          ENDIF

        ELSE IF (DRH.GE.DRH0) THEN

          FLUXQ          = FVDIQ2*QSAT(J,NL1)*DRH
          QTENVD(J,NL1)  = FLUXQ*RSIG(NL1) 
          QTENVD(J,NLEV) =-FLUXQ*RSIG(NLEV)

C set shallow cloud cover
          ICFLAG(J,4,1)=NLEV
          ICFLAG(J,4,2)=NL1
          CFRAC(J,4)=cloudshallow

          ENDIF
        ENDDO

C--   3. Vertical diffusion of moisture above the PBL

      DO K=3,NLEV-2

        IF (SIGMAH(K).GT.0.5) THEN

          DRH0   = RHGRAD*(SIGMA(K+1)-SIGMA(K))
          FVDIQ2 = FVDIQ*SIGMAH(K)

          DO J=1,NGP

            DRH=RH(J,K+1)-RH(J,K)

            IF (DRH.GE.DRH0) THEN
              FLUXQ        = FVDIQ2*QSAT(J,K)*DRH
              QTENVD(J,K)  = QTENVD(J,K)  +FLUXQ*RSIG(K)
              QTENVD(J,K+1)= QTENVD(J,K+1)-FLUXQ*RSIG(K+1)
            ENDIF

          ENDDO

        ENDIF

      ENDDO

C--   4. Damping of super-adiabatic lapse rate

      DO K=1,NL1
       DO J=1,NGP

         SE0 = SE(J,K+1)+SEGRAD*(PHI(J,K)-PHI(J,K+1))

         IF (SE(J,K).LT.SE0) THEN
           FLUXSE      = FVDISE*(SE0-SE(J,K))

           TTENVD(J,K) = TTENVD(J,K)+FLUXSE*RSIG(K)
           DO K1=K+1,NLEV 

             TTENVD(J,K1) = TTENVD(J,K1)-FLUXSE*RSIG1(K)

           ENDDO
         ENDIF

       ENDDO
      ENDDO
C--

      RETURN
      END
