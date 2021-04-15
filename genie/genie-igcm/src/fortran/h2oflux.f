      SUBROUTINE H2OFLUX(CKD,NLEV,ULAY,H2O,DP,PMID,TMID,QULAY,QH2O,QDP,
     $     QP,QT,TLEV,CLOUD,TRCL,MTRCL,WVCNT,UH2O,PH2O,
     $     TH2O,FUP,FDWN)
C     
C     Subroutine H2OFLUX calculates the irradiances (upward, downward 
C     and net) for water vapour line absorption. Wideband transmittance
C     is used for the upward, and modified wideband transmittance is 
C     used for the downward irradiances, which cover the whole 
C     thermal IR spectrum (0-3000)
C     
C     INPUT:  Water vapour continuum index (CKD)
C     Number of levels in the atmosphere without the surface (NLEV)
C     Abs. amount between 2 successive levels (ULAY)
C     Water vapour mid-point mixing ratio (H2O)
C     Thickness of the atmospheric layers (DP)
C     Mid-point pressures (PMID)
C     Mid-point temperatures(TMID)
C     Abs. amount in sub-layers (QULAY)
C     Water vapour mixing ratio in sub-layers (QH2O)
C     Thickness of the sub-layers (QDP)
C     Pressure in sub-layers (QP)
C     Temperature in sub-layers (QT)
C     Temperature at mid-levels (TLEV)
C     Cloud index (CLOUD)
C     Cloud transmittance (TRCL)
C     Cloud transmittance for the modified transm. case (MTRCL)
C     
C     OUTPUT: Effective water vapour amount (WVCNT)
C     Water vapour - path parameters (UH2O,PH2O,TH2O)
C     Upward and Downward irradiances at each mid-level(FUP,FDWN)

      IMPLICIT NONE
#include "parray.cmn"
      include 'files.cmn'
C-----------------
C     Input Variables
C-----------------
      INTEGER CKD,NLEV,CLOUD
      REAL ULAY(MXGAS,MXLEV)
      REAL H2O(MXLEV)
      REAL DP(MXLEV)
      REAL PMID(MXLEV)
      REAL TMID(MXLEV)
      REAL QULAY(MXLEV)
      REAL QH2O(MXLEV)
      REAL QDP(MXLEV)
      REAL QP(MXLEV)
      REAL QT(MXLEV)
      REAL TLEV(0:MXLEV)
      REAL TRCL(0:MXLEV-1,MXLEV)
      REAL MTRCL(0:MXLEV-1,MXLEV)

C------------------
C     Output Variables
C------------------

      REAL WVCNT(0:MXLEV-1,MXLEV)
      REAL UH2O(0:MXLEV-1,MXLEV)
      REAL PH2O(0:MXLEV-1,MXLEV)
      REAL TH2O(0:MXLEV-1,MXLEV)
      REAL FUP(0:MXLEV),FDWN(0:MXLEV)

C--------------------
C     Internal Variables
C--------------------

      INTEGER ILAY,IUP,IDWN,IFAIL
      INTEGER MOD               ! Index for transmittance type
                                ! (1 : Modified, 2 : Standard)

      REAL PF(0:MXLEV),DPF(MXLEV) ! Matrices used for the calculation
                                ! of the irradiances

      REAL UPATH                ! Absorber amount in an atmospheric path
      REAL WVEFF                ! Effective water vapour abs. amount
      REAL PEFF,TEFF            ! Effective pressure & effective temp.
      REAL CWVC                 ! Parameter 1/(296K)
      PARAMETER(CWVC=3.3783783e-03)
      REAL PLANCK               ! Planck function integrated over a band
      REAL TRANS                ! Transmittance of a layer

C     Pre-computed tables for H2O-line absorption
      REAL AWVL(2,34,28),BWVL(2,34,28)
      REAL CWVL(2,34,28),DWVL(2,34,28)

C     Pre-computed tables for H2O-continuum absorption
      REAL AWV(2,41,29,22),BWV(2,41,29,22)
      REAL CWV(2,41,29,22),DWV(2,41,29,22)
C
C     SAVE statements ensure presevation of lookup tables
C     for subsequent calls to the function.
C     NB This distributed initialisation is bad programming.
C
      SAVE AWVL,BWVL,CWVL,DWVL
      SAVE AWV,BWV,CWV,DWV

      integer ncid

      INTEGER IFIRST
      DATA IFIRST/1/

C     READ in tables
      IF (IFIRST.EQ.1) THEN
         IFIRST=0
!-------------------------------------------
!     open the water vapour file
!-------------------------------------------
         print*,' Opening ',fname_waterlines(1:ifname_waterlines)
         call open_file_nc(fname_waterlines(1:ifname_waterlines),ncid)
!-------------------------------------------
!     read in variables from water vapour file
!-------------------------------------------
         call get3d_data_nc(ncid,'awvl',2,34,28,awvl,ifail)
         call get3d_data_nc(ncid,'bwvl',2,34,28,bwvl,ifail)
         call get3d_data_nc(ncid,'cwvl',2,34,28,cwvl,ifail)
         call get3d_data_nc(ncid,'dwvl',2,34,28,dwvl,ifail)
         call get4d_data_nc(ncid,'awv',2,41,29,22,awv,ifail)
         call get4d_data_nc(ncid,'bwv',2,41,29,22,bwv,ifail)
         call get4d_data_nc(ncid,'cwv',2,41,29,22,cwv,ifail)
         call get4d_data_nc(ncid,'dwv',2,41,29,22,dwv,ifail)
!-------------------------------------------
!     close the water vapour file
!-------------------------------------------
         call close_file_nc(fname_waterlines(1:ifname_waterlines),
     :        ncid)
      ENDIF

C--------------------------------------------------------------------
C     Calculate irradiances
C--------------------------------------------------------------------

C     Boundary Conditions
      FUP(0)=PLANCK(0,TLEV(0))
      FDWN(NLEV)=0.0
C-----------------------------------------
C     Calculations for the upward irradiances
C-----------------------------------------
      MOD=1
C
C     Calculate arrays PF and DPF used in the calc. of the up. fluxes
C     PF:  Integrated Planck function at atmospheric levels
C     DPF: Differences of the PFs

      PF(0)=PLANCK(0,TLEV(0))

      DO ILAY=1,NLEV
         PF(ILAY)=PLANCK(0,TLEV(ILAY))
      END DO

      DO ILAY=1,NLEV
         DPF(ILAY)=PLANCK(0,TLEV(ILAY))-PLANCK(0,TLEV(ILAY-1))
      END DO

C     Initialise the irradiances

      DO ILAY=1,NLEV
         FUP(ILAY)=PF(ILAY)
      END DO
C
C     For each layer, calculate the transmittance and update the 
C     irradiances
C
      DO IUP=1,NLEV
         DO IDWN=0,IUP-1
C
C     Calculate absorber amount in the path between IDWN and IUP
C
            UPATH=QULAY(IDWN+1)
            IF ((IUP-IDWN).GT.1) THEN
               DO ILAY=IDWN+2,IUP
                  UPATH=UPATH+ULAY(1,ILAY)
               END DO
            END IF
            if (upath.eq.0.0) then
              upath=1e-20
              print*,'WARNING: upath corrected'
            endif

C     Calculate the effective pressure of the path
            PEFF=QP(IDWN+1)*QULAY(IDWN+1)
            IF ((IUP-IDWN).GT.1) THEN
               DO ILAY=IDWN+2,IUP
                  PEFF=PEFF+PMID(ILAY)*ULAY(1,ILAY)
               END DO
            END IF
            PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
            TEFF=QT(IDWN+1)*QULAY(IDWN+1)
            IF ((IUP-IDWN).GT.1) THEN
               DO ILAY=IDWN+2,IUP
                  TEFF=TEFF+TMID(ILAY)*ULAY(1,ILAY)
               END DO
            END IF
            TEFF=TEFF/UPATH


            IF (CKD.NE.1) THEN

C---  Line absorption
               CALL LINSEARCH(MOD,UPATH,PEFF,TEFF,AWVL,BWVL,
     $              CWVL,DWVL,TRANS)

            ELSE

C---  Include continuum, if required

C     Calculate effective water vapour amount
               IF (QH2O(IDWN+1).GT.(1.0D-04)) THEN
                  WVEFF=(QH2O(IDWN+1)**2)*PMID(IDWN+1)*QDP(IDWN+1)*
     $                 EXP(1800.0*((1.0/TMID(IDWN+1))-CWVC))
               ELSE
                  WVEFF=0.0
               END IF
               IF ((IUP-IDWN).GT.1) THEN
                  DO ILAY=IDWN+2,IUP
                     IF (H2O(ILAY).GT.(1.0D-04)) THEN
                        WVEFF=WVEFF+(H2O(ILAY)**2)*PMID(ILAY)*DP(ILAY)*
     $                       EXP(1800.0*((1.0/TMID(ILAY))-CWVC))
                     END IF
                  END DO
               END IF
               WVEFF=real((1.6178235D-06)*WVEFF)
               WVCNT(IDWN,IUP)=WVEFF

               IF (WVEFF.NE.0.0) THEN
                  IF (LOG10(WVEFF).GT.(-4.5)) THEN
                     CALL CONSEARCH(MOD,UPATH,PEFF,TEFF,WVEFF,AWV,BWV,
     $                    CWV,DWV,TRANS)
                  ELSE
                     CALL LINSEARCH(MOD,UPATH,PEFF,TEFF,AWVL,BWVL,
     $                    CWVL,DWVL,TRANS)
                  END IF
               ELSE
                  CALL LINSEARCH(MOD,UPATH,PEFF,TEFF,AWVL,BWVL,
     $                 CWVL,DWVL,TRANS)
               END IF

            END IF

C     Include cloud transmittance for cloudy skies
            IF (CLOUD.EQ.1) THEN
               TRANS=TRANS*MTRCL(IDWN,IUP)
            END IF

            FUP(IUP)=FUP(IUP)-TRANS*DPF(IDWN+1)

         END DO
      END DO

C--------------------------------------
C     Calculations for the downward fluxes
C--------------------------------------

      MOD=2
C
C     Calculate arrays PF and DPF used in the calc. of the dn. fluxes
C     PF:  Integrated Planck function at mid-points
C     DPF: Differences of the PFs

      DO ILAY=1,NLEV
         PF(ILAY)=PLANCK(0,TMID(ILAY))
      END DO

      DPF(NLEV)=PLANCK(0,TMID(NLEV))

      DO ILAY=1,NLEV-1
         DPF(ILAY)=PLANCK(0,TMID(ILAY))-PLANCK(0,TMID(ILAY+1))
      END DO

C     Initialise the irradiances

      DO ILAY=1,NLEV
         FDWN(ILAY-1)=PF(ILAY)
      END DO
C
C     For each layer, calculate the transmittance and update the 
C     irradiances
C
      DO IUP=1,NLEV
         DO IDWN=0,IUP-1
C
C     Calculate absorber amount in the path between IDWN and IUP
C
            UPATH=0.0
            DO ILAY=IDWN+1,IUP
               UPATH=UPATH+ULAY(1,ILAY)
            END DO
            if (upath.eq.0.0) then
              upath=1e-20
              print*,'WARNING: upath corrected'
            endif
            UH2O(IDWN,IUP)=UPATH


C     Calculate the effective pressure of the path
            PEFF=0.0
            DO ILAY=IDWN+1,IUP
               PEFF=PEFF+PMID(ILAY)*ULAY(1,ILAY)
            END DO
            PEFF=PEFF/UPATH
            PH2O(IDWN,IUP)=PEFF

C     Calculate the effective temperature of the path
            TEFF=0.0
            DO ILAY=IDWN+1,IUP
               TEFF=TEFF+TMID(ILAY)*ULAY(1,ILAY)
            END DO
            TEFF=TEFF/UPATH
            TH2O(IDWN,IUP)=TEFF

            IF (CKD.NE.1) THEN

               CALL LINSEARCH(MOD,UPATH,PEFF,TEFF,AWVL,BWVL,
     $              CWVL,DWVL,TRANS)

            ELSE

C---  Include continuum if required

C     Calculate effective water vapour amount
               WVEFF=0.0
               DO ILAY=IDWN+1,IUP
                  WVEFF=WVEFF+(H2O(ILAY)**2)*PMID(ILAY)*DP(ILAY)*
     $                 EXP(1800.0*((1.0/TMID(ILAY))-CWVC))
               END DO
               WVEFF=real((1.6178235D-06)*WVEFF)

               IF (WVEFF.NE.0.0) THEN
                  IF ((LOG10(WVEFF).LT.(-4.5)).AND.
     $                 (UPATH.LT.1.0D-02)) THEN
                     CALL LINSEARCH(MOD,UPATH,PEFF,TEFF,AWVL,BWVL,
     $                    CWVL,DWVL,TRANS)
                  ELSE
                     CALL CONSEARCH(MOD,UPATH,PEFF,TEFF,WVEFF,AWV,BWV,
     $                    CWV,DWV,TRANS)
                  END IF
               ELSE
                  CALL LINSEARCH(MOD,UPATH,PEFF,TEFF,AWVL,BWVL,
     $                 CWVL,DWVL,TRANS)
               END IF

            END IF

C     Include cloud transmittance for cloudy skies
            IF (CLOUD.EQ.1) THEN
               TRANS=TRANS*TRCL(IDWN,IUP)
            END IF

            FDWN(IDWN)=FDWN(IDWN)-TRANS*DPF(IUP)

         END DO
      END DO
C
      RETURN
      END
