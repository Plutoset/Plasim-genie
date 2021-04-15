      SUBROUTINE IRRAD(GAS,CKD,NLEV,PRES,PFLUX,TEMP,MMR,CL,ACL,LWCCL,
     $     BASCL,TOPCL,FUP,FDWN,FNET,DP,LLBLM,GR)

      IMPLICIT NONE

C     3-1-98
C     Piers commented out DP calc - it is now passed
C     18-1-00 SH
C     Fuller gas tables and choice between LBLM or NBM gas tables.
C     
C     Subroutine IRRAD calculates the the upward, downward and net
C     irradiances at each level of a given atmospheric profile, for a
C     given absorber
C     
C     INPUT:  Selection of the absorbers (GAS)
C     Water vapour continuum index (CKD)
C     Number of levels in the atmosphere without the surface (NLEV)
C     Pressure at each level (PRES)
C     Pressure at flux levels (PFLUX)
C     Temperature at each level (TEMP)
C     Absorber mass mixing ratio at each level (MMR)
C     Selection of cloud types (CL)
C     Cloud amount as a fraction (ACL)
C     Liquid water content (LWCCL)
C     Cloud base and top (BASCL,TOPCL)
C     Slab pressure DP
C     Gas table switch LLBLM
C     OUTPUT: Upward, Downward and Net irradiances at each mid-level
C     (FUP,FDWN,FNET)

#include "parray.cmn"
      include 'files.cmn'

C-----------------
C     Input Variables
C-----------------
      INTEGER GAS(MXGAS),CKD
      INTEGER NLEV
      REAL PRES(0:MXLEV),PFLUX(0:MXLEV)
      REAL TEMP(0:MXLEV),MMR(MXGAS,0:MXLEV)
      INTEGER CL(0:MXCL)
      REAL ACL(0:MXCL),LWCCL(MXCL),BASCL(0:MXCL),TOPCL(0:MXCL)
      REAL GR                   ! Constants  (grav. acceleration)
C------------------
C     Output Variables
C------------------
      REAL FUP(0:MXLEV),FDWN(0:MXLEV),FNET(0:MXLEV)
C--------------------
C     Internal Variables
C--------------------

      INTEGER IGAS,IBND,IHAL,ILAY,IUP,IDWN,ICL,IFAIL
      INTEGER CLOUD             ! Clear/Cloudy sky index
      REAL ULAY(MXGAS,MXLEV)    ! Abs. amount betwn 2 successive levels
      REAL H2O(MXLEV)           ! Water vapour mid-point mixing ratio
      REAL DP(MXLEV)            ! Thickness of the atmospheric layers
      REAL PMID(MXLEV)          ! Mid-point pressures
      REAL TMID(MXLEV)          ! Mid-point temperatures
      REAL QULAY(MXLEV)         ! Abs. amount in sub-layers
      REAL QH2O(MXLEV)          ! Water vap. mixing ratio in sub-layers
      REAL QDP(MXLEV)           ! Thickness of the sub-layers
      REAL QP(MXLEV)            ! Pressure in sub-layers
      REAL QT(MXLEV)            ! Temperature in sub-layers
      REAL TLEV(0:MXLEV)        ! Temperature at mid-levels
      REAL TSURF                ! Surface temperature

      REAL UPATH                ! Absorber amount in atmospheric path
      REAL WVEFF                ! Effective water vapour abs. amount
      REAL PEFF,TEFF            ! Effective pressure & effective temp.

      REAL WVCNT(0:MXLEV-1,MXLEV) ! Effective H2O amount for
                                ! continuum absorption
      REAL UH2O(0:MXLEV-1,MXLEV) ! H2O amount
      REAL PH2O(0:MXLEV-1,MXLEV) ! H2O effective pressure
      REAL TH2O(0:MXLEV-1,MXLEV) ! H2O effective pressure

      REAL TRANS                ! Transmittance
      REAL TRCNT                ! Continuum transmittance
      REAL HALTRAN              ! Halocarbon transmittance

      REAL SLP,INTCPT           ! Variables used for interpolation

      REAL TRIN(0:MXLEV-1,MXLEV) ! Input/Output trans. used for
      REAL TROUT(0:MXLEV-1,MXLEV) ! the treatment of spectral overlap

      REAL PLANCK               ! Planck function
      REAL PF(MXLEV),DPF(0:MXLEV) ! Matrices used for the calculation
                                ! of the irradiances
      REAL TRCL(0:MXLEV-1,MXLEV) ! Cloud transmittance
      REAL MTRCL(0:MXLEV-1,MXLEV) ! Modified tran. case

C     Pre-computed tables for gas absorption

      REAL AH2O(MXBAND,48,28),AGAS(MXBAND-1,48,28)
      REAL BH2O(MXBAND,48,28),BGAS(MXBAND-1,48,28)
      REAL CH2O(MXBAND,48,28),CGAS(MXBAND-1,48,28)
      REAL DH2O(MXBAND,48,28),DGAS(MXBAND-1,48,28)
      REAL AN03(48,28),BN03(48,28),CN03(48,28),DN03(48,28)
      REAL AC02(48,28),BC02(48,28),CC02(48,28),DC02(48,28)
      REAL AC06(48,28),BC06(48,28),CC06(48,28),DC06(48,28)
      REAL AC07(48,28),BC07(48,28),CC07(48,28),DC07(48,28)
C
C     SAVE statements ensure presevation of lookup tables
C     for subsequent calls to the function.
C     NB This distributed initialisation is bad programming.
C
      SAVE AH2O,AGAS,BH2O,BGAS,CH2O,CGAS,DH2O,DGAS 
      SAVE AN03,BN03,CN03,DN03,AC02,BC02,CC02,DC02
      SAVE AC06,BC06,CC06,DC06,AC07,BC07,CC07,DC07

      REAL FGUP(0:MXLEV)        ! Irradiances in bands of absorbers
      REAL FGDWN(0:MXLEV)       ! (other than the water vapour
                                !  0-3000 cm-1 band)
      REAL FUPCL(0:MXCL,MXLEV)  ! Irradiances at cloud levels in
      REAL FDWNCL(0:MXCL,MXLEV) ! the 0-3000 cm-1 band

      REAL FBACK                ! Background upward irradiance
      LOGICAL LLBLM
      integer ncid
      INTEGER IFIRST
      DATA IFIRST/1/

C     READ in tables
      IF (IFIRST.EQ.1) THEN
         IFIRST=0
         IF(LLBLM)THEN
            print*,' Opening file ',fname_gastabl(1:ifname_gastabl)
            call open_file_nc(fname_gastabl(1:ifname_gastabl), ncid)
         ELSE
            print*,' Opening file ',fname_gastabn(1:ifname_gastabn)
            call open_file_nc(fname_gastabn(1:ifname_gastabn), ncid)
         ENDIF
!-------------------------------------------
!     read in variables from water vapour file
!-------------------------------------------
         call get3d_data_nc(ncid,'ah2o',mxband,48,28,ah2o,ifail)
         call get3d_data_nc(ncid,'bh2o',mxband,48,28,bh2o,ifail)
         call get3d_data_nc(ncid,'ch2o',mxband,48,28,ch2o,ifail)
         call get3d_data_nc(ncid,'dh2o',mxband,48,28,dh2o,ifail)
         call get3d_data_nc(ncid,'agas',mxband-1,48,28,agas,ifail)
         call get3d_data_nc(ncid,'bgas',mxband-1,48,28,bgas,ifail)
         call get3d_data_nc(ncid,'cgas',mxband-1,48,28,cgas,ifail)
         call get3d_data_nc(ncid,'dgas',mxband-1,48,28,dgas,ifail)
         call get2d_data_nc(ncid,'an03',48,28,an03,ifail)
         call get2d_data_nc(ncid,'bn03',48,28,bn03,ifail)
         call get2d_data_nc(ncid,'cn03',48,28,cn03,ifail)
         call get2d_data_nc(ncid,'dn03',48,28,dn03,ifail)
         call get2d_data_nc(ncid,'ac02',48,28,ac02,ifail)
         call get2d_data_nc(ncid,'bc02',48,28,bc02,ifail)
         call get2d_data_nc(ncid,'cc02',48,28,cc02,ifail)
         call get2d_data_nc(ncid,'dc02',48,28,dc02,ifail)
         call get2d_data_nc(ncid,'ac06',48,28,ac06,ifail)
         call get2d_data_nc(ncid,'bc06',48,28,bc06,ifail)
         call get2d_data_nc(ncid,'cc06',48,28,cc06,ifail)
         call get2d_data_nc(ncid,'dc06',48,28,dc06,ifail)
         call get2d_data_nc(ncid,'ac07',48,28,ac07,ifail)
         call get2d_data_nc(ncid,'bc07',48,28,bc07,ifail)
         call get2d_data_nc(ncid,'cc07',48,28,cc07,ifail)
         call get2d_data_nc(ncid,'dc07',48,28,dc07,ifail)
         call close_file_nc('gastable file ', ncid)
         WRITE(6,*)'gases on:',gas(1),gas(2),gas(3),gas(4),gas(5),
     :        gas(6),gas(7),gas(8)
      ENDIF
C     
C     Calculate pressure, temperature absorber amount and water vapour
C     mixing ratio between the first two mid-levels 
C     (PMID,TMID,ULAY,H2O), split the layer into 2 and calculate 
C     the same for the upper sub-layer (QLAY,QH2O,QP,QT). 
C     Sub-layers are required for calculations involving modified 
C     transmittance. Also calculate the  temperature at 
C     mid-levels (TLEV)
C     
      PMID(1)=0.5*(0.5*(PRES(1)+PRES(2))+PRES(0))
      QP(1)=0.5*(PMID(1)+0.5*(PRES(1)+PRES(2)))

      SLP=(TEMP(0)-TEMP(2))/(PRES(0)-PRES(2))
      INTCPT=PRES(0)*TEMP(2)-PRES(2)*TEMP(0)
      INTCPT=INTCPT/(PRES(0)-PRES(2))
      TMID(1)=SLP*PMID(1)+INTCPT
      QT(1)=SLP*QP(1)+INTCPT

C     Temperature at the surface and the first mid-level
      TLEV(0)=TEMP(0)
      TLEV(1)=0.5*SLP*(PRES(1)+PRES(2))+INTCPT

      DO IGAS=1,MXGAS

         IF (GAS(IGAS).EQ.1) THEN

            IF ((IGAS.EQ.6).AND.(GAS(2).EQ.1)) THEN
               ULAY(IGAS,1)=ULAY(2,1)
               GOTO 1972
            END IF

            IF ((IGAS.EQ.7).AND.(GAS(3).EQ.1)) THEN
               ULAY(IGAS,1)=ULAY(3,1)
               GOTO 1972
            END IF

            SLP=(MMR(IGAS,0)-MMR(IGAS,2))/(PRES(0)-PRES(2))
            INTCPT=PRES(0)*MMR(IGAS,2)-PRES(2)*MMR(IGAS,0)
            INTCPT=INTCPT/(PRES(0)-PRES(2))
            ULAY(IGAS,1)=SLP*PMID(1)+INTCPT
            IF (IGAS.EQ.1) THEN
               H2O(1)=ULAY(IGAS,1)
            END IF
            ULAY(IGAS,1)=ULAY(IGAS,1)*DP(1)/GR

            IF (IGAS.EQ.1) THEN

               QULAY(1)=SLP*QP(1)+INTCPT
               QH2O(1)=QULAY(1)
               QDP(1)=0.25*(2*PRES(0)-PRES(1)-PRES(2))
               QULAY(1)=QULAY(1)*QDP(1)/GR

            END IF

 1972       CONTINUE

         END IF
      END DO

C     Calculate pressures and temperatures and absorber amounts
C     between two successive mid-levels and in the sub-layers

      DO ILAY=2,NLEV-1

C     Mid-point & sub-layer pressures

         PMID(ILAY)=PRES(ILAY)
         QP(ILAY)=0.5*(PMID(ILAY)+0.5*(PRES(ILAY)+PRES(ILAY+1)))

C     Mid-point & sub-layer temperatures

         TMID(ILAY)=TEMP(ILAY)

         SLP=(TEMP(ILAY)-TEMP(ILAY+1))/(PRES(ILAY)-PRES(ILAY+1))
         INTCPT=PRES(ILAY)*TEMP(ILAY+1)-PRES(ILAY+1)*TEMP(ILAY)
         INTCPT=INTCPT/(PRES(ILAY)-PRES(ILAY+1))
         QT(ILAY)=SLP*QP(ILAY)+INTCPT

C     Temperature at mid-levels
         TLEV(ILAY)=0.5*SLP*(PRES(ILAY)+PRES(ILAY+1))+INTCPT

C     Absorber amounts

C     DP(ILAY)=0.5*(PRES(ILAY-1)-PRES(ILAY+1))

         DO IGAS=1,MXGAS

            IF (GAS(IGAS).EQ.1) THEN

               IF ((IGAS.EQ.6).AND.(GAS(2).EQ.1)) THEN
                  ULAY(IGAS,ILAY)=ULAY(2,ILAY)
                  GOTO 1973
               END IF

               IF ((IGAS.EQ.7).AND.(GAS(3).EQ.1)) THEN
                  ULAY(IGAS,ILAY)=ULAY(3,ILAY)
                  GOTO 1973
               END IF

               ULAY(IGAS,ILAY)=MMR(IGAS,ILAY)*DP(ILAY)/GR
               IF (IGAS.EQ.1) THEN
                  H2O(ILAY)=MMR(IGAS,ILAY)
               END IF

               IF (IGAS.EQ.1) THEN

                  SLP=(MMR(IGAS,ILAY)-MMR(IGAS,ILAY+1))/
     $                 (PRES(ILAY)-PRES(ILAY+1))
                  INTCPT=PRES(ILAY)*MMR(IGAS,ILAY+1)-
     $                 PRES(ILAY+1)*MMR(IGAS,ILAY)
                  INTCPT=INTCPT/(PRES(ILAY)-PRES(ILAY+1))
                  QULAY(ILAY)=SLP*QP(ILAY)+INTCPT
                  QH2O(ILAY)=QULAY(ILAY)
                  QDP(ILAY)=0.25*(PRES(ILAY-1)-PRES(ILAY+1))
                  QULAY(ILAY)=QULAY(ILAY)*QDP(ILAY)/GR

               END IF

 1973          CONTINUE

            END IF

         END DO

      END DO

C     Calculate pressure, temperature and absorber amount for
C     the upper atmospheric layer and its sub-layers

      PMID(NLEV)=0.5*(PRES(NLEV)+0.5*PRES(NLEV-1))
      QP(NLEV)=0.5*PMID(NLEV)

      TMID(NLEV)=TEMP(NLEV)
      SLP=(TEMP(NLEV-1)-TEMP(NLEV))/(PRES(NLEV-1)-PRES(NLEV))
      INTCPT=PRES(NLEV-1)*TEMP(NLEV)-PRES(NLEV)*TEMP(NLEV-1)
      INTCPT=INTCPT/(PRES(NLEV-1)-PRES(NLEV))
      QT(NLEV)=SLP*QP(NLEV)+INTCPT

C     Temperature at the upper mid-level
      TLEV(NLEV)=0.5*SLP*PRES(NLEV)+INTCPT

      DO IGAS=1,MXGAS

         IF (GAS(IGAS).EQ.1) THEN

            IF ((IGAS.EQ.6).AND.(GAS(2).EQ.1)) THEN
               ULAY(IGAS,NLEV)=ULAY(2,NLEV)
               GOTO 1974
            END IF

            IF ((IGAS.EQ.7).AND.(GAS(3).EQ.1)) THEN
               ULAY(IGAS,NLEV)=ULAY(3,NLEV)
               GOTO 1974
            END IF

            ULAY(IGAS,NLEV)=MMR(IGAS,NLEV)*DP(NLEV)/GR
            IF (IGAS.EQ.1) THEN
               H2O(NLEV)=MMR(IGAS,NLEV)
            END IF

            IF (IGAS.EQ.1) THEN

               SLP=(MMR(IGAS,NLEV-1)-MMR(IGAS,NLEV))/
     $              (PRES(NLEV-1)-PRES(NLEV))
               INTCPT=PRES(NLEV-1)*MMR(IGAS,NLEV)-
     $              PRES(NLEV)*MMR(IGAS,NLEV-1)
               INTCPT=INTCPT/(PRES(NLEV-1)-PRES(NLEV))

               QULAY(NLEV)=SLP*QP(NLEV)+INTCPT
               QH2O(NLEV)=QULAY(NLEV)
               QDP(NLEV)=0.25*PRES(NLEV-1)
               QULAY(NLEV)=QULAY(NLEV)*QDP(NLEV)/GR

            END IF
 1974       CONTINUE

         END IF

      END DO

C     Set cloud index

      CLOUD=0
      DO ICL=0,MXCL
         IF (CL(ICL).EQ.1) THEN
            CLOUD=1
         END IF
      END DO

C     If cloudy skies then call the cloud scheme

      IF (CLOUD.EQ.1) THEN
         CALL CLDTRAN(NLEV,PFLUX,DP,CL,ACL,LWCCL,BASCL,TOPCL,
     $        TRCL,MTRCL)
      END IF

C---------------------------------
C     Calculation of  the irradiances
C---------------------------------

C---  Background upward irradiance

      TSURF=TLEV(0)
      FBACK=PLANCK(0,TSURF)

C---  Initialise irradiances

      DO ILAY=0,NLEV
         FUP(ILAY)=FBACK
         FDWN(ILAY)=0.0
         FNET(ILAY)=FBACK
      END DO

C---  Calculate the irradiances for water vapour absorption.
C     The wideband covers the whole thermal IR spectrum (0-3000cm-1)

      IF (GAS(1).EQ.1) THEN

         CALL H2OFLUX(CKD,NLEV,ULAY,H2O,DP,PMID,TMID,QULAY,QH2O,QDP,
     $        QP,QT,TLEV,CLOUD,TRCL,MTRCL,WVCNT,UH2O,PH2O,
     $        TH2O,FUP,FDWN)

C---  Avoid smoothing at cloud levels

         IF (CLOUD.EQ.1) THEN
            DO ICL=0,MXCL
               IF (CL(ICL).EQ.1) THEN
                  DO ILAY=1,NLEV
                     IF ((100.*PFLUX(ILAY).LE.(BASCL(ICL)+1.D4)).AND.
     $                    (100.*PFLUX(ILAY).GE.(TOPCL(ICL)-1.D4))) THEN
                        FUPCL(ICL,ILAY)=FUP(ILAY)
                        FDWNCL(ICL,ILAY)=FDWN(ILAY)
                     ELSE
                        FUPCL(ICL,ILAY)=0.0
                        FDWNCL(ICL,ILAY)=0.0
                     END IF
                  END DO
               END IF
            END DO
         END IF

C---  Smoothing of the irradiance profiles

         CALL SMFLUX(NLEV,PFLUX,FUP)
         CALL SMFLUX(NLEV,PFLUX,FDWN)

         IF (CLOUD.EQ.1) THEN
            DO ICL=0,MXCL
               IF (CL(ICL).EQ.1) THEN
                  DO ILAY=1,NLEV
                     IF (FUPCL(ICL,ILAY).NE.0.0) THEN
                        FUP(ILAY)=FUPCL(ICL,ILAY)
                        FDWN(ILAY)=FDWNCL(ICL,ILAY)
                     END IF
                  END DO
               END IF
            END DO
         END IF

         FUP(0)=PLANCK(0,TLEV(0))
         FDWN(NLEV)=0.0

         DO ILAY=0,NLEV
            FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
         END DO

      END IF

C-----------------------------------------------------------
C     S P E C T R A L    B A N D S    I N    T H E    R W B M
C-----------------------------------------------------------
C     IBND     0:      0 - 3000 cm-1   H2O
C     IBND     1:    540 -  820 cm-1   H2O + CO2
C     IBND     2:    980 - 1100 cm-1   H2O +  O3 + CO2
C     IBND     3:   1200 - 1400 cm-1   H2O + CH4
C     IBND     4:   2100 - 2300 cm-1   H2O + N2O
C     IBND     5:    900 -  980 cm-1   H2O + CO2
C     IBND     6:    650 -  770 cm-1   H2O +  O3 + CO2
C     IBND     7:    550 -  630 cm-1   H2O + N2O + CO2
C     IBND     8:   1100 - 1200 cm-1   H2O + N2O
C     IBND  9-12:    820 -  900 cm-1   H2O + HALOCARBON
C-----------------------------------------------------------

C-----------------------
C     Loop over bands
C-----------------------

      DO IBND=1,MXBAND

C     Skip the loop, if there is no gas that absorbs in the band

         IF ((IBND.EQ.1).AND.(GAS(2).EQ.0)) THEN
            GOTO 1975
         END IF
         IF ((IBND.EQ.2).AND.(GAS(3).EQ.0).AND.(GAS(6).EQ.0)) THEN
            GOTO 1975
         END IF
         IF ((IBND.EQ.3).AND.(GAS(4).EQ.0).AND.(GAS(5).EQ.0)) THEN
            GOTO 1975
         END IF
         IF ((IBND.EQ.4).AND.(GAS(5).EQ.0)) THEN
            GOTO 1975
         END IF
         IF ((IBND.EQ.5).AND.(GAS(6).EQ.0)) THEN
            GOTO 1975
         END IF
         IF ((IBND.EQ.6).AND.(GAS(7).EQ.0)) THEN
            GOTO 1975
         END IF
         IF ((IBND.EQ.7).AND.(GAS(5).EQ.0)) THEN
            GOTO 1975
         END IF
         IF ((IBND.EQ.8).AND.(GAS(5).EQ.0)) THEN
            GOTO 1975
         END IF
         IF ((IBND.EQ.9).AND.(GAS(8).EQ.0)) THEN
            GOTO 1975
         END IF

C     Initialise input transmittances (used for overlap treatment)

         DO IUP=1,NLEV
            DO IDWN=0,NLEV-1
               IF (CLOUD.EQ.1) THEN
                  TRIN(IDWN,IUP)=TRCL(IDWN,IUP) ! Cloudy skies
               ELSE
                  TRIN(IDWN,IUP)=1.0 ! Clear skies
               END IF
            END DO
         END DO

C     Calculate arrays PF and DPF used in the calculation of the fluxes
C     PF:  Integrated Planck function at mid-points
C     DPF: Differences of the PFs

         DO ILAY=1,NLEV
            PF(ILAY)=PLANCK(IBND,TMID(ILAY))
         END DO

         DPF(0)=PLANCK(IBND,TSURF)-PLANCK(IBND,TMID(1))
         DPF(NLEV)=PLANCK(IBND,TMID(NLEV))

         DO ILAY=1,NLEV-1
            DPF(ILAY)=PLANCK(IBND,TMID(ILAY))-PLANCK(IBND,TMID(ILAY+1))
         END DO


C---  If overlap with H2O then calculate H2O irradiances in the
C     absorber's band and subtract them from the total

         IF (GAS(1).EQ.1) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1

                  UPATH=UH2O(IDWN,IUP)
                  PEFF=PH2O(IDWN,IUP)
                  TEFF=TH2O(IDWN,IUP)

                  CALL GASSEARCH(1,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                 CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                 CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                 CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                  TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

C     Calculate self-broadened continuum transmittance
                  IF (CKD.EQ.1) THEN
                     WVEFF=WVCNT(IDWN,IUP)
c                     CALL GASCNT(IBND,WVEFF,TEFF,TRCNT)
                     CALL GASCNT(IBND,WVEFF,TRCNT)
                     TROUT(IDWN,IUP)=TROUT(IDWN,IUP)*TRCNT
                  END IF

                  FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                  FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)

               END DO
            END DO

            IF ((IBND.EQ.6).AND.(GAS(2).EQ.1)) THEN
               CONTINUE
            ELSE
               IF ((IBND.EQ.7).AND.(GAS(2).EQ.1)) THEN
                  CONTINUE
               ELSE
                  DO ILAY=0,NLEV
                     FUP(ILAY)=FUP(ILAY)-FGUP(ILAY)
                     FDWN(ILAY)=FDWN(ILAY)-FGDWN(ILAY)
                     FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
                  END DO
               END IF
            END IF

            DO IUP=1,NLEV
               DO IDWN=0,NLEV-1
                  TRIN(IDWN,IUP)=TROUT(IDWN,IUP)
               END DO
            END DO

         END IF

C     Set halocarbon band index (for different overlap cases)

         IF (IBND.EQ.9) THEN
            IHAL=9
            IF (GAS(2).EQ.1) THEN
               IHAL=10
            END IF
            IF (GAS(3).EQ.1) THEN
               IHAL=11
            END IF
            IF ((GAS(2).EQ.1).AND.GAS(3).EQ.1) THEN
               IHAL=12
            END IF
         END IF

C------------------------
C     BAND 1: 540-820 cm-1
C------------------------

         IF (IBND.EQ.1) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

                                ! CO2 absorption

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                  UPATH=0.0
                  DO ILAY=IDWN+1,IUP
                     UPATH=UPATH+ULAY(2,ILAY)
                  END DO

C     Calculate the effective pressure of the path
                  PEFF=0.0
                  DO ILAY=IDWN+1,IUP
                     PEFF=PEFF+PMID(ILAY)*ULAY(2,ILAY)
                  END DO
                  PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                  TEFF=0.0
                  DO ILAY=IDWN+1,IUP
                     TEFF=TEFF+TMID(ILAY)*ULAY(2,ILAY)
                  END DO
                  TEFF=TEFF/UPATH

                  CALL GASSEARCH(2,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                 CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                 CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                 CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                  TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

               END DO
            END DO

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1
                  FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                  FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
               END DO
            END DO

            IF (GAS(1).EQ.1) THEN
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            ELSE
               DO ILAY=0,NLEV
                  FUP(ILAY)=FBACK-PLANCK(IBND,TSURF)+FGUP(ILAY)
                  FDWN(ILAY)=FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            END IF

            GOTO 1975

         END IF

C------------------------
C     BAND 2: 980-1100 cm-1
C------------------------

         IF (IBND.EQ.2) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

            IF (GAS(3).EQ.1) THEN ! O3 absorption

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(3,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(3,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(3,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(3,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,NLEV-1
                     TRIN(IDWN,IUP)=TROUT(IDWN,IUP)
                  END DO
               END DO

            END IF

            IF (GAS(6).EQ.1) THEN ! CO2 absorption

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(6,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(6,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(6,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(6,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

            END IF

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1
                  FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                  FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
               END DO
            END DO

            IF (GAS(1).EQ.1) THEN
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            ELSE
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)-PLANCK(IBND,TSURF)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            END IF

            GOTO 1975

         END IF

C-------------------------
C     BAND 3: 1200-1400 cm-1
C-------------------------

         IF (IBND.EQ.3) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

            IF (GAS(4).EQ.1) THEN ! CH4 absorption

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(4,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(4,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(4,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(4,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,NLEV-1
                     TRIN(IDWN,IUP)=TROUT(IDWN,IUP)
                  END DO
               END DO

            END IF

            IF (GAS(5).EQ.1) THEN ! N2O absorption

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(5,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(5,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(5,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(5,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

            END IF
            DO IUP=1,NLEV
               DO IDWN=0,IUP-1
                  FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                  FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
               END DO
            END DO

            IF (GAS(1).EQ.1) THEN
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            ELSE
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)-PLANCK(IBND,TSURF)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            END IF

            GOTO 1975

         END IF

C-------------------------
C     BAND 4: 2100-2300 cm-1
C-------------------------

         IF (IBND.EQ.4) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

                                ! N2O absorption

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                  UPATH=0.0
                  DO ILAY=IDWN+1,IUP
                     UPATH=UPATH+ULAY(5,ILAY)
                  END DO

C     Calculate the effective pressure of the path
                  PEFF=0.0
                  DO ILAY=IDWN+1,IUP
                     PEFF=PEFF+PMID(ILAY)*ULAY(5,ILAY)
                  END DO
                  PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                  TEFF=0.0
                  DO ILAY=IDWN+1,IUP
                     TEFF=TEFF+TMID(ILAY)*ULAY(5,ILAY)
                  END DO
                  TEFF=TEFF/UPATH

                  CALL GASSEARCH(5,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                 CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                 CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                 CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                  TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

               END DO
            END DO

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1
                  FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                  FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
               END DO
            END DO

            IF (GAS(1).EQ.1) THEN
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            ELSE
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)-PLANCK(IBND,TSURF)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            END IF

            GOTO 1975

         END IF

C-----------------------
C     BAND 5: 900-980 cm-1
C-----------------------

         IF (IBND.EQ.5) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

                                ! CO2 absorption

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                  UPATH=0.0
                  DO ILAY=IDWN+1,IUP
                     UPATH=UPATH+ULAY(6,ILAY)
                  END DO

C     Calculate the effective pressure of the path
                  PEFF=0.0
                  DO ILAY=IDWN+1,IUP
                     PEFF=PEFF+PMID(ILAY)*ULAY(6,ILAY)
                  END DO
                  PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                  TEFF=0.0
                  DO ILAY=IDWN+1,IUP
                     TEFF=TEFF+TMID(ILAY)*ULAY(6,ILAY)
                  END DO
                  TEFF=TEFF/UPATH

                  CALL GASSEARCH(6,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                 CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                 CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                 CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                  TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

               END DO
            END DO

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1
                  FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                  FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
               END DO
            END DO

            IF (GAS(1).EQ.1) THEN
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            ELSE
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)-PLANCK(IBND,TSURF)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            END IF

            GOTO 1975

         END IF

C-----------------------
C     BAND 6: 650-770 cm-1
C-----------------------

         IF (IBND.EQ.6) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

            IF (GAS(2).NE.1) THEN
                                ! O3 absorption only
               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(7,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(7,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(7,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(7,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1
                     FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                     FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
                  END DO
               END DO

               IF (GAS(1).EQ.1) THEN
                  DO ILAY=0,NLEV
                     FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                     FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                     FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
                  END DO
               ELSE
                  DO ILAY=0,NLEV
                     FUP(ILAY)=FUP(ILAY)-PLANCK(IBND,TSURF)+FGUP(ILAY)
                     FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                     FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
                  END DO
               END IF

            ELSE
                                ! CO2 + O3 absorption
                                ! (a) CO2

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(2,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(2,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(2,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(2,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1
                     FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                     FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
                  END DO
               END DO

               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)-FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)-FGDWN(ILAY)
               END DO

                                ! (b) O3

C     Boundary Conditions
               FGUP(0)=PLANCK(IBND,TSURF)
               FGDWN(NLEV)=0.0

C     Initialise the irradiances
               DO ILAY=1,NLEV
                  FGUP(ILAY)=PF(ILAY)
                  FGDWN(ILAY-1)=PF(ILAY)
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

                     TRIN(IDWN,IUP)=TROUT(IDWN,IUP)

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(7,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(7,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(7,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(7,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1
                     FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                     FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
                  END DO
               END DO

               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO

            END IF

            GOTO 1975

         END IF

C-----------------------
C     BAND 7: 550-630 cm-1
C-----------------------

         IF (IBND.EQ.7) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

            IF (GAS(2).NE.1) THEN
                                ! N2O absorption only
               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(5,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(5,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(5,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(5,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1
                     FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                     FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
                  END DO
               END DO

               IF (GAS(1).EQ.1) THEN
                  DO ILAY=0,NLEV
                     FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                     FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                     FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
                  END DO
               ELSE
                  DO ILAY=0,NLEV
                     FUP(ILAY)=FUP(ILAY)-PLANCK(IBND,TSURF)+FGUP(ILAY)
                     FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                     FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
                  END DO
               END IF

            ELSE
                                ! CO2 + N2O absorption
                                ! (a) CO2

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(2,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(2,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(2,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(2,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1
                     FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                     FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
                  END DO
               END DO

               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)-FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)-FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO

                                ! (b) N2O

C     Boundary Conditions
               FGUP(0)=PLANCK(IBND,TSURF)
               FGDWN(NLEV)=0.0

C     Initialise the irradiances
               DO ILAY=1,NLEV
                  FGUP(ILAY)=PF(ILAY)
                  FGDWN(ILAY-1)=PF(ILAY)
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1

                     TRIN(IDWN,IUP)=TROUT(IDWN,IUP)

C     Calculate absorber amount in the path between IDWN and IUP
                     UPATH=0.0
                     DO ILAY=IDWN+1,IUP
                        UPATH=UPATH+ULAY(5,ILAY)
                     END DO

C     Calculate the effective pressure of the path
                     PEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        PEFF=PEFF+PMID(ILAY)*ULAY(5,ILAY)
                     END DO
                     PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                     TEFF=0.0
                     DO ILAY=IDWN+1,IUP
                        TEFF=TEFF+TMID(ILAY)*ULAY(5,ILAY)
                     END DO
                     TEFF=TEFF/UPATH

                     CALL GASSEARCH(5,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                    CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                    CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                    CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                     TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

                  END DO
               END DO

               DO IUP=1,NLEV
                  DO IDWN=0,IUP-1
                     FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                     FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
                  END DO
               END DO

               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO

            END IF

            GOTO 1975

         END IF
C-------------------------
C     BAND 8: 1100-1200 cm-1
C-------------------------

         IF (IBND.EQ.8) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

                                ! N2O absorption

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                  UPATH=0.0
                  DO ILAY=IDWN+1,IUP
                     UPATH=UPATH+ULAY(5,ILAY)
                  END DO

C     Calculate the effective pressure of the path
                  PEFF=0.0
                  DO ILAY=IDWN+1,IUP
                     PEFF=PEFF+PMID(ILAY)*ULAY(5,ILAY)
                  END DO
                  PEFF=PEFF/UPATH

C     Calculate the effective temperature of the path
                  TEFF=0.0
                  DO ILAY=IDWN+1,IUP
                     TEFF=TEFF+TMID(ILAY)*ULAY(5,ILAY)
                  END DO
                  TEFF=TEFF/UPATH

                  CALL GASSEARCH(5,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,
     $                 CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,
     $                 CN03,DN03,AC02,BC02,CC02,DC02,AC06,BC06,
     $                 CC06,DC06,AC07,BC07,CC07,DC07,TRANS)
                  TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

               END DO
            END DO

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1
                  FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                  FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
               END DO
            END DO

            IF (GAS(1).EQ.1) THEN
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            ELSE
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)-PLANCK(IBND,TSURF)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            END IF

            GOTO 1975

         END IF

C-----------------------
C     BAND 9: 820-900 cm-1
C-----------------------

         IF (IBND.EQ.9) THEN

C     Boundary Conditions
            FGUP(0)=PLANCK(IBND,TSURF)
            FGDWN(NLEV)=0.0

C     Initialise the irradiances
            DO ILAY=1,NLEV
               FGUP(ILAY)=PF(ILAY)
               FGDWN(ILAY-1)=PF(ILAY)
            END DO

                                ! Halocarbon absorption

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1

C     Calculate absorber amount in the path between IDWN and IUP
                  UPATH=0.0
                  DO ILAY=IDWN+1,IUP
                     UPATH=UPATH+ULAY(8,ILAY)
                  END DO

                  TRANS=HALTRAN(IHAL,UPATH)
                  TROUT(IDWN,IUP)=TRANS*TRIN(IDWN,IUP)

               END DO
            END DO

            DO IUP=1,NLEV
               DO IDWN=0,IUP-1
                  FGUP(IUP)=FGUP(IUP)+TROUT(IDWN,IUP)*DPF(IDWN)
                  FGDWN(IDWN)=FGDWN(IDWN)-TROUT(IDWN,IUP)*DPF(IUP)
               END DO
            END DO

            IF (GAS(1).EQ.1) THEN
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            ELSE
               DO ILAY=0,NLEV
                  FUP(ILAY)=FUP(ILAY)-PLANCK(IBND,TSURF)+FGUP(ILAY)
                  FDWN(ILAY)=FDWN(ILAY)+FGDWN(ILAY)
                  FNET(ILAY)=FUP(ILAY)-FDWN(ILAY)
               END DO
            END IF

         END IF

 1975    CONTINUE

      END DO


      END
