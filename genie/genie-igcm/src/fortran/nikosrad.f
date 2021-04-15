      SUBROUTINE NIKOSRAD(PR,T,H2O,O3,alat,HTLW,HTSW,DOY,CF,IC,
     $     RFLUXES,SWALB,ALON,ilandmask)

      IMPLICIT NONE

C     
C     No aerosols
c     Morcrette SW combined with Nikos LW scheme
C     Piers 27-1-98
C     
C     P  Pressure on Full levels(Pa) level 1 is ground
C     T Temperature on Full levels(K) level 1 is ground
C     H2O water vapour mmr on full levels level 1 is ground
C     O3 ozone mmr on full levels level 1 is ground
C     DOY Julian Day wanted (real)
C     NLEV number of levels
C     ALAT Latitude in degrees
C     ALON longitude in degrees
C     HTLW Output: lomgwave Heating Rate full levels etc (K/day)
C     HTSW Output: shortwave heating rate (K/DAY)
C     RFLUXES  Array to hold fluxes at top and bottom of atmosphere
C     1st index - flux 1=SW, 2=LW
C     2nd index - Direction 1=DN, 2=UP
C     3rd index - Where 1=TOP, 2=SURFACE
C     SWALB SW albedo
C     CF   CF(*,1) CLOUD fractions and CF(*,2) CLOUD LWP(g/m^2)
C     Index 1-deep convective, 2-high,3-mid,4-low
C     IC   IC(*,1) cloud start level, IC(*,2) cloud stop level
C     level 1 is ground
C     same index as cloud

C     -----------
C     Driving program for the extended atmospheric radiation scheme of
C     Lacis and Hansen and Nikos Christidis.
C     
C     
#include "parray.cmn"
      include 'radlw.cmn'
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'igcm_radn.cmn'
      include 'varalb.cmn'

      INTEGER NRLEV,NLEV,LHT,ILAY,ICL,ICLIC,LEV
      REAL    SVP,ALWP,SOLC,YCLOCK,AMU0,RDAYL,CDISSEM
      REAL    ZSCT,PSOL,ZCARDI,GRCP

      PARAMETER (NRLEV=MXLEV+1)
      REAL PR(NRLEV),T(NRLEV),H2O(NRLEV),O3(NRLEV),alat
      real htlw(nrlev),htsw(nrlev),swalb,DOY,alon
      REAL CF(4,2)              !cloud fractions,lwps
c     clouds in order of convective,high,mid,low
c     ie convective has index of 1 with low cloud index of 4
      INTEGER IC(4,2)           ! cloud positions
      REAL RFLUXES(2,2,2)
      LOGICAL LDIUR
C     Morcrette
      REAL FLS(nrlev+1)
      REAL SWDP(NRLEV),pclfr(nrlev),pqlwp(nrlev)
C     Nikos
      INTEGER CKD, IFIRST
      DATA IFIRST/1/
      REAL PRES(0:MXLEV),TEMP(0:MXLEV),MMR(MXGAS,0:MXLEV)
      INTEGER CL(0:MXCL)
      REAL ACL(0:MXCL),LWCCL(MXCL),BASCL(0:MXCL),TOPCL(0:MXCL)
C     Pressure at flux levels
      REAL PFLUX(0:MXLEV)

      real alb_spec
      real swalb_mod
      integer ilandmask

      integer ios

C------------------
C     Output Variables
C------------------

      REAL FUP(0:MXLEV),FDWN(0:MXLEV),FNET(0:MXLEV)
      REAL PDP(MXLEV)
C     long wave radiation namelist
      NAMELIST/INRADLW/LLBLM,GAS,VMRHALO
      LLBLM=.TRUE.
C     
C     Setup for LW scheme
      NLEV=NRLEV-1
      CKD=1                     ! water vapour continium
      IF (CKD.EQ.1.AND.IFIRST.EQ.1)THEN
         WRITE (6,*)'Water Vapour Continuum On'
      ELSEIF(CKD.EQ.0.AND.IFIRST.EQ.1)THEN
         WRITE (6,*)'Water Vapour Continuum Off'
      ENDIF
      IF (NL.NE.MXLEV)THEN
         WRITE(6,*)'JOB ABORTED! NL= ',NL,' MXLEV= ',MXLEV
         CALL ABORT
      ENDIF
C
C     set up default values.
C
      IF (IFIRST.EQ.1) THEN
         IFIRST=0
         IF(LLBLM)THEN
            PRINT *,'Transmittance tables created by lblm'
         ELSE
            PRINT *,'Transmittance tables created by nbm'
         ENDIF
C     
C     absorber index   Gas   yes(1)/no (0)  vol. mixing ratio  C
C     C
C     1            H2O      1           by model           C
C     2            CO2      1           358ppmv            C
C     3            O3       1           by model           C
C     4            CH4      0           1.72ppmv           C
C     5            N2O      0           312ppbv            C
C     6          CO2(minor) 0           358ppmv            C
C     7          O3(minor)  0           by model           C
C     8          Halocarbon 0           797x10-14          C
C     
         GAS(1)=1
         GAS(2)=1
         GAS(3)=1
         GAS(4)=0
         GAS(5)=0
         GAS(6)=0
         GAS(7)=0
         GAS(8)=0
C     run time is 7% quicker with gasses 4-8 off (over 12 days)
c         VMRCO2=358.0E-6*(44.011/28.964)
c         VMRCH4=1.72E-6*(16.043/28.964)
c         VMRN2O=312.0E-9*(44.014/28.964)
         VMRHALO=7.97E-12
C     READ namelists, overwrite defaults and write them out
         read(UNIT=7,NML=INRADLW,IOSTAT=ios)
         if (ios /= 0) then
            print*,'ERROR: could not read IGCM INRADLW namelist'
            stop
         end if
         WRITE(6,INRADLW)
      ENDIF
      DO LHT=1,NRLEV
C     MMR is mass mixing ratio of gases
c     PCLD(LHT)=0.0      ! set SW cloud opotical depth
         PCLFR(LHT)=0.0         ! set SW cloud fraction
         PQLWP(LHT)=0.0         ! set SW cloud lwp
         MMR(1,LHT-1)=H2O(LHT)
         MMR(2,LHT-1)=VMRCO2
         MMR(3,LHT-1)=O3(LHT)
         MMR(4,LHT-1)=VMRCH4
         MMR(5,LHT-1)=VMRN2O
         MMR(6,LHT-1)=MMR(2,LHT-1)
         MMR(7,LHT-1)=MMR(3,LHT-1)
         MMR(8,LHT-1)=VMRHALO*(125.5/28.964)
         PRES(LHT-1)=PR(LHT)
         TEMP(LHT-1)=T(LHT)
      ENDDO
C     Calculate pressures at flux levels

      PFLUX(0)=PRES(0)/100.0
      DO ILAY=1,NLEV-1
         PFLUX(ILAY)=(PRES(ILAY)+PRES(ILAY+1))/200.0
      END DO
      PFLUX(NLEV)=PRES(NLEV)/200.0
      PDP(1)=0.5*(2*PRES(0)-PRES(1)-PRES(2))
      DO ILAY=2,NLEV-1
         PDP(ILAY)=0.5*(PRES(ILAY-1)-PRES(ILAY+1))
      ENDDO
      PDP(NLEV)=0.5*PRES(NLEV-1)

C     clouds
      DO ICL=1,3                !low=1,3=high
         CL(ICL)=0
         ICLIC=5-ICL            !for ic 4=low,2=high,1=convective

         IF (CF(ICLIC,1).GT.0.0) THEN
C     Cloud present
            CL(ICL)=1
C     single level cloud
            LEV=IC(ICLIC,1)
            BASCL(ICL)=PR(IC(ICLIC,1))
            TOPCL(ICL)=PR(IC(ICLIC,1))
            ACL(ICL)=CF(ICLIC,1)
C     cloud LWP is 1% super saturation in g/m^2
            CF(ICLIC,2)=supersatlarg*SVP(T(LEV))/PR(LEV)*
     $           622*PDP(LEV-1)/GA
            LWCCL(ICL)=CF(ICLIC,2)/1000.0 ! g- Kg convert
c     sw cloud optical depth
            PCLFR(IC(ICLIC,1))=CF(ICLIC,1)
            PQLWP(IC(ICLIC,1))=CF(ICLIC,2)
         ENDIF
      ENDDO
c     convective cloud
      CL(0)=0
      IF (CF(1,1).gt.0.0) THEN
         CL(0)=1
         ACL(0)=CF(1,1)
         LEV=IC(1,1)
C     cloud LWP is 2% super saturation in g/m^2
         CF(1,2)=supersatconv*SVP(T(LEV))/PR(LEV)*
     $        622*PDP(LEV-1)/GA
         ALWP=CF(1,2)
         BASCL(0)=PR(IC(1,1))
         TOPCL(0)=PR(IC(1,2))
c     quarter cloud except in bottom level
         DO ICL=IC(1,1)+1,IC(1,2)
            PCLFR(ICL)=cloudconvnorm*CF(1,1)
            PQLWP(ICL)=ALWP
         ENDDO
         PCLFR(IC(1,1))=cloudconvbase*CF(1,1)
         PQLWP(IC(1,1))=ALWP
      ENDIF

C     call nikos LW scheme
      CALL IRRAD(GAS,CKD,NLEV,PRES,PFLUX,TEMP,MMR,CL,ACL,LWCCL,
     $     BASCL,TOPCL,FUP,FDWN,FNET,PDP,LLBLM,GA)

C     Setup SW code
      SOLC=1376.0               ! solar constant

      LDIUR=.FALSE.             !Diurnally averaged if false
C     
      YCLOCK=3.14159            !time of day in radians
C     YCLOCK= (DOY-REAL(INT(DOY))*2.*3.14159
C     

      CALL SOLANG(LDIUR,DOY,YCLOCK,ALAT,ALON,AMU0,
     :           RDAYL,CDISSEM,alb_spec)
c
      if (lvar_albedo.and.(ilandmask.eq.0)) then
      swalb_mod=min(swalb+alb_spec,0.9)
      else
      swalb_mod=swalb
      endif
C     
c     call to L+H

      ZSCT=SOLC*CDISSEM         ! Solar const * earth-sun distance
      PSOL=ZSCT*RDAYL           ! * fractional day length

c     CALL SW scheme
      ZCARDI=MMR(2,1)
      CALL RADSW(T,H2O,O3,PCLFR,PQLWP,AMU0,
     :     ZCARDI,PSOL,SWDP,PFLUX,SWALB_mod,FLS,GA)

C     WORKS OUT FLUXES
C     SW
      RFLUXES(1,1,1)=PSOL*AMU0  ! SW down top
      RFLUXES(1,1,2)=FLS(1)/(1.0-SWALB_mod) ! SW down bottom
      RFLUXES(1,2,1)=RFLUXES(1,1,1)-FLS(NRLEV+1) ! SW up top
      RFLUXES(1,2,2)=RFLUXES(1,1,2)*SWALB_mod ! SW up bottom
C     LW
      RFLUXES(2,1,1)=FDWN(NLEV) ! LW down top
      RFLUXES(2,1,2)=FDWN(0)    ! LW down bottom
      RFLUXES(2,2,1)=FUP(NLEV)  ! LW up top
      RFLUXES(2,2,2)=FUP(0)     ! LW up bottom

C     OUTPUT

      GRCP=(GA/CPD)*24*3600     ! g/Cp *24*3600

C     Calculate heating rates

      DO LHT=2,NRLEV
         HTSW(LHT)=GRCP*(FLS(LHT+1)-FLS(LHT))/SWDP(LHT)
         HTLW(LHT)=-GRCP*(FNET(LHT-1)-FNET(LHT-2))/SWDP(LHT)
      END DO
C     
      RETURN
      END
