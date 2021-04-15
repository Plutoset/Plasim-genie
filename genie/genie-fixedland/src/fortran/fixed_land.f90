MODULE fixed_land
!$Id: fixed_land.f90 2497 2005-07-14 14:26:35Z cvs-gw $
!#######################################################################################
! This module provides a "constant" land scheme, i.e. one that does not respond to 
! the forcing of other models, but does provide the outputs from land required by 
! other models. There will be different configurations of fixedland:
!   C1 "Do Nothing Land"   : T*=T1, albedo=1, LE=H=0, tau=?, runoff=precip, F[CO2]=0
!   C2 "Do Something Land" : T*, albedo, LE, H, tau, runoff, fx[CO2] all constant and uniform
!   C3 "Seasonal Land"     : T*, albedo, LE, H, tau, runoff, fx[CO2] geographically varying 
!                            monthly means from netCDF file
!
! INPUTS : stepnumber, SWin, LWin, [u1,v1], T1, q1, p*, P_lscale, P_conv, z1, [CO2]
! OUTPUTS: T*, LE, H, albedo, taux, tauy, runoff, fx[CO2]
!
! This f90 module provides subroutines initialise_fixedland(), genie_fixedland(), end_fixedland()
!
! PPH 1/6/04
!#######################################################################################

  INTEGER :: nlon           ! Number of atmosphere points on longitude axis
  INTEGER :: nlat           ! Number of atmosphere points on latitude axis
  INTEGER :: land_pts       ! Total number of land points on atmos grid

  INTEGER,DIMENSION(:,:),ALLOCATABLE :: ij_land

CONTAINS
!#########################################################
! 1. Work out number of land points on grid and where they 
!     are [DONE]
! 2. Read in fixed vegetation/tile variables [TODO]
! 3. Read in fixed fluxes [TODO]
!#########################################################
  SUBROUTINE initialise_fixedland(nlon_in,nlat_in,landsea_mask)
    IMPLICIT NONE

    INTEGER,INTENT(in) :: nlon_in, nlat_in
    INTEGER,INTENT(in),DIMENSION(nlon_in,nlat_in) :: landsea_mask
    INTEGER :: i,j,l

    !Copy grid lat/lon sizes to local module variables
    nlon = nlon_in
    nlat = nlat_in

    !Count up number of land points on grid
    land_pts = 0
    DO i=1,nlon
      DO j=1,nlat
        IF(landsea_mask(i,j).eq.1) land_pts=land_pts+1
      ENDDO     
    ENDDO

    !Store the (i,j) locations of land points
    ALLOCATE(ij_land(land_pts,2))
    l=0
    DO i=1,nlon
      DO j=1,nlat
        IF(landsea_mask(i,j).eq.1) THEN
          l=l+1
          ij_land(l,1) = i
          ij_land(l,2) = j
        ENDIF
      ENDDO     
    ENDDO

    !Read in fixed vegetation file
    !Read in fixed land output file

    RETURN
  END SUBROUTINE initialise_fixedland

!#########################################################
! At the mo this mimics, somewhat, the old c-goldstein
! land.  No surface hydrology - precip is runoff instantly
! to the ocean - and no surface latent or sensible heat
! fluxes.
!#########################################################
  SUBROUTINE genie_fixedland(lowestlu,lowestlv &
       ,precip_atm,lowestlt,lowestlh & 
       ,tstar_gb_land,albedo_land,evap_land,fx_le_land,fx_sen_land  &
       ,fx_momx_land,fx_momy_land,runoff_land,tice_land,albice_land)! &
    !,fx_co2_land)

    IMPLICIT NONE
!********************************
! DECLARE INPUT VARIABLES
!********************************
    REAL,INTENT(in) :: lowestlu(nlon,nlat)
    REAL,INTENT(in) :: lowestlv(nlon,nlat)
    REAL,INTENT(in) :: precip_atm(nlon,nlat)
    REAL,INTENT(in) :: lowestlt(nlon,nlat)
    REAL,INTENT(in) :: lowestlh(nlon,nlat)

!********************************
! DECLARE OUTPUT VARIABLES
!********************************
    REAL,INTENT(out) :: tstar_gb_land(nlon,nlat)
    REAL,INTENT(out) :: albedo_land(nlon,nlat)
    REAL,INTENT(out) :: evap_land(nlon,nlat)
    REAL,INTENT(out) :: fx_le_land(nlon,nlat)
    REAL,INTENT(out) :: fx_sen_land(nlon,nlat)
    REAL,INTENT(out) :: fx_momx_land(nlon,nlat)
    REAL,INTENT(out) :: fx_momy_land(nlon,nlat)
    REAL,INTENT(out) :: runoff_land(nlon,nlat)
    REAL,INTENT(out) :: tice_land(nlon,nlat)
    REAL,INTENT(out) :: albice_land(nlon,nlat)
!    REAL,INTENT(out) :: fx_co2_land(nlon,nlat) !Net land-to-atm CO2 flux (mol/m2/s)

!********************************
! DECLARE WORK VARIABLES
!********************************
    INTEGER :: i,j,l
    REAL,PARAMETER :: karman = 0.41    !von Karman's constant
    REAL,PARAMETER :: z0     = 0.01    !Land-surface roughness length (m)
    REAL,PARAMETER :: rhoa   = 1.2     !Air density (kg/m3)
    REAL           :: cd               !Stress coefficient
    REAL           :: wind             !Scalar wind speed (m/s)

    DO l=1,land_pts
      i=ij_land(l,1)
      j=ij_land(l,2)

      cd = karman*karman/(ALOG(lowestlh(i,j)/z0))
      wind = SQRT(lowestlu(i,j)**2 + lowestlv(i,j)**2)

      tstar_gb_land(i,j) = lowestlt(i,j)
      albedo_land(i,j)   = 0.2
      evap_land(i,j)     = 0.0
      fx_le_land(i,j)    = 0.0
      fx_sen_land(i,j)   = 0.0
      fx_momx_land(i,j)  = cd*rhoa*(wind+3.0)*lowestlu(i,j)
      fx_momy_land(i,j)  = cd*rhoa*(wind+3.0)*lowestlv(i,j)
      runoff_land(i,j)   = precip_atm(i,j)
      tice_land(i,j)     = lowestlt(i,j)
      albice_land(i,j)   = 0.7
!      fx_co2_land(i,j)   = 0.0
    ENDDO

    RETURN
  END SUBROUTINE genie_fixedland

!#########################################################
  SUBROUTINE end_fixedland
    IMPLICIT NONE

    print*,'FIXEDLAND %% Ending fixed land.'

    RETURN

  END SUBROUTINE end_fixedland

END MODULE fixed_land
