!#############################################################################
! Subtoutine to replace the EMBM fluxes over land with the genie-land fluxes
!#############################################################################
SUBROUTINE land_embm_combine(surf_sensible_atm,                   &
                   evap_atm,runoff_ocn,                           &
                   land_sensibleinst_atm,                         &
                   land_evap_atm,land_runoff_atm                  )


  USE land_const, only : land_pts,nlon,nlat,kr_g
  USE land_var, only : ij_land

  IMPLICIT NONE

!  REAL(kr_g),INTENT(inout),DIMENSION(nlon,nlat) :: surf_latent_atm
  REAL(kr_g),INTENT(inout),DIMENSION(nlon,nlat) :: surf_sensible_atm
  REAL(kr_g),INTENT(inout),DIMENSION(nlon,nlat) :: evap_atm
  REAL(kr_g),INTENT(inout),DIMENSION(nlon,nlat) :: runoff_ocn

!  REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: land_latentinst_atm
  REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: land_sensibleinst_atm
  REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: land_evap_atm
  REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: land_runoff_atm
!  REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: land_tstarinst_atm
!  REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: land_salb_atm 
!  REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: ocean_stressx2_ocn,ocean_stressy2_ocn

  INTEGER :: l,i,j
  REAL(kr_g),PARAMETER :: sigma = 5.67E-8

  !#########################################
  ! Override flux arrays
  !#########################################
  DO l=1,land_pts
    i=ij_land(l,1)
    j=ij_land(l,2)
!bowie    surf_latent_atm(i,j)   = -land_latentinst_atm(i,j)
    surf_sensible_atm(i,j) = -land_sensibleinst_atm(i,j)
    evap_atm(i,j)          = -land_evap_atm(i,j)
  ENDDO

  !#########################################
  ! Override the runoff from EMBM surflux with
  ! that from genie-land
  !#########################################
  runoff_ocn(:,:) = 0.0
  runoff_ocn(:,:) = land_runoff_atm(:,:)

  RETURN

END SUBROUTINE land_embm_combine
