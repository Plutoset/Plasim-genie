!#######################################################################################
! Subroutine to keep track of the mass and energy I/O of genie-land.
! Land reservoirs of water, carbon and energy are checked.
! PPH 11/09/04
!#######################################################################################
MODULE land_accounting

  USE phys_const, only : con_pi180, con_aearth, con_latfus
  USE land_const, only : nlat,nlon,land_pts,hcap_veg,dz_soil
  USE land_var
  IMPLICIT NONE

  REAL,PRIVATE :: lnd_tot_h2o0     ! Initial global total water on land (soil moisture + snow; kgH2O)
  REAL,PRIVATE :: lnd_tot_c0       ! Initial global total carbon on land (soil + vegetation; kgC)
  REAL,PRIVATE :: lnd_tot_e0       ! Initial global total heat/energy content of land (soil + vegetation; J)

  REAL,PRIVATE :: lnd_tot_h2o_beg  ! Global total water on land (soil moisture + snow; kgH2O)
  REAL,PRIVATE :: lnd_tot_c_beg    ! Global total carbon on land (soil + vegetation; kgC)
  REAL,PRIVATE :: lnd_tot_e_beg    ! Global total heat/energy content of land (soil + vegetation; J)

  REAL(kr_g),PRIVATE :: lnd_flx_h2o      ! Acccumulated global water flux to land (kg H2O)
  REAL(kr_g),PRIVATE :: lnd_flx_c        ! Acccumulated global carbon flux to land (kg C)
  REAL(kr_g),PRIVATE :: lnd_flx_e        ! Acccumulated global energy flux to land (J)

CONTAINS
  !###########################################################################################
  ! Routine to calculate initial water, carbon and heat
  ! reservoirs on land.
  !###########################################################################################
  SUBROUTINE init_lnd_account(aboxedge1_lon_atm,aboxedge1_lat_atm)
    IMPLICIT NONE
    REAL,INTENT(in),DIMENSION(nlon+1) :: aboxedge1_lon_atm   ! Longitude gridbox edges on atmos grid (degrees)
    REAL,INTENT(in),DIMENSION(nlat+1) :: aboxedge1_lat_atm   ! Latitude gridbox edges on atmos grid (degrees)
    REAL :: weightcheck
    INTEGER :: i,j,l,ti,tj

    CALL reset_lnd_account

    !######################################
    ! Calculate grid box area weightings
    !######################################
    lnd_weights(:)    = 0.0
    DO l=1,land_pts
      ti = ij_land(l,1)
      tj = ij_land(l,2)
      lnd_weights(l) = (SIN(aboxedge1_lat_atm(tj)*con_pi180) - SIN(aboxedge1_lat_atm(tj+1)*con_pi180)) &
                      *(aboxedge1_lon_atm(ti+1)-aboxedge1_lon_atm(ti))/360.0/2.0
      lnd_weights(l) = ABS(lnd_weights(l))
    ENDDO

    lnd_weights2(:,:) = 0.0
    DO j=1,nlat
      DO i=1,nlon
        lnd_weights2(i,j) = (SIN(aboxedge1_lat_atm(j)*con_pi180) - SIN(aboxedge1_lat_atm(j+1)*con_pi180)) &
                      *(aboxedge1_lon_atm(i+1)-aboxedge1_lon_atm(i))/360.0/2.0
        lnd_weights2(i,j) = ABS(lnd_weights2(i,j))
      ENDDO
    ENDDO

    weightcheck = 0.0
    DO j=1,nlat
      DO i=1,nlon
       weightcheck = weightcheck & 
                      + ABS((SIN(aboxedge1_lat_atm(j)*con_pi180) - SIN(aboxedge1_lat_atm(j+1)*con_pi180)) &
                       *(aboxedge1_lon_atm(i+1)-aboxedge1_lon_atm(i))/360.0/2.0)
      ENDDO
    ENDDO
    PRINT*,'LAND %% Check for weightings from land = ',weightcheck,SUM(lnd_weights(:))

    !######################################
    ! Calculate initial reservoir stores
    !######################################
    CALL now_ini_lnd_account

    lnd_tot_h2o_beg = lnd_tot_h2o0
    lnd_tot_c_beg   = lnd_tot_c0
    lnd_tot_e_beg   = lnd_tot_e0

    RETURN
  END SUBROUTINE init_lnd_account

  !###########################################################################################
  ! Add single timestep increments to accumulated fluxes
  !###########################################################################################
  SUBROUTINE update_lnd_account(fx_rain,fx_evap,fx_runoff    &
                              ,fx_swn,fx_lwn,fx_le2,fx_sen2  &
                              ,fx_co2                        )
    IMPLICIT NONE
    REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: fx_rain, fx_evap, fx_runoff     ! water fluxes (kg/m2/s)
    REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: fx_swn, fx_lwn, fx_le2, fx_sen2 ! Energy fluxes (W/m2)
    REAL(kr_g),INTENT(in),DIMENSION(nlon,nlat) :: fx_co2                          ! CO2 fluxes (kg C/m2/s)
    INTEGER :: i,j,l

    DO l=1,land_pts
      i=ij_land(l,1)
      j=ij_land(l,2)
      !##### Water #####
      lnd_flx_h2o = lnd_flx_h2o + (fx_rain(i,j))*lnd_weights2(i,j)*REAL(dt_land*con_aearth,KIND=kr_g)
      lnd_flx_h2o = lnd_flx_h2o - (-fx_evap(i,j))*lnd_weights2(i,j)*REAL(dt_land*con_aearth,KIND=kr_g)

      !##### Carbon #####
      lnd_flx_c   = lnd_flx_c + fx_co2(i,j)*lnd_weights2(i,j)*REAL(dt_land*con_aearth,KIND=kr_g)

      !##### Energy #####
      ! Note that phase change energy of rainfall/snowfall conversion 
      ! has been added to fx_sen2 before being passed to update_lnd_account
      lnd_flx_e   = lnd_flx_e + (fx_swn(i,j)+fx_lwn(i,j))*lnd_weights2(i,j)*REAL(dt_land*con_aearth,KIND=kr_g)
      lnd_flx_e   = lnd_flx_e - (-fx_le2(i,j)-fx_sen2(i,j))*lnd_weights2(i,j)*REAL(dt_land*con_aearth,KIND=kr_g)
    ENDDO

    ! Need to add runoff to water sum outside of loop as 
    ! values are only non-zero on non-land points following routeing
    lnd_flx_h2o = lnd_flx_h2o - SUM(fx_runoff(:,:)*lnd_weights2(:,:))*REAL(dt_land*con_aearth,KIND=kr_g)

    RETURN
  END SUBROUTINE update_lnd_account

  !###########################################################################################
  ! Compare accumulated fluxes with changes in reservoir stores through the accounting period
  !###########################################################################################
  SUBROUTINE check_lnd_account
    IMPLICIT NONE
    REAL :: lnd_tot_h2o_end, lnd_tot_c_end, lnd_tot_e_end
    REAL(kr_g) :: err_h2o, err_c, err_e

    !######################################
    ! Calculate current reservoir stores
    !######################################
    lnd_tot_h2o_end = get_lnd_now_w()
    lnd_tot_e_end   = get_lnd_now_e()
    lnd_tot_c_end   = get_lnd_now_c()

    !######################################
    ! Compare accumulated fluxes with change 
    ! in reservoirs
    !######################################
    err_h2o = REAL(lnd_tot_h2o_beg - lnd_tot_h2o_end, KIND=kr_g) + lnd_flx_h2o
    err_c   = REAL(lnd_tot_c_beg   - lnd_tot_c_end, KIND=kr_g)   + lnd_flx_c
    err_e   = REAL(lnd_tot_e_beg   - lnd_tot_e_end, KIND=kr_g)   + lnd_flx_e

    PRINT 999,'LAND %% Error in water flux account  =',err_h2o,' kg H2O' &
         ,err_h2o/(lnd_tot_h2o_beg+TINY(lnd_tot_h2o_beg))
    PRINT 999,'LAND %% Error in carbon flux account =',err_c,  ' kg C  ' &
         ,err_c/(lnd_tot_c_beg+TINY(lnd_tot_c_beg))
    PRINT 999,'LAND %% Error in energy flux account =',err_e,  ' J     ' &
         ,err_e/(lnd_tot_e_beg+TINY(lnd_tot_e_beg))

!998 FORMAT(A38,1X,E14.7,1X,A7)
999 FORMAT(A38,1X,E14.7,A7,1X,'(',E14.7,')')
    !######################################
    ! Setup ready for next checking period
    !######################################
    lnd_tot_h2o_beg = lnd_tot_h2o_end
    lnd_tot_c_beg   = lnd_tot_c_end
    lnd_tot_e_beg   = lnd_tot_e_end
    CALL reset_lnd_account

    RETURN
  END SUBROUTINE check_lnd_account

  !###########################################################################################
  ! Reset initial values of water, energy and carbon reservoirs
  !###########################################################################################
  SUBROUTINE now_ini_lnd_account
    IMPLICIT NONE
    lnd_tot_h2o0 = get_lnd_now_w()
    lnd_tot_c0   = get_lnd_now_c()
    lnd_tot_e0   = get_lnd_now_e()

    PRINT*,'LAND %% Setting new initial reservoir contents.'
    PRINT*,'LAND %% Initial global land water content  =',lnd_tot_h2o0,' kg H2O'
    PRINT*,'LAND %% Initial global land carbon content =',lnd_tot_c0,' kg C'
    PRINT*,'LAND %% Initial global land heat content   =',lnd_tot_e0,' J'
    ! Note : Estimated current terrestrial C reservoir = 2.02E15 kgC 
    !  (Schlesinger WH, 1997, "Biogeochemistry: an analysis of global change")

    CALL reset_lnd_account
    RETURN
  END SUBROUTINE now_ini_lnd_account

  !###########################################################################################
  ! Set accumulated fluxes to zero
  !###########################################################################################
  SUBROUTINE reset_lnd_account
    IMPLICIT NONE
    lnd_flx_h2o = 0.0
    lnd_flx_c   = 0.0
    lnd_flx_e   = 0.0
    RETURN
  END SUBROUTINE reset_lnd_account

  !###########################################################################################
  ! Subroutine to return the change in global land water, energy and carbon reservoirs since
  ! the start of the run.  Output values are GENIE precision reals.
  !###########################################################################################
  SUBROUTINE get_inc_lnd_account(inc_w_out, inc_e_out, inc_c_out)
    IMPLICIT NONE
    REAL(kr_g),INTENT(out),OPTIONAL :: inc_w_out, inc_e_out, inc_c_out

    IF(PRESENT(inc_w_out)) inc_w_out = REAL(get_lnd_now_w() - lnd_tot_h2o0, KIND=kr_g)
    IF(PRESENT(inc_e_out)) inc_e_out = REAL(get_lnd_now_e() - lnd_tot_e0, KIND=kr_g)
    IF(PRESENT(inc_c_out)) inc_c_out = REAL(get_lnd_now_c() - lnd_tot_c0, KIND=kr_g)

    RETURN
  END SUBROUTINE get_inc_lnd_account

  !###########################################################################################
  ! Functions to return global total land water, energy or carbon reservoirs
  !###########################################################################################
  REAL FUNCTION get_lnd_now_w() RESULT(total_w)
    IMPLICIT NONE
    ! total_w = Global total water on land (kg H2O)
    total_w = 0.0
    total_w = SUM(msoil(:)*lnd_weights(:))*con_aearth
    total_w = total_w + SUM(lying_snow(:)*lnd_weights(:))*con_aearth
  END FUNCTION get_lnd_now_w

  REAL FUNCTION get_lnd_now_e() RESULT(total_e)
    IMPLICIT NONE
    ! total_e = Global total energy on land (J)
    total_e = 0.0
    total_e = SUM(tstar_gb(:)*lnd_weights(:))*hcap_veg*con_aearth
    total_e = total_e + SUM(tsub1(:)*lnd_weights(:))*hcap_nvg(1)*dz_soil*con_aearth
    total_e = total_e - SUM(lying_snow(:)*lnd_weights(:))*con_latfus*con_aearth
  END FUNCTION get_lnd_now_e

  REAL FUNCTION get_lnd_now_c() RESULT(total_c)
    IMPLICIT NONE
    ! total_c = Global total carbon on land (kg C)
    total_c = 0.0
    total_c = SUM(cv(:)*lnd_weights(:))*con_aearth
    total_c = total_c + SUM(cs(:)*lnd_weights(:))*con_aearth
  END FUNCTION get_lnd_now_c

END MODULE land_accounting



!**********************************************************************
! This subroutine provides a wrapper to the land_accounting module
! subroutine now_ini_lnd_account to avoid having to USE a land module 
! in genie.F
! This call is to reset the initial t=0 land water, energy and carbon 
! reservoirs so that the GENIE-level conservation checks are correct.
!**********************************************************************
SUBROUTINE reini_lnd_account
  USE land_accounting
  IMPLICIT NONE
  CALL now_ini_lnd_account
  RETURN
END SUBROUTINE reini_lnd_account
