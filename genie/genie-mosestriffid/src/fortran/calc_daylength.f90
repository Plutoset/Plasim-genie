!#######################################################################################
! Subroutine to calculate the sunlit fraction of day of each gridpoint for use in the 
! photosynthesis/conductance calculations.
! Input: koverall
! Output: dayfrac
! 9/09/04 PPH
!#######################################################################################
SUBROUTINE calc_daylength

  USE phys_const
  USE land_const, only : land_pts
  USE land_var, only : latitudes, init_doy_angle, dt_doy_angle, nstepyear, dayfrac
  IMPLICIT NONE

  !########## Work variables ####################
  INTEGER :: n                          ! Loop counter
  REAL :: doy_angle                     ! Time of year angle (radians)
  REAL :: tv,c1,c2,c3                   ! Work variables for calculating elliptical angle of this timestep 
  REAL :: ve2doy_angle                  ! Angle between this timestep from Vernal Equinox (radians)
  REAL :: sindec, cosdec                ! Sine and cosine of declination angle at this timestep (-)
  REAL,DIMENSION(land_pts) :: tlat_tdec ! Product of tan of land point latitudes and tan of declination angle (-)

  ALLOCATE(dayfrac(land_pts,nstepyear))

  !##########################################
  ! Zero work and output arrays
  !##########################################
  doy_angle    = 0.0
  ve2doy_angle = 0.0
  sindec       = 0.0
  cosdec       = 0.0
  tlat_tdec(:) = 0.0
  dayfrac(:,:) = 0.0

  !##########################################
  ! Calc work orbital parameters
  !##########################################
  tv = con_orb_ecc*con_orb_ecc
  c1 = con_orb_ecc*(2.0 + 0.25*tv)
  c2 = 1.25 * tv
  c3 = (13.0/12.0)*tv*con_orb_ecc

  doy_angle = init_doy_angle
  DO n = 1,nstepyear
    !##########################################
    ! Calculate sine and cosine of declination
    ! angle as a function of angular time of year
    !##########################################
    ve2doy_angle = doy_angle + c1*SIN(doy_angle) + c2*SIN(2.0*doy_angle) + c3*SIN(3.0*doy_angle)
    sindec = SIN(con_orb_obl)*SIN(ve2doy_angle-con_orb_p2ve)
    cosdec = SQRT(1.0-(sindec*sindec))

    !##########################################
    ! Calculate half daylength and fraction of 
    ! day sun is above horizon (=H/pi)
    !##########################################
    tlat_tdec(:) = (SIN(latitudes(:))*sindec) &
                  /(COS(latitudes(:))*cosdec)
    WHERE(tlat_tdec<-1.0) tlat_tdec = -1.0
    WHERE(tlat_tdec> 1.0) tlat_tdec =  1.0
    dayfrac(:,n) = ACOS(-1.0*tlat_tdec(:))*con_rpi

    !##########################################
    ! Update angular time-of-year
    !##########################################
    doy_angle = doy_angle + dt_doy_angle

  ENDDO

!bowie  dayfrac(:,:) = 1.0
  RETURN

END SUBROUTINE calc_daylength
