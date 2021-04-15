!#########################################################################
!#########################################################################
!
!#########################################################################
!#########################################################################
PROGRAM calibrate_stoma

  USE land_const, only : alpha, kpar, f0, omega
  IMPLICIT NONE

  !###################################
  !###################################
  INTEGER,PARAMETER :: n_lat = 180
  INTEGER,PARAMETER :: n_dec = 100
  INTEGER,PARAMETER :: n_hou = 48
  INTEGER,PARAMETER :: ft    = 5                 !Choice of PFT
  REAL,DIMENSION(n_lat,n_dec,n_hou) :: solar     !Sub-daily TOA solar radiation (W/m2)
  REAL,DIMENSION(n_lat,n_dec,n_hou) :: par       !Approximate surface PAR (W/m2)
  REAL,DIMENSION(n_lat,n_dec,n_hou) :: gc        !Sub-daily canopy conductance (m/s)
  REAL,DIMENSION(n_lat,n_dec)       :: gc_target !Daily mean canopy conductance using sub-daily PAR (m/s)
  REAL,DIMENSION(n_lat,n_dec)       :: dayfrac   !Fraction of 24 hours when sun is above horizon
  REAL,DIMENSION(n_lat,n_dec,1)     :: solar_mn  !Daily mean TOA solar radiation (W/m2)
  REAL,DIMENSION(n_lat,n_dec,1)     :: par_mn    !Approximate daily-mean surface PAR (W/m2)
  REAL,DIMENSION(n_lat,n_dec,1)     :: gc_mn     !Final daily-mean forced conductance (m/s)
  INTEGER :: i                                   !Loop counter

  !###################################
  ! NAG variables
  !###################################
  INTEGER,PARAMETER :: n   = 2
  INTEGER,PARAMETER :: liw = n+2
  INTEGER,PARAMETER :: lw  = n*(n-1)/2 + 12*n
  INTEGER           :: ifail,ibound
  INTEGER,DIMENSION(liw)        :: iw
  INTEGER,DIMENSION(4)          :: iuser      !Data needed by FUNCT1
  REAL,DIMENSION(n)             :: bl, bu, x  !Lower/Upper bounds and parameter values
  REAL,DIMENSION(lw)            :: w          !Workspace for NAG routine
  REAL,DIMENSION(3*n_lat,n_dec) :: user       !Data needed by FUNCT1
  REAL                          :: f          !Objective function value

  EXTERNAL FUNCT1
 
  !###################################
  ! Setup initial parameters
  !###################################
  alpha(:) = (/   0.08,  0.08,  0.08, 0.040,  0.08 /)
  f0(:)    = (/  0.875, 0.875, 0.900, 0.800, 0.900 /) 
  kpar(:)  = (/   0.50,  0.50,  0.50,  0.50,  0.50 /)
  omega(:) = (/   0.15,  0.15,  0.15,  0.17,  0.15 /)
  x(:)     = (/alpha(ft), kpar(ft)/)

  print*,'%% Set initial parameters'

  !###################################
  ! Setup parameter bounds
  !###################################
  bl(:)  = (/0.0,0.001/)
  bu(:)  = (/5.0,1.0/)
  ibound = 0
  ifail  = 0
  print*,'%% Set parameter bounds'

  !###################################
  ! Setup "target data"
  !###################################
  CALL calc_toa_solar(n_lat, n_dec, n_hou, solar, dayfrac)
  CALL calc_par(n_lat, n_dec, n_hou, solar, par)
  CALL calc_conductance(n_lat, n_dec, n_hou, ft, par, dayfrac, gc)
  gc_target(:,:) = SUM(gc(:,:,:),DIM=3)/REAL(n_hou)
  print*,'%% Setup sub-daily target data'

  OPEN(unit=88,FILE="calibrate_stoma.subdaily.out")
  WRITE(88,'(100E12.5)') (gc_target(i,:), i=1,n_lat)
  CLOSE(88)

  !###################################
  ! Setup IUSER and USER arrays
  !###################################
  CALL calc_toa_solar(n_lat, n_dec, 1, solar_mn, dayfrac)
  CALL calc_par(n_lat, n_dec, 1, solar_mn, par_mn)
  print*,'%% Setup daily-mean forcing data'

  iuser(1) = n_lat
  iuser(2) = n_dec
  iuser(3) = n_hou
  iuser(4) = ft     !PFT choice

  user(1:n_lat,1:n_dec)           = gc_target(:,:)
  user(n_lat+1:2*n_lat,1:n_dec)   = par_mn(:,:,1)
  user(2*n_lat+1:3*n_lat,1:n_dec) = dayfrac(:,:)
  print*,'%% Setup user arrays to pass to NAG routine'

!  dayfrac(:,:) = 1.0
  CALL calc_conductance(n_lat, n_dec, 1, ft, par_mn, dayfrac, gc_mn)
  OPEN(unit=88,FILE="calibrate_stoma.daily.out")
  WRITE(88,'(100E12.5)') (gc_mn(i,:,1), i=1,n_lat)
  CLOSE(88)
  STOP

  !###################################
  ! Call NAG routine
  !###################################
  print*,'%% Calling NAG routine E04JYF()'
  CALL E04JYF(n, ibound, funct1, bl, bu, x, f, iw, liw, w, lw, iuser, user, ifail)

  !###################################
  ! Tidy up and end
  !###################################
  print*
  SELECT CASE(ifail)
  CASE(1)
    print*,'%% Problem with n,ibound,iw,liw ',n,ibound,liw,lw
  CASE(2)
    print*,'%% Did 400 odd iterations, then gave up.'
  CASE(3)
    print*,'%% No mimimum found - algorithm failed'
  CASE(4)
    print*,'%% Numerical overflow'
  CASE(5,6,7,8)
    print*,'%% Found what may be a minima with certainty of ',ifail
  CASE(9)
    print*,'%% Objective function got v. big, so rethink your FUNC1.'
  CASE(10)
    print*,'%% Weird failure, try a different starting point.'
  CASE DEFAULT
    print*,'%% You know, I think it may have worked!'
  END SELECT
  print*,'%% Final objective function value = ',f
  print*,'%% Final parameter values = ',x
  print*
  print*,'---------------------------------------------'

  CALL calc_conductance(n_lat, n_dec, 1, ft, par_mn, dayfrac, gc_mn)
  OPEN(unit=88,FILE="calibrate_stoma.daily.out")
  WRITE(88,'(100E12.5)') (gc_mn(i,:,1), i=1,n_lat)
  CLOSE(88)

END PROGRAM calibrate_stoma





!#########################################################################
! Routine called by NAG routine to calculate new value of objective 
! function
!#########################################################################
SUBROUTINE funct1(N, XC, FC, IUSER, USER)

  USE land_const

  IMPLICIT NONE

  !###################################
  ! NAG variables
  !###################################
  INTEGER,INTENT(in) :: n, iuser(4)
  REAL,INTENT(in)    :: xc(n), user(3*iuser(1),iuser(2))
  REAL,INTENT(inout) :: fc

  INTEGER :: n_lat, n_dec
  REAL,DIMENSION(iuser(1),iuser(2))   :: gc_target
  REAL,DIMENSION(iuser(1),iuser(2),1) :: par_mn
  REAL,DIMENSION(iuser(1),iuser(2))   :: dayfrac
  REAL,DIMENSION(iuser(1),iuser(2),1) :: gc_mn
  REAL,DIMENSION(iuser(1),iuser(2))   :: gc_error
  REAL :: gc_rmse

  n_lat = iuser(1)
  n_dec = iuser(2)

  gc_target(:,:) = 0.0
  par_mn(:,:,:)  = 0.0

  gc_target(:,:) = user(1:n_lat,1:n_dec)
  par_mn(:,:,1)  = user(n_lat+1:2*n_lat,1:n_dec)
  dayfrac(:,:)   = user(2*n_lat+1:3*n_lat,1:n_dec)

  CALL set_new_parameters(n,xc,iuser(4))
  CALL calc_conductance(n_lat, n_dec, 1, iuser(4), par_mn, dayfrac, gc_mn)

  gc_error(:,:) = gc_mn(:,:,1) - gc_target(:,:)
  gc_rmse = SQRT(SUM(gc_error(:,:)**2) / REAL(n_lat*n_dec))
  fc = gc_rmse

  print*,'XC =',xc,'   gc_rmse =',fc

  RETURN

END SUBROUTINE funct1

!#########################################################################
!#########################################################################
SUBROUTINE set_new_parameters(n,xc,ft)

  USE land_const, only : alpha,kpar
  IMPLICIT NONE
  INTEGER,INTENT(in) :: n,ft
  REAL,INTENT(in)    :: xc(n)

  alpha(ft)   = xc(1)
  kpar(ft)  = xc(2)

  RETURN

END SUBROUTINE set_new_parameters


!#########################################################################
! Returns rank 3 array of TOA solar radiation as function of latitude, 
! declination angle (proxy for day-of-year) and hour of day.
!#########################################################################
SUBROUTINE calc_toa_solar(n_lat, n_dec, n_hou, solar, dayfrac)

  USE phys_const

  IMPLICIT NONE
  !########### In  ################
  INTEGER,INTENT(in) :: n_lat, n_dec, n_hou

  !########### Out ################
  REAL,INTENT(out),DIMENSION(n_lat,n_dec,n_hou) :: solar
  REAL,INTENT(out),DIMENSION(n_lat,n_dec)       :: dayfrac

  !########### Work ###############
  REAL,PARAMETER :: s0 = 1367.0  !Solar constant at mean Earth-Sun distance (W/m2)

  REAL :: bnd_lat
  REAL :: bnd_dec
  REAL :: bnd_hou
  REAL :: d_lat
  REAL :: d_dec
  REAL :: d_hou

  REAL,DIMENSION(n_lat) :: r_lats, coszen
  REAL,DIMENSION(n_dec) :: r_decs
  REAL,DIMENSION(n_hou) :: r_hous
  REAL :: coszen2, hou_up, tantan
  
  INTEGER :: i,j,k  ! Loop counters

  CALL make_pi

  bnd_lat  = 0.95*con_pi/2.0   !Note only go up to ~85degrees lat else you get some large errors at the poles
  bnd_dec  = 23.5 * con_pi180
  bnd_hou  = con_pi

  d_lat = 2.0*bnd_lat/REAL(n_lat-1)
  d_dec = 2.0*bnd_dec/REAL(n_dec-1)
  IF(n_hou>1) d_hou = 2.0*bnd_hou/REAL(n_hou-1)


  DO i=1,n_lat
    r_lats(i) = -bnd_lat + REAL(i-1)*d_lat
  ENDDO
  DO i=1,n_dec
    r_decs(i) = -bnd_dec + REAL(i-1)*d_dec
  ENDDO
  DO i=1,n_hou
    r_hous(i) = -bnd_hou + REAL(i-1)*d_hou
  ENDDO

  solar(:,:,:) = 0.0
  IF(n_hou>1) THEN
    !#####################################
    ! Sub-daily insolation
    !#####################################
    DO k=1,n_hou
      DO j=1,n_dec
        coszen(:) = SIN(r_lats(:))*SIN(r_decs(j)) + COS(r_lats(:))*COS(r_decs(j))*COS(r_hous(k))
        WHERE(coszen<0.0) coszen=0.0
        solar(:,j,k) = s0*coszen(:)
      ENDDO
    ENDDO
    dayfrac(:,:) = 1.0
  ELSE
    !#####################################
    ! Daily mean insolation
    !#####################################
    DO j=1,n_dec
      DO i=1,n_lat
        tantan = (SIN(r_lats(i))*SIN(r_decs(j)))    &
               / (COS(r_lats(i))*COS(r_decs(j)))
        tantan = MIN(1.0,MAX(-1.0,tantan))
        hou_up = ACOS(-1.0*tantan)
        coszen2 = hou_up*SIN(r_lats(i))*SIN(r_decs(j)) + SIN(hou_up)*COS(r_lats(i))*COS(r_decs(j))
        IF(coszen2<0.0) coszen2=0.0
        solar(i,j,1) = s0*con_rpi*coszen2
        dayfrac(i,j) = hou_up*con_rpi
      ENDDO
    ENDDO
  ENDIF

  RETURN

END SUBROUTINE calc_toa_solar

!#########################################################################
! Reduces TOA solar to surface PAR by mean atmospheric 
! absorption/reflection, mean surface albedo, PAR fraction of solar 
! wavelengths.
!#########################################################################
SUBROUTINE calc_par(n_lat,n_dec,n_hou,solar,par)

  IMPLICIT NONE
  !########### In  ################
  INTEGER,INTENT(in) :: n_lat, n_dec, n_hou
  REAL,INTENT(in),DIMENSION(n_lat,n_dec,n_hou) :: solar
  !########### Out ################
  REAL,INTENT(out),DIMENSION(n_lat,n_dec,n_hou) :: par
  !########### Work ###############
  REAL,PARAMETER :: atm_absorb = 0.30   ! Fraction of TOA radiation atmos absorbs and reflects
  REAL,PARAMETER :: surf_alb   = 0.15   ! Surface albedo
  REAL,PARAMETER :: band_frac  = 0.50   ! Approximate fraction of solar radiation that is photosynthetically active

  par(:,:,:) = (1.0-atm_absorb) * (1.0-surf_alb) * band_frac * solar

  print*,'%% Maximum PAR =',MAXVAL(par(1,:,:))
  RETURN

END SUBROUTINE calc_par

!#########################################################################
!
!#########################################################################
SUBROUTINE calc_conductance(n_lat, n_dec, n_hou, ft, par, dayfrac, gc)

  USE land_const, only : lai_max,kpar

  IMPLICIT NONE

  INTEGER,INTENT(in) :: n_lat, n_dec, n_hou, ft
  REAL,INTENT(in),DIMENSION(n_lat,n_dec,n_hou) :: par
  REAL,INTENT(in),DIMENSION(n_lat,n_dec) :: dayfrac

  REAL,INTENT(out),DIMENSION(n_lat,n_dec,n_hou) :: gc

  INTEGER :: i,j,k              ! Loop counters
  REAL,PARAMETER :: dqc   = 1.0E-3
  REAL,PARAMETER :: co2   = 0.49E-3
  REAL,PARAMETER :: o2    = 0.23
  REAL,PARAMETER :: tair  = 21.0 + 273.15
  REAL,PARAMETER :: fsmc  = 1.0
  REAL,PARAMETER :: pstar = 1.0E5
  REAL :: fpar, lai
  REAL :: anetc, ci, rdc, gc_out

  lai = lai_max(ft)
  fpar = (1 - EXP(-kpar(ft)*lai)) / kpar(ft)
  DO k=1,n_hou
    DO j=1,n_dec
      DO i=1,n_lat
        gc_out = 0.0
        CALL canopy(1,(/1/),1,1,(/1/),ft,dqc,par(i,j,k),tair,co2,o2,pstar,fpar,fsmc,lai  & !IN
                    ,gc_out,anetc,ci,rdc,dayfrac(i,j)                                    ) !OUT
        gc(i,j,k) = gc_out
      ENDDO
    ENDDO
  ENDDO

  RETURN

END SUBROUTINE calc_conductance

