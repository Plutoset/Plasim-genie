MODULE make_restart

CONTAINS
!#############################################################################
! Table 1: Land cover classes
!
! FT Code  LAI  Land Cover Description
!
! 7  00   0   Open Water
! 7  01   0   Inland water
! 3  02   3   Bog or marsh
! 6  03   0   Ice
! 3  04   5  Paddy rice
! 3  05   3  Mangrove (tree swamp)
! 2  10   6  Dense needleleaf evergreen forest
! 2  11   6  Open needleleaf evergreen woodland
! 1  12   9  Dense mixed needleleaf and broadleaf evergreen and deciduous forest
! 1  13   9  Open mixed needleleaf and broadleaf, evergreen and deciduous woodland
! 1  14   9  Evergreen broadleaf woodland
! 1  15   7  Evergreen broadleaf cropland
! 1  16   3  Evergreen broadleaf shrub
! 2  17   4  Open deciduous needleleaf woodland
! 2  18   4  Dense deciduous needleleaf forest
! 1  19   9  Dense evergreen broadleaf forest
! 1  20   5  Dense deciduous broadleaf forest
! 1  21   5  Open deciduous broadleaf woodland
! 2  22   5  Deciduous tree crops (temperate)
! 1  23   9  Open tropical Woodland
! 1  24   4  Woodland + shrub
! 1  25   5  Dense drought deciduous forest
! 1  26   5  Open Drought deciduous woodland
! 5  27   3  Deciduous shrub
! 5  28   3  Thorn shrub
! 3  30   5   Temperate meadow and permanent pasture
! 3  31   4   Temperate rough grazing
! 4  32   4   Tropical grassland + shrub
! 4  33   4   Tropical pasture
! 3  34   2   Rough grazing + shrub
! ?3  35  4    Pasture + tree
! 4  36   1   Semi arid rough grazing
! 4  37   5   Tropical savanna (grassland + tree)
! ?3  39  5    Pasture + shrub
! ?3  40  5    Arable cropland
! ?3  41  5    Dry farm arable
! ?3  42  5    Nursery and market gardening
! 4  43   5   Cane sugar
! 4  44   5   Maize
! ?3  45  5    Cotton
! ?3  46  5    Coffee
! ?3  47  5    Vineyard
! ?3  48  5    Irrigated cropland
! ?3  49  5    Tea
! 1  50   9   Equatorial rain forest
! 1  51   9   Equatorial tree crop
! 1  52   7   Tropical broadleaf forest (slight seasonality)
! 5  61   1   Tundra
! 5  62   1   Dwarf shrub (tundra transition and high altitude wasteland)
! 8  70   0   Sand desert and barren land
! 8  71   0   Scrub desert and semi desert
! 8  73   0   Semi desert + scattered trees
! 6  80   0   Urban
!
!#############################################################################
SUBROUTINE make_new_restart(nlat_atm,nlon_atm,lats_atm,lons_atm,landseamask_atm)

  USE land_const, only: ntype,npft,lai_min
  USE land_general_utils

  IMPLICIT NONE
  !##############################################
  ! Variables related to atmos grid
  !##############################################
  INTEGER,INTENT(in) :: nlat_atm, nlon_atm
  REAL,INTENT(in),DIMENSION(nlat_atm,2)   :: lats_atm
  REAL,INTENT(in),DIMENSION(nlon_atm,2)   :: lons_atm
  REAL,INTENT(in),DIMENSION(nlon_atm,nlat_atm) :: landseamask_atm
  REAL,DIMENSION(nlon_atm,nlat_atm,ntype) :: tile_fracs_atm
  REAL,DIMENSION(nlon_atm,nlat_atm,npft)  :: tile_lais_atm

  !##############################################
  ! Variables related WHS data
  !##############################################
  INTEGER,PARAMETER :: wilson_nlat = 180
  INTEGER,PARAMETER :: wilson_nlon = 360
  INTEGER,PARAMETER :: nskip       = 1620
  INTEGER,PARAMETER :: nend        = 4860
  INTEGER,PARAMETER :: nread       = nend-nskip
!  INTEGER,DIMENSION(40,nread) :: soilin,soilmod,wilson_lai
  REAL,DIMENSION(wilson_nlat) :: wilson_lats
  REAL,DIMENSION(wilson_nlon) :: wilson_lons
  INTEGER,DIMENSION(wilson_nlon,wilson_nlat) :: soilin,soilmod,wilson_lai
  REAL,DIMENSION(wilson_nlon,wilson_nlat) :: wilson_pfts, wilson_types, wilson_lais

  INTEGER :: i,j                           !Loop counters
  INTEGER,DIMENSION(1) :: imx,imn,jmx,jmn  !Array locations
  INTEGER :: lon_adjust
  REAL    :: lonmin_atm, lonmax_atm
  LOGICAL :: l_s2n
  INTEGER :: o_beg,o_end,o_step

  !####################################################
  ! Reading original data and map to genie-land tile
  ! types.
  ! Ignore first 1620 lines of soil type data
  !####################################################
  OPEN(unit=77,file='soilveg.dat',STATUS='old')
  DO i=1,nskip
    READ(77,'(40I2)')
  ENDDO
  READ(77,'(40I2)')soilin
  CLOSE(77)

  soilmod(:,:)    = 0
  wilson_lai(:,:) = 0
  DO j=1,wilson_nlat
    DO i=1,wilson_nlon
      !####################################################
      ! Map Wilson types to TRIFFID/genie-land tile types
      !####################################################
      SELECT CASE (soilin(i,j))
        CASE(12:16,19:21,23:26,50:52) !BT
          soilmod(i,j) = 6
        CASE(10:11,17:18,22) !NT
          soilmod(i,j) = 6
        CASE(2,4:5,30,31,35,39:42,45:49) !C3
          soilmod(i,j) = 6
        CASE(32,33,34,36,37,43,44) !C4
          soilmod(i,j) = 6
        CASE(27,28,61,62) !SH
          soilmod(i,j) = 6
        CASE(70,71,73) !BS
          soilmod(i,j) = 6
        CASE(0,1) !IW
          soilmod(i,j) = 99
        CASE(3) !ICE
          soilmod(i,j) = 7
        CASE DEFAULT
          soilmod(i,j) = 99
      END SELECT
      !####################################################
      ! Map LAI of
      !####################################################
      SELECT CASE(soilin(i,j))
        CASE(36,61:63)
          wilson_lai(i,j) = 0.1
        CASE(34)
          wilson_lai(i,j) = 0.1
        CASE(2,5,16,27,28)
          wilson_lai(i,j) = 0.1
        CASE(17,18,24,31,32,33,35)
          wilson_lai(i,j) = 0.1
        CASE(4,20:22,25,26,30,37:49)
          wilson_lai(i,j) = 0.1
        CASE(10:11)
          wilson_lai(i,j) = 0.1
        CASE(15,52)
          wilson_lai(i,j) = 0.1
        CASE(12:14,19,23,50,51)
          wilson_lai(i,j) = 0.1
        CASE(0,1,3,70:73,80)
          wilson_lai(i,j) = 0
        CASE DEFAULT
          print*,'Undefined: ',soilin(i,j)
          wilson_lai(i,j) = 0
      END SELECT
    ENDDO
  ENDDO

  !####################################################
  ! Calc the Wilson gridbox midpoints
  !####################################################
  DO j=1,wilson_nlat
    wilson_lats(j) = 90.5 - REAL(j) !-89.5 + REAL(j)
  ENDDO
  DO i=1,wilson_nlon
    wilson_lons(i) = -179.0 + REAL(i-1)
  ENDDO

  wilson_types(:,:) = soilin(:,:)
  wilson_pfts(:,:)  = soilmod(:,:)
  wilson_lais(:,:)  = wilson_lai(:,:)

  print*,lats_atm(1,1),lats_atm(nlat_atm,1)
  print*,lons_atm(1,1),lons_atm(nlon_atm,1)
  IF(lats_atm(1,1)>lats_atm(nlat_atm,1)) THEN
    l_s2n = .false.
  ELSE
    l_s2n = .true.
    print*,'Adjusting WHS data latitudes to match atmos grid...'
    wilson_lats  = REFLECT(wilson_lats)
    wilson_types = REFLECT(wilson_types,2)
    wilson_pfts  = REFLECT(wilson_pfts,2)
    wilson_lais  = REFLECT(wilson_lais,2)
  ENDIF

  ! Shift longitude of WHS arrays to match atmos grid
  lonmin_atm        = MINVAL(lons_atm)
  lonmax_atm        = MAXVAL(lons_atm)
  lon_adjust        = -(FLOOR((MINVAL(wilson_lons)-lonmin_atm)/1.0))
  wilson_lons(:)    = CSHIFT(wilson_lons(:),lon_adjust,1)
  wilson_types(:,:) = CSHIFT(wilson_types(:,:),lon_adjust,1)
  wilson_pfts(:,:)  = CSHIFT(wilson_pfts(:,:),lon_adjust,1)
  wilson_lais(:,:)  = CSHIFT(wilson_lais(:,:),lon_adjust,1)


  WHERE(wilson_lons<=lonmin_atm) wilson_lons = wilson_lons + 360.0
  WHERE(wilson_lons>=lonmax_atm) wilson_lons = wilson_lons - 360.0
  PRINT*,'Adjusting WHS data longitudes to match atmos grid...',lonmin_atm,lon_adjust

  !####################################################
  ! Find Wilson boxes within each atmos gridbox and
  ! calculate fractional coverage of each tile type as
  ! a fraction of non-water area.
  !####################################################
  DO j=1,nlat_atm
    ! If Antarctica, set to land-ice
    IF(MAXVAL(lats_atm(j,:))<-60.0) THEN
      tile_fracs_atm(:,j,:) = 0.0
      tile_fracs_atm(:,j,7) = 1.0
      DO i=1,nlon_atm
        tile_lais_atm(i,j,:) = 0.1
      ENDDO
    ELSE
      DO i=1,nlon_atm
        imn = MINLOC(wilson_lons,MASK=(wilson_lons>=lons_atm(i,1))) !atmos lower bound
        imx = MAXLOC(wilson_lons,MASK=(wilson_lons< lons_atm(i,2))) !atmos upper bound
        IF(l_s2n) THEN
          jmn = MINLOC(wilson_lats,MASK=(wilson_lats> lats_atm(j,1))) !atmos lower bound
          jmx = MAXLOC(wilson_lats,MASK=(wilson_lats<=lats_atm(j,2))) !atmos upper bound
        ELSE
          jmx = MINLOC(wilson_lats,MASK=(wilson_lats> lats_atm(j,2))) !atmos lower bound
          jmn = MAXLOC(wilson_lats,MASK=(wilson_lats<=lats_atm(j,1))) !atmos upper bound
        ENDIF
!        print*,imn,imx,lons_atm(i,1),lons_atm(i,2)
!        print*,jmn,jmx,lats_atm(j,1),lats_atm(j,2)
!        print*,wilson_pfts(imn(1):imx(1),jmn(1):jmx(1))
        CALL calcfracs(ntype,npft,wilson_pfts(imn(1):imx(1),jmn(1):jmx(1)), &
                       wilson_lais(imn(1):imx(1),jmn(1):jmx(1)),            &
                       tile_fracs_atm(i,j,:),tile_lais_atm(i,j,:)           )
      ENDDO
    ENDIF
  ENDDO

  !TODO In final version get rid of this output?
  !####################################################
  ! Write out resulting fractions to 7 ascii files.
  ! Write out South to North for easier Ferret reading
  !####################################################
  
  IF(l_s2n) THEN
    o_beg  = 1
    o_end  = nlat_atm
    o_step = 1
  ELSE
    o_beg  = nlat_atm
    o_end  = 1
    o_step = -1
  ENDIF

  OPEN(unit=88,file="pft_frac.bt.out")
  DO j=o_beg,o_end,o_step
    WRITE(88,'(64E15.5)')tile_fracs_atm(:,j,1)*landseamask_atm(:,j)
  ENDDO
  CLOSE(88)

  OPEN(unit=88,file="pft_frac.nt.out")
  DO j=o_beg,o_end,o_step
    WRITE(88,'(64E15.5)')tile_fracs_atm(:,j,2)*landseamask_atm(:,j)
  ENDDO
  CLOSE(88)

  OPEN(unit=88,file="pft_frac.c3.out")
  DO j=o_beg,o_end,o_step
    WRITE(88,'(64E15.5)')tile_fracs_atm(:,j,3)*landseamask_atm(:,j)
  ENDDO
  CLOSE(88)

  OPEN(unit=88,file="pft_frac.c4.out")
  DO j=o_beg,o_end,o_step
    WRITE(88,'(64E15.5)')tile_fracs_atm(:,j,4)*landseamask_atm(:,j)
  ENDDO
  CLOSE(88)

  OPEN(unit=88,file="pft_frac.sh.out")
  DO j=o_beg,o_end,o_step
    WRITE(88,'(64E15.5)')tile_fracs_atm(:,j,5)*landseamask_atm(:,j)
  ENDDO
  CLOSE(88)

  OPEN(unit=88,file="pft_frac.bs.out")
  DO j=o_beg,o_end,o_step
    WRITE(88,'(64E15.5)')tile_fracs_atm(:,j,6)*landseamask_atm(:,j)
  ENDDO
  CLOSE(88)

  OPEN(unit=88,file="pft_frac.ice.out")
  DO j=o_beg,o_end,o_step
    WRITE(88,'(64E15.5)')tile_fracs_atm(:,j,7)*landseamask_atm(:,j)
  ENDDO
  CLOSE(88)

  !####################################################
  ! 
  !####################################################
  CALL make_land_restart(nlat_atm,nlon_atm,tile_fracs_atm,tile_lais_atm,landseamask_atm)

  RETURN

END SUBROUTINE make_new_restart

!#######################################################################################
!#######################################################################################
! Subroutine to calculate the tile fractions in each atmos/land gridbox based on
! Wilson and Henderson-Sellers 1x1 degree classes.
!#######################################################################################
!#######################################################################################
SUBROUTINE calcfracs(ntype,npft,this_wilson_pfts,this_wilson_lais, &
                       this_tile_fracs_atm,this_tile_lais_atm)

  USE land_const, only : lai_min

  IMPLICIT NONE

  INTEGER,INTENT(in) :: ntype, npft
  REAL,DIMENSION(:,:),INTENT(in)    :: this_wilson_pfts,this_wilson_lais
  REAL,DIMENSION(:,:),ALLOCATABLE   :: lai_temp
  REAL,DIMENSION(ntype),INTENT(out) :: this_tile_fracs_atm
  REAL,DIMENSION(npft),INTENT(out)  :: this_tile_lais_atm

  INTEGER :: npts,np
  INTEGER :: t

  ALLOCATE(lai_temp(SIZE(this_wilson_lais,1),SIZE(this_wilson_lais,2)))

  this_tile_fracs_atm(:) = 0.0
  npts = COUNT(this_wilson_pfts<90.0)
  IF(npts==0) THEN
    !All ocean points on Wilson grid
    this_tile_fracs_atm(:) = -99.0
    this_tile_lais_atm(:)  = -99.0
  ELSE
    !At least one land point on Wilson grid
    DO t=1,ntype
      np = COUNT(this_wilson_pfts==REAL(t))
      this_tile_fracs_atm(t) = REAL(np)/REAL(npts)

      lai_temp(:,:) = 0.0
      IF(np>0 .AND. t<=5) THEN
        WHERE(this_wilson_pfts==REAL(t)) lai_temp = this_wilson_lais
        this_tile_lais_atm(t) = SUM(lai_temp)/REAL(np)
      ELSEIF(t<=5) THEN
        this_tile_lais_atm(t) = lai_min(t)
      ENDIF
    ENDDO
  ENDIF
  IF(ALLOCATED(lai_temp)) DEALLOCATE(lai_temp)
  
  RETURN
END SUBROUTINE calcfracs

!#######################################################################################
!#######################################################################################
! Subroutine to initialise land variables and write a land restart file in the same 
! format at the on-line restart files.
!#######################################################################################
!#######################################################################################
SUBROUTINE make_land_restart(nlat_atm,nlon_atm,tile_fracs_atm,tile_lais_atm,landseamask_atm)

  USE land_const
  USE land_var
  USE land_diags
  USE land_restart

  IMPLICIT NONE

  INTEGER,INTENT(in) :: nlat_atm,nlon_atm
  REAL,INTENT(in),DIMENSION(nlon_atm,nlat_atm) :: landseamask_atm
  REAL,INTENT(inout),DIMENSION(nlon_atm,nlat_atm,ntype) :: tile_fracs_atm
  REAL,INTENT(inout),DIMENSION(nlon_atm,nlat_atm,npft)  :: tile_lais_atm
  REAL,DIMENSION(nlon_atm,nlat_atm) :: igcm_lsm
  REAL :: tv
  INTEGER :: nco1, ifail1, n,i,j

  !####################################################
  ! Apply land/sea mask to tile fractions
  !####################################################
  DO n=1,ntype
    tile_fracs_atm(:,:,n) = tile_fracs_atm(:,:,n)*landseamask_atm(:,:)
  ENDDO
  DO n=1,npft
    tile_lais_atm(:,:,n) = tile_lais_atm(:,:,n)*landseamask_atm(:,:)
  ENDDO

  !####################################################
  ! Calc land points and indices
  !####################################################
  land_pts = COUNT(landseamask_atm(:,:)==1.0)
  print*,'land_pts =',land_pts

  nlon = nlon_atm
  nlat = nlat_atm
  CALL allocate_land_var
  CALL allocate_land_diags

  n=0
  DO j=1,nlat_atm
    DO i=1,nlon_atm
      IF(landseamask_atm(i,j)==1.0) THEN
        n=n+1
        IF(ANY(tile_fracs_atm(i,j,:)==-99.0).or.ANY(tile_lais_atm(i,j,:)==-99.0)) THEN
          PRINT*,'No WHS land points in atmos gridbox ',i,j,', which suggests a problem with your landsea mask.'
          frac(n,1:npft) = frac_min
          frac(n,soil)   = 1.0 - REAL(npft)*frac_min
          frac(n,soil+1) = 0.0
          lai(n,:)       = 0.1
        ELSE
          frac(n,:)    = tile_fracs_atm(i,j,:)
! Set veg tile fracs to minimum of 'seeding fraction'
          WHERE(frac(n,1:npft) < frac_min) frac(n,1:npft) = frac_min
! Where not veg or ice, fill in with bare soil
          tv = SUM(frac(n,1:npft)) + frac(n,soil+1)
          IF(tv < 1.0) THEN
            frac(n,soil) = 1.0 - tv
          ELSEIF(tv > 1.0) THEN
            frac(n,soil)   = 0.0
            frac(n,soil+1) = frac(n,soil+1) - (tv - 1.0)
          ENDIF
          print*,'Tfrac = ',SUM(frac(n,:))
          lai(n,:)     = tile_lais_atm(i,j,:)
        ENDIF
        ht(n,:)      = (a_wl(:)/(a_ws(:)*eta_sl(:)))*(lai_min(:)**(b_wl(:)-1.0))
        z0_gb(n)     = 1.0
        albedo_gb(n) = 0.2

        cs(n)         = 1.0e-6
        lying_snow(n) = 0.0
        msoil(n)      = 400.0
        tsub1(n)      = 273.15 - 20.0
        tstar_gb(n)   = 273.15 - 20.0
        fx_le(n)      = 0.0
        fx_sen(n)     = 0.0
        evap(n)       = 0.0
        esub(n)       = 0.0
        gravdr(n)     = 0.0
        snowmelt_acc(n)= 0.0
        resp_s_dr(n)  = 0.0

        g_leaf_phen_acc(n,:) = 0.0
        g_leaf_acc(n,:)      = 0.0
        gpp_dr(n,:)          = 0.0
        npp_dr(n,:)          = 0.0
        resp_w_dr(n,:)       = 0.0
        tstar(n,:)           = 273.15 - 20.0
      ENDIF
    ENDDO
  ENDDO

  CALL land_restart_write('./')

  RETURN

END SUBROUTINE make_land_restart

END MODULE make_restart
