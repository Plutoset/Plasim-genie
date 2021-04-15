MODULE make_restart

  !#### Potential vegetation grid and data ####
  INTEGER,PARAMETER :: pveg_nlat = 360
  INTEGER,PARAMETER :: pveg_nlon = 720
  INTEGER,DIMENSION(pveg_nlon,pveg_nlat)   :: pveg_dat
  INTEGER,DIMENSION(pveg_nlon,pveg_nlat,3) :: pveg_dat_pfts
  REAL,DIMENSION(pveg_nlat) :: pveg_grid_midlat
  REAL,DIMENSION(pveg_nlon) :: pveg_grid_midlon
  REAL,PARAMETER :: primary_weight = 0.9
  LOGICAL :: l_s2n

CONTAINS
!#############################################################################
!  Program to read the ISLSCP II potential vegetation dataset of 
!  Ramankutty and Foley (1999), map R&F vegetation types to TRIFFID 
!  PFTs, regrid to GENIE-land resolutions (IGCM or EMBM) and output 
!  ASCII file
!
! Data from URL:
!   http://islscp2.sesda.com/ISLSCP2_1/html_pages/groups/veg/potential_veg_xdeg.html
!
! Reference for dataset publication:
!   Ramankutty, N., and J.A. Foley, Estimating historical changes in
!   global land cover: Croplands from 1700 to 1992, Global Biogeochemical
!   Cycles, 13, 997-1027, 1999.
!
! 20/01/05 PPH
!#############################################################################

  SUBROUTINE make_new_restart(nlat_atm,nlon_atm,lats_atm,lons_atm,landseamask_atm &
#ifdef useice
                          , icefrac_atm &
#endif
                          )
    USE land_const, only : ntype
    IMPLICIT NONE

    INTEGER,INTENT(in) :: nlat_atm, nlon_atm
    REAL,INTENT(in),DIMENSION(nlon_atm,2) :: lons_atm
    REAL,INTENT(in),DIMENSION(nlat_atm,2) :: lats_atm
    REAL,INTENT(in),DIMENSION(nlon_atm,nlat_atm) :: landseamask_atm
#ifdef useice
    REAL,INTENT(in),DIMENSION(nlon_atm,nlat_atm) :: icefrac_atm
#endif

    REAL,DIMENSION(nlon_atm,nlat_atm,ntype) :: tile_fracs_atm
    REAL,DIMENSION(nlon_atm,nlat_atm,ntype) :: tile_lais_atm

    IF(lats_atm(1,1)>lats_atm(nlat_atm,1)) THEN
      l_s2n = .false.
    ELSE
      l_s2n = .true.
    ENDIF


    CALL read_veg
    print*,'Read veg...'
    CALL map_veg_to_pft
    print*,'Mapped veg...'
    CALL regrid_veg(nlat_atm,nlon_atm,ntype,lats_atm,lons_atm,tile_fracs_atm,tile_lais_atm)
    print*,'Regridded veg...'
    CALL make_land_restart(nlat_atm,nlon_atm,tile_fracs_atm,tile_lais_atm,landseamask_atm &
#ifdef useice
                          , icefrac_atm &
#endif
                          )
    print*,'Made restart...'

    RETURN
  END SUBROUTINE make_new_restart

  !#####################################################################
  ! Subroutine to read original 0.5deg data file
  !#####################################################################
  SUBROUTINE read_veg
    IMPLICIT NONE
    INTEGER :: i ! Loop counter

    OPEN(unit=14,file="./potential_veg_xdeg/potential_veg_hd.asc")
    DO i=1,pveg_nlat
      READ(14,*) pveg_dat(:,i)
    ENDDO
    CLOSE(14)

    RETURN
  END SUBROUTINE read_veg

  !#####################################################################
  ! PFT1 PFT2
  !            0        Water Bodies 
  ! 1          1        Tropical Evergreen Forest/Woodland
  ! 1          2        Tropical Deciduous Forest/Woodland
  ! 1     2    3        Temperate Broadleaf Evergreen Forest/Woodland
  ! 2          4        Temperate Needleleaf Evergreen Forest/Woodland
  ! 1     2    5        Temperate Deciduous Forest/Woodland
  ! 2          6        Boreal Evergreen Forest/Woodland
  ! 2          7        Boreal Deciduous Forest/Woodland
  ! 2     1    8        Mixed Forest
  ! 4     5    9        Savanna
  ! 3         10        Grassland/Steppe
  ! 5         11        Dense Shrubland
  ! 6     5   12        Open Shrubland
  ! 5         13        Tundra
  ! 6         14        Desert
  ! 7         15        Polar desert/Rock/Ice
  !           16        No Data over Land
  !#####################################################################
  SUBROUTINE map_veg_to_pft
    IMPLICIT NONE
    INTEGER :: i,j
    
    DO j=1,pveg_nlat
      DO i=1,pveg_nlon
        pveg_dat_pfts(i,j,:) = 0
        SELECT CASE (pveg_dat(i,j))
          CASE(1,2)
            pveg_dat_pfts(i,j,1) = 1 !BT
            pveg_dat_pfts(i,j,3) = 9 !LAI
          CASE(3)
            pveg_dat_pfts(i,j,1) = 1 !BT
            pveg_dat_pfts(i,j,2) = 2 !NT
            pveg_dat_pfts(i,j,3) = 6 !LAI
          CASE(4,6,7)
            pveg_dat_pfts(i,j,1) = 2 !NT
            pveg_dat_pfts(i,j,3) = 5 !LAI
          CASE(5)
            pveg_dat_pfts(i,j,1) = 1 !BT
            pveg_dat_pfts(i,j,2) = 2 !NT
            pveg_dat_pfts(i,j,3) = 6 !LAI
          CASE(8)
            pveg_dat_pfts(i,j,1) = 2 !NT
            pveg_dat_pfts(i,j,2) = 1 !BT
            pveg_dat_pfts(i,j,3) = 7 !LAI
          CASE(9)
            pveg_dat_pfts(i,j,1) = 4 !C4
            pveg_dat_pfts(i,j,2) = 1 !BT
            pveg_dat_pfts(i,j,3) = 5 !LAI
          CASE(10)
            pveg_dat_pfts(i,j,1) = 3 !C3
            pveg_dat_pfts(i,j,3) = 2 !LAI
          CASE(11,13)
            pveg_dat_pfts(i,j,1) = 5 !SH
            pveg_dat_pfts(i,j,3) = 4 !LAI
          CASE(12)
            pveg_dat_pfts(i,j,1) = 6 !BS
            pveg_dat_pfts(i,j,2) = 5 !SH
            pveg_dat_pfts(i,j,3) = 4 !LAI
          CASE(14)
            pveg_dat_pfts(i,j,1) = 6 !BS
            pveg_dat_pfts(i,j,3) = 0 !LAI
          CASE(15)
            pveg_dat_pfts(i,j,1) = 7 !LI
            pveg_dat_pfts(i,j,3) = 0 !LAI
          CASE DEFAULT
            pveg_dat_pfts(i,j,1) = 0 !Unknown
            pveg_dat_pfts(i,j,3) = 0 !LAI
        END SELECT
      ENDDO
    ENDDO

    RETURN
  END SUBROUTINE map_veg_to_pft


  !#####################################################################
  ! Loop over atmos model grid, finding high-res dataset gridboxes 
  ! contained within each atmos box.
  !#####################################################################
  SUBROUTINE regrid_veg(nlat_atm,nlon_atm,ntype,lats_atm,lons_atm,tile_fracs,tile_lais)
    USE land_const, only : npft, soil, frac_min
    IMPLICIT NONE
    INTEGER,INTENT(in) :: nlat_atm, nlon_atm, ntype
    REAL,INTENT(in),DIMENSION(nlat_atm,2) :: lats_atm
    REAL,INTENT(in),DIMENSION(nlon_atm,2) :: lons_atm

    INTEGER :: i, j
    INTEGER :: imx, imn, jmx, jmn
    REAL,DIMENSION(nlon_atm,nlat_atm,ntype) :: tile_fracs
    REAL,DIMENSION(nlon_atm,nlat_atm,ntype) :: tile_lais

    CALL setup_pveg_grid(nlon_atm,lons_atm)

    tile_fracs(:,:,:) = -100.0
    DO j=1,nlat_atm
      DO i=1,nlon_atm
!        print*,shape(tile_fracs),nlon_atm,nlat_atm,i,j,tile_fracs(i,j,:)
        imn = MINVAL(MINLOC(pveg_grid_midlon,MASK=(pveg_grid_midlon>=lons_atm(i,1)))) !atmos longitude lower bound
        imx = MINVAL(MAXLOC(pveg_grid_midlon,MASK=(pveg_grid_midlon< lons_atm(i,2)))) !atmos longitude upper bound
        IF(l_s2n) THEN
          jmn = MINVAL(MINLOC(pveg_grid_midlat,MASK=(pveg_grid_midlat> lats_atm(j,1)))) !atmos latitude lower bound
          jmx = MINVAL(MAXLOC(pveg_grid_midlat,MASK=(pveg_grid_midlat<=lats_atm(j,2)))) !atmos latitude upper bound
        ELSE
          jmx = MINVAL(MINLOC(pveg_grid_midlat,MASK=(pveg_grid_midlat> lats_atm(j,2)))) !atmos latitude lower bound
          jmn = MINVAL(MAXLOC(pveg_grid_midlat,MASK=(pveg_grid_midlat<=lats_atm(j,1)))) !atmos latitude upper bound
        ENDIF

!        print*,i,j,'//',imn,imx,'//',jmn,jmx,'//',lons_atm(i,:),lats_atm(j,:)!,pveg_dat_pfts(imn:imx,jmn:jmx,1)

        CALL calcfracs(imx-imn,jmx-jmn,ntype, &
                       pveg_dat_pfts(imn:imx,jmn:jmx,1), &   ! Primary PFT
                       pveg_dat_pfts(imn:imx,jmn:jmx,2), &   ! Secondary PFT
                       pveg_dat_pfts(imn:imx,jmn:jmx,3), &   ! LAI of primary PFT
                       tile_fracs(i,j,:),tile_lais(i,j,:))

        !#################################################################
        ! Apply manual fix for Antarctica, which is "no data" in the 
        ! potential vegetation dataset
        !#################################################################
        if(tile_fracs(i,j,7) > 0.5) print*,'Aft Calcfracs',i,j,lons_atm(i,1),lats_atm(j,1),tile_fracs(i,j,:)!bowie
        IF(lats_atm(j,1).le.-60.0) THEN
          print*,'Antarctica %%',i,j,tile_fracs(i,j,:)
          tile_fracs(i,j,1:npft) = frac_min
          tile_fracs(i,j,soil)   = 0.0
          tile_fracs(i,j,soil+1) = 1.0 - SUM(tile_fracs(i,j,1:soil))
        ENDIF
      ENDDO
    ENDDO

    RETURN
  END SUBROUTINE regrid_veg

  !#####################################################################
  ! Potential vegetation data is on a 0.5x0.5 degree grid 
  !#####################################################################
  SUBROUTINE setup_pveg_grid(nlon_atm,lons_atm)
    USE land_general_utils
    IMPLICIT NONE

    INTEGER,INTENT(in) :: nlon_atm
    REAL,INTENT(in),DIMENSION(nlon_atm,2) :: lons_atm
    REAL,PARAMETER :: lon_0 = -180.0
    REAL,PARAMETER :: lat_0 = 90.0
    REAL,PARAMETER :: dxy   = 0.5
    INTEGER :: i,j

    DO j=1,pveg_nlat
      pveg_grid_midlat(j) = lat_0 - REAL(j)*dxy
    ENDDO
    DO i=1,pveg_nlon
      pveg_grid_midlon(i) = lon_0 + REAL(i)*dxy
      IF(pveg_grid_midlon(i).lt.MINVAL(lons_atm(:,1))) pveg_grid_midlon(i) = pveg_grid_midlon(i) + 360.0
      IF(pveg_grid_midlon(i).gt.MAXVAL(lons_atm(:,2))) pveg_grid_midlon(i) = pveg_grid_midlon(i) - 360.0
    ENDDO

    !###################################################################
    ! Shift pot. veg. grid and adat to match atmos model grid
    !###################################################################
    i = MINVAL(MINLOC(pveg_grid_midlon))
!    print*,pveg_grid_midlon,' //'
    pveg_grid_midlon(:)  = CSHIFT(pveg_grid_midlon(:),i-1,1)
!    print*,pveg_grid_midlon,' //'
    pveg_dat_pfts(:,:,:) = CSHIFT(pveg_dat_pfts(:,:,:),i-1,1)

    !###################################################################
    ! Reflect pot. veg. grid and data to match atmos model grid
    !###################################################################
    IF(l_s2n) THEN
      pveg_grid_midlat(:)  = REFLECT(pveg_grid_midlat(:))
      pveg_dat_pfts(:,:,1) = REFLECT(pveg_dat_pfts(:,:,1),2)
      pveg_dat_pfts(:,:,2) = REFLECT(pveg_dat_pfts(:,:,2),2)
      pveg_dat_pfts(:,:,3) = REFLECT(pveg_dat_pfts(:,:,3),2)
    ENDIF

    RETURN
  END SUBROUTINE setup_pveg_grid


  !#####################################################################
  ! Calculates the tile fractions for a single model atmos grid box
  ! based on the high-res tile types within that gridbox
  !#####################################################################
  SUBROUTINE calcfracs(nx,ny,ntype,pft_primary,pft_secondary,pft_lais,tile_fracs,tile_lais)
    USE land_const, only : lai_min
    IMPLICIT NONE
    INTEGER,INTENT(in) :: nx, ny, ntype
    INTEGER,INTENT(in),DIMENSION(nx,ny) :: pft_primary, pft_secondary, pft_lais
    REAL,INTENT(out),DIMENSION(ntype) :: tile_fracs
    REAL,INTENT(out),DIMENSION(ntype) :: tile_lais

    INTEGER,DIMENSION(2) :: ntl
    INTEGER :: i,ic
    REAL,DIMENSION(ntype,2) :: temp_fracs
    REAL :: tl


    !####################################################################
    ! Count the number of high-res points in this low-res grid box
    !####################################################################
    ntl(:) = 0
    ntl(1) = COUNT(pft_primary>0)
    ntl(2) = COUNT(pft_secondary>0)

!    print*,'POINT %% ',ntl(1)

    !####################################################################
    ! If there is at least 1 high-res land point in this low-res gridbox
    !####################################################################
    temp_fracs(:,:) = -99.0
    tile_lais(:)    = -99.0

    IF(ntl(1).ne.0) THEN
      DO i=1,ntype
        ! Calculate gridbox fractions based on primary tile fractions
        ic = 0
        ic = COUNT(pft_primary==i)
!        print*,i,ic
        temp_fracs(i,:) = 0.0
        temp_fracs(i,1) = REAL(ic)/REAL(ntl(1))

        ! Calculate the fractions of secondary tiles
        IF(ntl(2).ne.0) temp_fracs(i,2) = REAL(COUNT(pft_secondary==i))/REAL(ntl(2))

        ! Calculate gridbox mean LAI for each PFT tile
        IF(i.le.5 .and. ic.gt.0) THEN
          tl = REAL(SUM(pft_lais, MASK=(pft_primary==i)))/REAL(ic)
          tile_lais(i) = MAX(lai_min(i),tl)
        ELSEIF(i.le.5 .and. ic.eq.0) THEN
          tile_lais(i) = lai_min(i)
        ELSE
          tile_lais(i) = 0.0
        ENDIF

      ENDDO
    ENDIF

    !####################################################################
    ! If there is are secondary tiles defined in this gridbox then
    ! Add their contribution
    !####################################################################
    IF(ntl(2).gt.0) THEN
      tile_fracs(:) = primary_weight*temp_fracs(:,1) + (1.0-primary_weight)*temp_fracs(:,2)
    ELSE
      tile_fracs(:) = temp_fracs(:,1)
    ENDIF
!    print*,nx,ny,ntl,SUM(tile_fracs(:))

    RETURN
  END SUBROUTINE calcfracs


  !#######################################################################################
  !#######################################################################################
  ! Subroutine to initialise land variables and write a land restart file in the same 
  ! format at the on-line restart files.
  !#######################################################################################
  !#######################################################################################
  SUBROUTINE make_land_restart(nlat_atm,nlon_atm,tile_fracs_atm,tile_lais_atm,landseamask_atm &
#ifdef useice
                              , icefrac_atm &
#endif
                              )

    USE land_const
    USE land_var
    USE land_diags
    USE land_restart

    IMPLICIT NONE

    INTEGER,INTENT(in) :: nlat_atm,nlon_atm
    REAL,INTENT(in),DIMENSION(nlon_atm,nlat_atm) :: landseamask_atm
#ifdef useice
    REAL,INTENT(in),DIMENSION(nlon_atm,nlat_atm) :: icefrac_atm
#endif
    REAL,INTENT(inout),DIMENSION(nlon_atm,nlat_atm,ntype) :: tile_fracs_atm
    REAL,INTENT(inout),DIMENSION(nlon_atm,nlat_atm,npft)  :: tile_lais_atm
    REAL,DIMENSION(nlon_atm,nlat_atm) :: igcm_lsm
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
          print*,n,' frac=',tile_fracs_atm(i,j,:)!bowie
          IF(ANY(tile_fracs_atm(i,j,:)<-95.0).or.ANY(tile_lais_atm(i,j,:)<-95.0)) THEN
            PRINT*,'No data land points in atmos gridbox ',i,j
            frac(n,1:npft) = frac_min
#ifdef useice
            IF(icefrac_atm(i,j) > 0.05) THEN
              PRINT*,'  Ah, but there is',icefrac_atm(i,j)*100.0, '% of ice in supplied file, so using that.'
              frac(n,soil+1) = icefrac_atm(i,j) - REAL(npft)*frac_min
            ELSE
              frac(n,soil+1) = 0.0
            ENDIF
#else
            frac(n,soil+1) = 0.0
#endif
            frac(n,soil)   = 1.0 - REAL(npft)*frac_min - frac(n,soil+1)
            lai(n,:)       = lai_min(:)
            if(n.eq.419) print*,n,' frac=',frac(n,:)!bowie
          ELSE

            frac(n,:)    = tile_fracs_atm(i,j,:)
            WHERE(frac(n,1:npft) < frac_min) frac(n,1:npft) = frac_min
            lai(n,:)     = tile_lais_atm(i,j,:)

          ENDIF
          if(frac(n,7).gt.0.5) print*,'ICE %%',i,j,n,icefrac_atm(i,j),frac(n,7)

          ht(n,:)      = (a_wl(:)/(a_ws(:)*eta_sl(:)))*(lai(n,:)**(b_wl(:)-1.0))
          z0_gb(n)     = 1.0
          albedo_gb(n) = 0.3

          cs(n)         = 1.0e-6
          lying_snow(n) = 0.0
          msoil(n)      = 2000.0
          tsub1(n)      = 273.15 + 5.0
          tstar_gb(n)   = 273.15 + 5.0
          fx_le(n)      = 0.0
          fx_sen(n)     = 0.0
          evap(n)       = 0.0
          esub(n)       = 0.0
          gravdr(n)     = 0.0
          resp_s_dr(n)  = 0.0

          g_leaf_phen_acc(n,:) = 0.0
          g_leaf_acc(n,:)      = 0.0
          gpp_dr(n,:)          = 0.0
          npp_dr(n,:)          = 0.0
          resp_w_dr(n,:)       = 0.0
          tstar(n,:)           = 273.15 + 5.0
        ENDIF
      ENDDO
    ENDDO

    CALL land_restart_write('./')

    RETURN

  END SUBROUTINE make_land_restart

END MODULE make_restart
