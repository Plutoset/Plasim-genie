!#######################################################################################
!  
!  This f90 modules provides the routines for river-routing in GENIE.  It uses the
!  0.5x0.5 degree runoff destination file that comes with TRIP (Total Runoff and 
!  Integrating Pathways), Taikan Oki's runoff routing algorithm:
!
!  http://hydro.iis.u-tokyo.ac.jp/~taikan/TRIPDATA/TRIPDATA.html
!
!  Within GENIE, there is little call at the moment for detailed routing of runoff
!  on every timestep.  Instead a wet-point destination of runoff from each dry-point
!  is calculated at initialisation from the 1x1 degree TRIP data, and this runoff mask
!  is used for the rest of the simulation.
!
!  Ocean-grid longitudes must be monotonically increasing.
!
!  If making runoff mask from TRIP data the procedure is,
!   (1) Identify number of and i,j of valid outflow points on ocean grid
!   (2) Identify number of and i,j of valid outflow points on TRIP grid
!   (3) Calculate destinations of each land point indexed by TRIP outflow point numbers
! ***** THE NEXT STEP IS THE MOST IMPORTANT ONE *****
!   (4) Map TRIP outflow points (trip_outflow_indices) to ocean outflow points (ocn_outflow_indices)
!      (4.1) TRIP point lies in OCN wet box
!      (4.2) Find nearest adjacent OCN wet box N/E/S/W
!      (4.3) Find nearest adjacent OCN wet box NW/NE/SE/SW
!      (4.4) Apply geographical fix
!   (5) Map all TRIP land points (trip_runoff_dest) to ocean outflow points (trip2ocn_mapping)
!   (6) For each OCEAN dry point, find the dominant wet outflow point on TRIP grid (trip_runoff_ocnindices)
!
!#######################################################################################
MODULE make_runoff_map

  INTEGER :: nlato,nlono                          ! Number of latitude/longitude points on ocean grid
  INTEGER :: nout_lnd                             ! Total number of land points on ocean grid
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: ij_runoff ! (i,j) indices of runoff destinations (wet) as a land vector
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: ij_lando  ! (i,j) indices of land (dry) points on ocean grid
  INTEGER,DIMENSION(:,:,:),ALLOCATABLE :: ij_runoff_xy ! (i,j) indices of runoff destinations (wet) as a lon/lat grid
  INTEGER :: kmax
  INTEGER,PARAMETER :: nxy = 2
  REAL :: total_endorheic                         ! Total water not runoff to oceans

CONTAINS

!###################################################
!
!###################################################
  SUBROUTINE initialise_tethys(init_option,kmax_in, &
             nlat_ocn,nlon_ocn,landseamask_ocn,lats_ocn,lons_ocn, &
             flname)

    IMPLICIT NONE

    INTEGER,INTENT(in) :: init_option
    INTEGER,INTENT(in) :: kmax_in
    INTEGER,INTENT(in) :: nlat_ocn,nlon_ocn
    INTEGER,INTENT(in),DIMENSION(nlon_ocn,nlat_ocn) :: landseamask_ocn
    REAL,INTENT(in),DIMENSION(nlat_ocn,2) :: lats_ocn
    REAL,INTENT(in),DIMENSION(nlon_ocn,2) :: lons_ocn

    CHARACTER(len=*),INTENT(in) :: flname

    kmax = kmax_in

    SELECT CASE(init_option)
    CASE(1)
      CALL tethys_init_trip(nlat_ocn,nlon_ocn,landseamask_ocn,lats_ocn,lons_ocn)
    CASE(2)
      CALL tethys_init_trip(nlat_ocn,nlon_ocn,landseamask_ocn,lats_ocn,lons_ocn)
    CASE(3)
      CALL tethys_init_goldstein(nlat_ocn,nlon_ocn,lats_ocn,lons_ocn,landseamask_ocn)
    CASE DEFAULT
      CALL tethys_init_trip(nlat_ocn,nlon_ocn,landseamask_ocn,lats_ocn,lons_ocn)
    END SELECT

    RETURN

  END SUBROUTINE initialise_tethys

!###################################################
!
!###################################################
  SUBROUTINE tethys_init_trip(nlat_ocn,nlon_ocn,landseamask_ocn,lats_ocn,lons_ocn)

    IMPLICIT NONE

    !=======================================   
    ! Variables with intent in
    !=======================================
    INTEGER,INTENT(in) :: nlat_ocn,nlon_ocn
    INTEGER,INTENT(in),DIMENSION(nlon_ocn,nlat_ocn) :: landseamask_ocn
    REAL,INTENT(in),DIMENSION(nlat_ocn,2) :: lats_ocn
    REAL,INTENT(in),DIMENSION(nlon_ocn,2) :: lons_ocn
    REAL,DIMENSION(0:nlon_ocn+1,2) :: lons_ocn_halo

    !=======================================
    ! TRIP information
    !=======================================
    INTEGER,PARAMETER :: nlat_trip = 180
    INTEGER,PARAMETER :: nlon_trip = 360
    REAL,DIMENSION(nlon_trip) :: lons_trip
    REAL,DIMENSION(nlat_trip) :: lats_trip
    REAL,DIMENSION(0:nlon_trip+1,0:nlat_trip+1) :: outpts_trip
    INTEGER,PARAMETER,DIMENSION(8) :: di = (/ 0, 1, 1, 1, 0,-1,-1,-1/)
    INTEGER,PARAMETER,DIMENSION(8) :: dj = (/ 1, 1, 0,-1,-1,-1, 0, 1/)
    INTEGER :: lon_adjust     !Number x-boxes to shift TRIP data by to match input landsea mask
    REAL :: lonmin_ocn        !Minimum longitude of ocean grid

    !=======================================
    ! Loop counters and temporary indices
    !=======================================
    INTEGER :: i,j,n,nout_ocn,i2,j2,ifin,jfin,d
    INTEGER,DIMENSION(1) :: imn,imx,jmn,jmx,m
    INTEGER :: this_ind, this_i, this_j, iexit,count,nout_trip
    INTEGER :: j1, j2

    !=======================================
    ! Work variables for the mapping
    !=======================================
    INTEGER,DIMENSION(0:nlon_ocn+1,0:nlat_ocn+1) :: ocn_landsea,ocn_outflow_indices,ocn_runoff_dest
    INTEGER,DIMENSION(0:nlon_trip+1,0:nlat_trip+1) :: trip_outflow_indices,trip_runoff_dest,trip_runoff_ocnindices
    INTEGER,DIMENSION(:),ALLOCATABLE :: trip2ocn_mapping
    INTEGER,DIMENSION(:,:),ALLOCATABLE :: ocn_ij_outflow
    INTEGER,DIMENSION(4) :: adjwet
    REAL,DIMENSION(4) :: dbound
    INTEGER :: mode

    nlato = nlat_ocn
    nlono = nlon_ocn
    nout_lnd = COUNT(MASK=(landseamask_ocn>=kmax))
    print*,'nout_lnd =',nout_lnd
    ALLOCATE(ij_lando(nout_lnd,nxy))
    ALLOCATE(ij_runoff(nout_lnd,nxy))

    !======================================================
    ! 1) Identify valid outflow points on ocean grid
    !======================================================

    !Setup halo to make searching easier
    ocn_landsea(1:nlono,1:nlato)   = landseamask_ocn(:,:)
    ocn_landsea(1:nlono,0)         = kmax-1
    ocn_landsea(1:nlono,nlato+1)   = kmax-1
    ocn_landsea(0,0:nlato+1)       = ocn_landsea(nlono,0:nlato+1)
    ocn_landsea(nlono+1,0:nlato+1) = ocn_landsea(1,0:nlato+1)

    ocn_outflow_indices(:,:) = 0
    nout_ocn=0
    n=0
    DO j=1,nlato
      DO i=1,nlono
        IF(ocn_landsea(i,j)<kmax) THEN
          IF(ocn_landsea(i+1,j)>=kmax &    !East
         .or.ocn_landsea(i-1,j)>=kmax &    !West
         .or.ocn_landsea(i,j+1)>=kmax &    !North
         .or.ocn_landsea(i,j-1)>=kmax &    !South
         .or.ocn_landsea(i+1,j+1)>=kmax &  !NE  
         .or.ocn_landsea(i-1,j+1)>=kmax &  !NW
         .or.ocn_landsea(i+1,j-1)>=kmax &  !SE
         .or.ocn_landsea(i-1,j-1)>=kmax &  !SW
            ) THEN
            nout_ocn=nout_ocn+1
            ocn_outflow_indices(i,j)=nout_ocn
          ENDIF
        ELSE IF(ocn_landsea(i,j)>=kmax) THEN
          n=n+1
          ij_lando(n,1)=i
          ij_lando(n,2)=j
        ENDIF
      ENDDO
    ENDDO

    !Copy cyclic longitude boundaries to halo
    ocn_outflow_indices(0,:)       = ocn_outflow_indices(nlono,:)
    ocn_outflow_indices(nlono+1,:) = ocn_outflow_indices(1,:)

    !Save array indexing valid OCN outflow points
    ALLOCATE(ocn_ij_outflow(nout_ocn,nxy))
    n=0
    DO j=1,nlato
      DO i=1,nlono
        IF(ocn_outflow_indices(i,j)>0) THEN
          n=n+1
          ocn_ij_outflow(n,1)=i
          ocn_ij_outflow(n,2)=j
        ENDIF
      ENDDO
    ENDDO
!    print("(38I4)"),ocn_outflow_indices

    !======================================================
    ! 2) Identify valid outflow points on TRIP 1x1 degree grid.
    !    Don't reject any outflow points at this stage
    !======================================================
     DO i=1,nlon_trip
       lons_trip(i) = REAL(i)-0.5
     ENDDO
     DO j=1,nlat_trip
       lats_trip(j) = -90.5 + REAL(j)
     ENDDO

    outpts_trip(:,:)=0
    OPEN(55,file="trip.bin",access="DIRECT",recl=4*nlon_trip,form="UNFORMATTED")
    DO j=nlat_trip,1,-1
      READ(55,rec=nlat_trip-j+1) (outpts_trip(i,j),i=1,nlon_trip)
    ENDDO
    CLOSE(55)

    !Calculate if a longitude shift in the TRIP data is needed to match the input landsea mask
    lonmin_ocn = MINVAL(lons_ocn)
    lon_adjust = -(NINT((0.0-lonmin_ocn)/1.0))
    outpts_trip(1:nlon_trip,1:nlat_trip) = CSHIFT(outpts_trip(1:nlon_trip,1:nlat_trip),lon_adjust,1)
    lons_trip(:) = CSHIFT(lons_trip(:),lon_adjust)
    WHERE(lons_trip>lons_trip(nlon_trip)) lons_trip = lons_trip - 360.0
    print*,'ADJUST %%',lonmin_ocn,lon_adjust

    !Copy ocean grid longitudes to another array including "halo"
    lons_ocn_halo(1:nlon_ocn,:) = lons_ocn(:,:)
    lons_ocn_halo(0,:)          = lons_ocn_halo(1,:)
    lons_ocn_halo(nlon_ocn+1,:) = lons_ocn_halo(nlon_ocn,:)

    !Copy cyclic boundaries
    outpts_trip(0,:)           = outpts_trip(nlon_trip,:)
    outpts_trip(nlon_trip+1,:) = outpts_trip(1,:)

    nout_trip=0
    trip_outflow_indices(:,:)=0
    DO j=1,nlat_trip
      DO i=1,nlon_trip
        IF(outpts_trip(i,j)==9.0) THEN         
          nout_trip=nout_trip+1
          trip_outflow_indices(i,j)=nout_trip
        ENDIF
      ENDDO
    ENDDO

    print*,'nout_trip',nout_trip
!    print("(20I5)"),trip_outflow_indices(1:20,:)
    write(77,"(360I5)") trip_outflow_indices(1:nlon_trip,1:nlat_trip)

    !======================================================
    ! 3) Calculate destinations of each land point indexed by 
    !    numbers in trip_outflow_indices()
    !======================================================
    trip_runoff_dest(:,:)=0
    DO j=1,nlat_trip
      DO i=1,nlon_trip
        ! Operate on TRIP land points only
        IF(outpts_trip(i,j)>0.0) THEN
          this_ind=INT(outpts_trip(i,j))
          this_i=i
          this_j=j
          iexit=0
          ! Loop over runoff directions until you reach an outflow point ==9
          DO WHILE(this_ind.ne.9)
            ! Provide get-out for the WHILE loop.
            ! No runoff pathway should be more than 200 gridboxes long
            iexit=iexit+1
            IF(iexit==200) THEN
              PRINT*,">>>> DO WHILE loop got a bit big, exiting"
              STOP
            ENDIF

            ! Step in direction indicated by TRIP pathways.
            !             N
            !          8  1  2
            !        W 7     3 E
            !          6  5  4
            !             S
            this_i=this_i+di(this_ind)
            this_j=this_j+dj(this_ind)

            ! Adjust for cyclic longitude bounds
            IF(this_i==0) this_i=nlon_trip
            IF(this_i==nlon_trip+1) this_i=1

            ! Get flow direction of new point.
            this_ind = INT(outpts_trip(this_i,this_j))
          ENDDO
          trip_runoff_dest(i,j) = trip_outflow_indices(this_i,this_j)
        ENDIF
      ENDDO
    ENDDO

    write(78,"(360I5)") trip_runoff_dest(1:nlon_trip,1:nlat_trip)

    !======================================================
    ! 4) Map TRIP outflow points (trip_outflow_indices) to 
    !   ocean outflow points (ocn_outflow_indices).  This 
    !   is the most important step in making the GENIE runoff
    !   mask.  For each TRIP outflow point satisify one of 
    !   these conditions in this order,
    !    i) TRIP point lies in OCN wet box
    !   ii) Find nearest adjacent OCN wet box N/E/S/W
    !  iii) Find nearest adjacent OCN wet box NW/NE/SE/SW
    !   iv) Apply geographical fix
    !======================================================
    ALLOCATE(trip2ocn_mapping(nout_trip))
    trip2ocn_mapping(:)=-22
 
    n=0
    DO j=1,nlat_trip
      DO i=1,nlon_trip
        ifin=-99
        jfin=-99
        !Operate on TRIP river mouths
        IF(trip_outflow_indices(i,j)>0) THEN
          !Work out  which OCEAN box this TRIP river mouth is in
          DO i2=1,nlono
            IF(lons_trip(i)>=lons_ocn(i2,1).and.lons_trip(i)<lons_ocn(i2,2)) ifin=i2
          ENDDO
          DO j2=1,nlato
            IF(lats_trip(j)>=lats_ocn(j2,1).and.lats_trip(j)<lats_ocn(j2,2)) jfin=j2
          ENDDO
          n=n+1

          ! i) If this is an OCEAN wet point then TRIP river mouths map to that point
          IF(ocn_landsea(ifin,jfin)<kmax.and.ocn_outflow_indices(ifin,jfin)>0) THEN
            trip2ocn_mapping(n) = ocn_outflow_indices(ifin,jfin)

          !If this is an OCEAN dry point then do something more complicated to 
          !map mouth to a wet point
          ELSEIF(ocn_landsea(ifin,jfin)>=kmax) THEN
            adjwet(:)=-99
            dbound(:)=-99.9

            !Work out which boundaries are wet
            adjwet(1)=ocn_outflow_indices(ifin  ,jfin+1) !North 
            adjwet(2)=ocn_outflow_indices(ifin+1,jfin  ) !East
            adjwet(3)=ocn_outflow_indices(ifin  ,jfin-1) !South 
            adjwet(4)=ocn_outflow_indices(ifin-1,jfin  ) !West 

            !Work out distance (degrees) to NESW boundaries
            dbound(1)=ABS(lats_trip(j)-lats_ocn(jfin,2))
            dbound(2)=ABS(lons_trip(i)-lons_ocn(ifin,2))
            dbound(3)=ABS(lats_trip(j)-lats_ocn(jfin,1))
            dbound(4)=ABS(lons_trip(i)-lons_ocn(ifin,1))

            !Find nearest boundary that is wet
            m(1)=0
            DO d=1,4
              m=MINLOC(dbound,MASK=(dbound>=0.0))
              IF(adjwet(m(1))<=0) dbound(m(1))=-99.9
            ENDDO
            
            ! ii) Map TRIP river mouth to that wet point, if there was one.
            IF(adjwet(m(1))>0) THEN
              trip2ocn_mapping(n)=adjwet(m(1)) 
            ELSE 
              adjwet(:)=-99
            
              !Work out which boundaries are wet
              adjwet(1)=ocn_outflow_indices(ifin+1,jfin+1) !NE 
              adjwet(2)=ocn_outflow_indices(ifin-1,jfin+1) !NW
              adjwet(3)=ocn_outflow_indices(ifin+1,jfin-1) !SE 
              adjwet(4)=ocn_outflow_indices(ifin-1,jfin-1) !SW 

              !Work out distance (degrees) to NESW boundaries
              dbound(1)=SQRT(ABS(lats_trip(j)-lats_ocn(jfin,2))**2+ABS(lons_trip(i)-lons_ocn(ifin,2))**2)
              dbound(2)=SQRT(ABS(lats_trip(j)-lats_ocn(jfin,2))**2+ABS(lons_trip(i)-lons_ocn(ifin,1))**2)
              dbound(3)=SQRT(ABS(lats_trip(j)-lats_ocn(jfin,1))**2+ABS(lons_trip(i)-lons_ocn(ifin,2))**2)
              dbound(4)=SQRT(ABS(lats_trip(j)-lats_ocn(jfin,1))**2+ABS(lons_trip(i)-lons_ocn(ifin,1))**2)

              !Find nearest boundary that is wet
              m(1)=0
              DO d=1,4
                m=MINLOC(dbound,MASK=(dbound>=0.0))
                IF(adjwet(m(1))<=0) dbound(m(1))=-99.9
              ENDDO

              ! iii) Map TRIP river mouth to that wet point, if there was one.
              IF(adjwet(m(1))>0) THEN
                trip2ocn_mapping(n)=adjwet(m(1)) 
              ELSE
                !============================================================
                ! iv) There's no easy way to get round the fact that certain water 
                ! bodies that attract significant runoff are not resolved.
                ! This is where some geographical nudging takes place to make
                ! sure the runoff gets to somewhere like the right place.  In
                ! the case of endorheic regions we create special indices so
                ! the water is accounted for in the simulation.  The water
                ! will find its way into the system through evaporation.
                !============================================================
                CALL geofix(lats_trip(j),lons_trip(i),trip2ocn_mapping(n))
                if(trip2ocn_mapping(n)==999 .or. trip2ocn_mapping(n)==-22) print*,lats_trip(j),lons_trip(i),trip2ocn_mapping(n)
              ENDIF !end (iii) attempt
            ENDIF !end (ii) attempt
          ENDIF !end (i) attempt
!          if(trip2ocn_mapping(n)==-22) print*,'ZERO MAPPING at ',i,j
        ENDIF !end TRIP land points
      ENDDO
    ENDDO

    !======================================================
    ! 5) Map all TRIP land points (trip_runoff_dest) to ocean outflow 
    !    points (trip2ocn_mapping)
    !======================================================
    trip_runoff_ocnindices(:,:)=0
    DO j=1,nlat_trip
      DO i=1,nlon_trip
        IF(trip_runoff_dest(i,j)>0) &
           trip_runoff_ocnindices(i,j) = trip2ocn_mapping(trip_runoff_dest(i,j))
      ENDDO
    ENDDO
    write(79,"(360I5)") trip_runoff_ocnindices(1:nlon_trip,1:nlat_trip)

    !======================================================
    ! 6) For each OCEAN dry point, find the dominant (modal)
    !    wet outflow point on TRIP grid (trip_runoff_ocnindices).  
    !    I plan to modify this to pick out the top 3 
    !    destinations by fraction of TRIP landpoints in OCEAN
    !    box instead of the top 1 at present.  This'll make 
    !    the products here ocn_runoff_dest(i,j,3) and ocn_runoff_destfrac(i,j,3)
    !======================================================
    n=0
    ij_runoff(:,:)=0
    ocn_runoff_dest(:,:)=0
    DO j=1,nlato
      DO i=1,nlono
        IF(ocn_landsea(i,j)>=kmax) THEN
          n=n+1
          imn = -99
          imx = -99
          jmn = -99
          jmx = -99

          imn = MINLOC(lons_trip,MASK=(lons_trip>=lons_ocn(i,1)))
          imx = MAXLOC(lons_trip,MASK=(lons_trip<=lons_ocn(i,2)))
          jmn = MINLOC(lats_trip,MASK=(lats_trip>=lats_ocn(j,1).and.lats_trip<lats_ocn(j,2)))
          jmx = MAXLOC(lats_trip,MASK=(lats_trip>=lats_ocn(j,1).and.lats_trip<lats_ocn(j,2)))
          CALL tethys_calc_mode(imx(1)-imn(1),jmx(1)-jmn(1),trip_runoff_ocnindices(imn(1):imx(1),jmn(1):jmx(1)),mode)

          !For the few odd OCN points that contain no land on TRIP grid,
          !include the surrounding OCN points in the search for a mode.
          !Increase search by one ring of OCN gridboxes on each iteration.
          iexit = 0
          print*,'########################################################'
          DO WHILE(mode == -44)
            iexit = iexit + 1
            imn = MINLOC(lons_trip,MASK=(lons_trip>=lons_ocn_halo(i-iexit,1)))
            imx = MAXLOC(lons_trip,MASK=(lons_trip<=lons_ocn_halo(i+iexit,2)))
            j1 = MIN(j+iexit,nlato)
            j2 = MAX(j-iexit,1)
            print*,'While loop =',iexit,i,j,j1,j2

             jmn = MINLOC(lats_trip,MASK=(lats_trip>=lats_ocn(j2,1).and.lats_trip<lats_ocn(j1,2)))
             jmx = MAXLOC(lats_trip,MASK=(lats_trip>=lats_ocn(j2,1).and.lats_trip<lats_ocn(j1,2)))
!bowie Note for now the previous two lines need to be replaced with the next two lines if the 
!bowie land-sea mask runs North-to-South
!            jmn = MINLOC(lats_trip,MASK=(lats_trip>=lats_ocn(j1,1).and.lats_trip<lats_ocn(j2,2)))
!            jmx = MAXLOC(lats_trip,MASK=(lats_trip>=lats_ocn(j1,1).and.lats_trip<lats_ocn(j2,2)))
!             print*,imn(1),imx(1),jmn(1),jmx(1)
            print*,i,'(',lons_ocn_halo(i-iexit,1),lons_ocn_halo(i+iexit,2),')'
            print*,j,'(',lats_ocn(j1,1),lats_ocn(j2,2),')'
            print*,'Search size =',SIZE(trip_runoff_ocnindices(imn(1):imx(1),jmn(1):jmx(1)))
            CALL tethys_calc_mode(imx(1)-imn(1),jmx(1)-jmn(1),trip_runoff_ocnindices(imn(1):imx(1),jmn(1):jmx(1)),mode)
          ENDDO
          ocn_runoff_dest(i,j) = mode
          IF(mode<800) THEN
            ij_runoff(n,:) = ocn_ij_outflow(mode,:)
          ELSE
            ij_runoff(n,1) = 0
            ij_runoff(n,2) = 0
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    print("(32I4)"),ocn_runoff_dest(1:32,:)
    print*
    print("(32I4)"),ocn_runoff_dest(33:64,:)

    DEALLOCATE(trip2ocn_mapping)
    DEALLOCATE(ocn_ij_outflow)

    CALL tethys_ncwrite('runoff_mask_atmos.nc',lats_ocn(:,1),lons_ocn(:,1),landseamask_ocn &
                       ,ocn_runoff_dest(1:nlono,1:nlato))

    RETURN

  END SUBROUTINE tethys_init_trip

!###################################################
! Routine to calculate the mode of an integer array
!###################################################
  SUBROUTINE tethys_calc_mode(nx,ny,inarr,mode)

    IMPLICIT NONE

    INTEGER,INTENT(in) :: nx,ny
    INTEGER,INTENT(in),DIMENSION(nx,ny) :: inarr
    INTEGER,INTENT(out) :: mode

    INTEGER,DIMENSION(nx,ny) :: freq
    INTEGER :: t,i,j,id
    INTEGER,DIMENSION(2) :: im
 
    IF(MAXVAL(inarr).eq.0) THEN
!      print*,'No TRIP land points in this box OCN land point  ',nx,ny,'||',inarr
      mode=-44
      return
    ENDIF

    freq(:,:)=0
    DO i=1,nx
      DO j=1,ny
        !Work out frequency of good runoff points
        IF(inarr(i,j).gt.0 .and. inarr(i,j).lt.800) THEN
          t=inarr(i,j)
          if(t.eq.0) print*,'t=0'
          freq(i,j)=COUNT(MASK=(inarr==t))       
        !Give endorheic regions a nominal frequency so they are disfavoured
        ELSEIF(inarr(i,j).gt.0 .and. inarr(i,j).gt.800 .and. inarr(i,j).lt.999) THEN
          freq(i,j)=1
        ENDIF
      ENDDO
    ENDDO
    im=MAXLOC(freq)
    id=MAXVAL(freq)
    mode=inarr(im(1),im(2))

    IF(mode.eq.0) THEN      
      print*,'Mode is zero!',id,im,nx,ny,'||',inarr,'||',freq
      mode=-33
    ELSEIF(mode.eq.999) THEN   
      print*,'Mode is 999!',id,im,nx,ny,'||',inarr,'||'
      mode=-22
    ENDIF



    !if more than one value tied for mode
    IF(id .ne. COUNT(MASK=(freq==id))) THEN
!      print*,'More than one value tied for mode',id,'||',mode,'||',inarr
    ENDIF

    RETURN

  END SUBROUTINE tethys_calc_mode

!###################################################
! Routine to write out runoff routeing mask to 
! appropriate netCDF file for use at runtime.
!###################################################
  SUBROUTINE tethys_ncwrite(flname,lats_ocn,lons_ocn,landseamask,ocn_runoff_dest)
    IMPLICIT NONE

    !==============================
    ! Incoming variables
    !==============================
    CHARACTER(len=*),INTENT(in)      :: flname
    REAL,INTENT(in),DIMENSION(nlato) :: lats_ocn
    REAL,INTENT(in),DIMENSION(nlono) :: lons_ocn
    INTEGER,INTENT(in),DIMENSION(nlono,nlato) :: landseamask,ocn_runoff_dest

    !==============================
    ! Local work variables
    !==============================
    INTEGER,PARAMETER :: nmaxdims = 4                      !Maximum number of dimensions a single variable can have
    INTEGER,PARAMETER :: nall     = 7                      !Total number of variables
    INTEGER,PARAMETER :: nfiles   = 1                      !Number of output file types (e.g. monthly, annual, decadal)
    INTEGER           :: ncid,ndim,nvar                    !File id/number of dimensions/number of output variables
    INTEGER,DIMENSION(nall) :: natts,nattsvar,vdims,ndims  !
    INTEGER,DIMENSION(nmaxdims,nall) :: vadims             !Dimensions of a single variable
    INTEGER,DIMENSION(nall,nfiles)   :: iddimo, idvaro     !IDs of dimension and variables
    CHARACTER(len=200),DIMENSION(nall,nfiles) :: dimname, varname
    CHARACTER(len=200),DIMENSION(2,nmaxdims,nall) :: attdimname,attvarname

    INTEGER,DIMENSION(nout_lnd) :: indoutflow              !Indices of outflow points 1,2,...,nout_lnd
    INTEGER :: i,j,n                                       !Loop counters
    INTEGER :: loc_dim

    REAL,DIMENSION(nlono,nlato) :: wrk_landsea,wrk_destin
    REAL,DIMENSION(nlono,nlato,nxy) :: wrk_ijroff
    REAL,DIMENSION(nlono) :: lons_ocn_mid

!bowie    REAL,DIMENSION(nout_lnd,nxy):: wrk_ijroff

!    print("(36I4)"),ocn_runoff_dest
!    print*
!    print("(36I3)"),landseamask

    ndim=0
    nvar=0
    !=======================================================
    ! Setup netCDF file
    !=======================================================
    ndim=ndim+1
    dimname(ndim,1)='longitude'
    ndims(ndim)=nlono
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='longitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degrees_east'
    !==================================
    ndim=ndim+1
    dimname(ndim,1)='latitude'
    ndims(ndim)=nlato
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='latitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degrees_north'
    !==================================
    ndim=ndim+1
    dimname(ndim,1)='ij_index'
    ndims(ndim)=nxy
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='ij_index_outflow_points'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='none'
    !==================================
    nvar=nvar+1
    varname(nvar,:)='landseamask'
    vdims(nvar)=2
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='input_landseamask_ocean_grid'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='none'
    !==================================
    nvar=nvar+1
    varname(nvar,:)='destination_number'
    vdims(nvar)=2
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='outflow_destination_number'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='none'
    !==================================
    nvar=nvar+1
    varname(nvar,:)='destination_indices'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('ij_index',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='outflow_destination_index'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='none'

    !=======================================================
    ! Initialise netCDF file and write dimensions
    !=======================================================
    CALL ininc(flname,nmaxdims,ndim,nvar,natts,nattsvar, &
               vdims,vadims,ndims,dimname(:,1),varname(:,1), &
               attdimname,attvarname,ncid,iddimo(:,1),idvaro(:,1))

    DO n=1,nout_lnd
      indoutflow(n)=n
    ENDDO
    lons_ocn_mid(:) = lons_ocn(:) - 0.5*(lons_ocn(1)-lons_ocn(2))

    CALL writedim(ncid,iddimo(1,1),lons_ocn_mid)   !Longitude
    CALL writedim(ncid,iddimo(2,1),lats_ocn)       !Latitude
    CALL writedim(ncid,iddimo(3,1),(/1,2/))        !xy

    !=======================================================
    ! Write data to netCDF file
    !=======================================================
    wrk_landsea(:,:)  = REAL(landseamask(:,:))
    wrk_destin(:,:)   = REAL(ocn_runoff_dest(:,:))

    wrk_ijroff(:,:,:) = 0.0
    n=0
    DO j=1,nlato
      DO i=1,nlono
        IF(landseamask(i,j)>=kmax) THEN
          n=n+1
          wrk_ijroff(i,j,1) = REAL(ij_runoff(n,1))
          wrk_ijroff(i,j,2) = REAL(ij_runoff(n,2))
        ENDIF
      ENDDO
    ENDDO

    CALL writevar(ncid,idvaro(1,1),wrk_landsea)
    CALL writevar(ncid,idvaro(2,1),wrk_destin)
    CALL writevar(ncid,idvaro(3,1),wrk_ijroff)

    !=======================================================
    ! Close netCDF file
    !=======================================================
    CALL closenc(ncid)

    RETURN

  END SUBROUTINE tethys_ncwrite

!###################################################
! This routine calculates the runoff mask that was 
! originally in goldstein/EMBM.  This routine should
! replicate readroff.f in genie-embm.
!###################################################
  SUBROUTINE tethys_init_goldstein(nlat_ocn,nlon_ocn,lats_ocn,lons_ocn,landseamask_ocn)
    IMPLICIT NONE

    !Variables in
    INTEGER,INTENT(in) :: nlat_ocn,nlon_ocn
    REAL,INTENT(in),DIMENSION(nlat_ocn,2) :: lats_ocn
    REAL,INTENT(in),DIMENSION(nlon_ocn,2) :: lons_ocn
    INTEGER,INTENT(in),DIMENSION(nlon_ocn,nlat_ocn) :: landseamask_ocn

    INTEGER,DIMENSION(nlon_ocn,nlat_ocn) :: ocn_runoff_dest

    !Direction indicators in worbe file
    INTEGER,PARAMETER :: iroe=91
    INTEGER,PARAMETER :: iros=92
    INTEGER,PARAMETER :: irow=93
    INTEGER,PARAMETER :: iron=94

    !Loop counters
    INTEGER :: n,i,j,loop

    !Setup output array sizes
    nlato = nlat_ocn
    nlono = nlon_ocn
    nout_lnd = COUNT(MASK=(landseamask_ocn>=kmax))
    print*,'nout_lnd =',nout_lnd
    ALLOCATE(ij_lando(nout_lnd,2))
    ALLOCATE(ij_runoff(nout_lnd,2))
    
    n=0
    DO j=1,nlato
      DO i=1,nlono
        IF(landseamask_ocn(i,j)>=kmax) THEN
          n=n+1
          ij_lando(n,1) =i
          ij_lando(n,2) =j
          ij_runoff(n,1)=i
          ij_runoff(n,2)=j
          loop=0
          DO WHILE(landseamask_ocn(ij_runoff(n,1),ij_runoff(n,2))>=kmax)
            !If this is a dry point, then take one step in appropriate direction
            !           94
            !           N
            !      93 W   E 91
            !           S
            !           92
            IF(landseamask_ocn(ij_runoff(n,1),ij_runoff(n,2)).EQ.iroe)THEN
              ij_runoff(n,1) = ij_runoff(n,1) + 1
            ELSE IF(landseamask_ocn(ij_runoff(n,1),ij_runoff(n,2)).EQ.iros)THEN
              ij_runoff(n,2) = ij_runoff(n,2) - 1
            ELSE IF(landseamask_ocn(ij_runoff(n,1),ij_runoff(n,2)).EQ.irow)THEN
              ij_runoff(n,1) = ij_runoff(n,1) - 1
            ELSE IF(landseamask_ocn(ij_runoff(n,1),ij_runoff(n,2)).EQ.iron)THEN
              ij_runoff(n,2) = ij_runoff(n,2) + 1
            ENDIF

            !Adjust for periodic longitude boundaries
            IF(ij_runoff(n,1).EQ.nlono+1)THEN
              ij_runoff(n,1) = 1
            ELSEIF(ij_runoff(n,1).EQ.0)THEN
              ij_runoff(n,1) = nlono
            ENDIF

            !Avoid infinite WHILE loops
            loop=loop+1
            IF(loop.GT.100000)THEN
              PRINT*,'There is a problem calculating runoff'
              PRINT*,'Located at landseamask_ocn(',i,',',j,')'
              PRINT*,'landseamask_ocn(',i,',',j,') = ',landseamask_ocn(i,j)
              PRINT*,'ij_runoff(n,1) = ',ij_runoff(n,1)
              PRINT*,'ij_runoff(n,2) = ',ij_runoff(n,2)
              PRINT*,'landseamask_ocn(ij_runoff(n,1),ij_runoff(n,2)) = ' &
                    ,landseamask_ocn(ij_runoff(n,1),ij_runoff(n,1)) &
                    ,' (kmax = ',kmax,')'
              STOP 'LAND %% Problem calculating runoff in tethys_init_goldstein()'
            ENDIF
          ENDDO !End of DO WHILE
        ENDIF !End of dry points
      ENDDO
    ENDDO

    ocn_runoff_dest(:,:) = -99999.0
    CALL tethys_ncwrite('runoff_mask_gold36x36_orig.nc',lats_ocn(:,1),lons_ocn(:,1),landseamask_ocn &
                       ,ocn_runoff_dest)

    RETURN
  END SUBROUTINE tethys_init_goldstein

END MODULE make_runoff_map

!#######################################################################################
! Tethys was the mother of the river gods in Greek Mythology:
!
! "Tethys bore to Okeanos the whirling rivers,
! Neilos and Alpheios and deep-eddying Eridanos,
! Strymon and Maiandros and fair-flowing Istros,
! Phasis and Rhesos and Acheloios of the silver swirls,
! Nessos, Rhodios, Heptaporos, and Haliakmon,
! Grenikos, Aisepos, and divine Simoeis,
! Peneios, Hermos, and fair-flowing Kaikos,
! great Sangarios, Ladon, and Parthenios,
! Euenos, Ardeskos, and divine Skamandros." 
!
!   From Hesiod's "Theogony" 
! 
!#######################################################################################
