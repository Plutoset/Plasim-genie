c     This routine is quite igcm/embm-specific and should be edited. DJL.
c     Its purpose is to make a mask, interpmask_atm, which is 1 where
c       we can safely interpolate the atmospheric grid to the ocean grid
c       without losing any data due to differing land-sea masks between
c       the two grids or due to sea-ice interception.
c     Also interpmask_ocn is the same but in reverse.

      subroutine ini_weights(
     &       aboxedge1_lon_atm,aboxedge1_lat_atm,
     &       aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     &       ilandmask1_atm,ilandmask1_ocn,
     &       interpmask_atm,weighttot_atm,
     &       interpmask_ocn,weighttot_ocn)

      use genie_control
      use weights

      implicit none

      real
     &    aboxedge1_lon_atm(ilon1_atm+1),
     &    aboxedge1_lon_ocn(ilon1_ocn+1),
     &    aboxedge1_lat_atm(ilat1_atm+1),
     &    aboxedge1_lat_ocn(ilat1_ocn+1)

      integer 
     &    ilandmask1_atm(ilon1_atm,ilat1_atm),
     &    ilandmask1_ocn(ilon1_ocn,ilat1_ocn)

      real interpmask_atm(ilon1_atm,ilat1_atm)
      real interpmask_ocn(ilon1_ocn,ilat1_ocn)

      real weightcheck
      integer i,j
      real pi
      parameter(pi=3.141592654)
      integer ifail
      integer 
     &    iwork1_atm(ilon1_atm,ilat1_atm),
     &    iwork1_ocn(ilon1_ocn,ilat1_ocn)

      real weighttot_atm
      real weighttot_ocn

      integer grid_check

c     plumin is for the grid areas
      real plumin


c     grid_check is set to 1 later if the grids differ 
      grid_check=0

c     First of all, make a weight field for the atmosphere....
      if (aboxedge1_lat_atm(1).gt.aboxedge1_lat_atm(ilat1_atm+1)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0     
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          weight_atm(i,j)=plumin*(sin(aboxedge1_lat_atm(j)*2*pi/360.)-
     &              sin(aboxedge1_lat_atm(j+1)*2*pi/360.))*
     &              (aboxedge1_lon_atm(i+1)/360.-
     &              aboxedge1_lon_atm(i)/360.)/2.
          weightcheck=weightcheck+weight_atm(i,j)
        enddo
      enddo
      print*,'Check for weightings atm from ini_weights = ',weightcheck

c     ....then the ocean
      if (aboxedge1_lat_ocn(1).gt.aboxedge1_lat_ocn(ilat1_ocn+1)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0     
      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          weight_ocn(i,j)=plumin*(sin(aboxedge1_lat_ocn(j)*2*pi/360.)-
     &              sin(aboxedge1_lat_ocn(j+1)*2*pi/360.))*
     &              (aboxedge1_lon_ocn(i+1)/360.-
     &              aboxedge1_lon_ocn(i)/360.)/2.
          weightcheck=weightcheck+weight_ocn(i,j)
        enddo
      enddo
      print*,'Check for weightings ocn from ini_weights = ',weightcheck


c     WE WANT TO MAKE AN ARRAY, INTERPMASK_ATM, WHICH ALWAYS INTERPOLATES 
C       ONTO THE OCEAN GRID WITHOUT ANY LOSSES, I.E. IT IS ZERO ON OR NEAR 
C       LAND AND SEAICE.

c     First, interpolate land-sea mask from ocean to atmos grid, to form 
c       interpmask_atm

c     check to see if the two girds are actually the same.....
c     If they differ, set grid_check=1

      if ((ilon1_atm.ne.ilon1_ocn).or.(ilat1_atm.ne.ilat1_ocn)) then
        grid_check=1
        print*,'size of atm and ocn grids differs: ',
     :               ilon1_atm,ilon1_ocn,
     :               ilat1_atm,ilat1_ocn
      else
        do i=1,ilon1_atm+1
          if (aboxedge1_lon_atm(i)-aboxedge1_lon_ocn(i).gt.1e-4) then
            grid_check=1
            print*,'lons differ:',
     :             i,aboxedge1_lon_atm(i),aboxedge1_lon_ocn(i)
          endif
        enddo
        do j=1,ilat1_atm+1
          if (aboxedge1_lat_atm(j)-aboxedge1_lat_ocn(j).gt.1e-4) then
            grid_check=1
            print*,'lats differ:',
     :             j,aboxedge1_lat_atm(j),aboxedge1_lat_ocn(j)
          endif
        enddo
      endif
        
      print*,'grid_check=0 if same ocn and atm grids'
      print*,'grid_check=1 if different ocn and atm grids'
      print*,'grid_check= ',grid_check


c     This is for if the atm and ocn grids differ.....
      if (grid_check.eq.1) then

      iwork1_ocn(:,:)=1.0
      iwork1_atm(:,:)=1.0

      call awi_genie(ilon1_ocn,ilon1_ocn,aboxedge1_lon_ocn,
     :             ilat1_ocn,aboxedge1_lat_ocn,real(1-ilandmask1_ocn),
     :             iwork1_ocn,
     :             ilon1_atm,ilon1_atm,aboxedge1_lon_atm,
     :             ilat1_atm,aboxedge1_lat_atm,interpmask_atm,
     :             iwork1_atm,
     :             ifail)
      if (ifail.ne.0) then
        print*,'Interp_to_ocn: Error in ini_weights ',ifail
        stop 1
      end if

c     This next bit is effectively a mask for seaice.  
c     May have to be bigger then this for the LGM.
c     THIS IS VERY IGCM SPECIFIC AND SHOULD BE CHANGED DJL.
      interpmask_atm(:,1:11)=0.0
      interpmask_atm(:,22:32)=0.0
c     Make sure it's zero where the atmos grid is zero.
      interpmask_atm(:,:)=interpmask_atm(:,:)*(1.0-ilandmask1_atm(:,:))
c     Take away the bits which are not equal to one.
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (interpmask_atm(i,j).ne.1.0) interpmask_atm(i,j)=0.0
        enddo
      enddo

c     However, now that this is interpolated onto the ocean grid, 
c     we still need to reduce its size even further.

      call extrap(ilon1_atm,ilat1_atm,interpmask_atm,
     :          1-ilandmask1_atm,iwork1_atm,1)


c     This is for if the atm and ocn grids are the same.....
      else

      interpmask_atm(:,:)=1.0-real(ilandmask1_atm(:,:))
      
c     This bit gets rid of interpmask if we are at a higher latitiude
c       then +-50.
      do j=1,ilat1_atm
        if ( ( (aboxedge1_lat_atm(j)+aboxedge1_lat_atm(j+1))/2.0 
     :                    .gt. 50.0) .or.
     :       ( (aboxedge1_lat_atm(j)+aboxedge1_lat_atm(j+1))/2.0 
     :                    .lt. -50.0) ) then
          interpmask_atm(:,j)=0
        endif
      enddo


      endif     


c     For now, set the interpmask_ocn equal to the land-sea mask.
      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          interpmask_ocn(i,j)=1-ilandmask1_ocn(i,j)
        enddo
      enddo      

c     Now, interpmask_atm is 1.0 only where atm and ocn have an ocean point,
c       and only in subtropics and tropics.

c     now find the relative weight of the non-masked area:
      weighttot_atm=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          weighttot_atm=weighttot_atm+
     &        (weight_atm(i,j)*interpmask_atm(i,j))
        enddo
      enddo
      print*,'total atmos weight in weights is ',weighttot_atm

      weighttot_ocn=0.0
      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          weighttot_ocn=weighttot_ocn+
     &        (weight_ocn(i,j)*interpmask_ocn(i,j))
        enddo
      enddo
      print*,'total ocean weight in weights is ',weighttot_ocn

      return
      end
