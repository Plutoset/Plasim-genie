MODULE check_fluxes

  real :: water_flux_atmos = 0.0     ! calculated in igcm
  real :: water_flux_surface1 = 0.0  !
  real :: water_flux_surface2 = 0.0  !

contains

  !! routine to compare water fluxes computed
  !! in several ways
  subroutine compare_water_fluxes

    use averages
    use genie_util, ONLY: check_unit, check_iostat

    implicit none

    logical :: lWaterConserved = .true.  ! is water conserved?
    integer :: ios = 0                   ! able to open o/p file?
    real    :: rRelErr = 0.0             ! relative error between fluxes
    real    :: rTol = 1E-09              ! emprirically derived tolerance

    character(len=200) :: fname          ! o/p filename

    !! Ensure all values have been calculated
    if (water_flux_atmos == 0.0) then
       print*, "ERROR: atmos water flux not calculated"
       print*, "ERROR on line ", __LINE__, " of file ", __FILE__
       stop
    end if

    if (water_flux_surface1 == 0.0) then
       print*, "ERROR: surface water flux 1 not calculated"
       print*, "ERROR on line ", __LINE__, " of file ", __FILE__
       stop
    end if

    if (water_flux_surface2 == 0.0) then
       print*, "ERROR: surface water flux 2 not calculated"
       print*, "ERROR on line ", __LINE__, " of file ", __FILE__
       stop
    end if
    
    !! realative error for surface 1 & 2 (same sign)
    rRelErr = (water_flux_surface1 - water_flux_surface2) / &
         ((water_flux_surface1 + water_flux_surface2)/2)
    if (rRelErr >= rTol) then
       lWaterConserved = .false.
    end if

    !! realative error between atmos and surface1 (diff sign)
    rRelErr = (water_flux_atmos + water_flux_surface1) / &
         abs((water_flux_atmos - water_flux_surface1)/2)
    if (rRelErr >= rTol) then
       lWaterConserved = .false.
    end if

    !! realative error between atmos and surface2 (diff sign)
    rRelErr = (water_flux_atmos + water_flux_surface2) / &
         abs((water_flux_atmos - water_flux_surface2)/2)
    if (rRelErr >= rTol) then
       lWaterConserved = .false.
    end if

    !! write result of comparisons to file
    fname=trim(outputdir_name)//'/check_fluxes.txt'
    call check_unit(43,__LINE__,__FILE__)
    open(UNIT=43,FILE=fname,IOSTAT=ios,FORM="formatted")
    call check_iostat(ios,__LINE__,__FILE__)
    write (UNIT=43,FMT='(L1)',IOSTAT=ios) lWaterConserved
    call check_iostat(ios,__LINE__,__FILE__)
    endfile 43

  end subroutine compare_water_fluxes

  subroutine check_fluxes_atlantic_wrapper

    use genie_global

    implicit none

!     The point of this routine is to 
!     1) Provide a check of the total area of the lats/lons on
!          the atmosphere and ocean grid.
!     2) Sum up the energy balance on both grids and print out the net.

!     IT NEEDS SOME EDITING FOR EMBM/IGCM FLUXES BECAUSE OF MIS_MATCH
!       OF UNITS.  I SHOULD SORT THIS ASPAP.  djl.


!     ****************sort out the global weighting******************

      if (aboxedge1_lat_atm(1).gt.aboxedge1_lat_atm(ilat1_atm)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          weight_atm(i,j)=plumin*(sin(aboxedge1_lat_atm(j)*2*pi/360.)-&
     &              sin(aboxedge1_lat_atm(j+1)*2*pi/360.))*&
     &              (aboxedge1_lon_atm(i+1)/360.-&
     &              aboxedge1_lon_atm(i)/360.)/2.
          weightcheck=weightcheck+weight_atm(i,j)
        enddo
      enddo
!      print*,'Check for weightings from genie atm = ',weightcheck


!     ****************sort out the atlantic weighting******************

      at_mask(:,:)=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
           if ( (alon1_atm(i).gt.290).or.(alon1_atm(i).lt.20) ) &
                at_mask(i,j)=1
           if (alat1_atm(j).lt.-50) &
                at_mask(i,j)=0
           if (alat1_atm(j).gt.65) &
                at_mask(i,j)=1
           if ( (alon1_atm(i).gt.230).and.(alat1_atm(j).gt.50) ) &
                at_mask(i,j)=1
           if ( (alon1_atm(i).gt.265).and.(alat1_atm(j).gt.10) ) &
                at_mask(i,j)=1
           if ( (alon1_atm(i).lt.50).and.(alat1_atm(j).gt.30) ) &
                at_mask(i,j)=1
            if ( (alon1_atm(i).gt.260).and.(alat1_atm(j).gt.20) ) & 
                 at_mask(i,j)=1
           at_mask(i,j)=at_mask(i,j)*(1-ilandmask1_atm(i,j))
        enddo
      enddo

 


!     ****************ATLANTIC WATER TERMS******************

      at1_tmp=0.0
      at2_tmp=0.0
      at3_tmp=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (alat1_atm(j).gt.30) then
            at1_tmp=at1_tmp+weight_atm(i,j)*at_mask(i,j)*&
     &          ( (precip_atm(i,j))+&
     &             surf_evap_atm(i,j)+&
     &             land_runoff_atm(i,j) )
          else if (alat1_atm(j).lt.-30) then
            at3_tmp=at3_tmp+weight_atm(i,j)*at_mask(i,j)*&
     &          ( (precip_atm(i,j))+&
     &             surf_evap_atm(i,j)+&
     &             land_runoff_atm(i,j) )
          else
            at2_tmp=at2_tmp+weight_atm(i,j)*at_mask(i,j)*&
     &          ( (precip_atm(i,j))+&
     &             surf_evap_atm(i,j)+&
     &             land_runoff_atm(i,j) )
          endif
        enddo
      enddo
      at1=at1+at1_tmp
      at2=at2+at2_tmp
      at3=at3+at3_tmp

!     ****************OUTPUT TO SCREEN******************

          if (mod(koverall,kocn_loop).eq.0) then

      print*,'****************************************************'
      print*,'***********BEG CHECK_FLUXES_ATLANTIC****************'

      print*,'*****ATLANTIC FLUXES*****'
            print*,'Northern Sector:',at1*4.0*pi*6370e3*6370e3/&
     &                 (koverall*1e9)
      print*,'Equatorial Sector:',at2*4.0*pi*6370e3*6370e3/&
     &                 (koverall*1e9)
      print*,'Southern Sector:',at3*4.0*pi*6370e3*6370e3/&
     &                 (koverall*1e9)
      print*,'*****               *****'

      print*,'***********END CHECK_FLUXES_ATLANTIC****************'
      print*,'****************************************************'

      

   endif

 end subroutine check_fluxes_atlantic_wrapper

  subroutine check_fluxes_ocn_wrapper
    
  use genie_global
  use genie_control
  
  implicit none

      print*,'***********CHECK_FLUXES_OCN****************'

!     The point of this routine is to 
!     1) Provide a check of the total area of the lats/lons on
!          the atmosphere and ocean grid.
!     2) Sum up the energy balance on both grids and print out the net.

!     IT NEEDS SOME EDITING FOR EMBM/IGCM FLUXES BECAUSE OF MIS_MATCH
!       OF UNITS.  I SHOULD SORT THIS ASPAP.  djl.

      if (aboxedge1_lat_atm(1).gt.aboxedge1_lat_atm(ilat1_atm)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          weight_atm(i,j)=plumin*(sin(aboxedge1_lat_atm(j)*2*pi/360.)-&
     &              sin(aboxedge1_lat_atm(j+1)*2*pi/360.))*&
     &              (aboxedge1_lon_atm(i+1)/360.-&
     &              aboxedge1_lon_atm(i)/360.)/2.
          weightcheck=weightcheck+weight_atm(i,j)
        enddo
      enddo
      print*,'Check for weightings from genie atm = ',weightcheck

      if (aboxedge1_lat_ocn(1).gt.aboxedge1_lat_ocn(ilat1_ocn)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0
      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
!     Negative because latitudes go from -ve to +ve
          weight_ocn(i,j)=plumin*(sin(aboxedge1_lat_ocn(j)*2*pi/360.)-&
     &              sin(aboxedge1_lat_ocn(j+1)*2*pi/360.))*&
     &              (aboxedge1_lon_ocn(i+1)/360.-&
     &              aboxedge1_lon_ocn(i)/360.)/2.
          weightcheck=weightcheck+weight_ocn(i,j)
        enddo
      enddo
      print*,'Check for weightings from genie ocn = ',weightcheck


!     OK, Let's calulate a global mean for each variable.........
!     Need to have the latitude/longitude edges.
!     Energy terms::

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_netsolar_atm_meanocn=escn_netsolar_atm_meanocn+&
     &                 (netsolar_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total netsolar seaice-ocean atm= ',&
     &      escn_netsolar_atm_meanocn


      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            escn_netsolar_ocn=escn_netsolar_ocn+&
     &                 (netsolar_ocn(i,j)*&
     &                 weight_ocn(i,j))
          endif
        enddo
      enddo
      print*,'Total netsolar seaice-ocean ocn= ',escn_netsolar_ocn

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_netlong_atm_meanocn=escn_netlong_atm_meanocn+&
     &                 (netlong_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total netlong seaice-ocean atm= ',escn_netlong_atm_meanocn


      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            escn_netlong_ocn=escn_netlong_ocn&
     &                 +(netlong_ocn(i,j)*&
     &                 weight_ocn(i,j))
          endif
        enddo
      enddo
      print*,'Total netlong seaice-ocean ocn= ',escn_netlong_ocn

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_sensible_atm_meanocn=escn_sensible_atm_meanocn+&
     &                 (sensible_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total sensible seaice-ocean atm= ',&
     &      escn_sensible_atm_meanocn

      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            escn_sensible_ocn=escn_sensible_ocn+&
     &                 (sensible_ocn(i,j)*&
     &                 weight_ocn(i,j))
          endif
        enddo
      enddo
      print*,'Total sensible seaice-ocean ocn= ',escn_sensible_ocn

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_latent_atm_meanocn=escn_latent_atm_meanocn+&
     &                 (latent_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total latent seaice-ocean atm= ',escn_latent_atm_meanocn

      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            escn_latent_ocn=escn_latent_ocn+&
     &                 (latent_ocn(i,j)*&
     &                 weight_ocn(i,j))
          endif
        enddo
      enddo
      print*,'Total latent seaice-ocean ocn= ',escn_latent_ocn

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_conductflux_atm_meanocn=escn_conductflux_atm_meanocn+&
     &                 (conductflux_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total conductflux seaice-ocean atm= ',&
     &                 escn_conductflux_atm_meanocn

      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            escn_conductflux_ocn=escn_conductflux_ocn+&
     &                 (conductflux_ocn(i,j)*&
     &                 weight_ocn(i,j))
          endif
        enddo
      enddo
      print*,'Total conductflux seaice-ocean ocn= ',&
     &                 escn_conductflux_ocn


!     Water terms::

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            wscn_precip_atm_meanocn=wscn_precip_atm_meanocn+&
     &                 (precip_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total precip seaice-ocean atm= ',wscn_precip_atm_meanocn

      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            wscn_precip_ocn=wscn_precip_ocn+&
     &                 (precip_ocn(i,j)*&
     &                 weight_ocn(i,j))

          endif
        enddo
      enddo
      print*,'Total precip seaice-ocean ocn= ',wscn_precip_ocn

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            wscn_evap_atm_meanocn=wscn_evap_atm_meanocn+&
     &                 (evap_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total evap seaice-ocean atm= ',wscn_evap_atm_meanocn

      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            wscn_evap_ocn=wscn_evap_ocn+&
     &                 (evap_ocn(i,j)*&
     &                 weight_ocn(i,j))

          endif
        enddo
      enddo
      print*,'Total evap seaice-ocean ocn= ',wscn_evap_ocn

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            wscn_waterflux_atm_meanocn=wscn_waterflux_atm_meanocn+&
     &                 (waterflux_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total waterflux seaice-ocean atm= ',&
     &                      wscn_waterflux_atm_meanocn

      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            wscn_waterflux_ocn=wscn_waterflux_ocn+&
     &                 (waterflux_ocn(i,j)*&
     &                 weight_ocn(i,j))

          endif
        enddo
      enddo
      print*,'Total waterflux seaice-ocean ocn= ',&
     &                      wscn_waterflux_ocn

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            wscn_runoff_atm_meanocn=wscn_runoff_atm_meanocn+&
     &                 (runoff_atm_meanocn(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total runoff seaice-ocean atm= ',&
     &                      wscn_runoff_atm_meanocn

      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          if (ilandmask1_ocn(i,j).eq.0.0) then
            wscn_runoff_ocn=wscn_runoff_ocn+&
     &                 (runoff_ocn(i,j)*&
     &                 weight_ocn(i,j))

          endif
        enddo
      enddo
      print*,'Total runoff seaice-ocean ocn= ',&
     &                      wscn_runoff_ocn


!     Now calculate the net fluxes and write them to the screen.....

      enet_atm=escn_netsolar_atm_meanocn+escn_netlong_atm_meanocn+&
     &  escn_sensible_atm_meanocn+escn_latent_atm_meanocn+&
     &  escn_conductflux_atm_meanocn
      enetnoconduct_atm=escn_netsolar_atm_meanocn+&
     &  escn_netlong_atm_meanocn+&
     &  escn_sensible_atm_meanocn+escn_latent_atm_meanocn

      print*,'koverall=',koverall
      print*,'Net energy balance on atmospheric grid=',enet_atm
      print*,'Net energy balance on atmospheric grid'//&
     &      '(minus conductflux)=',enetnoconduct_atm
      print*,'Net energy balance on atmospheric grid (integrated)='&
     &         ,enetnoconduct_atm*kocn_loop*genie_timestep

      enet_ocn=escn_netsolar_ocn+escn_netlong_ocn+&
     &  escn_sensible_ocn+escn_latent_ocn+&
     &  escn_conductflux_ocn
      print*,'Net energy balance on oceanic grid=',enet_ocn

      print*,'Energy needed to go to netsolar per timestep= ',&
     &  -1.0*enet_ocn/real(istep_ocn)

      print*,'Total expected (1) change in OCEAN energy content=',&
     &  enet_ocn*4.0*pi*genie_timestep*kocn_loop*6370e3*6370e3

      print*,'Total expected (2) change in OCEAN energy content=',&
     &  enet_atm*4.0*pi*genie_timestep*kocn_loop*6370e3*6370e3

      wnet_atm=wscn_precip_atm_meanocn+wscn_evap_atm_meanocn+&
     &  wscn_waterflux_atm_meanocn+wscn_runoff_atm_meanocn
      print*,'Net water balance on atmospheric grid=',wnet_atm

      wnet_ocn=wscn_precip_ocn+wscn_evap_ocn+&
     &  wscn_waterflux_ocn+wscn_runoff_ocn
      print*,'Net water balance on oceanic grid=',wnet_ocn

      print*,'Water needed (1) to go to precip per timestep= ',&
     &  -1.0*wnet_ocn/real(istep_ocn)

      print*,'Water needed (2) to go to precip per timestep= ',&
     &  -1.0*wnet_atm/real(istep_ocn)

      print*,'Total expected (1) change in OCEAN water content=',&
     &  1.0*wnet_ocn*4.0*pi*genie_timestep*kocn_loop*6370e3*6370e3

      print*,'Total expected (2) change in OCEAN water content=',&
     &  1.0*wnet_atm*4.0*pi*genie_timestep*kocn_loop*6370e3*6370e3

      print*,'Number of timesteps completed = ',real(istep_ocn)

    end subroutine check_fluxes_ocn_wrapper

subroutine check_fluxes_surf_l_wrapper

  use genie_global
  use genie_control

  implicit none

!     The point of this routine is to 
!     1) Provide a check of the total area of the lats/lons on
!          the atmosphere and ocean grid.
!     2) Sum up the energy balance on both grids and print out the net.

!     IT NEEDS SOME EDITING FOR EMBM/IGCM FLUXES BECAUSE OF MIS_MATCH
!       OF UNITS.  I SHOULD SORT THIS ASPAP.  djl.

      if (aboxedge1_lat_atm(1).gt.aboxedge1_lat_atm(ilat1_atm)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          weight_atm(i,j)=plumin*(sin(aboxedge1_lat_atm(j)*2*pi/360.)-&
     &              sin(aboxedge1_lat_atm(j+1)*2*pi/360.))*&
     &              (aboxedge1_lon_atm(i+1)/360.-&
     &              aboxedge1_lon_atm(i)/360.)/2.
          weightcheck=weightcheck+weight_atm(i,j)
        enddo
      enddo
!      print*,'Check for weightings from genie atm = ',weightcheck

 
!     OK, Let's calulate a global mean for each variable.........
!     Need to have the latitude/longitude edges.

!     ****************OCEAN+SEAICE ENERGY TERMS******************




!     ****************LAND WATER TERMS******************
      tmp_check=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.1.0) then
            wscn_precip_atm_meansurf_l=wscn_precip_atm_meansurf_l+&
     &          ((precip_atm(i,j))*&
     &                 weight_atm(i,j))
            tmp_check=tmp_check+&
     &          ((precip_atm(i,j))*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo

      print*,'**precip this timestep surface-land atm**= ',&
     &      tmp_check*genie_timestep*4.0*pi*6370e3*6370e3

      tmp_check=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.1.0) then
            wscn_evap_atm_meansurf_l=wscn_evap_atm_meansurf_l+&
     &          (surf_evap_atm(i,j)*&
     &                 weight_atm(i,j))
            tmp_check=tmp_check+&
     &          (surf_evap_atm(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'**evap this timestep surface-land atm**= ',&
     &      tmp_check*genie_timestep*4.0*pi*6370e3*6370e3


!     Now calculate the net fluxes and write them to the screen.....

      wnet_atm_l=wscn_precip_atm_meansurf_l+wscn_evap_atm_meansurf_l

      print*,'PREC FROM CHECK_FLUX_SURF ON LAND=',+&
     &   wscn_precip_atm_meansurf_l*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'EVAP FROM CHECK_FLUX_SURF ON LAND=',+&
     &   wscn_evap_atm_meansurf_l*genie_timestep*4.0*pi*6370e3*6370e3

   end subroutine check_fluxes_surf_l_wrapper

  subroutine check_fluxes_surf_o_wrapper
    
    use genie_global
    use genie_control
    
    implicit none
    
!     The point of this routine is to 
!     1) Provide a check of the total area of the lats/lons on
!          the atmosphere and ocean grid.
!     2) Sum up the energy balance on both grids and print out the net.

!     IT NEEDS SOME EDITING FOR EMBM/IGCM FLUXES BECAUSE OF MIS_MATCH
!       OF UNITS.  I SHOULD SORT THIS ASPAP.  djl.

      print*,'****************************************************'
      print*,'***********BEG CHECK_FLUXES_SURF****************'


!     ****************sort out the global weighting******************

      if (aboxedge1_lat_atm(1).gt.aboxedge1_lat_atm(ilat1_atm)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          weight_atm(i,j)=plumin*(sin(aboxedge1_lat_atm(j)*2*pi/360.)-&
     &              sin(aboxedge1_lat_atm(j+1)*2*pi/360.))*&
     &              (aboxedge1_lon_atm(i+1)/360.-&
     &              aboxedge1_lon_atm(i)/360.)/2.
          weightcheck=weightcheck+weight_atm(i,j)
        enddo
      enddo
!      print*,'Check for weightings from genie atm = ',weightcheck


!     ****************OCEAN+SEAICE ENERGY TERMS******************

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_netsolar_atm_meansurf=escn_netsolar_atm_meansurf+&
     &                 (netsolar_atm(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total netsolar surface-ocean atm= ',&
     &      escn_netsolar_atm_meansurf*genie_timestep*4.0*pi*6370e3*6370e3

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_netlong_atm_meansurf=escn_netlong_atm_meansurf+&
     &                 (netlong_atm(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total netlong surface-ocean atm= ',&
     &    escn_netlong_atm_meansurf*genie_timestep*4.0*pi*6370e3*6370e3

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_sensible_atm_meansurf=escn_sensible_atm_meansurf+&
     &                 (surf_sensible_atm(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total sensible surface-ocean atm= ',&
     &      escn_sensible_atm_meansurf*genie_timestep*4.0*pi*6370e3*6370e3

      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            escn_latent_atm_meansurf=escn_latent_atm_meansurf+&
     &                 (surf_latent_atm(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'Total latent surface-ocean atm= ',&
     &       escn_latent_atm_meansurf*genie_timestep*4.0*pi*6370e3*6370e3




!     ****************OCEAN AND SEAICE WATER TERMS******************
      tmp_check=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            wscn_precip_atm_meansurf_o=wscn_precip_atm_meansurf_o+&
     &          ((precip_atm(i,j))*&
     &                 weight_atm(i,j))
            tmp_check=tmp_check+&
     &          ((precip_atm(i,j))*&
     &                 weight_atm(i,j))


          endif
        enddo
      enddo
      print*,'****Total precip surface-ocean atm this timestep****= ',&
     &      tmp_check*genie_timestep*4.0*pi*6370e3*6370e3


      tmp_check=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          if (ilandmask1_atm(i,j).eq.0.0) then
            wscn_evap_atm_meansurf_o=wscn_evap_atm_meansurf_o+&
     &          (surf_evap_atm(i,j)*&
     &                 weight_atm(i,j))
            tmp_check=tmp_check+&
     &          (surf_evap_atm(i,j)*&
     &                 weight_atm(i,j))
          endif
        enddo
      enddo
      print*,'****Total evap surface-ocean atm this timestep****= ',&
     &      tmp_check*genie_timestep*4.0*pi*6370e3*6370e3


!     ****************OUTPUT TO SCREEN******************
!     Now calculate the net fluxes and write them to the screen.....

      enet_atm=escn_netsolar_atm_meansurf+escn_netlong_atm_meansurf+&
     &  escn_sensible_atm_meansurf+escn_latent_atm_meansurf

      wnet_atm_o=wscn_precip_atm_meansurf_o+wscn_evap_atm_meansurf_o

      wnet_atm=wnet_atm_o+wnet_atm_l

          if (mod(koverall,kocn_loop).eq.0) then

             water_flux_surface1 = wnet_atm*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net water balance (kg) on atmospheric grid (integrated)='&
     &         ,water_flux_surface1
      print*,'Net water balance (kg) on atmospheric grid (ocean only)='&
     &         ,wnet_atm_o*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net water balance (kg) on atmospheric grid (land only)='&
     &         ,wnet_atm_l*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net precip on atmospheric grid (land only)='&
     &         ,wscn_precip_atm_meansurf_l*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net evap on atmospheric grid (land only)='&
     &         ,wscn_evap_atm_meansurf_l*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net precip on atmospheric grid (ocean only)='&
     &         ,wscn_precip_atm_meansurf_o*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net evap on atmospheric grid (ocean only)='&
     &         ,wscn_evap_atm_meansurf_o*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net precip on atmospheric grid (a+o)='&
     &         ,(wscn_precip_atm_meansurf_l+wscn_precip_atm_meansurf_o)&
     &         *genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net evap on atmospheric grid (a+o)='&
     &         ,(wscn_evap_atm_meansurf_l+wscn_evap_atm_meansurf_o)&
     &         *genie_timestep*4.0*pi*6370e3*6370e3
      print*,'Net energy balance (J) on atmospheric grid (integrated)='&
     &         ,enet_atm*genie_timestep*4.0*pi*6370e3*6370e3
      print*,'koverall=',koverall

      print*,'Change in energy in the ocean:',test_energy_ocean
      print*,'Change in energy in seaice:',test_energy_seaice
      print*,'Total change in energy:',&
     &          test_energy_ocean+test_energy_seaice

!     I NEED TO DEFINE TEST_WATER_SEAICE FOR SLABSEAICE AND FIXEDSEAICE
!     AND FIXEDOCEAN AND SLABOCEAN
      print*,'Change in water in the land:',test_water_land
      print*,'Change in water in the ocean:',test_water_ocean
      print*,'Change in water in seaice:',test_water_seaice
      water_flux_surface2 = test_water_ocean+&
     &      test_water_seaice+test_water_land
      print*,'Total change in water:', water_flux_surface2

      print*,'***********END CHECK_FLUXES_SURF****************'
      print*,'****************************************************'

   endif
   
 end subroutine check_fluxes_surf_o_wrapper

END MODULE check_fluxes
