c
c     NEED TO PUT THE LAST ARGUMENT TO EXTRAP INTO GENIE, RATHER 
c     THAN HARD-WIRED.  ALSO NEED TO CHECK THE HARDWIRED VALUES.
c
c     AY (22/07/04) and (08/09/04) :
c     References to alon2_atm, alon2_ocn, etc. removed as unused.
c     Users should feed in correct grids to be interpolated from/to
c     when subroutine is called.
c
c     AY (09/09/04) :
c     Added itype -4 for ocean --> atmosphere regriding with landmask
c     and conservation
c
      subroutine interp_ocn_atm(aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                         aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                         ilandmask1_atm,ilandmask1_ocn,
     :                         iwork1_atm,iwork1_ocn,
     :                         interpmask_atm,weighttot_atm,
     :                         interpmask_ocn,weighttot_ocn,
     :                         data_atm,data_ocn,
     :                         itype)
c
c     Added netcdf stuff in case diagnostic output is needed from here.
      use write_netcdf
      use genie_control
      use weights

      implicit none

      real aboxedge1_lon_atm(ilon1_atm+1),aboxedge1_lat_atm(ilat1_atm+1)
      real aboxedge1_lon_ocn(ilon1_ocn+1),aboxedge1_lat_ocn(ilat1_ocn+1)
      integer ilandmask1_atm(ilon1_atm,ilat1_atm),
     :        ilandmask1_ocn(ilon1_ocn,ilat1_ocn)
      integer iwork1_atm(ilon1_atm,ilat1_atm),
     :        iwork1_ocn(ilon1_ocn,ilat1_ocn)
      real interpmask_atm(ilon1_atm,ilat1_atm)
      real interpmask_ocn(ilon1_ocn,ilat1_ocn)
      real weighttot_atm
      real weighttot_ocn
      integer itype
c
      real data_atm(ilon1_atm,ilat1_atm),data_ocn(ilon1_ocn,ilat1_ocn)
      real dummy_atm(ilon1_atm,ilat1_atm)
      real dummy_ocn(ilon1_ocn,ilat1_ocn)
c
      real tot_atm,tot_ocn
c
      integer i,j,ifail
c
      if (itype.eq.1) then
c
c     This is for an atmosphere --> ocean interpolation....
c       with NO land-sea mask
c
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                iwork1_atm(i,j)=1.0
             enddo
          enddo
c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                iwork1_ocn(i,j)=1.0
             enddo
          enddo
c
          call awi_genie(ilon1_atm,ilon1_atm,aboxedge1_lon_atm,
     :             ilat1_atm,aboxedge1_lat_atm,data_atm,iwork1_atm,
     :             ilon1_ocn,ilon1_ocn,aboxedge1_lon_ocn,
     :             ilat1_ocn,aboxedge1_lat_ocn,data_ocn,iwork1_ocn,
     :             ifail)
c
          if (ifail.ne.0) then
             print*,'Interp_to_ocn: Error in awi_genie, itype 1 ',ifail
             stop 1
          endif
c
          print*,'this interp is not used at present....type 1'
          stop

      else if (itype.eq.2) then
c
c     This is for an atmosphere --> ocean interpolation....
c       with a land-sea mask
c
c     FIRST OF ALL, CHECK IF WE NEED TO INTERPOLATE AT ALL
      if ((ilon1_atm.eq.ilon1_ocn).and.(ilat1_atm.eq.ilat1_ocn)) then

          do j=1,ilat1_atm
            do i=1,ilon1_atm
      data_ocn(i,j)=data_atm(i,ilat1_atm+1-j)
            enddo
          enddo

      else

          do j=1,ilat1_atm
             do i=1,ilon1_atm
                dummy_atm(i,j)=data_atm(i,j)
             enddo
          enddo 
c
          if ( (ilon1_ocn.eq.36).and.(ilat1_ocn.eq.36) ) then
          call extrap(ilon1_atm,ilat1_atm,dummy_atm,
     :                ilandmask1_atm,iwork1_atm,1)
          elseif ( (ilon1_ocn.eq.72).and.(ilat1_ocn.eq.72) ) then
          call extrap(ilon1_atm,ilat1_atm,dummy_atm,
     :                ilandmask1_atm,iwork1_atm,2)
          else
          print*,'EXTRAP FACTOR NOT DEFINED IN INTERP_OCN_ATM'
          stop
          endif

c
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                iwork1_atm(i,j)=1-iwork1_atm(i,j)
             enddo
          enddo 
c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                iwork1_ocn(i,j)=1-ilandmask1_ocn(i,j)
             enddo
          enddo 
c
          call awi_genie(ilon1_atm,ilon1_atm,aboxedge1_lon_atm,
     :             ilat1_atm,aboxedge1_lat_atm,dummy_atm,iwork1_atm,
     :             ilon1_ocn,ilon1_ocn,aboxedge1_lon_ocn,
     :             ilat1_ocn,aboxedge1_lat_ocn,data_ocn,iwork1_ocn,
     :             ifail)
c 
         if (ifail.ne.0) then
             print*,'Interp_to_ocn: Error in awi_genie, itype 2 ',ifail
             stop 1
          endif

      endif

      else if (itype.eq.3) then
c
          call awi_genie(ilon1_atm,ilon1_atm,aboxedge1_lon_atm,
     :             ilat1_atm,aboxedge1_lat_atm,data_atm,ilandmask1_atm,
     :             ilon1_ocn,ilon1_ocn,aboxedge1_lon_ocn,
     :             ilat1_ocn,aboxedge1_lat_ocn,data_ocn,ilandmask1_ocn,
     :             ifail)
c 
         if (ifail.ne.0) then
             print*,'Interp_to_ocn: Error in awi_genie, itype 3 ',ifail
             stop 1
          endif
c
          print*,'this interp is not used at present....type 3'
          stop

      else if (itype.eq.4) then
c
c     This is for an atmosphere --> ocean interpolation....
c       with a land-sea mask, and a correction for conservation.
c
c     FIRST OF ALL, CHECK IF WE NEED TO INTERPOLATE AT ALL
      if ((ilon1_atm.eq.ilon1_ocn).and.(ilat1_atm.eq.ilat1_ocn)) then

          do j=1,ilat1_atm
             do i=1,ilon1_atm
      data_ocn(i,j)=data_atm(i,ilat1_atm+1-j)
             enddo
          enddo

      else

          do j=1,ilat1_atm
             do i=1,ilon1_atm
                dummy_atm(i,j)=data_atm(i,j)
             enddo
          enddo 
c
          if ( (ilon1_ocn.eq.36).and.(ilat1_ocn.eq.36) ) then
          call extrap(ilon1_atm,ilat1_atm,dummy_atm,
     :                ilandmask1_atm,iwork1_atm,1)
          elseif ( (ilon1_ocn.eq.72).and.(ilat1_ocn.eq.72) ) then
          call extrap(ilon1_atm,ilat1_atm,dummy_atm,
     :                ilandmask1_atm,iwork1_atm,2)
          else
          print*,'EXTRAP FACTOR NOT DEFINED IN INTERP_OCN_ATM'
          stop
          endif
c
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                iwork1_atm(i,j)=1-iwork1_atm(i,j)
             enddo
          enddo 
c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                iwork1_ocn(i,j)=1-ilandmask1_ocn(i,j)
             enddo
          enddo 
c
          call awi_genie(ilon1_atm,ilon1_atm,aboxedge1_lon_atm,
     :             ilat1_atm,aboxedge1_lat_atm,dummy_atm,iwork1_atm,
     :             ilon1_ocn,ilon1_ocn,aboxedge1_lon_ocn,
     :             ilat1_ocn,aboxedge1_lat_ocn,data_ocn,iwork1_ocn,
     :             ifail)
c
          if (ifail.ne.0) then
             print*,'Interp_to_ocn: Error in awi_genie, itype 4 ',ifail
             stop 1
          endif
c
c         Calculate the global avg value on atmos grid.....
c
          tot_atm=0.0
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                tot_atm=tot_atm+data_atm(i,j)*weight_atm(i,j)*
     :       (1-ilandmask1_atm(i,j))
             enddo
          enddo           
c
c         Now calculate the global avg value on ocean grid.....
c
          tot_ocn=0.0
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                tot_ocn=tot_ocn+data_ocn(i,j)*weight_ocn(i,j)*
     :       (1-ilandmask1_ocn(i,j))
             enddo
          enddo     
c
c         Make the correction.....
c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                data_ocn(i,j)=data_ocn(i,j)+(tot_atm-tot_ocn)*
     :          interpmask_ocn(i,j)/weighttot_ocn
             enddo
          enddo
c
      endif

      else if (itype.eq.-1) then
c
c     This is for an ocean --> atmosphere interpolation....
c       with NO land-sea mask.
c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                iwork1_ocn(i,j)=1.0
             enddo
          enddo
c
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                iwork1_atm(i,j)=1.0
             enddo
          enddo
c
          call awi_genie(ilon1_ocn,ilon1_ocn,aboxedge1_lon_ocn,
     :             ilat1_ocn,aboxedge1_lat_ocn,data_ocn,iwork1_ocn,
     :             ilon1_atm,ilon1_atm,aboxedge1_lon_atm,
     :             ilat1_atm,aboxedge1_lat_atm,data_atm,iwork1_atm,
     :             ifail)
c
          if (ifail.ne.0) then
             print*,'Interp_to_ocn: Error in awi_genie, itype -1 ',ifail
             stop 1
          endif
c
          print*,'this interp is not used at present....type -1'
          stop

      else if (itype.eq.-2) then
c
c     FIRST OF ALL, CHECK IF WE NEED TO INTERPOLATE AT ALL
      if ((ilon1_atm.eq.ilon1_ocn).and.(ilat1_atm.eq.ilat1_ocn)) then

          do j=1,ilat1_atm
            do i=1,ilon1_atm
      data_atm(i,j)=data_ocn(i,ilat1_atm+1-j)
            enddo
          enddo

      else

c     This is for an ocean --> atmosphere interpolation....
c       with a land-sea mask interpolation.
c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                dummy_ocn(i,j)=data_ocn(i,j)
             enddo
          enddo 
c

          if ( (ilon1_ocn.eq.36).and.(ilat1_ocn.eq.36) ) then
           call extrap(ilon1_ocn,ilat1_ocn,dummy_ocn,
     :                  ilandmask1_ocn,iwork1_ocn,3)
          elseif ( (ilon1_ocn.eq.72).and.(ilat1_ocn.eq.72) ) then
          call extrap(ilon1_ocn,ilat1_ocn,dummy_ocn,
     :                  ilandmask1_ocn,iwork1_ocn,4)
          else
          print*,'EXTRAP FACTOR NOT DEFINED IN INTERP_OCN_ATM'
          stop
          endif

c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                iwork1_ocn(i,j)=1-iwork1_ocn(i,j)
             enddo
          enddo
c
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                iwork1_atm(i,j)=1-ilandmask1_atm(i,j)
             enddo
          enddo
c
          call awi_genie(ilon1_ocn,ilon1_ocn,aboxedge1_lon_ocn,
     :             ilat1_ocn,aboxedge1_lat_ocn,dummy_ocn,iwork1_ocn,
     :             ilon1_atm,ilon1_atm,aboxedge1_lon_atm,
     :             ilat1_atm,aboxedge1_lat_atm,data_atm,iwork1_atm,
     :             ifail)
c
          if (ifail.ne.0) then
             print*,'Interp_to_ocn: Error in awi_genie, itype -2 ',ifail
             stop 1
          endif

      endif
c
      else if (itype.eq.-4) then

c     FIRST OF ALL, CHECK IF WE NEED TO INTERPOLATE AT ALL
      if ((ilon1_atm.eq.ilon1_ocn).and.(ilat1_atm.eq.ilat1_ocn)) then

          do j=1,ilat1_atm
             do i=1,ilon1_atm
      data_atm(i,j)=data_ocn(i,ilat1_atm+1-j)
             enddo
          enddo

      else
c
c     This is for an ocean --> atmosphere interpolation....
c       with a land-sea mask, and a correction for conservation.
c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                dummy_ocn(i,j)=data_ocn(i,j)
             enddo
          enddo 
c
          if ( (ilon1_ocn.eq.36).and.(ilat1_ocn.eq.36) ) then
           call extrap(ilon1_ocn,ilat1_ocn,dummy_ocn,
     :                  ilandmask1_ocn,iwork1_ocn,3)
          elseif ( (ilon1_ocn.eq.72).and.(ilat1_ocn.eq.72) ) then
          call extrap(ilon1_ocn,ilat1_ocn,dummy_ocn,
     :                  ilandmask1_ocn,iwork1_ocn,4)
          else
          print*,'EXTRAP FACTOR NOT DEFINED IN INTERP_OCN_ATM'
          stop
          endif

c
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                iwork1_ocn(i,j)=1-iwork1_ocn(i,j)
             enddo
          enddo 
c
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                iwork1_atm(i,j)=1-ilandmask1_atm(i,j)
             enddo
          enddo 
c
          call awi_genie(ilon1_ocn,ilon1_ocn,aboxedge1_lon_ocn,
     :             ilat1_ocn,aboxedge1_lat_ocn,dummy_ocn,iwork1_ocn,
     :             ilon1_atm,ilon1_atm,aboxedge1_lon_atm,
     :             ilat1_atm,aboxedge1_lat_atm,data_atm,iwork1_atm,
     :             ifail)
c
          if (ifail.ne.0) then
             print*,'Interp_to_ocn: Error in awi_genie, itype -4 ',ifail
             stop 1
          endif
c
c         Calculate the global avg value on ocean grid.....
c
          tot_ocn=0.0
          do j=1,ilat1_ocn
             do i=1,ilon1_ocn
                tot_ocn=tot_ocn+data_ocn(i,j)*weight_ocn(i,j)*
     :       (1-ilandmask1_ocn(i,j))
             enddo
          enddo           
c
c         Now calculate the global avg value on atmosphere grid.....
c
          tot_atm=0.0
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                tot_atm=tot_atm+data_atm(i,j)*weight_atm(i,j)*
     :       (1-ilandmask1_atm(i,j))
             enddo
          enddo     
c
c         Make the correction.....
c
          do j=1,ilat1_atm
             do i=1,ilon1_atm
                data_atm(i,j)=data_atm(i,j)+(tot_ocn-tot_atm)*
     :          interpmask_atm(i,j)/weighttot_atm
             enddo
          enddo
c
      endif

      else
c
      print*,'Illegal interpolation flag.'  
      print*,'Check genie-main/interp_ocn_atm.f and genie-main/genie.F'
      stop
c
      endif
c
      return
      end
