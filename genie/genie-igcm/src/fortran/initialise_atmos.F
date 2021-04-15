      SUBROUTINE INITIALISE_ATMOS(ilon1_atm,ilat1_atm,
     :     alon1_atm,alat1_atm,alon2_atm,alat2_atm,
     :     aboxedge1_lon_atm,aboxedge2_lon_atm,
     :     aboxedge1_lat_atm,aboxedge2_lat_atm,
     :     ilandmask1_atm,ilandmask2_atm,
     :     land_niter_tim,
     :     ocean_niter_tim,
     :     atmos_dt_tim,
     :     tstar_atm,
     :     surf_salb_atm,albedo_atm,
     :     netsolar_atm,netlong_atm,
     :     precip_atm,
     :     lgraphic,
     :     flag_goldocn,flag_goldsic,
     :     flag_land,
#ifdef USE_mosestriffid 
     :     klnd_loop,lowestlh,
#endif
     :     land_runoff_atm,
#ifdef USE_mosestriffid 
     :     land_tice_ice,
     :     land_fxco2_atm,
#endif
     :     land_salb_atm,
     :     orog_atm,
     :     landicefrac_atm,
     :     landsnowicefrac_atm,
     :     landsnowvegfrac_atm,
     :     landsnowdepth_atm,
     :     landicealbedo_atm,
     :     co2_atm,n2o_atm,ch4_atm,
     :     water_flux_atmos,
     :     glim_covmap,
     :     hrlons_atm,hrlats_atm,
     :     hrlonsedge_atm,hrlatsedge_atm
     :                           )

      implicit none
C     
C     Calls other initialisation routines.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'bats.cmn'
      include 'legau.cmn'
      include 'means.cmn'
      include 'orog_g.cmn'
      include 'physca.cmn'
      include 'outcon.cmn'
      include 'igcm_nlevs.cmn'
      include 'igcm_flux.cmn'
#include "parray.cmn"
#include "highres.cmn"

#include "precision.inc"

c     
      integer ilon1_atm,ilat1_atm
      integer ilandmask1_atm(mg,jgg),ilandmask2_atm(mg,jgg)
c     
      real(rk_in) ::  alon1_atm(mg),alat1_atm(jgg),
     :     alon2_atm(mg),alat2_atm(jgg),
     :     aboxedge1_lon_atm(mg+1),aboxedge2_lon_atm(mg+1),
     :     aboxedge1_lat_atm(jgg+1),aboxedge2_lat_atm(jgg+1)

      real(rk_in) :: tstar_atm(mg,jgg)  

      real(rk_in) :: netsolar_atm(mg,jgg)
      real(rk_in) :: netlong_atm(mg,jgg)


      real(rk_in) :: atmos_lowestlu_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: atmos_lowestlv_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: atmos_lowestlq_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: atmos_lowestlt_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: atmos_lowestlp_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: atmos_lowestlh_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: land_lowestlu_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_lowestlv_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_lowestlq_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_lowestlt_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: ocean_lowestlu_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_lowestlv_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_lowestlq_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_lowestlt_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: atmos_dt_tim
      real(rk_in) :: surfsigma,surfdsigma
      real(rk_in) :: albedo_atm(mg,jgg)

      real(rk_in) :: precip_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: orog_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: co2_atm(ilon1_atm,ilat1_atm),
     :               n2o_atm(ilon1_atm,ilat1_atm),
     :               ch4_atm(ilon1_atm,ilat1_atm)
 
      real(rk_in) :: mass14co2(ilon1_atm,ilat1_atm,nl)
      real(rk_in) :: ddtmass14co2(ilon1_atm,ilat1_atm,nl)
      real(rk_in) :: massair(ilon1_atm,ilat1_atm,nl)
      real(rk_in) :: psigma(nl)

      real :: water_flux_atmos

c     ****************************

c     FOR THE NEW SPLIT BLSURF:
c     This is the loop over the igcm landsurface scheme.
      integer surf_iter_tim
      integer land_niter_tim
      integer ocean_niter_tim

c     I think I can remove this from the output of surflux.....
      real(rk_in) :: land_latentinst_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: land_sensibleinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_stressxinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_stressyinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_evapinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_tstarinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_rough_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_qstar_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_salb_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: land_runoff_atm(ilon1_atm,ilat1_atm)

c     I think I can remove this from the output of surflux.....
      real(rk_in) :: ocean_latentinst_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: ocean_sensibleinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_stressxinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_stressyinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_evapinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_tstarinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_rough_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_qstar_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: ocean_salb_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: surf_tstarinst_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: surf_rough_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: surf_qstar_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: surf_salb_atm(ilon1_atm,ilat1_atm)


      real(rk_in) :: 
     :     surf_latent_atm(ilon1_atm,ilat1_atm),
     :     surf_sensible_atm(ilon1_atm,ilat1_atm),
     :     surf_stressx_atm(ilon1_atm,ilat1_atm),
     :     surf_stressy_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: 
     :     land_latent_atm(ilon1_atm,ilat1_atm),
     :     land_sensible_atm(ilon1_atm,ilat1_atm),
     :     land_stressx_atm(ilon1_atm,ilat1_atm),
     :     land_stressy_atm(ilon1_atm,ilat1_atm),
     :     land_evap_atm(ilon1_atm,ilat1_atm)

      real(rk_in) :: 
     :     ocean_latent_atm(ilon1_atm,ilat1_atm),
     :     ocean_sensible_atm(ilon1_atm,ilat1_atm),
     :     ocean_stressx_atm(ilon1_atm,ilat1_atm),
     :     ocean_stressy_atm(ilon1_atm,ilat1_atm),
     :     ocean_evap_atm(ilon1_atm,ilat1_atm)

c     For energy/water check:
      real(rk_in) :: test_water_land

c     For icesheet:
      real (rk_in) :: landicefrac_atm(mg,jgg)
      real (rk_in) :: landsnowicefrac_atm(mg,jgg)
      real (rk_in) :: landsnowvegfrac_atm(mg,jgg)
      real (rk_in) :: landsnowdepth_atm(mg,jgg)
      real (rk_in) :: landicealbedo_atm(mg,jgg)
      real (rk_in) :: glim_covmap(mg,jgg)


c     These variables must be at the same precision as libutil1
      real(rk_libutil1) alat1(jgg),alat2(jgg),
     :     aboxedge1_lat(jgg+1),aboxedge2_lat(jgg+1)

!****************************************************************
! Hopefully temporary fix of variables needed if you're running 
! with genie-land rather than igcm-land.
!****************************************************************
#ifdef USE_mosestriffid 
      REAL(rk_in),INTENT(in),DIMENSION(ilon1_atm,ilat1_atm) ::
     &     lowestlh
      REAL(rk_in),INTENT(in),DIMENSION(ilon1_atm,ilat1_atm) ::
     &     land_fxco2_atm,land_tice_ice
#endif
      LOGICAL,INTENT(in) :: flag_land
#ifdef USE_mosestriffid 
      INTEGER,INTENT(in) :: klnd_loop
#endif
!****************************************************************
      LOGICAL,INTENT(in) :: flag_goldocn,flag_goldsic


c
      integer i,j,ihem,iof,jj
      real ax
      logical lgraphic
c
      integer iconv_ice
      integer iconv_che

c     here are the boundary-layer variables:
      real(rk_in) :: u10m_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: v10m_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: t2m_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: q2m_atm(ilon1_atm,ilat1_atm)
      real(rk_in) :: rh2m_atm(ilon1_atm,ilat1_atm)

c     For high-res orog:
      real(rk_in) :: hrlons_atm(nhrlon),hrlats_atm(nhrlat)
      real(rk_in) :: hrlonsedge_atm(nhrlon+1),hrlatsedge_atm(nhrlat+1)

c     First of all, sort out the high-res orog:
      ax=360.0/real(nhrlon)
      do i=1,nhrlon
         hrlons_atm(i)=(i-1.0)*ax
         hrlonsedge_atm(i)=(i-0.5)*ax
      end do
      hrlonsedge_atm(nhrlon+1)=(nhrlon+0.5)*ax
      call gwtcnr(hrlats_atm,nhrlat/2)
      call gwtbox(hrlatsedge_atm,nhrlat/2)

c     
c     Initialise some variables to be zero!!
      land_latent_atm(:,:)=0.0
      ocean_latent_atm(:,:)=0.0
      land_sensible_atm(:,:)=0.0
      ocean_sensible_atm(:,:)=0.0
      land_stressx_atm(:,:)=0.0
      ocean_stressx_atm(:,:)=0.0
      land_stressy_atm(:,:)=0.0
      ocean_stressy_atm(:,:)=0.0
      land_evap_atm(:,:)=0.0
      ocean_evap_atm(:,:)=0.0
      land_evapinst_atm(:,:)=0.0
      ocean_evapinst_atm(:,:)=0.0
      land_tstarinst_atm(:,:)=0.0
      ocean_tstarinst_atm(:,:)=0.0
      land_rough_atm(:,:)=0.0
      ocean_rough_atm(:,:)=0.0
      land_qstar_atm(:,:)=0.0
      ocean_qstar_atm(:,:)=0.0
      land_salb_atm(:,:)=0.0
      ocean_salb_atm(:,:)=0.0
      land_lowestlu_atm(:,:)=0.0
      ocean_lowestlu_atm(:,:)=0.0
      land_lowestlv_atm(:,:)=0.0
      ocean_lowestlv_atm(:,:)=0.0
      land_lowestlt_atm(:,:)=0.0
      ocean_lowestlt_atm(:,:)=0.0
      land_lowestlq_atm(:,:)=0.0
      ocean_lowestlq_atm(:,:)=0.0
      temp2meter(:,:)=0.0
      temp2metera(:,:)=0.0
      temp2meterd(:,:,:)=0.0
      t2m_atm(:,:)=0.0

      glim_covmap(:,:)=0.0
c
c
      iconv_ice=1
      iconv_che=1

      psigma(:)=sigma(:)

      lgraphics=lgraphic  
c
      if (ilon1_atm.ne.mg.or.ilat1_atm.ne.jgg) then
         print*,' Model resolution error ',mg,jgg,ilon1_atm,ilat1_atm
         stop 1
      end if
c
      CALL INISET
c     reads some namelists, reads std data from file.
c
      CALL INIGAU
c     sets up some spectral constants
c
      CALL INISI
c     sets up some vertical constants
c
      CALL INIPHYS
c     reads some physical constants
c
      IF (LRESTIJ) THEN
         CALL INIRESIJ
      ELSE
         CALL INIRES
      ENDIF
c     sets up restoration stuff
c
      CALL INISTR
c     reads spectral restart data
c
      CALL INISURF
c     reads gridpoint restart data
c
      do j=1,jg
         do ihem=1,nhem
            iof=mgpp*(ihem-1)
            if (ihem.eq.1) then
               jj=j
            else
               jj=2*jg+1-j
            end if
            do i=1,mg
               gsg(i+iof,j)=orog_atm(i,jj)
               if (ilandmask1_atm(i,jj).eq.1) then
                  iland(i+iof,j)=1
                  ilandmask2_atm(i,jj)=1
               else
                  iland(i+iof,j)=0
                  ilandmask2_atm(i,jj)=0
               end if
            end do
         end do
      end do
c
c     
c     print out land mask
c     
      write(6,*) 'land mask looks like:'
      do j=1,jg
         write (6,'(128I1)')(iland(i,j),i=1,mg)
      enddo
      do j=jg,1,-1
         write (6,'(128I1)')(iland(i+mgpp,j),i=1,mg)
      enddo

      CALL INIQS
c     sets up pqsval
c     
      call ini_checks(doy,kstart,ktotal,itspd,imonth,kits,
     :     idoy,delt,pi2,lrstrt,kounth,kountr)
c     
      if (lmonthly) then
         call inimeans(tmn,mg,jgg,nl)
         call inimeans(umn,mg,jgg,nl)
         call inimeans(spmn,mg,jgg,1)
         call inimeans(mspmn,mg,jgg,1)
         call inimeans(u_pmn,mg,jgg,nl)
         call inimeans(v_pmn,mg,jgg,nl)
         call inimeans(t_pmn,mg,jgg,nl)
         call inimeans(q_pmn,mg,jgg,nl)
         call inimeans(g_pmn,mg,jgg,nl)
         call inimeans(convrain,mg,jgg,1)
         call inimeans(lscalerain,mg,jgg,1)
         call inimeans(tstarmn,mg,jgg,1)
         call inimeans(qstarmn,mg,jgg,1)
         call inimeans(sensheat,mg,jgg,1)
         call inimeans(latentheat,mg,jgg,1)
         call inimeans(netsolar,mg,jgg,1)
         call inimeans(netlong,mg,jgg,1)
         call inimeans(stressx,mg,jgg,1)
         call inimeans(stressy,mg,jgg,1)
         call inimeans(cloud_frac,mg,jgg,5)
      end if
c     
      if (lannual) then
         call inimeans(tmna,mg,jgg,nl)
         call inimeans(umna,mg,jgg,nl)
         call inimeans(spmna,mg,jgg,1)
         call inimeans(mspmna,mg,jgg,1)
         call inimeans(u_pmna,mg,jgg,nl)
         call inimeans(v_pmna,mg,jgg,nl)
         call inimeans(t_pmna,mg,jgg,nl)
         call inimeans(q_pmna,mg,jgg,nl)
         call inimeans(g_pmna,mg,jgg,nl)
         call inimeans(convraina,mg,jgg,1)
         call inimeans(lscaleraina,mg,jgg,1)
         call inimeans(tstarmna,mg,jgg,1)
         call inimeans(qstarmna,mg,jgg,1)
         call inimeans(sensheata,mg,jgg,1)
         call inimeans(latentheata,mg,jgg,1)
         call inimeans(netsolara,mg,jgg,1)
         call inimeans(netlonga,mg,jgg,1)
         call inimeans(stressxa,mg,jgg,1)
         call inimeans(stressya,mg,jgg,1)
         call inimeans(cloud_fraca,mg,jgg,5)
      end if
c     
      if (ldecadal) then
         call inimeans(tmnd,mg,jgg,nl*13)
         call inimeans(umnd,mg,jgg,nl*13)
         call inimeans(u_pmnd,mg,jgg,nl*13)
         call inimeans(v_pmnd,mg,jgg,nl*13)
         call inimeans(t_pmnd,mg,jgg,nl*13)
         call inimeans(q_pmnd,mg,jgg,nl*13)
         call inimeans(g_pmnd,mg,jgg,nl*13)
         call inimeans(spmnd,mg,jgg,1*13)
         call inimeans(mspmnd,mg,jgg,1*13)
         call inimeans(convraind,mg,jgg,1*13)
         call inimeans(lscaleraind,mg,jgg,1*13)
         call inimeans(tstarmnd,mg,jgg,1*13)
         call inimeans(qstarmnd,mg,jgg,1*13)
         call inimeans(sensheatd,mg,jgg,1*13)
         call inimeans(latentheatd,mg,jgg,1*13)
         call inimeans(netsolard,mg,jgg,1*13)
         call inimeans(netlongd,mg,jgg,1*13)
         call inimeans(stressxd,mg,jgg,1*13)
         call inimeans(stressyd,mg,jgg,1*13)
         call inimeans(cloud_fracd,mg,jgg,5*13)
      end if
c     
      if (lnetcdf) then
         if (ldaily) then
           call ini_netcdf(imonth,abs(iyear),sigma,alat,1)
         endif
         if (lmonthly) call ini_netcdf(imonth,abs(iyear),sigma,alat,2)
         if (lannual.and.lannual_restart) 
     :                 call ini_netcdf(imonth,abs(iyear),sigma,alat,3)
         if (ldecadal.and.ldecadal_restart) 
     :                 call ini_netcdf(imonth,abs(iyear),sigma,alat,4)
         if (iyear.lt.0) iyear=-iyear-1
      end if
c     

      ax=360.0/real(mg)
      do i=1,mg
         alon1_atm(i)=(i-1.0)*ax
         alon2_atm(i)=alon1_atm(i)
         aboxedge1_lon_atm(i)=(i-1.5)*ax
         aboxedge2_lon_atm(i)=(i-1.5)*ax
      end do
      aboxedge1_lon_atm(mg+1)=(mg-0.5)*ax
      aboxedge2_lon_atm(mg+1)=(mg-0.5)*ax

      print*,'in initialise_atmos, aboxedge1_lon_atm:'
      print*,aboxedge1_lon_atm      

      print*,'in initialise_atmos, alon1_atm:'
      print*,alon1_atm

      call gwtcnr(alat1,jg)
      call gwtcnr(alat2,jg)
      call gwtbox(aboxedge1_lat,jg)
      call gwtbox(aboxedge2_lat,jg)



      do j=1,jg
         do ihem=1,nhem
            iof=mgpp*(ihem-1)
            if (ihem.eq.1) then
               jj=j
            else
               jj=2*jg+1-j
            end if
            do i=1,mg
               if (iland(i+iof,j).eq.1) then
c                  ilandmask1_atm(i,jj)=1
c                  ilandmask2_atm(i,jj)=1
               else
c                  ilandmask1_atm(i,jj)=0
c                  ilandmask2_atm(i,jj)=0
                  if (.not.lshort) then
c                 This bit to enable clean restarts.
c                 Needed because these variables come from 
c                   the ocean which is initialised AFTER the igcm.
c                 Now moved to write_oceansurf_restart.F
c                  tstar_atm(i,jj)=tstar(i+iof,j)*ct-273.16
c                  albedo_atm(i,jj)=salb(i+iof,j)
                  endif
               end if
            end do
         end do
      end do
c

      call convert_gr_sp_orog(1)
c     initialises spectral orography if required....(1) means 
c       values have changed

c     Put the real into real*(rk_in)
      do j=1,jgg
        alat1_atm(j)=alat1(j)
        alat2_atm(j)=alat2(j)
        aboxedge1_lat_atm(j)=aboxedge1_lat(j)
        aboxedge2_lat_atm(j)=aboxedge2_lat(j)
      enddo
      aboxedge1_lat_atm(jgg+1)=aboxedge1_lat(jgg+1)
      aboxedge2_lat_atm(jgg+1)=aboxedge2_lat(jgg+1)

c     This bit is to pass some timestepping constants and
c       the land-sea mask to the igcm land scheme.

c     The /2. is becvause we want the timestep in genie to be the actual
c       timestep, not the leapfrog timestep.  The *2. is put back in in 
c       the modules which read atmos_dt_tim.
      atmos_dt_tim=delt2/(2.*ww)


      land_niter_tim=itsll
      ocean_niter_tim=itslo

!****************************************************************
! Initialise land-surface scheme
!****************************************************************
      if (flag_land) then
#ifdef USE_mosestriffid 
        call initialise_land(ilandmask1_atm,
     &       atmos_dt_tim,klnd_loop,
     &       alon1_atm,alat1_atm,ilon1_atm,ilat1_atm,
     &       aboxedge1_lon_atm,aboxedge1_lat_atm)
        call initialise_land_extra(netsolar_atm,netlong_atm,precip_atm,
     &       lowestlh)
        print*,'LAND %% Finished initialise_land'
!      elseif (flag_simpleland) then
!        call initialise_simpleland()
!      elseif (flag_fixedland) then
!        call initialise_fixedland(ilon1_atm,ilat1_atm,ilandmask1_atm)
!        print*,'FIXEDLAND %% Finished initialise_fixedland'
#endif
      endif
!****************************************************************

                  call igcm3_adiab(
     :    atmos_lowestlu_atm,atmos_lowestlv_atm,
     :    atmos_lowestlt_atm,atmos_lowestlq_atm,atmos_lowestlp_atm,
     :    atmos_lowestlh_atm,
     :    u10m_atm,v10m_atm,t2m_atm,q2m_atm,rh2m_atm,
     :    orog_atm,
     :    surfsigma,surfdsigma,psigma,iconv_ice,mass14co2,massair
     :                            )

      land_lowestlu_atm(:,:)=atmos_lowestlu_atm(:,:)
      ocean_lowestlu_atm(:,:)=atmos_lowestlu_atm(:,:)
      land_lowestlv_atm(:,:)=atmos_lowestlv_atm(:,:)
      ocean_lowestlv_atm(:,:)=atmos_lowestlv_atm(:,:)
      land_lowestlt_atm(:,:)=atmos_lowestlt_atm(:,:)
      ocean_lowestlt_atm(:,:)=atmos_lowestlt_atm(:,:)
      land_lowestlq_atm(:,:)=atmos_lowestlq_atm(:,:)
      ocean_lowestlq_atm(:,:)=atmos_lowestlq_atm(:,:)

                 do surf_iter_tim=1,land_niter_tim
!****************************************************************
! Call land-surface scheme to initialise surface state/fluxes for
! use in initialisation of IGCM atmos.
!****************************************************************
                   IF(flag_land) THEN
#ifdef USE_mosestriffid 
                  CALL genie_land_surflux(co2_atm,                  !IN
     &         netsolar_atm,netlong_atm,land_lowestlu_atm,          !IN
     &         land_lowestlv_atm,precip_atm,                        !IN
     &         land_lowestlt_atm,land_lowestlq_atm,                 !IN
     &         atmos_lowestlp_atm,lowestlh,                         !IN
     &         land_tstarinst_atm,                                  !OUT
     &         land_salb_atm,land_evap_atm,land_latentinst_atm,     !OUT
     &         land_sensibleinst_atm,land_stressxinst_atm,          !OUT
     &         land_stressyinst_atm,land_rough_atm,land_qstar_atm,  !OUT
     &         land_runoff_atm,land_tice_ice,landicealbedo_atm,     !OUT
     &         land_fxco2_atm,test_water_land)                      !OUT
#endif

                  land_stressx_atm(:,:)  = land_stressxinst_atm(:,:)
                  land_stressy_atm(:,:)  = land_stressyinst_atm(:,:)
                  land_latent_atm(:,:)   = land_latentinst_atm(:,:)
                  land_sensible_atm(:,:) = land_sensibleinst_atm(:,:)
                  land_evapinst_atm(:,:) = land_evap_atm(:,:)
                   ELSE
                  call igcm_land_surflux(0,
     :                 surf_iter_tim,land_niter_tim,
     :                 atmos_dt_tim,
     :                 surfsigma,
     :                 ilandmask1_atm,
     :                 netsolar_atm,netlong_atm,
     :                 precip_atm,
     :                 land_lowestlu_atm,land_lowestlv_atm,
     :                 land_lowestlt_atm,land_lowestlq_atm,
     :                 atmos_lowestlp_atm,
     :                 land_latentinst_atm,land_sensibleinst_atm,
     :                 land_stressxinst_atm,land_stressyinst_atm,
     :                 land_evapinst_atm,
     :                 land_latent_atm,land_sensible_atm,
     :                 land_stressx_atm,land_stressy_atm,
     :                 land_evap_atm,
     :                 land_tstarinst_atm,
     :                 land_rough_atm,
     :                 land_qstar_atm,
     :                 land_salb_atm,
     :                 land_runoff_atm,
     :                 test_water_land,
     :                 landicefrac_atm,
     :                 landsnowicefrac_atm,
     :                 landsnowvegfrac_atm,
     :                 landsnowdepth_atm,
     :                 landicealbedo_atm,
     :                 glim_covmap,.false.,.false.
     :                 )
                   ENDIF
!****************************************************************

                  call igcm_land_blayer(surf_iter_tim,
     :                 land_niter_tim,
     :                 atmos_dt_tim,
     :                 surfdsigma,
     :                 ilandmask1_atm,
     :                 land_sensibleinst_atm,
     :                 land_stressxinst_atm,land_stressyinst_atm,
     :                 land_evapinst_atm,
     :                 land_lowestlu_atm,land_lowestlv_atm,
     :                 land_lowestlt_atm,land_lowestlq_atm,
     :                 atmos_lowestlp_atm)

                  enddo

                 IF (flag_goldocn.and.flag_goldsic.and.(.not.lshort)) 
     :               then
                   call genie_restarts(0,
     :    .true.,
     :    alon1_atm,alat1_atm,
     :    ocean_sensible_atm,
     :    ocean_latent_atm,
     :    ocean_stressx_atm,
     :    ocean_stressy_atm,
     :    ocean_sensibleinst_atm,
     :    ocean_stressxinst_atm,
     :    ocean_stressyinst_atm,
     :    ocean_evapinst_atm,
     :    ocean_latentinst_atm,
     :    ocean_tstarinst_atm,
     :    ocean_rough_atm,
     :    ocean_qstar_atm,
     :    ocean_salb_atm,
     :    tstar_atm,
     :    albedo_atm,
     :    0)
                 endif

                 do surf_iter_tim=1,ocean_niter_tim

                 IF (flag_goldocn.and.flag_goldsic.and.(.not.lshort))
     :               then
c
                 else
                  call igcm_ocean_surflux(0,
     :                 surf_iter_tim,ocean_niter_tim,
     :                 surfsigma,
     :                 ilandmask1_atm,
     :                 netsolar_atm,netlong_atm,
     :                 precip_atm,
     :                 ocean_lowestlu_atm,ocean_lowestlv_atm,
     :                 ocean_lowestlt_atm,ocean_lowestlq_atm,
     :                 atmos_lowestlp_atm,
     :                 albedo_atm,tstar_atm,
     :                 ocean_latentinst_atm,ocean_sensibleinst_atm,
     :                 ocean_stressxinst_atm,ocean_stressyinst_atm,
     :                 ocean_evapinst_atm,
     :                 ocean_latent_atm,ocean_sensible_atm,
     :                 ocean_stressx_atm,ocean_stressy_atm,
     :                 ocean_evap_atm,
     :                 ocean_tstarinst_atm,
     :                 ocean_rough_atm,
     :                 ocean_qstar_atm,
     :                 ocean_salb_atm)
                 endif


                  call igcm_ocean_blayer(surf_iter_tim,
     :                 ocean_niter_tim,
     :                 atmos_dt_tim,
     :                 surfdsigma,
     :                 ilandmask1_atm,
     :                 ocean_sensibleinst_atm,
     :                 ocean_stressxinst_atm,ocean_stressyinst_atm,
     :                 ocean_evapinst_atm,
     :                 ocean_lowestlu_atm,ocean_lowestlv_atm,
     :                 ocean_lowestlt_atm,ocean_lowestlq_atm,
     :                 atmos_lowestlp_atm)

                  enddo

              surf_latent_atm(:,:) = 
     &                  ilandmask1_atm(:,:)*land_latent_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_latent_atm(:,:)
              surf_sensible_atm(:,:) = 
     &                  ilandmask1_atm(:,:)*land_sensible_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_sensible_atm(:,:)
              surf_stressx_atm(:,:) = 
     &                 ilandmask1_atm(:,:)*land_stressx_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_stressx_atm(:,:)
              surf_stressy_atm(:,:) = 
     &                 ilandmask1_atm(:,:)*land_stressy_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_stressy_atm(:,:)

              surf_tstarinst_atm(:,:) = 
     &                 ilandmask1_atm(:,:)*land_tstarinst_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_tstarinst_atm(:,:)
              surf_rough_atm(:,:) = 
     &                 ilandmask1_atm(:,:)*land_rough_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_rough_atm(:,:)
              surf_qstar_atm(:,:) = 
     &                 ilandmask1_atm(:,:)*land_qstar_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_qstar_atm(:,:)
              surf_salb_atm(:,:) = 
     &                 ilandmask1_atm(:,:)*land_salb_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_salb_atm(:,:)

              atmos_lowestlu_atm(:,:) = 
     &                 ilandmask1_atm(:,:)*land_lowestlu_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_lowestlu_atm(:,:)
              atmos_lowestlv_atm(:,:) = 
     &                 ilandmask1_atm*land_lowestlv_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_lowestlv_atm(:,:)
              atmos_lowestlt_atm(:,:) = 
     &                 ilandmask1_atm*land_lowestlt_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_lowestlt_atm(:,:)
              atmos_lowestlq_atm(:,:) = 
     &                 ilandmask1_atm*land_lowestlq_atm(:,:)
     &           + (1-ilandmask1_atm(:,:))*ocean_lowestlq_atm(:,:)


                  call igcm3_diab(
     :                 atmos_lowestlu_atm,atmos_lowestlv_atm,
     :                 atmos_lowestlt_atm,atmos_lowestlq_atm,
     :                 surf_salb_atm,surf_qstar_atm,
     :                 surf_tstarinst_atm,
     :                 surf_rough_atm,
     :                 surf_latent_atm,surf_sensible_atm,
     :                 surf_stressx_atm,surf_stressy_atm,
     :                 netsolar_atm,
     :                 netlong_atm,
     :                 precip_atm,
     :                 landsnowdepth_atm,
     :                 t2m_atm,
     :                 land_evap_atm,
     :                 co2_atm,n2o_atm,ch4_atm,
     :                 iconv_che,water_flux_atmos,
     :                 mass14co2,ddtmass14co2
     :               )
c     

c     Do some checks for the number of levels......
      if (nl.eq.22) then
      if ( (nlevrf.ne.2).or.
     :     (.not.l22l).or.
     :     (mxlev.ne.22).or.
     :     ((.not.coldstart).and.(lshort)).or.
     :     (itspd.ne.64) ) then
      print*,'You are not using the default settings for 22 levels'
      print*,'Change settings or edit initialise_atmos.f and recompile'
      stop
      endif
      endif

      if (nl.eq.10) then
      if ( (nlevrf.ne.1).or.
     :     (difusestrength.ne.0.0625).or.
     :     (mxlev.ne.10).or.
     :     (.not.coldstart).or.
     :     (itspd.ne.24).or.
     :     (lstretch) ) then
      print*,'You are not using the default settings for 10 levels'
      print*,'Change settings or edit initialise_atmos.f and recompile'
      stop
      endif
      endif

      if (nl.eq.7) then
      if ( (nlevrf.ne.1).or.
     :     (difusestrength.ne.0.0625).or.
     :     (mxlev.ne.7).or.
     :     ((.not.coldstart).and.(lshort)).or.
     :     (itspd.ne.24).or.
     :     (lstretch) ) then
      print*,'You are not using the default settings for 7 levels'
      print*,'Change settings or edit initialise_atmos.f and recompile'
      stop
      endif
      endif

      if (nl.eq.5) then
      if ( (nlevrf.ne.1).or.
     :     (difusestrength.ne.0.0625).or.
     :     (mxlev.ne.5).or.
     :     (itspd.ne.24).or.
     :     (lstretch) ) then
      print*,'You are not using the default settings for 5 levels'
      print*,'Change settings or edit initialise_atmos.f and recompile'
      stop
      endif
      endif

c     close NAMELIST file
      close(7)

      return
      end
