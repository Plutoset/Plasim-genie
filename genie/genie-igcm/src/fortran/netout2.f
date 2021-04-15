      subroutine netout2

      implicit none

c     
#include "param1.cmn"
      include 'param2.cmn'
      real rad
      parameter(rad=180.0/pi)
      include 'blank.cmn'
      include 'bats.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
      include 'legau.cmn'
      include 'outcon.cmn'
      include 'netdata.cmn'
      include 'means.cmn'
      include 'cpiers.cmn'
      include 'fluxes.cmn'

      INTEGER J,I,IHEM,IOF,JJ,K
c     
      REAL LUG(MG,JGG),LTG(MG,JGG),LTSTAR(MG,JGG),lqstar(mg,jgg),
     :     LTSENS(MG,JGG),LTSNOWD(MG,JGG),LTLATENT(MG,JGG),
     :     LTSTRESSX(MG,JGG),LTSTRESSY(MG,JGG),
     :     LTCLD(MG,JGG,5), LTTEMP2M(MG,JGG), LTLANDEVAP(MG,JGG),
     :     LTNETSOLAR(MG,JGG),LTNETLONG(MG,JGG),
     :     LTDOWNSOLAR(MG,JGG),LTDOWNLONG(MG,JGG),
     :     LTNETSOLAR_toa(MG,JGG),LTNETLONG_toa(MG,JGG),
     :     LTDOWNSOLAR_toa(MG,JGG),LTDOWNLONG_toa(MG,JGG)

      REAL TNETSOLAR(IGC,JG),TNETLONG(IGC,JG)
      REAL TDOWNSOLAR(IGC,JG),TDOWNLONG(IGC,JG)
      REAL TNETSOLAR_toa(IGC,JG),TNETLONG_toa(IGC,JG)
      REAL TDOWNSOLAR_toa(IGC,JG),TDOWNLONG_toa(IGC,JG)

      integer loc_dim
c     
      do j=1,jg
         do i=1,igc
            tnetsolar(i,j)=arflux(i,j,1)-arflux(i,j,2)
            tnetlong(i,j)=arflux(i,j,3)-arflux(i,j,4)
            tdownsolar(i,j)=arflux(i,j,1)
            tdownlong(i,j)=arflux(i,j,3)

            tnetsolar_toa(i,j)=arflux(i,j,5)-arflux(i,j,6)
            tnetlong_toa(i,j)=arflux(i,j,7)-arflux(i,j,8)
            tdownsolar_toa(i,j)=arflux(i,j,5)
            tdownlong_toa(i,j)=arflux(i,j,7)

         end do
      end do
c     
      do j=1,jg
         do ihem=1,nhem
            iof=(ihem-1)*mgpp
            if (ihem.eq.1) then
               jj=j
            else
               jj=jggp-j
            end if
            do i=1,mg
               lug(i,jj)=arrcr(i+iof,j)*real(itspd)
               ltg(i,jj)=arrlr(i+iof,j)*real(itspd)
               ltsens(i,jj)=ashbl(i+iof,j)*(-cv)*p0
               ltsnowd(i,jj)=asnowd(i+iof,j)
               lttemp2m(i,jj)=atemp2m(i+iof,j)
               ltlandevap(i,jj)=alandevap(i+iof,j)
               ltlatent(i,jj)=aslbl(i+iof,j)*(-cv)*p0
               ltstar(i,jj)=ct*atstar(i+iof,j)-273.16
               lqstar(i,jj)=aqstar(i+iof,j)
               ltnetsolar(i,jj)=tnetsolar(i+iof,j)
               ltnetlong(i,jj)=tnetlong(i+iof,j)
               ltdownsolar(i,jj)=tdownsolar(i+iof,j)
               ltdownlong(i,jj)=tdownlong(i+iof,j)
               ltnetsolar_toa(i,jj)=tnetsolar_toa(i+iof,j)
               ltnetlong_toa(i,jj)=tnetlong_toa(i+iof,j)
               ltdownsolar_toa(i,jj)=tdownsolar_toa(i+iof,j)
               ltdownlong_toa(i,jj)=tdownlong_toa(i+iof,j)
               ltstressx(i,jj)=assblx(i+iof,j)*p0
               ltstressy(i,jj)=assbly(i+iof,j)*p0
               do k=1,5
                  ltcld(i,jj,k)=acld(i+iof,j,k)
               end do
            end do
         end do
      end do
c     
      if (ldaily) then

      call writevar2(nc(1),
     :     idvar(loc_dim('convrain',varname(1,1),nall),1),
     :     lug,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('lscalerain',varname(1,1),nall),1),
     :     ltg,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('surftemp',varname(1,1),nall),1),
     :     ltstar,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('surfhum',varname(1,1),nall),1),
     :     lqstar,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('sensheat',varname(1,1),nall),1),
     :     ltsens,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('snowdepth',varname(1,1),nall),1),
     :     ltsnowd,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('temp2m',varname(1,1),nall),1),
     :     lttemp2m,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('landevap',varname(1,1),nall),1),
     :     ltlandevap,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('latentheat',varname(1,1),nall),1),
     :     ltlatent,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('netsolar',varname(1,1),nall),1),
     :     ltnetsolar,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('netlong',varname(1,1),nall),1),
     :     ltnetlong,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('downsolar',varname(1,1),nall),1),
     :     ltdownsolar,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('downlong',varname(1,1),nall),1),
     :     ltdownlong,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('netsolar_toa',varname(1,1),nall),1),
     :     ltnetsolar_toa,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('netlong_toa',varname(1,1),nall),1),
     :     ltnetlong_toa,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('downsolar_toa',varname(1,1),nall),1),
     :     ltdownsolar_toa,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('downlong_toa',varname(1,1),nall),1),
     :     ltdownlong_toa,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('stressx',varname(1,1),nall),1),
     :     ltstressx,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('stressy',varname(1,1),nall),1),
     :     ltstressy,1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('cloudl',varname(1,1),nall),1),
     :     ltcld(1,1,1),1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('cloudm',varname(1,1),nall),1),
     :     ltcld(1,1,2),1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('cloudh',varname(1,1),nall),1),
     :     ltcld(1,1,3),1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('cloudshc',varname(1,1),nall),1),
     :     ltcld(1,1,4),1,mg,1,jgg,inetcount,inetcount,0,0)
      call writevar2(nc(1),
     :     idvar(loc_dim('cloudc',varname(1,1),nall),1),
     :     ltcld(1,1,5),1,mg,1,jgg,inetcount,inetcount,0,0)
c     
      endif

      return
      end
c     
      subroutine finish_nc(imode)
      implicit none
c     
#include "param1.cmn"
      include 'param2.cmn'
      include 'legau.cmn'
      include 'blank.cmn'
      include 'bats.cmn'
      include 'means.cmn'
      include 'netdata.cmn'
c     
      integer :: imode,loc_dim
c
c     imode = 1   daily   files
c     imode = 2   monthly files
c     imode = 3   annual  files
c     imode = 4   decadal files
c     
      if (imode.eq.1) then
         call end_netcdf(1)
         call ini_netcdf(imonth,iyear,sigma,alat,1)
         inetcount=0
      else if (imode.eq.2) then
         call finish_means(umn,mg,jgg,nl,30*itspd)
         call finish_means(tmn,mg,jgg,nl,30*itspd)
         call finish_means(spmn,mg,jgg,1,30*itspd)
         call finish_means(mspmn,mg,jgg,1,30*itspd)
         call finish_means(u_pmn,mg,jgg,nl,30*itspd)
         call finish_means(v_pmn,mg,jgg,nl,30*itspd)
         call finish_means(t_pmn,mg,jgg,nl,30*itspd)
         call finish_means(q_pmn,mg,jgg,nl,30*itspd)
         call finish_means(g_pmn,mg,jgg,nl,30*itspd)
         call writevar(nc(imode),
     :        idvar(loc_dim('zonal_wind',varname(1,imode),nall),imode),
     :        umn)
         call writevar(nc(imode),
     :        idvar(loc_dim('temp',varname(1,imode),nall),imode),
     :        tmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('pstar',varname(1,imode),nall),imode),
     :        spmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('mslp',varname(1,imode),nall),imode),
     :        mspmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('zonal_wind_p',varname(1,imode),nall),
     :        imode),u_pmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('northward_wind_p',varname(1,imode),nall),
     :        imode),v_pmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('air_temperature_p',varname(1,imode),nall),
     :        imode),t_pmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('specific_humidity_p',
     :        varname(1,imode),nall),
     :        imode),q_pmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('geopotential_height_p',
     :        varname(1,imode),nall),
     :        imode),g_pmn)
         call inimeans(tmn,mg,jgg,nl)
         call inimeans(umn,mg,jgg,nl)
         call inimeans(spmn,mg,jgg,1)
         call inimeans(mspmn,mg,jgg,1)
         call inimeans(u_pmn,mg,jgg,nl)
         call inimeans(v_pmn,mg,jgg,nl)
         call inimeans(t_pmn,mg,jgg,nl)
         call inimeans(q_pmn,mg,jgg,nl)
         call inimeans(g_pmn,mg,jgg,nl)
c     
         call finish_means(convrain,mg,jgg,1,30)
         call finish_means(lscalerain,mg,jgg,1,30)
         call finish_means(tstarmn,mg,jgg,1,30)
         call finish_means(qstarmn,mg,jgg,1,30)
         call finish_means(sensheat,mg,jgg,1,30)
         call finish_means(snowdepth,mg,jgg,1,30)
         call finish_means(temp2meter,mg,jgg,1,30)
         call finish_means(levap,mg,jgg,1,30)
         call finish_means(latentheat,mg,jgg,1,30)
         call finish_means(netsolar,mg,jgg,1,30)
         call finish_means(netlong,mg,jgg,1,30)
         call finish_means(downsolar,mg,jgg,1,30)
         call finish_means(downlong,mg,jgg,1,30)
         call finish_means(netsolar_toa,mg,jgg,1,30)
         call finish_means(netlong_toa,mg,jgg,1,30)
         call finish_means(downsolar_toa,mg,jgg,1,30)
         call finish_means(downlong_toa,mg,jgg,1,30)
         call finish_means(stressx,mg,jgg,1,30)
         call finish_means(stressy,mg,jgg,1,30)
         call finish_means(cloud_frac,mg,jgg,5,30)
c     
         call writevar(nc(imode),
     :        idvar(loc_dim('convrain',varname(1,imode),nall),imode),
     :        convrain)
         call writevar(nc(imode),
     :        idvar(loc_dim('lscalerain',varname(1,imode),nall),imode),
     :        lscalerain)
         call writevar(nc(imode),
     :        idvar(loc_dim('surftemp',varname(1,imode),nall),imode),
     :        tstarmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('surfhum',varname(1,imode),nall),imode),
     :        qstarmn)
         call writevar(nc(imode),
     :        idvar(loc_dim('sensheat',varname(1,imode),nall),imode),
     :        sensheat)
         call writevar(nc(imode),
     :        idvar(loc_dim('snowdepth',varname(1,imode),nall),imode),
     :        snowdepth)
         call writevar(nc(imode),
     :        idvar(loc_dim('temp2m',varname(1,imode),nall),imode),
     :        temp2meter)
         call writevar(nc(imode),
     :        idvar(loc_dim('landevap',varname(1,imode),nall),imode),
     :        levap)
         call writevar(nc(imode),
     :        idvar(loc_dim('latentheat',varname(1,imode),nall),imode),
     :        latentheat)
         call writevar(nc(imode),
     :        idvar(loc_dim('netsolar',varname(1,imode),nall),imode),
     :        netsolar)
         call writevar(nc(imode),
     :        idvar(loc_dim('netlong',varname(1,imode),nall),imode),
     :        netlong)
         call writevar(nc(imode),
     :        idvar(loc_dim('downsolar',varname(1,imode),nall),imode),
     :        downsolar)
         call writevar(nc(imode),
     :        idvar(loc_dim('downlong',varname(1,imode),nall),imode),
     :        downlong)
         call writevar(nc(imode),
     :        idvar(loc_dim('netsolar_toa',varname(1,imode),nall),
     :                 imode),netsolar_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('netlong_toa',varname(1,imode),nall),
     :                 imode),netlong_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('downsolar_toa',varname(1,imode),nall),
     :                 imode),downsolar_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('downlong_toa',varname(1,imode),nall),
     :                 imode),downlong_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('stressx',varname(1,imode),nall),imode),
     :        stressx)
         call writevar(nc(imode),
     :        idvar(loc_dim('stressy',varname(1,imode),nall),imode),
     :        stressy)
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudl',varname(1,imode),nall),imode),
     :        cloud_frac(1,1,1))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudm',varname(1,imode),nall),imode),
     :        cloud_frac(1,1,2))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudh',varname(1,imode),nall),imode),
     :        cloud_frac(1,1,3))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudshc',varname(1,imode),nall),imode),
     :        cloud_frac(1,1,4))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudc',varname(1,imode),nall),imode),
     :        cloud_frac(1,1,5))
c     
         call end_netcdf(imode)
c     
         call inimeans(convrain,mg,jgg,1)
         call inimeans(lscalerain,mg,jgg,1)
         call inimeans(tstarmn,mg,jgg,1)
         call inimeans(qstarmn,mg,jgg,1)
         call inimeans(sensheat,mg,jgg,1)
         call inimeans(snowdepth,mg,jgg,1)
         call inimeans(temp2meter,mg,jgg,1)
         call inimeans(levap,mg,jgg,1)
         call inimeans(latentheat,mg,jgg,1)
         call inimeans(netsolar,mg,jgg,1)
         call inimeans(netlong,mg,jgg,1)
         call inimeans(downsolar,mg,jgg,1)
         call inimeans(downlong,mg,jgg,1)
         call inimeans(netsolar_toa,mg,jgg,1)
         call inimeans(netlong_toa,mg,jgg,1)
         call inimeans(downsolar_toa,mg,jgg,1)
         call inimeans(downlong_toa,mg,jgg,1)
         call inimeans(stressx,mg,jgg,1)
         call inimeans(stressy,mg,jgg,1)
         call inimeans(cloud_frac,mg,jgg,5)
c     
         call ini_netcdf(imonth,iyear,sigma,alat,imode)
      else if (imode.eq.3) then
         call finish_means(umna,mg,jgg,nl,360*itspd)
         call finish_means(tmna,mg,jgg,nl,360*itspd)
         call finish_means(spmna,mg,jgg,1,360*itspd)
         call finish_means(mspmna,mg,jgg,1,360*itspd)
         call finish_means(u_pmna,mg,jgg,nl,360*itspd)
         call finish_means(v_pmna,mg,jgg,nl,360*itspd)
         call finish_means(t_pmna,mg,jgg,nl,360*itspd)
         call finish_means(q_pmna,mg,jgg,nl,360*itspd)
         call finish_means(g_pmna,mg,jgg,nl,360*itspd)
         call writevar(nc(imode),
     :        idvar(loc_dim('zonal_wind',varname(1,imode),nall),imode),
     :        umna)
         call writevar(nc(imode),
     :        idvar(loc_dim('temp',varname(1,imode),nall),imode),
     :        tmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('pstar',varname(1,imode),nall),imode),
     :        spmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('mslp',varname(1,imode),nall),imode),
     :        mspmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('zonal_wind_p',varname(1,imode),nall),
     :        imode),u_pmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('northward_wind_p',varname(1,imode),nall),
     :        imode),v_pmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('air_temperature_p',varname(1,imode),nall),
     :        imode),t_pmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('specific_humidity_p',
     :        varname(1,imode),nall),
     :        imode),q_pmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('geopotential_height_p',
     :        varname(1,imode),nall),
     :        imode),g_pmna)
         call inimeans(umna,mg,jgg,nl)
         call inimeans(tmna,mg,jgg,nl)
         call inimeans(spmna,mg,jgg,1)
         call inimeans(mspmna,mg,jgg,1)
         call inimeans(u_pmna,mg,jgg,nl)
         call inimeans(v_pmna,mg,jgg,nl)
         call inimeans(t_pmna,mg,jgg,nl)
         call inimeans(q_pmna,mg,jgg,nl)
         call inimeans(g_pmna,mg,jgg,nl)
c     
         call finish_means(convraina,mg,jgg,1,360)
         call finish_means(lscaleraina,mg,jgg,1,360)
         call finish_means(tstarmna,mg,jgg,1,360)
         call finish_means(qstarmna,mg,jgg,1,360)
         call finish_means(sensheata,mg,jgg,1,360)
         call finish_means(snowdeptha,mg,jgg,1,360)
         call finish_means(temp2metera,mg,jgg,1,360)
         call finish_means(levapa,mg,jgg,1,360)
         call finish_means(latentheata,mg,jgg,1,360)
         call finish_means(netsolara,mg,jgg,1,360)
         call finish_means(netlonga,mg,jgg,1,360)
         call finish_means(downsolara,mg,jgg,1,360)
         call finish_means(downlonga,mg,jgg,1,360)
         call finish_means(netsolara_toa,mg,jgg,1,360)
         call finish_means(netlonga_toa,mg,jgg,1,360)
         call finish_means(downsolara_toa,mg,jgg,1,360)
         call finish_means(downlonga_toa,mg,jgg,1,360)
         call finish_means(stressxa,mg,jgg,1,360)
         call finish_means(stressya,mg,jgg,1,360)
         call finish_means(cloud_fraca,mg,jgg,5,360)
c     
         call writevar(nc(imode),
     :        idvar(loc_dim('convrain',varname(1,imode),nall),imode),
     :        convraina)
         call writevar(nc(imode),
     :        idvar(loc_dim('lscalerain',varname(1,imode),nall),imode),
     :        lscaleraina)
         call writevar(nc(imode),
     :        idvar(loc_dim('surftemp',varname(1,imode),nall),imode),
     :        tstarmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('surfhum',varname(1,imode),nall),imode),
     :        qstarmna)
         call writevar(nc(imode),
     :        idvar(loc_dim('sensheat',varname(1,imode),nall),imode),
     :        sensheata)
         call writevar(nc(imode),
     :        idvar(loc_dim('snowdepth',varname(1,imode),nall),imode),
     :        snowdeptha)
         call writevar(nc(imode),
     :        idvar(loc_dim('temp2m',varname(1,imode),nall),imode),
     :        temp2metera)
         call writevar(nc(imode),
     :        idvar(loc_dim('landevap',varname(1,imode),nall),imode),
     :        levapa)
         call writevar(nc(imode),
     :        idvar(loc_dim('latentheat',varname(1,imode),nall),imode),
     :        latentheata)
         call writevar(nc(imode),
     :        idvar(loc_dim('netsolar',varname(1,imode),nall),imode),
     :        netsolara)
         call writevar(nc(imode),
     :        idvar(loc_dim('netlong',varname(1,imode),nall),imode),
     :        netlonga)
         call writevar(nc(imode),
     :        idvar(loc_dim('downsolar',varname(1,imode),nall),imode),
     :        downsolara)
         call writevar(nc(imode),
     :        idvar(loc_dim('downlong',varname(1,imode),nall),imode),
     :        downlonga)
        call writevar(nc(imode),
     :        idvar(loc_dim('netsolar_toa',varname(1,imode),nall),
     :               imode),netsolara_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('netlong_toa',varname(1,imode),nall),
     :               imode),netlonga_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('downsolar_toa',varname(1,imode),nall),
     :               imode),downsolara_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('downlong_toa',varname(1,imode),nall),
     :               imode),downlonga_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('stressx',varname(1,imode),nall),imode),
     :        stressxa)
         call writevar(nc(imode),
     :        idvar(loc_dim('stressy',varname(1,imode),nall),imode),
     :        stressya)
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudl',varname(1,imode),nall),imode),
     :        cloud_fraca(1,1,1))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudm',varname(1,imode),nall),imode),
     :        cloud_fraca(1,1,2))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudh',varname(1,imode),nall),imode),
     :        cloud_fraca(1,1,3))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudshc',varname(1,imode),nall),imode),
     :        cloud_fraca(1,1,4))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudc',varname(1,imode),nall),imode),
     :        cloud_fraca(1,1,5))
c     
         call end_netcdf(imode)
c     
         call inimeans(convraina,mg,jgg,1)
         call inimeans(lscaleraina,mg,jgg,1)
         call inimeans(tstarmna,mg,jgg,1)
         call inimeans(qstarmna,mg,jgg,1)
         call inimeans(sensheata,mg,jgg,1)
         call inimeans(snowdeptha,mg,jgg,1)
         call inimeans(temp2metera,mg,jgg,1)
         call inimeans(levapa,mg,jgg,1)
         call inimeans(latentheata,mg,jgg,1)
         call inimeans(netsolara,mg,jgg,1)
         call inimeans(netlonga,mg,jgg,1)
         call inimeans(downsolara,mg,jgg,1)
         call inimeans(downlonga,mg,jgg,1)
         call inimeans(netsolara_toa,mg,jgg,1)
         call inimeans(netlonga_toa,mg,jgg,1)
         call inimeans(downsolara_toa,mg,jgg,1)
         call inimeans(downlonga_toa,mg,jgg,1)
         call inimeans(stressxa,mg,jgg,1)
         call inimeans(stressya,mg,jgg,1)
         call inimeans(cloud_fraca,mg,jgg,5)
c     
         call ini_netcdf(imonth,iyear,sigma,alat,imode)
      else if (imode.eq.4) then
         call finish_means(umnd,mg,jgg,nl*12,300*itspd)
         call finish_means(tmnd,mg,jgg,nl*12,300*itspd)
         call finish_means(spmnd,mg,jgg,1*12,300*itspd)
         call finish_means(mspmnd,mg,jgg,1*12,300*itspd)
         call finish_means(u_pmnd,mg,jgg,nl*12,300*itspd)
         call finish_means(v_pmnd,mg,jgg,nl*12,300*itspd)
         call finish_means(t_pmnd,mg,jgg,nl*12,300*itspd)
         call finish_means(q_pmnd,mg,jgg,nl*12,300*itspd)
         call finish_means(g_pmnd,mg,jgg,nl*12,300*itspd)
         call prep_decade(umnd,mg,jgg,nl)
         call prep_decade(tmnd,mg,jgg,nl)
         call prep_decade(spmnd,mg,jgg,1)
         call prep_decade(mspmnd,mg,jgg,1)
         call prep_decade(u_pmnd,mg,jgg,nl)
         call prep_decade(v_pmnd,mg,jgg,nl)
         call prep_decade(t_pmnd,mg,jgg,nl)
         call prep_decade(q_pmnd,mg,jgg,nl)
         call prep_decade(g_pmnd,mg,jgg,nl)
         call writevar(nc(imode),
     :        idvar(loc_dim('zonal_wind',varname(1,imode),nall),imode),
     :        umnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('temp',varname(1,imode),nall),imode),
     :        tmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('pstar',varname(1,imode),nall),imode),
     :        spmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('mslp',varname(1,imode),nall),imode),
     :        mspmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('zonal_wind_p',varname(1,imode),nall),
     :        imode),u_pmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('northward_wind_p',varname(1,imode),nall),
     :        imode),v_pmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('air_temperature_p',varname(1,imode),nall),
     :        imode),t_pmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('specific_humidity_p',
     :        varname(1,imode),nall),
     :        imode),q_pmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('geopotential_height_p',
     :        varname(1,imode),nall),
     :        imode),g_pmnd)
         call inimeans(umnd,mg,jgg,nl*12)
         call inimeans(tmnd,mg,jgg,nl*12)
         call inimeans(spmnd,mg,jgg,12)
         call inimeans(mspmnd,mg,jgg,12)
         call inimeans(u_pmnd,mg,jgg,nl*12)
         call inimeans(v_pmnd,mg,jgg,nl*12)
         call inimeans(t_pmnd,mg,jgg,nl*12)
         call inimeans(q_pmnd,mg,jgg,nl*12)
         call inimeans(g_pmnd,mg,jgg,nl*12)
c     
         call finish_means(convraind,mg,jgg,12,300)
         call prep_decade(convraind,mg,jgg,1)
         call finish_means(lscaleraind,mg,jgg,12,300)
         call prep_decade(lscaleraind,mg,jgg,1)
         call finish_means(tstarmnd,mg,jgg,12,300)
         call prep_decade(tstarmnd,mg,jgg,1)
         call finish_means(qstarmnd,mg,jgg,12,300)
         call prep_decade(qstarmnd,mg,jgg,1)
         call finish_means(sensheatd,mg,jgg,12,300)
         call prep_decade(sensheatd,mg,jgg,1)
         call finish_means(snowdepthd,mg,jgg,12,300)
         call prep_decade(snowdepthd,mg,jgg,1)
         call finish_means(temp2meterd,mg,jgg,12,300)
         call prep_decade(temp2meterd,mg,jgg,1)
         call finish_means(levapd,mg,jgg,12,300)
         call prep_decade(levapd,mg,jgg,1)
         call finish_means(latentheatd,mg,jgg,12,300)
         call prep_decade(latentheatd,mg,jgg,1)
         call finish_means(netsolard,mg,jgg,12,300)
         call prep_decade(netsolard,mg,jgg,1)
         call finish_means(netlongd,mg,jgg,12,300)
         call prep_decade(netlongd,mg,jgg,1)
         call finish_means(downsolard,mg,jgg,12,300)
         call prep_decade(downsolard,mg,jgg,1)
         call finish_means(downlongd,mg,jgg,12,300)
         call prep_decade(downlongd,mg,jgg,1)
         call finish_means(netsolard_toa,mg,jgg,12,300)
         call prep_decade(netsolard_toa,mg,jgg,1)
         call finish_means(netlongd_toa,mg,jgg,12,300)
         call prep_decade(netlongd_toa,mg,jgg,1)
         call finish_means(downsolard_toa,mg,jgg,12,300)
         call prep_decade(downsolard_toa,mg,jgg,1)
         call finish_means(downlongd_toa,mg,jgg,12,300)
         call prep_decade(downlongd_toa,mg,jgg,1)
         call finish_means(stressxd,mg,jgg,12,300)
         call prep_decade(stressxd,mg,jgg,1)
         call finish_means(stressyd,mg,jgg,12,300)
         call prep_decade(stressyd,mg,jgg,1)
         call finish_means(cloud_fracd,mg,jgg,5*12,300)
         call prep_decade(cloud_fracd(1,1,1,1),mg,jgg,1)
         call prep_decade(cloud_fracd(1,1,1,2),mg,jgg,1)
         call prep_decade(cloud_fracd(1,1,1,3),mg,jgg,1)
         call prep_decade(cloud_fracd(1,1,1,4),mg,jgg,1)
         call prep_decade(cloud_fracd(1,1,1,5),mg,jgg,1)


c     
         call writevar(nc(imode),
     :        idvar(loc_dim('convrain',varname(1,imode),nall),imode),
     :        convraind)
         call writevar(nc(imode),
     :        idvar(loc_dim('lscalerain',varname(1,imode),nall),imode),
     :        lscaleraind)
         call writevar(nc(imode),
     :        idvar(loc_dim('surftemp',varname(1,imode),nall),imode),
     :        tstarmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('surfhum',varname(1,imode),nall),imode),
     :        qstarmnd)
         call writevar(nc(imode),
     :        idvar(loc_dim('sensheat',varname(1,imode),nall),imode),
     :        sensheatd)
         call writevar(nc(imode),
     :        idvar(loc_dim('snowdepth',varname(1,imode),nall),imode),
     :        snowdepthd)
         call writevar(nc(imode),
     :        idvar(loc_dim('temp2m',varname(1,imode),nall),imode),
     :        temp2meterd)
         call writevar(nc(imode),
     :        idvar(loc_dim('landevap',varname(1,imode),nall),imode),
     :        levapd)
         call writevar(nc(imode),
     :        idvar(loc_dim('latentheat',varname(1,imode),nall),imode),
     :        latentheatd)
         call writevar(nc(imode),
     :        idvar(loc_dim('netsolar',varname(1,imode),nall),imode),
     :        netsolard)
         call writevar(nc(imode),
     :        idvar(loc_dim('netlong',varname(1,imode),nall),imode),
     :        netlongd)
         call writevar(nc(imode),
     :        idvar(loc_dim('downsolar',varname(1,imode),nall),imode),
     :        downsolard)
         call writevar(nc(imode),
     :        idvar(loc_dim('downlong',varname(1,imode),nall),imode),
     :        downlongd)
         call writevar(nc(imode),
     :        idvar(loc_dim('netsolar_toa',varname(1,imode),nall),
     :        imode),netsolard_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('netlong_toa',varname(1,imode),nall),
     :        imode),netlongd_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('downsolar_toa',varname(1,imode),nall),
     :        imode),downsolard_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('downlong_toa',varname(1,imode),nall),
     :        imode),downlongd_toa)
         call writevar(nc(imode),
     :        idvar(loc_dim('stressx',varname(1,imode),nall),imode),
     :        stressxd)
         call writevar(nc(imode),
     :        idvar(loc_dim('stressy',varname(1,imode),nall),imode),
     :        stressyd)
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudl',varname(1,imode),nall),imode),
     :        cloud_fracd(1,1,1,1))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudm',varname(1,imode),nall),imode),
     :        cloud_fracd(1,1,1,2))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudh',varname(1,imode),nall),imode),
     :        cloud_fracd(1,1,1,3))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudshc',varname(1,imode),nall),imode),
     :        cloud_fracd(1,1,1,4))
         call writevar(nc(imode),
     :        idvar(loc_dim('cloudc',varname(1,imode),nall),imode),
     :        cloud_fracd(1,1,1,5))
c     
         call end_netcdf(imode)
c     
         call inimeans(convraind,mg,jgg,12)
         call inimeans(lscaleraind,mg,jgg,12)
         call inimeans(tstarmnd,mg,jgg,12)
         call inimeans(qstarmnd,mg,jgg,12)
         call inimeans(sensheatd,mg,jgg,12)
         call inimeans(snowdepthd,mg,jgg,12)
         call inimeans(temp2meterd,mg,jgg,12)
         call inimeans(levapd,mg,jgg,12)
         call inimeans(latentheatd,mg,jgg,12)
         call inimeans(netsolard,mg,jgg,12)
         call inimeans(netlongd,mg,jgg,12)
         call inimeans(downsolard,mg,jgg,12)
         call inimeans(downlongd,mg,jgg,12)
         call inimeans(netsolard_toa,mg,jgg,12)
         call inimeans(netlongd_toa,mg,jgg,12)
         call inimeans(downsolard_toa,mg,jgg,12)
         call inimeans(downlongd_toa,mg,jgg,12)
         call inimeans(stressxd,mg,jgg,12)
         call inimeans(stressyd,mg,jgg,12)
         call inimeans(cloud_fracd,mg,jgg,5*12)
c     
         call ini_netcdf(imonth,iyear,sigma,alat,imode)
      end if
c     
      return
      end
c     
      subroutine prep_decade(field,mg,jgg,nl)
c
c     Calculates annual mean from monthly means, and
c     puts it into array element 13
c
      implicit none
      integer mg,jgg,nl
      real field(mg*jgg*nl,13)
c 
      integer i,ii
      real asum,ar
c
      ar=1.0/12.0
      do i=1,mg*jgg*nl
         asum=0.0
         do ii=1,12
            asum=asum+field(i,ii)
         end do
         field(i,13)=asum*ar
      end do
c
      return
      end
c     
      subroutine accum_means_g(imode)
      implicit none
#include "param1.cmn"
      include 'param2.cmn'
      include 'bats.cmn'
      include 'blank.cmn'
      include 'gridpp.cmn'
      include 'means.cmn'
      include 'fluxes.cmn'
c     
      REAL TNETSOLAR(IGC,JG),TNETLONG(IGC,JG)
      REAL TDOWNSOLAR(IGC,JG),TDOWNLONG(IGC,JG)
      REAL TNETSOLAR_toa(IGC,JG),TNETLONG_toa(IGC,JG)
      REAL TDOWNSOLAR_toa(IGC,JG),TDOWNLONG_toa(IGC,JG)

      integer imode,i,j
c     
c     This subroutine controls the accumulation of means
c     of the grid point variables using as input the daily means
c     Options are: imode=2   monthly means
c     
      if (imode.eq.2) then
         do j=1,jg
            do i=1,igc
               tnetsolar(i,j)=arflux(i,j,1)-arflux(i,j,2)
               tnetlong(i,j)=arflux(i,j,3)-arflux(i,j,4)
               tdownsolar(i,j)=arflux(i,j,1)
               tdownlong(i,j)=arflux(i,j,3)

               tnetsolar_toa(i,j)=arflux(i,j,5)-arflux(i,j,6)
               tnetlong_toa(i,j)=arflux(i,j,7)-arflux(i,j,8)
               tdownsolar_toa(i,j)=arflux(i,j,5)
               tdownlong_toa(i,j)=arflux(i,j,7)

            end do
         end do
         call accum_means(atstar,tstarmn,mg,jg,nhem,1,ct,273.16)
         call accum_means(aqstar,qstarmn,mg,jg,nhem,1,1.0,0.0)
         call accum_means(arrcr,convrain,mg,jg,nhem,1,real(itspd),0.0)
         call accum_means(arrlr,lscalerain,mg,jg,nhem,1,real(itspd),0.0)
         call accum_means(ashbl,sensheat,mg,jg,nhem,1,-cv*p0,0.0)
         call accum_means(asnowd,snowdepth,mg,jg,nhem,1,1.0,0.0)
         call accum_means(atemp2m,temp2meter,mg,jg,nhem,1,1.0,0.0)
         call accum_means(alandevap,levap,mg,jg,nhem,1,1.0,0.0)
         call accum_means(aslbl,latentheat,mg,jg,nhem,1,-cv*p0,0.0)
         call accum_means(tnetsolar,netsolar,mg,jg,nhem,1,1.0,0.0)
         call accum_means(tnetlong,netlong,mg,jg,nhem,1,1.0,0.0)
         call accum_means(tdownsolar,downsolar,mg,jg,nhem,1,1.0,0.0)
         call accum_means(tdownlong,downlong,mg,jg,nhem,1,1.0,0.0)
         call accum_means(tnetsolar_toa,netsolar_toa,
     :                  mg,jg,nhem,1,1.0,0.0)
         call accum_means(tnetlong_toa,netlong_toa,
     :                  mg,jg,nhem,1,1.0,0.0)
         call accum_means(tdownsolar_toa,downsolar_toa,
     :                  mg,jg,nhem,1,1.0,0.0)
         call accum_means(tdownlong_toa,downlong_toa,
     :                  mg,jg,nhem,1,1.0,0.0)
         call accum_means(assblx,stressx,mg,jg,nhem,1,p0,0.0)
         call accum_means(assbly,stressy,mg,jg,nhem,1,p0,0.0)
         call accum_means(acld(1,1,1),cloud_frac(1,1,1),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,2),cloud_frac(1,1,2),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,3),cloud_frac(1,1,3),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,4),cloud_frac(1,1,4),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,5),cloud_frac(1,1,5),
     :        mg,jg,nhem,1,1.0,0.0)
      else if (imode.eq.3) then
         do j=1,jg
            do i=1,igc
               tnetsolar(i,j)=arflux(i,j,1)-arflux(i,j,2)
               tnetlong(i,j)=arflux(i,j,3)-arflux(i,j,4)
               tdownsolar(i,j)=arflux(i,j,1)
               tdownlong(i,j)=arflux(i,j,3)

               tnetsolar_toa(i,j)=arflux(i,j,5)-arflux(i,j,6)
               tnetlong_toa(i,j)=arflux(i,j,7)-arflux(i,j,8)
               tdownsolar_toa(i,j)=arflux(i,j,5)
               tdownlong_toa(i,j)=arflux(i,j,7)
            end do
         end do
         call accum_means(atstar,tstarmna,mg,jg,nhem,1,ct,273.16)
         call accum_means(aqstar,qstarmna,mg,jg,nhem,1,1.0,0.0)
         call accum_means(arrcr,convraina,mg,jg,nhem,1,real(itspd),0.0)
         call accum_means(arrlr,lscaleraina,mg,jg,nhem,1,real(itspd),
     :        0.0)
         call accum_means(ashbl,sensheata,mg,jg,nhem,1,-cv*p0,0.0)
         call accum_means(asnowd,snowdeptha,mg,jg,nhem,1,1.0,0.0)
         call accum_means(atemp2m,temp2metera,mg,jg,nhem,1,1.0,0.0)
         call accum_means(alandevap,levapa,mg,jg,nhem,1,1.0,0.0)
         call accum_means(aslbl,latentheata,mg,jg,nhem,1,-cv*p0,0.0)
         call accum_means(tnetsolar,netsolara,mg,jg,nhem,1,1.0,0.0)
         call accum_means(tnetlong,netlonga,mg,jg,nhem,1,1.0,0.0)
         call accum_means(tdownsolar,downsolara,mg,jg,nhem,1,1.0,0.0)
         call accum_means(tdownlong,downlonga,mg,jg,nhem,1,1.0,0.0)
         call accum_means(tnetsolar_toa,netsolara_toa,
     :                                 mg,jg,nhem,1,1.0,0.0)
         call accum_means(tnetlong_toa,netlonga_toa,
     :                                 mg,jg,nhem,1,1.0,0.0)
         call accum_means(tdownsolar_toa,downsolara_toa,
     :                                 mg,jg,nhem,1,1.0,0.0)
         call accum_means(tdownlong_toa,downlonga_toa,
     :                                 mg,jg,nhem,1,1.0,0.0)
         call accum_means(assblx,stressxa,mg,jg,nhem,1,p0,0.0)
         call accum_means(assbly,stressya,mg,jg,nhem,1,p0,0.0)
         call accum_means(acld(1,1,1),cloud_fraca(1,1,1),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,2),cloud_fraca(1,1,2),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,3),cloud_fraca(1,1,3),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,4),cloud_fraca(1,1,4),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,5),cloud_fraca(1,1,5),
     :        mg,jg,nhem,1,1.0,0.0)
      else if (imode.eq.4) then
         do j=1,jg
            do i=1,igc
               tnetsolar(i,j)=arflux(i,j,1)-arflux(i,j,2)
               tnetlong(i,j)=arflux(i,j,3)-arflux(i,j,4)
               tdownsolar(i,j)=arflux(i,j,1)
               tdownlong(i,j)=arflux(i,j,3)

               tnetsolar_toa(i,j)=arflux(i,j,5)-arflux(i,j,6)
               tnetlong_toa(i,j)=arflux(i,j,7)-arflux(i,j,8)
               tdownsolar_toa(i,j)=arflux(i,j,5)
               tdownlong_toa(i,j)=arflux(i,j,7)

            end do
         end do
         call accum_means(atstar,tstarmnd(1,1,imonth),mg,jg,nhem,1,
     :        ct,273.16)
         call accum_means(aqstar,qstarmnd(1,1,imonth),mg,jg,nhem,1,
     :        1.0,0.0)
         call accum_means(arrcr,convraind(1,1,imonth),mg,jg,nhem,1,
     :        real(itspd),0.0)
         call accum_means(arrlr,lscaleraind(1,1,imonth),mg,jg,nhem,1,
     :        real(itspd),0.0)
         call accum_means(ashbl,sensheatd(1,1,imonth),mg,jg,nhem,
     :        1,-cv*p0,0.0)
         call accum_means(asnowd,snowdepthd(1,1,imonth),mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(atemp2m,temp2meterd(1,1,imonth),mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(alandevap,levapd(1,1,imonth),mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(aslbl,latentheatd(1,1,imonth),mg,jg,nhem,
     :        1,-cv*p0,0.0)
         call accum_means(tnetsolar,netsolard(1,1,imonth),mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(tnetlong,netlongd(1,1,imonth),mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(tdownsolar,downsolard(1,1,imonth),mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(tdownlong,downlongd(1,1,imonth),mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(tnetsolar_toa,netsolard_toa(1,1,imonth),
     :                        mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(tnetlong_toa,netlongd_toa(1,1,imonth),
     :                        mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(tdownsolar_toa,downsolard_toa(1,1,imonth),
     :                        mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(tdownlong_toa,downlongd_toa(1,1,imonth),
     :                        mg,jg,nhem,
     :        1,1.0,0.0)
         call accum_means(assblx,stressxd(1,1,imonth),mg,jg,nhem,
     :        1,p0,0.0)
         call accum_means(assbly,stressyd(1,1,imonth),mg,jg,nhem,
     :        1,p0,0.0)
         call accum_means(acld(1,1,1),cloud_fracd(1,1,imonth,1),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,2),cloud_fracd(1,1,imonth,2),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,3),cloud_fracd(1,1,imonth,3),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,4),cloud_fracd(1,1,imonth,4),
     :        mg,jg,nhem,1,1.0,0.0)
         call accum_means(acld(1,1,5),cloud_fracd(1,1,imonth,5),
     :        mg,jg,nhem,1,1.0,0.0)
      end if
c     
      return
      end

