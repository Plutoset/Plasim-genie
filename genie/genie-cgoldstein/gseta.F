* 
* subroutine gseta, sets up  atmosphere and sea-ice
* introduced 11/2/02 (Bob)
*
      subroutine gseta

      include 'var.cmn'

      real tv, tv2, tv3, tatm, relh0_ocean, relh0_land,
     1     circum, sigma, diffamp(2), diffwid, difflin, diffend
      real scl_co2, pc_co2_rise,u_tau_ice,ch_ice,cpo_ice
      real  asurf

      integer i, j, k, l, natl1a, npac1a, natl1b, npac1b, natl1c, npac1c 
      integer nboxmin, nboxtot, nboxe, nboxw, nboxn, nboxs

#ifdef ents
c     for time-varyting co2 constants:
      include '../genie-simpleland/tim_ents.cmn'
#endif

c read parameters

      print*,'diffamp(1),diffamp(2), diffwid, difflin'
      read(5,*)diffamp(1),diffamp(2), diffwid, difflin
      print*,diffamp(1),diffamp(2), diffwid, difflin

c parameter beta relates vertically-averaged advective transport
c to surface advective transport

      print*,'betaz(1),betam(1),betaz(2),betam(2)'
      read(5,*)betaz(1),betam(1),betaz(2),betam(2)
      print*,betaz(1),betam(1),betaz(2),betam(2)

c constants used in SSW parameterization

      sigma = 5.67e-8
      emo = 0.94 * sigma
      ema = 0.85 * sigma
c v2 more logical to set solar constant in oscsol.f
c     solconst = 1368.
      tfreez = 0.

c constants used in OLW parameterization

      b00 = 2.43414e2
      b10 = -3.47968e1
      b20 = 1.02790e1
      b01 = 2.60065
      b11 = -1.62064
      b21 = 6.34856e-1
      b02 = 4.40272e-3
      b12 = -2.26092e-2
      b22 = 1.12265e-2
      b03 = -2.05237e-5
      b13 = -9.67000e-5
      b23 = 5.62925e-5

cc EMBM stuff follows...

      asurf = rsc*rsc*ds*dphi

c climatological albedo (similar to Weaver et al. 2001)

      do j=1,jmax
         tv = asin(s(j))
         tv2 = 0.2 + 0.36*0.5*(1.0 - cos(2.0*tv))
         do i=1,imax
            albcl(i,j) = tv2
         enddo
      enddo

c atmospheric SSW absorption coefficient, value over land purely diagnostic

      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax)then
               ca(i,j)=0.3
            else
               ca(i,j)=1.0
            endif
         enddo
      enddo

c read some scalings

      print*,'scl_co2,pc_co2_rise'
      read(5,*)scl_co2,pc_co2_rise
      print*,scl_co2,pc_co2_rise

c factor corresponding to radiative forcing of 4 W/m**2
c per doubling of atmospheric CO2

      delf2x = 5.77

c reference CO2 conc. (ppm)
!      co20 = 278.
! Reference CO2 conc. (atms.) units change made by pph 20/11/03
c more consistent to use 280 for Holocene steady state nre 2/3/5
      co20 = 278.0e-6

c scale co20 by factor scl_co2

      do j = 1,jmax
         do i = 1,imax
            co2(i,j) = scl_co2*co20
         enddo
      enddo

#ifdef ents
#ifdef atchem
#else
c     Now, if we have ents running then read in the goin file...
c     See if we want to run with prescribed atmospheric co2 forcing...
c     If we do, then read in file and re-set initial co2...
      read(5,'(200a)') junk
      print*,trim(junk)
      read(5,*) t_co2,nco2,co2steps
      print*,t_co2,nco2,co2steps
      read(5,'(200a)') filenameco2
      print*,trim(filenameco2)
      if (t_co2.gt.0) then
        open(77,file=trim(filenameco2))
        do i=1,nco2
          read(77,*) co2_vect(i)
        enddo
      close(77)
      co2(:,:)=co2_vect(1)
      print*,'co2:',co2(1,1)
      endif
#endif

c     Now, if we have ents running then read in the goin file...
c     See if we want to run with prescribed orbital paramters...
c     If we do, then read in file and re-set initial co2...
      read(5,'(200a)') junk
      print*,trim(junk)
      read(5,*) t_orbit,norbit,orbitsteps
      print*,t_orbit,norbit,orbitsteps
      read(5,'(200a)') filenameorbit
      print*,trim(filenameorbit)
      if (t_orbit.gt.0) then
        open(77,file=trim(filenameorbit))
        do i=1,norbit
          read(77,*) orbitecc_vect(i)
        enddo
        do i=1,norbit
          read(77,*) orbitobl_vect(i)
        enddo
        do i=1,norbit
          read(77,*) orbitpre_vect(i)
        enddo
        do i=1,norbit
          read(77,*) orbittau_vect(i)
        enddo
      close(77)
c     radfor wants sine of obliquity in radians
c     orbitobl_vect(:)=sin(2*pi*orbitobl_vect(:)/360.0)
c     radfor wants perihelion in radians
c     orbitpre_vect(:)=2*pi*orbitpre_vect(:)/360.0
      endif
#endif

c     ryear = 1.0/(365*86400)
      ryear = 1.0/syr

      rate_co2 = pc_co2_rise*0.01*tsc*dtatm*ndta*ryear

c more constants

      rhoair = 1.25
      rho0 = 1e3
      rhoao = rhoair/rho0
c depth scale for atmospheric thermal b.l. used by Weaver et al. (2001)
      hatmbl(1) = 8400.
      cpa = 1004.
c latent heat of vapourization (J/kg)
      hlv = 2.501e6
c latent heat of fusion of ice (J/kg)
      hlf = 3.34e5
c latent heat of sublimation (J/kg)
c     hls = 2.835e6
c for conservation of heat, require
      hls = hlv + hlf 

c scaling for heat forcing of atmosphere

      rfluxsca = rsc/(hatmbl(1)*usc*rhoair*cpa)   

c atmospheric winds

c     open(35,file='u500_ncep.silo')
      open(35,file='uncep.silo')
      read(35,*)((uatm(1,i,j),i=1,imax),j=1,jmax)
      close(35)

c     open(35,file='v500_ncep.silo')
      open(35,file='vncep.silo')
      read(35,*)((uatm(2,i,j),i=1,imax),j=1,jmax)
      close(35)

c conditional zonal average

      do j=1,jmax
         if(j.le.2.or.j.ge.jmax-1)then
         do l=1,2
            tv = 0.
            do i=1,imax
               tv = tv + uatm(l,i,j)
            enddo
            tv = tv / imax
            do i=1,imax
               uatm(l,i,j) = tv
            enddo
         enddo
         endif
      enddo

c remove zonal average of v else fail mass conservation (may not be 
c disastrous).

      do i=1,imax
         do j=1,jmax
            uatm(1,i,j) = uatm(1,i,j)/usc
            uatm(2,i,j) = uatm(2,i,j)/usc
c           uatm(2,i,j) = uatm(2,i,j)*0.0
         enddo
         uatm(2,i,jmax) = 0.
      enddo

c parameters for extra heat diffusion where pptn high

c     diffmod0 = 60e6
      diffmod0 = 0.
c     ppmin = 2./(365.*86400.)
c     ppmax = 4./(365.*86400.)
      ppmin = 2./syr
      ppmax = 4./syr

c nre simpler diffusivity

      diffend = exp(-(0.5*pi/diffwid)**2)
c     open(47,file='diff_ADVDIF.dat')
      do j=1,jmax
         tv = asin(s(j))
         tv2 = asin(sv(j))
c Weaver diffusivities as interpolated by Bob
c        read(47,'(4x,5e15.5)')tv3,diffa(1,1,j)
c    1   ,diffa(2,1,j),diffa(1,2,j),diffa(2,2,j)
         diffa(2,1,j) = diffamp(2)
         diffa(2,2,j) = diffamp(2)
c Weaver-type diffusivities but analytical  
c        diffa(1,1,j) = diffamp(1)*(0.02 + 0.2*(tv+0.5*pi)/pi      
c    &                + 0.8*0.5*(1 + cos(2*tv)))
c        diffa(1,2,j) = diffamp(1)*(0.02 + 0.2*(tv2+0.5*pi)/pi      
c    &                + 0.8*0.5*(1 + cos(2*tv2)))
c Weaver-type diffusivities, symmetrical 
c        diffa(1,1,j) = diffamp(1)*(0.2 + 0.8*0.5*(1 + cos(2*tv)))
c        diffa(1,2,j) = diffamp(1)*(0.2 + 0.8*0.5*(1 + cos(2*tv2)))
c Variable-width and asymmetry (or slope) thermal diffusivity
         diffa(1,1,j) = diffamp(1)*(difflin*2.0*(tv+0.5*pi)/pi
     &              + (1.0-difflin)*(exp(-(tv/diffwid)**2) - diffend)
     &              /(1.0 - diffend))
         diffa(1,2,j) = diffamp(1)*(difflin*2.0*(tv2+0.5*pi)/pi
     &              + (1.0-difflin)*(exp(-(tv2/diffwid)**2) - diffend)
     &              /(1.0 - diffend))
c non-dimensionalise diffusivities

         diffa(1,1,j) = diffa(1,1,j)/(rsc*usc)
         diffa(1,2,j) = diffa(1,2,j)/(rsc*usc)
         diffa(2,1,j) = diffa(2,1,j)/(rsc*usc)
         diffa(2,2,j) = diffa(2,2,j)/(rsc*usc)
c        write(47,'(i4,5e15.5)')j,asin(s(j)),diffa(1,1,j)*rsc*usc
c    1   ,diffa(2,1,j)*rsc*usc,diffa(1,2,j)*rsc*usc,diffa(2,2,j)*rsc*usc
      enddo
c     close(47)

c scale height for specific humidity (Peixoto and Oort 1992)
      hatmbl(2) = 1800.

c consts for saturation specific humidity (Bolton 1980)

      const1 = 3.80*1e-3
      const2 = 21.87
      const3 = 265.5
      const4 = 17.67
      const5 = 243.5

c threshold relative humidity

      rmax = 0.85

c scaling for P-E forcing of atmosphere

c     rpmesca = rsc*rho0/(dsc*usc*rhoair)
      rpmesca = rsc*rho0/(hatmbl(2)*usc*rhoair)

c reconstruct surface wind field for bulk turbulent transfer and
c zonally average near poles as for uatm for stability

c     open(55,file='usurf.dat')

      cd = 0.0013

      do j=1,jmax
         tv3 = 0.
         do i=1,imax
            if(i.eq.1) then
               tv = (tau(1,i,j)+tau(1,imax,j))/2
            else
               tv = (tau(1,i,j)+tau(1,i-1,j))/2
            endif
            if(j.eq.1) then
               tv2 = tau(2,i,j)/2
            else
               tv2 = (tau(2,i,j)+tau(2,i,j-1))/2
            endif
            usurf(i,j) = sqrt((sqrt(tv**2 + tv2**2))
     1          *rh0sc*dsc*usc*fsc/(rhoair*cd*scf))
c    1          *rh0sc*dsc*usc*fsc/(rhoair*cd))
            tv3 = tv3 + usurf(i,j)
         enddo
         do i=1,imax
            if(j.le.2.or.j.ge.jmax-1)usurf(i,j) = tv3/imax
c           write(55,*)usurf(i,j)
         enddo
      enddo

c     close(55)
c-----------------------------------------------------------------------
c sea ice parameters
c-----------------------------------------------------------------------

c read sea-ice parameters

      print*,'diffsic'
      read(5,*)diffsic
      print*,diffsic

c non-dimensionalise eddy diffusivity
      diffsic = diffsic/(rsc*usc)
      
c freezing temperature for average seawater (deg C)
      tsic = -1.8
c constant ice conductivity (W/m/K)
      consic = 2.166
c in parameterization of heat flux at base of sea ice:
c empirical constant
      ch_ice = 0.0058
c skin friction velocity (m/s)
      u_tau_ice = 0.02
c specific heat of sea water under ice at constant pressure (J/kg/K)
      cpo_ice = 4044
c representative ice density (kg/m**3)
      rhoice = 913.
c representative under-seaice water density (kg/m**3)
c     rho0sea = 1035.
c useful constant proportional to inverse timscale for surface freezing
c     rsictscsf = ch_ice*u_tau_ice*rho0sea*cpo_ice
      rsictscsf = ch_ice*u_tau_ice*rho0*cpo_ice
      print*,'rsictscsf = ',rsictscsf
      rsictscsf = dsc*dz(kmax)*rho0*cpo_ice/(17.5*86400.0)
      print*,'rsictscsf = ',rsictscsf
c minimum average sea-ice thickness over a grid cell
      hmin = 0.01
      rhmin = 1.0/hmin
c density ratios
      rhooi = rho0/rhoice 
      rhoio = rhoice/rho0 
c melting factor
      rrholf = 1.0/(rhoice*hlf)

c read initial atmos state
      print*,'tatm relh0_ocean relh0_land'
      read(5,*)tatm,relh0_ocean,relh0_land
      print*,tatm,relh0_ocean,relh0_land

c read implicit Atlantic-to-Pacific freshwater fluxes in south Atlantic,
c tropical Atlantic and north Atlantic: extra1a, extra1b, extra1c Sv

      print*,'extra1a extra1b extra1c'
      read(5,*)extra1a,extra1b,extra1c
      print*,extra1a,extra1b,extra1c

ccc use extra1a, extra1b, extra1c, basins data to set up P-E adjustments

c find total no. of Pac/Atl gridboxes

c in south Atlantic (to 20 deg S)
      npac1a = 0
      natl1a = 0
      do j=9,12
cAR   do j=5,6 
         npac1a = npac1a + ipf(j) - ips(j) + 1
         natl1a = natl1a + iaf(j) - ias(j) + 1
      enddo

c in tropical Atlantic (20 deg S to 24 deg N)
      npac1b = 0
      natl1b = 0
      do j=13,25
cAR   do j=7,12 
         npac1b = npac1b + ipf(j) - ips(j) + 1
         natl1b = natl1b + iaf(j) - ias(j) + 1
      enddo

c in north Atlantic (north of 24 deg N) NB INCLUDES DRY POINTS
      npac1c = 0
      natl1c = 0
      do j=26,jmax
cAR   do j=13,jmax
         do i=ips(j),ipf(j)
            if(k1(i,j).le.kmax)npac1c = npac1c + 1
         enddo
         do i=ias(j),iaf(j)
            if(k1(i,j).le.kmax)natl1c = natl1c + 1
         enddo
      enddo

      print*,natl1a, npac1a, natl1b, npac1b, natl1c, npac1c 
      print*,'natl1a, npac1a, natl1b, npac1b, natl1c, npac1c '

ccc increase/decrease P-E in Pacific/Atlantic as in Broecker (1991)
ccc [after Oort 1983]: net freshwater loss by Atlantic = 0.32 Sv
ccc here add/remove total extra1a, extra1b, extra1c Sv of freshwater
ccc equally by area in Pac/Atl resp.

      do j=1,jmax
         do i=1,imax
            pmeadj(i,j) = 0.
         enddo
      enddo

      do j=9,12
cAR   do j=5,6 
         do i=ips(j),ipf(j)
            pmeadj(i,j) = 1e6*extra1a/(npac1a*asurf)
         enddo
         do i=ias(j),iaf(j)
            pmeadj(i,j) = -1e6*extra1a/(natl1a*asurf)
         enddo
      enddo

      do j=13,25
cAR   do j=7,12 
         do i=ips(j),ipf(j)
            pmeadj(i,j) = 1e6*extra1b/(npac1b*asurf)
         enddo
         do i=ias(j),iaf(j)
            pmeadj(i,j) = -1e6*extra1b/(natl1b*asurf)
         enddo
      enddo

      do j=26,jmax
cAR   do j=13,jmax
         do i=ips(j),ipf(j)
            if(k1(i,j).le.kmax)
     1         pmeadj(i,j) = 1e6*extra1c/(npac1c*asurf)
         enddo
         do i=ias(j),iaf(j)
            if(k1(i,j).le.kmax)
     1         pmeadj(i,j) = -1e6*extra1c/(natl1c*asurf)
         enddo
      enddo

ccc initialize atmosphere

      do j=1,jmax
         do i=1,imax

c initial air temperatures

            tq(1,i,j) = tatm
            tq1(1,i,j) = tq(1,i,j)

c initial specific humidities
c set to relh0_ocean*qsat_ocean over ocean and relh0_land*qsat_atmos over land

            if(k1(i,j).le.kmax)then
               if(ts1(1,i,j,kmax).gt.tsic) then
                  tq(2,i,j) = relh0_ocean*const1*
     1            exp(const2*ts1(1,i,j,kmax)/(ts1(1,i,j,kmax)+const3))
               else
                  tq(2,i,j) = relh0_ocean*const1*
     1            exp(const4*ts1(1,i,j,kmax)/(ts1(1,i,j,kmax)+const5))
               endif
            else
               if(tq1(1,i,j).gt.0.0) then
                  tq(2,i,j) = relh0_land*const1*exp(const2
     1               *tq1(1,i,j)/(tq1(1,i,j)+const3))
               else
                  tq(2,i,j) = relh0_land*const1*exp(const4
     1               *tq1(1,i,j)/(tq1(1,i,j)+const5))
               endif
            endif

            tq1(2,i,j) = tq(2,i,j)

c other stuff
            qb(i,j) = 0.
            evap(i,j) = 0.
            fx0neto(i,j) = 0.

ccc initialize  sea ice

c thickness ,fractional area and temperature

            varice(1,i,j) = 0.
            varice1(1,i,j) = varice(1,i,j)
            varice(2,i,j) = 0.
            varice1(2,i,j) = varice(2,i,j)
            tice(i,j) = 0.

c rate of change due to thermodynamics

            dtha(1,i,j) = 0.
            dtha(2,i,j) = 0.

c other stuff
          
            evapsic(i,j) = 0.
            fx0sic(i,j) = 0.
            fxlw(i,j) = 0.
            fxsen(i,j) = 0.

#ifdef dosc
c v2 seasonal. Annual averages

            do l=1,2
               tqavg(l,i,j) = 0.
               haavg(l,i,j) = 0.
            enddo
            relhavg(i,j) = 0.
            ticeavg(i,j) = 0.
#endif

         enddo
      enddo

ccc set up runoff catchment data

      call readroff

c diagnostic calculation of global heat source

      ghs = 0.

      end
