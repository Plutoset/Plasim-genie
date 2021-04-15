c radfor.F fortran routine to calculate radiative forcing for c-goldstein
c started 30/5/3 Neil R. Edwards
c loosely based on Peter Cox's SUNNY.f or see Peixoto and Oort 1992 p.99
c
c nyear = no. dt per year
c osct  = angular time of year (0,..,2pi)
c oscsind = sin of declination
c oscsob = sin of obliquity
c osce = eccentricity
c oscgam = ??
c oscday = 0.5 * sunlit (angular) fraction of day, ie 1/2 length of day
c solfor = solar forcing = scaling factor * integral of cosine of solar 
c          elevation during daylight
c
      subroutine radfor(iistep)

      include 'var.cmn'
#ifdef ents
      include '../genie-simpleland/var_ents.cmn'
      include '../genie-simpleland/tim_ents.cmn'

      real alboavg(maxj)
#endif

      integer istep, j
      real rpi, osce, oscsob, oscgam, tv, osce1, osce2, osce3, osce4
      real oscryr, osct, oscv, oscsolf, oscsind, oscss, osccc, osctt 
      real oscday, solavg(maxj),osctau0,osctau1
      integer iistep
#ifdef ents
      integer time_1,time_2
      real time_frac
#endif

      character*7 iistep_string

      osce=0.0167
      oscsob=0.397789
      oscgam=1.352631
c     The value in the UM:
c      osctau0=2.5
c     The value to retrieve previous c-goldstein results:
      osctau0=-0.5

c     overwrite these values if t_orbit=2 or 1

#ifdef ents

      if (t_orbit.eq.2) then
        osce=orbitecc_vect(1)
        oscsob=orbitobl_vect(1)
        oscgam=orbitpre_vect(1)
        osctau0=orbittau_vect(1)
        print*,'orbitvars:',iistep
        print*,'orbitosce,oscsob:',osce,oscsob
        print*,'orbitoscgam,orbitosctau0',oscgam,osctau0
      endif

      if (t_orbit.eq.1) then
        time_1=int(iistep/real(orbitsteps))+1
        time_2=time_1+1
        time_frac=(mod(iistep,orbitsteps))/real(orbitsteps)
        if (time_2.le.norbit) then
          osce=(1-time_frac)*orbitecc_vect(time_1)+
     &                time_frac*orbitecc_vect(time_2)
          oscsob=(1-time_frac)*orbitobl_vect(time_1)+
     &                time_frac*orbitobl_vect(time_2)

          if (abs(orbitpre_vect(time_1)-
     &        orbitpre_vect(time_2)).gt.pi) then

            if (orbitpre_vect(time_1).gt.orbitpre_vect(time_2)) then
              oscgam=mod((1-time_frac)*orbitpre_vect(time_1)+
     &                time_frac*(orbitpre_vect(time_2)+2*pi),2*pi)
            else
              oscgam=mod((1-time_frac)*(orbitpre_vect(time_1)+2*pi)+
     &                time_frac*(orbitpre_vect(time_2)),2*pi)
            endif
  
          else
          oscgam=(1-time_frac)*orbitpre_vect(time_1)+
     &                time_frac*orbitpre_vect(time_2)
          endif

          if (abs(orbittau_vect(time_1)-
     &        orbittau_vect(time_2)).gt.daysperyear/2.0) then

            if (orbittau_vect(time_1).gt.orbittau_vect(time_2)) then
              osctau0=mod((1-time_frac)*orbittau_vect(time_1)+
     &                time_frac*(orbittau_vect(time_2)+
     &                daysperyear),daysperyear)
            else
              osctau0=mod((1-time_frac)*
     &                (orbittau_vect(time_1)+daysperyear)+
     &                time_frac*(orbittau_vect(time_2)),daysperyear)
            endif


          else
          osctau0=(1-time_frac)*orbittau_vect(time_1)+
     &                time_frac*orbittau_vect(time_2)
          endif

        else 
          if (time_frac.ne.0) print*,'Time out of bounds for orbit'
          osce=orbitecc_vect(norbit)
          oscsob=orbitobl_vect(norbit)
          oscgam=orbitpre_vect(norbit)
          osctau0=orbittau_vect(norbit)
        endif

      if (mod(iistep-1,10000).eq.0) then
        print*,'orbitvars:',iistep,time_1,time_frac
        print*,'orbitosce,oscsob:',osce,oscsob
        print*,'orbitoscgam,orbitosctau0',oscgam,osctau0
      endif

      endif
#endif

c      open(1,file='oscsun.dat')
c      open(1,file='oscalbo.dat')

      rpi = 1.0/pi
      solconst = 1368.0

      tv = osce*osce
      osce1 = osce * (2.0 - 0.25*tv)
      osce2 = 1.25 * tv 
      osce3 = osce*tv * 13./12.
      osce4 = ((1.0 + 0.5*tv)/(1.0 - tv))**2 
      oscryr = 2.0*pi/float(nyear)
      osctau1 = osctau0 + 0.5

      do istep=1,nyear
         osct = ( float(mod(istep-1,nyear)+1) -
     &       nyear*osctau1/daysperyear ) *oscryr
         do j=1,jmax
            oscv = osct + osce1*sin(osct) + osce2*sin(2.0*osct) 
     &           + osce3*sin(3.0*osct)
            oscsolf = osce4*(1.0 + osce*cos(oscv))**2
            oscsind = oscsob*sin(oscv-oscgam)

            oscss = oscsind * s(j)
            osccc = sqrt(1.0 - oscsind**2) * c(j)
            osctt = min(1.0,max(-1.0,oscss/osccc))
            
            oscday = acos(- osctt)

            solfor(j,istep) = solconst*oscsolf*rpi*(oscss*oscday
     &                      + osccc*sin(oscday))
#ifdef ents
            call ocean_alb(oscss,osccc,oscday,j,istep)
#endif
c           write(1,'(e15.5)')solfor(j,istep)
c           write(1,'(e15.5)')albo(j,istep)
         enddo
      enddo

#ifdef dosc
#else
c
c replace variable forcing by its average
c
      do j=1,jmax
         solavg(j) = 0.
         do istep=1,nyear
            solavg(j) = solavg(j) + solfor(j,istep)
#ifdef ents
            alboavg(j) = alboavg(j) + albo(j,istep)
#endif
         enddo
      enddo
      do j=1,jmax
         do istep=1,nyear
            solfor(j,istep) = solavg(j)/nyear
#ifdef ents
            albo(j,istep) = alboavg(j)/nyear
#endif
         enddo
      enddo
#endif

c      close(1)


      end
