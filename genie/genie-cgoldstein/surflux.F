*
* subroutine surflux.f for program goldstein introduced 2/8/01  
* computes heat and freshwater fluxes at atmosphere-ocean/seaice/land interface
* iteration scheme for tice improved nre 18/6/3
* edited for seasonal forcing using istot = total number of timesteps
* Added #ifdefs for land-atmosphere fluxes and changed filename from 
*   surflux.f to surflux.F 25/8/03 pph
* 
      subroutine surflux(istot,istep)

      include 'var.cmn'
#ifdef dland
      include '../genie-land/var.land.cmn'
#endif
#ifdef ents
      include '../genie-simpleland/var_ents.cmn'
      include '../genie-simpleland/tim_ents.cmn'

      real albl,beta,tolld
      real tldlast,dtld,dtld1
      real dfxsens,dfxlw,devap,dqsato
      real dum_talt,dum_hum,dum_pptn
      real dqalt,qalt,q0,qsalt
      real asoil,fxswsica

      real co2_exact
      real orog_exact(maxi,maxj)
      real lice_exact(maxi,maxj)
      integer time_1,time_2
      real time_frac

      integer itld,devapcheck,itldmax

      parameter( itldmax = 21, tolld = 1e-3)
#endif

c      real ce, ch, cesic, chsic, rq, tv0,tv1,tv2,tv3, tol, set0
      real ce, ch, cesic, chsic,  tv0,tv1,tv2,tv3, tol, set0
      real albsic, fxswsic , fxlwsic, fxsensic , fx0oa, fx0sica
      real qsatsic, zeroc, alw, ticold, cfxsensic, salt, dho, dhsic
      real dhadj, tieqn, dtieq
      real deltq,talt,lmd
     
      parameter(zeroc = 273.15)

      integer i, j, l, iter, itice, istot, istep

      parameter(itice = 21 , tol=1e-10)

#ifdef ents
c If we have ents running, then calculate the 
c   atmospheric co2 concentration, orog, and icemask  if they're varying.
      if (t_co2.eq.1) then
        time_1=int(istot/real(co2steps))+1
        time_2=time_1+1
        time_frac=(mod(istot,co2steps))/real(co2steps)
        if (time_2.le.nco2) then
          co2_exact=(1-time_frac)*co2_vect(time_1)+
     &                time_frac*co2_vect(time_2)
        else 
          if (time_frac.ne.0) print*,'Time out of bounds for co2'
          co2_exact=co2_vect(nco2)
        endif
        if (mod(istot,10000).eq.0) then
          print*,'co2:',istot,time_1,time_frac,co2_exact
        endif
      endif
      if (t_orog.eq.1) then
        time_1=int(istot/real(orogsteps))+1
        time_2=time_1+1
        time_frac=(mod(istot,orogsteps))/real(orogsteps)
        if (time_2.le.norog) then
          orog_exact(:,:)=(1-time_frac)*orog_vect(:,:,time_1)+
     &                time_frac*orog_vect(:,:,time_2)
        else
          if (time_frac.ne.0) print*,'Time out of bounds for orog'
          orog_exact(:,:)=orog_vect(:,:,norog)
        endif
        if (mod(istot,10000).eq.0) then
          print*,'orog:',istot,time_1,time_frac,
     &        sum(orog_exact)/size(orog_exact)
        endif
      endif
      if (t_lice.eq.1) then
        time_1=int(istot/real(licesteps))+1
        time_2=time_1+1
        time_frac=(mod(istot,licesteps))/real(licesteps)
        if (time_2.le.nlice) then
          lice_exact(:,:)=(1-time_frac)*lice_vect(:,:,time_1)+
     &                time_frac*lice_vect(:,:,time_2)
        else
          if (time_frac.ne.0) print*,'Time out of bounds for lice'
          lice_exact(:,:)=lice_vect(:,:,nlice)
        endif
        if (mod(istot,10000).eq.0) then
          print*,'lice:',istot,time_1,time_frac,
     &        sum(lice_exact)/size(lice_exact)
        endif
      endif
#endif




c initialize integrated runoff
      do i=1,imax
         do j=1,jmax
            runoff(i,j) = 0.
         enddo
      enddo

#ifdef ents
#ifdef icemelt
cmsw Add previous year's ice melt from Greenland to
cmsw ocean boxes around Greenland equally. Only works
cmsw for 36*36 model grid.
      if(icemeltfwfswitch.eq.1)then
         runoff(22,34)=glmelt
         runoff(23,34)=glmelt
         runoff(24,34)=glmelt
         runoff(22,33)=glmelt
         runoff(23,33)=glmelt
         runoff(24,33)=glmelt
      endif
#endif
#endif

ccc main i,j loop to compute surface flux terms

      do i=1,imax
         do j=1,jmax

cmsw
cmsw Optional effect of altitude in ENTS.
cmsw Air temp calculated at altitude using lapse
cmsw rate. This air temp used in calc.s of SVP and
cmsw longwave radiation.
cmsw
#ifdef ents
            if(orogswitch.eq.1)then
               talt=tq1(1,i,j)+(lapse*orog(i,j))
            else
               talt=tq1(1,i,j)
            endif

            lmd = lambda
#else
            talt=tq1(1,i,j)
            lmd = 1.
#endif

c Precipitation over land and ocean

            qsata(i,j) = const1*exp(const4*talt
     1                         /(talt+const5))
            deltq = lmd*(tq1(2,i,j)-rmax*qsata(i,j))

            pptn(i,j) = max(0.0,(deltq
     1               *rhoao*hatmbl(2)*rdtdim))

c nre instantaneous precipitation
c nre 6/10/3 set tq=tq1 for tstipa
c calc relative humidity rq after P not before

            tq1(2,i,j) = min(tq1(2,i,j),tq1(2,i,j)-deltq)

            tq(2,i,j) = tq1(2,i,j)

c            rq = tq1(2,i,j)/qsata(i,j)
            relh(i,j) = tq1(2,i,j)/qsata(i,j)

c use climatological albedo in calculating incoming shortwave radiation
#ifdef ents
            albedo(i,j) = atm_alb(i,j,imth)
c            relh(i,j)=rq
#else
            albedo(i,j) = albcl(i,j)
#endif
c shortwave radiation

c           fxsw(i,j) = solfor(j)*(1. - albedo(i,j))
c cgv2 seasonal
            fxsw(i,j) = solfor(j,mod(istot-1,nyear)+1)
     1                  *(1.- albedo(i,j))

c outgoing planetary longwave

            tv0 = b00 + relh(i,j)*(b10 + b20*relh(i,j))
            tv1 = b01 + relh(i,j)*(b11 + b21*relh(i,j))
            tv2 = b02 + relh(i,j)*(b12 + b22*relh(i,j))
            tv3 = b03 + relh(i,j)*(b13 + b23*relh(i,j))

c update co2 concentration should be moved to mains or tstepa??
c compound increase at fractional rate co2_rate (read in gseta.f)
            
            co2(i,j) = co2(i,j) + rate_co2*co2(i,j)

#ifdef ents
c If we have ents running, then add the possibility of
c   changing co2, orog, and lice depending on the current time.
      if (t_co2.eq.1) then
        co2(i,j) = co2_exact
      endif
      if (t_orog.eq.1) then
        orog(i,j) = orog_exact(i,j)
      endif
      if (t_lice.eq.1) then
        slicemask(i,j) = ceiling(lice_exact(i,j))
      else
        lice_exact(i,j) = slicemask(i,j)
      endif
#endif


            fxplw(i,j) = tv0 + talt*(tv1 
     1                        + talt*(tv2 
     2                        + talt*tv3))
     3                        - delf2x*log(co2(i,j)/co20)

c latent heat flux into atmos associated with condensation 
c nre with no account taken of snow melting, must assume all pptn is rain

            fxlata(i,j) = rho0*pptn(i,j)*hlv

c-----------------------------------------------------------------------
c calculate terms over ocean or ice
c-----------------------------------------------------------------------
            if(k1(i,j).le.kmax) then

c longwave radiation 

               alw = tq1(1,i,j)+zeroc
               alw = alw * alw
               alw = alw * alw
               alw = ema * alw

c surface salinity-dependent freezing point:

               salt = saln0+ts1(2,i,j,kmax)
c              salt = ts1(2,i,j,kmax)
               tsfreez(i,j) = salt*(-0.0575 + 0.0017*sqrt(salt)
     1                              - 0.0002*salt)
c or constant:
c              tsfreez(i,j) = tsic

c maximum amount of heat available in first layer
c nre rsictscsf must be changed if dt>17.5 days, see gseta

               qb(i,j) = rsictscsf*(tsfreez(i,j)-ts1(1,i,j,kmax))


c-----------------------------------------------------------------------
c calculate terms over ice
c-----------------------------------------------------------------------
               if(varice1(2,i,j).gt.0.0)then

c let albedo over sea ice vary as a function of tair (Holland et al. 1993)

c                 albsic = max(0.20,min(0.6,0.40 - 0.04*tq1(1,i,j)))
c for snowball tests
                  albsic = max(0.20,min(0.7,0.40 - 0.04*tq1(1,i,j)))
c                 fxswsic = solfor(j)*(1. - albsic)
c cgv2 seasonal
                  fxswsic = solfor(j,mod(istot-1,nyear)+1)*(1. - albsic)
#ifdef ents
     1                      *(1.-albedo(i,j))

cmsw Added variable to fix wrong application of fxswsic in flux to atm. 14/12/04

                  fxswsica = solfor(j,mod(istot-1,nyear)+1)
     1                       *(1.-albedo(i,j))
#endif

c first need to calculate T_ice

                  do iter=1,itice
                     ticold = tice(i,j)
c Dalton number
                     cesic = 1.0e-3*(1.0022 - 0.0822*(tq1(1,i,j)
     1                     - ticold) + 0.0266*usurf(i,j))
                     cesic = max(6.0e-5,min(2.19e-3,cesic))

                     chsic = 0.94*cesic

c sensible heat flux 
                     cfxsensic = rhoair*chsic*cpa*usurf(i,j)

                     qsatsic = const1*exp(const2*ticold         
     1                            /(ticold + const3))

                     evapsic(i,j) = max(0.0,(qsatsic - tq1(2,i,j))
     1                         *rhoao*cesic*usurf(i,j))

                     tieqn = varice1(1,i,j)*((1-ca(i,j))*fxswsic + alw 
     1                     - emo*(ticold+zeroc)**4  - cfxsensic*(ticold 
     2                     - tq1(1,i,j)) - rho0*hls*evapsic(i,j) )
     3                     + consic*(tsfreez(i,j)-ticold)

                     dtieq = varice1(1,i,j)*( 
     1                     - 4.0*emo*(ticold+zeroc)**3 - cfxsensic
     1                     - hls*rhoair*cesic*usurf(i,j)*qsatsic*const2
     2                     *const3/((ticold + const3)**2)
     4                     *0.5*(1.0 + sign(1.0,qsatsic - tq1(2,i,j))) )
     5                     - consic

                     tice(i,j) = ticold - tieqn/dtieq

c keep old code until comparison done
c                    tice(i,j) = (consic*tsfreez(i,j) + varice1(1,i,j)*
c    1                      (cfxsensic*tq1(1,i,j)
c    2                      + (1-ca(i,j))*fxswsic + alw 
c    3                      - rho0*hls*evapsic(i,j) 
c    4                      + emo*(ticold+zeroc)**3*(3.0*ticold-zeroc))
c    5                      )/(consic + varice1(1,i,j)*(cfxsensic 
c    6                      + 4.0*emo*(ticold+zeroc)**3))

c                if(i.eq.1)print*,istot,j,iter
c    &                ,tice(i,j),ticold,tice(i,j)-ticold,tieqn,dtieq

                     if(abs(tice(i,j) - ticold).lt.tol .or.     
     1                  ticold.gt.tfreez.and.tieqn.gt.0.0)goto 10
                  enddo

       print*,'warning sea-ice iteration failed at',istot,i,j
     &                ,tice(i,j),ticold,tice(i,j)-ticold,tieqn,dtieq
   10             tice(i,j) = min(tfreez,tice(i,j))

c recalc everything in case of resetting of tice

                  fxlwsic = emo*(tice(i,j)+zeroc )**4 - alw

                  cesic = 1.0e-3*(1.0022 - 0.0822*(tq1(1,i,j)
     1                  - tice(i,j)) + 0.0266*usurf(i,j))
                  cesic = max(6.0e-5,min(2.19e-3,cesic))

                  chsic = 0.94*cesic

                  cfxsensic = rhoair*chsic*cpa*usurf(i,j)

                  fxsensic = cfxsensic*(tice(i,j) - tq1(1,i,j))

                  qsatsic = const1*exp(const2*tice(i,j)      
     1                            /(tice(i,j) + const3))

                  evapsic(i,j) = max(0.0,(qsatsic - tq1(2,i,j))
     1                         *rhoao*cesic*usurf(i,j))

                  fx0sic(i,j) = (1-ca(i,j))*fxswsic -fxsensic
     1                        - fxlwsic - rho0*hls*evapsic(i,j)

#ifdef ents
cmsw Added to fix application of albedo over sea-ice. 14/12/04
                  fx0sica = ca(i,j)*fxswsica
#else
                  fx0sica = ca(i,j)*fxswsic 
#endif
     1                     + fxlata(i,j)
     2                     + fxsensic + fxlwsic
     3                     - fxplw(i,j)

                  dhsic = rrholf*(qb(i,j) - fx0sic(i,j)) 
     1                  - rhooi*evapsic(i,j)

               else
                  fx0sica = 0.0
                  dhsic = 0.
                  evapsic(i,j) = 0.
                  tice(i,j) = 0.
               endif

c-----------------------------------------------------------------------
c over open ocean
c-----------------------------------------------------------------------

               fxlw(i,j) = emo*(ts1(1,i,j,kmax)+zeroc)**4 - alw

c Dalton number

               ce = 1.0e-3*(1.0022 - 0.0822*(tq1(1,i,j)-
     1              ts1(1,i,j,kmax)) + 0.0266*usurf(i,j))
               ce = max(6.0e-5,min(2.19e-3,ce))

               ch = 0.94*ce

c sensible heat flux from ocean to atmosphere

	       fxsen(i,j) = rhoair*ch*cpa*usurf(i,j)*
     1                  (ts1(1,i,j,kmax)-tq1(1,i,j))

c evaporation/sublimation rate 

               qsato(i,j) = const1*exp(const4*ts1(1,i,j,kmax)
     1                         /(ts1(1,i,j,kmax)+const5))

               evap(i,j) = max(0.0,(qsato(i,j) - tq1(2,i,j))
     1                   *rhoao*ce*usurf(i,j))

c net heat flux into atmosphere

               fx0oa = ca(i,j)*fxsw(i,j) + fxlata(i,j) + fxsen(i,j) 
     1               + fxlw(i,j) - fxplw(i,j)

c add proportions over open ocean and sea ice

               fx0a(i,j) = (1-varice1(2,i,j))*fx0oa
     1                      + varice1(2,i,j)*fx0sica

c heat flux from atmosphere into open ocean 

#ifdef ents
               fx0o(i,j) = ((1-ca(i,j))*fxsw(i,j)*
     1             (1.-albo(j,imth)))
     2                   - fxsen(i,j)
     3                   - fxlw(i,j) - rho0*hlv*evap(i,j)
#else
               fx0o(i,j) = (1-ca(i,j))*fxsw(i,j) - fxsen(i,j)
     1                   - fxlw(i,j) - rho0*hlv*evap(i,j)
#endif

c net heat flux into ocean from atmosphere and sea ice
c including possible ice growth over open ocean

               fx0neto(i,j) = varice1(2,i,j)*qb(i,j)
     1                      + (1-varice1(2,i,j))*max(qb(i,j)
     2                      ,fx0o(i,j))

               dho = max(0.0,rrholf*(qb(i,j) - fx0o(i,j)))

               dtha(1,i,j) = varice1(2,i,j)*dhsic 
     1                     + (1-varice1(2,i,j))*dho

               dtha(2,i,j) = max(0.0,rhmin*dho*(1-varice1(2,i,j)))
c              if(varice1(1,i,j).gt.1e-12) dtha(2,i,j) = dtha(2,i,j)
c    1                     + min(0.0,0.5*varice1(2,i,j)*dtha(1,i,j)
c    2                     /varice1(1,i,j))
c nre 28/10/2 we now think this should be 
               if(varice1(1,i,j).gt.1e-12) dtha(2,i,j) = dtha(2,i,j)
     1                     + min(0.0,0.5*varice1(2,i,j)*varice1(2,i,j)
     2                     * dhsic/varice1(1,i,j))

cmsw Calculate planetary albedo over ocean boxes
#ifdef ents
               albs(i,j)=(albo(j,imth)
     1         - (varice1(2,i,j)*(albo(j,imth)-albsic)))
               palb(i,j)=(1.-albedo(i,j))*(albo(j,imth)
     1         - (varice1(2,i,j)*(albo(j,imth)-albsic)))
     2         + albedo(i,j)
#endif

c global heat source diagnostic

               ghs = ghs + varice1(2,i,j)*(fx0sic(i,j) + fx0sica)
     1             + (1-varice1(2,i,j))*(fx0o(i,j) + fx0oa)
     2             + rhoice*hlf*(dtha(1,i,j)
     3             + evapsic(i,j)*varice1(2,i,j)*rhooi)
            else
c-----------------------------------------------------------------------
c calculate terms over land
c-----------------------------------------------------------------------
#ifdef dland
c longwave radiation
               alw = tq1(1,i,j)+zeroc
               alw = alw * alw
               alw = alw * alw
               alw = ema * alw

cpph Net longwave over land (positive upward)
               fxlw(i,j) = eml*(tstarl(i,j)**4) - alw

ccc new formulation with introduction of land (Bob, 19/3/03)
               fx0a(i,j) = ca(i,j)*fxsw(i,j) + fxlata(i,j)
     1                   + fxlw(i,j) + fxsen(i,j) - fxplw(i,j)

               fx0o(i,j) = (1-ca(i,j))*fxsw(i,j) - fxsen(i,j)
     1                   - fxlw(i,j) - rho0*hlv*evap(i,j)

ccc replace pptn with gravitational drainage from LAND (Bob, 19/3/03)
               if(drain(i,j).gt.0.0) then
                  runoff(iroff(i,j),jroff(i,j)) = 
     1               runoff(iroff(i,j),jroff(i,j)) + drain(i,j)
               endif

c Update global heat souce over land
               ghs = ghs + fx0a(i,j) + fx0o(i,j)

#else

c-----------------------------------------------------------------------
c ENTS radiation and hydrology
c-----------------------------------------------------------------------
#ifdef ents

cmsw Offline model: switch variables

               if(offlineswitch.eq.1)then
                  dum_talt=talt
                  dum_hum=tq1(2,i,j)
                  dum_pptn=pptn(i,j)
cmsw Replace with offline data
                  if(orogswitch.eq.1)then
                     talt=tncep(i,j,imth)+(lapse*orog(i,j))
                  else
                     talt=tncep(i,j,imth)
                  endif
                  qsalt=const1*exp(const4*talt
     1                           /(talt+const5))
                  tq1(2,i,j)=rhncep(i,j,imth)*qsalt
                  pptn(i,j)=pncep(i,j,imth)
               endif

cmsw Snow

               if(tqld(1,i,j).lt.-5.and.talt.lt.-5.
     1            and.pptn(i,j).gt.0.)then
                 snow(i,j)=1
               endif

cmsw Snow melt

               if(tqld(1,i,j).ge.-5.and.talt.ge.-5.
     1            and.snow(i,j).eq.1)then
                 snow(i,j)=0
               endif

cmsw Calculate field capacity

               bcap(i,j)=min(k8,k9+(k10*Csoil(i,j)))

cmsw calculate new roughness length and transfer coefficients

               z0(i,j)=max(0.001,kz0*Cveg(i,j))
               chl(i,j)=1./(((1./0.41)*log(10./z0(i,j)))**2)
               cel(i,j)=chl(i,j)

cmsw Albedo functionality

c#ifdef fixedveg
c               fv(i,j)=fvfv(i,j)
c#endif
               asoil=max(apeat,((apeat-asand)*k10*Csoil(i,j)
     1                   /(k8-k9))+asand)
               albs(i,j)=(fv(i,j)*aveg)+((1.-fv(i,j))*asoil)

               if(snow(i,j).eq.1.and.snowswitch.eq.1)then
                  albs(i,j)=((asnow-asnowv)*exp(-k7*Cveg(i,j)))
     1                       +asnowv
               endif

               if(slicemask(i,j).eq.2)then
c#ifdef fixedveg
c                  fv(i,j)=0.
c#endif
                  albs(i,j)=albs(i,j)*(2-lice_exact(i,j)) +
     1                            0.8*(lice_exact(i,j)-1)
                  z0(i,j)=z0(i,j)*(2-lice_exact(i,j)) +
     1                            0.001*(lice_exact(i,j)-1)
                  bcap(i,j)=bcap(i,j)*(2-lice_exact(i,j)) +
     1                            k9*(lice_exact(i,j)-1)
               endif

cmsw Newton-Raphson iteration loop to solve for eqm
cmsw land temperature. Justified for 1 ocean timestep.

               do itld=1,itldmax

cmsw sensible heat flux from land to atmosphere

                  fxsen(i,j) = rhoair*chl(i,j)*cpa*usurfl(i,j,imth)*
     1                         (tqld(1,i,j)-talt)

cmsw longwave radiation
                  alw = tq1(1,i,j)+zeroc
                  alw = alw * alw
                  alw = alw * alw
                  alw = ema * alw
cmsw Net longwave over land (positive upward)
                  fxlw(i,j) = eml*((tqld(1,i,j)+zeroc)**4)
     1                      - alw

cmsw Evaporation
                  if(tqld(1,i,j).le.0.)then
                     qsato(i,j) = const1*exp(const2*tqld(1,i,j)
     1                           /(tqld(1,i,j)+const3))
                  else
                     qsato(i,j) = const1*exp(const4*tqld(1,i,j)
     1                           /(tqld(1,i,j)+const5))
                  endif

cmsw devapcheck used to find out if evap has a differential (0=N, 1=Y)
                  if(tqld(2,i,j).gt.0.)then
                     beta=min(1.,(tqld(2,i,j)/bcap(i,j))**4)
                     evap(i,j) = max(0.0,(qsato(i,j) - tq1(2,i,j))
     1                         *rhoao*cel(i,j)*usurfl(i,j,imth)*beta)

                     devapcheck=1
                  else
                     evap(i,j)=0.
                     devapcheck=0
                  endif

                  if(evap(i,j)*dtdim.gt.tqld(2,i,j))then
                     evap(i,j)=tqld(2,i,j)*rdtdim
                     devapcheck=0
                  endif

cmsw ODE for land temp wrt to time

                  dtld = rhcld*((1-ca(i,j))*fxsw(i,j)*(1-albs(i,j))
     1                  - fxsen(i,j) - fxlw(i,j) - rho0*hlv*evap(i,j))

cmsw Sensible heat derivative

                  dfxsens=rhoair*chl(i,j)*cpa*usurfl(i,j,imth)

cmsw net longwave derivative

                  dfxlw=4.*eml*((tqld(1,i,j)+zeroc)**3)

cmsw evap derivative
                 if(devapcheck.eq.0)then
                   devap=0.
                 else
                    if(tqld(1,i,j).le.0.)then
                       dqsato = ((const1*const2*const3)/
     1                         (tqld(1,i,j)+const3)**2)
     2                          *exp(const2*tqld(1,i,j)
     3                           /(tqld(1,i,j)+const3))
                    else
                       dqsato = ((const1*const4*const5)/
     1                         (tqld(1,i,j)+const5)**2)
     2                           *exp(const4*tqld(1,i,j)
     3                           /(tqld(1,i,j)+const5))
                    endif

cmsw Calculate evaporation diff. (bare soil) or evapotranspiration diff. (veg)
cmsw depending on whether carbon feedbacks on climate chosen (carbonswitch)

                    devap=rhoao*cel(i,j)*usurfl(i,j,imth)*beta
     1                    *dqsato
                 endif

cmsw total derivative

                 dtld1 = -rhcld*(dfxsens+dfxlw+
     1                   (rho0*hlv*devap))

cmsw update last value

                 tldlast=tqld(1,i,j)

cmsw calculate new value of tqld(1,i,j)

                 tqld(1,i,j)=tqld(1,i,j)-(dtld/dtld1)

                 if(abs(tldlast-tqld(1,i,j)).lt.tolld) exit

                 if(itld.eq.itldmax)then

       print*,'Land rad. calc. did not converge to specified tol'
       print*,'at point',i,j
       print*,'final value was ',tqld(1,i,j)
       print*,'last iteration value was ',tldlast
       print*,' '
                 endif

              enddo

cmsw Radiation fluxes

              fx0a(i,j) = ca(i,j)*fxsw(i,j) + fxlata(i,j)
     1                  + fxlw(i,j) + fxsen(i,j) - fxplw(i,j)

              fx0o(i,j) = (1-ca(i,j))*fxsw(i,j)*(1-albs(i,j))
     1             - fxsen(i,j) - fxlw(i,j) - rho0*hlv*evap(i,j)

cmsw Bucket land hydrology: update bucket sizes

               tqld(2,i,j)=tqld(2,i,j)+((pptn(i,j)-evap(i,j))
     1                       *dtdim)

cmsw runoff scheme: in the case of land water bucket gt bucket capacity, bcap,
cmsw find nearest ocean
cmsw gridbox/gridboxes and add the surplus as runoff there

               if(tqld(2,i,j).gt.bcap(i,j)) then
                  runoff(iroff(i,j),jroff(i,j)) =
     1               runoff(iroff(i,j),jroff(i,j))
     2               +((tqld(2,i,j)-bcap(i,j))*rdtdim)
                  tqld(2,i,j)=bcap(i,j)
               endif

cmsw Calculate planetary albedo over land

               palb(i,j)=((1.-albedo(i,j))*albs(i,j))+albedo(i,j)

cmsw Offline model: switch variables back

               if(offlineswitch.eq.1)then
                  talt=dum_talt
                  tq1(2,i,j)=dum_hum
                  pptn(i,j)=dum_pptn
               endif

c Update global heat souce over land
               ghs = ghs + fx0a(i,j) + fx0o(i,j)

c---------------------------------------------------------------

#else
c For use in goldstein versions with no land scheme

               fx0a(i,j) = fxsw(i,j) + fxlata(i,j)
     1                  - fxplw(i,j)

ccc runoff scheme: in the case of land pptn .ne. zero, find nearest ocean
ccc gridbox/gridboxes and add as runoff there

               runoff(iroff(i,j),jroff(i,j)) =
     1               runoff(iroff(i,j),jroff(i,j)) + pptn(i,j)

c Update global heat souce over land
               ghs = ghs + fx0a(i,j)

#endif

#endif
            endif

ccc perturb runoff by extra0 Sv 
c nre extra0 def'n altered to avoid unnecessary division

c over 34 grid boxes of mid and high latitude N.Atlantic

ccc ...for Stefan's intercomparison expts (NB. no compenstion elsewhere)...

c over 62 gridboxes south of convective regions (20-50N) in N.Atlantic:

c     if(j.ge.25.and.j.le.32.and.i.ge.ias(j).and.i.le.iaf(j))

c over 18 gridboxes within convective regions (50-70N) in N.Atlantic:

c           if(j.ge.33.and.j.le.35.and.i.ge.ias(j).and.i.le.iaf(j))
c    1         runoff(i,j) = runoff(i,j) + extra0

ccc end of main i,j loop

         enddo
      enddo

ccc short i,j loop to add in effects of runoff (non-local in i,j)
c  and set up fluxes

      do i=1,imax
         do j=1,jmax

c freshwater forcing in m/s open ocean P-E over ocean gridboxes
c evap zero over land, 
c ??nre P goes straight through as no snow allowed, but E is E total
c for atm model and solely E_ocean for the fwflux
c NB E_sic cancels in fwfxneto expression, it looks wrong but it isn't.

            pme(i,j) = pptn(i,j) - evap(i,j)*(1-varice1(2,i,j))
     1                  - evapsic(i,j)*varice1(2,i,j)

c non-dimensionalize surface fluxes for use in tstepa:

            tqa(1,i,j) = fx0a(i,j)*rfluxsca
c           tqa(2,i,j) = - pme(i,j)*rpmesca
c nre instantaneous precipitation
ccc new formulation with introduction of land (Bob, 20/3/03)
            tqa(2,i,j) = (evap(i,j)*(1-varice1(2,i,j))
     1                 + evapsic(i,j)*varice1(2,i,j))*rpmesca

ccc optional split timestep for atmosphere
c           tq(1,i,j) = tq1(1,i,j) + dt(kmax)*tqa(1,i,j)
c           tq(2,i,j) = tq1(2,i,j) + dt(kmax)*tqa(2,i,j)
c           tq1(1,i,j) = tq(1,i,j)
c           tq1(2,i,j) = tq(2,i,j)
c reset tqa to zero if used here (tqa subsequently used in tstepa)
c           tqa(1,i,j) = 0
c           tqa(2,i,j) = 0

c latent heat flux associated with evaporation or sublimation
c purely diagnostic variable

            fxlato(i,j) = rho0*evap(i,j)*hlv*(1-varice1(2,i,j))
     1                 + rho0*hls*evapsic(i,j)*varice1(2,i,j)
         enddo
      enddo

c update ice 

      call tstepsic

c modify heat and salinity fluxes into ocean according to sea-ice update;
c prevent <0%, >100% fractional area A and set A=H=0 if H<Hmin
c in which case add an amount -H of ice, hence need to add appropriate
c heat and freshwater fluxes.

      do j=1,jmax
         do i=1,imax
            if(kmax.ge.k1(i,j))then
               fwfxneto(i,j) = pme(i,j) + pmeadj(i,j)
     1                + runoff(i,j) - rhoio*dtha(1,i,j)
               varice(2,i,j) = max(0.0,min(1.0,varice(2,i,j)))
               if(varice(1,i,j).lt.hmin)then
                  fx0neto(i,j) = fx0neto(i,j) - varice(1,i,j)*rhoice*hlf
     1               *rdtdim
                  fwfxneto(i,j) = fwfxneto(i,j) + varice(1,i,j)*rhoio 
     1               *rdtdim
                  ghs = ghs - varice(1,i,j)*rhoice*hlf*rdtdim
                  do l=1,2
                     varice(l,i,j) = 0.
                  enddo
               endif

c upper boundary conditions for ocean 
c for v3_1 (implicit) ocean code the source is stored in ts(...,kmax+1) 
c non-dimensionalize surface fluxes for use in tstepo

               ts(1,i,j,kmax+1) = - fx0neto(i,j)*rfluxsc
               ts(2,i,j,kmax+1) = fwfxneto(i,j)*rpmesco
               ts1(1,i,j,kmax+1) = ts(1,i,j,kmax+1)
               ts1(2,i,j,kmax+1) = ts(2,i,j,kmax+1)

               do l=1,2
                  varice1(l,i,j) = varice(l,i,j)
               enddo
            endif
         enddo
      enddo

      end
