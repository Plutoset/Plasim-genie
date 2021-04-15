*
* subroutine surflux.f for program goldstein introduced 2/8/01  
* computes heat and freshwater fluxes at atmosphere-ocean/seaice/land interface
* 
      subroutine surflux

      include 'var.cmn'

c$$$      real*8 ce, ch, cesic, chsic, rq, tv0,tv1,tv2,tv3, tol, set0
      real*8 ce, ch, cesic, chsic, rq, tv0,tv1,tv2,tv3, tol
      real*8 albsic, fxswsic , fxlwsic, fxsensic , fx0oa, fx0sica
      real*8 qsatsic, zeroc, alw, ticold, cfxsensic, salt, dho, dhsic
c$$$      real*8 dhadj
     
      parameter(zeroc = 273.15)

      integer i, j, l, iter, itice

      parameter(itice = 20 , tol=1e-10  )

c initialize integrated runoff
      do i=1,imax
         do j=1,jmax
            runoff(i,j) = 0.
         enddo
      enddo

ccc main i,j loop to compute surface flux terms

      do i=1,imax
         do j=1,jmax

c pptn (over land and ocean)
c - need saturation vapour pressure, relative humidity

            qsata(i,j) = const1*exp(const4*tq1(1,i,j)
     1                         /(tq1(1,i,j)+const5))

            pptn(i,j) = max(0.0,(tq1(2,i,j) - rmax*qsata(i,j))
     1               *rhoao*hatmbl(2)*rdtdim)
c    1               *rhoao*hatmbl(2)/(tsc*dt(kmax)))

c nre instantaneous precipitation

            tq1(2,i,j) = min(tq1(2,i,j),rmax*qsata(i,j))

c relative humidity
            rq = tq1(2,i,j)/qsata(i,j)

c use climatological albedo in calculating incoming shortwave radiation

            albedo(i,j) = albcl(i,j)

c shortwave radiation

            fxsw(i,j) = solfor(j)*(1. - albedo(i,j))

c outgoing planetary longwave

            tv0 = b00 + rq*(b10 + b20*rq)
            tv1 = b01 + rq*(b11 + b21*rq)
            tv2 = b02 + rq*(b12 + b22*rq)
            tv3 = b03 + rq*(b13 + b23*rq)

c update co2 concentration should be moved to mains or tstepa??
c compound increase at fractional rate co2_rate (read in gseta.f)
            
            co2(i,j) = co2(i,j) + rate_co2*co2(i,j)

            fxplw(i,j) = tv0 + tq1(1,i,j)*(tv1 
     1                        + tq1(1,i,j)*(tv2 
     2                        + tq1(1,i,j)*tv3))
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

                  albsic = max(0.20,min(0.6,0.40 - 0.04*tq1(1,i,j)))
                  fxswsic = solfor(j)*(1. - albsic)

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

                     evapsic(i,j) = (qsatsic - tq1(2,i,j))
     1                         *rhoao*cesic*usurf(i,j)

                     tice(i,j) = (consic*tsfreez(i,j) + varice1(1,i,j)*
     1                      (cfxsensic*tq1(1,i,j)
     2                      + (1-ca(i,j))*fxswsic + alw 
     3                      - rho0*hls*evapsic(i,j) 
     4                      + emo*(ticold+zeroc)**3*(3.0*ticold-zeroc))
     5                      )/(consic + varice1(1,i,j)*(cfxsensic 
     6                      + 4.0*emo*(ticold+zeroc)**3))
                     if(abs(tice(i,j) - ticold).lt.tol) goto 10
                  enddo
c                 print*,'tice non-convergence at',
c    1                    i,j,tice(i,j),tice(i,j)-ticold

   10             tice(i,j) = min(tfreez,tice(i,j))

                  fxlwsic = emo*(tice(i,j)+zeroc )**4 - alw

                  fxsensic = cfxsensic*(tice(i,j) - tq1(1,i,j))

                  fx0sic(i,j) = (1-ca(i,j))*fxswsic -fxsensic
     1                        - fxlwsic - rho0*hls*evapsic(i,j)

                  fx0sica = ca(i,j)*fxswsic 
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

               evap(i,j) = (qsato(i,j) - tq1(2,i,j))
     1                   *rhoao*ce*usurf(i,j)

c net heat flux into atmosphere

               fx0oa = ca(i,j)*fxsw(i,j) + fxlata(i,j) + fxsen(i,j) 
     1               + fxlw(i,j) - fxplw(i,j)

c add proportions over open ocean and sea ice

               fx0a(i,j) = (1-varice1(2,i,j))*fx0oa
     1                      + varice1(2,i,j)*fx0sica

c heat flux from atmosphere into open ocean 

               fx0o(i,j) = (1-ca(i,j))*fxsw(i,j) - fxsen(i,j)
     1                   - fxlw(i,j) - rho0*hlv*evap(i,j)

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

c global heat source diagnostic

               ghs = ghs + varice1(2,i,j)*(fx0sic(i,j) + fx0sica)
     1             + (1-varice1(2,i,j))*(fx0o(i,j) + fx0oa)
     2             + rhoice*hlf*(dtha(1,i,j)
     3             + evapsic(i,j)*varice1(2,i,j)*rhooi)
            else
c-----------------------------------------------------------------------
c calculate terms over land
c-----------------------------------------------------------------------
c 'zero's' set in gseta. Evap=0 over land (no moisture or heat capacity).

               fx0a(i,j) = fxsw(i,j) + fxlata(i,j)
     1                  - fxplw(i,j)

ccc runoff scheme: in the case of land pptn .ne. zero, find nearest ocean
ccc gridbox/gridboxes and add as runoff there

               if(pptn(i,j).ne.0.0) then
                  runoff(iroff(i,j),jroff(i,j)) = 
     1               runoff(iroff(i,j),jroff(i,j)) + pptn(i,j)
               endif

               ghs = ghs + fx0a(i,j)
            endif

ccc perturb runoff by extra0 Sv 
c nre extra0 def'n altered to avoid unnecessary division

ccc ...for Stefan's intercomparison expts (NB. no compenstion elsewhere)...
c over 62 gridboxes south of convective regions (20-50N) in N.Atlantic:
c             if(j.ge.25.and.j.le.32.and.i.ge.ias(j).and.i.le.iaf(j))
c     1         runoff(i,j) = runoff(i,j) + extra0*rextra0
c or over 18 gridboxes within convective regions (50-70N) in N.Atlantic:
c            if(j.ge.33.and.j.le.35.and.i.ge.ias(j).and.i.le.iaf(j))
c     1         runoff(i,j) = runoff(i,j) + extra0*rextra0

ccc ...for GENIE expts.
c over 24 gridboxes in extratropics (~45-70N) of N.Atlantic:
            if(j.ge.32.and.j.le.35.and.i.ge.ias(j).and.i.le.iaf(j))
     1         runoff(i,j) = runoff(i,j) + extra0*rextra0n
c compensated over 94 gridboxes in tropics (0-~45N):
            if(j.ge.19.and.j.le.31.and.i.ge.ias(j).and.i.le.iaf(j))
     1         runoff(i,j) = runoff(i,j) - extra0*rextra0s

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
c for atm model and soley E_ocean for the fwflux

            pme(i,j) = pptn(i,j) - evap(i,j)*(1-varice1(2,i,j))
     1                  - evapsic(i,j)*varice1(2,i,j)

c non-dimensionalize surface fluxes for use in tstepa:

            tqa(1,i,j) = fx0a(i,j)*rfluxsca
c           tqa(2,i,j) = - pme(i,j)*rpmesca
c nre instantaneous precipitation
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
