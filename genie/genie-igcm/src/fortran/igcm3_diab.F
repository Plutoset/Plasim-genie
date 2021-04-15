      subroutine igcm3_diab(
     :     lowestlu_atm,lowestlv_atm,
     :     lowestlt_atm,lowestlq_atm,
     :     surfsalb_atm,surfqstar_atm,
     :     surftstar_atm,
     :     rough_atm,
     :     surflatent_atm,surfsensible_atm,
     :     surfstressx_atm,surfstressy_atm,
     :     surfnetsolar_atm,surfnetlong_atm,
     :     surfprec_atm,
     :     surfhsnow_atm,
     :     t2m_atm,
     :     land_evap_atm,
     :     co2_atm,n2o_atm,ch4_atm,
     :     iconv_che,water_flux_atmos,
     :     mass14co2,ddt14co2
     :                     )

      implicit none 

C**********************************************************************
C     IGCM3_1
C     
C     FULL PHYSICS + MIXED LAYER OCEAN
C     
C     DEPARTMENT OF METEOROLOGY      UNIVERSITY OF READING
C     
C**********************************************************************
C     
C     IGCM Intermediate Global Circulation Model
C     
C     Department of Meteorology, University of Reading, UK
C     
C     This code is maintained at Reading using the Cray NUPDATE utility,
C     which has been ported to SUN and SGI.  Each parameter statement or
C     common block is maintained in a separate COMDECK and each subroutine
C     is in a separate DECK.
C     
C     IGCM1 - first portable version derived from the Reading Simple Global
C     Circulation model.  Created in February 1997.  Tested on SUN,
C     SGI and Cray
C     
C     NUPDATE modsets included in this version
C     SC961212 - to use generic intrinsic function names
C     - to use separate work arrays for REALs and INTEGERs
C     SC970203 - to make the addition of white noise switchable
C     
C     IGCM2 - first portable version with simple parametrizations, moisture
C     and additional optional tracers.  The parametrizations closely
C     follow those in the previous Cray-specific moist model, BGCM5.
C     Moisture/tracer advection can be in advective or flux form.
C     Piers Forster, April 1997.
C     
C     New common block/decks included in this version:
C     COMGRM,GRIDPP,PHYSCA,PTENDZ,XTRAP
C     
C     New routines/decks included in this version:
C     BLAYER,VDIFF,CONVEC,DRYADJ         }    physical
C     CUDIF,CUBM,CBCON,CBADJ,LSCRN,RADN  } parametrizations
C     ICTRAC,INIPHYS       -  initialisation
C     DLSGCR,PVCR          -  initialise a PV tracer
C     DGRMLT,LTIDT,LTDDIA  -  grid-point calculations & transforms
C     
C     NUPDATE modsets included in this version:
C     TRAC - multiple tracer advection
C     PHYS - infrastructure for parametrizations
C     
C     IGCM3   IGCM2 with orography and modified physics. Knows about day of
C     Year for solar insolation, surface temperature and humidity,
C     Land/ Sea contrast in surface fields.
C     IGCM3_1   IGCM3 tidied and with bug fixes. New long wave radiation
C     code with more gasses.
C     SH 2000
C     COMDECK   COMMON BLOCK   DESCRIPTION
C     ======================================================================
C     PARAM1   (PARAMETERs)   Model resolution parameters
C     PARAM2   (PARAMETERs)   Basic constants derived from PARAM1,
C     mainly for array dimensions
C     BLANK                   Basic planetary constants plus information
C     on the vertical grid and spectral domain
C     BATS     BATS           Constant arrays and variables associated
C     mainly with time and vertical differencing
C     LEGAU    LEGAU          Legendre polynomials and information about
C     gaussian latitudes
C     OUTCON   OUTCON         Switches, counters and constants controlling
C     the type and frequency of model output
C     COMFFT   COMFFT         Arrays and constants for the FFT
C     POLYNO   POLYNO         Polynomial to aid vectorisation of the
C     Legendre transforms
C     RESTIJ   RESTIJ         Restoration temperature field and constants
C     which determine it, plus timescales
C     RESTOR   RESTOR         Restoration fields and timescale
C     BALAN    BALAN          Constants and arrays for balancing
C     GRIDP    GRIDP          Grid point arrays, defined as REAL,
C     with multilevel arrays 1-dimensional
C     GRIDP2   GRIDP2         Grid point arrays, defined as REAL,
C     with multilevel arrays 2-dimensional
C     GRIDP3   GRIDP3         Grid point arrays, defined as COMPLEX,
C     with multilevel arrays 2-dimensional
C     SPECTR   SPECTR         Spectral arrays
C     COMGRM   COMGRM         Vertical derivative arrays for interpolation
C     PHYSCA   PHYS           Variables/arrays for the parametrizations
C     PTENDZ   PTENDZ         Zonally averaged parametrization diagnostics
C     GRIDPP   GRIDP          Grid point arrays, defined as REAL, for the
C     diabatic timestep.  This equivalences the
C     parametrization forcings to fields in GRIDP
C     XTRAP    XTRAP          Equivalence moisture to the first tracer
C     CPIERS   CPIERS
C     OROG_G   GSG
C     QSTABS   QSTABS
C     RADHT    RADHT
C     PARRAY
C     RADLW    RADLW          Switches and arrays for LW radiation scheme.
C     
C     DECK      SUBROUTINE     DESCRIPTION
C     ======================================================================
C     MLTRI    MLTRI          Main program.  Calls subroutines to perform
C     the spectral transforms, balancing and time-
C     stepping.  Organises the counters and output.
C     INITAL   INITAL         Calls the initialisation subroutines.
C     INISET   INISET         Sets the default namelist values, reads the
C     namelists INPPL, INPRN, INPOP, then checks
C     and writes the namelist values.  Sets certain
C     run-specific common-block values.
C     INIGAU   INIGAU         Initialises the gaussian latitudes & weights.
C     INISI    INISI          Initialises arrays and variables for the
C     vertical structure and semi-implicit scheme.
C     INIRESIJ INIRESIJ       Sets up restoration variables and arrays, by
C     setting the default values for namelist
C     INPRSIJ and then reading the namelist.
C     INIRES   INIRES         Sets up restoration variables and arrays by
C     setting the default values for namelist
C     INPRS and then reading the namelist.
C     INISTR   INISTR         Reads spectral data for a start/restart run.
C     INIBAL   INIBAL         Reads the namelist INPBL and computes arrays
C     needed for balancing.
C     INISP    INISP          Initialises the spectral arrays.
C     ICTRAC   ICTRAC         Initialises moisture, optional tracers
C     and a zonally symmetric surface state.
C     INIPHYS  INIPHYS        Initialises parametrizations.
C     INIQS    INIQS          Calculates saturation specific humidity.
C     INISURF  INISURF        Initialises surface model.
C     BALANC   BALANC         Performs balancing from wind (vorticity)
C     to mass (temperature, surface pressure).
C     BLAYER   BLAYER         Surface turbulent fluxes of momentum, heat
C     and moisture, affecting the lowest level only.
C     BLSURF   BLSURF         Time-splitting integration of surface and
C     boundary layer schemes.
C     CALNDR_igcm   CALNDR_igcm         Calculated month and day from Julian day.
C     CBADJ    CBADJ          Deep convective adjustment scheme,
C     based on the Betts-Miller scheme.
C     CBCON    CBCON          Deep convection. Old routine using a crude
C     non-entraining mass-flux scheme.
C     CLDTRN   CLDTRN         Cloud parameterisation for radiation scheme.
C     COLAMT   COLAMT         Calculates column amount.
C     CONVEC   CONVEC         Main convection routine. Finds unstable layers
C     based on parcel instability and
C     calls relevant routines.
C     CUBM     CUBM           Shallow convective adjustment scheme,
C     based on the Betts-Miller scheme.
C     CUDIF    CUDIF          Shallow convection. Old routine based on
C     enhanced vertical diffusion. Default-not used.
C     DANALV   DANALV         Direct transforms for the diabatic
C     timestep.  Optimised for vector machines.
C     DEDD     DEDD           Computes cloud reflectivity for SW.
C     DIFUSE   DIFUSE         Calculates spectral tendencies from restor-
C     ation (if included) and hyper-diffusion.
C     DGRMLT   DGMRLT         Grid-point calculations in diabatic timestep
C     and calls to the parametrization routines.
C     DLSGCR   DLSGCR         Calculates vertical derivative.
C     DRYADJ   DRYADJ         Dry convective adjustment (heat and moisture).
C     DSTEP    DSTEP          Performs the diabatic part of the timestep,
C     including completion of the time-filter.
C     ENERGY   ENERGY         Calculates & writes various global diagnostic
C     quantities.
C     HANAL    HANAL          Direct Legendre transform from Fourier to
C     spectral space.  Black-box routine for one
C     transform-type for a multi-level field.
C     HANAL1   HANAL1         Ditto, for a single-level field.
C     HANALV   HANALV         Ditto, but fast vectorising version for all
C     fields/transform-types in the adiabatic step.
C     HEXP     HEXP           Inverse Legendre transform from spectral to
C     Fourier space.  Black-box routine for one
C     transform-type for a multi-level field.
C     HEXP1    HEXP1          Ditto, for a single-level field.
C     HEXPV    HEXPV          Ditto, but fast vectorising version for all
C     fields/transform-types in the adiabatic step.
C     INTERP   INTERPF        Interpolates from climatology to model vertical
C     grid. Not used for O3.
C     IRRAD    IRRAD          LW radiation scheme.
C     LGNDRE   LGNDRE         Calculates the Legendre polynomials and their
C     derivatives at a single latitude.
C     LSCRN    LSCRN          Stratiform precipitation, removing super-
C     saturation without re-evaporation.
C     LTD      LTD            Direct Legendre transform for the adiabatic
C     part of the timestep.  Control routine.
C     LTDDIA   LTDDIA         Inverse Legendre transforms for the diabatic
C     timestep.
C     LTI      LTI            Inverse Legendre transform for the adiabatic
C     part of the timestep.  Control routine.
C     LTIDT    LTIDT          Inverse Legendre transform required to
C     initialise moisture and tracers.
C     MASCOR   MASCOR         Global mass correction.
C     MATINV   MATINV         Inverts a matrix.
C     MGRMLT   MGRMLT         Computes grid point values of the non-linear
C     contributions to the tendencies.
C     MORC     RADN           Initialises radiation scheme.
C     NIKOS    NIKOSRAD       Calls LW and SW radiation schemes.
C     NOISE    NOISE          Uses a random number generator to add a white
C     noise perturbation to (ln) surface pressure.
C     O3INTERP O3INTERP       INTERPF for O3.
C     PVCR     PVCR           Calculates PV to initialise a tracer.
C     RADSW    RADSW          Initialises SW radiation scheme.
C     SETRES   SETRES         Sets up and saves a restoration state from
C     the initial zonally averaged state.
C     SETTEE   SETTEE         Modifies the restoration temperature field
C     to include a seasonal cycle.
C     SETZT    SETZT          Sets up a restoration temperature field,
C     using lapse-rate and tropopause parameters.
C     SFCT     SFCT           Finds T and Q from DOY and climatology.
C     SPDEL2   SPDEL2         Compute del-squared or its inverse for a
C     spectral field.
C     SURFM    SURFM          Performs soil moisture/ snow depth update.
C     SW       SW             Computes short wave radiation fluxes.
C     SWTT     SWTT           Computes transmission functions for SW.
C     TBAL     TBAL           Performs balancing from mass (temperature,
C     surface pressure) to wind (vorticity).
C     TSTEP    TSTEP          Performs an adiabatic timestep in spectral
C     space, including the semi-implicit treatment
C     of gravity wave propagation.  Performs the
C     explicit part of the time-filter.
C     VDIFF    VDIFF          Vertical diffusion of momentum, heat
C     and moisture throughout the atmospheric depth,
C     with constant exchange coefficients.
C     
C     =====================================================================
C     
#include "param1.cmn"
      include 'param2.cmn'
      integer iddaf,iddag,iddad,iddz
      PARAMETER(IDDAF=NFTGW*IGC,IDDAG=NFTWG*IGC,IDDAD=NFTGD*IGC
     *     ,IDDZ=(11*NL+4)*JGG)
      include 'balan.cmn'
      include 'bats.cmn'
      include 'blank.cmn'
      include 'comfft.cmn'
      include 'gridp.cmn'
      include 'legau.cmn'
      include 'outcon.cmn'
      include 'physca.cmn'
      include 'polyno.cmn'
      include 'ptendz.cmn'
      include 'restij.cmn'
      include 'restor.cmn'
      include 'spectr.cmn'
      include 'gridss.cmn'
      include 'cpiers.cmn'
      include 'stats.cmn'
      include 'means.cmn'
      include 'orog_g.cmn'
      include 'radht.cmn'
      include 'fluxes.cmn'
      include 'igcm_prec.cmn'
      include 'gridpf.cmn'
      include 'extra.cmn'
      include 'tendencies.cmn'
      include 'rough.cmn'
#include "parray.cmn"
      include 'radlw.cmn'
      include 'butterfly.cmn'
c     
      include 'igcm_area.cmn'
      include 'flux_adjust.cmn'

#include "precision.inc"
c
      real(rk_in) :: lowestlu_atm(mg,jgg)
      real(rk_in) :: lowestlv_atm(mg,jgg)
      real(rk_in) :: lowestlq_atm(mg,jgg)
      real(rk_in) :: lowestlt_atm(mg,jgg)
      real(rk_in) :: surftstar_atm(mg,jgg)
      real(rk_in) :: surflatent_atm(mg,jgg)
      real(rk_in) :: surfsensible_atm(mg,jgg)
      real(rk_in) :: surfstressx_atm(mg,jgg)
      real(rk_in) :: surfstressy_atm(mg,jgg)
      real(rk_in) :: surfsalb_atm(mg,jgg)
      real(rk_in) :: surfqstar_atm(mg,jgg)
      real(rk_in) :: surfnetsolar_atm(mg,jgg)
      real(rk_in) :: surfnetlong_atm(mg,jgg)
      real(rk_in) :: surfprec_atm(mg,jgg)
      real(rk_in) :: surfhsnow_atm(mg,jgg)
      real(rk_in) :: t2m_atm(mg,jgg)
      real(rk_in) :: land_evap_atm(mg,jgg)
      real(rk_in) :: rough_atm(mg,jgg)
      real(rk_in) :: co2_atm(mg,jgg)
      real(rk_in) :: n2o_atm(mg,jgg)
      real(rk_in) :: ch4_atm(mg,jgg)
      real(rk_in) :: mass14co2(mg,jgg,nl)
      real(rk_in) :: ddt14co2(mg,jgg,nl)
      integer :: iconv_che
      real :: water_flux_atmos

      integer ihem,iof,jj,j,ih,l,ii,i,iii,kk  
c     
c     
      REAL DAD(IDDAD)
     *     ,DDZ(IDDZ),ADDZ(IDDZ)
      EQUIVALENCE (DAD(1),TNLG(1))
     :     ,(DDZ(1),UTVDZ(1)),(ADDZ(1),AUTVDZ(1))


c     **********************************************
c     FOR WATER CONSERVATION CHECK......

      real qg_copy(igd,jg)

      real tot_precip
      real tot_precip_ocean
      real tot_precip_land

      real radearth
      parameter(radearth=6370e3)

      real qg_ea_old1
      real qg_ea_old2
      real qg_bd_old1
      real qg_bd_old2
      real prec_old1
      real prec_old2
      real evap_old1
      real evap_old2
      save qg_ea_old1,qg_ea_old2
      save qg_bd_old1,qg_bd_old2
      save prec_old1,prec_old2,evap_old1,evap_old2

      integer fcount
      integer ncount
      parameter(ncount=1000)

      real dan_f(ncount)
      real dan_q(ncount)

      real dan_alpha
      parameter(dan_alpha=0.04)

      integer ifirst
      data ifirst/1/
      save ifirst

      real firstevapall
      real firstprecocean
      save firstevapall,firstprecocean

      integer water_o


c     **********************************************
c       call blsurf
c     ***************************************************

c     ***************************************************
c      First of all, if it's the first time, and butterfly is on, 
c        then apply the pertubation
       if (lbutterfly) then
       if (ifirst.eq.1) then
         lowestlu_atm(1,1)=lowestlu_atm(1,1)+butterfly_u
       endif
       endif
c     ***************************************************

c     ***************************************************
       if (lcheckwater) then

       qg_ea_old2=qg_ea_old1
       qg_ea_old1=0.0
       qg_bd_old2=qg_bd_old1
       qg_bd_old1=0.0

       DO IH=1,JG
         JH=IH
            do ihem=1,nhem
               iof=(ihem-1)*mgpp
               if (ihem.eq.1) then
                  jj=jh
               else
                  jj=jgg+1-jh
               end if
               do i=1,mg
                 ii=(nl-1)*igc+i+iof
                 qg_copy(ii,jh)=lowestlq_atm(i,jj)
                 do l=1,nl-1
                   ii=(l-1)*igc+i+iof
                   qg_copy(ii,jh)=tragf(ii,1,jh)
                 enddo

                 do l=1,nl
                 ii=(l-1)*igc+i+iof

                 qg_ea_old1=qg_ea_old1+tragf(ii,1,jh)*plgf(i+iof,jh)*
     :      dsigma(l)*p0/ga*
     :      4.0*pi*6370e3*6370e3*weight_atm(i,jj)

                 qg_bd_old1=qg_bd_old1+qg_copy(ii,jh)*plgf(i+iof,jh)*
     :      dsigma(l)*p0/ga*
     :      4.0*pi*6370e3*6370e3*weight_atm(i,jj)

                 enddo
               enddo
            enddo
      enddo

      if (ifirst.eq.1) then
        firstevapall=(qg_bd_old1-qg_ea_old1)/2.0
      endif

      endif
c     ***************************************************


c     ***************************************************
c     first of all, calculate the gases if required
      if (iconv_che.eq.1) then
      print*,'IGCM is updating gases'
c     OK, let's cheat and not do a weighted average,
c       seen as gases are well-mixed anyway.
      vmrco2=0.0
      vmrn2o=0.0
      vmrch4=0.0
      do j=1,jgg
        do i=1,mg
          vmrco2=vmrco2+co2_atm(i,j)
          vmrn2o=vmrn2o+n2o_atm(i,j)
          vmrch4=vmrch4+ch4_atm(i,j)
        enddo
      enddo
      vmrco2=vmrco2/(mg*jgg)
      vmrn2o=vmrn2o/(mg*jgg)
      vmrch4=vmrch4/(mg*jgg)
      iconv_che=0
      endif
c     ***************************************************


       JL=1     
       DO IH=1,JG
         JH=IH

c        These only need go from levels 1 to nl-1.  I should fix this.

         DO I=1,IGD
            UG(I)=UGF(I,JH)
            VG(I)=VGF(I,JH)
            ZG(I)=ZGF(I,JH)
            DG(I)=DGF(I,JH)
            TG(I)=TGF(I,JH)
         enddo

         DO II=1,NTRAC
            DO I=1,IGD
               TRAG(I,II)=TRAGF(I,II,JH)
            END DO   
         END DO


c        These are just the lowest model level tendencies....
            do ihem=1,nhem
               iof=(ihem-1)*mgpp
               if (ihem.eq.1) then
                  jj=jh
               else
                  jj=jgg+1-jh
               end if
               do i=1,mg
                 ii=(nl-1)*igc+i+iof

                 ug(ii)=lowestlu_atm(i,jj)*cs(jh)/cv
                 vg(ii)=lowestlv_atm(i,jj)*cs(jh)/cv
                 tg(ii)=(lowestlt_atm(i,jj)+273.16)/ct
c          Tracer 1 hard-wired to water here...
                 trag(ii,1)=lowestlq_atm(i,jj)

            TNLG(Ii)=iland(i+iof,jh)*land_tendlowestlt_atm(i,jj)+
     :    (1-iland(i+iof,jh))*ocean_tendlowestlt_atm(i,jj)
            fug(Ii)=iland(i+iof,jh)*land_tendlowestlu_atm(i,jj)+
     :    (1-iland(i+iof,jh))*ocean_tendlowestlu_atm(i,jj)
            fvg(Ii)=iland(i+iof,jh)*land_tendlowestlv_atm(i,jj)+
     :    (1-iland(i+iof,jh))*ocean_tendlowestlv_atm(i,jj)

c
         DO IIi=1,1
            tranlg(ii,IIi)=iland(i+iof,jh)*land_tendlowestlq_atm(i,jj)+
     :    (1-iland(i+iof,jh))*ocean_tendlowestlq_atm(i,jj)
         enddo

C     ************************************************************************
c     OK, WITHOUT THIS IF-LOOP (which is never true!), THE MODEL DOES
c       NOT DO PERFECT RESTARTS!!!!!  THIS IS VERY WORRYING!!
C       I THINK IT IS SOMETHING TO DO WITH THE INTEL OPTIMISER, BECAUSE
C       THE BUG DOES NOT OCCUR WHEN OPTIMISATION IS OFF.  I'M NOT SURE
C       EXACTLY WHAT THE BUG IS, BUT THE RESULT IS THAT ALTHOUGH THE MODEL RUNS
C       AS NORMAL, THE RESTART FILE IT WRITES IS NOT CORRECT FOR NETSOLAR, NETLONG,
C       PREC_LARG AND PREC_CONV. DJL 30/6/2004......
C       I THINK IT MAY BE RELATED TO A SIMIALR PROBLEM I HAD WITH THE LAND-SURFACE
C       RESTARTS PREVIOUSLY.  THIS NEEDS LOOKING AT SOME TIME......30/6/2004......
             if (iland(i+iof,jh).gt.2) then
                print*,'problem!!!!!'
             endif
C     ************************************************************************

               enddo
            enddo


            do ihem=1,nhem
               iof=(ihem-1)*mgpp
               if (ihem.eq.1) then
                  jj=jh
               else
                  jj=jgg+1-jh
               end if
               do i=1,mg
                 ii=igc+i+iof

            SLBL(I+iof)=-surflatent_atm(i,jj)/(cv*p0)
            PJG(I+iof)=surfstressy_atm(i,jj)/p0
            PMG(I+iof)=surfstressx_atm(i,jj)/p0
            SHBL(i+iof)=-surfsensible_atm(i,jj)/(cv*p0)
            SNOWD(i+iof)=surfhsnow_atm(i,jj)
            TEMP2M(i+iof)=t2m_atm(i,jj)
            LANDEVAP(i+iof)=land_evap_atm(i,jj)

            tstar(i+iof,jh)=(surftstar_atm(i,jj)+273.16)/ct
            salb(i+iof,jh)=surfsalb_atm(i,jj)
            qstar(i+iof,jh)=surfqstar_atm(i,jj)

            rough(i+iof,jh)=ga/(radea*ww*ww)*rough_atm(i,jj)/radea

               enddo
            enddo



         DO I=1,IGC
            PLG(I)=PLGF(I,JH)
         END DO  

      DO L=1,NL-1
         DO J=1,IGC
            ii=(l-1)*igc+j
            fUG(ii)=0.0
            fvg(ii)=0.0
            TNLG(ii)=0.0
            do iii=1,ntrac
            tranlg(ii,iii)=0.0
            enddo
         END DO
      END DO


       CALL DGRMLT

c **********************************************************
c JSS - add tracer tendencies from 14Catm module
c      do L=1,1
c        do j=1,IGC
c         ii=(l-1)*igc+j
c          do iii=2,ntrac
cc           tranlg(ii,iii)=1.0
c          enddo
c        enddo
c       enddo

        if (ntrac.ge.2) then
         do kk=2,ntrac
         do ihem=1,nhem
           iof=(ihem-1)*mgpp
           if (ihem.eq.1) then
             jj=jh
           else
             jj=jgg+1-jh
           end if
           do i=1,mg
              do l=1,nl
                ii=(l-1)*igc+i+iof
                if (ifirst.eq.1) ddt14co2(i,j,l)=0.0
                mass14co2(i,jj,l)=0.0
                tranlg(ii,kk)=tranlg(ii,kk) + ddt14co2(i,jj,l)
              enddo
           enddo
         enddo
        enddo
        endif

c **********************************************************      

       
C     Go from grid point space to spectral space using
C     direct Legendre and Fourier transforms
C     
         DO I=1,NTGD
            CALL FFT991(DAD(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,
     +           1,MGPP,MG,NCRAY,-1)
         END DO
         CALL FFT991(DAD(1+ NTGD*NCRAY*MGPP),WORK,TRIG,IFAX,
     +        1,MGPP,MG,NRSTGD,-1)
C     
         CALL LTDDIA
         JL=JL+JINC
      END DO
C     
C     Write accumulated diagnostics to history file.
C     and daily data to netcdf  
C     
      IF (KOUTH.EQ.KOUNTH) CALL WRITENC_HISTG1(0)
C     
      IF (KOUTD.EQ.KOUNTD) THEN
         if (lnetcdf) then
            inetcount=inetcount+1
            call netout2
            if (lmonthly) then
               icounter(2)=icounter(2)+1
               call accum_means_g(2)
            end if
            if (lannual.and.lannual_restart) then 
               icounter(4)=icounter(4)+1
               call accum_means_g(3)
            end if
            if (ldecadal.and.ldecadal_restart) then 
               icounter(6)=icounter(6)+1
               call accum_means_g(4)
            end if
         end if
#ifdef lgraph
         if (lgraphics) call plotfields2(0)
#endif
      ENDIF
C     
      IF(KOUTH.GE.1.AND.KOUTH.LE.KOUNTH) THEN
         DO I=1,IDDZ
            ADDZ(I)=ADDZ(I)+DDZ(I)
         END DO
      ENDIF
      IF (KOUTH.EQ.KOUNTH) CALL WRITENC_HISTG1(1)      
C     
C     Apply dissipation and optional linear restoration.
C     
      CALL DIFUSE
C     
C     Update spectral fields in the diabatic timestep.
C     
      CALL DSTEP
C

C     End of timestep
c     This variable, TAVE is never used, so just get rid of it....
c      DO J=1,IGB
c         TAVE(j*2-1)=TAVE(J*2-1)+REAL(T(J))
c         TAVE(j*2)=TAVE(J*2)+AIMAG(T(J))
c      ENDDO
C     

         do jh=1,jg
            do ihem=1,nhem
               iof=(ihem-1)*mgpp
               if (ihem.eq.1) then
                  jj=jh
               else
                  jj=jgg+1-jh
               end if
               do i=1,mg

c     DJL
c     OUTPUT FLUXES AT ALL GRIDPOINTS..............

               surfprec_atm(i,jj)=(prec_larg(i+iof,jh)+
     &              prec_conv(i+iof,jh))*(itspd/24.0)/3600.0

               surfnetsolar_atm(i,jj)=rrflux(i+iof,jh,1)-
     &                               rrflux(i+iof,jh,2)  
               surfnetlong_atm(i,jj)=rrflux(i+iof,jh,3)-
     &                               rrflux(i+iof,jh,4)

               end do
            end do
         end do
c
c     ****************************************************************
      if (lfluxadjust_water) then

         if (ifirst.eq.1) then
         water_totarea(:)=0.0
            do j=1,jgg
               do i=1,mg
                  if (water_mask(i,j).ne.0) then
                     if (water_mask(i,j).lt.0) then
                        water_o=4
                     else
                        water_o=3
                     endif
         water_totarea(nint(water_mask(i,j))+water_o)=
     :       water_totarea(nint(water_mask(i,j))+water_o)+
     :       weight_atm(i,j)*
     :       4.0*pi*6370e3*6370e3
                  endif
               enddo
            enddo
         endif

         do j=1,jgg
            do i=1,mg
               if (water_mask(i,j).ne.0) then
                  if (water_mask(i,j).lt.0) then
                     water_o=4
                  else
                     water_o=3
                  endif
                  surfprec_atm(i,j)=surfprec_atm(i,j)+
     :                 1e9*
     :                 water_transport(abs(nint(water_mask(i,j))))*
     :                 sign(1.0,water_mask(i,j))/
     :                 water_totarea(nint(water_mask(i,j))+water_o)
               endif
            enddo
         enddo
         
      endif


c     ****************************************************************



              

c     ****************************************************************

      if (lcheckwater) then

      prec_old2=prec_old1

      tot_precip_ocean=0.0
      tot_precip_land=0.0
      tot_precip=0.0
         do jh=1,jg
            do ihem=1,nhem
               iof=(ihem-1)*mgpp
               if (ihem.eq.1) then
                  jj=jh
               else
                  jj=jgg+1-jh
               end if
               do i=1,mg
        tot_precip=tot_precip+surfprec_atm(i,jj)*
     :      weight_atm(i,jj)*4.0*pi*radearth*radearth*
     :      3600.0*24.0/real(itspd)

        tot_precip_ocean=tot_precip_ocean+surfprec_atm(i,jj)*
     :      weight_atm(i,jj)*4.0*pi*radearth*radearth*
     :      3600.0*(1-iland(i+iof,jh))*24.0/real(itspd)

        tot_precip_land=tot_precip_land+surfprec_atm(i,jj)*
     :      weight_atm(i,jj)*4.0*pi*radearth*radearth*
     :      3600.0*iland(i+iof,jh)*24.0/real(itspd)


               enddo
            enddo
         enddo

      if (ifirst.eq.1) then
        firstprecocean=tot_precip_ocean
      endif

      prec_old1=tot_precip*2.0            

      evap_old1=qg_bd_old1-qg_ea_old1
      evap_old2=qg_bd_old2-qg_ea_old2

      dan_q(1)=qg_ea_old2
      dan_q(2)=qg_ea_old1

c     NOTE MINUS SIGN HERE!!!!
      dan_f(1)=evap_old2-prec_old2
      dan_f(2)=evap_old1-prec_old1

      do fcount=3,ncount
        dan_q(fcount)=dan_alpha/2*dan_f(fcount-1) +
     :              (1-dan_alpha)*dan_f(fcount-2) +
     :              dan_alpha*dan_q(fcount-1) +
     :              (1-dan_alpha)*dan_q(fcount-2)
      enddo


c     New term firstprecocean is because with the new initialisation,
c       (as of 15th Oct 2004), the initial precip is no longer zero 
c       as before.  We only correct over ocean becuase the land precip
c       is used in the genie.F budget....
      print*,'*************************************************'
      print*,kind(water_flux_atmos)
      water_flux_atmos = ((dan_q(ncount)+dan_q(ncount-1))/2.0)+
     :         tot_precip_land-firstevapall+firstprecocean     
      print*,'FINAL QG IN ATMOSPHERE IS: ', water_flux_atmos
      print*,'**************************************************'

      endif

      if (ifirst.eq.1) then
        ifirst=0
      endif

c     ****************************************************************

      return
      END
