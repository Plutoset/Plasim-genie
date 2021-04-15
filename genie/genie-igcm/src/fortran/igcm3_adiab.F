      subroutine igcm3_adiab(
     :     lowestlu_atm,lowestlv_atm,
     :     lowestlt_atm,lowestlq_atm,lowestlp_atm,
     :     lowestlh_atm,
     :     u10m_atm,v10m_atm,t2m_atm,q2m_atm,rh2m_atm,
     :     orog_atm,
     :     surfsigma,surfdsigma,psigma,iconv_ice,mass14co2,massair
     :                      )

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
      include 'gridpf.cmn'
      include 'extra.cmn'
      include 'igcm_orbit.cmn'
      include 'igcm_nlevs.cmn'
      include 'igcm_area.cmn'
#include "highres.cmn"
c     
#include "precision.inc"
c     
      real(rk_in) :: lowestlu_atm(mg,jgg)
      real(rk_in) :: lowestlv_atm(mg,jgg)
      real(rk_in) :: lowestlq_atm(mg,jgg)
      real(rk_in) :: lowestlt_atm(mg,jgg)
      real(rk_in) :: lowestlp_atm(mg,jgg)
      real(rk_in) :: lowestlh_atm(mg,jgg)
      real(rk_in) :: orog_atm(mg,jgg)
      real(rk_in) :: surfsigma,surfdsigma
      real(rk_in) :: mass14co2(mg,jgg,nl)
      real(rk_in) :: massair(mg,jgg,nl)
      integer iconv_ice


      integer ihem,iof,jj,j,kk,ih,l,ii,i,k,il,ikk,ll

      data icounter/0,0,0,0,0,0/
c     
      REAL DAG(IDDAG),DAF(IDDAF)
     *     ,ADDZ(IDDZ)
      EQUIVALENCE (DAG(1),UG(1)),(DAF(1),SPG(1))
     :     ,(ADDZ(1),AUTVDZ(1))

      real gg_orig
      real sigfac
      real xlapse
      real rxlaps
      integer lof
      real tsrf(igc)
      real pqsat
      real(rk_in) :: psigma(nl)

c     here are the boundary-layer variables:
      real u10m(igc),v10m(igc),t2m(igc),q2m(igc)
      real(rk_in) :: u10m_atm(mg,jgg)
      real(rk_in) :: v10m_atm(mg,jgg)
      real(rk_in) :: t2m_atm(mg,jgg)
      real(rk_in) :: q2m_atm(mg,jgg)
      real(rk_in) :: rh2m_atm(mg,jgg)

c     this is for lowest model level data:
      real rsigf,thnl,thbar


c     *******************************************************
c     This is an example of how to call the write_netcdf 
c     diagnostic tools:
c      call write_netcdf_igcm('/home/ggdjl/test_igcm.nc',
c     :                      tstar*ct-273.15,'temp')
c      call write_netcdf_std('/home/ggdjl/test_std.nc',
c     :                      albedo_atm,'albedo')
c     *******************************************************

c     First of all, set the orography.
c     This is potentially used in the routine inpr...
c     It is also used to then calculate the spectral coefficients...
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
            end do
         end do
      end do

      call convert_gr_sp_orog(iconv_ice)
      iconv_ice=0

C     Adiabatic part of timestep. Preset tendencies to zero.
C     
      DO I=1,IGA
         VP(I) =0.0
         SPA(I)=0.0
      END DO
      DO I=1,IGB
         ZT(I)=0.0
         DT(I)=0.0
         TT(I)=0.0
         DTE(I)=0.0
      END DO
      DO KK=1,NTRAC
         DO I=1,IGB
            TRAT(I,KK)=0.0
         END DO
      END DO
C     
      IF (KOUNT.EQ.0) THEN
C     
C     Initialise new tracer fields.
c        BEWARE HERE.
c     Now I have taken out qstar from the igcm proper, 
c       this routine may have some difficulty restarting properly, 
c       or even working properly.
c     Shouldn't be a problem as we always have LRSTRT=.true.
         IF (.NOT. LRSTRT) CALL ICTRAC
C     
C     Add white noise perturbation
C     
         IF (LNOISE.AND..NOT.LRSTRT) CALL NOISE
C     
C     Calculate restoration arrays
C     
         IF (.NOT. LRESTIJ) CALL SETRES
C     
      ENDIF
C     
      IF (JGL.EQ.1) REWIND 25
      JL=1
C     
C     set surface pressure to 976mb
C     
      IF (DAY.LT.0.1) THEN
         print *,' setting surface pressure to 976mb'
         SP(1)=CMPLX((sqr2*(0.976-1.0)),0.0)
      ENDIF
    
      if (mod(kount,itspd/4).eq.0.or.kount.lt.10) then
        print*,'In igcm3_adiab, kount,day,idoy,doy,imonth,iyear:'
        print*,kount,day,idoy,doy,imonth,iyear
      endif
C     
C     Main loop over latitudes
C     
      DO IH=1,JG
         JH=IH       

         IF(JGL.EQ.1) READ(25) ALP,DALP,RLP,RDLP
C     
C     Go from spectral space to grid point space using
C     inverse Legendre and Fourier transforms
C     

         CALL LTI
         DO I=1,NTWG
            CALL FFT991(DAG(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,1,
     +           MGPP,MG,NCRAY,1)
         END DO
         CALL FFT991(DAG(1+ NTWG*NCRAY*MGPP),WORK,TRIG,IFAX,1,
     +        MGPP,MG,NRSTWG,1)
C     
C     Calculate nonlinear terms
C     
         CALL MGRMLT
C     
c
c     This bit of code is copied from the original igcm 
c       diagnostic package, igflux1.src.
c     MSLP from subroutine SRFCR.
c     H from subroutine GHGCR


c     I understand the orography bit, but not the bit before.
c     I'm not 100% the calculation is correct.
c     It uses g(nl*nl), which is set up in inisi.f
c
C     Compute geopotential height field.  Integrate hydrostatic
C     equation and add orography if non-zero.  
c
c
      DO 10 I=1,IGD
   10 GG(I)=0.
C
C     Integrate hydrostatic equation for H (=GG).
C
      IL=0
      DO 20 L=1,NL
      DO 20 K=1,NL
      IL=IL+1
      GG_orig=G(IL)
      DO 20 IHEM=1,NHEM
      II=(L-1)*IGC+(IHEM-1)*MGPP
      IKK=(K-1)*IGC+(IHEM-1)*MGPP
      DO 20 I=1,MG
      II=II+1
      IKK=IKK+1
   20 GG(II)=GG(II)+GG_orig*(TG(IKK)+T0(K))
C
c     Add orography for H (=GG).
c
      DO 30 IHEM=1,NHEM
      IOF=(IHEM-1)*MGPP
      DO 30 L=1,NL
      K=(L-1)*IGC+IOF
      DO 30 I=1,MG
      K=K+1
   30 GG(K)=GG(K)+ga*GSG(I+IOF,JL)/cg
c
c     For mslp.
      XLAPSE=.0065*RD/GA
      SIGFAC=SIGMA(NL)**(-XLAPSE)
      LOF=NLM*IGC
      DO II=1,IGC
      TSRF(II)=(TG(LOF+II)+T0(nl))*SIGFAC
      enddo
      RXLAPS=1./XLAPSE
c
C     interpolate onto pressure levels
         call intpr
c     
C     Save grid point fields for use in NETOUT1
C     
c     spg=exp(plg)-1
         DO L=1,NL
            IOF=(L-1)*IGC
            DO I=1,IGC
               II=I+IOF
               ZG1(II,IH)=WW*ZG(II)
               DG1(II,IH)=WW*DG(II)
               UG1(II,IH)=CV*UG(II)/CS(IH)
               VG1(II,IH)=CV*VG(II)/CS(IH)
               TG1(II,IH)=CT*(TG(II)+T0(L)) - 273.16
               QG1(II,IH)=TRAG(II,1)/(SPG(I)+1)
               GG1(II,IH)=GG(II)*cg/ga
               u_pg1(ii,ih)=cv*u_pg(ii)/cs(ih)
               v_pg1(ii,ih)=cv*v_pg(ii)/cs(ih)
               t_pg1(ii,ih)=ct*(t_pg(ii)+t0(l)) - 273.16
               q_pg1(ii,ih)=q_pg(II)/(spg(i)+1)
               g_pg1(ii,ih)=g_pg(ii)*cg/ga
            END DO
         END DO
         DO I=1,IGC
            SPG1(I,IH)=P0*(SPG(I) + 1.0)
            mspg1(I,IH)=p0*exp(PLG(i))*
     +        ((1.+XLAPSE*ga*GSG(i,ih)/(TSRF(i)*cg))**RXLAPS)
         END DO
C     
C     Go from grid point space to spectral space using
C     direct Legendre and Fourier transforms
C     
         DO I=1,NTGW
            CALL FFT991(DAF(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,1,
     +           MGPP,MG,NCRAY,-1)
         END DO
         CALL FFT991(DAF(1+ NTGW*NCRAY*MGPP),WORK,TRIG,IFAX,1,
     +        MGPP,MG,NRSTGW,-1)
         CALL LTD
         JL=JL+JINC
      END DO
C     
C     First timestep - output history and diagnostics
C     

         
      IF (KOUNT.EQ.0) THEN
         CALL WRITENC_HISTS
         IF (LRESTIJ) CALL WRITENC_RESTOR
         CALL ENERGY
      ELSE IF (KOUNT.EQ.KSTART) THEN
         CALL ENERGY
      ENDIF
C     
      IF (LRESTIJ) CALL WRITENC_RESTOR

C     
C     Write a restart record
C     
      IF (KOUTR.EQ.KOUNTR) CALL WRITENC_RESTART
C     
C     Write a history record
C     
      IF (KOUTH.EQ.KOUNTH) THEN
         CALL WRITENC_HISTS
         KOUTH=0
      END IF

C     
C     Write daily data
C     
      KFLAG=0
      IF (KOUTD.EQ.KOUNTD) THEN
         if (lnetcdf.and.ldaily) CALL NETOUT1
#ifdef lgraph
         if (lgraphics) call plotfields(1)
#endif

         KOUTD=0
         KFLAG=1
      END IF
C     
C     Output diagnostics
C     
      if (lnetcdf.and.kount.gt.kstart) then
         if (lmonthly) then
            icounter(1)=icounter(1)+1
            call accum_means(tg1,tmn,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(ug1,umn,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(spg1,spmn,mg,jg,nhem,1,1.0,0.0)
            call accum_means(mspg1,mspmn,mg,jg,nhem,1,1.0,0.0)
            call accum_means(u_pg1,u_pmn,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(v_pg1,v_pmn,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(t_pg1,t_pmn,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(q_pg1,q_pmn,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(g_pg1,g_pmn,mg,jg,nhem,nl,1.0,0.0)
         end if
         if (lannual.and.lannual_restart) then
            icounter(3)=icounter(3)+1 
            call accum_means(tg1,tmna,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(ug1,umna,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(spg1,spmna,mg,jg,nhem,1,1.0,0.0)
            call accum_means(mspg1,mspmna,mg,jg,nhem,1,1.0,0.0)
            call accum_means(u_pg1,u_pmna,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(v_pg1,v_pmna,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(t_pg1,t_pmna,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(q_pg1,q_pmna,mg,jg,nhem,nl,1.0,0.0)
            call accum_means(g_pg1,g_pmna,mg,jg,nhem,nl,1.0,0.0)
         end if
         if (ldecadal.and.ldecadal_restart) then
            icounter(5)=icounter(5)+1 
            call accum_means(tg1,tmnd(1,1,1,imonth),
     :           mg,jg,nhem,nl,1.0,0.0)
            call accum_means(ug1,umnd(1,1,1,imonth),
     :           mg,jg,nhem,nl,1.0,0.0)
            call accum_means(spg1,spmnd(1,1,imonth),
     :           mg,jg,nhem,1,1.0,0.0)
            call accum_means(mspg1,mspmnd(1,1,imonth),
     :           mg,jg,nhem,1,1.0,0.0)
            call accum_means(u_pg1,u_pmnd(1,1,1,imonth),
     :           mg,jg,nhem,nl,1.0,0.0)
            call accum_means(v_pg1,v_pmnd(1,1,1,imonth),
     :           mg,jg,nhem,nl,1.0,0.0)
            call accum_means(t_pg1,t_pmnd(1,1,1,imonth),
     :           mg,jg,nhem,nl,1.0,0.0)
            call accum_means(q_pg1,q_pmnd(1,1,1,imonth),
     :           mg,jg,nhem,nl,1.0,0.0)
            call accum_means(g_pg1,g_pmnd(1,1,1,imonth),
     :           mg,jg,nhem,nl,1.0,0.0)
         end if
      end if
C     
      IF (KOUTE.EQ.KOUNTE) THEN
         CALL ENERGY
         KOUTE=0
      ENDIF
C     
      KOUTD=KOUTD+1
      KOUTE=KOUTE+1
      KOUTH=KOUTH+1
      KOUTR=KOUTR+1
      KOUNT=KOUNT+1
C     
c     IF (KOUNT.EQ.1.AND.KITS.GT.0) DAY=DAY+DELT/PI2
c     DAY=DAY+DELT/PI2
c     
c     Note that the revised calculations for day and doy
c     are required to minimise rounding errors over a
c     long integration. The formula does not work for KITS>0
c     
      DAY=(DELT/PI2)*KOUNT
      IF (.NOT. LPERPET) THEN
c     IF(KOUNT.EQ.1.AND.KITS.GT.0) DOY=DOY+DELT/PI2
c     DOY=DOY+DELT/PI2
         idoy=idoy+1
         imonth=int((idoy-1)/(itspd*30))+1
         doy=(delt/pi2)*idoy
         if (imonth.gt.12) then
            imonth=imonth-12
            iyear=iyear+1
         end if
         if (idoy.ge.360*itspd+1) then
            doy=0.0
            idoy=1
         end if
      ENDIF

c     ------------------------------
c     This is for the varying-orbit calculation
      if (vary_exact_orbit) then 
      daynum=mod(idoy/itspd,30)+
     :              ((imonth-1)*30)+(360*(iyear-2000))-iniday
c      print*,'In igcm3_adiab, daynum,iniday:',daynum,iniday
      ichange_orbit=1
      endif
c     ------------------------------
C     
C     Output diagnostics
C     

      if (imonth_ch.ne.imonth.and.imonth_ch.ne.0.and.kflag.eq.1) then
         if (ldaily) then
           call finish_nc(1)
         endif

         if (lmonthly) then
            if (icounter(1).ne.30*itspd) then
               print*,' failure a ',icounter
               stop
            endif
            icounter(1)=0
            if (icounter(2).ne.30) then
               print*,' failure b ',icounter
               stop
            endif
            icounter(2)=0
            call finish_nc(2)
         end if


         if (lannual.and.imonth_ch.eq.12) then
            if (lannual_restart) then
               if (icounter(3).ne.360*itspd) then
                  print*,' failure c ',icounter
                  stop
               end if
               icounter(3)=0
               if (icounter(4).ne.360) then
                  print*,' failure d ',icounter
                  stop
               end if
               icounter(4)=0
               call finish_nc(3) 
            else
               lannual_restart=lannual
               call ini_netcdf(imonth,iyear,sigma,alat,3)
            end if
         end if 
         if (ldecadal.and.mod(iyear,10).eq.0.and.imonth_ch.eq.12) then
            if (ldecadal_restart) then
               if (icounter(5).ne.3600*itspd) then
                  print*,' failure e ',icounter
                  stop
               end if
               icounter(5)=0
               if (icounter(6).ne.3600) then
                  print*,' failure f ',icounter
                  stop
               end if
               icounter(6)=0
               call finish_nc(4) 
            else
               ldecadal_restart=ldecadal
               call ini_netcdf(imonth,iyear,sigma,alat,4)
            end if
         end if 
      end if
      imonth_ch=imonth
C     
C     Adiabatic part of timestep
C     

      CALL TSTEP

C     
C     Mass correction.
C     
      IF (LMASCOR) THEN
         CALL MASCOR
      ELSE IF(LRESTIJ) THEN
C     Simple fix to maintain mass.
         SP(1)=CMPLX(0.,0.)
      ENDIF
C     
C     Diabatic part of timestep. Preset tendencies to zero.
C     
      DO I=1,IGB
         ZT(I)=0.0
         DT(I)=0.0
         TT(I)=0.0
      END DO
      DO KK=1,NTRAC
         DO I=1,IGB
            TRAT(I,KK)=0.0
         END DO
      END DO
C     
C     Set parameters for optional Newtonian cooling.
C     
      IF (LRESTIJ) CALL SETTEE
C     
C     Preset accumulated diagnostics.
C     
      IF (KOUTH.EQ.1) THEN
         DO I=1,IDDZ
            ADDZ(I)=0.0
         END DO
      ENDIF
C     
C     Loop over latitude for spectral transforms
C     and calculation of diabatic tendencies.
C     
      IF (JGL.EQ.1) REWIND(25)
      JL=1
      DO IH=1,JG
         JH=IH
         IF(JGL.EQ.1) READ(25) ALP,DALP,RLP,RDLP
C     
C     Go from spectral space to grid point space using
C     inverse Legendre and Fourier transforms
C     



         CALL LTI
         DO I=1,NTWG
            CALL FFT991(DAG(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,
     +           1,MGPP,MG,NCRAY,1)
         END DO
         CALL FFT991(DAG(1+ NTWG*NCRAY*MGPP),WORK,TRIG,IFAX,
     +        1,MGPP,MG,NRSTWG,1)


c        surflux and diab want the temp without the t0 normalisation... 
         DO L=1,NL
            IOF=(L-1)*IGC
            DO I=1,IGC
               II=I+IOF
               TG(II)=TG(II)+T0(L)
            END DO
         END DO

c        surflux and diab want the pressure without the log normalisation...
         DO I=1,igc
            PLG(i)=EXP(PLG(i))
         END DO        

c        This is done here so it doesn't have to be done in surflux....
         IF (LFLUX) THEN
c        Convert from volume mixing ratio to mass mixing ratio.
c        (assume tracer 1 is moisture)
C ntrac loop added by JSS so that C14 tracer can also be converted to mass mixing ratio.
         do ll=1,NTRAC
           do l=1,nl
             DO I=1,igc
               ii=(l-1)*igc+i
               trag(ii,ll)=trag(ii,ll)/PLG(i)
             enddo
           enddo
         enddo
         ENDIF


c        Could tidy this up by only writing 1 to nl-1

         DO I=1,IGD
            UGF(I,JH)=UG(I)
            VGF(I,JH)=VG(I)
            ZGF(I,JH)=ZG(I)
            DGF(I,JH)=DG(I)
            TGF(I,JH)=TG(I)
         END DO
         DO II=1,NTRAC
            DO I=1,IGD
               TRAGF(I,II,JH)=TRAG(I,II)
            END DO   
         END DO
         DO I=1,IGC
            PLGF(I,JH)=PLG(I)
         END DO   

      if (lcalc_blayer_vals) then

      call convert_free_blayer(cv*ug/cs(jh),p0*plg,
     :         u10m,10.0,sigma,tstar(:,jh))
      call convert_free_blayer(cv*vg/cs(jh),p0*plg,
     :         v10m,10.0,sigma,tstar(:,jh))
      call convert_free_blayer(ct*tg-273.16,p0*plg,
     :         t2m,2.0,sigma,tstar(:,jh))
      call convert_free_blayer(trag(:,1),p0*plg,
     :         q2m,2.0,sigma,tstar(:,jh))            

            do ihem=1,nhem
               iof=(ihem-1)*mgpp
               if (ihem.eq.1) then
                  jj=jh
               else
                  jj=jgg+1-jh
               end if
               do i=1,mg
                 ii=(nl-1)*igc+i+iof

                 u10m_atm(i,jj)=u10m(i+iof)
                 v10m_atm(i,jj)=v10m(i+iof)
                 t2m_atm(i,jj)=t2m(i+iof)
                 q2m_atm(i,jj)=q2m(i+iof)
c     This bit is copied from lscrn.f, line 41.
c       it also assumes that sigma(2m) is approx 1
                 rh2m_atm(i,jj)=q2m(i+iof)*plg(i+iof)/
     :              pqsat((t2m(i+iof)+273.16)/ct)

               enddo
            enddo
      endif


            do ihem=1,nhem
               iof=(ihem-1)*mgpp
               if (ihem.eq.1) then
                  jj=jh
               else
                  jj=jgg+1-jh
               end if
               do i=1,mg
                 ii=(nl-1)*igc+i+iof

                 lowestlu_atm(i,jj)=cv*ug(ii)/cs(jh)
                 lowestlv_atm(i,jj)=cv*vg(ii)/cs(jh)
                 lowestlq_atm(i,jj)=trag(ii,1)
                 lowestlt_atm(i,jj)=ct*tg(ii)-273.16
                 lowestlp_atm(i,jj)=p0*plg(i+iof)
c     **************************************************
c     For the lowest model-level height.....
                 rsigf=2.*(1-sigma(nl))/(1+sigma(nl))
                 thnl=(lowestlt_atm(i,jj)+273.16)/(sigma(nl)**akap)
                 thbar=0.5*(thnl+(tstar(j,jh)+273.16))
                 lowestlh_atm(i,jj)=rsigf*thbar*gascon/ga
c     **************************************************
               enddo
            enddo



         JL=JL+JINC
      END DO   

      surfsigma=sigma(nl)
      surfdsigma=dsigma(nl)

c **************************************************************
c JSS - put tracer mass mixing ratio onto genie atmos grid
      if (ntrac.ge.2) then
      do kk=2,ntrac
       do ih=1,jg
        jh=ih
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

                      mass14co2(i,jj,l)=tragf(ii,kk,jh)
                      massair(i,jj,l)=plgf(i+iof,jh)*
     :                     dsigma(l)*p0/ga*
     :                     4.0*pi*6370e3*6370e3*weight_atm(i,jj)

                   enddo
                enddo
          enddo
        enddo
       enddo
       endif

       do l=1,nl
         psigma(l)=sigma(l)
       enddo
c *******************************************************

      return
      END
C     
