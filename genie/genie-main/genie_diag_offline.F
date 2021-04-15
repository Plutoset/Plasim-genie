c
c Offline computation of diagnostics for GENIE using routines avaialable
c on a per-module basis from the modules themselves via calls to
c functionality which is included in their respective libraries. The
c initialisation code of the included modules is used here to ensure the
c configuration of the modules to be identical as in the corresponding
c model executable.
c
c Currently only diagnostics from annually-averaged output of the EMBM,
c GOLDSTEIN, and GOLDSTEIN-sea ice modules are taken into account. The
c framework is designed to be extensible. This code is based on
c 'genie_costfn_offline.F' (which reuses code fragments from
c 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted form).
c

      program genie_diag_offline

      use genie_control
      use genie_global
      use genie_util, ONLY:message,die

      use genie_ini_wrappers

      implicit none

      include 'netcdf.inc'

c Configuration of the error function
      character*256 configfile
      logical exists
      character*13 yearstr

c Temperature diagnostics
      real mean_T,mean_T_vol,var_T,var_T_vol,mean_Tobs,mean_Tobs_vol
     $     ,var_Tobs,var_Tobs_vol,rmsnorm_T,rmsnorm_T_vol
      integer n_T
c Surface temperature diagnostics
      real mean_Tsurf,mean_Tsurf_vol,var_Tsurf,var_Tsurf_vol
     $     ,mean_Tsurfobs,mean_Tsurfobs_vol,var_Tsurfobs
     $     ,var_Tsurfobs_vol,rmsnorm_Tsurf,rmsnorm_Tsurf_vol
      integer n_Tsurf
c Salinity diagnostics
      real mean_S,mean_S_vol,var_S,var_S_vol,mean_Sobs,mean_Sobs_vol
     $     ,var_Sobs,var_Sobs_vol,rmsnorm_S,rmsnorm_S_vol
      integer n_S
c Surface salinity diagnostics
      real mean_Ssurf,mean_Ssurf_vol,var_Ssurf,var_Ssurf_vol
     $     ,mean_Ssurfobs,mean_Ssurfobs_vol,var_Ssurfobs
     $     ,var_Ssurfobs_vol,rmsnorm_Ssurf,rmsnorm_Ssurf_vol
      integer n_Ssurf
c Circulation diagnostics
      real opsia_max,opsia_max_lat,opsia_max_depth,opsia_min
     $     ,opsia_eq_max,opsia_eq_min,opsia_eq_zero_depth,opsip_max
     $     ,opsip_min,opsi_depth_min,tf_drake
c Surface air temperature diagnostics
      real mean_Tatm,mean_Tatm_area,var_Tatm,var_Tatm_area ,mean_Tatmobs
     $     ,mean_Tatmobs_area,var_Tatmobs ,var_Tatmobs_area,rmsnorm_Tatm
     $     ,rmsnorm_Tatm_area,mean_Tatm_SH_area,mean_Tatm_NH_area
      integer n_Tatm
c Surface air specific humidity diagnostics
      real mean_q,mean_q_area,var_q,var_q_area
     $     ,mean_qobs,mean_qobs_area,var_qobs
     $     ,var_qobs_area,rmsnorm_q,rmsnorm_q_area
      integer n_q
c Surface air relative humidity diagnostics
      real mean_r,mean_r_area,var_r,var_r_area
     $     ,mean_robs,mean_robs_area,var_robs
     $     ,var_robs_area,rmsnorm_r,rmsnorm_r_area
      integer n_r
c Sea-ice diagnostics
      real ice_vol,ice_area,ice_vol_SH,ice_area_SH,ice_vol_NH
     $     ,ice_area_NH

c ======================================================================
c     Initialise global GENIE parameters
      call initialise_genie

c ======================================================================
c     Configure evaulation of cost function
      configfile='diag.config'
      inquire(file=trim(configfile), exist=exists)
      if (.not. exists) then
         call die('Could not find the external configuration file'//
     $        ' for the cost function evaluation',1)
      end if
      open(unit=431, file=configfile)
      read(431,*) yearstr
      close(431)
      print *,'Processing av_',yearstr

c ======================================================================
c     Check available configuraitons
      if (.not.((flag_goldsteinocean)
     &     .or.(flag_ebatmos)
     &     .or.(flag_goldsteinseaice))) then
         call die('Only available if either of the EMBM, GOLDSTEIN, or'
     &        //' GOLDSTEIN-sea ice modules is active')
      endif

c ======================================================================
c     Initialise GOLDSTEIN ocean model
      if (flag_goldsteinocean) then
         call initialise_goldocean_wrapper(reinit=.true.)
      endif
c ======================================================================
c     Initialise EMBM atmosphere model
      if (flag_ebatmos) then
         call initialise_embm_wrapper(reinit=.true.)
      endif
c ======================================================================
c     Initialise GOLDSTEIN-sea ice sea-ice model
      if (flag_goldsteinseaice) then
         call ini_goldsteinseaice_wrapper(reinit=.true.)
      endif

      call message("",1)
      call message
     $     ('*******************************************************',1)
      call message(' Initialisation complete, offline',1)
      call message(' computation of diagnostics starting...',1)
      call message
     $     ('******************************************************',1)
      call message("",1)

c======================================================================
c Get diagnostics for individual modules
c======================================================================
      open(unit=433, file='diag.out')

      if (flag_goldsteinocean) then
         call diag_goldstein_T_annual_av(yearstr,mean_T,mean_T_vol
     $        ,var_T,var_T_vol,mean_Tobs,mean_Tobs_vol,var_Tobs
     $        ,var_Tobs_vol,rmsnorm_T,rmsnorm_T_vol,n_T)
         call diag_goldstein_Tsurf_annual_av(yearstr,mean_Tsurf
     $        ,mean_Tsurf_vol,var_Tsurf,var_Tsurf_vol,mean_Tsurfobs
     $        ,mean_Tsurfobs_vol,var_Tsurfobs,var_Tsurfobs_vol
     $        ,rmsnorm_Tsurf,rmsnorm_Tsurf_vol,n_Tsurf)
         call diag_goldstein_S_annual_av(yearstr,mean_S,mean_S_vol
     $        ,var_S,var_S_vol,mean_Sobs,mean_Sobs_vol ,var_Sobs
     $        ,var_Sobs_vol ,rmsnorm_S,rmsnorm_S_vol,n_S)
         call diag_goldstein_Ssurf_annual_av(yearstr,mean_Ssurf
     $        ,mean_Ssurf_vol,var_Ssurf,var_Ssurf_vol,mean_Ssurfobs
     $        ,mean_Ssurfobs_vol,var_Ssurfobs,var_Ssurfobs_vol
     $        ,rmsnorm_Ssurf,rmsnorm_Ssurf_vol,n_Ssurf)
         call diag_goldstein_circulation_annual_av(yearstr,opsia_max
     $        ,opsia_max_lat,opsia_max_depth,opsia_min,opsia_eq_max
     $        ,opsia_eq_min,opsia_eq_zero_depth,opsip_max,opsip_min
     $        ,opsi_depth_min,tf_drake)
      write(433,*) '# *** 3-dimensional temperature field ***'
      write(433,*) '# volume-weighted statistics'
      write(433,*) '# ======================================'
      write(433,*) '# Mean of T field (deg C):'
      write(433,*) mean_T_vol
      write(433,*) '# Variance of T field (deg C^2):'
      write(433,*) var_T_vol
      write(433,*) '# RMS model-data difference (deg C):'
      write(433,*) rmsnorm_T_vol
      write(433,*) '# Mean of Tobs field (deg C):'
      write(433,*) mean_Tobs_vol
      write(433,*) '# Variance of Tobs field (deg C^2):'
      write(433,*) var_Tobs_vol
      write(433,*) '# *** 3-dimensional temperature field ***'
      write(433,*) '# statistics of individual, unweighted'
      write(433,*) '# values'
      write(433,*) '# ======================================'
      write(433,*) '# Number of individual points of the temp. field:'
      write(433,*) n_T
      write(433,*) '# Mean of T field (deg C):'
      write(433,*) mean_T
      write(433,*) '# Variance of T field (deg C^2)'
      write(433,*) var_T
      write(433,*) '# Mean of Tobs field (deg C)'
      write(433,*) mean_Tobs
      write(433,*) '# Variance of Tobs field (deg C^2)'
      write(433,*) var_Tobs
      write(433,*) '# RMS model-data difference (deg C)'
      write(433,*) rmsnorm_T
      write(433,*) '# *** 2-dimensional sea-surface temp. field ***'
      write(433,*) '# area-weighted statistics'
      write(433,*) '==============================================='
      write(433,*) 'Mean of T field (deg C)'
      write(433,*) mean_Tsurf_vol
      write(433,*) 'Variance of T field (deg C^2)'
      write(433,*) var_Tsurf_vol
      write(433,*) 'Mean of Tobs field (deg C)'
      write(433,*) mean_Tsurfobs_vol
      write(433,*) 'Variance of Tobs field (deg C^2)'
      write(433,*) var_Tsurfobs_vol
      write(433,*) 'RMS model-data difference (deg C)'
      write(433,*) rmsnorm_Tsurf_vol
      write(433,*) '# *** 2-dimensional sea-surface temp. field ***'
      write(433,*) '# statistics of individual, unweighted values'
      write(433,*) '# ============================================='
      write(433,*) '# Number of individual points of the temp. field:'
      write(433,*) n_Tsurf
      write(433,*) '# Mean of T field (deg C)'
      write(433,*) mean_Tsurf
      write(433,*) '# Variance of T field (deg C^2)'
      write(433,*) var_Tsurf
      write(433,*) '# Mean of Tobs field (deg C)'
      write(433,*) mean_Tsurfobs
      write(433,*) '# Variance of Tobs field (deg C^2)'
      write(433,*) var_Tsurfobs
      write(433,*) '# # RMS model-data difference (deg C)'
      write(433,*) rmsnorm_Tsurf
      write(433,*) '# *** 3-dimensional salinity field ***'
      write(433,*) '# volume-weighted statistics'
      write(433,*) '# ===================================='
      write(433,*) '# Mean of S field (psu)'
      write(433,*) mean_S_vol
      write(433,*) '# Variance of S field (psu^2)'
      write(433,*) var_S_vol
      write(433,*) '# Mean of Sobs field (psu)'
      write(433,*) mean_Sobs_vol
      write(433,*) '# Variance of Sobs field (psu^2)'
      write(433,*) var_Sobs_vol
      write(433,*) '# RMS model-data difference (psu)'
      write(433,*) rmsnorm_S_vol
      write(433,*) '# *** 3-dimensional salinity field ***'
      write(433,*) '# statistics of individual, unweighted'
      write(433,*) '# values'
      write(433,*) '# ===================================='
      write(433,*) '# Number of individual points of the sal. field:'
      write(433,*) n_S
      write(433,*) '# Mean of S field (psu):'
      write(433,*) mean_S
      write(433,*) '# Variance of S field (psu^2):'
      write(433,*) var_S
      write(433,*) '# Mean of Sobs field (psu):'
      write(433,*) mean_Sobs
      write(433,*) '# Variance of Sobs field (psu^2):'
      write(433,*) var_Sobs
      write(433,*) '# RMS model-data difference (psu):'
      write(433,*) rmsnorm_S
      write(433,*) '# *** 2-dimensional sea-surface salinity field ***'
      write(433,*) '# area-weighted statistics'
      write(433,*) '# ================================================'
      write(433,*) '# Mean of S field (area weighted) (psu):'
      write(433,*) mean_Ssurf_vol
      write(433,*) '# Variance of S field (area weighted) (psu^2):'
      write(433,*) var_Ssurf_vol
      write(433,*) '# Mean of Sobs field (area weighted) (psu):'
      write(433,*) mean_Ssurfobs_vol
      write(433,*) '# Variance of Sobs field (area weighted) (psu^2):'
      write(433,*) var_Ssurfobs_vol
      write(433,*) '# RMS model-data difference (area weighted) (psu):'
      write(433,*) rmsnorm_Ssurf_vol
      write(433,*) '# *** 2-dimensional sea-surface salinity field ***'
      write(433,*) '# statistics of individual, unweighted values'
      write(433,*) '# ================================================'
      write(433,*) '# Number of individual points of the sal. field:'
      write(433,*) n_Ssurf
      write(433,*) '# Mean of S field (psu):'
      write(433,*) mean_Ssurf
      write(433,*) '# Variance of S field (psu^2):'
      write(433,*) var_Ssurf
      write(433,*) '# Mean of Sobs field (psu):'
      write(433,*) mean_Ssurfobs
      write(433,*) '# Variance of Sobs field (psu^2):'
      write(433,*) var_Ssurfobs
      write(433,*) '# RMS model-data difference (psu):'
      write(433,*) rmsnorm_Ssurf
      write(433,*) '# ***      Circulation      ***'
      write(433,*) '# Circulation quantities in Sv'
      write(433,*) '# ============================='
      write(433,*) '# Minimum depth for overturning diagnostics (m):'
      write(433,*) opsi_depth_min
      write(433,*) '# Maximum Atlantic overturning strength (Sv):'
      write(433,*) opsia_max
      write(433,*) '# Latitude of max. of Atlantic overturning strength (Sv):'
      write(433,*) opsia_max_lat
      write(433,*) '# Depth of max. of Atlantic overturning strength (Sv):'
      write(433,*) opsia_max_depth
      write(433,*) '# Minimum Atlantic overturning strength (Sv):'
      write(433,*) opsia_min
      write(433,*) '# Maximum overturning strength at the equator in'//
     &     ' the Atlantic (Sv):'
      write(433,*) opsia_eq_max
      write(433,*) '# Minimum overturning strength at the equator in'//
     &     ' the Atlantic (Sv):'
      write(433,*) opsia_eq_min
      write(433,*) '# Uppermost depth of zero overturning value at the'//
     $     ' equator in the Atlantic (Sv):'
      write(433,*) opsia_eq_zero_depth
      write(433,*) '# Maximum Pacific overturning strength (Sv):'
      write(433,*) opsip_max
      write(433,*) '# Minimum Pacific overturning strength (Sv):'
      write(433,*) opsip_min
      write(433,*) '# Drake Passage through flow (Sv):'
      write(433,*) tf_drake
      endif

      if (flag_ebatmos) then
         call diag_embm_T_annual_av(yearstr,mean_Tatm ,mean_Tatm_area
     $        ,var_Tatm,var_Tatm_area,mean_Tatmobs ,mean_Tatmobs_area
     $        ,var_Tatmobs,var_Tatmobs_area ,rmsnorm_Tatm
     $        ,rmsnorm_Tatm_area,n_Tatm,mean_Tatm_NH_area
     $        ,mean_Tatm_SH_area)
         call diag_embm_q_annual_av(yearstr,mean_q ,mean_q_area
     $        ,var_q,var_q_area,mean_qobs ,mean_qobs_area
     $        ,var_qobs,var_qobs_area ,rmsnorm_q
     $        ,rmsnorm_q_area,n_q)
         call diag_embm_r_annual_av(yearstr,mean_r ,mean_r_area
     $        ,var_r,var_r_area,mean_robs ,mean_robs_area
     $        ,var_robs,var_robs_area ,rmsnorm_r
     $        ,rmsnorm_r_area,n_r)
      write(433,*) '# *** 2-dimensional surface air temp. field ***'
      write(433,*) '# area-weighted statistics'
      write(433,*) '# ============================================='
      write(433,*) '# Mean of Tatm field:'
      write(433,*) mean_Tatm_area
      write(433,*) '# Variance of Tatm field (deg C^2):'
      write(433,*) var_Tatm_area
      write(433,*) '# Mean of Tatmobs field (deg C):'
      write(433,*) mean_Tatmobs_area
      write(433,*) '# Variance of Tatmobs field (deg C^2):'
      write(433,*) var_Tatmobs_area
      write(433,*) '# RMS model-data difference (deg C):'
      write(433,*) rmsnorm_Tatm_area
      write(433,*) '# Mean of Tatm field in SH (deg C):'
      write(433,*) mean_Tatm_SH_area
      write(433,*) '# Mean of Tatm field in NH (deg C):'
      write(433,*) mean_Tatm_NH_area
      write(433,*) '# *** 2-dimensional surface air temp. field ***'
      write(433,*) '# statistics of individual, unweighted values'
      write(433,*) '# ============================================='
      write(433,*) '# Number of individual points of the temp. field:'
      write(433,*) n_Tatm
      write(433,*) '# Mean of Tatm field (deg C):'
      write(433,*) mean_Tatm
      write(433,*) '# Variance of Tatm field (deg C^2):'
      write(433,*) var_Tatm
      write(433,*) '# Mean of Tatmobs field (deg C):'
      write(433,*) mean_Tatmobs
      write(433,*) '# Variance of Tatmobs field (deg C^2):'
      write(433,*) var_Tatmobs
      write(433,*) '# RMS model-data difference (deg C):'
      write(433,*) rmsnorm_Tatm
      write(433,*) '# *** 2-dimensional surf. spec. hum. field ***'
      write(433,*) '# simulated humidity after precipitation'
      write(433,*) '# area-weighted statistics'
      write(433,*) '# ================================================'
      write(433,*) '# Mean of q field (kg/kg):'
      write(433,*) mean_q_area
      write(433,*) '# Variance of q field (kg^2/kg^2):'
      write(433,*) var_q_area
      write(433,*) '# Mean of qobs field (kg/kg):'
      write(433,*) mean_qobs_area
      write(433,*) '# Variance of qobs field (kg^2/kg^2):'
      write(433,*) var_qobs_area
      write(433,*) '# RMS model-data difference (kg/kg):'
      write(433,*) rmsnorm_q_area
      write(433,*) '# *** 2-dimensional surf. spec. hum. field ***'
      write(433,*) '# simulated humidity after precipitation'
      write(433,*) '# statistics of individual, unweighted values'
      write(433,*) '# ================================================'
      write(433,*) '# Number of individual points of the hum. field:'
      write(433,*) n_q
      write(433,*) '# Mean of q field (kg/kg):'
      write(433,*) mean_q
      write(433,*) '# Variance of q field (kg^2/kg^2):'
      write(433,*) var_q
      write(433,*) '# Mean of qobs field (kg/kg):'
      write(433,*) mean_qobs
      write(433,*) '# Variance of qobs field (kg^2/kg^2):'
      write(433,*) var_qobs
      write(433,*) '# RMS model-data difference (kg/kg):'
      write(433,*) rmsnorm_q
      write(433,*) '# *** 2-dimensional surf. rel. hum. field ***'
      write(433,*) '# simulated humidity after precipitation'
      write(433,*) '# area-weighted statistics'
      write(433,*) '# ==============================================='
      write(433,*) '# Mean of r field (1):'
      write(433,*) mean_r_area
      write(433,*) '# Variance of r field (1):'
      write(433,*) var_r_area
      write(433,*) '# Mean of robs field (1):'
      write(433,*) mean_robs_area
      write(433,*) '# Variance of robs field (1):'
      write(433,*) var_robs_area
      write(433,*) '# RMS model-data difference (1):'
      write(433,*) rmsnorm_r_area
      write(433,*) '# *** 2-dimensional surf. rel. hum. field ***'
      write(433,*) '# simulated humidity after precipitation'
      write(433,*) '# statistics of individual, unweighted values'
      write(433,*) '# ==============================================='
      write(433,*) '# Number of individual points of the hum. field:'
      write(433,*) n_r
      write(433,*) '# Mean of r field (1):'
      write(433,*) mean_r
      write(433,*) '# Variance of r field (1):'
      write(433,*) var_r
      write(433,*) '# Mean of robs field (1):'
      write(433,*) mean_robs
      write(433,*) '# Variance of robs field (1):'
      write(433,*) var_robs
      write(433,*) '# RMS model-data difference (1):'
      write(433,*) rmsnorm_r
      endif

      if (flag_goldsteinseaice) then
         call diag_goldsteinseaice_ice_annual_av(yearstr,ice_vol
     $        ,ice_area,ice_vol_SH,ice_area_SH,ice_vol_NH,ice_area_NH)

      write(433,*) '# *** integrals of 2-dimensional'
      write(433,*) '# sea-ice height and fractional cover ***'
      write(433,*) '# ======================================='
      write(433,*) '# Ice volume (m^3)'
      write(433,*) ice_vol
      write(433,*) '# Ice area (m^2)'
      write(433,*) ice_area
      write(433,*) '# SH ice volume (m^3)'
      write(433,*) ice_vol_SH
      write(433,*) '# SH ice area (m^2)'
      write(433,*) ice_area_SH
      write(433,*) '# NH ice volume (m^3)'
      write(433,*) ice_vol_NH
      write(433,*) '# NH ice area (m^2)'
      write(433,*) ice_area_NH
      endif

 435  format(5f20.16)
      close(433)

      call message("",1)
      call message
     $     ('*******************************************************',1)
      call message(' Shutting down, arrivederci',1)
      call message
     $     ('*******************************************************',1)
      call message("",1)
c
      stop
      end
