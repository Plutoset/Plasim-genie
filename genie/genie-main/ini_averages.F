      subroutine ini_averages
      
      use precision
      use genie_control
      use averages
      use genie_util, only : message

      implicit none

c     set atmosphere variables to zero
      if (write_flag_atm) then
         av_netsolar_atm(:,:)    = 0.0
         av_netlong_atm(:,:)     = 0.0
         av_sensible_atm(:,:)    = 0.0
         av_latent_atm(:,:)      = 0.0
         av_stressx_atm(:,:)     = 0.0
         av_stressy_atm(:,:)     = 0.0
         av_seaiceflux_atm(:,:)  = 0.0
         av_conductflux_atm(:,:) = 0.0
         av_evap_atm(:,:)        = 0.0
         av_precip_atm(:,:)      = 0.0
         av_runoff_atm(:,:)      = 0.0
         av_waterflux_atm(:,:)   = 0.0
         av_seaicefrac_atm(:,:)  = 0.0
         av_tstar_atm(:,:)       = 0.0
         av_albedo_atm(:,:)      = 0.0
      endif

c     set ocean variables to zero
      if (write_flag_ocn) then
         av_netsolar_ocn(:,:)    = 0.0
         av_netlong_ocn(:,:)     = 0.0
         av_sensible_ocn(:,:)    = 0.0
         av_latent_ocn(:,:)      = 0.0
         av_stressx_ocn(:,:)     = 0.0
         av_stressy_ocn(:,:)     = 0.0
         av_seaiceflux_ocn(:,:)  = 0.0
         av_conductflux_ocn(:,:) = 0.0
         av_evap_ocn(:,:)        = 0.0
         av_precip_ocn(:,:)      = 0.0
         av_runoff_ocn(:,:)      = 0.0
         av_waterflux_ocn(:,:)   = 0.0
         av_seaicefrac_ocn(:,:)  = 0.0
         av_tstar_ocn(:,:)       = 0.0
         av_albedo_ocn(:,:)      = 0.0
      endif

c     set sea-ice variables to zero
      if (write_flag_sic) then
         av_netsolar_sic(:,:)    = 0.0
         av_netlong_sic(:,:)     = 0.0
         av_sensible_sic(:,:)    = 0.0
         av_latent_sic(:,:)      = 0.0
         av_stressx_sic(:,:)     = 0.0
         av_stressy_sic(:,:)     = 0.0
         av_seaiceflux_sic(:,:)  = 0.0
         av_conductflux_sic(:,:) = 0.0
         av_evap_sic(:,:)        = 0.0
         av_precip_sic(:,:)      = 0.0
         av_runoff_sic(:,:)      = 0.0
         av_waterflux_sic(:,:)   = 0.0
         av_seaicefrac_sic(:,:)  = 0.0
         av_tstar_sic(:,:)       = 0.0
         av_albedo_sic(:,:)      = 0.0
      endif

      call message("initialised variables that hold averages",1)

      return
      end
