MODULE averages

  use precision
  use genie_control

  implicit none

  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_netsolar_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_netlong_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_sensible_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_latent_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_stressx_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_stressy_atm

  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_netsolar_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_netlong_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_sensible_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_latent_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_stressx_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_stressy_ocn

  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_netsolar_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_netlong_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_sensible_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_latent_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_stressx_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_stressy_sic
        
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_seaiceflux_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_conductflux_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_evap_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_precip_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_runoff_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_waterflux_atm

  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_seaiceflux_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_conductflux_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_evap_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_precip_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) ::  av_runoff_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_waterflux_ocn

  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_seaiceflux_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_conductflux_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_evap_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_precip_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_runoff_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_waterflux_sic

  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_seaicefrac_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_tstar_atm
  real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) :: av_albedo_atm

  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_seaicefrac_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_tstar_ocn
  real(rk_libnc1), dimension(ilon1_ocn,ilat1_ocn) :: av_albedo_ocn

  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_seaicefrac_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_tstar_sic
  real(rk_libnc1), dimension(ilon1_sic,ilat1_sic) :: av_albedo_sic

  real(rk_libnc1), dimension(ilon1_atm) :: av_alon1_atm
  real(rk_libnc1), dimension(ilat1_atm) :: av_alat1_atm
  real(rk_libnc1), dimension(ilon1_ocn) :: av_alon1_ocn
  real(rk_libnc1), dimension(ilat1_ocn) :: av_alat1_ocn
  real(rk_libnc1), dimension(ilon1_sic) :: av_alon1_sic
  real(rk_libnc1), dimension(ilat1_sic) :: av_alat1_sic
   
END MODULE averages
