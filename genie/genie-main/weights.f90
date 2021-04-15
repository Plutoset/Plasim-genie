MODULE weights

  use genie_control

  real, dimension(ilon1_atm,ilat1_atm) :: weight_atm
  real, dimension(ilon1_ocn,ilat1_ocn) :: weight_ocn
  real, dimension(ilon1_sic,ilat1_sic) :: weight_sic

END MODULE weights
