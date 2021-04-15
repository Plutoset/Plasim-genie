! ******************************************************************
! common module for atmospheric geochemistry -
! should take from ocean biogenie instead ???
! JSS
!
! ******************************************************************

module ichem_util

      USE ichem_var

      implicit none

contains

! isotope delta calculations

    FUNCTION fun_calc_isotope_delta(tot_mass,iso_mass,loc_standard)

      implicit none
      real::fun_calc_isotope_delta
      real,intent(in)::tot_mass,iso_mass,loc_standard
      real::fractional_abundance,loc_R
      
      if(tot_mass.gt.+0.99999e-19) then
          fractional_abundance = iso_mass/tot_mass
      else
          fractional_abundance = 0.0
      end if

      loc_R = fractional_abundance/(1.0 - fractional_abundance)

      fun_calc_isotope_delta = 1000.0*(loc_R/loc_standard - 1.0)
      
    END FUNCTION fun_calc_isotope_delta

      
end module ichem_util
