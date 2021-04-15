
! ******************************************************************************************************************************** !
! cpl_comp_atmlnd.f90 - modified copy of cpl_comp_atmocn.f90
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE AtChem atmospheric composition
SUBROUTINE cpl_comp_atmlnd(     &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_n_i_lnd,dum_n_j_lnd, &
     & dum_sfcatm,              &
     & dum_sfcatm_lnd           &
     & )
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  integer,intent(in)::dum_n_i_lnd,dum_n_j_lnd
  real,dimension(dum_n_atm,dum_n_i_atm,dum_n_j_atm),intent(in)::dum_sfcatm        ! atmosphere-surface tracer composition; atm grid
  real,dimension(dum_n_atm,dum_n_i_lnd,dum_n_j_lnd),intent(inout)::dum_sfcatm_lnd ! atmosphere-surface tracer composition; lnd grid
  ! 
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
  ! NOTE: do not copy the first 2 tracers (SAT and humidity) as these values are set directly by the EMBM
  dum_sfcatm_lnd(3:dum_n_atm,:,:) = dum_sfcatm(3:dum_n_atm,:,:)
  ! 
end SUBROUTINE cpl_comp_atmlnd
! ******************************************************************************************************************************** !

