
! ******************************************************************************************************************************** !
! cpl_comp_atmocn.f90
! AtCheM interface atmospheric compositional integrator
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE AtChem atmospheric composition
SUBROUTINE cpl_comp_atmocn(     &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_sfcatm,              &
     & dum_sfcatm1              &
     & )
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
  real,dimension(dum_n_atm,dum_n_i_atm,dum_n_j_atm),intent(in)::dum_sfcatm     ! atmosphere-surface tracer composition; atm grid
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_sfcatm1 ! atmosphere-surface tracer composition; ocn grid
  ! 
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
  ! NOTE: do not copy the first 2 tracers (SAT and humidity) as these values are set directly by the EMBM
  dum_sfcatm1(3:dum_n_atm,:,:) = dum_sfcatm(3:dum_n_atm,:,:)
  ! 
end SUBROUTINE cpl_comp_atmocn
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE EMBM TRACERS
SUBROUTINE cpl_comp_EMBM(       &
     & dum_n_atm,               &
     & dum_n_i_atm,dum_n_j_atm, &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_t,                   &
     & dum_q,                   &
     & dum_sfcatm1)
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_atm
  integer,intent(in)::dum_n_i_atm,dum_n_j_atm
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn
  real,dimension(dum_n_i_atm,dum_n_j_atm),intent(in)::dum_t
  real,dimension(dum_n_i_atm,dum_n_j_atm),intent(in)::dum_q
  real,dimension(dum_n_atm,dum_n_i_ocn,dum_n_j_ocn),intent(inout)::dum_sfcatm1 ! atmosphere-surface tracer composition; ocn grid
  ! 
  ! ANY DIFFERENCE BETWEEN OCEAN AND ATMOSPHERE GRIDS WILL HAVE TO BE TAKEN INTO ACCOUNT HERE
  ! NOTE: currently no summation done!
  dum_sfcatm1(1,:,:) = dum_t(:,:)
  dum_sfcatm1(2,:,:) = dum_q(:,:)
  ! 
end SUBROUTINE cpl_comp_EMBM
! ******************************************************************************************************************************** !
