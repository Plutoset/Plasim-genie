! ******************************************************************************************************************************** !
! SETUP GEM
SUBROUTINE initialise_gem()
  USE gem_util
  USE gem_data
  ! local variables
  integer::ia,io,is                                              ! tracer counter

  print*,' '
  print*,'======================================================='
  print*,' Initialising GEM ocean geochemistry module'
  print*,'======================================================='

  ! *** load goin information ***
  call sub_load_goin_gem()

  ! *** initialize GeM ***
  ! initialize tracer definitions
  CALL sub_init_tracer_atm()
  CALL sub_init_tracer_ocn()
  CALL sub_init_tracer_sed()
  
  print*,'--- TRACER SELECTION ---'
  print*,'Selected atmosphere tracers:                        : '
  do ia=1,n_atm
     if (atm_select(ia)) print*,ia,' = ',trim(string_longname_atm(ia))
  end do
  print*,'Selected ocean tracers:                             : '
  do io=1,n_ocn
     if (ocn_select(io)) print*,io,' = ',trim(string_longname_ocn(io))
  end do
  print*,'Selected sediment tracers:                          : '
  do is=1,n_sed
     if (sed_select(is)) print*,is,' = ',trim(string_longname_sed(is))
  end do
  
  print*,'======================================================='
  print*,' Initialisation of GEM ocean geochemistry module complete'
  print*,'======================================================='
  print*,' '

end SUBROUTINE initialise_gem
! ******************************************************************************************************************************** !
