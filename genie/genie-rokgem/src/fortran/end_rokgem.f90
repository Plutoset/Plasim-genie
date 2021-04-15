
! File: end_rokgem.f90
!
! Description: shuts down RokGeM

SUBROUTINE end_rokgem()

  USE rokgem_lib, ONLY: deallocate_arrays

  print*,' '
  print*,'======================================================='
  print*,' Initialising rokgem module shutdown'
  print*,'======================================================='
  print*,' '

  call rest_rokgem()
  call deallocate_arrays()

  print*,' '
  print*,'======================================================='
  print*,' rokgem module shutdown'
  print*,'======================================================='
  print*,' '

END SUBROUTINE end_rokgem
! ******************************************************************************************************************************** !
