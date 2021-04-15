
! ******************************************************************************************************************************** !
! END BioGeM
SUBROUTINE end_biogem()
  USE biogem_lib
  USE biogem_data
  
  print*,' '
  print*,'======================================================='
  print*,' Initialising BIOGEM module shutdown'
  print*,'======================================================='
  print*,' '

  ! *** output audit diagnostics ***
  ! reporting of initial/final tracer inventories
  IF (ctrl_audit) THEN
     PRINT*,' '
     PRINT*,'*** BioGeM tracer audit diagnostics ***'
     CALL sub_data_audit_diagnostics()
  END IF

  IF (ctrl_debug_lvl2) PRINT*,'saving netcdf record numbers',ncout2d_ntrec,ncout3d_ntrec
  OPEN(unit=out,status='replace',file=TRIM(par_outdir_name)//'netcdf_record_numbers',form='formatted',action='write')
  WRITE(unit=out,fmt='(i6)') ncout2d_ntrec,ncout3d_ntrec                             
  close(unit=out)

  print*,' '
  print*,'======================================================='
  print*,' BIOGEM module shutdown'
  print*,'======================================================='
  print*,' '

END SUBROUTINE end_biogem
! ******************************************************************************************************************************** !
