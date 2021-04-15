!#######################################################################################
!  Beginnings of a land end-of-run tidy-up routine
!  PPH 15/01/04
!#######################################################################################
SUBROUTINE end_land

  USE land_var, only : deallocate_land_var
  USE land_diags, only : deallocate_land_diags
  USE land_runoff, only : deallocate_land_runoff

  IMPLICIT NONE

  PRINT*,'LAND %% Ending land scheme.  Thankyou for flying with GENIE-land.'

  call deallocate_land_var()
  call deallocate_land_diags()
  call deallocate_land_runoff()

  RETURN
END SUBROUTINE end_land
