! ******************************************************************************************************************************** !
! gem_data.f90
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE gem_data

  
  USE gem_cmn
  IMPLICIT NONE
  SAVE
  
  
CONTAINS
  
  
  ! ****************************************************************************************************************************** !
  ! LOAD SEDGEM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_gem()
    ! local variables
    integer::ios                                                 !
    ! read data_GEM file
    open(unit=in,file='data_GEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open GEM initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_GEM file
    read(UNIT=in,NML=ini_gem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read GEM namelist'
       stop
    else
       close(unit=in)
    end if
    ! set and report namelist data
    ! --- TRACER SELECTION  ------------------------------------------------------------------------------------------------------ !
    ! NOTE: reported at end of initialise_gem when tracer name information is available
    ! --- MISC CONTROLS  --------------------------------------------------------------------------------------------------------- !
    print*,'--- MISC CONTROLS ---'
    print*,'assumed longitudinal offset of the grid             : ',par_grid_lon_offset
    par_carbconstset_name = trim(par_carbconstset_name)//'/'
    print*,'carbonate dissociation constants set                : ',trim(par_carbconstset_name)
    print*,'pH solution tolerance                               : ',par_carbchem_pH_tolerance
    print*,'pH solution maximum number of iterations            : ',par_carbchem_pH_iterationmax
    print*,'Exit upon pH solution failure?                      : ',ctrl_carbchem_fail
    ! --- I/O: DIRECTORY DEFINITIONS --------------------------------------------------------------------------------------------- !
    print*,'--- I/O: DIRECTORY DEFINITIONS ---'
    par_gem_indir_name = trim(par_gem_indir_name)//'/'
    print*,'Input dir. name                                     : ',trim(par_gem_indir_name)
    print*,'filetype for series output files for GEM modules    : ',trim(string_results_ext)
    ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ############################################################################# !
    !
    ! ############################################################################################################################ !
  END SUBROUTINE sub_load_goin_gem
  ! ****************************************************************************************************************************** !


END MODULE gem_data
