! 
! File:          land_landembm_Impl.F90
! Symbol:        land.landembm-v0.1.1
! Symbol Type:   class
! Babel Version: 0.11.0
! sidl Created:  20061129 18:14:01 GMT
! Generated:     20061129 18:14:10 GMT
! Description:   Server-side implementation for land.landembm
! 
! WARNING: Automatically generated; only changes within splicers preserved
! 
! babel-version = 0.11.0
! source-line   = 15
! source-url    = file:/homes/sp1003/genie-babel/genie-land/babel/land.sidl
! 


! 
! Symbol "land.landembm" (version 0.1.1)
! 


#include "land_landembm_fAbbrev.h"
#include "land_landembm_interface_fAbbrev.h"
#include "sidl_ClassInfo_fAbbrev.h"
#include "sidl_BaseInterface_fAbbrev.h"
#include "sidl_RuntimeException_fAbbrev.h"
#include "sidl_BaseException_fAbbrev.h"
#include "sidl_BaseClass_fAbbrev.h"
#include "sidl_double_fAbbrev.h"
! DO-NOT-DELETE splicer.begin(_miscellaneous_code_start)
! Insert-Code-Here {_miscellaneous_code_start} (extra code)
! DO-NOT-DELETE splicer.end(_miscellaneous_code_start)




! 
! Method:  _ctor[]
! Class constructor called when the class is created.
! 

recursive subroutine land_landembm__ctor_mi(self, exception)
  use sidl
  use sidl_BaseInterface
  use sidl_RuntimeException
  use land_landembm
  use land_landembm_impl
  ! DO-NOT-DELETE splicer.begin(land.landembm._ctor.use)
  ! Insert-Code-Here {land.landembm._ctor.use} (use statements)
  ! DO-NOT-DELETE splicer.end(land.landembm._ctor.use)
  implicit none
  type(land_landembm_t) :: self ! in
  type(sidl_BaseInterface_t) :: exception ! out

! DO-NOT-DELETE splicer.begin(land.landembm._ctor)
! Insert-Code-Here {land.landembm._ctor} (_ctor method)
! DO-NOT-DELETE splicer.end(land.landembm._ctor)
end subroutine land_landembm__ctor_mi


! 
! Method:  _dtor[]
! Class destructor called when the class is deleted.
! 

recursive subroutine land_landembm__dtor_mi(self, exception)
  use sidl
  use sidl_BaseInterface
  use sidl_RuntimeException
  use land_landembm
  use land_landembm_impl
  ! DO-NOT-DELETE splicer.begin(land.landembm._dtor.use)
  ! Insert-Code-Here {land.landembm._dtor.use} (use statements)
  ! DO-NOT-DELETE splicer.end(land.landembm._dtor.use)
  implicit none
  type(land_landembm_t) :: self ! in
  type(sidl_BaseInterface_t) :: exception ! out

! DO-NOT-DELETE splicer.begin(land.landembm._dtor)
! Insert-Code-Here {land.landembm._dtor} (_dtor method)
! DO-NOT-DELETE splicer.end(land.landembm._dtor)
end subroutine land_landembm__dtor_mi


! 
! Method:  _load[]
! Static class initializer called exactly once before any user-defined method is dispatched
! 

recursive subroutine land_landembm__load_mi(exception)
  use sidl
  use sidl_BaseInterface
  use sidl_RuntimeException
  use land_landembm
  use land_landembm_impl
  ! DO-NOT-DELETE splicer.begin(land.landembm._load.use)
  ! Insert-Code-Here {land.landembm._load.use} (use statements)
  ! DO-NOT-DELETE splicer.end(land.landembm._load.use)
  implicit none
  type(sidl_BaseInterface_t) :: exception ! out

! DO-NOT-DELETE splicer.begin(land.landembm._load)
! Insert-Code-Here {land.landembm._load} (_load method)
! DO-NOT-DELETE splicer.end(land.landembm._load)
end subroutine land_landembm__load_mi


! 
! Method:  run_land_embm_combine[]
! 

recursive subroutine run_land_embm_comb2fy4ttk26m_mi(self,                     &
  surf_sensible_atm_bbl, evap_atm_bbl, runoff_ocn_bbl,                         &
  land_sensibleinst_atm_bbl, land_evap_atm_bbl, land_runoff_atm_bbl, retval,   &
  exception)
  use sidl
  use sidl_BaseInterface
  use sidl_RuntimeException
  use land_landembm
  use sidl_double_array
  use land_landembm_impl
  ! DO-NOT-DELETE splicer.begin(land.landembm.run_land_embm_combine.use)
  ! Insert-Code-Here {land.landembm.run_land_embm_combine.use} (use statements)

  use genie_control
  use castings
    
  ! DO-NOT-DELETE splicer.end(land.landembm.run_land_embm_combine.use)
  implicit none
  type(land_landembm_t) :: self ! in
  type(sidl_double_2d) :: surf_sensible_atm_bbl ! inout
  type(sidl_double_2d) :: evap_atm_bbl ! inout
  type(sidl_double_2d) :: runoff_ocn_bbl ! inout
  type(sidl_double_2d) :: land_sensibleinst_atm_bbl ! in
  type(sidl_double_2d) :: land_evap_atm_bbl ! in
  type(sidl_double_2d) :: land_runoff_atm_bbl ! in
  integer (kind=sidl_int) :: retval ! out
  type(sidl_BaseInterface_t) :: exception ! out

! DO-NOT-DELETE splicer.begin(land.landembm.run_land_embm_combine)
! Insert-Code-Here {land.landembm.run_land_embm_combine} (run_land_embm_combine method)

  real, dimension(ilon1_atm, ilat1_atm) :: surf_sensible_atm = 0.0
  real, dimension(ilon1_atm, ilat1_atm) :: evap_atm = 0.0
  real, dimension(ilon1_ocn, ilat1_ocn) :: runoff_ocn = 0.0
  real, dimension(ilon1_atm, ilat1_atm) :: land_sensibleinst_atm = 0.0
  real, dimension(ilon1_atm, ilat1_atm) :: land_evap_atm = 0.0
  real, dimension(ilon1_atm, ilat1_atm) :: land_runoff_atm = 0.0
  
  call castSidlDouble2DtoSimple2D(surf_sensible_atm_bbl, surf_sensible_atm)  
  call castSidlDouble2DtoSimple2D(evap_atm_bbl, evap_atm)  
  call castSidlDouble2DtoSimple2D(runoff_ocn_bbl, runoff_ocn)  
  call castSidlDouble2DtoSimple2D(land_sensibleinst_atm_bbl, land_sensibleinst_atm)
  call castSidlDouble2DtoSimple2D(land_evap_atm_bbl, land_evap_atm)
  call castSidlDouble2DtoSimple2D(land_runoff_atm_bbl, land_runoff_atm)

  call land_embm_combine(surf_sensible_atm, evap_atm, &
                     runoff_ocn, land_sensibleinst_atm, &
                     land_evap_atm, land_runoff_atm)

  call create2dcol(ilon1_atm, ilat1_atm, surf_sensible_atm_bbl)
  call create2dcol(ilon1_atm, ilat1_atm, evap_atm_bbl)
  call create2dcol(ilon1_ocn, ilat1_ocn, runoff_ocn_bbl)
  
  call castSimple2DtoSidlDouble2D(surf_sensible_atm, surf_sensible_atm_bbl)
  call castSimple2DtoSidlDouble2D(evap_atm, evap_atm_bbl)
  call castSimple2DtoSidlDouble2D(runoff_ocn, runoff_ocn_bbl)


! DO-NOT-DELETE splicer.end(land.landembm.run_land_embm_combine)
end subroutine run_land_embm_comb2fy4ttk26m_mi


! DO-NOT-DELETE splicer.begin(_miscellaneous_code_end)
! Insert-Code-Here {_miscellaneous_code_end} (extra code)
! DO-NOT-DELETE splicer.end(_miscellaneous_code_end)
