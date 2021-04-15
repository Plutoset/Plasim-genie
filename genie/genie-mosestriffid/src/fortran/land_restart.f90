!$Id: land_restart.f90 2510 2005-07-15 09:19:38Z cvs-gw $
!#######################################################################################
!  Module containing variables and methods/subroutines used for 
!  reading and writing land restart files.  Calling tree is given
!  below
!  
!  land_restart_read - land_restart_setup
!                    - land_nc_read              - land_nc_get
!  land_restart_write- land_restart_create_fname
!                    - land_restart_wdata_precise
!
!  Fields are written on vectors of land points rather than lat/long 
!  lat/long grid for smaller files.
!  PPH 24/5/04
!
!  Rewritten to not use libnc1 as the routines in that library are a bit restrictive
!  e.g. only allows definition of 4 dimensions per netCDF file, as opposed to up to
!  512 allowed by the netcdf library itself.
!  PPH 11/07/2005 
!#######################################################################################
MODULE land_restart
  INTERFACE land_nc_get
    MODULE PROCEDURE land_nc_get_0d, land_nc_get_1d, land_nc_get_2d, land_nc_get_3d, land_nc_get_4d
  END INTERFACE

  INTEGER :: nco      ! Netcdf file ID
  INTEGER :: status   ! Output status from netCDF library routines
CONTAINS
!#######################################################################################
!  READING RESTART FILES
!#######################################################################################
  SUBROUTINE land_restart_read(fname)
    IMPLICIT NONE
    CHARACTER(len=*),INTENT(in) :: fname

    CALL land_restart_read_open(fname)
    CALL land_nc_read
    PRINT*,'LAND %% Restart read complete'

    RETURN
  END SUBROUTINE land_restart_read

!#######################################################################################
  SUBROUTINE land_restart_read_open(fname)
    USE land_var, only : nstep_land, init_doy_angle
    USE netcdf
    IMPLICIT NONE
    CHARACTER(len=*),INTENT(in) :: fname
    LOGICAL :: lexist
    
    !#####################################################
    ! Land time variables required for a perfect restart
    !#####################################################
    INQUIRE(FILE=TRIM(fname)//'.cleanrstart',EXIST=lexist)
    IF(lexist) THEN
      OPEN(UNIT=45,FILE=TRIM(fname)//'.cleanrstart')
      READ(45,*) nstep_land, init_doy_angle
      CLOSE(45)
    ELSE
      PRINT*,'LAND %% Cannot find file '//TRIM(fname)//'.cleanrstart'//', so using default values:'
      PRINT*,'LAND %% nstep_land =',nstep_land
      PRINT*,'LAND %% init_doy_angle =',init_doy_angle
    ENDIF  

    !#####################################################
    ! Open existing netCDF file for read
    !#####################################################
    PRINT*,'LAND %% Reading restart file '//TRIM(fname)
    status = NF90_OPEN(fname, nf90_nowrite, nco)
    IF (status /= nf90_noerr) THEN
      PRINT*,'LAND %% Error opening netCDF restart file ', TRIM(fname)
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      STOP
    ENDIF

    RETURN
  END SUBROUTINE land_restart_read_open

!#######################################################################################
  SUBROUTINE land_nc_read

    USE land_var
    USE land_diags
    USE netcdf

    IMPLICIT NONE

    status = land_nc_get(nco,'cs',cs)
    status = land_nc_get(nco,'lying_snow',lying_snow)
    status = land_nc_get(nco,'msoil',msoil)
    status = land_nc_get(nco,'tsub1',tsub1)
    status = land_nc_get(nco,'tstar_gb',tstar_gb)
    status = land_nc_get(nco,'fx_le',fx_le)
    status = land_nc_get(nco,'fx_sen',fx_sen)
    status = land_nc_get(nco,'z0_gb',z0_gb)
    status = land_nc_get(nco,'albedo_gb',albedo_gb)
    status = land_nc_get(nco,'evap',evap)
    status = land_nc_get(nco,'esub',esub)
    status = land_nc_get(nco,'runoff',gravdr)
    status = land_nc_get(nco,'snowmelt',snowmelt_acc)
    status = land_nc_get(nco,'resp_s_dr',resp_s_dr)

    status = land_nc_get(nco,'frac',frac)
    status = land_nc_get(nco,'lai',lai)
    status = land_nc_get(nco,'ht',ht)
    status = land_nc_get(nco,'g_leaf_phen_acc',g_leaf_phen_acc)
    status = land_nc_get(nco,'g_leaf_acc',g_leaf_acc)
    status = land_nc_get(nco,'gpp_dr',gpp_dr)
    status = land_nc_get(nco,'npp_dr',npp_dr)
    status = land_nc_get(nco,'resp_w_dr',resp_w_dr)
    status = land_nc_get(nco,'tstar',tstar)

    ! If any of these variables are not present in the restart file,
    ! land_nc_get issues a warning, zeros the array and continues.
    status = land_nc_get(nco,'igcm_swnet',igcm_swnet,l_ignorevar=.true.)
    status = land_nc_get(nco,'igcm_lwnet',igcm_lwnet,l_ignorevar=.true.)
    status = land_nc_get(nco,'igcm_prec',igcm_prec,l_ignorevar=.true.)
    status = land_nc_get(nco,'igcm_z1',igcm_z1,l_ignorevar=.true.)

    ! If any of these variables are not present in the restart file,
    ! or if there is a dimension conflict, land_nc_get issues a warning, zeros 
    ! the array and continues.  This is to prevent having to remake new standard 
    ! restart files each time a diagnostic is added removed.
    status = land_nc_get(nco,'acc_diags',diag_var,l_ignorevar=.true.,l_ignoredim=.true.)
    status = land_nc_get(nco,'acc_diags_tile',diag_var_tile,l_ignorevar=.true.,l_ignoredim=.true.)

    status = land_nc_get(nco,'nstep_land',retarri=nstep_land)
    status = land_nc_get(nco,'init_doy_angle',retarrf=init_doy_angle)

    status = NF90_CLOSE(nco)

    RETURN
  END SUBROUTINE land_nc_read

!#######################################################################################
! WRITING RESTART FILES
!#######################################################################################
  SUBROUTINE land_restart_write(dirname)
    IMPLICIT NONE
 
    CHARACTER(len=*),INTENT(in) :: dirname
    CHARACTER(len=20)           :: wfname

    CALL land_restart_create_fname(wfname)
    CALL land_restart_wdata_precise(dirname,wfname)

    RETURN
  END SUBROUTINE land_restart_write

!#######################################################################################
  SUBROUTINE land_restart_create_fname(wfname)
    USE land_var, only : nstep_land
    IMPLICIT NONE
    CHARACTER(len=20),INTENT(out) :: wfname
    CHARACTER(len=8) :: cstep
    
    WRITE(cstep,'(i8.8)')nstep_land
    wfname='land_rs_'//cstep//'.nc'

    RETURN
  END SUBROUTINE land_restart_create_fname

!#######################################################################################
  SUBROUTINE land_restart_wdata_precise(dirname,wfname)

    USE land_const, only : land_pts,ntype
    USE land_var
    USE land_diags
    USE netcdf

    IMPLICIT NONE

    CHARACTER(len=*),INTENT(in) :: dirname
    CHARACTER(len=*),INTENT(in) :: wfname
    CHARACTER(len=200)          :: fullnom

    INTEGER :: i

!   FOR NETCDF:
    INTEGER :: ncid
    INTEGER :: lndcoordid,tilcoordid,pftcoordid,dcoordid,dtcoordid

    INTEGER :: lid,tid,pid,did,iid

    INTEGER :: csid,lying_snowid,msoilid,tsub1id,tstar_gbid
    INTEGER :: fx_leid,fx_senid,z0_gbid,albedo_gbid,evapid
    INTEGER :: esubid,gravdrid,snowmelt_accid,resp_s_drid
    INTEGER :: igcm_swnetid,igcm_lwnetid,igcm_precid,igcm_z1id

    INTEGER :: fracid,laiid,htid,g_leaf_phen_accid,g_leaf_accid
    INTEGER :: gpp_drid,npp_drid,resp_w_drid,tstarid

    INTEGER :: diag_varid,diag_var_tileid

    INTEGER :: nstepid, doyid

    INTEGER :: dimpass(2)
    INTEGER :: dimpass3(3)

    REAL,DIMENSION(land_pts)    :: lndcoord
    REAL,DIMENSION(ntype)       :: tilcoord
    REAL,DIMENSION(npft)        :: pftcoord
    REAL,DIMENSION(ndiags)      :: dcoord
    REAL,DIMENSION(ndiags_tile) :: dtcoord

    !LAND POINTS
    DO i=1,land_pts
      lndcoord(i) = REAL(i)
    ENDDO

    !TILES
    DO i=1,ntype
      tilcoord(i) = REAL(i)
    ENDDO

    !PFTS
    DO i=1,npft
      pftcoord(i) = REAL(i)
    ENDDO

    !GBM DIAGNOSTICS
    DO i=1,ndiags
      dcoord(i) = REAL(i)
    ENDDO

    !TILE DIAGNOSTICS
    DO i=1,ndiags_tile
      dtcoord(i) = REAL(i)
    ENDDO

    !########################################
    ! Create new netCDF restart file
    !########################################
    fullnom=TRIM(dirname)//TRIM(wfname)
    PRINT*,'LAND %% Writing precise restart file '//TRIM(fullnom)
    status = nf90_create(TRIM(fullnom), nf90_clobber, ncid)

    !########################################
    ! Define dimensions
    !########################################
    status = nf90_def_dim(ncid,'land_points',land_pts   ,lndcoordid)
    status = nf90_def_dim(ncid,'tile'       ,ntype      ,tilcoordid)
    status = nf90_def_dim(ncid,'pft'        ,npft       ,pftcoordid)
    status = nf90_def_dim(ncid,'diags'      ,ndiags     ,dcoordid)
    status = nf90_def_dim(ncid,'diags_tile' ,ndiags_tile,dtcoordid)

    !########################################
    ! Define variables
    !########################################
    status = nf90_def_var(ncid,'land_points',nf90_double,lndcoordid,lid)
    status = nf90_def_var(ncid,'tile'       ,nf90_double,tilcoordid,tid)
    status = nf90_def_var(ncid,'pft'        ,nf90_double,pftcoordid,pid)
    status = nf90_def_var(ncid,'diags'      ,nf90_double,dcoordid  ,did)
    status = nf90_def_var(ncid,'diags_tile' ,nf90_double,dtcoordid ,iid)

    status = nf90_def_var(ncid,'cs'        ,nf90_double,lndcoordid,csid)
    status = nf90_def_var(ncid,'lying_snow',nf90_double,lndcoordid,lying_snowid)
    status = nf90_def_var(ncid,'msoil'     ,nf90_double,lndcoordid,msoilid)
    status = nf90_def_var(ncid,'tsub1'     ,nf90_double,lndcoordid,tsub1id)
    status = nf90_def_var(ncid,'tstar_gb'  ,nf90_double,lndcoordid,tstar_gbid)
    status = nf90_def_var(ncid,'fx_le'     ,nf90_double,lndcoordid,fx_leid)
    status = nf90_def_var(ncid,'fx_sen'    ,nf90_double,lndcoordid,fx_senid)
    status = nf90_def_var(ncid,'z0_gb'     ,nf90_double,lndcoordid,z0_gbid)
    status = nf90_def_var(ncid,'albedo_gb' ,nf90_double,lndcoordid,albedo_gbid)
    status = nf90_def_var(ncid,'evap'      ,nf90_double,lndcoordid,evapid)
    status = nf90_def_var(ncid,'esub'      ,nf90_double,lndcoordid,esubid)
    status = nf90_def_var(ncid,'runoff'    ,nf90_double,lndcoordid,gravdrid)
    status = nf90_def_var(ncid,'snowmelt'  ,nf90_double,lndcoordid,snowmelt_accid)
    status = nf90_def_var(ncid,'resp_s_dr' ,nf90_double,lndcoordid,resp_s_drid)
    status = nf90_def_var(ncid,'igcm_swnet',nf90_double,lndcoordid,igcm_swnetid)
    status = nf90_def_var(ncid,'igcm_lwnet',nf90_double,lndcoordid,igcm_lwnetid)
    status = nf90_def_var(ncid,'igcm_prec' ,nf90_double,lndcoordid,igcm_precid)
    status = nf90_def_var(ncid,'igcm_z1'   ,nf90_double,lndcoordid,igcm_z1id)

    dimpass(1) = lndcoordid
    dimpass(2) = tilcoordid
    status = nf90_def_var(ncid,'frac' ,nf90_double,dimpass,fracid)
    status = nf90_def_var(ncid,'tstar',nf90_double,dimpass,tstarid)

    dimpass(1) = lndcoordid
    dimpass(2) = pftcoordid
    status = nf90_def_var(ncid,'lai'            ,nf90_double,dimpass,laiid)
    status = nf90_def_var(ncid,'ht'             ,nf90_double,dimpass,htid)
    status = nf90_def_var(ncid,'g_leaf_phen_acc',nf90_double,dimpass,g_leaf_phen_accid)
    status = nf90_def_var(ncid,'g_leaf_acc'     ,nf90_double,dimpass,g_leaf_accid)
    status = nf90_def_var(ncid,'gpp_dr'         ,nf90_double,dimpass,gpp_drid)
    status = nf90_def_var(ncid,'npp_dr'         ,nf90_double,dimpass,npp_drid)
    status = nf90_def_var(ncid,'resp_w_dr'      ,nf90_double,dimpass,resp_w_drid)

    dimpass(1) = lndcoordid
    dimpass(2) = dcoordid
    status = nf90_def_var(ncid,'acc_diags',nf90_double,dimpass,diag_varid)

    dimpass3(1) = lndcoordid
    dimpass3(2) = tilcoordid
    dimpass3(3) = dtcoordid
    status = nf90_def_var(ncid,'acc_diags_tile',nf90_double,dimpass3,diag_var_tileid)

    ! Add variables previously in cleanrstart file
    status = nf90_def_var(ncid,'nstep_land',nf90_int,nstepid)
    status = nf90_def_var(ncid,'init_doy_angle',nf90_double,doyid)

    !########################################
    ! Leave netCDF define mode
    !########################################
    status = nf90_enddef(ncid)

    !########################################
    ! Write data to netCDF files at genie.F
    ! precision
    !########################################
    status = nf90_put_var(ncid,lid,lndcoord)
    status = nf90_put_var(ncid,tid,tilcoord)
    status = nf90_put_var(ncid,pid,pftcoord)
    status = nf90_put_var(ncid,did,dcoord)
    status = nf90_put_var(ncid,iid,dtcoord)

    status = nf90_put_var(ncid,csid,cs)
    status = nf90_put_var(ncid,lying_snowid,lying_snow)
    status = nf90_put_var(ncid,msoilid,msoil)
    status = nf90_put_var(ncid,tsub1id,tsub1)
    status = nf90_put_var(ncid,tstar_gbid,tstar_gb)
    status = nf90_put_var(ncid,fx_leid,fx_le)
    status = nf90_put_var(ncid,fx_senid,fx_sen)
    status = nf90_put_var(ncid,z0_gbid,z0_gb)
    status = nf90_put_var(ncid,albedo_gbid,albedo_gb)
    status = nf90_put_var(ncid,evapid,evap)
    status = nf90_put_var(ncid,esubid,esub)
    status = nf90_put_var(ncid,gravdrid,gravdr)
    status = nf90_put_var(ncid,snowmelt_accid,snowmelt_acc)
    status = nf90_put_var(ncid,resp_s_drid,resp_s_dr)
    status = nf90_put_var(ncid,igcm_swnetid,igcm_swnet)
    status = nf90_put_var(ncid,igcm_lwnetid,igcm_lwnet)
    status = nf90_put_var(ncid,igcm_precid,igcm_prec)
    status = nf90_put_var(ncid,igcm_z1id,igcm_z1)

    status = nf90_put_var(ncid,fracid,frac)
    status = nf90_put_var(ncid,tstarid,tstar)
    status = nf90_put_var(ncid,diag_varid,diag_var)
    status = nf90_put_var(ncid,diag_var_tileid,diag_var_tile)

    status = nf90_put_var(ncid,laiid,lai)
    status = nf90_put_var(ncid,htid,ht)
    status = nf90_put_var(ncid,g_leaf_phen_accid,g_leaf_phen_acc)
    status = nf90_put_var(ncid,g_leaf_accid,g_leaf_acc)
    status = nf90_put_var(ncid,gpp_drid,gpp_dr)
    status = nf90_put_var(ncid,npp_drid,npp_dr)
    status = nf90_put_var(ncid,resp_w_drid,resp_w_dr)

    status = nf90_put_var(ncid,nstepid,nstep_land)
    status = nf90_put_var(ncid,doyid,init_doy_angle)

    !########################################
    ! Close netCDF restart file
    !########################################
    status = nf90_close(ncid)  

    RETURN
  END SUBROUTINE land_restart_wdata_precise


!#######################################################################################
! The following functions are synonyms for the function 
!   outstatus = land_nc_get(ncin,cname,retarr)
! that reads a single variable from an already open netCDF file.  Where retarr can be 
! of rank 0,1,2,3,4 with an interface provided in the header of this module.
!#######################################################################################
  INTEGER FUNCTION land_nc_get_0d(ncin,cname,retarri,retarrf,l_ignorevar) RESULT(outstatus)
    USE netcdf
    IMPLICIT NONE
    INTEGER,INTENT(in)              :: ncin   ! NetCDF handle
    CHARACTER(len=*),INTENT(in)     :: cname  ! Variable name
    REAL,OPTIONAL,INTENT(inout)     :: retarrf ! Variable values
    INTEGER,OPTIONAL,INTENT(inout)  :: retarri ! Variable values
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignorevar ! Exit subroutine rather than stop if variable not present

    INTEGER                         :: VarId     ! Work current variable id

    outstatus = -1

    !########################################
    ! Get handle for the current variable
    !########################################
    status = nf90_inq_varid(ncin,cname,VarId)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% Error finding '//TRIM(cname)//' in restart file.'
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      IF(PRESENT(l_ignorevar).and.l_ignorevar) THEN
        IF(PRESENT(retarri)) retarri = 0
        IF(PRESENT(retarrf)) retarrf = 0.0
        outstatus = status
        RETURN
      ELSE
        STOP
      ENDIF
    ENDIF

    !########################################
    ! Get the variable data values
    !########################################
    IF(PRESENT(retarri)) status = nf90_get_var(ncin,VarId,retarri)
    IF(PRESENT(retarrf)) status = nf90_get_var(ncin,VarId,retarrf)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% NetCDF restart read error when getting values of variable '//TRIM(cname)
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      STOP
    ENDIF

    outstatus = status
  END FUNCTION land_nc_get_0d


  INTEGER FUNCTION land_nc_get_1d(ncin,cname,retarr,l_ignorevar,l_ignoredim) RESULT(outstatus)
    USE netcdf
    IMPLICIT NONE
    INTEGER,INTENT(in)              :: ncin   ! NetCDF handle
    CHARACTER(len=*),INTENT(in)     :: cname  ! Variable name
    REAL,DIMENSION(:),INTENT(inout) :: retarr ! Variable values
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignorevar ! Exit subroutine rather than stop if variable not present
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignoredim ! Exit subroutine rather than stop if dimensions are wrong

    INTEGER,PARAMETER               :: nd = 1    ! No. of dimensions of retarr
    INTEGER                         :: VarId     ! Work current variable id
    INTEGER                         :: VarType   ! Work current variable data type
    INTEGER,DIMENSION(nd)           :: VarDimIds ! Word IDs of current variable dimensions
    INTEGER                         :: d, DimLen ! Work variables
    CHARACTER(len=40)               :: DimNom    ! Work dimension name

    outstatus = -1

    !########################################
    ! Get handle for the current variable
    !########################################
    status = nf90_inq_varid(ncin,cname,VarId)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% Error finding '//TRIM(cname)//' in restart file.'
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      IF(PRESENT(l_ignorevar).and.l_ignorevar) THEN
        retarr(:) = 0.0
        outstatus = status
        RETURN
      ELSE
        STOP
      ENDIF
    ENDIF

    !########################################
    ! Get information about current variable from netCDf file.
    ! Check the dimensions of the file variable match the inout array.
    !########################################
    status = nf90_Inquire_Variable(ncin,VarId,xtype=VarType,dimids=VarDimIds)
    DO d=1,nd
      status = nf90_Inquire_Dimension(ncin,VarDimIds(d),name=DimNom,len=DimLen)
      IF(status /= nf90_noerr) THEN
        PRINT*,'LAND %% NetCDF restart read error when getting dimensions of variable '//TRIM(cname)
        PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
        STOP
      ELSE IF(DimLen /= SIZE(retarr,d)) THEN
        PRINT*,'LAND %% NetCDF restart read error when getting variable '//TRIM(cname)
        PRINT*,'LAND %% Length of dimension',d,'('//TRIM(DimNom)//') in file does not match array.',DimLen,SIZE(retarr,d)
        IF(PRESENT(l_ignoredim).and.l_ignoredim) THEN
          retarr(:) = 0.0
          outstatus = status
          RETURN
        ELSE
          STOP
        ENDIF
      ENDIF
    ENDDO

    !########################################
    ! Get the variable data values
    !########################################
    status = nf90_get_var(ncin,VarId,retarr)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% NetCDF restart read error when getting values of variable '//TRIM(cname)
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      STOP
    ENDIF

    outstatus = status
  END FUNCTION land_nc_get_1d







  INTEGER FUNCTION land_nc_get_2d(ncin,cname,retarr,l_ignorevar,l_ignoredim) RESULT(outstatus)
    USE netcdf
    IMPLICIT NONE
    INTEGER,INTENT(in)              :: ncin   ! NetCDF handle
    CHARACTER(len=*),INTENT(in)     :: cname  ! Variable name
    REAL,DIMENSION(:,:),INTENT(inout) :: retarr ! Variable values
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignorevar ! Exit subroutine rather than stop if variable not present
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignoredim ! Exit subroutine rather than stop if dimensions are wrong

    INTEGER,PARAMETER               :: nd = 2    ! No. of dimensions of retarr
    INTEGER                         :: VarId     ! Work current variable id
    INTEGER                         :: VarType   ! Work current variable data type
    INTEGER,DIMENSION(nd)           :: VarDimIds ! Word IDs of current variable dimensions
    INTEGER                         :: d, DimLen ! Work variables
    CHARACTER(len=40)               :: DimNom    ! Work dimension name

    outstatus = -1

    !########################################
    ! Get handle for the current variable
    !########################################
    status = nf90_inq_varid(ncin,cname,VarId)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% Error finding '//TRIM(cname)//' in restart file.'
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      IF(PRESENT(l_ignorevar).and.l_ignorevar) THEN
        retarr(:,:) = 0.0
        outstatus   = status
        RETURN
      ELSE
        STOP
      ENDIF
    ENDIF

    !########################################
    ! Get information about current variable from netCDf file.
    ! Check the dimensions of the file variable match the inout array.
    !########################################
    status = nf90_Inquire_Variable(ncin,VarId,xtype=VarType,dimids=VarDimIds)
    DO d=1,nd
      status = nf90_Inquire_Dimension(ncin,VarDimIds(d),name=DimNom,len=DimLen)
      IF(status /= nf90_noerr) THEN
        PRINT*,'LAND %% NetCDF restart read error when getting dimensions of variable '//TRIM(cname)
        PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
        STOP
      ELSE IF(DimLen /= SIZE(retarr,d)) THEN
        PRINT*,'LAND %% NetCDF restart read error when getting variable '//TRIM(cname)
        PRINT*,'LAND %% Length of dimension',d,'('//TRIM(DimNom)//') in file does not match array.',DimLen,SIZE(retarr,d)
        IF(PRESENT(l_ignoredim).and.l_ignoredim) THEN
          retarr(:,:) = 0.0
          outstatus   = status
          RETURN
        ELSE
          STOP
        ENDIF
      ENDIF
    ENDDO

    !########################################
    ! Get the variable data values
    !########################################
    status = nf90_get_var(ncin,VarId,retarr)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% NetCDF restart read error when getting values of variable '//TRIM(cname)
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      STOP
    ENDIF

    outstatus = status
  END FUNCTION land_nc_get_2d





  INTEGER FUNCTION land_nc_get_3d(ncin,cname,retarr,l_ignorevar,l_ignoredim) RESULT(outstatus)
    USE netcdf
    IMPLICIT NONE
    INTEGER,INTENT(in)              :: ncin   ! NetCDF handle
    CHARACTER(len=*),INTENT(in)     :: cname  ! Variable name
    REAL,DIMENSION(:,:,:),INTENT(inout) :: retarr ! Variable values
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignorevar ! Exit subroutine rather than stop if variable not present
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignoredim ! Exit subroutine rather than stop if dimensions are wrong

    INTEGER,PARAMETER               :: nd = 3    ! No. of dimensions of retarr
    INTEGER                         :: VarId     ! Work current variable id
    INTEGER                         :: VarType   ! Work current variable data type
    INTEGER,DIMENSION(nd)           :: VarDimIds ! Word IDs of current variable dimensions
    INTEGER                         :: d, DimLen ! Work variables
    CHARACTER(len=40)               :: DimNom    ! Work dimension name

    outstatus = -1

    !########################################
    ! Get handle for the current variable
    !########################################
    status = nf90_inq_varid(ncin,cname,VarId)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% Error finding '//TRIM(cname)//' in restart file.'
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      IF(PRESENT(l_ignorevar).and.l_ignorevar) THEN
        retarr(:,:,:) = 0.0
        outstatus     = status
        RETURN
      ELSE
        STOP
      ENDIF
    ENDIF

    !########################################
    ! Get information about current variable from netCDf file.
    ! Check the dimensions of the file variable match the inout array.
    !########################################
    status = nf90_Inquire_Variable(ncin,VarId,xtype=VarType,dimids=VarDimIds)
    DO d=1,nd
      status = nf90_Inquire_Dimension(ncin,VarDimIds(d),name=DimNom,len=DimLen)
      IF(status /= nf90_noerr) THEN
        PRINT*,'LAND %% NetCDF restart read error when getting dimensions of variable '//TRIM(cname)
        PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
        STOP
      ELSE IF(DimLen /= SIZE(retarr,d)) THEN
        PRINT*,'LAND %% NetCDF restart read error when getting variable '//TRIM(cname)
        PRINT*,'LAND %% Length of dimension',d,'('//TRIM(DimNom)//') in file does not match array.',DimLen,SIZE(retarr,d)
        IF(PRESENT(l_ignoredim).and.l_ignoredim) THEN
          retarr(:,:,:) = 0.0
          outstatus     = status
          RETURN
        ELSE
          STOP
        ENDIF
      ENDIF
    ENDDO

    !########################################
    ! Get the variable data values
    !########################################
    status = nf90_get_var(ncin,VarId,retarr)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% NetCDF restart read error when getting values of variable '//TRIM(cname)
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      STOP
    ENDIF

    outstatus = status
  END FUNCTION land_nc_get_3d





  INTEGER FUNCTION land_nc_get_4d(ncin,cname,retarr,l_ignorevar,l_ignoredim) RESULT(outstatus)
    USE netcdf
    IMPLICIT NONE
    INTEGER,INTENT(in)              :: ncin   ! NetCDF handle
    CHARACTER(len=*),INTENT(in)     :: cname  ! Variable name
    REAL,DIMENSION(:,:,:,:),INTENT(inout) :: retarr ! Variable values
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignorevar ! Exit subroutine rather than stop if variable not present
    LOGICAL,OPTIONAL,INTENT(in)     :: l_ignoredim ! Exit subroutine rather than stop if dimensions are wrong

    INTEGER,PARAMETER               :: nd = 4    ! No. of dimensions of retarr
    INTEGER                         :: VarId     ! Work current variable id
    INTEGER                         :: VarType   ! Work current variable data type
    INTEGER,DIMENSION(nd)           :: VarDimIds ! Word IDs of current variable dimensions
    INTEGER                         :: d, DimLen ! Work variables
    CHARACTER(len=40)               :: DimNom    ! Work dimension name

    outstatus = -1

    !########################################
    ! Get handle for the current variable
    !########################################
    status = nf90_inq_varid(ncin,cname,VarId)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% Error finding '//TRIM(cname)//' in restart file.'
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      IF(PRESENT(l_ignorevar).and.l_ignorevar) THEN
        retarr(:,:,:,:) = 0.0
        outstatus       = status
        RETURN
      ELSE
        STOP
      ENDIF
    ENDIF

    !########################################
    ! Get information about current variable from netCDf file.
    ! Check the dimensions of the file variable match the inout array.
    !########################################
    status = nf90_Inquire_Variable(ncin,VarId,xtype=VarType,dimids=VarDimIds)
    DO d=1,nd
      status = nf90_Inquire_Dimension(ncin,VarDimIds(d),name=DimNom,len=DimLen)
      IF(status /= nf90_noerr) THEN
        PRINT*,'LAND %% NetCDF restart read error when getting dimensions of variable '//TRIM(cname)
        PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
        STOP
      ELSE IF(DimLen /= SIZE(retarr,d)) THEN
        PRINT*,'LAND %% NetCDF restart read error when getting variable '//TRIM(cname)
        PRINT*,'LAND %% Length of dimension',d,'('//TRIM(DimNom)//') in file does not match array.',DimLen,SIZE(retarr,d)
        IF(PRESENT(l_ignoredim).and.l_ignoredim) THEN
          retarr(:,:,:,:) = 0.0
          outstatus       = status
          RETURN
        ELSE
          STOP
        ENDIF
      ENDIF
    ENDDO

    !########################################
    ! Get the variable data values
    !########################################
    status = nf90_get_var(ncin,VarId,retarr)
    IF(status /= nf90_noerr) THEN
      PRINT*,'LAND %% NetCDF restart read error when getting values of variable '//TRIM(cname)
      PRINT*,'LAND %% '//TRIM(NF90_STRERROR(status))
      STOP
    ENDIF

    outstatus = status
  END FUNCTION land_nc_get_4d


END MODULE land_restart
