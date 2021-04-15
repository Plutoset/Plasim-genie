 
!----------------------------------------------------------------------
!>
!> Module: local_input
!>
!> Data input routines: The routines are generally stateless (access via
!> filename of the input file. These routines use the NetCDF routines
!> provided by the module "local_netcdf".
!> 
!----------------------------------------------------------------------
module local_input

  use genie_util, only: message,die

  use local_netcdf

  PRIVATE

  PUBLIC :: readReal1dVariable,readReal2dVariable,readReal3dVariable,&
    &  readReal3dRecordVariable,getInfoReal2dVariable, &
    &  getInfoReal3dVariable,getInfoReal3dRecordVariable

contains

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: openInput
  !>
  !> opens input file, returns handle to output file
  !> 
  !----------------------------------------------------------------------
  subroutine openInput(filename,ID)

    character(len=*),intent(in) :: filename

    integer :: ID

    call message("Opening input file in read-only mode!",3)
    call openNetCDFRead(filename,ID)

  end subroutine openInput

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: closeInOutput
  !>
  !> closes input/output file
  !> 
  !----------------------------------------------------------------------
  subroutine closeInOutput(ID)

    integer :: ID

    call closeNetCDF(ID)

  end subroutine closeInOutput

  !----------------------------------------------------------------------
  !>
  !> Subroutine: getInfoReal2dVariable
  !>
  !> reads and returns information (dimension names, long name,
  !> standard name, units, missing value) about Variable
  !> 
  !----------------------------------------------------------------------
  subroutine getInfoReal2dVariable(filename,varName,varDimNames,&
      & varDimLens,varLongName,varStandardName,varUnits,varMissingValue, &
      & varNormalise)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(out),dimension(2) :: varDimNames
    integer,intent(out),dimension(2) :: varDimLens

    character(len=*),intent(out),optional :: varLongname,varStandardname,varUnits

    real,intent(out),optional :: varMissingValue

    logical,intent(in),optional :: varNormalise

    type(real2dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    call openInput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = varName

    call lookupVars(ID,varNetCDF)

    if (varNetCDF(1)%id < 0) then

       call die("Variable not found!")

    else

       allocate(varNetCDF(1)%data(varNetCDF(1)%dimLens(1),varNetCDF(1)%dimLens(2)),stat=status)
       if (status /= 0) then
          call die("Could not allocate storage")
       endif

       call readVars(ID,varNetCDF)

       if (present(varNormalise).and.(varNormalise.eqv..true.)) then
! normalise variable
          call normaliseReal2dVariable(ID,varNetCDF)
       endif

       varDimNames(1)=varNetCDF(1)%dimNames(1)
       varDimLens(1)=varNetCDF(1)%dimLens(1)
       varDimNames(2)=varNetCDF(1)%dimNames(2)
       varDimLens(2)=varNetCDF(1)%dimLens(2)
       if (present(varLongName)) varLongName=varNetCDF(1)%basicAtts%long_name
       if (present(varStandardName)) varStandardName=varNetCDF(1)%basicAtts%standard_name
       if (present(varUnits)) varUnits=varNetCDF(1)%basicAtts%units
       if (present(varMissingValue)) varMissingValue=varNetCDF(1)%basicAtts%missing_value

       deallocate(varNetCDF(1)%data,stat=status)
       if (status /= 0) then
          call die("Could not deallocate storage")
       endif

    endif

    call closeInOutput(ID)

    deallocate(varNetCDF,stat=status)
    if (status /= 0) then
       call die("Could not deallocate storage")
    endif

  end subroutine getInfoReal2dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: getInfoReal3dVariable
  !>
  !> reads and returns information (dimension names, long name,
  !> standard name, units, missing value) about Variable
  !> 
  !----------------------------------------------------------------------
  subroutine getInfoReal3dVariable(filename,varName,varDimNames, &
    &  varDimLens,varLongName,varStandardName,varUnits,varMissingValue, &
    &  varNormalise)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(out),dimension(3) :: varDimNames
    integer,intent(out),dimension(3) :: varDimLens

    character(len=*),intent(out),optional :: varLongname,varStandardname,varUnits

    real,intent(out),optional :: varMissingValue

    logical,intent(in),optional :: varNormalise

    type(real3dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    call openInput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif

    varNetCDF(1)%name = varName

    call lookupVars(ID,varNetCDF)

    if (varNetCDF(1)%id < 0) then

       call die("Variable not found!")

    else

       allocate(varNetCDF(1)%data(varNetCDF(1)%dimLens(1),varNetCDF(1)%dimLens(2),varNetCDF(1)%dimLens(3)),stat=status)
       if (status /= 0) then
          call die("Could not allocate storage")
       endif

       call readVars(ID,varNetCDF)

       if (present(varNormalise).and.(varNormalise.eqv..true.)) then
! normalise variable
          call normaliseReal3dVariable(ID,varNetCDF)
       endif

       varDimNames(1)=varNetCDF(1)%dimNames(1)
       varDimLens(1)=varNetCDF(1)%dimLens(1)
       varDimNames(2)=varNetCDF(1)%dimNames(2)
       varDimLens(2)=varNetCDF(1)%dimLens(2)
       varDimNames(3)=varNetCDF(1)%dimNames(3)
       varDimLens(3)=varNetCDF(1)%dimLens(3)
       if (present(varLongName)) varLongName=varNetCDF(1)%basicAtts%long_name
       if (present(varStandardName)) varStandardName=varNetCDF(1)%basicAtts%standard_name
       if (present(varUnits)) varUnits=varNetCDF(1)%basicAtts%units
       if (present(varMissingValue)) varMissingValue=varNetCDF(1)%basicAtts%missing_value

       deallocate(varNetCDF(1)%data,stat=status)
       if (status /= 0) then
          call die("Could not deallocate storage")
       endif

    endif

    call closeInOutput(ID)

    deallocate(varNetCDF,stat=status)
    if (status /= 0) then
       call die("Could not deallocate storage")
    endif

  end subroutine getInfoReal3dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: getInfoReal3dRecordVariable
  !>
  !> reads and returns information (dimension names, long name,
  !> standard name, units, missing value) about Variable
  !> 
  !----------------------------------------------------------------------
  subroutine getInfoReal3dRecordVariable(filename,varName,varDimNames, &
     &  varDimLens,varRecordDimName,varRecordDimLen,varLongName, &
     &  varStandardName,varUnits,varMissingValue,varNormalise)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(out),dimension(3) :: varDimNames
    integer,intent(out),dimension(3) :: varDimLens

    character(len=*),intent(out) :: varRecordDimName
    integer,intent(out) :: varRecordDimLen

    character(len=*),intent(out),optional :: varLongname,varStandardname,varUnits

    real,intent(out),optional :: varMissingValue

    logical,intent(in),optional :: varNormalise

    type(real3dRecordVar),pointer,dimension(:) :: varNetCDF

    integer,dimension(3) :: nonrecdim
    integer :: ID,status,recdimindex

    call openInput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif

    varNetCDF(1)%name = varName

    call lookupVars(ID,varNetCDF)

    recdimindex = varNetCDF(1)%recordDimIndex
    varRecordDimName = varNetCDF(1)%DimNames(recdimindex)
    varRecordDimLen = varNetCDF(1)%DimLens(recdimindex)
    select case (recdimindex)
    case (0)
       call die("Variable not defined along a record dimension!")
    case (1)
       nonrecdim=(/2,3,4/)
    case (2)
       nonrecdim=(/1,2,4/)
    case (3)
       nonrecdim=(/1,2,4/)
    case (4)
       nonrecdim=(/1,2,3/)
    end select

    if (varNetCDF(1)%id < 0) then

       call die("Variable not found!")

    else

       allocate(varNetCDF(1)%data(varNetCDF(1)%dimLens(nonrecdim(1)),varNetCDF(1)%dimLens(nonrecdim(2)), &
            & varNetCDF(1)%dimLens(nonrecdim(3))),stat=status)
       if (status /= 0) call die("Could not allocate storage")

       ! read last recorded slice of data
       call readVars(ID,varNetCDF,varNetCDF(1)%dimLens(recdimindex))

       if (present(varNormalise).and.(varNormalise.eqv..true.)) then
! normalise variable
          call normaliseReal3dRecordVariable(ID,varNetCDF)
       endif

       varDimNames(1)=varNetCDF(1)%dimNames(1)
       varDimLens(1)=varNetCDF(1)%dimLens(1)
       varDimNames(2)=varNetCDF(1)%dimNames(2)
       varDimLens(2)=varNetCDF(1)%dimLens(2)
       varDimNames(3)=varNetCDF(1)%dimNames(3)
       varDimLens(3)=varNetCDF(1)%dimLens(3)
       if (present(varLongName)) varLongName=varNetCDF(1)%basicAtts%long_name
       if (present(varStandardName)) varStandardName=varNetCDF(1)%basicAtts%standard_name
       if (present(varUnits)) varUnits=varNetCDF(1)%basicAtts%units
       if (present(varMissingValue)) varMissingValue=varNetCDF(1)%basicAtts%missing_value

       deallocate(varNetCDF(1)%data,stat=status)
       if (status /= 0) then
          call die("Could not deallocate storage")
       endif

    endif

    call closeInOutput(ID)

    deallocate(varNetCDF,stat=status)
    if (status /= 0) then
       call die("Could not deallocate storage")
    endif

  end subroutine getInfoReal3dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: readReal1dVariable
  !>
  !> reads Variable
  !> 
  !----------------------------------------------------------------------
  subroutine readReal1dVariable(filename,varName,varValues,varNormalise)

    character(len=*),intent(in) :: filename,varName

    real,dimension(:),intent(out) :: varValues

    logical,intent(in),optional :: varNormalise

    type(real1dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    call openInput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) call die("Could not allocate storage")

    varNetCDF(1)%name=varName

    call lookupVars(ID,varNetCDF)

    if (varNetCDF(1)%id < 0) then

       call die("Variable not found!")

    else

       allocate(varNetCDF(1)%data(varNetCDF(1)%dimLens(1)),stat=status)
       if (status /= 0) then
          call die("Could not allocate storage")
       endif

       call readVars(ID,varNetCDF)

       if (present(varNormalise).and.(varNormalise.eqv..true.)) then
! normalise variable
          call normaliseReal1dVariable(ID,varNetCDF)
       endif

       call closeInOutput(ID)

       varValues(:) = varNetCDF(1)%data

       deallocate(varNetCDF(1)%data,stat=status)
       if (status /= 0) then
          call die("Could not deallocate storage")
       endif

    endif

    deallocate(varNetCDF,stat=status)
    if (status /= 0) then
       call die("Could not deallocate storage")
    endif

  end subroutine readReal1dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: readReal2dVariable
  !>
  !> reads Variable
  !> 
  !----------------------------------------------------------------------
  subroutine readReal2dVariable(filename,varName,varValues,varNormalise)

    character(len=*),intent(in) :: filename,varName

    real,dimension(:,:),intent(out) :: varValues

    logical,intent(in),optional :: varNormalise

    type(real2dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    call openInput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) call die("Could not allocate storage")

    varNetCDF(1)%name=varName

    call lookupVars(ID,varNetCDF)

    if (varNetCDF(1)%id < 0) then

       call die("Variable not found!")

    else

       allocate(varNetCDF(1)%data(varNetCDF(1)%dimLens(1),varNetCDF(1)%dimLens(2)),stat=status)
       if (status /= 0) then
          call die("Could not allocate storage")
       endif

       call readVars(ID,varNetCDF)

       if (present(varNormalise).and.(varNormalise.eqv..true.)) then
! normalise variable
          call normaliseReal2dVariable(ID,varNetCDF)
       endif

       call closeInOutput(ID)

       varValues(:,:) = varNetCDF(1)%data

       deallocate(varNetCDF(1)%data,stat=status)
       if (status /= 0) then
          call die("Could not deallocate storage")
       endif

    endif

    deallocate(varNetCDF,stat=status)
    if (status /= 0) then
       call die("Could not deallocate storage")
    endif

  end subroutine readReal2dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: readReal3dVariable
  !>
  !> reads Variable
  !> 
  !----------------------------------------------------------------------
  subroutine readReal3dVariable(filename,varName,varValues,varNormalise)

    character(len=*),intent(in) :: filename,varName

    real,dimension(:,:,:),intent(out) :: varValues

    logical,intent(in),optional :: varNormalise

    type(real3dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    call openInput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) call die("Could not allocate storage")

    varNetCDF(1)%name=varName

    call lookupVars(ID,varNetCDF)

    if (varNetCDF(1)%id < 0) then

       call die("Variable not found!")

    else

       allocate(varNetCDF(1)%data(varNetCDF(1)%dimLens(1),varNetCDF(1)%dimLens(2),varNetCDF(1)%dimLens(3)),stat=status)
       if (status /= 0) then
          call die("Could not allocate storage")
       endif

       call readVars(ID,varNetCDF)

       if (present(varNormalise).and.(varNormalise.eqv..true.)) then
! normalise variable
          call normaliseReal3dVariable(ID,varNetCDF)
       endif

       call closeInOutput(ID)

       varValues(:,:,:) = varNetCDF(1)%data

       deallocate(varNetCDF(1)%data,stat=status)
       if (status /= 0) then
          call die("Could not deallocate storage")
       endif

    endif

    deallocate(varNetCDF,stat=status)
    if (status /= 0) then
       call die("Could not deallocate storage")
    endif

  end subroutine readReal3dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: readReal3dRecordVariable
  !>
  !> reads Variable
  !> 
  !----------------------------------------------------------------------
  subroutine readReal3dRecordVariable(filename,varName,varRecIndex,varValues,varNormalise)

    character(len=*),intent(in) :: filename,varName

    integer,intent(in) :: varRecIndex

    real,dimension(:,:,:),intent(out) :: varValues

    logical,intent(in),optional :: varNormalise

    type(real3dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status,varRecordDimIndex

    call openInput(filename,ID)

    allocate(varNetCDF(1),stat=status)
    if (status /= 0) call die("Could not allocate storage")

    varNetCDF(1)%name=varName

    call lookupVars(ID,varNetCDF)

    varRecordDimIndex = varNetCDF(1)%RecordDimIndex

    if (varRecIndex .gt. varNetCDF(1)%dimLens(varRecordDimIndex)) then
       call die("Record index not available!")
    endif

    if (varNetCDF(1)%id < 0) then

       call die("Variable not found!")

    else

       allocate(varNetCDF(1)%data(varNetCDF(1)%dimLens(1),varNetCDF(1)%dimLens(2),varNetCDF(1)%dimLens(3)),stat=status)
       if (status /= 0) then
          call die("Could not allocate storage")
       endif

       call readVars(ID,varNetCDF,varNetCDF(1)%dimLens(varRecordDimIndex))

       if (present(varNormalise).and.(varNormalise.eqv..true.)) then
! normalise variable
          call normaliseReal3dRecordVariable(ID,varNetCDF)
       endif

       call closeInOutput(ID)

       varValues(:,:,:) = varNetCDF(1)%data

       deallocate(varNetCDF(1)%data,stat=status)
       if (status /= 0) then
          call die("Could not deallocate storage")
       endif

    endif

    deallocate(varNetCDF,stat=status)
    if (status /= 0) then
       call die("Could not deallocate storage")
    endif

  end subroutine readReal3dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: normaliseReal1dVariable
  !>
  !> normalises variable and axes defined on a subset of longitude,
  !> latitude, depth/z axes:
  !>  i) Rearrange sequence of available axes according to sequence
  !>  longitude, latitude, depth/z, other axes
  !>  ii) test if longitude, latitdue strictly increasing monotonic,
  !>  flip if necessary, stop if not strictly monotonic
  !>  iii) rotate dataset along longitudinal direction to occupy range
  !>  [0,360] degrees east
  !>  iv) test if depth/z strictly increasing, flip if necessary,
  !>  stop if not strictly monotonic
  !>
  !----------------------------------------------------------------------
  subroutine normaliseReal1dVariable(ID,varNetCDF)

    type(real1dVar),pointer,dimension(:) :: varNetCDF
    type(real1dVar),pointer :: varNetCDF2

    type(real1dVar),pointer,dimension(:) :: axes

    integer :: ID, status, n, i, ilon, ilat, iz, ishift
    integer,dimension(1) :: ii
    real :: londiff

    logical :: reverse

    ilon = -1
    ilat = -1
    iz = -1
    reverse = .false.
    i = 1
    select case (trim(varNetCDF(1)%dimNames(1)))
    case ('longitude','Longitude','LONGITUDE','lon','Lon','LON')
       ilon = 1
       i=i+1
    end select
    select case (trim(varNetCDF(1)%dimNames(1)))
    case ('latitude','Latitude','LATITUDE','lat','Lat','LAT')
       ilat = 1
       i=i+1
    end select
    select case (trim(varNetCDF(1)%dimNames(1)))
    case ('depth','Depth','DEPTH','z','Z','zt')
       iz = 1
       i=i+1
    end select

! read axes
    allocate(axes(1),stat=status)
    if (status /= 0) call die("Could not allocate storage")
    axes(1)%name=varNetCDF(1)%dimNames(1)
    call lookupVars(ID,axes)
    if (axes(1)%id < 0) then
       call die("Axis not found!")
    else
       allocate(axes(1)%data(varNetCDF(1)%dimLens(1)),stat=status)
       if (status /= 0) call die("Could not allocate storage")
    endif
    call readVars(ID,axes)

! test if reversal of direction of axes is required
    if ((ilon.ge.1).or.(ilat.ge.1).or.(iz.ge.1)) then
       if (axes(1)%data(1).gt.axes(1)%data(2)) then
          reverse=.true.
       endif
    endif
! verify if axes are strictly monotonic
    if ((ilon.ge.1).or.(ilat.ge.1).or.(iz.ge.1)) then
       if (reverse) then
          do n=2,axes(1)%dimLens(1)
             if (axes(1)%data(n).ge.axes(1)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(1)%dimLens(1)
             if (axes(1)%data(n).le.axes(1)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif

! compute offset required to shift longitudinal extent into range
! [0,360] degrees east by searching index with then smallest distance
! of its longitude modulo 360 from 0)
    ishift = 1
    londiff = 9e19
    if (ilon.ge.1) then
       do n=1,varNetCDF(1)%dimLens(ilon)
          if (modulo(axes(ilon)%data(n),360.0).lt.londiff) then
             ishift = n
             londiff = modulo(axes(ilon)%data(n),360.0)
          endif
       enddo
    endif
    
! apply transformation
    allocate(varNetCDF2,stat=status)
    if (status /= 0) call die("Could not allocate storage")
    varNetCDF2%name = varNetCDF(1)%name
    varNetCDF2%dimIDs(1) = varNetCDF(1)%dimIDs(1)
    varNetCDF2%dimLens(1) = varNetCDF(1)%dimLens(1)
    varNetCDF2%dimNames(1) = varNetCDF(1)%dimNames(1)
    varNetCDF2%nAtts = varNetCDF(1)%nAtts
    varNetCDF2%id = varNetCDF(1)%id
    varNetCDF2%basicAtts = varNetCDF(1)%basicAtts
    allocate(varNetCDF2%data(varNetCDF2%dimLens(1)),stat=status)
    if (status /= 0) call die("Could not allocate storage")
    do i=1,varNetCDF2%dimLens(1)
       ii(1) = i
       if (ilon.ge.1) then
          ii(ilon) = modulo(ii(ilon)+ishift-2,varNetCDF(1)%dimLens(ilon))+1
       endif
       if (reverse) ii(1) = varNetCDF2%dimLens(1)+1-ii(1)
       varNetCDF2%data(i) = varNetCDF(1)%data(ii(1))
    enddo
    deallocate(varNetCDF(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate storage")
    varNetCDF(1) = varNetCDF2
    deallocate(varNetCDF2,stat=status)
    if (status /= 0) call die("Could not deallocate storage")

! deallocate memeory
    deallocate(axes(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate storage")
    deallocate(axes,stat=status)
    if (status /= 0) call die("Could not deallocate storage")

  end subroutine normaliseReal1dVariable

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: normaliseReal2dVariable
  !>
  !> normalises variable and axes defined on a subset of longitude,
  !> latitude, depth/z axes:
  !>  i) Rearrange sequence of available axes according to sequence
  !>  longitude, latitude, depth/z, other axes
  !>  ii) test if longitude, latitdue strictly increasing monotonic,
  !>  flip if necessary, stop if not strictly monotonic
  !>  iii) rotate dataset along longitudinal direction to occupy range
  !>  [0,360] degrees east
  !>  iv) test if depth/z strictly increasing, flip if necessary,
  !>  stop if not strictly monotonic
  !>
  !----------------------------------------------------------------------
  subroutine normaliseReal2dVariable(ID,varNetCDF)

    type(real2dVar),pointer,dimension(:) :: varNetCDF
    type(real2dVar),pointer :: varNetCDF2

    type(real1dVar),pointer,dimension(:) :: axes

    integer :: ID, status, n, i, j, ilon, ilat, iz, ishift
    integer,dimension(2) :: ii
    real :: londiff

    integer,dimension(2) :: reindex
    logical,dimension(2) :: reverse

! construct 'reindex' array which describes permutations of dimensions
    ilon = -1
    ilat = -1
    iz = -1
    do n=1,2
       reindex(n) = -1
       reverse(n) = .false.
    enddo
    i = 1
    do n=1,2
       select case (trim(varNetCDF(1)%dimNames(n)))
       case ('longitude','Longitude','LONGITUDE','lon','Lon','LON')
          reindex(i) = n
          ilon = n
          i=i+1
       end select
    enddo
    do n=1,2
       select case (trim(varNetCDF(1)%dimNames(n)))
       case ('latitude','Latitude','LATITUDE','lat','Lat','LAT')
          reindex(i) = n
          ilat = n
          i=i+1
       end select
    enddo
    do n=1,2
       select case (trim(varNetCDF(1)%dimNames(n)))
       case ('depth','Depth','DEPTH','z','Z')
          reindex(i) = n
          iz = n
          i=i+1
       end select
    enddo
    do n=1,2
       if (reindex(n).eq.-1) then
          reindex(i) = n
          i=i+1
       endif
    enddo

! read axes
    allocate(axes(2),stat=status)
    if (status /= 0) call die("Could not allocate storage")
    do n=1,2
       axes(n)%name=varNetCDF(1)%dimNames(n)
    enddo
    call lookupVars(ID,axes)
    do n=1,2
       if (axes(n)%id < 0) then
          call die("Axis not found!")
       else
          allocate(axes(n)%data(varNetCDF(1)%dimLens(n)),stat=status)
          if (status /= 0) call die("Could not allocate storage")
       endif
    enddo
    call readVars(ID,axes)

! test if reversal of direction of axes is required
    if (ilon.ge.1) then
       if (axes(ilon)%data(1).gt.axes(ilon)%data(2)) then
          reverse(ilon)=.true.
       endif
    endif
    if (ilat.ge.1) then
       if (axes(ilat)%data(1).gt.axes(ilat)%data(2)) then
          reverse(ilat)=.true.
       endif
    endif
    if (iz.ge.1) then
       if (axes(iz)%data(1).gt.axes(iz)%data(2)) then
          reverse(iz)=.true.
       endif
    endif
! verify if axes are strictly monotonic
    if (ilon.ge.1) then
       if (reverse(ilon)) then
          do n=2,axes(ilon)%dimLens(1)
             if (axes(ilon)%data(n).ge.axes(ilon)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(ilon)%dimLens(1)
             if (axes(ilon)%data(n).le.axes(ilon)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif
    if (ilat.ge.1) then
       if (reverse(ilat)) then
          do n=2,axes(ilat)%dimLens(1)
             if (axes(ilat)%data(n).ge.axes(ilat)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(ilat)%dimLens(1)
             if (axes(ilat)%data(n).le.axes(ilat)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif
    if (iz.ge.1) then
       if (reverse(iz)) then
          do n=2,axes(iz)%dimLens(1)
             if (axes(iz)%data(n).ge.axes(iz)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(iz)%dimLens(1)
             if (axes(iz)%data(n).le.axes(iz)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif

! compute offset required to shift longitudinal extent into continuous range
! [0,360] degrees east by searching index with then smallest distance
! of its longitude modulo 360 from 0)
    ishift = 1
    londiff = 9e19
    if (ilon.ge.1) then
       do n=1,varNetCDF(1)%dimLens(ilon)
          if (modulo(axes(ilon)%data(n),360.0).lt.londiff) then
             ishift = n
             londiff = modulo(axes(ilon)%data(n),360.0)
          endif
       enddo
    endif

! apply transformation
    allocate(varNetCDF2,stat=status)
    if (status /= 0) call die("Could not allocate storage")
    varNetCDF2%name = varNetCDF(1)%name
    do n=1,2
       varNetCDF2%dimIDs(n) = varNetCDF(1)%dimIDs(reindex(n))
       varNetCDF2%dimLens(n) = varNetCDF(1)%dimLens(reindex(n))
       varNetCDF2%dimNames(n) = varNetCDF(1)%dimNames(reindex(n))
    enddo
    varNetCDF2%nAtts = varNetCDF(1)%nAtts
    varNetCDF2%id = varNetCDF(1)%id
    varNetCDF2%basicAtts = varNetCDF(1)%basicAtts
    allocate(varNetCDF2%data(varNetCDF2%dimLens(1),varNetCDF2%dimLens(2)),stat=status)
    if (status /= 0) call die("Could not allocate storage")
    do j=1,varNetCDF2%dimLens(2)
       do i=1,varNetCDF2%dimLens(1)
          ii(reindex(1)) = i
          ii(reindex(2)) = j
          if (ilon.ge.1) then
             ii(ilon) = modulo(ii(ilon)+ishift-2,varNetCDF(1)%dimLens(ilon))+1
          endif
          if (reverse(reindex(1))) ii(reindex(1)) = varNetCDF2%dimLens(1)+1-ii(reindex(1))
          if (reverse(reindex(2))) ii(reindex(2)) = varNetCDF2%dimLens(1)+1-ii(reindex(2))
          varNetCDF2%data(i,j) = varNetCDF(1)%data(ii(1),ii(2))
       enddo
    enddo
    deallocate(varNetCDF(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate storage")
    varNetCDF(1) = varNetCDF2
    deallocate(varNetCDF2,stat=status)
    if (status /= 0) call die("Could not deallocate storage")

! deallocate memeory
    do n=1,2
       deallocate(axes(n)%data,stat=status)
       if (status /= 0) call die("Could not deallocate storage")
    enddo
    deallocate(axes,stat=status)
    if (status /= 0) call die("Could not deallocate storage")

  end subroutine normaliseReal2dVariable

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: normaliseReal3dVariable
  !>
  !> normalises variable and axes defined on a subset of longitude,
  !> latitude, depth/z axes:
  !>  i) Rearrange sequence of available axes according to sequence
  !>  longitude, latitude, depth/z, other axes
  !>  ii) test if longitude, latitdue strictly increasing monotonic,
  !>  flip if necessary, stop if not strictly monotonic
  !>  iii) rotate dataset along longitudinal direction to occupy range
  !>  [0,360] degrees east
  !>  iv) test if depth/z strictly increasing, flip if necessary,
  !>  stop if not strictly monotonic
  !>
  !----------------------------------------------------------------------
  subroutine normaliseReal3dVariable(ID,varNetCDF)

    type(real3dVar),pointer,dimension(:) :: varNetCDF
    type(real3dVar),pointer :: varNetCDF2

    type(real1dVar),pointer,dimension(:) :: axes

    integer :: ID, status, n, i, j, k, ilon, ilat, iz, ishift
    integer,dimension(3) :: ii
    real :: londiff

    integer,dimension(3) :: reindex
    logical,dimension(3) :: reverse

! construct 'reindex' array which describes permutations of dimensions
    ilon = -1
    ilat = -1
    iz = -1
    do n=1,3
       reindex(n) = -1
       reverse(n) = .false.
    enddo
    i = 1
    do n=1,3
       select case (trim(varNetCDF(1)%dimNames(n)))
       case ('longitude','Longitude','LONGITUDE','lon','Lon','LON')
          reindex(i) = n
          ilon = n
          i=i+1
       end select
    enddo
    do n=1,3
       select case (trim(varNetCDF(1)%dimNames(n)))
       case ('latitude','Latitude','LATITUDE','lat','Lat','LAT')
          reindex(i) = n
          ilat = n
          i=i+1
       end select
    enddo
    do n=1,3
       select case (trim(varNetCDF(1)%dimNames(n)))
       case ('depth','Depth','DEPTH','z','Z')
          reindex(i) = n
          iz = n
          i=i+1
       end select
    enddo
    do n=1,3
       if (reindex(n).eq.-1) then
          reindex(i) = n
          i=i+1
       endif
    enddo
! read axes
    allocate(axes(3),stat=status)
    if (status /= 0) call die("Could not allocate storage")
    do n=1,3
       axes(n)%name=varNetCDF(1)%dimNames(n)
    enddo
    call lookupVars(ID,axes)
    do n=1,3
       if (axes(n)%id < 0) then
          call die("Axis not found!")
       else
          allocate(axes(n)%data(varNetCDF(1)%dimLens(n)),stat=status)
          if (status /= 0) call die("Could not allocate storage")
       endif
    enddo
    call readVars(ID,axes)

! test if reversal of direction of axes is required
    if (ilon.ge.1) then
       if (axes(ilon)%data(1).gt.axes(ilon)%data(2)) then
          reverse(ilon)=.true.
       endif
    endif
    if (ilat.ge.1) then
       if (axes(ilat)%data(1).gt.axes(ilat)%data(2)) then
          reverse(ilat)=.true.
       endif
    endif
    if (iz.ge.1) then
       if (axes(iz)%data(1).gt.axes(iz)%data(2)) then
          reverse(iz)=.true.
       endif
    endif

! verify if axes are strictly monotonic
    if (ilon.ge.1) then
       if (reverse(ilon)) then
          do n=2,axes(ilon)%dimLens(1)
             if (axes(ilon)%data(n).ge.axes(ilon)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(ilon)%dimLens(1)
             if (axes(ilon)%data(n).le.axes(ilon)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif
    if (ilat.ge.1) then
       if (reverse(ilat)) then
          do n=2,axes(ilat)%dimLens(1)
             if (axes(ilat)%data(n).ge.axes(ilat)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(ilat)%dimLens(1)
             if (axes(ilat)%data(n).le.axes(ilat)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif
    if (iz.ge.1) then
       if (reverse(iz)) then
          do n=2,axes(iz)%dimLens(1)
             if (axes(iz)%data(n).ge.axes(iz)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(iz)%dimLens(1)
             if (axes(iz)%data(n).le.axes(iz)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif

! compute offset required to shift longitudinal extent into continuous range
! [0,360] degrees east by searching index with then smallest distance
! of its longitude modulo 360 from 0)
    ishift = 1
    londiff = 9e19
    if (ilon.ge.1) then
       do n=1,varNetCDF(1)%dimLens(ilon)
          if (modulo(axes(ilon)%data(n),360.0).lt.londiff) then
             ishift = n
             londiff = modulo(axes(ilon)%data(n),360.0)
          endif
       enddo
    endif

! apply transformation
    allocate(varNetCDF2,stat=status)
    if (status /= 0) call die("Could not allocate storage")
    varNetCDF2%name = varNetCDF(1)%name
    do n=1,3
       varNetCDF2%dimIDs(n) = varNetCDF(1)%dimIDs(reindex(n))
       varNetCDF2%dimLens(n) = varNetCDF(1)%dimLens(reindex(n))
       varNetCDF2%dimNames(n) = varNetCDF(1)%dimNames(reindex(n))
    enddo
    varNetCDF2%nAtts = varNetCDF(1)%nAtts
    varNetCDF2%id = varNetCDF(1)%id
    varNetCDF2%basicAtts = varNetCDF(1)%basicAtts
    allocate(varNetCDF2%data(varNetCDF2%dimLens(1),varNetCDF2%dimLens(2),varNetCDF2%dimLens(3)),stat=status)
    if (status /= 0) call die("Could not allocate storage")
    do k=1,varNetCDF2%dimLens(3)
       do j=1,varNetCDF2%dimLens(2)
          do i=1,varNetCDF2%dimLens(1)
             ii(reindex(1)) = i
             ii(reindex(2)) = j
             ii(reindex(3)) = k
             if (ilon.ge.1) then
                ii(ilon) = modulo(ii(ilon)+ishift-2,varNetCDF(1)%dimLens(ilon))+1
             endif
             if (reverse(reindex(1))) ii(reindex(1)) = varNetCDF2%dimLens(1)+1-ii(reindex(1))
             if (reverse(reindex(2))) ii(reindex(2)) = varNetCDF2%dimLens(1)+1-ii(reindex(2))
             if (reverse(reindex(3))) ii(reindex(3)) = varNetCDF2%dimLens(1)+1-ii(reindex(3))
             varNetCDF2%data(i,j,k) = varNetCDF(1)%data(ii(1),ii(2),ii(3))
          enddo
       enddo
    enddo
    deallocate(varNetCDF(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate storage")
    varNetCDF(1) = varNetCDF2
    deallocate(varNetCDF2,stat=status)
    if (status /= 0) call die("Could not deallocate storage")
! deallocate memeory
    do n=1,3
       deallocate(axes(n)%data,stat=status)
       if (status /= 0) call die("Could not deallocate storage")
    enddo
    deallocate(axes,stat=status)
    if (status /= 0) call die("Could not deallocate storage")

  end subroutine normaliseReal3dVariable

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: normaliseReal3dRecordVariable
  !>
  !> normalises variable and axes defined on a subset of longitude,
  !> latitude, depth/z axes:
  !>  i) Rearrange sequence of available axes according to sequence
  !>  longitude, latitude, depth/z, other axes
  !>  ii) test if longitude, latitdue strictly increasing monotonic,
  !>  flip if necessary, stop if not strictly monotonic
  !>  iii) rotate dataset along longitudinal direction to occupy range
  !>  [0,360] degrees east
  !>  iv) test if depth/z strictly increasing, flip if necessary,
  !>  stop if not strictly monotonic
  !>
  !----------------------------------------------------------------------
  subroutine normaliseReal3dRecordVariable(ID,varNetCDF)

    type(real3dRecordVar),pointer,dimension(:) :: varNetCDF
    type(real3dRecordVar),pointer :: varNetCDF2

    type(real1dVar),pointer,dimension(:) :: axes

    integer :: ID, status, n, i, j, k, ilon, ilat, iz, ishift
    integer,dimension(3) :: ii
    real :: londiff

    integer :: recdimindex
    integer,dimension(3) :: reindex
    logical,dimension(3) :: reverse

! construct 'reindex' array which describes permutations of dimensions
    recdimindex=varNetCDF(1)%recordDimIndex
    ilon = -1
    ilat = -1
    iz = -1
    do n=1,3
       reindex(n) = -1
       reverse(n) = .false.
    enddo
    i = 1
    do n=1,4
       if (n.ne.recdimindex) then
          select case (trim(varNetCDF(1)%dimNames(n)))
          case ('longitude','Longitude','LONGITUDE','lon','Lon','LON')
             reindex(i) = n
             ilon = n
             i=i+1
          end select
       endif
    enddo
    do n=1,4
       if (n.ne.recdimindex) then
          select case (trim(varNetCDF(1)%dimNames(n)))
          case ('latitude','Latitude','LATITUDE','lat','Lat','LAT')
             reindex(i) = n
             ilat = n
             i=i+1
          end select
       endif
    enddo
    do n=1,4
       if (n.ne.recdimindex) then
          select case (trim(varNetCDF(1)%dimNames(n)))
          case ('depth','Depth','DEPTH','z','Z')
             reindex(i) = n
             iz = n
             i=i+1
          end select
       endif
    enddo
    do n=1,4
       if ((n.ne.recdimindex).and.(reindex(n).eq.-1)) then
          reindex(i) = n
          i=i+1
       endif
    enddo
! read axes
    allocate(axes(3),stat=status)
    if (status /= 0) call die("Could not allocate storage")
    do n=1,4
       if (n.ne.recdimindex) then
          axes(n)%name=varNetCDF(1)%dimNames(n)
       endif
    enddo
    call lookupVars(ID,axes)
    do n=1,4
       if (n.ne.recdimindex) then
          if (axes(n)%id < 0) then
             call die("Axis not found!")
          else
             allocate(axes(n)%data(varNetCDF(1)%dimLens(n)),stat=status)
             if (status /= 0) call die("Could not allocate storage")
          endif
       endif
    enddo
    call readVars(ID,axes)

! test if reversal of direction of axes is required
    if (ilon.ge.1) then
       if (axes(ilon)%data(1).gt.axes(ilon)%data(2)) then
          reverse(ilon)=.true.
       endif
    endif
    if (ilat.ge.1) then
       if (axes(ilat)%data(1).gt.axes(ilat)%data(2)) then
          reverse(ilat)=.true.
       endif
    endif
    if (iz.ge.1) then
       if (axes(iz)%data(1).gt.axes(iz)%data(2)) then
          reverse(iz)=.true.
       endif
    endif

! verify if axes are strictly monotonic
    if (ilon.ge.1) then
       if (reverse(ilon)) then
          do n=2,axes(ilon)%dimLens(1)
             if (axes(ilon)%data(n).ge.axes(ilon)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(ilon)%dimLens(1)
             if (axes(ilon)%data(n).le.axes(ilon)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif
    if (ilat.ge.1) then
       if (reverse(ilat)) then
          do n=2,axes(ilat)%dimLens(1)
             if (axes(ilat)%data(n).ge.axes(ilat)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(ilat)%dimLens(1)
             if (axes(ilat)%data(n).le.axes(ilat)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif
    if (iz.ge.1) then
       if (reverse(iz)) then
          do n=2,axes(iz)%dimLens(1)
             if (axes(iz)%data(n).ge.axes(iz)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       else
          do n=2,axes(iz)%dimLens(1)
             if (axes(iz)%data(n).le.axes(iz)%data(n-1)) call die("Axis not strictly monotonic!")
          enddo
       endif
    endif

! compute offset required to shift longitudinal extent into continuous range
! [0,360] degrees east by searching index with then smallest distance
! of its longitude modulo 360 from 0)
    ishift = 1
    londiff = 9e19
    if (ilon.ge.1) then
       do n=1,varNetCDF(1)%dimLens(ilon)
          if (modulo(axes(ilon)%data(n),360.0).lt.londiff) then
             ishift = n
             londiff = modulo(axes(ilon)%data(n),360.0)
          endif
       enddo
    endif

! apply transformation
    allocate(varNetCDF2,stat=status)
    if (status /= 0) call die("Could not allocate storage")
    varNetCDF2%name = varNetCDF(1)%name
    do n=1,3
       varNetCDF2%dimIDs(n) = varNetCDF(1)%dimIDs(reindex(n))
       varNetCDF2%dimLens(n) = varNetCDF(1)%dimLens(reindex(n))
       varNetCDF2%dimNames(n) = varNetCDF(1)%dimNames(reindex(n))
    enddo
    varNetCDF2%nAtts = varNetCDF(1)%nAtts
    varNetCDF2%id = varNetCDF(1)%id
    varNetCDF2%basicAtts = varNetCDF(1)%basicAtts
    allocate(varNetCDF2%data(varNetCDF2%dimLens(1),varNetCDF2%dimLens(2),varNetCDF2%dimLens(3)),stat=status)
    if (status /= 0) call die("Could not allocate storage")
    do k=1,varNetCDF2%dimLens(3)
       do j=1,varNetCDF2%dimLens(2)
          do i=1,varNetCDF2%dimLens(1)
             ii(reindex(1)) = i
             ii(reindex(2)) = j
             ii(reindex(3)) = k
             if (ilon.ge.1) then
                ii(ilon) = modulo(ii(ilon)+ishift-2,varNetCDF(1)%dimLens(ilon))+1
             endif
             if (reverse(reindex(1))) ii(reindex(1)) = varNetCDF2%dimLens(1)+1-ii(reindex(1))
             if (reverse(reindex(2))) ii(reindex(2)) = varNetCDF2%dimLens(1)+1-ii(reindex(2))
             if (reverse(reindex(3))) ii(reindex(3)) = varNetCDF2%dimLens(1)+1-ii(reindex(3))
             varNetCDF2%data(i,j,k) = varNetCDF(1)%data(ii(1),ii(2),ii(3))
          enddo
       enddo
    enddo
    deallocate(varNetCDF(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate storage")
    varNetCDF(1) = varNetCDF2
    deallocate(varNetCDF2,stat=status)
    if (status /= 0) call die("Could not deallocate storage")
! deallocate memeory
    do n=1,3
       deallocate(axes(n)%data,stat=status)
       if (status /= 0) call die("Could not deallocate storage")
    enddo
    deallocate(axes,stat=status)
    if (status /= 0) call die("Could not deallocate storage")

  end subroutine normaliseReal3dRecordVariable

end module local_input
