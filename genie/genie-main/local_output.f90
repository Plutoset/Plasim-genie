 
!----------------------------------------------------------------------
!>
!> Module: local_output
!>
!> Output routines: The routines are generally stateless (access via
!> filename of the output file. These routines use the NetCDF routines
!> provided by the module "local_netcdf".
!> 
!----------------------------------------------------------------------
module local_output

  use genie_util, only: message,die

  use local_netcdf

  PRIVATE

  interface writeVariable
     module procedure writeReal2dVariable
     module procedure writeReal3dVariable
  end interface writeVariable

  interface writeRecordVariable
     module procedure writeReal1dRecordVariable
     module procedure writeReal2dRecordVariable
     module procedure writeReal3dRecordVariable
     module procedure writeInteger1dRecordVariable
     module procedure writeInteger2dRecordVariable
     module procedure writeInteger3dRecordVariable
  end interface writeRecordVariable

  PUBLIC :: resetOutput,defineDimension,defineRecordDimension,writeVariable,writeRecordVariable

contains

  !----------------------------------------------------------------------
  !>
  !> Internal subroutine: openOutput
  !>
  !> opens output file, returns handle to output file
  !> 
  !----------------------------------------------------------------------
  subroutine openOutput(filename,ID,readonly,reset)

    character(len=*),intent(in) :: filename

    integer :: ID

    logical,optional,intent(in) :: readonly
    logical,optional,intent(in) :: reset

    logical :: exists

    if (present(readonly).and.(readonly)) then
       call message("Opening existing output file in read-only mode!",3)
       call openNetCDFRead(filename,ID)
    else
       inquire(file=filename,exist=exists)
       if (.not.exists) then
          call message("Creating new output file: "//filename,3)
          call createNetCDF(filename,ID)
          call endDef(ID)
       else
          if (present(reset).and.(reset)) then
             call message("Resetting existing output file: "//filename,3)
             call createNetCDF(filename,ID,.true.)
          else
             call message("Opening existing output file: "//filename,3)
             call openNetCDFWrite(filename,ID)
          endif
       endif
    endif

  end subroutine openOutput

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
  !> Subroutine: resetOutput
  !>
  !> resets (or creates) output file
  !> 
  !----------------------------------------------------------------------
  subroutine resetOutput(filename)

    character(len=*),intent(in) :: filename

    integer ID

    call openOutput(filename,ID,.false.,.true.)
    call closeInOutput(ID)

  end subroutine resetOutput

  !----------------------------------------------------------------------
  !>
  !> Subroutine: defineDimension
  !>
  !> defines Dimension
  !> 
  !----------------------------------------------------------------------

  subroutine defineDimension(filename,dimName,dimValues,dimBoundariesLower,dimBoundariesUpper,dimLongName,dimStandardName,dimUnits)

    character(len=*),intent(in) :: filename,dimName

    real,dimension(:),intent(in) :: dimValues

    real,dimension(:),intent(in),optional :: dimBoundariesLower,dimBoundariesUpper

    character(len=*),intent(in),optional :: dimLongName,dimStandardName,dimUnits

    integer :: ID,status,dimlen

    type(realDimInfo),pointer,dimension(:) :: dimNetCDF

    dimLen = size(dimValues)

    call openOutput(filename,ID)

    allocate(dimNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    allocate(dimNetCDF(1)%coords(1:dimlen),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    allocate(dimNetCDF(1)%boundsLower(dimlen),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    allocate(dimNetCDF(1)%boundsUpper(dimlen),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif

    if (present(dimBoundariesLower).or.present(dimBoundariesUpper)) then
       dimNetCDF(1)%boundsDefine = .true.
    endif
    dimNetCDF(1)%name = dimName
    dimNetCDF(1)%len = dimLen
    if (present(dimLongName)) then
       dimNetCDF(1)%basicAtts%long_name = dimLongName
    else
       dimNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(dimLongName)) then
       dimNetCDF(1)%basicAtts%standard_name = dimStandardName
    else
       dimNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(dimLongName)) then
       dimNetCDF(1)%basicAtts%units = dimUnits
    else
       dimNetCDF(1)%basicAtts%units = ""
    endif
    dimNetCDF(1)%coords(:) = dimValues(:)
    if (present(dimBoundariesLower)) then
       dimNetCDF(1)%boundsLower(:) = dimBoundariesLower(:)
    else
       dimNetCDF(1)%boundsLower(:) = dimValues(:)
    endif
    if (present(dimBoundariesUpper)) then
       dimNetCDF(1)%boundsUpper(:) = dimBoundariesUpper(:)
    else
       dimNetCDF(1)%boundsUpper(:) = dimValues(:)
    endif

    call defineDims(ID,dimNetCDF)

    call closeInOutput(ID)

    deallocate(dimNetCDF(1)%coords)
    deallocate(dimNetCDF(1)%boundsLower)
    deallocate(dimNetCDF(1)%boundsUpper)
    deallocate(dimNetCDF)

  end subroutine defineDimension

  !----------------------------------------------------------------------
  !>
  !> Subroutine: defineRecordDimension
  !>
  !> defines record Dimension
  !> 
  !----------------------------------------------------------------------

  subroutine defineRecordDimension(filename,dimName,dimBoundaries,dimLongName,dimStandardName,dimUnits)

    character(len=*),intent(in) :: filename,dimName

    logical,optional :: dimBoundaries

    character(len=*),intent(in),optional :: dimLongName,dimStandardName,dimUnits

    integer :: ID,status

    type(realRecordDimInfo),pointer,dimension(:) :: dimNetCDF

    call openOutput(filename,ID)

    allocate(dimNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif

    dimNetCDF(1)%coordsDefine = .true.
    if (present(dimBoundaries).and.dimBoundaries.eqv..true.) then
       dimNetCDF(1)%boundsDefine = .true.
    endif
    dimNetCDF(1)%name = dimName
    if (present(dimLongName)) then
       dimNetCDF(1)%basicAtts%long_name = dimLongName
    else
       dimNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(dimStandardName)) then
       dimNetCDF(1)%basicAtts%standard_name = dimStandardName
    else
       dimNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(dimUnits)) then
       dimNetCDF(1)%basicAtts%units = dimUnits
    else
       dimNetCDF(1)%basicAtts%units = ""
    endif
    call defineDims(ID,dimNetCDF)

    call closeInOutput(ID)

    deallocate(dimNetCDF)

  end subroutine defineRecordDimension

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal1dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal1dRecordVariable( &
       & filename, &
       & varName, &
       & varValues, &
       & recordCoord, &
       & recordCoordBounds, &
       & varDimNames, &
       & varLongName, &
       & varStandardName, &
       & varUnits, &
       & varMissingValue, &
       & offset)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in),dimension(2),optional :: varDimNames

    real,dimension(:),intent(in) :: varValues

    real,intent(in) :: recordCoord
    real,intent(in),dimension(2),optional :: recordCoordBounds

    character(len=*),intent(in),optional :: varLongName,varStandardName,varUnits

    real,intent(in),optional :: varMissingValue

    integer,intent(in),optional :: offset

    type(real1dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(1) :: arraySize

    call openOutput(filename,ID)
    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = trim(varName)
    if (present(varLongName)) then
       varNetCDF(1)%basicAtts%long_name = varLongName
    else
       varNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(varStandardName)) then
       varNetCDF(1)%basicAtts%standard_name = varStandardName
    else
       varNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(varUnits)) then
       varNetCDF(1)%basicAtts%units = varUnits
    else
       varNetCDF(1)%basicAtts%units = ""
    endif
    if (present(varMissingValue)) then
       varNetCDF(1)%basicAtts%missing_value = varMissingValue
    else
       varNetCDF(1)%basicAtts%missing_value = -9.9e19
    endif
    varNetCDF(1)%data(:) = varValues(:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       if (.not.present(varDimNames)) then
          call die("Dimension names required!")
       endif

       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    if (present(offset)) then
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds,offset=offset)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/),offset=offset)
       endif
    else
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/))
       endif
    endif
    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal1dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal2dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal2dRecordVariable( &
       & filename, &
       & varName, &
       & varValues, &
       & recordCoord, &
       & recordCoordBounds, &
       & varDimNames, &
       & varLongName, &
       & varStandardName, &
       & varUnits, &
       & varMissingValue, &
       & offset)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in),dimension(3),optional :: varDimNames

    real,dimension(:,:),intent(in) :: varValues

    real,intent(in) :: recordCoord
    real,intent(in),dimension(2),optional :: recordCoordBounds

    character(len=*),intent(in),optional :: varLongName,varStandardName,varUnits

    real,intent(in),optional :: varMissingValue

    integer,intent(in),optional :: offset

    type(real2dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(2) :: arraySize

    call openOutput(filename,ID)
    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = trim(varName)
    if (present(varLongName)) then
       varNetCDF(1)%basicAtts%long_name = varLongName
    else
       varNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(varStandardName)) then
       varNetCDF(1)%basicAtts%standard_name = varStandardName
    else
       varNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(varUnits)) then
       varNetCDF(1)%basicAtts%units = varUnits
    else
       varNetCDF(1)%basicAtts%units = ""
    endif
    if (present(varMissingValue)) then
       varNetCDF(1)%basicAtts%missing_value = varMissingValue
    else
       varNetCDF(1)%basicAtts%missing_value = -9.9e19
    endif
    varNetCDF(1)%data(:,:) = varValues(:,:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       if (.not.present(varDimNames)) then
          call die("Dimension names required!")
       endif

       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    if (present(offset)) then
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds,offset=offset)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/),offset=offset)
       endif
    else
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/))
       endif
    endif
    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal2dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal3dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal3dRecordVariable( &
       & filename, &
       & varName, &
       & varValues, &
       & recordCoord, &
       & recordCoordBounds, &
       & varDimNames, &
       & varLongName, &
       & varStandardName, &
       & varUnits, &
       & varMissingValue, &
       & offset)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in),dimension(4),optional :: varDimNames

    real,dimension(:,:,:),intent(in) :: varValues

    real,intent(in) :: recordCoord
    real,intent(in),dimension(2),optional :: recordCoordBounds

    character(len=*),intent(in),optional :: varLongName,varStandardName,varUnits

    real,intent(in),optional :: varMissingValue

    integer,intent(in),optional :: offset

    type(real3dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(3) :: arraySize

    call openOutput(filename,ID)
    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2),arraySize(3)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = trim(varName)
    if (present(varLongName)) then
       varNetCDF(1)%basicAtts%long_name = varLongName
    else
       varNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(varStandardName)) then
       varNetCDF(1)%basicAtts%standard_name = varStandardName
    else
       varNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(varUnits)) then
       varNetCDF(1)%basicAtts%units = varUnits
    else
       varNetCDF(1)%basicAtts%units = ""
    endif
    if (present(varMissingValue)) then
       varNetCDF(1)%basicAtts%missing_value = varMissingValue
    else
       varNetCDF(1)%basicAtts%missing_value = -9.9e19
    endif
    varNetCDF(1)%data(:,:,:) = varValues(:,:,:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       if (.not.present(varDimNames)) then
          call die("Dimension names required!")
       endif

       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    if (present(offset)) then
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds,offset=offset)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/),offset=offset)
       endif
    else
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/))
       endif
    endif
    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal3dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeInteger1dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeInteger1dRecordVariable( &
       & filename, &
       & varName, &
       & varValues, &
       & recordCoord, &
       & recordCoordBounds, &
       & varDimNames, &
       & varLongName, &
       & varStandardName, &
       & varUnits, &
       & varMissingValue, &
       & offset)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in),dimension(2),optional :: varDimNames

    integer,dimension(:),intent(in) :: varValues

    real,intent(in) :: recordCoord
    real,intent(in),dimension(2),optional :: recordCoordBounds

    character(len=*),intent(in),optional :: varLongName,varStandardName,varUnits

    real,intent(in),optional :: varMissingValue

    integer,intent(in),optional :: offset

    type(integer1dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(1) :: arraySize

    call openOutput(filename,ID)
    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = trim(varName)
    if (present(varLongName)) then
       varNetCDF(1)%basicAtts%long_name = varLongName
    else
       varNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(varStandardName)) then
       varNetCDF(1)%basicAtts%standard_name = varStandardName
    else
       varNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(varUnits)) then
       varNetCDF(1)%basicAtts%units = varUnits
    else
       varNetCDF(1)%basicAtts%units = ""
    endif
    if (present(varMissingValue)) then
       varNetCDF(1)%basicAtts%missing_value = varMissingValue
    else
       varNetCDF(1)%basicAtts%missing_value = -9.9e19
    endif
    varNetCDF(1)%data(:) = varValues(:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       if (.not.present(varDimNames)) then
          call die("Dimension names required!")
       endif

       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    if (present(offset)) then
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds,offset=offset)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/),offset=offset)
       endif
    else
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/))
       endif
    endif
    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeInteger1dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeInteger2dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeInteger2dRecordVariable( &
       & filename, &
       & varName, &
       & varValues, &
       & recordCoord, &
       & recordCoordBounds, &
       & varDimNames, &
       & varLongName, &
       & varStandardName, &
       & varUnits, &
       & varMissingValue, &
       & offset)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in),dimension(3),optional :: varDimNames

    integer,dimension(:,:),intent(in) :: varValues

    real,intent(in) :: recordCoord
    real,intent(in),dimension(2),optional :: recordCoordBounds

    character(len=*),intent(in),optional :: varLongName,varStandardName,varUnits

    real,intent(in),optional :: varMissingValue

    integer,intent(in),optional :: offset

    type(integer2dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(2) :: arraySize

    call openOutput(filename,ID)
    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = trim(varName)
    if (present(varLongName)) then
       varNetCDF(1)%basicAtts%long_name = varLongName
    else
       varNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(varStandardName)) then
       varNetCDF(1)%basicAtts%standard_name = varStandardName
    else
       varNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(varUnits)) then
       varNetCDF(1)%basicAtts%units = varUnits
    else
       varNetCDF(1)%basicAtts%units = ""
    endif
    if (present(varMissingValue)) then
       varNetCDF(1)%basicAtts%missing_value = varMissingValue
    else
       varNetCDF(1)%basicAtts%missing_value = -9.9e19
    endif
    varNetCDF(1)%data(:,:) = varValues(:,:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       if (.not.present(varDimNames)) then
          call die("Dimension names required!")
       endif

       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    if (present(offset)) then
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds,offset=offset)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/),offset=offset)
       endif
    else
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/))
       endif
    endif
    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeInteger2dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeInteger3dRecordVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeInteger3dRecordVariable( &
       & filename, &
       & varName, &
       & varValues, &
       & recordCoord, &
       & recordCoordBounds, &
       & varDimNames, &
       & varLongName, &
       & varStandardName, &
       & varUnits, &
       & varMissingValue, &
       & offset)

    character(len=*),intent(in) :: filename,varName

    character(len=*),intent(in),dimension(4),optional :: varDimNames

    integer,dimension(:,:,:),intent(in) :: varValues

    real,intent(in) :: recordCoord
    real,intent(in),dimension(2),optional :: recordCoordBounds

    character(len=*),intent(in),optional :: varLongName,varStandardName,varUnits

    real,intent(in),optional :: varMissingValue

    integer,intent(in),optional :: offset

    type(integer3dRecordVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(3) :: arraySize

    call openOutput(filename,ID)
    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2),arraySize(3)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = trim(varName)
    if (present(varLongName)) then
       varNetCDF(1)%basicAtts%long_name = varLongName
    else
       varNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(varStandardName)) then
       varNetCDF(1)%basicAtts%standard_name = varStandardName
    else
       varNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(varUnits)) then
       varNetCDF(1)%basicAtts%units = varUnits
    else
       varNetCDF(1)%basicAtts%units = ""
    endif
    if (present(varMissingValue)) then
       varNetCDF(1)%basicAtts%missing_value = varMissingValue
    else
       varNetCDF(1)%basicAtts%missing_value = -9.9e19
    endif
    varNetCDF(1)%data(:,:,:) = varValues(:,:,:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       if (.not.present(varDimNames)) then
          call die("Dimension names required!")
       endif

       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    if (present(offset)) then
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds,offset=offset)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/),offset=offset)
       endif
    else
       if (present(recordCoordBounds)) then
          call appendVars(ID,varNetCDF,recordCoord,recordCoordBounds)
       else
          call appendVars(ID,varNetCDF,recordCoord,(/recordCoord,recordCoord/))
       endif
    endif
    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeInteger3dRecordVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal2dVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal2dVariable( &
       & filename, &
       & varName, &
       & varValues, &
       & varDimNames, &
       & varLongName, &
       & varStandardName, &
       & varUnits, &
       & varMissingValue)

    character(len=*),intent(in) :: filename,varName

    real,dimension(:,:),intent(in) :: varValues

    character(len=*),intent(in),dimension(2),optional :: varDimNames

    character(len=*),intent(in),optional :: varLongName,varStandardName,varUnits

    real,intent(in),optional :: varMissingValue

    type(real2dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(2) :: arraySize

    call openOutput(filename,ID)
    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = trim(varName)
    if (present(varLongName)) then
       varNetCDF(1)%basicAtts%long_name = varLongName
    else
       varNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(varStandardName)) then
       varNetCDF(1)%basicAtts%standard_name = varStandardName
    else
       varNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(varUnits)) then
       varNetCDF(1)%basicAtts%units = varUnits
    else
       varNetCDF(1)%basicAtts%units = ""
    endif
    if (present(varMissingValue)) then
       varNetCDF(1)%basicAtts%missing_value = varMissingValue
    else
       varNetCDF(1)%basicAtts%missing_value = -9.9e19
    endif
    varNetCDF(1)%data(:,:) = varValues(:,:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       if (.not.present(varDimNames)) then
          call die("Dimension names required!")
       endif

       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    call writeVars(ID,varNetCDF)

    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal2dVariable

  !----------------------------------------------------------------------
  !>
  !> Subroutine: writeReal3dVariable
  !>
  !> defines Variable, if non-existing
  !> writes Variable
  !> 
  !----------------------------------------------------------------------
  subroutine writeReal3dVariable( &
       & filename, &
       & varName, &
       & varValues, &
       & varDimNames, &
       & varLongName, &
       & varStandardName, &
       & varUnits, &
       & varMissingValue)

    character(len=*),intent(in) :: filename,varName

    real,dimension(:,:,:),intent(in) :: varValues

    character(len=*),intent(in),dimension(2),optional :: varDimNames

    character(len=*),intent(in),optional :: varLongName,varStandardName,varUnits

    real,intent(in),optional :: varMissingValue

    type(real3dVar),pointer,dimension(:) :: varNetCDF

    integer :: ID,status

    integer,dimension(3) :: arraySize

    call openOutput(filename,ID)
    allocate(varNetCDF(1),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    arraySize=shape(varValues)
    allocate(varNetCDF(1)%data(arraySize(1),arraySize(2),arraySize(3)),stat=status)
    if (status /= 0) then
       call die("Could not allocate storage")
    endif
    varNetCDF(1)%name = trim(varName)
    if (present(varLongName)) then
       varNetCDF(1)%basicAtts%long_name = varLongName
    else
       varNetCDF(1)%basicAtts%long_name = ""
    endif
    if (present(varStandardName)) then
       varNetCDF(1)%basicAtts%standard_name = varStandardName
    else
       varNetCDF(1)%basicAtts%standard_name = ""
    endif
    if (present(varUnits)) then
       varNetCDF(1)%basicAtts%units = varUnits
    else
       varNetCDF(1)%basicAtts%units = ""
    endif
    if (present(varMissingValue)) then
       varNetCDF(1)%basicAtts%missing_value = varMissingValue
    else
       varNetCDF(1)%basicAtts%missing_value = -9.9e19
    endif
    varNetCDF(1)%data(:,:,:) = varValues(:,:,:)

    call lookupVars(ID,varNetCDF)
    if (varNetCDF(1)%id < 0) then

       if (.not.present(varDimNames)) then
          call die("Dimension names required!")
       endif

       call dimVars(ID,varNetCDF,varDimNames)

       call defineVars(ID,varNetCDF)

    endif

    call writeVars(ID,varNetCDF)

    call closeInOutput(ID)

    deallocate(varNetCDF(1)%data)
    deallocate(varNetCDF)

  end subroutine writeReal3dVariable

end module local_output
