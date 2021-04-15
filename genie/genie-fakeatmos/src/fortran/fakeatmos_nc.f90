
module fakeatmos_nc

contains

  subroutine fakeatmos_ncwrite(filename,field,fieldname)

    use fakeatmos_common, only:rk,handle_err

    implicit none

    include 'netcdf.inc'

    character(*),           intent(in) :: filename
    real(rk),dimension(:,:),intent(in) :: field
    character(*),           intent(in) :: fieldname

    integer :: nx,ny
    integer :: status,ncid,dimidx,dimidy,varid

    nx=size(field,1) ; ny=size(field,2)

    status=nf_create(filename,0,ncid)
    call handle_err(status)
    status=nf_redef(ncid)
    call handle_err(status)
    status=nf_def_dim(ncid,'x',nx,dimidx)
    call handle_err(status)
    status=nf_def_dim(ncid,'y',ny,dimidy)
    call handle_err(status)

    select case(rk)
    case(4)
       status=nf_def_var(ncid,fieldname,NF_FLOAT, 2,(/dimidx,dimidy/),varid)
       call handle_err(status)
       status=nf_enddef(ncid)
       call handle_err(status)
       status=nf_put_vara_real(ncid,varid,(/1,1/),(/nx,ny/),field)
       call handle_err(status)
    case(8)
       status=nf_def_var(ncid,fieldname,NF_DOUBLE,2,(/dimidx,dimidy/),varid)
       call handle_err(status)
       status=nf_enddef(ncid)
       call handle_err(status)
       status=nf_put_vara_double(ncid,varid,(/1,1/),(/nx,ny/),field)
       call handle_err(status)
    case default
       print*,'Unsupported kind'
       status=nf_close(ncid)
       call handle_err(status)
       stop
    end select

    status=nf_close(ncid)
    call handle_err(status)

  end subroutine fakeatmos_ncwrite

end module fakeatmos_nc
