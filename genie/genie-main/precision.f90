MODULE precision

  !     This is a file to determine the precision of the libzxp,libnc1 
  !       and libutil1 modules...

  integer :: rk_libzxp
  integer :: rk_libutil1
  integer :: rk_libnc1

#ifdef zxreal4
      parameter(rk_libzxp=4)
#endif

#ifdef zxreal8
      parameter(rk_libzxp=8)
#endif

#ifdef utreal4
      parameter(rk_libutil1=4)
#endif

#ifdef utreal8
      parameter(rk_libutil1=8)
#endif

#ifdef ncreal4
      parameter(rk_libnc1=4)
#endif

#ifdef ncreal8
      parameter(rk_libnc1=8)
#endif

END MODULE precision
