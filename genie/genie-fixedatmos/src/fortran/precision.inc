!    This is a file to determine the precision of the libutil1 module...

      integer rk_libutil1

#ifdef utreal4
      parameter(rk_libutil1=4)
#endif

#ifdef utreal8
      parameter(rk_libutil1=8)
#endif

