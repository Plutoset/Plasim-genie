c     This is a file to determine the precision of the genie module...

      integer rk_in,rk_libutil1

#ifdef real4
      parameter(rk_in=4)
#endif

#ifdef real8
      parameter(rk_in=8)
#endif

#ifdef utreal4
      parameter(rk_libutil1=4)
#endif

#ifdef utreal8
      parameter(rk_libutil1=8)
#endif
