c     This is a file to determine the precision of the genie module...

#ifdef real4
      integer rk_in
      parameter(rk_in=4)
#endif

#ifdef real8
      integer rk_in
      parameter(rk_in=8)
#endif

#ifdef utreal4
      integer rk_libutil1
      parameter(rk_libutil1=4)
#endif

#ifdef utreal8
      integer rk_libutil1
      parameter(rk_libutil1=8)
#endif
