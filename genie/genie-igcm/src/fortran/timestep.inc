C
C     Constants needed for timestep
C
c     This is the new style (24 hour day)
      real my_pi
      parameter(my_pi=3.14159265359)
      real ww
      parameter(ww=2*my_pi/(24.0*3600.0))
c     
c     This is the old style (sidereal day)
c      parameter(ww=7.292E-5)
C
