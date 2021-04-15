
      subroutine igcm_land_runoff(runoff,input,i,jj)

c     The purpose of this subroutine is to
c     add run-off to the IGCM land scheme's
c     run-off field

      implicit none

#include "igcm_land_resolution.inc"
      include 'igcm_area.cmn'

      real runoff(mg,jgg)
      real input
      integer i,jj

        runoff(NINT(runoff_index(i,jj,1)),
     $         NINT(runoff_index(i,jj,2)))=
     $  runoff(NINT(runoff_index(i,jj,1)),
     $         NINT(runoff_index(i,jj,2)))+
     $    input*
     $    weight_atm(i,jj)/
     $    weight_atm(NINT(runoff_index(i,jj,1))
     &              ,NINT(runoff_index(i,jj,2)))

      return
      end
