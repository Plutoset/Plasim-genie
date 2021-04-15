      module geniemod
      implicit none

      integer NLAT_ATM,NLEV_ATM,NPRO_ATM

      parameter(NLAT_ATM = 32)
      parameter(NLEV_ATM = 10)
      parameter(NPRO_ATM = 1)

! coupling inputs from genie
      real :: genie_sst(NLAT_ATM*NLAT_ATM*2)
      real :: genie_icet(NLAT_ATM*NLAT_ATM*2)
      real :: genie_hght_sic(NLAT_ATM*NLAT_ATM*2)
      real :: genie_frac_sic(NLAT_ATM*NLAT_ATM*2)
      real :: genie_alb_sic(NLAT_ATM*NLAT_ATM*2)
      real :: genie_dflux(4,NLAT_ATM*NLAT_ATM*2) !not used
     
      real :: genie_co2
 
! IO Directories
      character :: indir_name*200
      character :: outdir_name*200
      character :: rstdir_name*200

! flux correction (GENIE lon-lat grid)
      real :: genie_apm(NLAT_ATM*2,NLAT_ATM)
      real :: scale_apm

      end module geniemod
