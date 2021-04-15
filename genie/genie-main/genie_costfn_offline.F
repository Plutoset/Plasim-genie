
c
c Offline computation of a cost function for GENIE from error scores
c supplied on a per-module basis from the modules themselves via calls
c to functionality included in their respective libraries. The
c initialisation code of the included modules is used here to ensure
c identical configuration of the modules than in the corresponding model
c executable.
c
c Currently only error scores from the EMBM and GOLDSTEIN modules are
c taken into account by implementing the computation according to the
c source code of 'genie-main/src/fortran/genie_ea_go_gs.f90'. The
c framework is designed to be extensible. Code fragments from
c 'genie-main/src/fortran/genie_ea_go_gs.f90' have been reused in
c adapted form here.
c

      program genie_costfn_offline

      use genie_control
      use genie_global
      use genie_util, ONLY:message,die

      use genie_ini_wrappers

      implicit none

      include 'netcdf.inc'

c Configuration of the error function
      character*256 configfile
      logical exists
      character*13 yearstr

c Evaluation
      integer nerr
      parameter(nerr=4)
      integer ntot(nerr)
      real err(nerr)

c ======================================================================
c     Initialise global GENIE parameters
      call initialise_genie

c ======================================================================
c     Configure evaulation of cost function
      configfile='costfn.config'
      inquire(file=trim(configfile), exist=exists)
      if (.not. exists) then
         call die('Could not find the external configuration file'//
     $        ' for the cost function evaluation',1)
      end if
      open(unit=431, file=configfile)
      read(431,*) yearstr
      close(431)
      print *,'Processing av_',yearstr

c ======================================================================
c     Check available configuraitons
      if (.not.((flag_goldsteinocean).and.(flag_ebatmos))) then
         call die('Only available if EMBM and GOLDSTEIN are active')
      endif

c ======================================================================
c     Initialise GOLDSTEIN ocean model
      if (flag_goldsteinocean) then
         call initialise_goldocean_wrapper(reinit=.true.)
      endif
c ======================================================================
c     Initialise EMBM atmosphere model
      if (flag_ebatmos) then
         call initialise_embm_wrapper(reinit=.true.)
      endif

      call message("",1)
      call message
     $     ('*******************************************************',1)
      call message(' Initialisation complete, simulation offline',1)
      call message(' evaluation of the cost function',1)
      call message
     $     ('******************************************************',1)
      call message("",1)

c======================================================================
c Calculate the individual RMS errors for each field
c======================================================================
      if (flag_goldsteinocean) then
         call rmsnorm_goldstein_T(yearstr,err(1),ntot(1))
         call rmsnorm_goldstein_S(yearstr,err(2),ntot(2))
      endif
      if (flag_ebatmos) then
         call rmsnorm_embm_T(yearstr,err(3),ntot(3))
         call rmsnorm_embm_q(yearstr,err(4),ntot(4))
      endif

c======================================================================
c Calculate a composite cost function
c======================================================================

c Original C-GOLDSTEIN composition
      if ((flag_goldsteinocean).and.(flag_ebatmos)) then
         print *,'C-GOLDSTEIN error: ',sqrt( ((err(1)**2*ntot(1)) +
     $        (err(2)**2*ntot(2)) + (err(3)**2*ntot(3)) + (err(4)**2
     $        *ntot(4))) / (ntot(1) + ntot(2) + ntot(3) + ntot(4)))
         print *,'ntot(1): ',ntot(1),' ntot(2): ',ntot(2),' ntot(3): '
     $        ,ntot(3),' ntot(4): ',ntot(4)

         open(unit=433, file='costfn.out')
         write(433,435) err(1), err(2), err(3), err(4), sqrt( ((err(1)
     $        **2*ntot(1)) + (err(2)**2*ntot(2)) + (err(3)**2*ntot(3)) +
     $        (err(4)**2*ntot(4))) / (ntot(1) + ntot(2) + ntot(3) +
     $        ntot(4)))
 435     format(5f20.16)
         close(433)
      endif

      call message("",1)
      call message
     $     ('*******************************************************',1)
      call message(' Shutdown complete, arrivederci',1)
      call message
     $     ('*******************************************************',1)
      call message("",1)
c
      stop
      end
