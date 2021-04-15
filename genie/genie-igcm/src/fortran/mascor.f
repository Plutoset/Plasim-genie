C**********************************************************
C     SUBROUTINE MASCOR
C**********************************************************
      SUBROUTINE MASCOR

      IMPLICIT NONE

C     
C     MASCOR - Correction of global average surface pressure.
C     No longer requires KOUNTE=1 to work correctly.
C     
C     Mike Blackburn  15/05/2000.  Original version for IGCM2.
C     
C     Purpose.
C     --------
C     
C     This subroutine modifies the m=0,n=0 coefficient of ln(ps)
c     to correct the mean surface pressure (total mass) at that at the
c     start of the run, preventing drift in extended integrations.
C     
C     Interface.
C     ----------
C     
C     MASCOR is called from the main program MLTRI.
C     Timestep counters are stored in common BATS.
C     Mean surface pressures and switches are stored in common STATS.
C     Spectral arrays SP,SPA are stored in common SPECTR.
C     
C     Method.
C     -------
C     
C     Rescale the global mean surface pressure to its initial value
c     by adding the required constant to m=0,n=0 ln(ps) coefficient.
C     Use the time-lagged mean pressure to avoid numerical instability.
C     
C     It is assumed there are no sources or sinks in the mass
C     continuity equation (i.e. will not work if SPA changes in the 
C     diabatic part of the time step).
C     The correction should be applied to the
C     mass of dry air if precip/evap mass variations were included.
C    
c     ---------------------------------------
c+DJL
c     this routine works on the restart value of sp,
c     so we may have to be careful when restarting a 
c     simulation in which the orography has changed.......
c     i think this should be ok, but it's worth flagging up.
c     also, the standard restart file for a 'cold-start' simulation
c     is present-day spa, so we should be OK if we do an LGM run,
c     as the total mass of the atmosphere will still be that of present.      
c-DJL
c     ---------------------------------------
c
#include "param1.cmn"
      include 'param2.cmn'
      include 'bats.cmn'
      include 'spectr.cmn'
      include 'stats.cmn'

      INTEGER KTEMP
      REAL    GMSP,ZSPRAT,ZLNRAT,ZSPDIF

C     
C     -----------------------------------------------------------------
C     
C     Extract the global mass.
C     
      GMSP=1.0+REAL(SPA(1))/SQRT(2.0)
C     
C     -----------------------------------------------------------------
C     
C     Preserve the mass at (re)start as required.
C     Note that the timestep counter has already been incremented.
C     Pressure at start of initial run. 
C     Otherwise read in from restart file.
C     
      IF ((KOUNT.EQ.KSTART+1).AND.GMSP0.EQ.0.0) THEN
         GMSP0=GMSP
         GMSPMI=GMSP
         WRITE(6,6900) GMSP
      ENDIF
C     
C     -----------------------------------------------------------------
C     
C     Compute correction.
C     
      ZSPRAT=GMSPMI/GMSP0
      ZLNRAT=LOG(ZSPRAT)
      SP(1)=SP(1)-SQRT(2.)*CMPLX(ZLNRAT,0.)
C     
C     -----------------------------------------------------------------
C     
C     Print diagnostics, correcting for (KITS-1) offset.
C     
      IF (KITS.GT.0) THEN
         KTEMP=KOUNT+1-KITS
      ELSE
         KTEMP=KOUNT
      ENDIF
      IF (LMASPRT.AND.
     :     (KTEMP.LE.(KSTART+ITSPD).OR.MOD(KTEMP,ITSPD).EQ.0)) THEN
         ZSPDIF=GMSPMI-GMSP0
         WRITE(6,6901) GMSPMI,GMSP0,ZSPDIF,ZLNRAT,REAL(SP(1))
      ENDIF
C     
C     -----------------------------------------------------------------
C     
C     Update the time-lagged mass.
C     
      GMSPMI=GMSP
C     
C     -----------------------------------------------------------------
C     
C     Formats.
C     
 6900 FORMAT(' GLOBAL MASS RESET TO CURRENT RESTART VALUE =',1PE15.8)
 6901 FORMAT('  PMEAN=',1PE15.8,' PM0=',1PE15.8,
     :      ' DIFF=',1PE15.8,' LN(PS) CORR=',1PE15.8,
     :      ' LN(PS)=',1PE15.8)
C     
C     -----------------------------------------------------------------
C     
      RETURN
      END
