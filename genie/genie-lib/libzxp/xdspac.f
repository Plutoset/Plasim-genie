C ******************************************************************
      SUBROUTINE XDSPAC(PSIZE)
C Define a normalized device (ND) space (0.0,XSIDE,0.0,YSIDE) on device
C provided. The size of this space is 'PSIZE' times of the max device
C space.  YSIDE should be 1.0, XSIDE can be bigger or smaller that 1.0
C depending on the device.
C XSIDE, YSIDE should be passed to ZXPLOT through common block XPDS01.
C This routine sets up this space using GHOST.
C
      include 'uniras.inc'
C
      REAL PSIZE1
      REAL PSIZE
      REAL XSIDE
      REAL YSIDE
C
      COMMON /XPSIZE/ PSIZE1
      COMMON /XPSD01/ XSIDE, YSIDE
      PSIZE1=PSIZE
      XSIDE=1.9/1.5
      YSIDE=1.0
      CALL XMPSC(XSIDE,YSIDE)
      RETURN
      END
