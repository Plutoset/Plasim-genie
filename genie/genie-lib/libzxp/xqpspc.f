      SUBROUTINE XQPSPC(PX1,PX2,PY1, PY2)
C Return the current picture space parameters defined by XPSPAC
C subject to no picture scaling.
C
      REAL PX1
      REAL PL
      REAL PX2
      REAL PR
      REAL PY1
      REAL PB
      REAL PY2
      REAL PT
      REAL RANGEX
      REAL XRANGE
      REAL RANGEY
      REAL YRANGE
C
      COMMON /XPHY01/PL,PR,PB,PT,XRANGE,YRANGE
      PX1=PL
      PX2=PR
      PY1=PB
      PY2=PT
      RETURN
      ENTRY XQRANG( RANGEX,RANGEY)
C Return the actual length of picture sides measured in ND-space
C subject to picture scaling. ( X and Y denote direction of axes
C before coordinate rotation but subject to overall picture ratation.)
      RANGEX=XRANGE
      RANGEY=YRANGE
      RETURN
      END
