      SUBROUTINE XAXES(XO,XSTEP,YO,YSTEP)
C Draw X and Y axis through (XO,YO) with tick interval XSTEP and YSTEP
C If XSTEP or YSTEP=0.0,the intervals are set automatically
      REAL XO
      REAL YO
      REAL XSTEP
      REAL YSTEP
C
      CALL XAXISX(XO,YO,XSTEP)
      CALL XAXISY(XO,YO,YSTEP)
      RETURN
      END
