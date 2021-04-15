      SUBROUTINE ZSF001(X,Y,IPLOT,PY,PYP,YP,DX)
C
      INTEGER IPLOT
      INTEGER ILAST
      REAL X
      REAL Y
      REAL XX
      REAL PY
      REAL YP
      REAL PYP
      REAL YY
      REAL DX
C
      SAVE ILAST
      IF(IPLOT.EQ.ILAST) THEN
        IF(IPLOT.GT.0) THEN
            CALL XPENDN(X,Y)
        ENDIF
      ELSE
        XX=(PY-Y)/(YP-PYP + PY-Y)
        YY=Y+(YP-Y)*XX
        XX=X+XX*DX
        IF(IPLOT.EQ.0) THEN
            CALL XPENDN(XX,YY)
            IPLOT=0
        ELSE
            CALL XPENUP(XX,YY)
            CALL XPENDN(X, Y )
            IPLOT=1
        ENDIF
      ENDIF
      ILAST=IPLOT
      RETURN
      ENTRY      ZSF002(X,Y,IPLOT)
C      this entry is used to initialize the state of plotting line
        CALL XPENUP(X,Y)
      ILAST=IPLOT
      RETURN
      ENTRY      ZSF003(X,Y)
      IF(ILAST.GT.0) THEN
        CALL XPENDN(X,Y)
      ELSE
        CALL XPENUP(X,Y)
      ENDIF
      RETURN
      END
