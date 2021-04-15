      SUBROUTINE XTPNDN (X,Y)
C Join point (x,y) defined in maths space with current line thickness
C
      REAL XP1
      REAL XPEN
      REAL YP1
      REAL YPEN
      REAL XP2
      REAL X
      REAL YP2
      REAL Y
      INTEGER LTHICK
      REAL DX
      REAL DY
      REAL DPX
      REAL DTHICK
      REAL DPY
      REAL AK
      REAL A
      REAL FLEN
      REAL BLEN
      INTEGER NPD
      REAL XMPEN
      REAL YMPEN
      REAL HF1
      REAL HB1
      REAL HF2
      REAL HB2
      INTEGER LFULL
C
      COMMON /XPEN11/ XPEN,YPEN,FLEN,BLEN,NPD,XMPEN,YMPEN
      COMMON /XLPN13/ HF1,HB1,HF2,HB2 , LFULL ,LTHICK, DTHICK
      XP1=XPEN
      YP1=YPEN
      XP2=X
      YP2=Y
      IF( LTHICK.EQ.1) THEN
      CALL PPENDN( XP2, YP2)
      ELSE
      DX=XP2-XP1
      DY=YP2-YP1
      IF( ABS(DX).LT.1.0E-6) THEN
        DPX=DTHICK
        DPY=0.0
      ELSE
        AK=DY/DX
        A=SQRT(1.0+AK*AK)
        DPX=DTHICK*AK/A
        DPY=DTHICK/A
      ENDIF
      CALL PPENDN( XP2,YP2)
      CALL PPENUP( XP2+DPX, YP2-DPY)
      CALL PPENDN( XP1+DPX, YP1-DPY)
      CALL PPENUP( XP1-DPX, YP1+DPY)
      CALL PPENDN( XP2-DPX, YP2+DPY)
      CALL PPENUP( XP2,YP2)
      ENDIF
      XPEN=XP2
      YPEN=YP2
      RETURN
      END
