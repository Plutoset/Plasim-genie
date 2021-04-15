      SUBROUTINE XCLIMT(FMAX,FMIN,FINC )
C 
      REAL XL
      REAL XR
      REAL YB
      REAL YT
      REAL XRG
      REAL YRG
      REAL SIZ0
      REAL SIZ
      REAL FMIN
      REAL FMAX
      REAL FINC
C
      CHARACTER CH*150
      CALL XQMAP(XL,XR,YB,YT)
      CALL XQRANG( XRG, YRG )
      CALL XQCHMG( SIZ0 )
      SIZ=0.04*MIN( XRG, YRG)
      CALL XCHMAG(SIZ)
      WRITE(CH,'(''Min='',G9.3E2,'' Max='',G9.3E2,
     :    '' Contour interval='',G9.3E2)')FMIN,FMAX,FINC
      CALL XCHARC( 0.5*(XL+XR), YT+0.03*(YT-YB),CH(1:54) )
      CALL XCHMAG( SIZ0)
      RETURN
      END
