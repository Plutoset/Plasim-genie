
      SUBROUTINE XVLIMT(UMAX,UMIN,VMAX,VMIN )
      REAL XL
      REAL XR
      REAL YB
      REAL YT
      REAL SIZ0
      REAL XRG
      REAL YRG
      REAL SIZ
      REAL UMAX
      REAL UMIN
      REAL VMAX
      REAL VMIN
      CHARACTER CH*80
      CALL XQMAP(XL,XR,YB,YT)
      CALL XQCHMG( SIZ0 )
      CALL XQRANG( XRG, YRG )
      CALL XQCHMG( SIZ0 )
      SIZ=0.04*MIN( XRG, YRG)
      CALL XCHMAG(SIZ)
      WRITE(CH,'('' Umax='',G9.3E2,'' Umin='',G9.3E2)')UMAX,UMIN
      WRITE(CH(31:60),'('' Wmax='',G9.3E2,'' Wmin='',G9.3E2)')VMAX,VMIN
      CALL XCHARL(XL,YT+0.03*(YT-YB),CH(31:60))
      CALL XCHARL(XL,YT+0.08*(YT-YB),CH(1:30) )
      CALL XCHMAG( SIZ0)
      RETURN
      END
