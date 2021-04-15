      SUBROUTINE XVECTU(U,V,MD,M,ISTEP,N,JSTEP,XLENG,UUNIT)
C Assess ranges if U,V values, and set length XLENG at which the unit
C vector UUNIT is plotted in x-direction.
C The length of vector in y-direction is scaled according to mapping.
C (Notice the non-isotropicity.)
C XLENG was set as XSCALE/(M-1)*ISTEP where XSCALE is the horizontal
C scale of mapped area.
C UUNIT was set that the longest vector falls between 0.75*XLENG
C and 1.5*XLENG in length.
C
      INTEGER MD
      INTEGER N
      REAL UMAX
      REAL UMIN
      REAL VMAX
      REAL VMIN
      INTEGER J
      INTEGER JSTEP
      INTEGER I
      INTEGER M
      INTEGER ISTEP
      REAL UNIT
      REAL UUNIT
      INTEGER KVMODE
      REAL XL
      REAL XR
      REAL YB
      REAL YT
      REAL XLENG
      REAL VSC
      REAL VSC0
      INTEGER KVM
      INTEGER KARTYP
      REAL CSIZE
C
      REAL U(MD,N),V(MD,N)
      COMMON /XART36/ KARTYP,KVMODE,VSC
      SAVE UMAX, UMIN, VMAX, VMIN
      UMAX=U(1,1)
      UMIN=UMAX
      VMAX=V(1,1)
      VMIN=VMAX
      DO 5 J=1,N,JSTEP
      DO 5 I=1,M,ISTEP
      UMAX=MAX(UMAX,U(I,J))
      UMIN=MIN(UMIN,U(I,J))
      VMAX=MAX(VMAX,V(I,J))
 5      VMIN=MIN(VMIN,V(I,J))
      WRITE(6,'('' Umax='',G10.4E2,'' Umin='',G10.4E2,
     : '' Vmax='',G10.4E2,'' Vmin='',G10.4E2)')UMAX,UMIN,VMAX,VMIN
      IF (UMAX.EQ.UMIN.AND.VMAX.EQ.VMIN ) GO TO 500

      UNIT=UUNIT
      IF( KVMODE.EQ.2) GOTO 105

 25   IF( MAX( ABS(UMAX), ABS(UMIN) ).LT. UNIT*0.75 ) THEN
      UNIT=UNIT/2
      WRITE(6,100) UNIT
 100  FORMAT(' Max vector < 0.75*UNIT , UNIT is halved. UNIT='
     :   ,3F9.4)
      GO TO 25
      ENDIF

 30   IF( MAX( ABS(UMAX),ABS(UMIN) ).GT.UNIT*1.5  ) THEN
      UNIT=UNIT*2
      WRITE(6,200) UNIT
 200  FORMAT(' Max vector > 1.5 *UNIT ,UNIT is doubled. UNIT='
     :   ,3F9.4)
      GO TO 30
      ENDIF
 543  CONTINUE
 105  UUNIT=UNIT

 500  CALL XQMAP(XL,XR,YB,YT)
      XLENG=(XR-XL)/(M-1)* ISTEP*VSC

      RETURN
      ENTRY XVSCAL( VSC0)
      VSC=VSC0
      RETURN
      ENTRY XVMODE(KVM)
      KVMODE=KVM
      RETURN
      ENTRY XVLMT(  CSIZE)
C Call XVLIMT with UMAX,UMIN,VMAX,VMIN saved in XVECTU.
C CSIZE set the size of characters. Default is about 0.012.
C Default value is assumed if CSIZE is 0.0.
      CSIZE=CSIZE
      CALL XVLIMT(UMAX,UMIN,VMAX,VMIN)
      RETURN
      END
