C
      SUBROUTINE XCMIXL
C
      INTEGER LCPTN
      INTEGER LABTYP
      INTEGER LTYPE
      INTEGER ICLF
      INTEGER NCLF
      INTEGER LHILIT
      INTEGER KHILIT
      INTEGER IHLF
      INTEGER NHLF
      INTEGER KCT0
      INTEGER KCZERO
      REAL CLREF
C
      COMMON/XCRF17/CLREF,LCPTN,LABTYP,ICLF,LHILIT,IHLF,KCT0
C Contour plotting pattern is set so that lines are dash,dotted,solid
C for negative ,zero, positve values respectively. This is default.
      LCPTN=0
      RETURN
      ENTRY XCFULL
      LCPTN=1
      RETURN
      ENTRY XCDASH
C Set contour plotting pattern as dash   lines.
      LCPTN=2
      RETURN
      ENTRY XCDOT
      LCPTN=3
      RETURN
      ENTRY XCLTYP( LTYPE)
C Define type of labels on contours.
C LTYPE-- parameter controling contour labeling.
C LTYPE <0,  label is specified by user through XLABEL,
C       =0,  no labeling is done.
C       =1,  label the contour number,  number=0 for zero contour.
C       =2,  label the contour values.
C By default LTYPE=2.
C Note setting LTYPE=0 is the only way to suppress labels outside
C routine XCONTA as XLBON and XLBOFF are called inside XCONTA.
      LABTYP=LTYPE
      RETURN
      ENTRY XCLFRQ( NCLF)
C Set contour labeling frequency so that every NCLFth contour relative
C to reference contour is labeled. Default NCLF=2.
      ICLF=NCLF
      RETURN
      ENTRY XHILIT( KHILIT )
      LHILIT=KHILIT
      RETURN
      ENTRY XHLFRQ( NHLF )
      IHLF=NHLF
      RETURN
      ENTRY XCZERO( KCZERO )
C Option of zero contour plotting.
C KCZERO=0, zero line is suppressed, by default KCZERO=1.
      KCT0=KCZERO
      RETURN
      END
