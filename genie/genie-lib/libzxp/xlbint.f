      SUBROUTINE XLBINT( NCLI)
C
      REAL XRG
      REAL YRG
      REAL DLABEL
      INTEGER NCLI
      INTEGER ICLI
      INTEGER KLBTYP
      REAL SIZLB
      REAL H1
      REAL HLABEL
      REAL H
      REAL WLABEL
      INTEGER LCLAB
      INTEGER LABROT
      INTEGER KROT
      INTEGER ICLON
      INTEGER KLBON
      INTEGER LCH
C
      CHARACTER CLABEL*20 ,CLABL*(*), LABEL*20, LBFORM*(*)
      COMMON /XLAB14/ DLABEL,WLABEL,HLABEL,SIZLB,KLBTYP,ICLI,ICLON
      COMMON /XLAB15/CLABEL
      COMMON /XLAB16/ LCLAB
      COMMON /XLBA33/ LABROT
      CALL XQRANG( XRG,YRG)
      DLABEL=XRG/NCLI
      ICLI=NCLI
      RETURN
      ENTRY XLBSIZ( H1 )
      KLBTYP=1
      SIZLB=H1
      RETURN
      ENTRY XLBMAG( H)
      KLBTYP=0
      HLABEL=H
      WLABEL= HLABEL*LCLAB*0.77
      RETURN
      ENTRY XLBROT(KROT)
      LABROT=KROT
      RETURN
      ENTRY XLBON
      ICLON=1
      RETURN
      ENTRY XLBOFF
      ICLON=0
      RETURN
      ENTRY XQLBON(KLBON)
      KLBON=ICLON
      RETURN
      ENTRY XLABEL( CLABL )
      CLABEL=CLABL
      LCLAB=   LEN ( CLABL )
      WLABEL=HLABEL*LCLAB*0.77
      RETURN
      ENTRY XQLABL( LABEL , LCH)
      LABEL= CLABEL
      LCH=LCLAB
      RETURN
      ENTRY XLBFM ( LBFORM )
      LBFORM=LBFORM
      RETURN
      END
