      SUBROUTINE HEXP1(SV,GV,NLS,ITYPE)

      IMPLICIT NONE
      
C     
C     Performs an indirect Legendre transform for a single-level field,
C     from spectral to Fourier space.
C     
C     The following types of Legendre function and thence types of
C     transform may be used:
C     ITYPE=1,2  :  ALP   :  ALPN(,,,1)   :  normal transform.
C     ITYPE=3,4  :  DALP  :  ALPN(,,,2)   :  y-derivative.
C     ITYPE=5,6  :  RLP   :  ALPN(,,,3)   :  del(-2).
C     ITYPE=7,8  :  RDLP  :  ALPN(,,,4)   :  y-derivative of del(-2).
C     An even/odd value of ITYPE denotes a spectral field of even/odd
C     symmetry.
C     
C     Version for RSGUP3.                     Mike Blackburn,  10.01.95.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      COMPLEX SV(IGA),GV(IGL),TEMP
      REAL ALPN(NWJ2,2,JGL,4)
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))

      INTEGER NLS,ITYPE,IALP,ISPAR,IGPAR,IA,IM,IP,M,N,IB

C     
 6900 FORMAT(/' ***ABORT : HEXP1 CALLED WITH INVALID TYPE =',I5)
 6910 FORMAT(/' ***ABORT : HEXP1 CALLED WITH NLS =',I3,' : MUST BE 1')
C     
C     Check this is a single-level call.
C     
      IF (NLS.NE.1) THEN
         WRITE(6,6910) NLS
         CALL ABORT
      ENDIF
C     
C     Use ITYPE to define transform type and symmetry labels.
C     ISPAR is symmetry of spectral field    = 0 for D,T,SP etc.
C     = 1 for Z.
C     IGPAR is symmetry of Fourier field: same as ISPAR unless transform
C     involves a d/dy.
C     
      IF (ITYPE.LE.0.OR.ITYPE.GE.9) THEN
         WRITE(6,6900) ITYPE
         CALL ABORT
      ENDIF
      IALP=(ITYPE+1)/2
      ISPAR=MOD(ITYPE,2)
      IGPAR=ISPAR
      IF (IALP.EQ.2.OR.IALP.EQ.4) IGPAR=1-ISPAR
C     
C     Perform inverse Legendre transform from spectral space to form
C     the even and odd contributions to the Fourier transforms.
C     Separate code for NHEM=1,2 to increase efficiency.
C     
      IF (NHEM.EQ.1) THEN
         IA=ISPAR+1
         IM=0
         IP=0
         DO M=0,MM-1,MOCT
            IM=IM+1
            DO N=M,NN-1,2
               IP=IP+1
               GV(IM)=GV(IM) + ALPN(IP,IA,JL,IALP)*SV(IP)
            END DO
         END DO
      ELSE
         IA=ISPAR+1
         IB=2-ISPAR
         IM=0
         IP=0
         DO M=0,MM-1,MOCT
            IM=IM+1
            DO N=M,NN-1,2
               IP=IP+1
               GV(IM    )=GV(IM    ) + ALPN(IP,IA,JL,IALP)*SV(IP     )
               GV(IM+IDL)=GV(IM+IDL) + ALPN(IP,IB,JL,IALP)*SV(IP+NWJ2)
            END DO
         END DO
      ENDIF
C     
C     For a global run, sum and difference even and odd contributions
C     to give the complete Fourier transforms at the northern and
C     southern latitude rows.  Separate code for each symmetry:
C     IGPAR=0 : even (IA) precedes odd (IB).
C     IGPAR=1 : odd (IA) precedes even (IB).
C     
      IF (NHEM.EQ.2) THEN
         IF (IGPAR.EQ.0) THEN
            DO IA=1,NWW
               IB=IA+IDL
               TEMP=GV(IA)
               GV(IA)=TEMP+GV(IB)
               GV(IB)=TEMP-GV(IB)
            END DO
         ELSE
            DO IA=1,NWW
               IB=IA+IDL
               TEMP=GV(IA)
               GV(IA)=GV(IB)+TEMP
               GV(IB)=GV(IB)-TEMP
            END DO
         ENDIF
      ENDIF
C     
      RETURN
      END
