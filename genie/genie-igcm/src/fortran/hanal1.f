      SUBROUTINE HANAL1(GV,GVW,SV,NLS,ITYPE)

      IMPLICIT NONE

C     
C     Performs a direct Legendre transform for a single-level field,
C     from Fourier to spectral space.
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
C     A Fourier work array GVW prevents corruption of the input Fourier
C     array GV that would otherwise occur in global runs.
C     
C     NOTE: The y-derivative transforms use integration by parts and
C     are valid only if the input field has zero zonal mean at
C     both poles.
C     
C     Version for RSGUP3.                     Mike Blackburn,  05.01.95.
C     Work array now a dummy argument (ANSI). Mike Blackburn,  04.09.96.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'polyno.cmn'
      COMPLEX GV(IGL),GVW(IGL),SV(IGA)
      REAL ALPN(NWJ2,2,JGL,4)
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))

      INTEGER NLS,ITYPE,IALP,ISPAR,IGPAR,IA,IB,IHEM,IP,IM,M,N
      REAL    AWT

C     
 6900 FORMAT(/' ***ABORT : HANAL1 CALLED WITH INVALID TYPE =',I5)
 6910 FORMAT(/' ***ABORT : HANAL1 CALLED WITH NLS =',I3,' : MUST BE 1')
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
      IF (ITYPE.LE.0.OR.ITYPE.GT.8) THEN
         WRITE(6,6900) ITYPE
         CALL ABORT
      ENDIF
      IALP=(ITYPE+1)/2
      ISPAR=MOD(ITYPE,2)
      IGPAR=ISPAR
      IF (IALP.EQ.2.OR.IALP.EQ.4) IGPAR=1-ISPAR
C     
C     For a global run, sum and difference the complete Fourier
C     transforms at the northern and southern latitude rows to give
C     the even and odd contributions.
C     Separate code for each symmetry:
C     IGPAR=0 : even (IA) to precede odd (IB).
C     IGPAR=1 : odd (IA) to precede even (IB).
C     
      IF (NHEM.EQ.2) THEN
         IF (IGPAR.EQ.0) THEN
            DO IA=1,NWW
               IB=IA+IDL
               GVW(IA)=0.5*(GV(IA)+GV(IB))
               GVW(IB)=0.5*(GV(IA)-GV(IB))
            END DO
         ELSE
            DO IA=1,NWW
               IB=IA+IDL
               GVW(IA)=0.5*(GV(IA)-GV(IB))
               GVW(IB)=0.5*(GV(IA)+GV(IB))
            END DO
         ENDIF
      ENDIF
C     
C     Set up the appropriate Gaussian weight for the current latitude,
C     dependent on transform type.
C     Assumes JH in /LEGAU/ contains latitude counter from calling loop.
C     
      IF (IALP.EQ.1) AWT=AW(JH)*CSSQ(JH)
      IF (IALP.EQ.2) AWT=-AW(JH)
      IF (IALP.EQ.3) AWT=AW(JH)*CSSQ(JH)
      IF (IALP.EQ.4) AWT=-AW(JH)
C     
C     Calculate POLY array in vector loop before main transform.
C     
      DO IHEM=1,NHEM
         IA=(ISPAR+1)*(2-IHEM) + (2-ISPAR)*(IHEM-1)
         DO IP=1,NWJ2
            POLY(IP,IHEM,IALP)=AWT*ALPN(IP,IA,JL,IALP)
         END DO
      END DO
C     
C     Perform direct Legendre transform from the even and odd
C     parts of the Fourier transforms to spectral space.
C     Separate code for NHEM=1,2 to increase efficiency.
C     
      IF (NHEM.EQ.1) THEN
         IM=0
         IP=0
         DO M=0,MM-1,MOCT
            IM=IM+1
            DO N=M,NN-1,2
               IP=IP+1
               SV(IP)=SV(IP) + POLY(IP,1,IALP)*GV(IM)
            END DO
         END DO
      ELSE
         IM=0
         IP=0
         DO M=0,MM-1,MOCT
            IM=IM+1
            DO N=M,NN-1,2
               IP=IP+1
               SV(IP     )=SV(IP     ) + POLY(IP,1,IALP)*GVW(IM    )
               SV(IP+NWJ2)=SV(IP+NWJ2) + POLY(IP,2,IALP)*GVW(IM+IDL)
            END DO
         END DO
      ENDIF
C     
      RETURN
      END
