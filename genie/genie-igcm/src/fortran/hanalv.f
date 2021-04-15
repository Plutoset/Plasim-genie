      SUBROUTINE HANALV(SPA,VP,DTE,TT,QT,DT,ZT,SPG,VPG,EG
     *     ,TNLG,QNLG,FUG,FVG,VTG,VQG,FVGT,FUGT)

      IMPLICIT NONE

C     
C     Perform all the direct Legendre transforms for the adiabatic
C     part of the timestep at the current latitude (pair), in place
C     of separate calls to HANAL for individual transform types.
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
C     Maximum vector efficiency is achieved by chaining all multi-level
C     transforms in one loop and by chaining all single-level transforms
C     in a second loop.
C     
C     All dummy argument arrays are declared complex.
C     All array dimensions are parameters.
C     Multi-level arrays are 3-dimensional.
C     
C     Tracers (QNLG,QT) are included conditionally inside inner loops.
C     This does *not* affect vectorisation, since the number of tracers,
C     NTRAC, is a parameter and the relevant IF constructs are removed
C     at compilation.  The same applies to code conditional on NHEM.
C     (Only tested on Cray J90 with cf77).
C     
C     NOTE: The y-derivative transforms use integration by parts and
C     are valid only if the input field has zero zonal mean at
C     both poles.
C     
C     NOTE: *** THE INPUT FOURIER FIELDS ARE MODIFIED IF GLOBAL ***
C     
C     Version for RSGUP3.                     Mike Blackburn,  12.01.95.
C     Include tracers for IGCM2.              Piers Forster,   ??.04.97.
C     Conditional separate loops for tracers. Mike Blackburn,  04.09.98.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'polyno.cmn'
C     
      COMPLEX     SPA(NWJ2,NHEM),VP(NWJ2,NHEM),DTE(NWJ2,NHEM,NL)
     *     ,TT(NWJ2,NHEM,NL),DT(NWJ2,NHEM,NL),ZT(NWJ2,NHEM,NL)
     *     ,QT(NWJ2,NHEM,NL*NTRAC)
      COMPLEX     SPG(IDL,NHEM),VPG(IDL,NHEM),EG(IDL,NHEM,NL)
     *     ,TNLG(IDL,NHEM,NL),FUG(IDL,NHEM,NL),FVG(IDL,NHEM,NL)
     *     ,VTG(IDL,NHEM,NL),FVGT(IDL,NHEM,NL),FUGT(IDL,NHEM,NL)
     *     ,QNLG(IDL,NHEM,NL*NTRAC),VQG(IDL,NHEM,NL*NTRAC)
      COMPLEX     TEMP
      REAL        ALPN(NWJ2,2,JGL,4)
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))

      INTEGER IM,L,IHEM,IP,M,N,IPM
      REAL    AW1256,AW3478

C     
C     For a global run, sum and difference the complete Fourier
C     transforms at the northern and southern latitude rows to give
C     the even and odd contributions : E=(N+S)/2, O=(N-S)/2.
C     For Fourier fields symmetric about equator  : even precedes odd.
C     For Fourier fields asymmetric about equator : odd precedes even.
C     
      IF (NHEM.EQ.2) THEN
C     
C     Single-level fields.
C     
         DO IM=1,NWW
C     Surface pressure : symmetric.
            TEMP=SPG(IM,1)
            SPG(IM,1)=0.5*(TEMP+SPG(IM,2))
            SPG(IM,2)=0.5*(TEMP-SPG(IM,2))
C     Surface pressure tendency : symmetric.
            TEMP=VPG(IM,1)
            VPG(IM,1)=0.5*(TEMP+VPG(IM,2))
            VPG(IM,2)=0.5*(TEMP-VPG(IM,2))
         END DO
C     
C     Multi-level fields.
C     
         DO IM=1,NWW
            DO L=1,NL
C     Divergence tendency : energy term : symmetric.
               TEMP=EG(IM,1,L)
               EG(IM,1,L)=0.5*(TEMP+EG(IM,2,L))
               EG(IM,2,L)=0.5*(TEMP-EG(IM,2,L))
C     Temperature tendency : main + d/dx part : symmetric.
               TEMP=TNLG(IM,1,L)
               TNLG(IM,1,L)=0.5*(TEMP+TNLG(IM,2,L))
               TNLG(IM,2,L)=0.5*(TEMP-TNLG(IM,2,L))
               IF (NTRAC.EQ.1) THEN
C     Tracer tendency : main + d/dx part : symmetric.
                  TEMP=QNLG(IM,1,L)
                  QNLG(IM,1,L)=0.5*(TEMP+QNLG(IM,2,L))
                  QNLG(IM,2,L)=0.5*(TEMP-QNLG(IM,2,L))
               ENDIF
C     Divergence tendency : d/dx part : symmetric.
               TEMP=FUG(IM,1,L)
               FUG(IM,1,L)=0.5*(TEMP+FUG(IM,2,L))
               FUG(IM,2,L)=0.5*(TEMP-FUG(IM,2,L))
C     Vorticity tendency : d/dx part : anti-symmetric.
               TEMP=FVG(IM,1,L)
               FVG(IM,1,L)=0.5*(TEMP-FVG(IM,2,L))
               FVG(IM,2,L)=0.5*(TEMP+FVG(IM,2,L))
C     Temperature tendency : d/dy part : anti-symmetric.
               TEMP=VTG(IM,1,L)
               VTG(IM,1,L)=0.5*(TEMP-VTG(IM,2,L))
               VTG(IM,2,L)=0.5*(TEMP+VTG(IM,2,L))
               IF (NTRAC.EQ.1) THEN
C     Tracer tendency : d/dy part : anti-symmetric.
                  TEMP=VQG(IM,1,L)
                  VQG(IM,1,L)=0.5*(TEMP-VQG(IM,2,L))
                  VQG(IM,2,L)=0.5*(TEMP+VQG(IM,2,L))
               ENDIF
C     Divergence tendency : d/dy part : anti-symmetric.
               TEMP=FVGT(IM,1,L)
               FVGT(IM,1,L)=0.5*(TEMP-FVGT(IM,2,L))
               FVGT(IM,2,L)=0.5*(TEMP+FVGT(IM,2,L))
C     Vorticity tendency : d/dy part : symmetric.
               TEMP=FUGT(IM,1,L)
               FUGT(IM,1,L)=0.5*(TEMP+FUGT(IM,2,L))
               FUGT(IM,2,L)=0.5*(TEMP-FUGT(IM,2,L))
            END DO
         END DO
C     
C     Tracers, treated as a single (NL*NTRAC)-level field.
C     
         IF (NTRAC.GT.1) THEN
            DO IM=1,NWW
               DO L=1,NL*NTRAC
C     Tracer tendency : main + d/dx part : symmetric.
                  TEMP=QNLG(IM,1,L)
                  QNLG(IM,1,L)=0.5*(TEMP+QNLG(IM,2,L))
                  QNLG(IM,2,L)=0.5*(TEMP-QNLG(IM,2,L))
C     Tracer tendency : d/dy part : anti-symmetric.
                  TEMP=VQG(IM,1,L)
                  VQG(IM,1,L)=0.5*(TEMP-VQG(IM,2,L))
                  VQG(IM,2,L)=0.5*(TEMP+VQG(IM,2,L))
               END DO
            END DO
         ENDIF
C     
      ENDIF
C     
C     Set up the appropriate Gaussian weight for the current latitude,
C     dependent on transform type.
C     Assumes JH in /LEGAU/ contains latitude counter from calling loop.
C     
      AW1256=AW(JH)*CSSQ(JH)
      AW3478=-AW(JH)
C     
C     Calculate POLY array in a vector loop before the main transforms,
C     for the required Legendre Function types.
C     Both even and odd functions are required, irrespective of NHEM.
C     Second subscript of ALPN denotes odd or even subset of Legendre
C     functions, and depends of symmetry of spectral field.
C     Fourth subscript of ALPN is Legendre function type, (ITYPE+1)/2.
C     
      DO IHEM=1,2
         DO IP=1,NWJ2
            POLY(IP,IHEM,1)=AW1256*ALPN(IP,IHEM,JL,1)
            POLY(IP,IHEM,2)=AW3478*ALPN(IP,IHEM,JL,2)
         END DO
      END DO
C     
C     Transform single-level fields.
C     Vectorisation is over total wavenumber for each zonal wavenumber.
C     
      IM=0
      IP=0
      DO M=0,MM-1,MOCT
         IM=IM+1
         DO N=M,NN-1,2
            IP=IP+1
C     Surface pressure          : type 2.
            SPA(IP,1)=SPA(IP,1) + POLY(IP,1,1)*SPG(IM,1)
C     Surface pressure tendency : type 2.
            VP (IP,1)=VP (IP,1) + POLY(IP,1,1)*VPG(IM,1)
            IF (NHEM.EQ.2) THEN
C     Surface pressure          : type 2.
               SPA(IP,2)=SPA(IP,2) + POLY(IP,2,1)*SPG(IM,2)
C     Surface pressure tendency : type 2.
               VP (IP,2)=VP (IP,2) + POLY(IP,2,1)*VPG(IM,2)
            ENDIF
         END DO
      END DO
C     
C     Transform multi-level fields.
C     Inner loop vectorisation is over total wavenumber, to access
C     spectral memory sequentially, avoiding skip distances being a
C     multiple of 8 (which causes memory bank conflicts on Cray vector
C     machines).
C     
      IM=0
      IP=0
      DO M=0,MM-1,MOCT
         IM=IM+1
         IPM=IP
         DO L=1,NL
            IP=IPM
            DO N=M,NN-1,2
               IP=IP+1
C     Divergence tendency  : energy term      : type 2.
               DTE(IP,1,L)=DTE(IP,1,L) + POLY(IP,1,1)*EG  (IM,1,L)
C     Temperature tendency : main + d/dx part : type 2.
C     Temperature tendency : d/dy part        : type 4.
               TT (IP,1,L)=TT (IP,1,L) + POLY(IP,1,1)*TNLG(IM,1,L)
     *              + POLY(IP,1,2)*VTG (IM,1,L)
               IF (NTRAC.EQ.1) THEN
C     Tracer tendency   : main + d/dx part : type 2.
C     Tracer tendency   : d/dy part        : type 4.
                  QT(IP,1,L)=QT(IP,1,L) +POLY(IP,1,1)*QNLG(IM,1,L)
     *                 +POLY(IP,1,2)*VQG (IM,1,L)
               ENDIF
C     Divergence tendency  : d/dx part        : type 2.
C     Divergence tendency  : d/dy part        : type 4.
               DT (IP,1,L)=DT (IP,1,L) + POLY(IP,1,1)*FUG (IM,1,L)
     *              + POLY(IP,1,2)*FVGT(IM,1,L)
C     Vorticity tendency   : d/dx part        : type 1.
C     Vorticity tendency   : d/dy part        : type 3.
               ZT (IP,1,L)=ZT (IP,1,L) + POLY(IP,2,1)*FVG (IM,1,L)
     *              + POLY(IP,2,2)*FUGT(IM,1,L)
               IF (NHEM.EQ.2) THEN
C     Divergence tendency  : energy term      : type 2.
                  DTE(IP,2,L)=DTE(IP,2,L) + POLY(IP,2,1)*EG  (IM,2,L)
C     Temperature tendency : main + d/dx part : type 2.
C     Temperature tendency : d/dy part        : type 4.
                  TT (IP,2,L)=TT (IP,2,L) + POLY(IP,2,1)*TNLG(IM,2,L)
     *                 + POLY(IP,2,2)*VTG (IM,2,L)
                  IF (NTRAC.EQ.1) THEN
C     Tracer tendency   : d/dy part        : type 4.
                     QT(IP,2,L)=QT(IP,2,L) +POLY(IP,2,1)*QNLG(IM,2,L)
     *                    +POLY(IP,2,2)*VQG (IM,2,L)
                  ENDIF
C     Divergence tendency  : d/dx part        : type 2.
C     Divergence tendency  : d/dy part        : type 4.
                  DT (IP,2,L)=DT (IP,2,L) + POLY(IP,2,1)*FUG (IM,2,L)
     *                 + POLY(IP,2,2)*FVGT(IM,2,L)
C     Vorticity tendency   : d/dx part        : type 1.
C     Vorticity tendency   : d/dy part        : type 3.
                  ZT (IP,2,L)=ZT (IP,2,L) + POLY(IP,1,1)*FVG (IM,2,L)
     *                 + POLY(IP,1,2)*FUGT(IM,2,L)
               ENDIF
            END DO
         END DO
      END DO
C     
C     Transform tracers, treated as a single (NL*NTRAC)-level field.
C     
      IF (NTRAC.GT.1) THEN
         IM=0
         IP=0
         DO M=0,MM-1,MOCT
            IM=IM+1
            IPM=IP
            DO L=1,NL*NTRAC
               IP=IPM
               DO N=M,NN-1,2
                  IP=IP+1
C     Tracer tendency   : main + d/dx part : type 2.
C     Tracer tendency   : d/dy part        : type 4.
                  QT(IP,1,L)=QT(IP,1,L)+POLY(IP,1,1)*QNLG(IM,1,L)
     *                 +POLY(IP,1,2)*VQG (IM,1,L)
                  IF (NHEM.EQ.2) THEN
                     QT(IP,2,L)=QT(IP,2,L)+POLY(IP,2,1)*QNLG(IM,2,L)
     *                    +POLY(IP,2,2)*VQG (IM,2,L)
                  ENDIF
               END DO
            END DO
         END DO
      ENDIF
C     
      RETURN
      END
