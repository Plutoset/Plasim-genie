

C***********************************************************************
C*                                                                     *
C*                         SUBROUTINE GASSEARCH                        *
C*                                                                     *
C***********************************************************************

      SUBROUTINE GASSEARCH(IGAS,IBND,UPATH,PEFF,TEFF,AH2O,BH2O,CH2O,
     $                     DH2O,AGAS,BGAS,CGAS,DGAS,AN03,BN03,CN03,
     $                     DN03,AC02,BC02,CC02,DC02,AC06,BC06,CC06,
     $                     DC06,AC07,BC07,CC07,DC07,TRANS)

C Subroutine GASSEARCH calculates the transmittane of a homogeneous
C atmospheric path, using the data of pre-computed tables stored
C in 'gastab'
C
C INPUT:  Gas index (IGAS)
C         Band index (IBND)
C         Absorber amount (UPATH)
C         Pressure (PEFF)
C         Temperature (TEFF)
C         Pre-computed tables (AH2O,BH2O,CH2O,DH2O,AGAS,BGAS,CGAS,DGAS,
C                              AN03,BN03,CN03,DN03,AC02,BC02,CC02,DC02,
C                              AC06,BC06,CC06,DC06,AC07,BC07,CC07,DC07)
C
C OUTPUT: Transmittance (TRANS)

      IMPLICIT NONE
#include "parray.cmn"

C-----------------
C Input Variables
C-----------------

      INTEGER IGAS,IBND
      REAL UPATH,PEFF,TEFF

      REAL AH2O(MXBAND,48,28),AGAS(MXBAND-1,48,28)
      REAL BH2O(MXBAND,48,28),BGAS(MXBAND-1,48,28)
      REAL CH2O(MXBAND,48,28),CGAS(MXBAND-1,48,28)
      REAL DH2O(MXBAND,48,28),DGAS(MXBAND-1,48,28)
      REAL AN03(48,28),BN03(48,28),CN03(48,28),DN03(48,28)
      REAL AC02(48,28),BC02(48,28),CC02(48,28),DC02(48,28)
      REAL AC06(48,28),BC06(48,28),CC06(48,28),DC06(48,28)
      REAL AC07(48,28),BC07(48,28),CC07(48,28),DC07(48,28)

C-----------------
C Output Variable
C-----------------

      REAL TRANS

C--------------------
C Internal Variables
C--------------------

      INTEGER IABS,IPRES,NABS,NPRES
      PARAMETER(NABS=48,NPRES=28)
      REAL USTR(NABS),PSTR(NPRES)  ! Stored values of absorber
                                     ! amount and pressure

      REAL FU,FFU,FP,FFP          ! Variables
      REAL A0,A1,A2,B0,B1,B2      ! used for
      REAL C0,C1,C2,D0,D1,D2      ! interpolation

C Stored values of absorber amount (kg m-2)

      DATA(USTR(IABS),IABS=1,NABS)/  1.0D-08, 1.0D-07, 5.0D-07,
     $                               1.0D-06, 2.0D-06, 4.0D-06,
     $                               6.0D-06, 8.0D-06, 1.0D-05,
     $                               2.0D-05, 4.0D-05, 6.0D-05,
     $                               8.0D-05, 1.0D-04, 2.0D-04,
     $                               4.0D-04, 6.0D-04, 8.0D-04,
     $                               1.0D-03, 2.0D-03, 4.0D-03,
     $                               6.0D-03, 8.0D-03, 1.0D-02,
     $                               2.0D-02, 4.0D-02, 6.0D-02,
     $                               8.0D-02, 1.0D-01, 2.0D-01,
     $                               4.0D-01, 6.0D-01, 8.0D-01,
     $                               1.0, 2.0, 4.0, 6.0, 8.0,
     $                               1.0D+01, 2.0D+01, 4.0D+01,
     $                               6.0D+01, 8.0D+01, 1.0D+02,
     $                               2.0D+02, 4.0D+02, 6.0D+02,
     $                               8.0D+02/

C Stored values of pressure (Pa)

      DATA(PSTR(IPRES),IPRES=1,NPRES)/ 1.0, 1.0D+01, 2.0D+01, 4.0D+01,
     $                                 6.0D+01, 8.0D+01, 1.0D+02,
     $                                 2.0D+02, 4.0D+02, 6.0D+02,
     $                                 8.0D+02, 1.0D+03, 2.0D+03,
     $                                 4.0D+03, 6.0D+03, 8.0D+03,
     $                                 1.0D+04, 2.0D+04, 3.0D+04,
     $                                 4.0D+04, 5.0D+04, 6.0D+04,
     $                                 7.0D+04, 8.0D+04, 9.0D+04,
     $                                 1.0D+05, 1.1D+05, 1.2D+05/


      FU=99.
      FP=99.

C Find the table index for the absorber amount

      IABS=INT(NABS/2.)

      IF (UPATH.NE.USTR(IABS)) THEN

         IF (UPATH.LT.USTR(IABS)) THEN

            IABS=INT(IABS/2.)

            IF (UPATH.LT.USTR(IABS)) THEN
               IABS=1
               IF (UPATH.GT.USTR(1)) THEN
                  DO WHILE(UPATH.GT.USTR(IABS))
                     IABS=IABS+1
                  END DO
               END IF
               IF (UPATH.LE.USTR(1)) THEN
                  IABS=2
                  FFU=0.
                  FU=1.
               END IF
            ELSE
               IF (UPATH.NE.USTR(IABS)) THEN
                  DO WHILE(UPATH.GT.USTR(IABS))
                     IABS=IABS+1
                  END DO
               END IF
            END IF

         ELSE

            IABS=IABS+INT(NABS/4.)

            IF (UPATH.GT.USTR(IABS)) THEN
               IF (UPATH.GE.USTR(NABS)) THEN
                  IABS=NABS
                  FFU=1.
                  FU=0.
               ELSE
                  DO WHILE(UPATH.GT.USTR(IABS))
                     IABS=IABS+1
                  END DO
               END IF
            ELSE
               IF (UPATH.NE.USTR(IABS)) THEN
                  IABS=INT(NABS/2.)
                  DO WHILE(UPATH.GT.USTR(IABS))
                     IABS=IABS+1
                  END DO
               END IF
            END IF

         END IF

      END IF


C Find the table index for the pressure

      IPRES=INT(NPRES/2.)

      IF (PEFF.NE.PSTR(IPRES)) THEN

         IF (PEFF.LT.PSTR(IPRES)) THEN

            IPRES=INT(IPRES/2.)

            IF (PEFF.LE.PSTR(IPRES)) THEN
               IPRES=2
               IF (PEFF.GT.PSTR(1)) THEN
                  DO WHILE(PEFF.GT.PSTR(IPRES))
                     IPRES=IPRES+1
                  END DO
               END IF
               IF (PEFF.LE.PSTR(1)) THEN
                  FFP=0.
                  FP=1.
               END IF
            ELSE
               IF (PEFF.NE.PSTR(IPRES)) THEN
                  DO WHILE(PEFF.GT.PSTR(IPRES))
                     IPRES=IPRES+1
                  END DO
               END IF
            END IF

         ELSE

            IPRES=IPRES+INT(NPRES/4.)

            IF (PEFF.GT.PSTR(IPRES)) THEN
               IF (PEFF.GE.PSTR(NPRES)) THEN
                  IPRES=NPRES
                  FFP=1.
                  FP=0.
               ELSE
                  DO WHILE(PEFF.GT.PSTR(IPRES))
                     IPRES=IPRES+1
                  END DO
               END IF
            ELSE
               IF (PEFF.NE.PSTR(IPRES)) THEN
                  IPRES=INT(NPRES/2.)
                  DO WHILE(PEFF.GT.PSTR(IPRES))
                     IPRES=IPRES+1
                  END DO
               END IF
            END IF

         END IF

      END IF

C Transmittance calculation

      TEFF=TEFF-250.0

C----------------------
C Interpolation scheme
C----------------------

      IF ((FU.GT.98.).AND.(FP.GT.98)) THEN
         FU=(USTR(IABS)-UPATH)/(USTR(IABS)-USTR(IABS-1))
         FP=(PSTR(IPRES)-PEFF)/(PSTR(IPRES)-PSTR(IPRES-1))
         FFU=1.-FU
         FFP=1.-FP
      END IF

      IF (IGAS.EQ.1) THEN

         A1=AH2O(IBND,IABS,IPRES)*FFP+
     $      AH2O(IBND,IABS,IPRES-1)*FP
         A2=AH2O(IBND,IABS-1,IPRES)*FFP+
     $      AH2O(IBND,IABS-1,IPRES-1)*FP
         A0=A1*FFU+A2*FU

         B1=BH2O(IBND,IABS,IPRES)*FFP+
     $      BH2O(IBND,IABS,IPRES-1)*FP
         B2=BH2O(IBND,IABS-1,IPRES)*FFP+
     $      BH2O(IBND,IABS-1,IPRES-1)*FP
         B0=B1*FFU+B2*FU

         C1=CH2O(IBND,IABS,IPRES)*FFP+
     $      CH2O(IBND,IABS,IPRES-1)*FP
         C2=CH2O(IBND,IABS-1,IPRES)*FFP+
     $      CH2O(IBND,IABS-1,IPRES-1)*FP
         C0=C1*FFU+C2*FU

         D1=DH2O(IBND,IABS,IPRES)*FFP+
     $      DH2O(IBND,IABS,IPRES-1)*FP
         D2=DH2O(IBND,IABS-1,IPRES)*FFP+
     $      DH2O(IBND,IABS-1,IPRES-1)*FP
         D0=D1*FFU+D2*FU

         TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
         GOTO 1976

      END IF

      IF (IGAS.EQ.2) THEN

         IF (IBND.EQ.1) THEN
            A1=AGAS(IBND,IABS,IPRES)*FFP+
     $         AGAS(IBND,IABS,IPRES-1)*FP
            A2=AGAS(IBND,IABS-1,IPRES)*FFP+
     $         AGAS(IBND,IABS-1,IPRES-1)*FP
            A0=A1*FFU+A2*FU

            B1=BGAS(IBND,IABS,IPRES)*FFP+
     $         BGAS(IBND,IABS,IPRES-1)*FP
            B2=BGAS(IBND,IABS-1,IPRES)*FFP+
     $         BGAS(IBND,IABS-1,IPRES-1)*FP
            B0=B1*FFU+B2*FU

            C1=CGAS(IBND,IABS,IPRES)*FFP+
     $         CGAS(IBND,IABS,IPRES-1)*FP
            C2=CGAS(IBND,IABS-1,IPRES)*FFP+
     $         CGAS(IBND,IABS-1,IPRES-1)*FP
            C0=C1*FFU+C2*FU

            D1=DGAS(IBND,IABS,IPRES)*FFP+
     $         DGAS(IBND,IABS,IPRES-1)*FP
            D2=DGAS(IBND,IABS-1,IPRES)*FFP+
     $         DGAS(IBND,IABS-1,IPRES-1)*FP
            D0=D1*FFU+D2*FU

            TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
            GOTO 1976
         END IF

         IF (IBND.EQ.6) THEN
            A1=AC06(IABS,IPRES)*FFP+AC06(IABS,IPRES-1)*FP
            A2=AC06(IABS-1,IPRES)*FFP+AC06(IABS-1,IPRES-1)*FP
            A0=A1*FFU+A2*FU

            B1=BC06(IABS,IPRES)*FFP+BC06(IABS,IPRES-1)*FP
            B2=BC06(IABS-1,IPRES)*FFP+BC06(IABS-1,IPRES-1)*FP
            B0=B1*FFU+B2*FU

            C1=CC06(IABS,IPRES)*FFP+CC06(IABS,IPRES-1)*FP
            C2=CC06(IABS-1,IPRES)*FFP+CC06(IABS-1,IPRES-1)*FP
            C0=C1*FFU+C2*FU

            D1=DC06(IABS,IPRES)*FFP+DC06(IABS,IPRES-1)*FP
            D2=DC06(IABS-1,IPRES)*FFP+DC06(IABS-1,IPRES-1)*FP
            D0=D1*FFU+D2*FU

            TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
            GOTO 1976
         END IF

         IF (IBND.EQ.7) THEN
            A1=AC07(IABS,IPRES)*FFP+AC07(IABS,IPRES-1)*FP
            A2=AC07(IABS-1,IPRES)*FFP+AC07(IABS-1,IPRES-1)*FP
            A0=A1*FFU+A2*FU

            B1=BC07(IABS,IPRES)*FFP+BC07(IABS,IPRES-1)*FP
            B2=BC07(IABS-1,IPRES)*FFP+BC07(IABS-1,IPRES-1)*FP
            B0=B1*FFU+B2*FU

            C1=CC07(IABS,IPRES)*FFP+CC07(IABS,IPRES-1)*FP
            C2=CC07(IABS-1,IPRES)*FFP+CC07(IABS-1,IPRES-1)*FP
            C0=C1*FFU+C2*FU

            D1=DC07(IABS,IPRES)*FFP+DC07(IABS,IPRES-1)*FP
            D2=DC07(IABS-1,IPRES)*FFP+DC07(IABS-1,IPRES-1)*FP
            D0=D1*FFU+D2*FU

            TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
            GOTO 1976
         END IF

      END IF

      IF ((IGAS.EQ.3).OR.(IGAS.EQ.4).OR.
     $    (IGAS.EQ.7).OR.(IBND.EQ.8)) THEN

         A1=AGAS(IBND,IABS,IPRES)*FFP+
     $      AGAS(IBND,IABS,IPRES-1)*FP
         A2=AGAS(IBND,IABS-1,IPRES)*FFP+
     $      AGAS(IBND,IABS-1,IPRES-1)*FP
         A0=A1*FFU+A2*FU

         B1=BGAS(IBND,IABS,IPRES)*FFP+
     $      BGAS(IBND,IABS,IPRES-1)*FP
         B2=BGAS(IBND,IABS-1,IPRES)*FFP+
     $      BGAS(IBND,IABS-1,IPRES-1)*FP
         B0=B1*FFU+B2*FU

         C1=CGAS(IBND,IABS,IPRES)*FFP+
     $      CGAS(IBND,IABS,IPRES-1)*FP
         C2=CGAS(IBND,IABS-1,IPRES)*FFP+
     $      CGAS(IBND,IABS-1,IPRES-1)*FP
         C0=C1*FFU+C2*FU

         D1=DGAS(IBND,IABS,IPRES)*FFP+
     $      DGAS(IBND,IABS,IPRES-1)*FP
         D2=DGAS(IBND,IABS-1,IPRES)*FFP+
     $      DGAS(IBND,IABS-1,IPRES-1)*FP
         D0=D1*FFU+D2*FU

         TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
         GOTO 1976

      END IF

      IF (IGAS.EQ.5) THEN

         IF ((IBND.EQ.4).OR.(IBND.EQ.7)) THEN
            A1=AGAS(IBND,IABS,IPRES)*FFP+
     $         AGAS(IBND,IABS,IPRES-1)*FP
            A2=AGAS(IBND,IABS-1,IPRES)*FFP+
     $         AGAS(IBND,IABS-1,IPRES-1)*FP
            A0=A1*FFU+A2*FU

            B1=BGAS(IBND,IABS,IPRES)*FFP+
     $         BGAS(IBND,IABS,IPRES-1)*FP
            B2=BGAS(IBND,IABS-1,IPRES)*FFP+
     $         BGAS(IBND,IABS-1,IPRES-1)*FP
            B0=B1*FFU+B2*FU

            C1=CGAS(IBND,IABS,IPRES)*FFP+
     $         CGAS(IBND,IABS,IPRES-1)*FP
            C2=CGAS(IBND,IABS-1,IPRES)*FFP+
     $         CGAS(IBND,IABS-1,IPRES-1)*FP
            C0=C1*FFU+C2*FU

            D1=DGAS(IBND,IABS,IPRES)*FFP+
     $         DGAS(IBND,IABS,IPRES-1)*FP
            D2=DGAS(IBND,IABS-1,IPRES)*FFP+
     $         DGAS(IBND,IABS-1,IPRES-1)*FP
            D0=D1*FFU+D2*FU

            TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
            GOTO 1976
         END IF

         IF (IBND.EQ.3) THEN
            A1=AN03(IABS,IPRES)*FFP+AN03(IABS,IPRES-1)*FP
            A2=AN03(IABS-1,IPRES)*FFP+AN03(IABS-1,IPRES-1)*FP
            A0=A1*FFU+A2*FU

            B1=BN03(IABS,IPRES)*FFP+BN03(IABS,IPRES-1)*FP
            B2=BN03(IABS-1,IPRES)*FFP+BN03(IABS-1,IPRES-1)*FP
            B0=B1*FFU+B2*FU

            C1=CN03(IABS,IPRES)*FFP+CN03(IABS,IPRES-1)*FP
            C2=CN03(IABS-1,IPRES)*FFP+CN03(IABS-1,IPRES-1)*FP
            C0=C1*FFU+C2*FU

            D1=DN03(IABS,IPRES)*FFP+DN03(IABS,IPRES-1)*FP
            D2=DN03(IABS-1,IPRES)*FFP+DN03(IABS-1,IPRES-1)*FP
            D0=D1*FFU+D2*FU

            TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
            GOTO 1976
         END IF

      END IF

      IF (IGAS.EQ.6) THEN

         IF (IBND.EQ.5) THEN
            A1=AGAS(IBND,IABS,IPRES)*FFP+
     $         AGAS(IBND,IABS,IPRES-1)*FP
            A2=AGAS(IBND,IABS-1,IPRES)*FFP+
     $         AGAS(IBND,IABS-1,IPRES-1)*FP
            A0=A1*FFU+A2*FU

            B1=BGAS(IBND,IABS,IPRES)*FFP+
     $         BGAS(IBND,IABS,IPRES-1)*FP
            B2=BGAS(IBND,IABS-1,IPRES)*FFP+
     $         BGAS(IBND,IABS-1,IPRES-1)*FP
            B0=B1*FFU+B2*FU

            C1=CGAS(IBND,IABS,IPRES)*FFP+
     $         CGAS(IBND,IABS,IPRES-1)*FP
            C2=CGAS(IBND,IABS-1,IPRES)*FFP+
     $         CGAS(IBND,IABS-1,IPRES-1)*FP
            C0=C1*FFU+C2*FU

            D1=DGAS(IBND,IABS,IPRES)*FFP+
     $         DGAS(IBND,IABS,IPRES-1)*FP
            D2=DGAS(IBND,IABS-1,IPRES)*FFP+
     $         DGAS(IBND,IABS-1,IPRES-1)*FP
            D0=D1*FFU+D2*FU

            TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
            GOTO 1976
         END IF

         IF (IBND.EQ.2) THEN
            A1=AC02(IABS,IPRES)*FFP+AC02(IABS,IPRES-1)*FP
            A2=AC02(IABS-1,IPRES)*FFP+AC02(IABS-1,IPRES-1)*FP
            A0=A1*FFU+A2*FU

            B1=BC02(IABS,IPRES)*FFP+BC02(IABS,IPRES-1)*FP
            B2=BC02(IABS-1,IPRES)*FFP+BC02(IABS-1,IPRES-1)*FP
            B0=B1*FFU+B2*FU

            C1=CC02(IABS,IPRES)*FFP+CC02(IABS,IPRES-1)*FP
            C2=CC02(IABS-1,IPRES)*FFP+CC02(IABS-1,IPRES-1)*FP
            C0=C1*FFU+C2*FU

            D1=DC02(IABS,IPRES)*FFP+DC02(IABS,IPRES-1)*FP
            D2=DC02(IABS-1,IPRES)*FFP+DC02(IABS-1,IPRES-1)*FP
            D0=D1*FFU+D2*FU

            TRANS=D0+TEFF*(C0+TEFF*(B0+TEFF*A0))
         END IF

      END IF

 1976 CONTINUE

      TEFF=TEFF+250.0


      END
