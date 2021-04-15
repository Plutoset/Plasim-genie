

C***********************************************************************
C*                                                                     *
C*                         SUBROUTINE LINSEARCH                        *
C*                                                                     *
C***********************************************************************

      SUBROUTINE LINSEARCH(MOD,UPATH,PEFF,TEFF,AWVL,BWVL,
     $                     CWVL,DWVL,TRANS)

C Subroutine LINSEARCH calculates the water vapour line transmittane of
C a homogeneous  atmospheric path, using the data of pre-computed
C tables stored in 'wvlin.f'
C
C INPUT:  Index for transmittance type (MOD)
C         Absorber amount (UPATH)
C         Pressure (PEFF)
C         Pre-computed tables (AWVL,BWVL,CWVL,DWVL)
C         Temperature (TEFF)
C
C OUTPUT: Transmittance (TRANS)

      IMPLICIT NONE

C-----------------
C Input Variables
C-----------------

      INTEGER MOD
      REAL UPATH,PEFF,TEFF
      REAL AWVL(2,34,28),BWVL(2,34,28)
      REAL CWVL(2,34,28),DWVL(2,34,28)

C-----------------
C Output Variable
C-----------------

      REAL TRANS

C--------------------
C Internal Variables
C--------------------

      INTEGER IABS,IPRES,NABS,NPRES
      PARAMETER(NABS=34,NPRES=28)
      REAL USTR(NABS),PSTR(NPRES)  ! Stored values of absorber
                                     ! amount and pressure

      REAL TRU,DTU,TRP,DTP         ! Variables used for interpolation


C Stored values of absorber amount (kg m-2)

      DATA(USTR(IABS),IABS=1,NABS)/  1.0D-09, 1.0D-08, 5.0D-08,
     $                               1.0D-07, 5.0D-07, 1.0D-06,
     $                               5.0D-06, 1.0D-05, 5.0D-05,
     $                               1.0D-04, 5.0D-04, 1.0D-03,
     $                               5.0D-03, 1.0D-02, 2.0D-02,
     $                               4.0D-02, 6.0D-02, 8.0D-02,
     $                               1.0D-01, 2.0D-01, 4.0D-01,
     $                               6.0D-01, 8.0D-01, 1.0, 2.0,
     $                               4.0, 6.0, 8.0, 10.0, 20.0,
     $                               40.0, 60.0, 80.0, 100.0/


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

      TRANS=DWVL(MOD,IABS,IPRES)+TEFF*(CWVL(MOD,IABS,IPRES)+
     $      TEFF*(BWVL(MOD,IABS,IPRES)+
     $      TEFF*AWVL(MOD,IABS,IPRES)))

C-------------------------------------------
C Simple interpolation/extrapolation scheme
C-------------------------------------------

      IF (IABS.GT.1) THEN  ! Avoid extrapolation for small abs. amounts

         IF (UPATH.NE.USTR(IABS)) THEN

            TRU=DWVL(MOD,IABS-1,IPRES)+TEFF*(CWVL(MOD,IABS-1,IPRES)+
     $          TEFF*(BWVL(MOD,IABS-1,IPRES)+
     $          TEFF*AWVL(MOD,IABS-1,IPRES)))
            DTU=(TRANS-TRU)*
     $          (UPATH-USTR(IABS))/(USTR(IABS)-USTR(IABS-1))
            TRU=TRANS+DTU       ! Correction for the abs. amount
            IF (TRU.LT.0.0) THEN
               TRU=0.0          ! Very high abs. amounts
            END IF

         ELSE                   ! Avoid interpolation
            TRU=TRANS
         END IF

         IF (PEFF.NE.PSTR(IPRES)) THEN

            TRP=DWVL(MOD,IABS,IPRES-1)+TEFF*(CWVL(MOD,IABS,IPRES-1)+
     $          TEFF*(BWVL(MOD,IABS,IPRES-1)+
     $          TEFF*AWVL(MOD,IABS,IPRES-1)))
            DTP=(TRANS-TRP)*
     $          (PEFF-PSTR(IPRES))/(PSTR(IPRES)-PSTR(IPRES-1))
            TRP=TRU+DTP         ! Correction for the pressure
            IF (TRP.LT.0.0) THEN
               TRP=0.0          ! Very high pressures
            END IF
            TRANS=TRP

         ELSE                   ! Avoid interpolation
            TRANS=TRU
         END IF

      END IF


      END
