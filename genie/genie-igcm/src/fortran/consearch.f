

C***********************************************************************
C*                                                                     *
C*                         SUBROUTINE CONSEARCH                        *
C*                                                                     *
C***********************************************************************

      SUBROUTINE CONSEARCH(MOD,UPATH,PEFF,TEFF,WVEFF,AWV,BWV,
     $                     CWV,DWV,TRANS)

C Subroutine CONSEARCH calculates the water vapour transmittane of a
C homogeneous atmospheric path for both line+continuum absorption over
C the whole thermal IR spectrum (0-3000 cm-1). The subroutine uses the
C data of pre-computed tables stored in 'wvcon.f'
C
C INPUT:  Index for transmittance type (MOD)
C         Absorber amount (UPATH)
C         Pressure (PEFF)
C         Temperature (TEFF)
C         Effective water vapour abs. amount (WVEFF)
C         Pre-computed tables (AWV,BWV,CWV,DWV)
C
C OUTPUT: Transmittance (TRANS)

      IMPLICIT NONE

C-----------------
C Input Variables
C-----------------

      INTEGER MOD
      REAL UPATH,PEFF,TEFF,WVEFF
      REAL AWV(2,41,29,22),BWV(2,41,29,22)
      REAL CWV(2,41,29,22),DWV(2,41,29,22)

C-----------------
C Output Variable
C-----------------

      REAL TRANS

C--------------------
C Internal Variables
C--------------------

      INTEGER IABS,IPRES,IWV,NABS,NPRES,NWV
      PARAMETER(NABS=29,NPRES=22,NWV=41)
      REAL USTR(NABS)          ! Stored values of abs. amount,
      REAL PSTR(NPRES)         ! pressure and
      REAL WVSTR(NWV)          ! logarithm of eff. H2O amount

      REAL TR(0:2),TRU,TRP     ! Variables used
      REAL DTU,DTP,DTU1,DTU2   ! for interpolation
      REAL SLP, INTCPT

C Stored values of absorber amount (kg m-2)

      DATA(USTR(IABS),IABS=1,NABS)/
     $                               1.0D-06, 5.0D-06, 1.0D-05,
     $                               5.0D-05, 1.0D-04, 5.0D-04,
     $                               1.0D-03, 5.0D-03, 1.0D-02,
     $                               2.0D-02, 4.0D-02, 6.0D-02,
     $                               8.0D-02, 1.0D-01, 2.0D-01,
     $                               4.0D-01, 6.0D-01, 8.0D-01,
     $                               1.0, 2.0, 4.0, 6.0, 8.0,
     $                               10.0, 20.0, 40.0, 60.0,
     $                               80.0, 100.0/


C Stored values of pressure (Pa)

      DATA(PSTR(IPRES),IPRES=1,NPRES)/ 1.0D+02, 2.0D+02, 4.0D+02,
     $                                 6.0D+02, 8.0D+02, 1.0D+03,
     $                                 2.0D+03, 4.0D+03, 6.0D+03,
     $                                 8.0D+03, 1.0D+04, 2.0D+04,
     $                                 3.0D+04, 4.0D+04, 5.0D+04,
     $                                 6.0D+04, 7.0D+04, 8.0D+04,
     $                                 9.0D+04, 1.0D+05, 1.1D+05,
     $                                 1.2D+05/

C Stored values of the logarithm of the H2O effective amount

      DATA(WVSTR(IWV),IWV=1,NWV)/ -4.6, -4.4, -4.2, -4.0, -3.8, -3.6,
     $                            -3.4, -3.2, -3.0, -2.9, -2.8, -2.7,
     $                            -2.6, -2.5, -2.4, -2.3, -2.2, -2.1,
     $                            -2.0, -1.9, -1.8, -1.7, -1.6, -1.5,
     $                            -1.4, -1.3, -1.2, -1.1, -1.0, -0.9,
     $                            -0.8, -0.7, -0.6, -0.5, -0.4, -0.3,
     $                            -0.2, -0.1,  0.0,  0.1,  0.2/

      WVEFF=LOG10(WVEFF)

C Find the table index for the mixing ratio

      IWV=INT(NWV/2.)

      IF (WVEFF.NE.WVSTR(IWV)) THEN

         IF (WVEFF.LT.WVSTR(IWV)) THEN

            IWV=INT(IWV/2.)

            IF (WVEFF.LT.WVSTR(IWV)) THEN
               IWV=1
               DO WHILE(WVEFF.GT.WVSTR(IWV))
                  IWV=IWV+1
               END DO
            ELSE
               IF (WVEFF.NE.WVSTR(IWV)) THEN
                  DO WHILE(WVEFF.GT.WVSTR(IWV))
                     IWV=IWV+1
                  END DO
               END IF
            END IF

         ELSE

            IWV=IWV+INT(NWV/4.)

            IF (WVEFF.GT.WVSTR(IWV)) THEN
               IF (WVEFF.GE.WVSTR(NWV)) THEN
                  IWV=NWV
               ELSE
                  DO WHILE(WVEFF.GT.WVSTR(IWV))
                     IWV=IWV+1
                  END DO
               END IF
            ELSE
               IF (WVEFF.NE.WVSTR(IWV)) THEN
                  IWV=INT(NWV/2.)
                  DO WHILE(WVEFF.GT.WVSTR(IWV))
                     IWV=IWV+1
                  END DO
               END IF
            END IF

         END IF

      END IF

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

      IF(PEFF.LT.PSTR(1))THEN
        IPRES=1
        WRITE(6,*)'WARNING: Top level pressure < 1mbar. Out
     :             of range of pre-computed table.'
      ELSE

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
      ENDIF
C-------------------------------------------
C Simple interpolation/extrapolation scheme
C-------------------------------------------

C PIERS' fix
      IF (IWV.LE.1) IWV=2
C--- MOD=1

C An interpolated transmittance (with respect to pressure and absorber
C amount) is calculated on two successive levels of effective H2O amount
C and the final transmittance results from the interpolation between
C the 2 values

      TEFF=TEFF-250.0

      IF (MOD.EQ.1) THEN

         TR(0)=DWV(MOD,IWV,IABS,IPRES)+
     $         TEFF*(CWV(MOD,IWV,IABS,IPRES)+
     $         TEFF*(BWV(MOD,IWV,IABS,IPRES)+
     $         TEFF*AWV(MOD,IWV,IABS,IPRES)))

         TRANS=TR(0)

         IF (IABS.GT.1) THEN    ! Avoid extrap. for small abs. amounts

            IF (UPATH.NE.USTR(IABS)) THEN

               TRU=DWV(MOD,IWV,IABS-1,IPRES)+
     $             TEFF*(CWV(MOD,IWV,IABS-1,IPRES)+
     $             TEFF*(BWV(MOD,IWV,IABS-1,IPRES)+
     $             TEFF*AWV(MOD,IWV,IABS-1,IPRES)))
               DTU=(TRANS-TRU)*
     $             (UPATH-USTR(IABS))/(USTR(IABS)-USTR(IABS-1))
               TRU=TR(0)+DTU    ! Correction for the abs. amount
               IF (TRU.LT.0.0) THEN
                  TRU=0.0       ! Very high abs. amounts
               END IF

            ELSE                ! Avoid interpolation
               TRU=TR(0)
            END IF

            IF (PEFF.NE.PSTR(IPRES)) THEN

               TRP=DWV(MOD,IWV,IABS,IPRES-1)+
     $             TEFF*(CWV(MOD,IWV,IABS,IPRES-1)+
     $             TEFF*(BWV(MOD,IWV,IABS,IPRES-1)+
     $             TEFF*AWV(MOD,IWV,IABS,IPRES-1)))
               DTP=(TRANS-TRP)*
     $              (PEFF-PSTR(IPRES))/(PSTR(IPRES)-PSTR(IPRES-1))
               TRP=TRU+DTP      ! Correction for the pressure
               IF (TRP.LT.0.0) THEN
                  TRP=0.0       ! Very high pressures
               END IF
               TR(1)=TRP

            ELSE                ! Avoid interpolation
               TR(1)=TRU
            END IF

            TRANS=TR(1)

         END IF

C Interpolation between effective absorber amounts

         IF (WVEFF.NE.WVSTR(IWV)) THEN

            TR(0)=DWV(MOD,IWV-1,IABS,IPRES)+
     $            TEFF*(CWV(MOD,IWV-1,IABS,IPRES)+
     $            TEFF*(BWV(MOD,IWV-1,IABS,IPRES)+
     $            TEFF*AWV(MOD,IWV-1,IABS,IPRES)))

            IF (IABS.GT.1) THEN ! Avoid extrap. for small abs. amounts

               IF (UPATH.NE.USTR(IABS)) THEN

                  TRU=DWV(MOD,IWV-1,IABS-1,IPRES)+
     $                TEFF*(CWV(MOD,IWV-1,IABS-1,IPRES)+
     $                TEFF*(BWV(MOD,IWV-1,IABS-1,IPRES)+
     $                TEFF*AWV(MOD,IWV-1,IABS-1,IPRES)))
                  DTU=(TRANS-TRU)*
     $                (UPATH-USTR(IABS))/(USTR(IABS)-USTR(IABS-1))
                  TRU=TR(0)+DTU   ! Correction for the abs. amount
                  IF (TRU.LT.0.0) THEN
                     TRU=0.0      ! Very high abs. amounts
                  END IF

               ELSE               ! Avoid interpolation
                  TRU=TR(0)
               END IF

               IF (PEFF.NE.PSTR(IPRES)) THEN

                  TRP=DWV(MOD,IWV-1,IABS,IPRES-1)+
     $                TEFF*(CWV(MOD,IWV-1,IABS,IPRES-1)+
     $                TEFF*(BWV(MOD,IWV-1,IABS,IPRES-1)+
     $                TEFF*AWV(MOD,IWV-1,IABS,IPRES-1)))
                  DTP=(TRANS-TRP)*
     $                (PEFF-PSTR(IPRES))/(PSTR(IPRES)-PSTR(IPRES-1))
                  TRP=TRU+DTP     ! Correction for the pressure
                  IF (TRP.LT.0.0) THEN
                     TRP=0.0      ! Very high pressures
                  END IF
                  TR(2)=TRP

               ELSE               ! Avoid interpolation
                  TR(2)=TRU
               END IF

            END IF

            SLP=(TR(1)-TR(2))/(WVSTR(IWV)-WVSTR(IWV-1))
            INTCPT=WVSTR(IWV)*TR(2)-WVSTR(IWV-1)*TR(1)
            INTCPT=INTCPT/(WVSTR(IWV)-WVSTR(IWV-1))
            TRANS=SLP*WVEFF+INTCPT

         END IF

C--- MOD=2

C The transmittances between 2 successive levels of the effective H2O
C are projected on a value on the actual level and interpolation is
C performed on the actual level. Interpolation between pressure levels
C is not considered

C Transmittance calculation

      ELSE

         TR(1)=DWV(MOD,IWV,IABS,IPRES)+
     $         TEFF*(CWV(MOD,IWV,IABS,IPRES)+
     $         TEFF*(BWV(MOD,IWV,IABS,IPRES)+
     $         TEFF*AWV(MOD,IWV,IABS,IPRES)))

         TR(2)=DWV(MOD,IWV-1,IABS,IPRES)+
     $         TEFF*(CWV(MOD,IWV-1,IABS,IPRES)+
     $         TEFF*(BWV(MOD,IWV-1,IABS,IPRES)+
     $         TEFF*AWV(MOD,IWV-1,IABS,IPRES)))

C Interpolation between effective absorber amounts

         IF (WVEFF.NE.WVSTR(IWV)) THEN
            TR(0)=TR(2)+(TR(1)-TR(2))*(WVEFF-WVSTR(IWV-1))/
     $            (WVSTR(IWV)-WVSTR(IWV-1))
         ELSE
            TR(0)=TR(1)
         END IF

C Interpolation between absorber amounts

         IF (IABS.GT.1) THEN    ! Avoid extrap. for small abs. amounts

            IF (UPATH.NE.USTR(IABS)) THEN

               TRU=DWV(MOD,IWV,IABS-1,IPRES)+
     $             TEFF*(CWV(MOD,IWV,IABS-1,IPRES)+
     $             TEFF*(BWV(MOD,IWV,IABS-1,IPRES)+
     $             TEFF*AWV(MOD,IWV,IABS-1,IPRES)))
               DTU1=(TR(1)-TRU)*
     $              (UPATH-USTR(IABS))/(USTR(IABS)-USTR(IABS-1))

               TRU=DWV(MOD,IWV-1,IABS-1,IPRES)+
     $             TEFF*(CWV(MOD,IWV-1,IABS-1,IPRES)+
     $             TEFF*(BWV(MOD,IWV-1,IABS-1,IPRES)+
     $             TEFF*AWV(MOD,IWV-1,IABS-1,IPRES)))
               DTU2=(TR(2)-TRU)*
     $              (UPATH-USTR(IABS))/(USTR(IABS)-USTR(IABS-1))

               IF (WVEFF.NE.WVSTR(IWV)) THEN
                  DTU=DTU2+(DTU1-DTU2)*(WVEFF-WVSTR(IWV-1))/
     $               (WVSTR(IWV)-WVSTR(IWV-1))
               ELSE
                  DTU=0.0
               END IF

               TRU=TR(0)+DTU    ! Correction for the abs. amount
               IF (TRU.LT.0.0) THEN
                  TRU=0.0       ! Very high abs. amounts
               END IF

            ELSE                ! Avoid interpolation
               TRU=TR(0)
            END IF

            TRANS =TRU

         ELSE

            TRANS=TR(0)

         END IF

      END IF

      IF (TRANS.LT.0.0) THEN
         TRANS=0.0
      END IF

      IF (TRANS.GT.1.0) THEN
         TRANS=1.0
      END IF

      END
