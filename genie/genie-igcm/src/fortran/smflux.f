

C***********************************************************************
C*                                                                     *
C*                         SUBROUTINE SMFLUX                           *
C*                                                                     *
C***********************************************************************

      SUBROUTINE SMFLUX(NLEV,PFLUX,FLUX)

C Subroutine SMFLUX smooths out the flux profiles, assuming that
C the fluxes in an atmospheric layers of a certain thickness above and
C below an atmospheric level vary as   FLUX = A0 + A1*P + A2*(P**2)
C
C INPUT:  Number of levels in the atmosphere without the surface (NLEV)
C         Pressure at each level (PFLUX)
C         Irradiance at each mid-level (FUP)
C
C OUTPUT: Smoothed upward and downward irradiances at
C         each level (FUP,FDWN)

      IMPLICIT NONE
#include "parray.cmn"

C-----------------
C Input Variables
C-----------------

      INTEGER NLEV
      REAL PFLUX(0:MXLEV),FLUX(0:MXLEV)

C--------------------
C Internal Variables
C--------------------

      INTEGER ILAY,ILEV,KOUNTSM
      REAL THICKN    ! Thickness of a layer above/below a level in mb
      REAL*8 DET,A0(0:MXLEV),A1(0:MXLEV),A2(0:MXLEV)
      INTEGER SN                               ! Sums used for least
      REAL*8 SX,SX2,SX3,SX4,SX2Y,SXY,SY        ! square fit method


C--- Set the thickness of the layers in which regression is performed

      IF (NLEV.GT.30) THEN
         THICKN=75.0
      ELSE
         THICKN=100.0
      ENDIF

C--- Calculate regression coefficients for the first atmospheric level

      SN=0
      SX=0.0
      SX2=0.0
      SX3=0.0
      SX4=0.0
      SX2Y=0.0
      SXY=0.0
      SY=0.0

      DO ILAY=0,NLEV
         IF ((PFLUX(0)-PFLUX(ILAY)).LE.100.0) THEN
            SN=SN+1
            SX=SX+PFLUX(ILAY)
            SX2=SX2+(PFLUX(ILAY)**2)
            SX3=SX3+(PFLUX(ILAY)**3)
            SX4=SX4+(PFLUX(ILAY)**4)
            SX2Y=SX2Y+((PFLUX(ILAY)**2)*FLUX(ILAY))
            SXY=SXY+(PFLUX(ILAY)*FLUX(ILAY))
            SY=SY+FLUX(ILAY)
         END IF
      END DO

      DET=(SX2**3)+(SX*SX*SX4)+(SN*SX3*SX3)-
     $     SX2*(SN*SX4+2*SX*SX3)

      IF ((DET.EQ.0.0).OR.(SN.LE.2)) THEN
         SN=0
         SX=0.0
         SX2=0.0
         SX3=0.0
         SX4=0.0
         SX2Y=0.0
         SXY=0.0
         SY=0.0
         DO ILAY=0,2
            SN=SN+1
            SX=SX+PFLUX(ILAY)
            SX2=SX2+(PFLUX(ILAY)**2)
            SX3=SX3+(PFLUX(ILAY)**3)
            SX4=SX4+(PFLUX(ILAY)**4)
            SX2Y=SX2Y+((PFLUX(ILAY)**2)*FLUX(ILAY))
            SXY=SXY+(PFLUX(ILAY)*FLUX(ILAY))
            SY=SY+FLUX(ILAY)
         END DO
         DET=(SX2**3)+(SX*SX*SX4)+(SN*SX3*SX3)-
     $        SX2*(SN*SX4+2*SX*SX3)
      END IF

      A0(0)=(SX2*SX2*SX2Y)+SX4*(SX*SXY-SX2*SY)+
     $       SX3*(SY*SX3-SXY*SX2-SX2Y*SX)

      A1(0)=(SX*SY*SX4)+SN*(SX3*SX2Y-SX4*SXY)+
     $       SX2*(SXY*SX2-SX*SX2Y-SX3*SY)

      A2(0)=(SY*SX2*SX2)+SN*(SXY*SX3-SX2*SX2Y)+
     $       SX*(SX*SX2Y-SX3*SY-SX2*SXY)

      A0(0)=A0(0)/DET
      A1(0)=A1(0)/DET
      A2(0)=A2(0)/DET

C--- Calculate regression coefficients for the upper atmospheric level

      SN=0.0
      SX=0.0
      SX2=0.0
      SX3=0.0
      SX4=0.0
      SX2Y=0.0
      SXY=0.0
      SY=0.0

      DO ILAY=1,NLEV
         IF ((PFLUX(ILAY)-PFLUX(NLEV)).LE.200.0) THEN
            SN=SN+1
            SX=SX+PFLUX(ILAY)
            SX2=SX2+(PFLUX(ILAY)**2)
            SX3=SX3+(PFLUX(ILAY)**3)
            SX4=SX4+(PFLUX(ILAY)**4)
            SX2Y=SX2Y+((PFLUX(ILAY)**2)*FLUX(ILAY))
            SXY=SXY+(PFLUX(ILAY)*FLUX(ILAY))
            SY=SY+FLUX(ILAY)
         END IF
      END DO

      DET=(SX2**3)+(SX*SX*SX4)+(SN*SX3*SX3)-
     $     SX2*(SN*SX4+2*SX*SX3)
      IF ((DET.EQ.0.0).OR.(SN.LE.2)) THEN
         SN=0.0
         SX=0.0
         SX2=0.0
         SX3=0.0
         SX4=0.0
         SX2Y=0.0
         SXY=0.0
         SY=0.0
         DO ILAY=NLEV-2,NLEV
            SN=SN+1
            SX=SX+PFLUX(ILAY)
            SX2=SX2+(PFLUX(ILAY)**2)
            SX3=SX3+(PFLUX(ILAY)**3)
            SX4=SX4+(PFLUX(ILAY)**4)
            SX2Y=SX2Y+((PFLUX(ILAY)**2)*FLUX(ILAY))
            SXY=SXY+(PFLUX(ILAY)*FLUX(ILAY))
            SY=SY+FLUX(ILAY)
         END DO
         DET=(SX2**3)+(SX*SX*SX4)+(SN*SX3*SX3)-
     $        SX2*(SN*SX4+2*SX*SX3)
      END IF

      A0(NLEV)=(SX2*SX2*SX2Y)+SX4*(SX*SXY-SX2*SY)+
     $          SX3*(SY*SX3-SXY*SX2-SX2Y*SX)

      A1(NLEV)=(SX*SY*SX4)+SN*(SX3*SX2Y-SX4*SXY)+
     $          SX2*(SXY*SX2-SX*SX2Y-SX3*SY)

      A2(NLEV)=(SY*SX2*SX2)+SN*(SXY*SX3-SX2*SX2Y)+
     $          SX*(SX*SX2Y-SX3*SY-SX2*SXY)

      A0(NLEV)=A0(NLEV)/DET
      A1(NLEV)=A1(NLEV)/DET
      A2(NLEV)=A2(NLEV)/DET

C--- Calculate regression coefficients for the rest of the levels

      DO ILAY=1,NLEV-1

         KOUNTSM=0.

 1997    CONTINUE

         SN=0
         SX=0.0
         SX2=0.0
         SX3=0.0
         SX4=0.0
         SX2Y=0.0
         SXY=0.0
         SY=0.0

         DO ILEV=0,NLEV
            IF (ABS(PFLUX(ILAY)-PFLUX(ILEV)).LE.THICKN) THEN
               SN=SN+1
               SX=SX+PFLUX(ILEV)
               SX2=SX2+(PFLUX(ILEV)**2)
               SX3=SX3+(PFLUX(ILEV)**3)
               SX4=SX4+(PFLUX(ILEV)**4)
               SX2Y=SX2Y+((PFLUX(ILEV)**2)*FLUX(ILEV))
               SXY=SXY+(PFLUX(ILEV)*FLUX(ILEV))
               SY=SY+FLUX(ILEV)
            END IF
         END DO

         DET=(SX2**3)+(SX*SX*SX4)+(SN*SX3*SX3)-
     $        SX2*(SN*SX4+2*SX*SX3)

         IF ((DET.EQ.0.0).OR.(SN.LE.2)) THEN
            KOUNTSM=KOUNTSM+1
            IF (KOUNTSM.GT.5)THEN
               WRITE(6,*)'SUBROUTINE SMFLUX STUCK IN INFINITE
     $                    LOOP. ABORT.'
               CALL ABORT
            ENDIF
            THICKN=THICKN+100.0
            GOTO 1997
         END IF

         A0(ILAY)=(SX2*SX2*SX2Y)+SX4*(SX*SXY-SX2*SY)+
     $             SX3*(SY*SX3-SXY*SX2-SX2Y*SX)

         A1(ILAY)=(SX*SY*SX4)+SN*(SX3*SX2Y-SX4*SXY)+
     $             SX2*(SXY*SX2-SX*SX2Y-SX3*SY)

         A2(ILAY)=(SY*SX2*SX2)+SN*(SXY*SX3-SX2*SX2Y)+
     $             SX*(SX*SX2Y-SX3*SY-SX2*SXY)

         A0(ILAY)=A0(ILAY)/DET
         A1(ILAY)=A1(ILAY)/DET
         A2(ILAY)=A2(ILAY)/DET

      END DO

      DO ILAY=0,NLEV
         FLUX(ILAY)=real(A0(ILAY)+PFLUX(ILAY)*(A1(ILAY)+
     $              A2(ILAY)*PFLUX(ILAY)))
         IF (FLUX(ILAY).LT.0.0) THEN
            FLUX(ILAY)=0.0
         END IF
      END DO


      END
