C *****************************COPYRIGHT******************************
C (c) CROWN COPYRIGHT 1997, METEOROLOGICAL OFFICE, All Rights Reserved.
C
C Use, duplication or disclosure of this code is subject to the
C restrictions as set forth in the contract.
C
C                Meteorological Office
C                London Road
C                BRACKNELL
C                Berkshire UK
C                RG12 2SZ
C
C If no contract has been raised with this copy of the code, the use,
C duplication or disclosure of it is strictly prohibited.  Permission
C to do so must first be obtained in writing from the Head of Numerical
C Modelling at the above address.
C ******************************COPYRIGHT******************************
!!! Subroutine LOTKA --------------------------------------------------
!!!
!!! Purpose : Updates fractional coverage of each functional type.
!!!           Based on the Lotka-Volterra equations of interspecies
!!!           competition.
!!!
!!!
!!!  Model            Modification history:
!!! version  Date
!!!  4.4     10/97     New Deck. Peter Cox
!!!  4.5  12/05/98     Operate only on points indexed with TRIF_INDEX
!!!                    and correct calculation of NOSOIL.  Richard Betts
!!!
!!!END ----------------------------------------------------------------
      SUBROUTINE LOTKA (LAND_FIELD,TRIF_PTS,TRIF_INDEX
     &,                 C_VEG,FORW,FRAC_VS,FRAC_AGRIC,GAMMA,LAI,PC_S
     &,                 FRAC,DFRAC)

      USE land_const
      IMPLICIT NONE

      INTEGER
     & LAND_FIELD                 ! IN Total number of land points.
     &,TRIF_PTS                   ! IN Number of points on which
!                                 !    TRIFFID may operate
     &,TRIF_INDEX(LAND_FIELD)     ! IN Indices of land points on
!                                 !    which TRIFFID may operate
     &,K,L,M,N,T                  ! WORK Loop counters.
     &,DOM(LAND_FIELD,NPFT)       ! WORK Dominance hierachy.

      REAL
     & C_VEG(LAND_FIELD,NPFT)     ! IN Carbon content of vegetation
                                  !    (kg C/m2).
     &,FORW                       ! IN Forward timestep weighting.
     &,FRAC_VS(LAND_FIELD)        ! IN Total fractional cover of
!                                 !    vegetation and soil.
     &,FRAC_AGRIC(LAND_FIELD)     ! IN Fraction of agriculture.
     &,GAMMA                      ! IN Inverse timestep (/360days).
     &,LAI(LAND_FIELD,NPFT)       ! IN Leaf area index.
     &,PC_S(LAND_FIELD,NPFT)      ! IN Net carbon flux available for
                                  !    spreading (kg C/m2/360days).
     &,FRAC(LAND_FIELD,NTYPE)     ! INOUT Fractional cover of each
C                                 !       Functional Type.
     &,DFRAC(LAND_FIELD,NPFT)     ! OUT Increment to the areal fraction
C                                 !     during the timestep (/timestep).
     &,B(LAND_FIELD,NPFT)         ! WORK Mean rate of change of
C                                 !      vegetation fraction over
C                                 !      the timestep (kg C/m2/360days).
     &,DB_DFRAC(LAND_FIELD,NPFT,NPFT)
C                                 ! WORK Rate of change of B
C                                 !      with vegetation fraction.
     &,COM(LAND_FIELD,NPFT,NPFT)  ! WORK Coefficients representing
C                                 !      the influence of one type
C                                 !      (second argument) on another
C                                 !      (first argument).
     &,DIFF_SUM                   ! WORK Difference divided by sum
C                                 !      for competing canopy heights.
     &,HC1,HC2,HC3,HC4            ! WORK Competing canopy heights (m).
     &,NOSOIL(LAND_FIELD)         ! WORK Fractional area not available
C                                 !      to vegetation.
     &,SPACE(LAND_FIELD,NPFT)     ! WORK Space available for invasion.

      REAL
     + POW                        ! Power in sigmoidal function.
      PARAMETER(POW = 20.0)

C----------------------------------------------------------------------
C Define competition coefficients and the dominance hierachy
C----------------------------------------------------------------------

      DO N=1,NPFT
        DO M=1,NPFT
          DO T=1,TRIF_PTS
            L=TRIF_INDEX(T)
            COM(L,N,M) = 1.0
          ENDDO
        ENDDO
      ENDDO

      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)

        HC1 = A_WL(1)/(A_WS(1)*ETA_SL(1))*(LAI(L,1)**(B_WL(1)-1))
        HC2 = A_WL(2)/(A_WS(2)*ETA_SL(2))*(LAI(L,2)**(B_WL(2)-1))
        DIFF_SUM = (HC1-HC2)/(HC1+HC2)

        COM(L,1,2) = 1.0/(1+EXP(POW*DIFF_SUM))    ! BT vs NT
        COM(L,1,3) = 0.0                          ! BT vs C3G
        COM(L,1,4) = 0.0                          ! BT vs C4G
        COM(L,1,5) = 0.0                          ! BT vs S

        COM(L,2,1) = 1.0-COM(L,1,2)               ! NT vs BT
        COM(L,2,3) = 0.0                          ! NT vs C3G
        COM(L,2,4) = 0.0                          ! NT vs C4G
        COM(L,2,5) = 0.0                          ! NT vs S

        HC3 = A_WL(3)/(A_WS(3)*ETA_SL(3))*(LAI(L,3)**(B_WL(3)-1))
        HC4 = A_WL(4)/(A_WS(4)*ETA_SL(4))*(LAI(L,4)**(B_WL(4)-1))
        DIFF_SUM = (HC3-HC4)/(HC3+HC4)

        COM(L,3,4) = 1.0/(1+EXP(POW*DIFF_SUM))    ! C3G vs C4G
        COM(L,4,3) = 1.0-COM(L,3,4)               ! C4G vs C3G

        COM(L,5,3) = 0.0                          ! S vs C3G
        COM(L,5,4) = 0.0                          ! S vs C4G

        IF (HC1 .GE. HC2) THEN
          DOM(L,1) = 1
          DOM(L,2) = 2
        ELSEIF (HC1 .LT. HC2) THEN
          DOM(L,1) = 2
          DOM(L,2) = 1
        ENDIF

        DOM(L,3) = 5

        IF (HC3 .GE. HC4) THEN
          DOM(L,4) = 3
          DOM(L,5) = 4
        ELSEIF (HC3 .LT. HC4) THEN
          DOM(L,4) = 4
          DOM(L,5) = 3
        ENDIF

      ENDDO

C----------------------------------------------------------------------
C Calculate the space available for the expansion of each FT
C----------------------------------------------------------------------
      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)
        NOSOIL(L) = 1 - FRAC_VS(L)
      ENDDO

C----------------------------------------------------------------------
C Exclude non-crop types from agricultural regions
C----------------------------------------------------------------------
      DO K=1,NPFT
        DO T=1,TRIF_PTS
          L=TRIF_INDEX(T)
          N=DOM(L,K)
          SPACE(L,N)=1.0-NOSOIL(L)-FRAC_AGRIC(L)*(1-CROP(N))
     &                            -FRAC_MIN*(NPFT-K)
        ENDDO
      ENDDO

      DO N=1,NPFT
        DO M=1,NPFT
        DO T=1,TRIF_PTS
          L=TRIF_INDEX(T)
            SPACE(L,N)=SPACE(L,N)-COM(L,N,M)*FRAC(L,M)
          ENDDO
        ENDDO
      ENDDO

C----------------------------------------------------------------------
C Calculate the variables required for the implicit calculation.
C Divide the update equation by FRAC to eliminate the (unstable)
C bare soil solution.
C----------------------------------------------------------------------
      DO N=1,NPFT
        DO T=1,TRIF_PTS
          L=TRIF_INDEX(T)
          B(L,N) = PC_S(L,N)*SPACE(L,N)/C_VEG(L,N)-G_AREA(N)


          DO M=1,NPFT
            DB_DFRAC(L,N,M) = -COM(L,N,M)*PC_S(L,N)/C_VEG(L,N)
          ENDDO
        ENDDO
      ENDDO

C----------------------------------------------------------------------
C Update the areal fractions
C----------------------------------------------------------------------
      CALL COMPETE (DOM,LAND_FIELD,TRIF_PTS,TRIF_INDEX
     &,             B,DB_DFRAC,FORW,GAMMA,NOSOIL
     &,             FRAC,DFRAC)

      RETURN
      END
