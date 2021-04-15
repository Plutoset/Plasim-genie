!#######################################################################################
!#######################################################################################
!  This module provides various general routines that I've found useful.
!    REFLECT(arrin[,dim}) : Reflects a 1-d or 2-d array about an axis.
!
! PPH 25/08/04
!#######################################################################################
!#######################################################################################
MODULE land_general_utils
  INTERFACE reflect
    MODULE PROCEDURE reflect_r1d, reflect_i1d, reflect_r2d, reflect_i2d
  END INTERFACE
  !##################################################################
CONTAINS
  !###############################################
  FUNCTION reflect_r1d(arrin)
    IMPLICIT NONE
    REAL,INTENT(in),DIMENSION(:) :: arrin
    REAL,DIMENSION(SIZE(arrin,1)) :: reflect_r1d 
    INTEGER :: i,s

    s=SIZE(arrin,1)
    DO i=1,s
      reflect_r1d(i) = arrin(s-i+1)
    ENDDO
  END FUNCTION reflect_r1d

  !###############################################
  FUNCTION reflect_i1d(arrin)
    IMPLICIT NONE
    INTEGER,INTENT(in),DIMENSION(:) :: arrin
    INTEGER,DIMENSION(SIZE(arrin,1)) :: reflect_i1d
    INTEGER :: i,s

    s=SIZE(arrin,1)
    DO i=1,s
      reflect_i1d(i) = arrin(s-i+1)
    ENDDO
  END FUNCTION reflect_i1d

  !###############################################
  FUNCTION reflect_r2d(arrin,dim)
    IMPLICIT NONE
    REAL,INTENT(in),DIMENSION(:,:) :: arrin
    INTEGER,INTENT(in),OPTIONAL :: dim
    INTEGER :: d
    REAL,DIMENSION(SIZE(arrin,1),SIZE(arrin,2)) :: reflect_r2d
    INTEGER :: i,s

    IF(.not.PRESENT(dim)) THEN 
      d=1
    ELSE 
      d=dim
    ENDIF

    s=SIZE(arrin,d)
    SELECT CASE(d)
      CASE(2)
        DO i=1,s
          reflect_r2d(:,i) = arrin(:,s-i+1)
        ENDDO
      CASE DEFAULT
        DO i=1,s
          reflect_r2d(i,:) = arrin(s-i+1,:)
        ENDDO
    END SELECT

  END FUNCTION reflect_r2d

  !###############################################
  FUNCTION reflect_i2d(arrin,dim)
    IMPLICIT NONE
    INTEGER,INTENT(in),DIMENSION(:,:) :: arrin
    INTEGER,INTENT(in),OPTIONAL :: dim
    INTEGER :: d
    INTEGER,DIMENSION(SIZE(arrin,1),SIZE(arrin,2)) :: reflect_i2d
    INTEGER :: i,s

    IF(.not.PRESENT(dim)) THEN 
      d=1
    ELSE 
      d=dim
    ENDIF

    s=SIZE(arrin,d)
    SELECT CASE(d)
      CASE(2)
        DO i=1,s
          reflect_i2d(:,i) = arrin(:,s-i+1)
        ENDDO
      CASE DEFAULT
        DO i=1,s
          reflect_i2d(i,:) = arrin(s-i+1,:)
        ENDDO
    END SELECT
  END FUNCTION reflect_i2d

END MODULE land_general_utils
