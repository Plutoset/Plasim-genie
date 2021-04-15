      SUBROUTINE MYFILT(ZI,NNI,MMI,MOCTI,NHEMI,FILT,MMMAX)
      IMPLICIT NONE
      INTEGER MMMAX
      COMPLEX ZI(*)
      REAL FILT(0:MMMAX)
      INTEGER NNI,MMI,MOCTI,NHEMI
C
      INTEGER II,IHEM,MP,JP
C
      II=0
      DO IHEM=1,NHEMI
         DO MP=1,MMI,MOCTI
            DO JP=MP,NNI,2
               II=II+1
               ZI(II)=ZI(II)*FILT(JP+IHEM-2)
            END DO
         END DO
      END DO
C
      END
C
      SUBROUTINE MYFILT8(ZI,NNI,MMI,MOCTI,NHEMI,FILT,MMMAX)
      IMPLICIT NONE
      INTEGER MMMAX
      COMPLEX*16 ZI(*)
      REAL*8 FILT(0:MMMAX)
      INTEGER NNI,MMI,MOCTI,NHEMI
C
      INTEGER II,IHEM,MP,JP
C
      II=0
      DO IHEM=1,NHEMI
         DO MP=1,MMI,MOCTI
            DO JP=MP,NNI,2
               II=II+1
               ZI(II)=ZI(II)*FILT(JP+IHEM-2)
            END DO
         END DO
      END DO
C
      END