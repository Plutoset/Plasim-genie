C
      SUBROUTINE ZFUD(IFUD,R1FUD,R2FUD,ICOUNT)
C
      INTEGER I
      INTEGER ICOUNT
      INTEGER IFUD
      REAL R1FUD
      REAL R2FUD
C
      DIMENSION IFUD(*),R1FUD(*),R2FUD(*)
      DO 20 I=1,ICOUNT
      IF(IFUD(I).EQ.0)CALL ZPENUP(R1FUD(I),R2FUD(I))
   20 IF(IFUD(I).EQ.1)CALL ZPENDN(R1FUD(I),R2FUD(I))
      IFUD(1)=0
      R1FUD(1)=R1FUD(ICOUNT)
      R2FUD(1)=R2FUD(ICOUNT)
      ICOUNT=1
      RETURN
      END
