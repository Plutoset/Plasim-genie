      SUBROUTINE XVECTR(U,V,X,Y,MD,M,ISTEP,N,JSTEP,XLENG,UUNIT)
C Plot vector feilds ( U,V)   . Unit X-component UUNIT is plotted with
C length XLENG in mapped maths coordinate. Length of vector in
C Y-direction is scaled according to the mapping.
C
      INTEGER MD
      INTEGER N
      INTEGER J
      INTEGER JSTEP
      INTEGER I
      INTEGER M
      INTEGER ISTEP
      REAL XLENG
      REAL UUNIT
C
      REAL U(MD,N),V(MD,N),X(MD,N),Y(MD,N)
      DO 15 J=1,N,JSTEP
      DO 15 I=1,M,ISTEP
        CALL XARROW( U(I,J),V(I,J),X(I,J),Y(I,J),XLENG,UUNIT)
 15   CONTINUE
      RETURN
      END
