      SUBROUTINE DCOLOR(DIMSK,IMSK,DIX,DICV,DNCNT,istyle)
      include 'mappings.inc'
      include 'colours.inc'
C
      INTEGER I
      INTEGER II
      INTEGER II1
C
      INTEGER DIX,IMSK(*),DNCNT,istyle
      REAL DIMSK(*),DICV(DNCNT)
C
1004  FORMAT(25I3)
C
      DO 21 I=1,DIX
      IF (DIMSK(I).EQ.999999.OR.DIMSK(I).EQ.999998.OR.
     :    DIMSK(I).EQ.999997.OR.DIMSK(I).EQ.999996) THEN
      IMSK(I)=0
      ELSE
      DO 31 II=1,DNCNT
      II1=II
      IF(DICV(II).GT.DIMSK(I))GO TO 41
31      CONTINUE
        IF (DIMSK(I).GT.DICV(DNCNT))II1=DNCNT+1
41      IF(II1.LT.0)II1=0
      IMSK(I)=II1
      ENDIF
21    CONTINUE
C
      if (istyle.eq.2) then
         do i=1,dix
            ii1=imsk(i)
            if ((ii1.gt.1).and.(ii1.le.dncnt)) then
               if (dicv(ii1)*dicv(ii1-1).le.0.) then
                  imsk(i)=-4
               endif
            else if (ii1.eq.1) then
               if (dicv(1)*(2*dicv(1)-dicv(2)).le.0) then
                  imsk(i)=-4
               endif
            else
               if (dicv(dncnt)*(2*dicv(dncnt)-dicv(dncnt-1))
     $              .le.0.) then
                  imsk(i)=-4
               endif
            endif
         enddo
      endif

      
      IF(LSAVE.AND.ISAVE.GT.0)THEN
       WRITE(NCHAN,1004)(IMSK(I),I=1,DIX)
      ENDIF
C
      DO I=1,DIX
         IF (DIMSK(I).EQ.999999) THEN
            IMSK(I)=0
         ELSE IF(DIMSK(I).EQ.999998) THEN
           IMSK(I)=-1
         ELSE IF(DIMSK(I).EQ.999997) THEN
          IMSK(I)=-2
         ELSE IF(DIMSK(I).EQ.999996) THEN
          IMSK(I)=-3
         ELSE
            IF (IMSK(I).GT.DNCNT+1) IMSK(I)=DNCNT+1
         END IF
      END DO
C
      RETURN
      END
