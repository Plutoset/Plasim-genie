C UTILITY ROUTINES:
      FUNCTION   ICLENG( CH )
      INTEGER ICLENG
      INTEGER IC
      INTEGER L
      CHARACTER*(*) CH
      ICLENG=0
      IC=LEN( CH )
      DO 5 L=1,IC
 5    IF( CH(L:L).NE.' ') ICLENG=L
      RETURN
      END
