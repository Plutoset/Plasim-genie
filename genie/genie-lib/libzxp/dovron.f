
      SUBROUTINE DOVRON
      include 'mappings.inc'
      INCLUDE 'uniras.inc'
C
      INTEGER ITEMP
C
1000  FORMAT(6I4)
C
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
C
      IF(LSAVE.AND.ISAVE.GT.0)THEN
      ITEMP=-2
      WRITE(NCHAN,1000)ITEMP,ICOLPC,IDASH,IDASH,IDASH,IDASH
      ENDIF
      RETURN
      ENTRY DOVROF
C
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
C
      IF(LSAVE.AND.ISAVE.GT.0)THEN
      ITEMP=-2
      WRITE(NCHAN,1000)ITEMP,ICOLPC,IDASH,IDASH,IDASH,IDASH
      ENDIF
      RETURN

      ENTRY DSAVON
C
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
C
      LSAVE=.TRUE.
      RETURN

      ENTRY DSAVOF
C
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
C
      LSAVE=.FALSE.
      RETURN

      ENTRY XSAVLB
C
      IF(IJOIN.GT.1) THEN
         CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
         IF(LSAVE.AND.ISAVE.GT.0)THEN
          ITEMP=-4
          WRITE(NCHAN,1000)ITEMP,ICOLPC,IDASH,IDASH,IDASH,IDASH
         ENDIF
      ENDIF

      END
