      SUBROUTINE SSSOOO(IRENEW,IJOIN)
      include 'mappings.inc'
C
      INTEGER IRENEW
      INTEGER IJOIN
      INTEGER ITEMP
      INTEGER I
      INTEGER ISK
C
1000  FORMAT(6I4)
1001  FORMAT(20I3)
1002  FORMAT(11F7.2)

      IF(LSAVE.AND.ISAVE.GT.0.AND.
     :  ( (IRENEW.EQ.1.AND.IJOIN.GE.IJOINM1).OR.
     :    (IRENEW.EQ.2.AND.IJOIN.GT.1) ) )  THEN
      ITEMP=(IJOIN-1)/ISKIP+1
      IF(MOD(IJOIN,ISKIP).EQ.0.AND.ITEMP.GT.4.AND.ISKIP.GT.1)THEN
        ITEMP=ITEMP+1
        WRITE(NCHAN,1000)ITEMP,ICOLPC,IDASH,IDASH,IDASH,IDASH
        IF(ISAVE.EQ.1)THEN
          WRITE(NCHAN,1001)(IXJ(I),IYJ(I),I=1,IJOIN,ISKIP),
     :                        IXJ(IJOIN),IYJ(IJOIN)
          IXJ(1)=IXJ(IJOIN)
          IYJ(1)=IYJ(IJOIN)
        ELSEIF(ISAVE.EQ.2)THEN
          WRITE(NCHAN,1002)(XJ(I),YJ(I),I=1,IJOIN,ISKIP),
     :                        XJ(IJOIN),YJ(IJOIN)
          XJ(1)=XJ(IJOIN)
          YJ(1)=YJ(IJOIN)
        ENDIF
      ELSE
        ISK=ISKIP
        IF(ITEMP.LE.4)THEN
          ITEMP=IJOIN
          ISK=1
        ENDIF
        WRITE(NCHAN,1000)ITEMP,ICOLPC,IDASH,IDASH,IDASH,IDASH
        IF(ISAVE.EQ.1)THEN
          WRITE(NCHAN,1001)(IXJ(I),IYJ(I),I=1,IJOIN,ISK)
          IXJ(1)=IXJ(IJOIN)
          IYJ(1)=IYJ(IJOIN)
        ELSEIF(ISAVE.EQ.2)THEN
          WRITE(NCHAN,1002)(XJ(I),YJ(I),I=1,IJOIN,ISK)
          XJ(1)=XJ(IJOIN)
          YJ(1)=YJ(IJOIN)
        ENDIF
      ENDIF
      IJOIN=1
      ENDIF
      END
