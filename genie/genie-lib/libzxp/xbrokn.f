C
      SUBROUTINE XBROKN(IF1,IB1,IF2,IB2)
C Set line atribute as broken line segments.
C IF1 IF2 set length of  line segments plotted in unit of 0.001
C that of the total vertical ND-space range.
C IB1 IB2 set length of blanks between line segments in unit of 0.001
C that of the total vertical ND-space range.
C
      REAL H
      INTEGER IF1
      INTEGER IF2
      REAL HF1
      REAL HB1
      INTEGER IB1
      REAL HF2
      REAL HB2
      INTEGER IB2
      INTEGER IPV
      INTEGER IPSC
      INTEGER LFULL
      INTEGER KF1
      INTEGER KB1
      INTEGER KF2
      INTEGER KB2
      INTEGER KFULL
      INTEGER LTHICK
      REAL DTHICK
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C     
      COMMON /XLPN13/ HF1,HB1,HF2,HB2 , LFULL ,LTHICK, DTHICK
CPV
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
C
CPV
      H=0.001
CPV  change for efficient dashed lines
      IF(IF1.EQ.0)IF1=1
      IF(IF2.EQ.0)IF2=1
CPV
      HF1=H*IF1
      HB1=H*IB1
      HF2=H*IF2
      HB2=H*IB2
CPV  change for efficient dashed lines
      IF (IPV.EQ.1) THEN
      CALL BROKEN(IF1,IB1,IF2,IB2)
        IF(IPSC.EQ.1)CALL XBKPSC(IF1,IB1,IF2,IB2)
      ENDIF
CPV
      LFULL=0
      RETURN
      ENTRY XDASH
C Set line atribute as dash line.
      H=0.001
      HF1=H*10
      HB1=H*5
      HF2=H*10
      HB2=H*5
CPV  change for efficient dashed lines
      IF(IPV.EQ.1)THEN
      CALL BROKEN(10,5,10,5)
      IF(IPSC.EQ.1)CALL XDHPSC
      ENDIF
CPV
      LFULL=0
      RETURN
      ENTRY XDOT
C Set line atribute as dash line.
      H=0.001
      HF1=H*1
      HB1=H*6
      HF2=H*1
      HB2=H*6
CPV  change for efficient dashed lines
      IF(IPV.EQ.1)THEN
      CALL BROKEN(1,6,1,6)
      IF(IPSC.EQ.1)CALL XDTPSC
      ENDIF
CPV
      LFULL=0
      RETURN
      ENTRY XQBRKN(KF1,KB1,KF2,KB2)
      H=0.001
      KF1=HF1/H
      KB1=HB1/H
      KF2=HF2/H
      KB2=HB2/H
      RETURN
      ENTRY XFULL
C Set line atribute as solid (full) line.
CPV  change for efficient dashed lines
      IF(IPV.EQ.1)THEN
      CALL FULL
      IF(IPSC.EQ.1)CALL XFLPSC
      ENDIF
CPV
      LFULL=1
      RETURN
      ENTRY XQFULL(KFULL)
      KFULL=LFULL
      RETURN
CPV
      ENTRY XPVOFF
      IPV=0
      RETURN
      ENTRY XPVON
      IPV=1
CPV
      RETURN
      END
