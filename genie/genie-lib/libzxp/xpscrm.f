      SUBROUTINE XPSCRM
C
      include 'vpsc.inc'
C
      INTEGER I
C
      DO 100 I=1,IPSCON
 100  BACKSPACE NOUT
c
      ICHR=0
      IPSCRM=0
      IPSCLT=-1
      IPSCCL=-100
      IPSTHI=1
      IPSCON=0
      ILINE=1
      ISLINE=1
      RETURN
c
      ENTRY XPSCE
      IPSCRM=1
      END
