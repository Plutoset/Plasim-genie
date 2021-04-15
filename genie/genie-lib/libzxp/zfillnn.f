C
      SUBROUTINE ZFILLNN(X,Y,NP,IUNMAP)
C
C      Routine to fill in polygon in colour fill
C
C
C     The following is valid only if you are creating a replay metafile
C     Two permutations on a similar theme. The first fills in an
C     arbitary set of points. This almost works but for some reason,
C     fails in a few places. I think this is due to working with
C     integers rather than real.
C
C     The second method assumes that everything can be split into
C     a series of triangles and this is used. It's abit faster and
C     fills in the whole screen but fails to produce a perfect result
C
      include 'uniras.inc'
      include 'mappings.inc'
      include 'colours.inc'
C
      INTEGER NP
      INTEGER I
      INTEGER IUNMAP
      INTEGER IXVMAP
      INTEGER IYMAP
      INTEGER IPSC
      INTEGER ICOLZ
      INTEGER NRASX
      INTEGER NRASY
      INTEGER NRASCL
      INTEGER NP1
      INTEGER IXX
      INTEGER IYY
      INTEGER IIISTY
      INTEGER ICOLZ1
      INTEGER IXMIN
      INTEGER ICXMIN
      INTEGER IXMAX
      INTEGER ICXMAX
      INTEGER IYMIN
      INTEGER ICYMIN
      INTEGER IYMAX
      INTEGER ICYMAX
      INTEGER J
      INTEGER LMSK
      INTEGER IFLAG
      INTEGER III
      INTEGER I1
      REAL XINT
      INTEGER II
      INTEGER I2
      INTEGER I3
      REAL X2
      REAL X3
      INTEGER ICOLL
      INTEGER IMASKCOL
      INTEGER IRESCALE
      INTEGER ITEMP
      INTEGER NCNT
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      REAL X(*),Y(*)
      INTEGER IX1(IJOINM1),IY1(IJOINM1),IX(3),IY(3)
      REAL XX(IJOINM1),YY(IJOINM1)
      INTEGER DIX,IMSK(DIX)
      LOGICAL LV,LTEST1,LTEST2
      INTEGER ZICOLOR
C
      SAVE ICOLZ,ICOLZ1
C
      IF(NP.LT.3.OR.NP.GT.IJOINM1)THEN
        PRINT*,' INVALID NP IN ZFILLNN ',NP
        RETURN
      ENDIF
C
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
C
      DO 200 I=1,NP
         XX(I)=X(I)
       YY(I)=Y(I)
         IF (IUNMAP.EQ.0) THEN
          CALL XTRANS(XX(I),YY(I))
         END IF
       IX1(I)=IXVMAP(XX(I))
       IY1(I)=IYMAP(YY(I))
 200  CONTINUE
C
      IF (IPSC.NE.0) CALL XZPSC(XX,YY,NP)
      CALL X_FILLPOLY(NP,IX1,IY1,ICOLZ)
C
      do i=1,np
         ix1(i)=ix1(i)+1
         iy1(i)=iy1(i)+1
      end do
      CALL VIDRAS(NRASX,NRASY,NRASCL,LV,IGIF)
      LTEST1=LV.OR.IGIF.NE.0
      LTEST2=LSAVE.AND.ISAVE.GT.0
C
      IF (ITYSCR.GE.5.AND..NOT.LTEST1) RETURN
      IF (.NOT.LTEST2.AND..NOT.LTEST1) RETURN
C
C       Only needed for producing replay metafiles
C
C
C     Because a PC screen has very limited resultion. Sone points that
C     are different XX,YY have the same IXX,IYY. Try to remove them
C
      NP1=0
      DO 220 I=1,NP
         IXX=IX1(I)
         IYY=IY1(I)
C
C  If first point then do nothing
C
         IF (I.EQ.1) THEN
            NP1=1
            IX1(NP1)=IXX
            IY1(NP1)=IYY
C
C  If same as last point, then do nothing
C
         ELSE IF (IXX.EQ.IX1(NP1).AND.IYY.EQ.IY1(NP1)) THEN
C
C If not and next point then just add
C
         ELSE IF (NP1.EQ.1) THEN
           NP1=NP1+1
           IX1(NP1)=IXX
           IY1(NP1)=IYY
C
C Finally check to see if points are pure horizontal or
C pure vertical
C
         ELSE IF (IYY.EQ.IY1(NP1).AND.IYY.EQ.IY1(NP1-1)) THEN
           IF ( (IX1(NP1).GE.IX1(NP1-1).AND.IXX.GT.IX1(NP1)).OR.
     :          (IX1(NP1).LE.IX1(NP1-1).AND.IXX.LT.IX1(NP1)) ) THEN
              IX1(NP1)=IXX
           ELSE
              NP1=NP1+1
              IX1(NP1)=IXX
              IY1(NP1)=IYY
           ENDIF
         ELSE IF (IXX.EQ.IX1(NP1).AND.IXX.EQ.IX1(NP1-1)) THEN
           IF ( (IY1(NP1).GE.IY1(NP1-1).AND.IYY.GT.IY1(NP1)).OR.
     :          (IY1(NP1).LE.IY1(NP1-1).AND.IYY.LT.IY1(NP1)) ) THEN
              IY1(NP1)=IYY
           ELSE
              NP1=NP1+1
              IX1(NP1)=IXX
              IY1(NP1)=IYY
           ENDIF
         ELSE
           NP1=NP1+1
           IX1(NP1)=IXX
           IY1(NP1)=IYY
         ENDIF
 220  CONTINUE
C
C     Now choose between styles
C
      IIISTY=2
C
      IF (IIISTY.EQ.1) THEN
C
C     If only have short sequence return
C
      IF (NP1.EQ.1) THEN
         IF (LTEST2) CALL SSSLLL(IX1(1),IY1(1),IX1(1),IY1(1),ICOLZ1)
         IF (LTEST1) CALL VLINE(IX1(1),IY1(1),IX1(1),IY1(1),ICOLZ)
         RETURN
      ELSE IF (NP1.EQ.2) THEN
         IF (LTEST2) CALL SSSLLL(IX1(1),IY1(1),IX1(2),IY1(2),ICOLZ1)
         IF (LTEST1) CALL VLINE(IX1(1),IY1(1),IX1(2),IY1(2),ICOLZ)
         RETURN
      END IF
C
      CALL IMINS(IX1,NP1,IXMIN,ICXMIN)
      CALL IMAXS(IX1,NP1,IXMAX,ICXMAX)
      CALL IMINS(IY1,NP1,IYMIN,ICYMIN)
      CALL IMAXS(IY1,NP1,IYMAX,ICYMAX)
C
      IF (IX1(1).NE.IX1(NP1).AND.IY1(1).NE.IY1(NP1)) THEN
        NP1=NP1+1
        IX1(NP1)=IX1(1)
        IY1(NP1)=IY1(1)
      ENDIF
C
      IF (IXMIN.EQ.IXMAX) THEN
         IF (IYMIN.EQ.IYMAX) THEN
            IF (LTEST2) CALL SSSLLL(IXMIN,IYMIN,IXMIN,IYMIN,ICOLZ1)
            IF (LTEST1) CALL VLINE(IXMIN,IYMIN,IXMIN,IYMIN,ICOLZ)
         ELSE
            IF (LTEST2) CALL SSSLLL(IXMIN,IYMIN,IXMAX,IYMAX,ICOLZ1)
            IF (LTEST1) CALL VLINE(IXMIN,IYMIN,IXMAX,IYMAX,ICOLZ)
         ENDIF
      ELSE IF (IYMIN.EQ.IYMAX) THEN
         IF (LTEST2) CALL SSSLLL(IXMIN,IYMIN,IXMAX,IYMAX,ICOLZ1)
         IF (LTEST1) CALL VLINE(IXMIN,IYMIN,IXMAX,IYMAX,ICOLZ)
      ELSE IF (IYMIN+1.EQ.IYMAX) THEN
         IF (LTEST2) CALL SSSLLL(IXMIN,IYMIN,IXMAX,IYMIN,ICOLZ1)
         IF (LTEST2) CALL SSSLLL(IXMIN,IYMAX,IXMAX,IYMAX,ICOLZ)
         IF (LTEST1) CALL VLINE(IXMIN,IYMIN,IXMAX,IYMIN,ICOLZ)
         IF (LTEST1) CALL VLINE(IXMIN,IYMAX,IXMAX,IYMAX,ICOLZ)
      ELSE
C
         DO I=2,NP1
            IF (LTEST2)
     :       CALL SSSLLL(IX1(I-1),IY1(I-1),IX1(I),IY1(I),ICOLZ1)
            IF (LTEST1)
     :       CALL VLINE(IX1(I-1),IY1(I-1),IX1(I),IY1(I),ICOLZ)
         END DO
C
         DO 1100 J=IYMIN,IYMAX
         LMSK=0
         DO 1100 I=IXMIN-1,IXMAX
         IFLAG=0
         III=I
         DO 120 I1=2,NP1
            IF((IY1(I1-1).LE.J.AND.IY1(I1).GT.J).OR.
     :         (IY1(I1-1).GT.J.AND.IY1(I1).LE.J))THEN
C
C      POSSIBLE INTERSECTION
C
               XINT=REAL((J-IY1(I1-1))*(IX1(I1)-IX1(I1-1)))
     :             /REAL(IY1(I1)-IY1(I1-1)) + IX1(I1-1)
               IF (XINT.GT.REAL(I-1).AND.XINT.LE.REAL(I)) THEN
C
C      WE HAVE INTERSECTION
C
                  IFLAG=IFLAG+1
               END IF
            ELSE IF (IY1(I1-1).EQ.J.AND.IY1(I1).EQ.J) THEN
               IF (I.GE.IX1(I1-1).AND.I.LE.IX1(I1)) THEN
                  IFLAG=1
                  LMSK=0
                  III=I
               END IF
            END IF
120      CONTINUE
         IF (MOD(IFLAG,2).EQ.1) LMSK=1-LMSK
         IF (LMSK.EQ.1) THEN
            IF (LTEST2) CALL SSSLLL(III,J,III,J,ICOLZ1)
            IF (LTEST1) CALL VLINE(III,J,III,J,ICOLZ)
         END IF
1100     CONTINUE
      END IF
C
      ELSE
C
C     Fill in triangles
C
      IX(1)=IX1(1)
      IY(1)=IY1(1)
      DO 500 II=1,NP1-2
         IX(2)=IX1(II+1)
         IX(3)=IX1(II+2)
         IY(2)=IY1(II+1)
         IY(3)=IY1(II+2)
         CALL IMINS(IY,3,IYMIN,ICYMIN)
         CALL IMAXS(IY,3,IYMAX,ICYMAX)
         IF(IYMIN.EQ.IYMAX) THEN
           CALL IMINS(IX,3,IXMIN,ICXMIN)
           CALL IMAXS(IX,3,IXMAX,ICXMAX)
           IF (LTEST2) CALL SSSLLL(IXMIN,IYMIN,IXMAX,IYMAX,ICOLZ1)
           IF (LTEST1) CALL VLINE(IXMIN,IYMIN,IXMAX,IYMAX,ICOLZ)
         ELSE
           I1=ICYMIN
           I2=ICYMAX
           IF(I1+I2.EQ.3)THEN
              I3=3
           ELSEIF(I1+I2.EQ.4)THEN
              I3=2
           ELSEIF(I1+I2.EQ.5)THEN
              I3=1
           ENDIF
           DO 510 I=IYMIN,IYMAX
           IF(IY(I2).EQ.IY(I1)) THEN
              X2=IX(I2)
           ELSE
          X2=REAL((IX(I2)-IX(I1))*(I-IY(I1)))/REAL(IY(I2)-IY(I1))+IX(I1)
           ENDIF
           IF(I.LE.IY(I3))THEN
              IF(IY(I3).EQ.IY(I1))THEN
                 X3=IX(I3)
              ELSE
          X3=REAL((IX(I3)-IX(I1))*(I-IY(I1)))/REAL(IY(I3)-IY(I1))+IX(I1)
              ENDIF
           ELSE
              IF(IY(I3).EQ.IY(I2))THEN
                 X3=IX(I3)
              ELSE
          X3=REAL((IX(I3)-IX(I2))*(I-IY(I2)))/REAL(IY(I3)-IY(I2))+IX(I2)
              ENDIF
           ENDIF
           IF (LTEST2) CALL SSSLLL(NINT(X2),I,NINT(X3),I,ICOLZ1)
           IF (LTEST1) CALL VLINE(NINT(X2),I,NINT(X3),I,ICOLZ)
 510       CONTINUE
         ENDIF
 500  CONTINUE
C

      END IF
      RETURN
C
      ENTRY ZFILLCOL(ICOLL)
C
      ICOLZ1=ICOLL
      IF (ICOLL.LE.-10) THEN
         ICOLZ=ICOLL+512
         ICOLZ1=ICOLZ
      else if (icoll.eq.-9) then
         if (imaskcol.ne.-1) then
            icolz=imaskcol
         else
            icolz=1
         end if
      ELSE IF (ICOLL.LE.0) THEN
         ICOLZ=0
      ELSE
         ICOLZ=ICV(ICOLL)
      ENDIF
C
      RETURN
C
      ENTRY DPICT(IMSK,DIX,IRESCALE)
C
      CALL XZRPSC(IMSK,DIX)
      IF (IGIF.NE.0)
     :  CALL VIDFIL(IMSK,DIX,IXRASO,IYRASO,IRESCALE,ITYSCR,ICV)
C
      ITEMP=IXRASO
      DO 100 I=1,DIX
C
      IF (IMSK(I).NE.0.AND.ITYSCR.LT.5) THEN
         III=IMSK(I)
         IF (III.EQ.-20) THEN
            III=-1
         else if (iii.le.-5) then
            iii=iabs(iii)
         else if (III.LT.-4) THEN
            III=0
         else if (iii.eq.-4) then
            iii=1
         ELSE IF (III.EQ.-3) THEN
            III=1
         ELSE IF (III.EQ.-2) THEN
            III=1
         ELSE IF (III.EQ.-1) THEN
            III=0
         ELSE IF (III.EQ.0) THEN
            III=0
         ELSE
            IF (IRESCALE.EQ.0) III=ICV(III)
         ENDIF
         if (iii.eq.1.and.imaskcol.ne.-1) iii=imaskcol
c         Call X_LineTo(itemp,iyraso,-1)
c         Call X_LineTo(itemp,iyraso,iii)
          if (iii.ge.0) Call X_SetPixel(itemp,iyraso,iii)
      END IF
C
      ITEMP=ITEMP+1
100   CONTINUE
      IYRASO=IYRASO+IOFRAS
      RETURN
C
      ENTRY ZCOLOR(NCNT)
C
      DO I=1,NCNT+1
         ICV(I)=NOFFSET + NINT(REAL((I-1)*(NCOL-NOFFSET-1))/REAL(NCNT))
      END DO
      RETURN
C
      ENTRY ZMASKCH(ZICOLOR)
c
      IMASKCOL=ZICOLOR
      return
C
      ENTRY QMASKCH(ZICOLOR)
c
      ZICOLOR=IMASKCOL
      RETURN
      END
