C
      SUBROUTINE ZCSTLN(NCHNL)
C1.2
C1.2  Modified to produce low resolution coastline
C1.2
C1.3
C1.3  Modified so that coastlines can be stored in array
C1.3
      INTEGER IDIM,IDATA1,IDATA2
      INTEGER I,IMULT,ISILENT,IOUT,ILAST,INC,ISEGC
      REAL    X,Y,XLAST,YLAST,XST,YST,XEND
      INTEGER IA,NODATA,ISEGM,IFAIL,II,III
      INTEGER NCHNL,ISEG
      PARAMETER(IDIM=8000)
      REAL POINTS(IDATA2,2)
      INTEGER INSEG(IDATA1),NSEG,ICSTSW
      real xout(idim),yout(idim)
      SAVE NSEG,ICSTSW
C
      include 'zpltpm.cmn'
c      COMMON/ZPLTPM/CX,CY,ALONS,ALATS,SALATS,CALATS,RC,AMP,SNWD,CSWD
c     :,CENT,ISTL,SWITCH,XH,YH,RAD,YEND,DX,DY,YTOP,RDX,RDY,RTOP,NM,NJ
c     :,MG,JG,JG2,JG2P,MGP,MGHP,DLONG,DLAT,PI,SVAGL,CVAGL,SROT,CROT,Q
c     :,ROUSN,ROUCS,PI2,ALFA,BETA,FANST,FANED,ITRUNC,AMQ,AMR,AK,YB,AB
c     :,ICOASRD,INCCST,ICOORD,CENTY,X1LIM,X2LIM,Y1LIM,Y2LIM
c     :,Z1STEP,Z1CHAR,IZTYPE,IVECTC,ARM
C
      DATA ICSTSW/0/
C
      IMULT=1
      IF (ISTL.EQ.3.OR.ISTL.EQ.4) THEN
         IF (BETA.NE.PI2.AND.ITRUNC.EQ.1) IMULT=4
      END IF
C
      ISILENT=0
      IF (ICSTSW.NE.0) THEN
         call zoutln
         call xclear
         call zgetln(xout,yout,iout)
         if (iout.gt.idim) then
            print*,' zcstln: redimension (and in zcstfl) ',iout,idim
            stop 1
         end if
         if (isilent.ne.0) then
            print*,iout
            do i=1,iout
               print*,i,xout(i),yout(i)
            end do
         end if
         iout=iout-1
      END IF
C
      ILAST=-999
      INC=0
      ISEGC=0
  200 CONTINUE
      READ (NCHNL,*,END=400) ISEG,X,Y
      IF(INCCST.LE.1)THEN
         IF (ISEG.NE.ILAST)THEN
            if (ilast.ne.-999) then
               IF (ICSTSW.NE.0)
     :             call zcstfl(xout,yout,iout)
            END IF
          CALL ZPROJC(X,Y,0,IA)
          ILAST=ISEG
         ELSE
          CALL ZPROJC(X,Y,1,IA)
         ENDIF
      ELSE
         INC=INC+1
         IF (ISEG.NE.ILAST) THEN
          IF (INC.GT.1) CALL ZPROJC(XLAST,YLAST,1,IA)
            if (ilast.ne.-999) then
               IF (ICSTSW.NE.0)
     :             call zcstfl(xout,yout,iout)
            END IF
          CALL ZPROJC(X,Y,0,IA)
             ILAST=ISEG
          INC=0
         ELSE
             XLAST=X
          YLAST=Y
          IF (INC.EQ.INCCST) THEN
             CALL ZPROJC(X,Y,1,IA)
             INC=0
          ENDIF
         ENDIF
      ENDIF
      GOTO 200
  400 CONTINUE
      IF (INCCST.GT.1) THEN
         IF(INC.GT.1)CALL ZPROJC(XLAST,YLAST,1,IA)
      ENDIF
      if (ilast.ne.-999) then
         IF (ICSTSW.NE.0)
     :       call zcstfl(xout,yout,iout)
      END IF
      REWIND NCHNL

      RETURN
C1.3
      ENTRY ZCSTRD(NCHNL,INSEG,IDATA1,POINTS,IDATA2)
C
      do i=1,idata1
         inseg(i)=0
      end do
C
      NSEG=1
      NODATA=0
      ILAST=-999
      ISEGM=0
      INC=0
      ISEGC=0
  500 CONTINUE
      READ (NCHNL,*,END=600) ISEG,X,Y
      IF (INCCST.LE.1) THEN
         IF (ISEG.NE.ILAST) THEN
          IF (ISEGM.NE.0) THEN
             INSEG(NSEG)=ISEGM
             NSEG=NSEG+1
          ENDIF
          ISEGM=1
          NODATA=NODATA+1
          POINTS(NODATA,1)=X
          POINTS(NODATA,2)=Y
          ILAST=ISEG
         ELSE
          ISEGM=ISEGM+1
          NODATA=NODATA+1
          POINTS(NODATA,1)=X
          POINTS(NODATA,2)=Y
         ENDIF
      ELSE
         INC=INC+1
         IF (ISEG.NE.ILAST) THEN
          IF (INC.GT.1) THEN
             ISEGM=ISEGM+1
             NODATA=NODATA+1
             POINTS(NODATA,1)=XLAST
             POINTS(NODATA,2)=YLAST
          ENDIF
          IF (ISEGM.NE.0) THEN
             INSEG(NSEG)=ISEGM
             NSEG=NSEG+1
          ENDIF
          ISEGM=1
          NODATA=NODATA+1
          POINTS(NODATA,1)=X
          POINTS(NODATA,2)=Y
          ILAST=ISEG
          INC=0
         ELSE
          XLAST=X
          YLAST=Y
          IF (INC.EQ.INCCST) THEN
               ISEGM=ISEGM+1
             NODATA=NODATA+1
             POINTS(NODATA,1)=X
             POINTS(NODATA,2)=Y
             INC=0
          ENDIF
         ENDIF
      ENDIF
      GOTO 500
  600 CONTINUE
      IF (INCCST.GT.1)THEN
         IF (INC.GT.1)THEN
          ISEGM=ISEGM+1
          NODATA=NODATA+1
          POINTS(NODATA,1)=XLAST
          POINTS(NODATA,2)=YLAST
         ENDIF
      ENDIF
      IF (ISEGM.NE.0) INSEG(NSEG)=ISEGM
C
      IFAIL=0
C
      IF(NSEG.GT.IDATA1)THEN
      PRINT*,' Too many segments for array, increase size ',idata1,nseg
      IFAIL=1
      ENDIF
C      IF(NSEG.NE.IDATA1)PRINT*,' Number of coastline segments = ',NSEG
C
      IF(NODATA.GT.IDATA2)THEN
      PRINT*,' Too many coastline points for array, increase size '
      IFAIL=1
      ENDIF
C      IF(NODATA.NE.IDATA2)PRINT*,' Number of coastline point = ',NODATA
C
      IF(IFAIL.EQ.1)STOP
C
      ICOASRD=1
C
      REWIND NCHNL
      RETURN
      ENTRY ZCSTPL(INSEG,IDATA1,POINTS,IDATA2)
C
C
      IMULT=1
      IF (ISTL.EQ.3.OR.ISTL.EQ.4) THEN
         IF (BETA.NE.PI2.AND.ITRUNC.EQ.1) IMULT=4
      END IF
C
      isilent=0
      IF (ICSTSW.NE.0) THEN
         call zoutln
         call xclear
         call zgetln(xout,yout,iout)
         if (iout.gt.idim) then
            print*,' zcstln: redimension (and in zcstfl) ',iout,idim
            stop 1
         end if
         iout=iout-1
         if (isilent.ne.0) then
            print*,' iout ',iout
            do i=1,iout
               print*,i,xout(i),yout(i)
            end do
            read(5,*)
         end if
      END IF
C
      NODATA=0
      DO I=1,NSEG
         ISEGM=INSEG(I)
         NODATA=NODATA+1
         CALL ZPROJC(POINTS(NODATA,1),POINTS(NODATA,2),0,IA)
         XST=POINTS(NODATA,1)
         YST=POINTS(NODATA,2)
         DO II=2,ISEGM
            NODATA=NODATA+1
            IF (IMULT.EQ.1) THEN
               CALL ZPROJC(POINTS(NODATA,1),POINTS(NODATA,2),1,IA)
            ELSE
               XEND=POINTS(NODATA,1)
               YEND=POINTS(NODATA,2)
               DX=(XEND-XST)/REAL(IMULT)
               DY=(YEND-YST)/REAL(IMULT)
               DO III=1,IMULT-1
                  XST=XST+DX
                  YST=YST+DY
                  CALL ZPROJC(XST,YST,1,IA)
               END DO
               CALL ZPROJC(POINTS(NODATA,1),POINTS(NODATA,2),1,IA)
               XST=XEND
               YST=YEND                  
            END IF
         END DO
         IF (ICSTSW.NE.0) call zcstfl(xout,yout,iout)
      END DO
C1.3
      RETURN
      ENTRY ZCSTSW
         ICSTSW=1-ICSTSW
      RETURN
      END
