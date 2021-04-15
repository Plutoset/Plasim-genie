C
      Subroutine ZJINIT
C
      INTEGER LABON
      INTEGER ISPSAVE
      INTEGER IJOIN
      REAL PIH
      INTEGER III
      INTEGER ISTLS
      REAL CXS
      REAL CYS
      REAL XHS
      REAL YHS
      REAL DXS
      REAL DYS
      REAL YTOPS
      INTEGER MGS
      INTEGER JG2S
      REAL RANGL
      REAL ANGLE
      REAL RATIO
      REAL ROU
      REAL ALFAS
      REAL BETAS
      INTEGER ITRN
      REAL R1
      REAL ANGL
      REAL R2
      REAL ANGH
      REAL AL
      REAL AT
      REAL YBS
      REAL CXXS
      REAL X2LIMS
      REAL X1LIMS
      REAL CYYS
      REAL Y2LIMS
      REAL Y1LIMS
      INTEGER INC
      INTEGER ITTYPE
      REAL ZHEIGHT
      REAL ZFREQ
      REAL XJ
      REAL YJ
      REAL ZSTEP
      REAL ZLEN
      REAL ZCHAR
      INTEGER NLABEL
      INTEGER IHTLAB
C
      include 'zpltpm.cmn'
c      COMMON/ZPLTPM/CX,CY,ALONS,ALATS,SALATS,CALATS,RC,AMP,SNWD,CSWD
c     :,CENT,ISTL,SWITCH,XH,YH,RAD,YEND,DX,DY,YTOP,RDX,RDY,RTOP,NM,NJ
c     :,MG,JG,JG2,JG2P,MGP,MGHP,DLONG,DLAT,PI,SVAGL,CVAGL,SROT,CROT,Q
c     :,ROUSN,ROUCS,PI2,ALFA,BETA,FANST,FANED,ITRUNC,AMQ,AMR,AK,YB,AB
c     :,ICOASRD,INCCST,ICOORD,CENTY,X1LIM,X2LIM,Y1LIM,Y2LIM
c     :,Z1STEP,Z1CHAR,IZTYPE,IVECTC,ARM
C
C
C1.3
C
      INTEGER MLIM
      PARAMETER (MLIM=8000)
      COMMON/ZEXTRA/ISPSAVE,IJOIN,XJ(MLIM),YJ(MLIM)
C
C
C
      COMMON/ZLABL/PIH,ZSTEP,ZLEN,ZCHAR,NLABEL,LABON,IHTLAB
C
C
****************************************************July,1987*********
*                                                                    *
*                GBPLOT GAPHICS PACKAGE                              *
*               ------------------------                             *
*                  by ZHANG Zuojun                                   *
*                                                                    *
*     Contouring & vector plotting can be performed in following     *
*     projection space according to the style parameter Istyle       *
*                                                                    *
*     Istyle=0: Non-projected space i.e data space (retangular plot) *
*     Istyle=1: Simple longitude-latitude projection                 *
*     Istyle=2: General conformal projection: .Mercator sylindrical  *
*                                             .Lambert conical       *
*                                             .polar-stereographic   *
*     Istyle=3: Polar visual image  (circular plot)                  *
*     Istyle=4: Polar stereographic projection                       *
*     Istyle=5: Equatorial visual image                              *
*     Istyle=6: Equatorial stereographic projection                  *
*     Istyle=7: Oblique visual image                                 *
*     Istyle=8: Oblique stereographic projection                     *
*                                                                    *
**********************************************************************
      PRINT*,' ZPLOT PC VERSION 1.4  PJV  1 APR 1991'
      PI=4.*ATAN(1.0)
      PI2=2.*PI
      CX=0.
C1.2
      CY=0.
C1.3
      INCCST=1
      ICOASRD=0
      Z1STEP=0.70
      Z1CHAR=0.0125
      IVECTC=0
      IZTYPE= 3
      LABON=0
      ISPSAVE=0
C1.4
      ARM=1.

      RC=1.
      ISTL=4
      SWITCH=1.
      XH=1.5
      YH=LOG(TAN(0.25*PI+0.5*75.*PI/180.))*XH/PI
      DX=20.
      DY=20.
      YTOP=80.
      MG=100
      JG=25
      SVAGL=0.
      CVAGL=1.
      SROT=0.
      CROT=1.
      AMP=1.
      AMQ=1.
      AMR=1.
      SNWD=1.
      CSWD=0.
      ROUSN=0.4*SIN(0.05*PI)
      ROUCS=0.4*COS(0.05*PI)
      ALFA=0.
      BETA=PI2
      AK=LOG(SQRT(3.))/LOG(2./SQRT(3.)+1.)
      Q=1.
      YB=0.
      AB=0.
      FANST=ALFA-0.5*BETA
      FANED=ALFA+0.5*BETA
      ITRUNC=1
      ALONS=CX*PI/180.
      ALATS=CY*PI/180.
      SALATS=SIN(ALATS)
      CALATS=COS(ALATS)
      CENT=XH*CX/180.
C1.2
      CENTY=YH*CY/90.
      YEND=2.*ATAN(EXP(PI*YH/XH))-0.5*PI
      RAD=XH/PI
      RDX=DX*PI/180.
      RDY=DY*PI/180.
      RTOP=YTOP*PI/180.
      NM=360./DX
      NJ=YTOP/DY
      JG2=JG+JG
      JG2P=JG2+1
      MGP=MG+1
      MGHP=MG/2+1
      DLONG=PI2/MG
      DLAT=RTOP/JG
C1.2
      X1LIM=-XH
      X2LIM=XH
      Y1LIM=-YH
      Y2LIM=YH
      ICOORD=0.0
C1.3
      IJOIN=1
      PIH=0.5*PI
      RETURN
C*****************************
      ENTRY ZSPSAVE(III)
      ISPSAVE=III
      RETURN
C*****************************
      ENTRY ZSTSTL(ISTLS)
      ISTL=ISTLS
      IF(ISTL.LT.0.OR.ISTL.GT.9) ISTL=0
      SWITCH=MOD(ISTL+1,2)
      IF(ISTL.LE.2.OR.ISTL.EQ.5.OR.ISTL.EQ.6) RETURN
      IF(SWITCH.EQ.0.) THEN
      IF(CSWD.GT.SVAGL) THEN
          AMQ=(1.-SVAGL*CSWD)/SNWD
          AMR=1.
      ELSE
          AMQ=CVAGL
          AMR=SNWD*CVAGL/(1.-SVAGL*CSWD)
      ENDIF
      ELSE
      AMQ=(1.+CSWD)/SNWD
      AMR=1.
      ENDIF
      RETURN
C*****************************
      ENTRY ZSTCEN(CXS,CYS)
      CX=CXS
      IF(CXS.GT. 180.) CX=MOD(CXS+180.,360.)-180.
      IF(CXS.LE.-180.) CX=MOD(CXS-180.,360.)+180.
      CY=CYS
      CENT=XH*CX/180.
C1.2
      CENTY=YH*CY/90.
      IF(CY.GE.0.) RC= 1.
      IF(CY.LT.0.) RC=-1.
      ALONS=CX*PI/180.
      ALATS=CY*PI/180.
      SALATS=SIN(ALATS)
      CALATS=COS(ALATS)
      RETURN
C*****************************
      ENTRY ZSTRET(XHS,YHS)
C1.2
      X1LIM=X1LIM*XHS/XH
      X2LIM=X2LIM*XHS/XH
      Y1LIM=Y1LIM*YHS/YH
      Y2LIM=Y2LIM*YHS/YH

      XH=XHS
      YH=YHS
      YEND=2.*ATAN(EXP(PI*YH/XH))-0.5*PI
      RAD=XH /PI
      CENT=XH*CX/180.
      CENTY=YH*CY/90.
      RETURN
C*****************************
      ENTRY ZSTLNT(DXS,DYS,YTOPS)
      DX=DXS
      DY=DYS
      YTOP=YTOPS
      RDX=DX*PI/180.
      RDY=DY*PI/180.
      RTOP=YTOP*PI/180.
      NM=360./DX
      NJ=YTOP/DY
      IF(YTOP.NE.DY*NJ) NJ=NJ+1
      DLAT=RTOP/JG
      RETURN
C*****************************
      ENTRY ZSTFRG(MGS,JG2S)
      MG=MGS
      JG=JG2S/2
      JG2=JG+JG
      JG2P=JG2+1
      MGP=MG+1
      MGHP=MG/2+1
      DLONG=PI2/MG
      DLAT=RTOP/JG
      RETURN
C*****************************
      ENTRY ZSTVAG(ANGLE)
      RANGL=ANGLE*PI/180.
      IF(ANGLE.GE.90.*PI.OR.ANGLE.LT.0.0) RANGL=0.
      SVAGL=SIN(RANGL)
      CVAGL=COS(RANGL)
      SNWD=CVAGL
      CSWD=SVAGL
      IF(SWITCH.EQ.0.) THEN
      AMQ=CVAGL
      AMR=1.
      ELSE
      AMQ=(1.+CSWD)/SNWD
      AMR=1.
      ENDIF
      RETURN
C*****************************
      ENTRY ZSTILT(ANGLE)
      RANGL=ANGLE*PI/180.
      IF(RANGL.EQ.0..OR.CALATS.EQ.0.) THEN
      CROT=1.
      SROT=0.
      RETURN
      ENDIF
      IF(COS(RANGL).GT.CALATS) THEN
      CROT=1.
      ELSE
      CROT=COS(RANGL)/CALATS
      ENDIF
      SROT=SQRT(1.-CROT*CROT)*RC*RANGL/ABS(RANGL)
      RETURN
C*****************************
      ENTRY ZSTROT(ANGLE)
      RANGL=ANGLE*PI/180.
      CROT=COS(RANGL)
      SROT=SIN(RANGL)
      RETURN
C*****************************
      ENTRY ZSTAMP(RATIO)
      AMP=RATIO
      RETURN
C*****************************
      ENTRY ZSTHTA(ANGLE)
      SNWD=SIN(PI*ANGLE/180.)
      CSWD=COS(PI*ANGLE/180.)
      IF(SWITCH.EQ.0.) THEN
      IF(CSWD.GT.SVAGL) THEN
          AMQ=(1.-SVAGL*CSWD)/SNWD
          AMR=1.
      ELSE
          AMQ=CVAGL
          AMR=SNWD*CVAGL/(1.-SVAGL*CSWD)
      ENDIF
      ELSE
      AMQ=(1.+CSWD)/SNWD
      AMR=1.
      ENDIF
      RETURN
C*****************************
      ENTRY ZSTARW(ANGLE,ROU)
      ROUSN=ROU*SIN(PI*ANGLE/180.)
      ROUCS=ROU*COS(PI*ANGLE/180.)
      RETURN
C*****************************
      ENTRY ZSTFAN(ALFAS,BETAS,ITRN)
      ALFA=PI*ALFAS/180.
      BETA=PI*BETAS/180.
      ITRUNC=ITRN
      FANST=ALFA-0.5*BETA
      FANED=ALFA+0.5*BETA
      RETURN
C*****************************
      ENTRY ZSTLMA(ANGL,ANGH)
      R1=ANGL*PI/180.
      R2=ANGH*PI/180.
      IF(ABS(R1).GT.ABS(R2)) THEN
      Q=R1/ABS(R1)
      ELSEIF(ABS(R1).LT.ABS(R2)) THEN
      Q=R2/ABS(R2)
      ELSE
      AK=ABS(0.5*(SIN(R1)+SIN(R2)))
      Q=0.
      IF(AK.NE.0.) Q=R1/ABS(R1)
      RETURN
      ENDIF
      AL= TAN(0.25*PI-0.5*R1*Q)
      AT= TAN(0.25*PI-0.5*R2*Q)
      AK=LOG(COS(R1)/COS(R2))/LOG(AL/AT)
      RETURN
C*****************************
      ENTRY ZSTLMB(ANGL,ANGH)
      R1=ANGL*PI/180.
      R2=ANGH*PI/180.
      IF(R1.EQ.R2) THEN
       AB=0.
       RETURN
      ENDIF
      IF(AK.NE.0.) THEN
      AL=(TAN(0.25*PI-0.5*Q*R1))**AK
      AT=(TAN(0.25*PI-0.5*Q*R2))**AK
      YB=Q*(AL+AT)/ABS(AL-AT)
      ELSE
      AL=LOG(TAN(0.25*PI-0.5*R1))
      AT=LOG(TAN(0.25*PI-0.5*R2))
      YB=(AL+AT)/ABS(AL-AT)
      ENDIF
      AB=2./ABS(AL-AT)
      RETURN
C*****************************
      ENTRY ZSTLMC(YBS,ANGLE)
      YB=YBS
      RANGL=ABS(ANGLE)*PI/180.
      IF(RANGL.EQ.0.) THEN
      AB=0.
      RETURN
      ELSEIF(Q.EQ.0.) THEN
      AB=1./LOG(TAN(.25*PI+.5*RANGL))
      ELSE
      AB=1./(TAN(0.5*RANGL))**AK
      ENDIF
      RETURN
C1.2************************
      ENTRY ZSTRUN(X1LIMS,X2LIMS,Y1LIMS,Y2LIMS)
C
C     First of all calculate centre of plot

C
      CXXS=0.5*(X2LIMS+X1LIMS)
      CYYS=0.5*(Y2LIMS+Y1LIMS)
C     Next bit of code copy of ZSTCEN
      CX=CXXS
      IF(CXXS.GT. 180.) CX=MOD(CXXS+180.,360.)-180.
      IF(CXXS.LE.-180.) CX=MOD(CXXS-180.,360.)+180.
      CY=CYYS
      CENT=XH*CX/180.
      CENTY=YH*CY/90.
      IF(CY.GE.0.) RC= 1.
      IF(CY.LT.0.) RC=-1.
      ALONS=CX*PI/180.
      ALATS=CY*PI/180.
      SALATS=SIN(ALATS)
      CALATS=COS(ALATS)
C
C     Now calculate bounds on plot in user coordinates
C
      X1LIM=(X1LIMS-CXXS)*XH/180.0
      X2LIM=(X2LIMS-CXXS)*XH/180.0
      Y1LIM=(Y1LIMS-CYYS)*YH/90.0
      Y2LIM=(Y2LIMS-CYYS)*YH/90.0
C
      RETURN
C1.2
      ENTRY ZSCOOR
      ICOORD=1-ICOORD
      RETURN
      ENTRY ZSTMOL(RATIO)
      ARM=RATIO
      RETURN
C1.3**********************************
      ENTRY ZSTSKP(INC)
      INCCST=INC
      RETURN
C1.3**********************************
      ENTRY ZSCHAR(ITTYPE,ZHEIGHT,ZFREQ)
      IF(ITTYPE.NE.0)THEN
      IZTYPE=ITTYPE
      ENDIF
      IF(ZHEIGHT.GT.0.0)THEN
      Z1CHAR=ZHEIGHT
      ENDIF
      IF(ZFREQ.GT.0.0)THEN
      Z1STEP=ZFREQ
      ENDIF
      RETURN
      END
