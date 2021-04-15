      SUBROUTINE SOLANG (LDIUR,DOY,YCLOCK,ALAT,alon,
     :     AMU0,RDAYL,CDISSEM,alb_spec)

      implicit none

C**********************************************************
C             SUBROUTINE SOLANG
C**********************************************************
C Pm.M. deF   27-1-98
C inputs
C LDIUR  ! Logical for diurnal average
C DOY    ! Julian day of year
C YCLOCK !
C ALAT   ! Latitude in degrees
C ALON   ! Longitude in degrees
C outputs
C AMU0   ! Cosine of solar zenith angle
C RDAYL  ! Fractional day length
C CDISSEM  ! Suns relative distance as a fraction

C**** *SOLANG* - FOR SOLAR ZENITH ANGLE AND RELATIVE DAYLENGTH.
C
C     PURPOSE.
C     --------
C
C          THIS ROUTINE GIVES DIFFERENT RESULTS DEPENDING ON A LOGICAL
C     SWITCH. IF LDIUR IS TRUE ONE OBTAINS ACTUAL SOLAR ZENITH ANGLES
C     AND VALUES OF ONE OR ZERO DEPENDING ON THE SIGN OF THE FORMER. IF
C     LDIUR IS FALSE ONE GETS THE SAME ANSWERS AT ALL POINTS, I.E. MEAN
C     VALUE OF THE DAYTIME SOLAR ZENITH ANGLE AND RELATIVE LENGTH OF
C     THE DAY.
C
C   LDIUR .... true: sun at time, false: diurnally averaged
C
C     ----------
C
c
c     DJL - This routine appears to be extrememly sensitive to the order of
c       certain claculations, which shouldn't in theory affect any results.
c
c     ----------------------------------------
c     For implicit none...........
      real zdecli,crae,zcrae,zsin,zsqcst,zeqtim,
     :     zzen1,zzen2,zzen3,ztim1,ztim2,ztim3,
     :     coslon,sinlon,zl,zs1,zs2,zs3,sigma2
      real p_zytime,p_zeccsq,p_zecccu,p_zytim2,
     :     p_zytv,p_zdisse,p_zdecli,p_zeqtim
      integer jlon
c     ----------------------------------------   

      REAL DOY,ALAT,RDAYL,AMU0,YCLOCK,ALON,XLON
C
      REAL XLAT,YTIME,YEARL,API,ZC1YT,ZS1YT,ZC2YT,ZS2YT,CDISSEM
      INTEGER JDAY

      REAL ZMU0(128),ZRDAYL(128)
      real zalb_spec(128)
      real alb_spec
      real cosz
      include 'varalb.cmn'
      LOGICAL LDIUR
      REAL ZCDIS(5),ZCEQT(5),ZCDEC(5)
      DATA ZCDIS/+1.000110,+0.034221,+0.001280,+0.000719,+0.000077/
      DATA CRAE/+0.1277E-02/
      DATA ZCDEC/+0.006918,-0.399912,+0.070257,-0.006758,+0.000907/
      DATA ZCEQT/+0.000075,+0.001868,-0.032077,-0.014615,-0.040849/

c     ----------------------------------------
c     For variable orbit..........
      include 'igcm_orbit.cmn'
      integer time1,time2
      real timefrac
      INTEGER IFIRST
      data ifirst/1/
      save ifirst
      real, allocatable, dimension(:) :: zecc_vect
      real, allocatable, dimension(:) :: zobl_vect
      real, allocatable, dimension(:) :: zw_vect
      real, allocatable, dimension(:) :: zproff_vect
      save zecc_vect,zobl_vect,zw_vect,zproff_vect
      integer ntimes
      save ntimes
      integer ifail,ncid
      logical lexist
c     ----------------------------------------

      YEARL=360.0   ! 360 DAY YEAR
      API=2.0*ASIN(1.0)
      XLAT=API*alat/180.0
      XLON=API*ALON/180.0
      JDAY=INT(DOY)
      YTIME = (REAL(JDAY)+YCLOCK/2./API)/YEARL*2.*API
C
C diurnal cycle part
C
C*    COMPUTATIONAL CONSTANTS.
C     ------------- ----------
C
      ZC1YT=COS(YTIME)
      ZS1YT=SIN(YTIME)
      ZC2YT=ZC1YT**2-ZS1YT**2
      ZS2YT=2.*ZS1YT*ZC1YT
      CDISSEM=ZCDIS(1)+ZCDIS(2)*ZC1YT+ZCDIS(3)*ZS1YT+ZCDIS(4)*ZC2YT
     *       +ZCDIS(5)*ZS2YT
C
      ZCRAE=CRAE*(CRAE+2.)
C     ------------------------------------------------------------------
C*         2.     SOLAR ANGLE AND OZONE/AEROSOL PARAMETERS COMPUTATIONS.
C                 ----- ----- --- ------------- ---------- -------------
C
C
C*         2.1     INTRODUCE THE LATITUDE DEPENDENCY.
      ZSIN = SIN(XLAT)
      ZSQCST = SQRT(1.0-ZSIN**2)
      ZDECLI=ZCDEC(1)+ZCDEC(2)*ZC1YT+ZCDEC(3)*ZS1YT+ZCDEC(4)*ZC2YT
     *       +ZCDEC(5)*ZS2YT
      ZEQTIM=ZCEQT(1)+ZCEQT(2)*ZC1YT+ZCEQT(3)*ZS1YT+ZCEQT(4)*ZC2YT
     *       +ZCEQT(5)*ZS2YT

c     ++++++++++++++++++++++++++++++++++++++++++++++++
c     This is for if we have time-varying orbital paramters

      if (vary_exact_orbit) then

      if (ifirst.eq.1) then

         ifail=0
         if (fname_orbit_igcm.eq.'xxx') then
            print*,' Missing filename for igcm orbit'
            ifail=1
         else
            inquire(file=trim(fname_orbit_igcm),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :       trim(fname_orbit_igcm)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop
         end if
         print*,' igcm: Opening orbit file for read(1): ',
     &            trim(fname_orbit_igcm)
         call open_file_nc(
     &       trim(fname_orbit_igcm),ncid)
         call get1di_data_nc(ncid,'ntimes',1,ntimes,ifail)
         if (ifail.ne.0) then
           print*,'variable ntimes not found in igcm orbit'
           stop
         end if
         call close_file_nc(
     &       trim(fname_orbit_igcm),ncid)

         print*,'ntimes in fixedchem = ',ntimes   
          allocate(zecc_vect(ntimes))
          allocate(zobl_vect(ntimes))
          allocate(zw_vect(ntimes))
          allocate(zproff_vect(ntimes))

         print*,' igcm: Opening orbit file for read(2): ',
     &            trim(fname_orbit_igcm)
         call open_file_nc(
     &       trim(fname_orbit_igcm),ncid)
         call get1d_data_nc(ncid,'zecc',ntimes,zecc_vect,ifail)
         if (ifail.ne.0) then
           print*,'variable zecc not found in igcm orbit'
           stop
         end if   
         call get1d_data_nc(ncid,'zobl',ntimes,zobl_vect,ifail)
         if (ifail.ne.0) then
           print*,'variable zobl not found in igcm orbit'
           stop
         end if   
         call get1d_data_nc(ncid,'zw',ntimes,zw_vect,ifail)
         if (ifail.ne.0) then
           print*,'variable zw not found in igcm orbit'
           stop
         end if   
         call get1d_data_nc(ncid,'zproff',ntimes,zproff_vect,ifail)
         if (ifail.ne.0) then
           print*,'variable zproff not found in igcm orbit'
           stop
         end if   
         call close_file_nc(
     &       trim(fname_orbit_igcm),ncid)     

      ifirst=0
      endif ! end of if(ifirst)


      if (ichange_orbit.eq.1 ) then

      ichange_orbit=0

      time1=int(daynum/real(dt_vals) + 1)
      time2=time1+1
      timefrac=mod(daynum,dt_vals)/real(dt_vals)

      print*,'in solang:daynum,iniday,time1,time2,timefrac ',
     :             daynum,iniday,time1,time2,timefrac

      if (time1.le.0) then
      print*,'igcm: warning, too early for orbital record'
      p_zecc=zecc_vect(1)
      p_zobl=zobl_vect(1)
      p_zw=zw_vect(1)
      p_zproff=zproff_vect(1)
      print*,'p_zecc=',p_zecc
      endif

      if ( (time2.le.ntimes).and.(time1.gt.0) ) then
      print*,'igcm orbit: interpolating times...'
      p_zecc=zecc_vect(time1)*(1-timefrac) + 
     :         zecc_vect(time2)*(timefrac)
      p_zobl=zobl_vect(time1)*(1-timefrac) + 
     :         zobl_vect(time2)*(timefrac)
      p_zw=zw_vect(time1)*(1-timefrac) + 
     :         zw_vect(time2)*(timefrac)
      p_zproff=zproff_vect(time1)*(1-timefrac) + 
     :         zproff_vect(time2)*(timefrac)
      print*,'p_zecc=',p_zecc
      endif

      if (time2.gt.ntimes) then
      print*,'igcm: warning, too late for orbital record'
      p_zecc=zecc_vect(ntimes)
      p_zobl=zobl_vect(ntimes)
      p_zw=zw_vect(ntimes)
      p_zproff=zproff_vect(ntimes)
      print*,'p_zecc=',p_zecc
      endif

c     ------------------------
c     Copied from iniphys.f:
      p_zobl=p_zobl*api/180.
      p_zw=p_zw*api/180.        
c     Should change this to 360......?
      p_zproff=p_zproff*2.*api/365.
c     ------------------------

      endif ! end of if(ichange)

      endif ! end of if(varying orbit).

c     ++++++++++++++++++++++++++++++++++++++++++++++++


c+djl*****PAUL'S CODE:.....      
      if (exact_orbit) then
      p_zytime=ytime
      p_ZECCSQ=p_ZECC*p_ZECC
      p_ZECCCU=p_ZECC*p_ZECC*p_ZECC
      p_ZYTIM2=p_ZYTIME+p_ZPROFF
      p_ZYTV=p_ZYTIM2 + (2.*p_ZECC-p_ZECCCU/4.)*SIN(p_ZYTIM2)
     :+1.25*p_ZECCSQ*SIN(2.*p_ZYTIM2)+13./12.*p_ZECCCU*SIN(3.*p_ZYTIM2)
      p_ZDISSE=((1. + p_ZECC*COS(p_ZYTV))/(1.-p_ZECCSQ))**2.
      p_ZDECLI=ASIN(SIN(p_ZOBL)*SIN(p_ZYTV+p_ZW))
c     ----------------------------
c     this line was originally commented out by PJV, and replaced by the zero
c      p_ZEQTIM=p_ZYTIM2-p_ZYTV
      p_ZEQTIM=0.
c     ----------------------------
      cdissem=p_ZDISSE
      zdecli=p_ZDECLI
      zeqtim=p_ZEQTIM
      endif
c-djl*****END OF PAUL'S CODE:.....      


      ZZEN1=SIN(ZDECLI)
      ZZEN2=COS(ZDECLI)*COS(YCLOCK+ZEQTIM)
      ZZEN3=COS(ZDECLI)*SIN(YCLOCK+ZEQTIM)
C
      ZTIM1 = ZZEN1 * ZSIN
      ZTIM2 =-ZZEN2 * ZSQCST
      ZTIM3 = ZZEN3 * ZSQCST

C
C     ---------------------------------------------------------------
C*         2.     COMPUTATIONS IF DIURNAL CYCLE "ON".
C                 ------------ -- ------- ----- -----
C
c      COSLON=1.0
c      SINLON=0.0
      COSLON=COS(XLON)
      SINLON=SIN(XLON)
      IF(LDIUR) THEN
        AMU0=ZTIM1+ZTIM2*COSLON+ZTIM3*SINLON
        IF(AMU0.GE.0.)THEN
          AMU0=AMU0
          RDAYL=1.0
        ELSE
          AMU0=0.0
          RDAYL=0.0
        ENDIF
C                 ------------------------------------
C*         3.     COMPUTATIONS IF DIURNAL CYCLE "OFF".
C                 ------------ -- ------- ----- ------
      ELSE
        DO 301 JLON=1,128
          ZL=2.*API*(JLON-1.)/128.
          ZMU0(JLON)=ZTIM1+ZTIM2*COS(ZL)+ZTIM3*SIN(ZL)
          IF(ZMU0(JLON).GE.0.)THEN
            ZMU0(JLON)=ZMU0(JLON)
            ZRDAYL(JLON)=1.0
          ELSE
            ZMU0(JLON)=0.0
            ZRDAYL(JLON)=0.0
          ENDIF
  301   CONTINUE

      if (lvar_albedo) then
        DO JLON=1,128
          ZL=2.*API*(JLON-1.)/128.
          cosz=ZTIM1+ZTIM2*COS(ZL)+ZTIM3*SIN(ZL)
          IF(cosz.GE.0.)THEN
          zalb_spec(jlon)=cosz*
     :       (2.6e-2/(cosz**1.7+0.065)+
     :        0.15*(cosz-0.1)*(cosz-0.5)*(cosz-1.0))
          ELSE
            zalb_spec(jlon)=0.0
          ENDIF
        enddo
      endif

        ZS1=SIGMA2(128,ZMU0(1),1)
        ZS2=SIGMA2(128,ZRDAYL(1),1)
        IF(ZS2.NE.0.) THEN
          ZS1=ZS1/ZS2
          ZS2=ZS2/128.
        END IF
        AMU0=ZS1
        RDAYL=ZS2

      if (lvar_albedo) then
        ZS3=SIGMA2(128,zalb_spec(1),1)
        IF(ZS2.NE.0.) THEN
          ZS3=ZS3/(ZS2*128.0)
        END IF
        if (zs1.ne.0) then
        alb_spec=zs3/zs1
        else
        alb_spec=0.3
        endif
      endif

      END IF
C
      AMU0=CRAE/(SQRT(AMU0**2+ZCRAE)-AMU0)

      RETURN
      END
