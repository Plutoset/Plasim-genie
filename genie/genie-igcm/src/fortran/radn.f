      SUBROUTINE RADN(TROPHT)

      IMPLICIT NONE

C     
C     RADIATION SCHEME. MORCRETTE PMF 13/5/97
C     It passes the pressure of the
C     full sigma levels and the surface
C     to the Radiation scheme
C     (So the morcrette code has one more level than NL)
C     water vapour values come from QG and QSTAR
C     and temperatures from TG and TSTAR
C     
C     Dingmin's changes to morcrette
C     combine bottom levels
C     and set bottom level heating rate to zero as
C     this is now the ground and is not a layer
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'bats.cmn'
      include 'physca.cmn'
      include 'cpiers.cmn'
      include 'orog_g.cmn'
      include 'radht.cmn'
      include 'files.cmn'
      include 'climatologies.cmn'
      REAL QG(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))

      INTEGER NTSTEP,NSKIP,MTH1,L,I,IHEM,JH1,J,MTH2
      INTEGER IOFM,ILAST,IDOCALC,IMP,IMM,LD,K
      REAL    CHRF,ALAT1,SWALB,ALON,HTNETO,A,B

c     
c     Climatology ozone and water values. '1' is current month, '2' is
c     the next month. (SMR 30-05-97)
c     11-03-97 i.e. nl=15 read in from climatology. Interpolated to
c     nl.gt.15 in subroutine interpo3.
c     
      real o3clim1(15,mg,nhem,jg),h2oclim1(15,mg,nhem,jg)
      real o3clim2(15,mg,nhem,jg),h2oclim2(15,mg,nhem,jg)
c     
c     Morcrette profiles (have extra level).
c     
      REAL PR(NL+1),T(NL+1),h2o(nl+1),o3(nl+1),htlw(nl+1),htsw(nl+1)

      real o3mod1(nl,mg,nhem,jg) ! ozone interpolated to model
                                ! levels. Current month.
      real h2omod1(nl,mg,nhem,jg) ! water interpolated to model
                                ! levels. Current month.
      real o3mod2(nl,mg,nhem,jg) ! ozone interpolated to model
                                ! levels. Next month.
      real h2omod2(nl,mg,nhem,jg) ! water interpolated to model
                                ! levels. Next month.
      real tropht(mg,nhem,jg)   ! tropopause height.

      integer ifirst            ! If =1, first time reading o3
                                ! and h2o (2 months' worth).

      integer cmth              ! Current month counter. = month
                                ! number +1 'cos have 13 months
                                ! (dec in twice)
      character mn(13)*3        ! Month names. 13 to enable
                                ! wrap-around.
      real amfrac               ! fraction through month
*---------------------------
      integer ifirstcol         ! =1 first time through column
                                ! calculation (open new file).
      real ps                   ! sfc pressure (used in
                                ! interpolation from climatology
                                ! to model).
      integer im                ! Pointer for array plg (for
                                ! getting sfc pressure).

      integer ic(4,2)           !cloud positions (deep,h,m,l;bottom,top)
      real cf(4,2)              !cloud fraction,lwp deep,high,mid,low
C     
C     Array to hold fluxes at top and bottom of atmosphere
C     1st index - flux 1=SW, 2=LW
C     2nd index - Direction 1=DN, 2=UP
C     3rd index - Where 1=TOP, 2=SURFACE
C     
      real fluxes(2,2,2)

      save                      ! Want to keep things like dcompl.

      data mn/'jan','feb','mar','apr','may','jun','jul',
     &     'aug','sep','oct','nov','dec','dum'/


      DATA IFIRST/1/
      data ifirstcol/1/

      CHRF=86400.*WW*CT         ! non-dimensionalise heating rates
c     
c     Skipping part set up here.
c     Radiation scheme only called every nskip longitudes
c     nskip must divide exactly into mg for longitude.
c     ntstep is the number of timesteps to skip.
c     
      ntstep=itspd              ! N.B. No point any more frequent as
                                ! Morcrette doing diurnal averages 
                                ! anyway.
      if (jg.eq.16.and.mg.eq.64) then ! Crude variation of nskip
                                ! with latitude.
         nskip=4
         if (jh.eq.1) nskip=32
         if (jh.eq.2) nskip=16
         if (jh.eq.3.or.jh.eq.4.or.jh.eq.5) nskip=8
         if (jh.eq.13.or.jh.eq.14.or.jh.eq.15.or.jh.eq.16) nskip=2
      else if (jg.eq.32.and.mg.eq.128) then ! Crude variation of nskip
                                ! with latitude.
         nskip=8
         if (jh.eq.1) nskip=64 
         if (jh.eq.2) nskip=64
         if (jh.eq.3) nskip=32
         if (jh.eq.4) nskip=32
         if (jh.eq.5.or.jh.eq.6.or.jh.eq.7) nskip=16
         if (jh.eq.8.or.jh.eq.9.or.jh.eq.10) nskip=16
         if (jh.eq.25.or.jh.eq.26.or.jh.eq.27.or.jh.eq.28) nskip=4
         if (jh.eq.29.or.jh.eq.30.or.jh.eq.31.or.jh.eq.32) nskip=4
      else if (jg.eq.18.and.mg.eq.36) then ! Crude variation of nskip
                                ! with latitude.
! this loop is never actually used at run-time, but was added to avoid
! having to implement a conditional compilation (JK Hughes)
         nskip=4
         if (jh.eq.1) nskip=36
         if (jh.eq.2) nskip=18
         if (jh.eq.3.or.jh.eq.4.or.jh.eq.5) nskip=4
         if ((jh.ge.6).and.(jh.le.18)) nskip=2
      endif

c     
C     this sets the non-dimensional gridpoint temperature tendency 
C     (TTRD) to get to TTRD from K/day divide the K/day heating rate by
C     (86400*WW*CT)
C     TTRD(non-dim)=HTRT(k/day)/(86400*WW*CT)
C     
c     
C     Reads in bogus ozone and water vapour on first call other data 
C     such as albedo and number of levels is in the include file
C     


c     Read in required ozone and water vapour from climatology


      IF (ifirst.eq.1) then     ! First time through (read in
                                ! two months of o3 and h2o if
                                ! lperpet .false. or just one if
                                ! lperpet .true.).

         CALL CALNDR_igcm(DOY,MTH1,AMFRAC)
         CMTH=MTH1
         if (jh.eq.1) print*,' Reading ozone month ',cmth,mn(cmth)
         do l=1,15
            do i=1,mg
               do ihem=1,nhem
                  jh1=jh
                  if (ihem.eq.2) jh1=jgg+1-jh
                  o3clim1(l,i,ihem,jh)=ozone_clim(i,jh1,l,cmth)
                  h2oclim1(l,i,ihem,jh)=water_clim(i,jh1,l,cmth)
               end do
            end do
         end do
         CMTH=MTH1+1
         if (cmth.eq.13) cmth=1
         if (jh.eq.1) print*,' Reading ozone month ',cmth,mn(cmth)
         do l=1,15
            do i=1,mg
               do ihem=1,nhem
                  jh1=jh
                  if (ihem.eq.2) jh1=jgg+1-jh
                  o3clim2(l,i,ihem,jh)=ozone_clim(i,jh1,l,cmth)
                  h2oclim2(l,i,ihem,jh)=water_clim(i,jh1,l,cmth)
               end do
            end do
         end do
c     ----------------------------- Interpolate to model vertical grid.
         j =jh
         DO ihem=1,nhem
            DO i=1,mg
               im=i+(ihem-1)*mgpp
               ps=1.0E5
               call interpf(h2oclim1(1,i,ihem,j),h2omod1(1,i,ihem,j),ps)
               call O3INTERP(o3clim1(1,i,ihem,j),o3mod1(1,i,ihem,j),ps)
               call interpf(h2oclim2(1,i,ihem,j),h2omod2(1,i,ihem,j),ps)
               call O3INTERP(o3clim2(1,i,ihem,j),o3mod2(1,i,ihem,j),ps)
            end do
         end do
         IF (JH.EQ.JG) THEN
            IFIRST=0

!     Open file for column amount calculations.
            IF (ifirstcol.eq.1) then
               ifirstcol=0
c               open(unit=55,file='column.dat',status='new')
            ENDIF

c            write(55,*)'******** In ifirst loop'
c            write(55,*)'******** Start month is: ',mn(cmth)
c            write(55,*)'**********************************'
            call colamt(o3mod1,h2omod1)
         ENDIF

      ENDIF                     ! end of first time bit (reading in and
                                ! interpolating o3 and h2o).

c     -------------------- If it's time to change month of o3 (and h2o)
c     for a time-varying o3 run.
c     
      CALL CALNDR_igcm(DOY,MTH2,AMFRAC)
      IF (.not. lperpet) then
         IF (MTH1.NE.MTH2) THEN
c     begin smrdebug
            IF (JH.EQ.1) THEN
c               write(55,*)'**************************************'
c               write(55,*)'Start of ''time to change ozone'' block: '
c               write(55,*)'kount: ',kount
c               write(55,*)'kount/itspd i.e. no. of days: ',kount/itspd
c               write(55,*)'Month changing from ',mn(cmth),' to ',
c     &              mn(cmth+1)
c               write(55,*)'*********************************'
c     end smrdebug


!     Copy 'next' to 'current'.
               do j=1,jg        ! loop over latitude
                  do ihem=1,nhem !           hemispheres
                     do i=1,mg  !           longitude
                        do l=1,nl !           height
                           o3mod1(l,i,ihem,j)=o3mod2(l,i,ihem,j)
                           h2omod1(l,i,ihem,j)=h2omod2(l,i,ihem,j)
                        enddo
                     enddo
                  enddo
               enddo

!     And fetch new 'next':

            ENDIF

            CMTH=MTH2+1
            IF (cmth.eq.13) cmth=1 ! Wrap-around to January.
            if (jh.eq.1) print*,' Reading ozone month ',cmth,mn(cmth)
            do l=1,15
               do i=1,mg
                  do ihem=1,nhem
                     jh1=jh
                     if (ihem.eq.2) jh1=jgg+1-jh
                     o3clim2(l,i,ihem,jh)=ozone_clim(i,jh1,l,cmth)
                     h2oclim2(l,i,ihem,jh)=water_clim(i,jh1,l,cmth)
                  end do
               end do
            end do
c     
            j=jh
            do ihem=1,nhem      !           hemispheres
               do i=1,mg        !           longitude
                  im=i+(ihem-1)*mgpp
                  ps=1.0E5
                  call interpf(h2oclim2(1,i,ihem,j),h2omod2(1,i,ihem,j),
     :                 ps)
                  call O3INTERP(o3clim2(1,i,ihem,j),o3mod2(1,i,ihem,j),
     :                 ps)
               ENDDO            ! end of loop over longitude
            ENDDO               ! end of loop over hemispheres
c
            IF (JH.EQ.JG) THEN
               MTH1=MTH2        ! stops loop repeating
               call colamt(o3mod1,h2omod1)
            ENDIF
         ENDIF
      ENDIF
c     --------------------------------- Now start the radiation bit.
c     
C     loop over hemispheres
      IOFM=0
C     
      DO 800 ihem=1,nhem

c     calculates heating rates every ntstep time steps
c     
C     
         IF (mod(kount,ntstep).eq.1) then
C     
C     Does do Radn scheme
C     
C     
C     loop over longitudes for radn calculation
            ilast=0
            DO i=1,mg
               im=i+iofm
               idocalc=0
               IF ((i.eq.1).or.(i-ilast.ge.nskip)) then
                  idocalc=1
               ELSE
                  IF (LNNSK) THEN
                     imp=im+1
                     IF (imp.gt.(mg+iofm)) imp=1+iofm
                     imm=im-1
                     IF (((iland(im,jh).eq.1).and.
     $                    (iland(imp,jh).eq.0)).or.
     $                    ((iland(im,jh).eq.0).and.
     $                    (iland(imp,jh).eq.1)).or.
     $                    ((iland(im,jh).eq.1).and.
     $                    (iland(imm,jh).eq.0)).or.
     $                    ((iland(im,jh).eq.0).and.
     $                    (iland(imm,jh).eq.1))) THEN
                        idocalc=1
                     ENDIF
                  ENDIF
               ENDIF
               IF (idocalc.eq.1) then

c     ------------------------------ First set SURFACE VALUES
c     Pressure, units of Pa
                  PR(1)=PLG(im)*P0
c     T, units of K
                  T(1)=TSTAR(IM,JH)*CT
c     water, units of mmr

                  IF (lclim) then ! If using h2o from climatology.
                     h2o(1)=AMFRAC*h2omod2(NL,i,ihem,JH)+
     +                    (1.0-AMFRAC)*h2omod1(NL,i,ihem,JH)
                  ELSE          ! using h2o from model.
                     h2o(1)=QSTAR(IM,JH)
                  ENDIF

                  o3(1)=AMFRAC*o3mod2(NL,i,ihem,JH)+
     +                 (1.0-AMFRAC)*o3mod1(NL,i,ihem,JH)

c     --------------------------------------- Now set rest of column.

                  DO LD=1,NL    ! Start of loop over column.
                     L=NL+2-LD  ! Reverse index (Morc goes
                                ! bottom up).
                     PR(L)=SIGMA(LD)*PR(1) ! Pressure
                     T(L)=TG(im,ld)*CT ! Temperature

C     Water vapour, including dealing with minimum values.

                     IF (lclim) then ! Use h2o from climatology.

                        h2o(l)=AMFRAC*h2omod2(ld,i,ihem,JH)+
     +                       (1.0-AMFRAC)*h2omod1(ld,i,ihem,JH)

                        h2o(L)=MAX(6.0E-6,h2o(L))
C     fix water vapour for levels above climatological tropopause to be
C     6e-6 ppmv.
                        IF(PR(L).LE.TROPHT(i,ihem,JH))THEN
C     remove following line if no fix required
                           h2o(L)=6.0E-6
                        ENDIF   ! end of strat. water vapour fix

                     ELSE       ! h2o from model

                        h2o(L)=MAX(6.0E-6,QG(im,LD))

C     fix water vapour for levels above climatological tropopause
                        IF(PR(L).LE.TROPHT(i,ihem,JH))THEN
                           h2o(L)=6.0E-6
                        ENDIF   ! end of strat. water vapour fix
                     ENDIF

c     Ozone

                     o3(l)=AMFRAC*o3mod2(ld,i,ihem,JH)+
     +                    (1.0-AMFRAC)*o3mod1(ld,i,ihem,JH)


                  ENDDO         ! End of loop over column.
c     ----------------------------------------------------- And alat1

                  alat1=alat(JH)*REAL(-(ihem*2.)+3)

c     9-7-97 Piers' clouds
c     deep and shallow
c
                  cf(1,1)=max(cfrac(im,5),cfrac(im,4)) 
                  cf(2,1)=cfrac(im,3) !high cloud fraction
                  cf(3,1)=cfrac(im,2) ! mid cloud fraction
                  cf(4,1)=cfrac(im,1) !low cloud fraction

C     cloud levels reversed
c     choose lowest level for bottom
c bottom deep/shallow
                  ic(1,1)=max(icflag(im,4,1),icflag(im,5,1))
C     choose hightest level for top
c top deep/shallow
                  ic(1,2)=min(icflag(im,4,2),icflag(im,5,2)) 
C     reverse
                  ic(1,1)=NLP-ic(1,1)+1
                  ic(1,2)=NLP-ic(1,2)+1
C     makes sure bottom cloud isn't in bottom level
                  if (ic(1,1).eq.2) ic(1,1)=3
                  if (ic(1,2).eq.2) ic(1,1)=3

                  ic(2,1)=NLP-icflag(im,3,1)+1 ! high pos
                  ic(3,1)=NLP-icflag(im,2,1)+1 ! mid pos
                  ic(4,1)=NLP-icflag(im,1,1)+1 ! low pos

                  SWALB=SALB(IM,JH)
C     cloud cf and ic passed. fluxes returned.
C     which is net flux at TOA in profile
C     Call radiation scheme
                  alon=REAL(i-1)/REAL(mg)*360.0
                  call nikosrad(pr,t,h2o,o3,alat1,htlw,htsw,DOY,cf,ic,
     $                 fluxes,swalb,alon,iland(im,jh))

c     store net flux in PNET
                  PNET(IM,JH)=fluxes(1,1,1)-fluxes(1,2,1)+fluxes(2,1,1)-
     $                 fluxes(2,2,1)
                  SNET(IM,JH)=fluxes(1,1,2)-fluxes(1,2,2)+fluxes(2,1,2)-
     $                 fluxes(2,2,2)
                  rrflux(im,jh,1)=fluxes(1,1,2)
                  rrflux(im,jh,2)=fluxes(1,2,2)
                  rrflux(im,jh,3)=fluxes(2,1,2)
                  rrflux(im,jh,4)=fluxes(2,2,2)
                  rrflux(im,jh,5)=fluxes(1,1,1)
                  rrflux(im,jh,6)=fluxes(1,2,1)
                  rrflux(im,jh,7)=fluxes(2,1,1)
                  rrflux(im,jh,8)=fluxes(2,2,1)

                  DO l=nl,1,-1
c     bottom heating rate is zero in morecret
                     LD=NL+1-L
                     IM=I+IOFM
                     HTNETO=HTNET(IHem,JH,I,LD)
                     htnet(ihem,jh,i,ld)=htlw(l+1)+htsw(l+1)

c     sets this heating rate
                     TTRD(IM,LD)=(HTNETO
     $                    +HTNET(IHEM,JH,I,LD))/(CHRF*2.)

c     put in linear interpolation of heating rates between this
c     longitude and last one calculated (i-nskip)
                     IF (i-ilast.gt.1) then
                        DO j=ilast+1,i-1
                           a=REAL(j-ilast)/REAL(i-ilast)
                           b=1.-a
                           HTNETO=HTNET(IHEM,JH,J,LD)
                           htnet(ihem,jh,j,ld)=a*htnet(ihem,jh,i,ld)+
     $                          b*htnet(ihem,jh,ilast,ld)
                           im=j+iofm
                           TTRD(IM,LD)=(HTNETO
     $                          +HTNET(IHEM,JH,J,LD))/(CHRF*2.)
                           IF (l.eq.nl) then
                              pnet(im,jh)=a*pnet(i+iofm,jh)+
     $                             b*pnet(ilast+iofm,jh)
                              snet(im,jh)=a*snet(i+iofm,jh)+
     $                             b*snet(ilast+iofm,jh)
                              DO k=1,8
                                 rrflux(im,jh,k)=a*rrflux(i+iofm,jh,k)
     $                                +b*rrflux(ilast+iofm,jh,k)
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF
                  ENDDO
                  ilast=i
C     end of conditional execution of morcrette code
               ENDIF
C     end of loop over longitudes
            ENDDO
            IF (ilast.ne.mg) then
               DO j=ilast+1,mg
                  a=REAL(j-ilast)/REAL(mg+1-ilast)
                  b=1.-a
                  im=j+iofm
                  DO l=nl,1,-1
                     ld=nl+1-l
                     HTNETO=HTNET(IHEM,JH,J,LD)
                     htnet(ihem,jh,j,ld)=a*htnet(ihem,jh,1,ld)+
     $                    b*htnet(ihem,jh,ilast,ld)
                     TTRD(IM,LD)=(HTNET(IHEM,JH,J,LD)
     $                    +HTNETO)/(CHRF*2.)
                     IF (l.eq.nl) then
                        pnet(im,jh)=a*pnet(1+iofm,jh)+
     $                       b*pnet(ilast+iofm,jh)
                        snet(im,jh)=a*snet(1+iofm,jh)+
     $                       b*snet(ilast+iofm,jh)
                        DO k=1,8
                           rrflux(im,jh,k)=a*rrflux(1+iofm,jh,k)
     $                          +b*rrflux(ilast+iofm,jh,k)
                        ENDDO
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
c     if (ihem.eq.2) print *, 'rad ',jh,(pnet(im1,jh),im1=1,IGC)
         ELSE
C     
c     Doesn't do rad scheme (simply uses old heating rates)
C     
            DO i=1,mg
               DO LD=1,NL
                  im=i+IOFM
                  TTRD(im,LD)=(htnet(ihem,jh,i,ld))/CHRF
               ENDDO
            ENDDO
         ENDIF
         IOFM=MGPP
 800  CONTINUE                  ! end of loop over hemispheres
      IF (LSHORT.AND.(KOUNT.eq.1)) then
         DO l=1,nl
            DO i=1,igc
               ttrd(i,l)=ttrd(i,l)*2.
            ENDDO
         ENDDO
      ENDIF

      RETURN
      END
