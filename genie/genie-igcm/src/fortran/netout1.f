      SUBROUTINE NETOUT1

      IMPLICIT NONE

C     
C     This subroutine gives output of dynamic fields
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'bats.cmn'
      include 'gridp.cmn'
      include 'legau.cmn'
      include 'outcon.cmn'
      include 'gridss.cmn'
      include 'restor.cmn'
      include 'netdata.cmn'
      include 'means.cmn'
C     
      INTEGER J,L,IHEM,JJ,IOF,I
      REAL LUG(MG,JGG,NL),LTG(MG,JGG,NL),lspg(mg,jgg),lmspg(mg,jgg)
      real lu_pg(MG,JGG,NL),lv_pg(MG,JGG,NL),lt_pg(MG,JGG,NL)
      real lq_pg(MG,JGG,NL),lg_pg(MG,JGG,NL)
      integer loc_dim
C     
C     Output is wanted.
C     
      DO J=1,JG
         DO L=1,NL
            DO IHEM=1,NHEM
               IF (IHEM.EQ.1) THEN
                  JJ=J
               ELSE
                  JJ=JGGP-J
               ENDIF
               IOF=MGPP*(IHEM-1)+(L-1)*IGC
               DO I=1,MG
                  LUG(I,JJ,L)=UG1(I+IOF,J)
                  LTG(I,JJ,L)=TG1(I+IOF,J)
                  LU_pG(I,JJ,L)=U_pG1(I+IOF,J)
                  LV_pG(I,JJ,L)=V_pG1(I+IOF,J)
                  LT_pG(I,JJ,L)=T_pG1(I+IOF,J)
                  LQ_pG(I,JJ,L)=Q_pG1(I+IOF,J)
                  LG_pG(I,JJ,L)=G_pG1(I+IOF,J)
               END DO
            end do
         end do
      end do
c     


      DO J=1,JG
         DO IHEM=1,NHEM
            IF (IHEM.EQ.1) THEN
               JJ=J
            ELSE
               JJ=JGGP-J
            ENDIF
            IOF=MGPP*(IHEM-1)
            DO I=1,MG
               LSPG(I,JJ)=SPG1(I+IOF,J)
               LmSPG(I,JJ)=mSPG1(I+IOF,J)
            END DO
         end do
      end do
C     
      call writevar2(nc(1),
     :     idvar(loc_dim('zonal_wind',varname(1,1),nall),1),
     :     lug,1,mg,1,jgg,1,nl,inetcount,inetcount)
      call writevar2(nc(1),
     :     idvar(loc_dim('temp',varname(1,1),nall),1),
     :     ltg,1,mg,1,jgg,1,nl,inetcount,inetcount)
      call writevar2(nc(1),
     :     idvar(loc_dim('zonal_wind_p',varname(1,1),nall),1),
     :     lu_pg,1,mg,1,jgg,1,nl,inetcount,inetcount)
      call writevar2(nc(1),
     :     idvar(loc_dim('northward_wind_p',varname(1,1),nall),1),
     :     lv_pg,1,mg,1,jgg,1,nl,inetcount,inetcount)
      call writevar2(nc(1),
     :     idvar(loc_dim('air_temperature_p',varname(1,1),nall),1),
     :     lt_pg,1,mg,1,jgg,1,nl,inetcount,inetcount)
      call writevar2(nc(1),
     :     idvar(loc_dim('specific_humidity_p',varname(1,1),nall),1),
     :     lq_pg,1,mg,1,jgg,1,nl,inetcount,inetcount)
      call writevar2(nc(1),
     :     idvar(loc_dim('geopotential_height_p',varname(1,1),nall),1),
     :     lg_pg,1,mg,1,jgg,1,nl,inetcount,inetcount)
      call writevar2(nc(1),
     :     idvar(loc_dim('pstar',varname(1,1),nall),1),
     :     lspg,1,mg,1,jgg,inetcount,inetcount,-1,-1)
      call writevar2(nc(1),
     :     idvar(loc_dim('mslp',varname(1,1),nall),1),
     :     lmspg,1,mg,1,jgg,inetcount,inetcount,-1,-1)
C     
      RETURN
      END
