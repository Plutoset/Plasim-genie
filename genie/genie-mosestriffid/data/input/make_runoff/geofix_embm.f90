SUBROUTINE geofix(lat_trip,lon_trip,trip2ocn_mapping)

  IMPLICIT NONE

  REAL,INTENT(in) :: lat_trip,lon_trip
  INTEGER,INTENT(inout) :: trip2ocn_mapping

  trip2ocn_mapping=999

 !Hudson bay (unresolved)
 IF(lat_trip>=50.0  .and. &
    lat_trip<=65.0  .and. &
    lon_trip>=-98.0 .and. &
    lon_trip<=-75.0       &
   ) trip2ocn_mapping=260
 !Baltic/North sea (unresolved)
 IF(lat_trip>=50.0 .and. &
    lat_trip<=60.0 .and. &
    lon_trip>=0.0  .and. &
    lon_trip<=40.0       &
   ) trip2ocn_mapping=265
 !Black sea (unresolved)
 IF(lat_trip>=40.0 .and. &
    lat_trip<=50.0 .and. &
    lon_trip>=25.0 .and. &
    lon_trip<=40.0       &
   ) trip2ocn_mapping=218
 !Red sea (unresolved)
 IF(lat_trip>=10.0 .and. &
    lat_trip<=30.0 .and. &
    lon_trip>=30.0 .and. &
    lon_trip<=50.0       &
   ) trip2ocn_mapping=1 !162
 !Lake Chad (endorheic)
 IF(lat_trip>=0.0  .and. &
    lat_trip<=27.0 .and. &
    lon_trip>=0.0  .and. &
    lon_trip<=30.0       &
   ) trip2ocn_mapping=881
 !Caspian sea (endorheic)
 IF(lat_trip>=30.0 .and. &
    lat_trip<=65.0 .and. &
    lon_trip>=40.0 .and. &
    lon_trip<=70.0       &
   ) trip2ocn_mapping=882
 !Himalaya (endorheic)
 IF(lat_trip>=30.0  .and. &
    lat_trip<=65.0  .and. &
    lon_trip>=70.0  .and. &
    lon_trip<=120.0       &
   ) trip2ocn_mapping=883
 
  RETURN

END SUBROUTINE geofix
