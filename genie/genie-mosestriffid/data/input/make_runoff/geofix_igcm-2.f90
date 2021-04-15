SUBROUTINE geofix(lat_trip,lon_trip,trip2ocn_mapping)

  IMPLICIT NONE

  REAL,INTENT(in) :: lat_trip,lon_trip
  INTEGER,INTENT(inout) :: trip2ocn_mapping

  trip2ocn_mapping=999

  !####################################
  ! Unresolved water bodies
  !####################################
  !Black sea (unresolved)
  IF(lat_trip>=40.0 .and. &
     lat_trip<=50.0 .and. &
     lon_trip>=10.0 .and. &
     lon_trip<=40.0       &
    ) trip2ocn_mapping=116
  !Red sea (unresolved)
  IF(lat_trip>=10.0 .and. &
     lat_trip<=28.0 .and. &
     lon_trip>=30.0 .and. &
     lon_trip<=50.0       &
    ) trip2ocn_mapping=174
  !Euphrates/Persian Gulf (unresolved)
  IF(lat_trip>=28.0 .and. &
     lat_trip<=40.0 .and. &
     lon_trip>=40.0 .and. &
     lon_trip<=50.0       &
    ) trip2ocn_mapping=148
 !Hudson bay (unresolved)
 IF(lat_trip>=50.0  .and. &
    lat_trip<=65.0  .and. &
    lon_trip>=262.0 .and. &
    lon_trip<=285.0       &
   ) trip2ocn_mapping=73
 !Baltic/North sea (unresolved)
 IF(lat_trip>=50.0 .and. &
    lat_trip<=60.0 .and. &
    lon_trip>=0.0  .and. &
    lon_trip<=40.0       &
   ) trip2ocn_mapping=97

 !Ross Bay? (unresolved)
 IF(lat_trip>=-90.0 .and. &
    lat_trip<=-50.0 .and. &
    lon_trip>=150.0 .and. &
    lon_trip<=220.0       &
   ) trip2ocn_mapping=325

 !Weddell Sea1 (unresolved)
 IF(lat_trip>=-90.0 .and. &
    lat_trip<=-50.0 .and. &
    lon_trip>=270.0 .and. &
    lon_trip<=320.0       &
   ) trip2ocn_mapping=348
 !Weddell Sea2 (unresolved)
 IF(lat_trip>=-90.0 .and. &
    lat_trip<=-50.0 .and. &
    lon_trip> 320.0 .and. &
    lon_trip<=359.0       &
   ) trip2ocn_mapping=350
 !Queen Mary Coast (unresolved)
 IF(lat_trip>=-90.0 .and. &
    lat_trip<=-50.0 .and. &
    lon_trip>= 90.0 .and. &
    lon_trip<=105.0       &
   ) trip2ocn_mapping=290

 !Greenland1 (unresolved)
 IF(lat_trip>=80.0  .and. &
    lat_trip<=90.0  .and. &
    lon_trip>=320.0 .and. &
    lon_trip<=340.0       &
   ) trip2ocn_mapping=4
 !Greenland2 (unresolved)
 IF(lat_trip>=65.0  .and. &
    lat_trip<=90.0  .and. &
    lon_trip>=300.0 .and. &
    lon_trip<=320.0       &
   ) trip2ocn_mapping=55
 !Greenland3 (unresolved)
 IF(lat_trip>=65.0  .and. &
    lat_trip<=82.0  .and. &
    lon_trip>=320.0 .and. &
    lon_trip<=340.0       &
   ) trip2ocn_mapping=56

  !####################################
  ! Internal drainage basins
  !####################################
  !Lake Chad (endorheic)
  IF(lat_trip>=0.0  .and. &
     lat_trip<=27.0 .and. &
     lon_trip>=0.0  .and. &
     lon_trip<=30.0       &
    ) trip2ocn_mapping=881
  !Kalahari (endorheic)
  IF(lat_trip>=-40.0 .and. &
     lat_trip<=-10.0 .and. &
     lon_trip>=10.0  .and. &
     lon_trip<=30.0        &
    ) trip2ocn_mapping=882
  !Himalaya (endorheic)
  IF(lat_trip>=30.0  .and. &
     lat_trip<=65.0  .and. &
     lon_trip>=60.0  .and. &
     lon_trip<=120.0       &
    ) trip2ocn_mapping=883
  !Saudi desert (endorheic)
  IF(lat_trip>=15.0  .and. &
     lat_trip<=25.0  .and. &
     lon_trip>=45.0  .and. &
     lon_trip<=55.0        &
    ) trip2ocn_mapping=884
  !Caspian sea (endorheic)
  IF(lat_trip>=30.0 .and. &
     lat_trip<=65.0 .and. &
     lon_trip>=40.0 .and. &
     lon_trip<=70.0       &
    ) trip2ocn_mapping=885
  !Aussie Outback (endorheic)
  IF(lat_trip>=-40.0 .and. &
     lat_trip<=-20.0 .and. &
     lon_trip>=130.0 .and. &
     lon_trip<=145.0       &
    ) trip2ocn_mapping=886

  RETURN

END SUBROUTINE geofix
