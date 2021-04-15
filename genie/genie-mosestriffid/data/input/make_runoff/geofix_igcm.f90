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
    ) trip2ocn_mapping=152
  !Euphrates/Persian Gulf (unresolved)
  IF(lat_trip>=30.0 .and. &
     lat_trip<=40.0 .and. &
     lon_trip>=40.0 .and. &
     lon_trip<=50.0       &
    ) trip2ocn_mapping=169

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
  !Aussie Outback (endorheic)
  IF(lat_trip>=-40.0 .and. &
     lat_trip<=-20.0 .and. &
     lon_trip>=130.0 .and. &
     lon_trip<=145.0       &
    ) trip2ocn_mapping=885
 
  RETURN

END SUBROUTINE geofix
