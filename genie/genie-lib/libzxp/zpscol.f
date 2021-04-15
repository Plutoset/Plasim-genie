c
      SUBROUTINE ZPSCOL(LINCOL,CHARCL)
      integer lincol
      character*3 charcl
c
      IF (LINCOL.EQ.0) THEN
         CHARCL='sbk'
      ELSEIF (LINCOL.EQ.1) THEN
         CHARCL='srd'
      ELSEIF (LINCOL.EQ.2) THEN
         CHARCL='sgr'
      ELSEIF (LINCOL.EQ.3) THEN
         CHARCL='sbl'
      ELSEIF (LINCOL.EQ.4) THEN
         CHARCL='swh'
      ELSEIF (LINCOL.EQ.5) THEN
         CHARCL='sg1'
      ELSEIF (LINCOL.EQ.6) THEN
         CHARCL='sg2'
      ELSEIF (LINCOL.EQ.7) THEN
         CHARCL='sg3'
      ELSEIF (LINCOL.EQ.8) THEN
         CHARCL='scy'
      ELSEIF (LINCOL.EQ.9) THEN
         CHARCL='sa1'
      ELSEIF (LINCOL.EQ.10) THEN
         CHARCL='sa2'
      ELSEIF (LINCOL.EQ.11) THEN
         CHARCL='sa3'
      ELSEIF (LINCOL.EQ.12) THEN
         CHARCL='sa4'
      ELSEIF (LINCOL.EQ.13) THEN
         CHARCL='sa5'
      ELSE
         CHARCL='sa6'
      END IF
c
      return
      end
