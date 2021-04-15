      SUBROUTINE INIGS(Z,GSG,NN,MM,NHEM,MOCT,NWJ2,MG,JG,AW,CS,TRIG,
     :                 IFAX,ALP,DALP,RLP,RDLP,POLY,CMPA,IOPT)
c     :                 WORK,IFAX,ALP,DALP,RLP,RDLP,POLY,CMPA,IOPT)
      IMPLICIT NONE
      INTEGER NN,MM,NHEM,MOCT,NWJ2,MG,JG,IOPT,IFAX(*)
      COMPLEX Z(*)
c      REAL GSG((MG+2)*NHEM,JG),AW(*),CS(*),TRIG(*),WORK(*)
      REAL GSG((MG+2)*NHEM,JG),AW(*),CS(*),TRIG(*)
      REAL ALP(NWJ2+NWJ2,JG),DALP(NWJ2+NWJ2,JG),
     :     RLP(NWJ2+NWJ2,JG),RDLP(NWJ2+NWJ2,JG)
      REAL POLY(NWJ2,NHEM)
      COMPLEX CMPA(*)
C
      INTEGER I,JH
C
C      INITILISE FIELDS TO ZERO AND NONDIMESIONALISE
C
      IF (IOPT.GE.0) THEN
         DO I=1,NWJ2*NHEM
            Z(I)=CMPLX(0.0,0.0)
         ENDDO
         DO JH=1,JG
C
C           CALL FFT991(GSG(1,JH),WORK,TRIG,IFAX,1,MG+2,MG,NHEM,-1)
C
            CALL FFT(GSG(1,JH),MG+2,MG,NHEM,-1,TRIG,IFAX)
C
            IF (IOPT.EQ.1) THEN
               CALL HANAL_lib(GSG(1,JH),Z,1,1,POLY,ALP,DALP,RLP,RDLP,AW,
     :              CS,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
            ELSE IF (IOPT.EQ.2) THEN
               CALL HANAL_lib(GSG(1,JH),Z,1,2,POLY,ALP,DALP,RLP,RDLP,AW,
     :              CS,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
            ELSE IF (IOPT.EQ.11) THEN
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)*SQRT(CS(JH))
               END DO
               CALL HANAL_lib(GSG(1,JH),Z,1,1,POLY,ALP,DALP,RLP,RDLP,AW,
     :              CS,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
            ELSE IF (IOPT.EQ.12) THEN
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)*SQRT(CS(JH))
               END DO
               CALL HANAL_lib(GSG(1,JH),Z,1,2,POLY,ALP,DALP,RLP,RDLP,AW,
     :              CS,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
            ELSE
               PRINT*,' This option not coded ',IOPT
               STOP
            END IF
C
         ENDDO
      ENDIF
C
      IF (IOPT.LE.0) THEN
         DO JH=1,JG
            IF (IOPT.EQ.-10) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,3,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/SQRT(CS(JH))
               END DO
            ELSE IF (IOPT.EQ.-9) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,1,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-8) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,2,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-7) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,4,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/SQRT(CS(JH))
               END DO
            ELSE IF (IOPT.EQ.-6) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,4,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/CS(JH)
               END DO
            ELSE IF (IOPT.EQ.-5) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,1,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-4) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,2,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-3) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,3,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/CS(JH)
               END DO
            ELSE IF (IOPT.EQ.-2) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,2,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-1) THEN
               CALL HEXP_lib(Z,GSG(1,JH),1,1,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE
               PRINT*,' This option not coded yet ',IOPT
               STOP
            END IF
C
            IF (IOPT.EQ.-4.OR.IOPT.EQ.-5) THEN
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/CS(JH)
               END DO 
               CALL EIM(GSG(1,JH),GSG(1,JH),1,CMPA,((MG+2)*NHEM)/2)
            ELSE IF (IOPT.EQ.-8.OR.IOPT.EQ.-9) THEN
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/sqrt(CS(JH))
               END DO 
               CALL EIM(GSG(1,JH),GSG(1,JH),1,CMPA,((MG+2)*NHEM)/2)
            END IF
C
C           CALL FFT991(GSG(1,JH),WORK,TRIG,IFAX,1,MG+2,MG,NHEM,1)
C
            CALL FFT(GSG(1,JH),MG+2,MG,NHEM,1,TRIG,IFAX)
C
         ENDDO
      ENDIF
C
      RETURN
      END
c
      SUBROUTINE INIGS8(Z,GSG,NN,MM,NHEM,MOCT,NWJ2,MG,JG,AW,CS,TRIG,
     :                 IFAX,ALP,DALP,RLP,RDLP,POLY,CMPA,IOPT)
c     :                 WORK,IFAX,ALP,DALP,RLP,RDLP,POLY,CMPA,IOPT)
      IMPLICIT NONE
      INTEGER NN,MM,NHEM,MOCT,NWJ2,MG,JG,IOPT,IFAX(*)
      COMPLEX*16 Z(*)
      REAL*8 GSG((MG+2)*NHEM,JG),AW(*),CS(*),TRIG(*)
c      REAL*8 GSG((MG+2)*NHEM,JG),AW(*),CS(*),TRIG(*),WORK(*)
      REAL*8 ALP(NWJ2+NWJ2,JG),DALP(NWJ2+NWJ2,JG),
     :     RLP(NWJ2+NWJ2,JG),RDLP(NWJ2+NWJ2,JG)
      REAL*8 POLY(NWJ2,NHEM)
      COMPLEX*16 CMPA(*)
C
      INTEGER I,JH
C
C      INITILISE FIELDS TO ZERO AND NONDIMESIONALISE
C
      IF (IOPT.GE.0) THEN
         DO I=1,NWJ2*NHEM
            Z(I)=CMPLX(0.0,0.0)
         ENDDO
         DO JH=1,JG
C
C           CALL FFT991(GSG(1,JH),WORK,TRIG,IFAX,1,MG+2,MG,NHEM,-1)
C
            CALL FFT8(GSG(1,JH),MG+2,MG,NHEM,-1,TRIG,IFAX)
C
            IF (IOPT.EQ.1) THEN
               CALL HANAL8(GSG(1,JH),Z,1,1,POLY,ALP,DALP,RLP,RDLP,AW,
     :              CS,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
            ELSE IF (IOPT.EQ.2) THEN
               CALL HANAL8(GSG(1,JH),Z,1,2,POLY,ALP,DALP,RLP,RDLP,AW,
     :              CS,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
            ELSE IF (IOPT.EQ.11) THEN
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)*SQRT(CS(JH))
               END DO
               CALL HANAL8(GSG(1,JH),Z,1,1,POLY,ALP,DALP,RLP,RDLP,AW,
     :              CS,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
            ELSE IF (IOPT.EQ.12) THEN
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)*SQRT(CS(JH))
               END DO
               CALL HANAL8(GSG(1,JH),Z,1,2,POLY,ALP,DALP,RLP,RDLP,AW,
     :              CS,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
            ELSE
               PRINT*,' This option not coded ',IOPT
               STOP
            END IF
C
         ENDDO
      ENDIF
C
      IF (IOPT.LE.0) THEN
         DO JH=1,JG
            IF (IOPT.EQ.-10) THEN
               CALL HEXP8(Z,GSG(1,JH),1,3,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)*SQRT(CS(JH))
               END DO
            ELSE IF (IOPT.EQ.-9) THEN
               CALL HEXP8(Z,GSG(1,JH),1,1,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-8) THEN
               CALL HEXP8(Z,GSG(1,JH),1,2,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-7) THEN
               CALL HEXP8(Z,GSG(1,JH),1,4,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)*SQRT(CS(JH))
               END DO
            ELSE IF (IOPT.EQ.-6) THEN
               CALL HEXP8(Z,GSG(1,JH),1,4,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/CS(JH)
               END DO
            ELSE IF (IOPT.EQ.-5) THEN
               CALL HEXP8(Z,GSG(1,JH),1,1,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-4) THEN
               CALL HEXP8(Z,GSG(1,JH),1,2,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-3) THEN
               CALL HEXP8(Z,GSG(1,JH),1,3,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/CS(JH)
               END DO
            ELSE IF (IOPT.EQ.-2) THEN
               CALL HEXP8(Z,GSG(1,JH),1,2,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE IF (IOPT.EQ.-1) THEN
               CALL HEXP8(Z,GSG(1,JH),1,1,
     :         ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
            ELSE
               PRINT*,' This option not coded yet ',IOPT
               STOP
            END IF
C
            IF (IOPT.EQ.-4.OR.IOPT.EQ.-5) THEN
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/CS(JH)
               END DO 
               CALL EIM8(GSG(1,JH),GSG(1,JH),1,CMPA,((MG+2)*NHEM)/2)
            ELSE IF (IOPT.EQ.-8.OR.IOPT.EQ.-9) THEN
               DO I=1,(MG+2)*NHEM
                  GSG(I,JH)=GSG(I,JH)/sqrt(CS(JH))
c                  GSG(I,JH)=GSG(I,JH)
               END DO 
               CALL EIM8(GSG(1,JH),GSG(1,JH),1,CMPA,((MG+2)*NHEM)/2)
            END IF
C
C           CALL FFT991(GSG(1,JH),WORK,TRIG,IFAX,1,MG+2,MG,NHEM,1)
C
            CALL FFT8(GSG(1,JH),MG+2,MG,NHEM,1,TRIG,IFAX)
C
         ENDDO
      ENDIF
C
      RETURN
      END
