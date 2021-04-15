      SUBROUTINE HEXP_lib(SV,GV,NLS,ITYPE,
     :     ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
      IMPLICIT NONE
C     
      INTEGER NLS,ITYPE
      COMPLEX SV(*),GV(*)
C     
      INTEGER MM,MOCT,NN,MG,JG,NWJ2,NHEM,JH
      REAL ALP(NWJ2+NWJ2,JG),DALP(NWJ2+NWJ2,JG),
     :     RLP(NWJ2+NWJ2,JG),RDLP(NWJ2+NWJ2,JG),
     :     POLY(NWJ2,NHEM)
      COMPLEX TEMP
C     
C     PERFORMS INDIRECT LEGENDRE TRANSFORM FOR A (SET OF) FIELD(S)
C     HAVING A TOTAL OF NLS LEVELS, FROM SPECTRAL TO FOURIER SPACE.
C     THE TYPE OF FUNCTION AND THENCE OF TRANSFORM IS CHOSEN BY ITYPE
C     WITH THE FOLLOWING CURRENTLY DEFINED:
C     ITYPE=1,2   : ALP     : NORMAL TRANSFORM.
C     ITYPE=3,4   : DALP    : Y-DERIVATIVE.
C     ITYPE=5,6   : RLP     : DEL(-2).
C     ITYPE=7,8   : RDLP    : Y-DERIVATIVE OF DEL(-2).
C     AN EVEN/ODD VALUE OF ITYPE DENOTES A SPECTRAL FIELD OF EVEN/ODD
C     SYMMETRY.
C     
      REAL PI,PI2
      PARAMETER(PI=3.14159265359,PI2=2.*PI)
c     
      INTEGER IDL,IGL,I,IALP,IGPAR,ISPAR,IHEM,IA,IB,INC,IP,
     :     IM,MP,IV,M,NWW,IN,II
c     
C     PRESET FOURIER ARRAY TO ZERO.
C     
      NWW=1+(MM-1)/MOCT
      IDL=(MG+2)/2
      IGL=IDL*NHEM
C     
      DO I=1,IGL*NLS
         GV(I)=0.
      END DO
C     
C     USE ITYPE TO DEFINE TRANSFORM TYPE AND SYMMETRY LABELS.
C     ISPAR IS SYMMETRY OF SPECTRAL FIELD    = 0 FOR D,T,SP ETC.
C     = 1 FOR Z.
C     IGPAR IS SYMMETRY OF FOURIER FIELD: SAME AS ISPAR UNLESS 
C     TRANSFORM INVOLVES A D/DY.
C     
      IF (ITYPE.LE.0.OR.ITYPE.GE.9) THEN
         WRITE(6,'(/'' ***ABORT : HEXP CALLED WITH TYPE ='',I5)')
     :        ITYPE
         STOP
      ENDIF
      IALP=(ITYPE+1)/2
      ISPAR=MOD(ITYPE,2)
      IGPAR=ISPAR
      IF (IALP.EQ.2.OR.IALP.EQ.4) IGPAR=1-ISPAR
C     
C     CALCULATE POLY ARRAY IN VECTOR LOOP BEFORE MAIN TRANSFORM.
C     
      DO 30 IHEM=1,NHEM
         INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
         IA=INC-1
         IF (IALP.EQ.1) THEN
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=ALP(IA,JH)
            ENDDO
         ELSEIF (IALP.EQ.2) THEN
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=DALP(IA,JH)
            ENDDO
         ELSEIF (IALP.EQ.3) THEN
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=RLP(IA,JH)
            ENDDO
         ELSEIF (IALP.EQ.4) THEN
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=RDLP(IA,JH)
            ENDDO
         ENDIF
 30   CONTINUE
C     
C     PERFORM INVERSE LEGENDRE TRANSFORM FROM SPECTRAL SPACE TO FORM
C     THE EVEN AND ODD CONTRIBUTIONS TO THE FOURIER TRANSFORMS.
C     SEPARATE CODE FOR NHEM=1,2 TO INCREASE EFFICIENCY.
C     
      IF (NHEM.EQ.1) THEN
         IM=-IDL
         IP=-NWJ2
         DO MP=1,MM,MOCT
            IM=IM+1
            DO IN=MP,NN,2
               IP=IP+1
               DO IV=1,NLS
                  GV(IM+IV*IDL)=GV(IM+IV*IDL) + 
     :                 POLY(IP+NWJ2,1)*SV(IP+IV*NWJ2)
               END DO
            END DO
         END DO
      ELSE
         IM=-IDL
         IP=-NWJ2
         DO MP=1,MM,MOCT
            IM=IM+1
            DO IN=MP,NN,2
               IP=IP+1
               DO IV=1,NLS*NHEM
                  II=IV-((IV-1)/NHEM)*NHEM
                  GV(IM+IV*IDL)=GV(IM+IV*IDL) + 
     :                 POLY(IP+NWJ2,II)*SV(IP+IV*NWJ2)
               END DO
            END DO
         END DO
      ENDIF
C     
C     FOR A GLOBAL RUN, SUM AND DIFFERENCE EVEN AND ODD CONTRIBUTIONS
C     TO GIVE THE COMPLETE FOURIER TRANSFORMS AT THE NORTHERN AND
C     SOUTHERN LATITUDE ROWS.  SEPARATE CODE FOR EACH SYMMETRY:
C     IGPAR=0 : EVEN (IA) PRECEDES ODD (IB).
C     IGPAR=0 : ODD (IA) PRECEDES EVEN (IB).
C     
      IF (NHEM.EQ.2) THEN
         IF (IGPAR.EQ.0) THEN
            DO M=1,NWW
               IA=M-IGL
C     DIR$ IVDEP
               DO IV=1,NLS
                  IA=IA+IGL
                  IB=IA+IDL
                  TEMP=GV(IA)
                  GV(IA)=TEMP+GV(IB)
                  GV(IB)=TEMP-GV(IB)
               END DO
            END DO
         ELSE
            DO M=1,NWW
               IA=M-IGL
C     DIR$ IVDEP
               DO IV=1,NLS
                  IA=IA+IGL
                  IB=IA+IDL
                  TEMP=GV(IA)
                  GV(IA)=GV(IB)+TEMP
                  GV(IB)=GV(IB)-TEMP
               END DO
            END DO
         ENDIF
      ENDIF
C     
      RETURN
      END
c
      SUBROUTINE HEXP8(SV,GV,NLS,ITYPE,
     :     ALP,DALP,RLP,RDLP,POLY,MM,NN,MOCT,JH,MG,JG,NWJ2,NHEM)
      IMPLICIT NONE
C     
      INTEGER NLS,ITYPE
      COMPLEX*16 SV(*),GV(*)
C     
      INTEGER MM,MOCT,NN,MG,JG,NWJ2,NHEM,JH
      REAL*8 ALP(NWJ2+NWJ2,JG),DALP(NWJ2+NWJ2,JG),
     :     RLP(NWJ2+NWJ2,JG),RDLP(NWJ2+NWJ2,JG),
     :     POLY(NWJ2,NHEM)
      COMPLEX*16 TEMP
C     
C     PERFORMS INDIRECT LEGENDRE TRANSFORM FOR A (SET OF) FIELD(S)
C     HAVING A TOTAL OF NLS LEVELS, FROM SPECTRAL TO FOURIER SPACE.
C     THE TYPE OF FUNCTION AND THENCE OF TRANSFORM IS CHOSEN BY ITYPE
C     WITH THE FOLLOWING CURRENTLY DEFINED:
C     ITYPE=1,2   : ALP     : NORMAL TRANSFORM.
C     ITYPE=3,4   : DALP    : Y-DERIVATIVE.
C     ITYPE=5,6   : RLP     : DEL(-2).
C     ITYPE=7,8   : RDLP    : Y-DERIVATIVE OF DEL(-2).
C     AN EVEN/ODD VALUE OF ITYPE DENOTES A SPECTRAL FIELD OF EVEN/ODD
C     SYMMETRY.
C     
      REAL*8 PI,PI2
      PARAMETER(PI=3.14159265359,PI2=2.*PI)
c     
      INTEGER IDL,IGL,I,IALP,IGPAR,ISPAR,IHEM,IA,IB,INC,IP,
     :     IM,MP,IV,M,NWW,IN,II
c     
C     PRESET FOURIER ARRAY TO ZERO.
C     
      NWW=1+(MM-1)/MOCT
      IDL=(MG+2)/2
      IGL=IDL*NHEM
C     
      DO I=1,IGL*NLS
         GV(I)=0.
      END DO
C     
C     USE ITYPE TO DEFINE TRANSFORM TYPE AND SYMMETRY LABELS.
C     ISPAR IS SYMMETRY OF SPECTRAL FIELD    = 0 FOR D,T,SP ETC.
C     = 1 FOR Z.
C     IGPAR IS SYMMETRY OF FOURIER FIELD: SAME AS ISPAR UNLESS 
C     TRANSFORM INVOLVES A D/DY.
C     
      IF (ITYPE.LE.0.OR.ITYPE.GE.9) THEN
         WRITE(6,'(/'' ***ABORT : HEXP CALLED WITH TYPE ='',I5)')
     :        ITYPE
         STOP
      ENDIF
      IALP=(ITYPE+1)/2
      ISPAR=MOD(ITYPE,2)
      IGPAR=ISPAR
      IF (IALP.EQ.2.OR.IALP.EQ.4) IGPAR=1-ISPAR
C     
C     CALCULATE POLY ARRAY IN VECTOR LOOP BEFORE MAIN TRANSFORM.
C     
      DO 30 IHEM=1,NHEM
         INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
         IA=INC-1
         IF (IALP.EQ.1) THEN
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=ALP(IA,JH)
            ENDDO
         ELSEIF (IALP.EQ.2) THEN
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=DALP(IA,JH)
            ENDDO
         ELSEIF (IALP.EQ.3) THEN
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=RLP(IA,JH)
            ENDDO
         ELSEIF (IALP.EQ.4) THEN
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=RDLP(IA,JH)
            ENDDO
         ENDIF
 30   CONTINUE
C     
C     PERFORM INVERSE LEGENDRE TRANSFORM FROM SPECTRAL SPACE TO FORM
C     THE EVEN AND ODD CONTRIBUTIONS TO THE FOURIER TRANSFORMS.
C     SEPARATE CODE FOR NHEM=1,2 TO INCREASE EFFICIENCY.
C     
      IF (NHEM.EQ.1) THEN
         IM=-IDL
         IP=-NWJ2
         DO MP=1,MM,MOCT
            IM=IM+1
            DO IN=MP,NN,2
               IP=IP+1
               DO IV=1,NLS
                  GV(IM+IV*IDL)=GV(IM+IV*IDL) + 
     :                 POLY(IP+NWJ2,1)*SV(IP+IV*NWJ2)
               END DO
            END DO
         END DO
      ELSE
         IM=-IDL
         IP=-NWJ2
         DO MP=1,MM,MOCT
            IM=IM+1
            DO IN=MP,NN,2
               IP=IP+1
               DO IV=1,NLS*NHEM
                  II=IV-((IV-1)/NHEM)*NHEM
                  GV(IM+IV*IDL)=GV(IM+IV*IDL) + 
     :                 POLY(IP+NWJ2,II)*SV(IP+IV*NWJ2)
               END DO
            END DO
         END DO
      ENDIF
C     
C     FOR A GLOBAL RUN, SUM AND DIFFERENCE EVEN AND ODD CONTRIBUTIONS
C     TO GIVE THE COMPLETE FOURIER TRANSFORMS AT THE NORTHERN AND
C     SOUTHERN LATITUDE ROWS.  SEPARATE CODE FOR EACH SYMMETRY:
C     IGPAR=0 : EVEN (IA) PRECEDES ODD (IB).
C     IGPAR=0 : ODD (IA) PRECEDES EVEN (IB).
C     
      IF (NHEM.EQ.2) THEN
         IF (IGPAR.EQ.0) THEN
            DO M=1,NWW
               IA=M-IGL
C     DIR$ IVDEP
               DO IV=1,NLS
                  IA=IA+IGL
                  IB=IA+IDL
                  TEMP=GV(IA)
                  GV(IA)=TEMP+GV(IB)
                  GV(IB)=TEMP-GV(IB)
               END DO
            END DO
         ELSE
            DO M=1,NWW
               IA=M-IGL
C     DIR$ IVDEP
               DO IV=1,NLS
                  IA=IA+IGL
                  IB=IA+IDL
                  TEMP=GV(IA)
                  GV(IA)=GV(IB)+TEMP
                  GV(IB)=GV(IB)-TEMP
               END DO
            END DO
         ENDIF
      ENDIF
C     
      RETURN
      END
