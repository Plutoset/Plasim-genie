      SUBROUTINE HANAL_lib(GV,SV,NLS,ITYPE,POLY,ALP,DALP,RLP,RDLP,AW,CS,
     :     JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
      IMPLICIT NONE
      COMPLEX GV(*),SV(*)
      INTEGER NLS,ITYPE,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2
      REAL AW(*),CS(*)
C     
C     PERFORMS DIRECT LEGENDRE TRANSFORM FOR A (SET OF) FIELD(S)
C     HAVING A TOTAL OF NLS LEVELS, FROM FOURIER TO SPECTRAL SPACE.
C     THE TYPE OF FUNCTION AND THENCE OF TRANSFORM IS CHOSEN BY ITYPE
C     WITH THE FOLLOWING CURRENTLY DEFINED:
C     ITYPE=1,2   : ALP     : NORMAL TRANSFORM.
C     ITYPE=3,4   : DALP    : Y-DERIVATIVE.
C     AN EVEN/ODD VALUE OF ITYPE DENOTES A SPECTRAL FIELD OF EVEN/ODD
C     SYMMETRY.
C     
      REAL PI,PI2
      PARAMETER(PI=3.14159265359,PI2=2.*PI)
      REAL ALP(NWJ2+NWJ2,JG),DALP(NWJ2+NWJ2,JG),RLP(NWJ2+NWJ2,JG),
     :     RDLP(NWJ2+NWJ2,JG)
      REAL POLY(NWJ2,NHEM)
      COMPLEX TEMP
C     
      INTEGER IALP,ISPAR,IGPAR,M,IA,IB,IV,IHEM,INC,MP,IN,IM,IP,II
     :     ,NWW,IDL,IGL
      REAL AWT
C     
C     
C     USE ITYPE TO DEFINE TRANSFORM TYPE AND SYMMETRY LABELS.
C     ISPAR IS SYMMETRY OF SPECTRAL FIELD    = 0 FOR D,T,SP ETC.
C     = 1 FOR Z.
C     IGPAR IS SYMMETRY OF FOURIER FIELD: SAME AS ISPAR UNLESS 
C     TRANSFORM INVOLVES A D/DY.
C     
      NWW=1+(MM-1)/MOCT
      IDL=(MG+2)/2
      IGL=IDL*NHEM
C     
      IF (ITYPE.LE.0.OR.ITYPE.GE.5) THEN
         WRITE(6,'(/'' ***ABORT : HANAL CALLED WITH TYPE ='',I5)') 
     :        ITYPE
         STOP
      ENDIF
      IALP=(ITYPE+1)/2
      ISPAR=MOD(ITYPE,2)
      IGPAR=ISPAR
      IF (IALP.EQ.2) IGPAR=1-ISPAR
C     
C     FOR A GLOBAL RUN, SUM AND DIFFERENCE THE COMPLETE FOURIER
C     TRANSFORMS AT THE NORTHERN AND SOUTHERN LATITUDE ROWS TO GIVE
C     THE EVEN AND ODD CONTRIBUTIONS.
C     SEPARATE CODE FOR EACH SYMMETRY:
C     IGPAR=0 : EVEN (IA) TO PRECEDE ODD (IB).
C     IGPAR=0 : ODD (IA) TO PRECEDE EVEN (IB).
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
                  GV(IA)=.5*(TEMP+GV(IB))
                  GV(IB)=.5*(TEMP-GV(IB))
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
                  GV(IA)=.5*(TEMP-GV(IB))
                  GV(IB)=.5*(TEMP+GV(IB))
               END DO
            END DO
         ENDIF
      ENDIF
C     
C     SET UP APPROPRIATE GAUSSIAN WEIGHT FOR CURRENT LATITUDE.
C     DEPENDS ON TRANSFORM TYPE.
C     ASSUMES JH IN /LEGAU/ CONTAINS LATITUDE COUNTER FROM CALLING 
C     LOOP.
C     
      IF (IALP.EQ.1) AWT=AW(JH)*CS(JH)
      IF (IALP.EQ.2) AWT=-AW(JH)
C     
C     CALCULATE POLY ARRAY IN VECTOR LOOP BEFORE MAIN TRANSFORM.
C     
      IF (IALP.EQ.1) THEN
         DO IHEM=1,NHEM
            INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
            IA=INC-1
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=AWT*ALP(IA,JH)
            ENDDO
         ENDDO
      ELSEIF (IALP.EQ.2) THEN
         DO IHEM=1,NHEM
            INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
            IA=INC-1
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=AWT*DALP(IA,JH)
            ENDDO
         ENDDO
      ELSEIF (IALP.EQ.3) THEN
         DO IHEM=1,NHEM
            INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
            IA=INC-1
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=AWT*RLP(IA,JH)
            ENDDO
         ENDDO
      ELSEIF (IALP.EQ.4) THEN
         DO IHEM=1,NHEM
            INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
            IA=INC-1
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=AWT*RDLP(IA,JH)
            ENDDO
         ENDDO
      ENDIF
C     
C     PERFORM DIRECT LEGENDRE TRANSFORM FROM THE EVEN AND ODD
C     PARTS OF THE FOURIER TRANSFORMS TO SPECTRAL SPACE.
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
                  SV(IP+IV*NWJ2)=SV(IP+IV*NWJ2) + 
     :                 POLY(IP+NWJ2,1)*GV(IM+IV*IDL)
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
                  SV(IP+IV*NWJ2)=SV(IP+IV*NWJ2) + 
     :                 POLY(IP+NWJ2,II)*GV(IM+IV*IDL)
               END DO
            END DO
         END DO
      ENDIF
C     
      RETURN
      END
C
      SUBROUTINE HANAL8(GV,SV,NLS,ITYPE,POLY,ALP,DALP,RLP,RDLP,AW,CS,
     :     JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2)
      IMPLICIT NONE
      COMPLEX*16 GV(*),SV(*)
      INTEGER NLS,ITYPE,JH,NN,MM,NHEM,MOCT,MG,JG,NWJ2
      REAL*8 AW(*),CS(*)
C     
C     PERFORMS DIRECT LEGENDRE TRANSFORM FOR A (SET OF) FIELD(S)
C     HAVING A TOTAL OF NLS LEVELS, FROM FOURIER TO SPECTRAL SPACE.
C     THE TYPE OF FUNCTION AND THENCE OF TRANSFORM IS CHOSEN BY ITYPE
C     WITH THE FOLLOWING CURRENTLY DEFINED:
C     ITYPE=1,2   : ALP     : NORMAL TRANSFORM.
C     ITYPE=3,4   : DALP    : Y-DERIVATIVE.
C     AN EVEN/ODD VALUE OF ITYPE DENOTES A SPECTRAL FIELD OF EVEN/ODD
C     SYMMETRY.
C     
      REAL*8 PI,PI2
      PARAMETER(PI=3.14159265359,PI2=2.*PI)
      REAL*8 ALP(NWJ2+NWJ2,JG),DALP(NWJ2+NWJ2,JG),RLP(NWJ2+NWJ2,JG),
     :     RDLP(NWJ2+NWJ2,JG)
      REAL*8 POLY(NWJ2,NHEM)
      COMPLEX*16 TEMP
C     
      INTEGER IALP,ISPAR,IGPAR,M,IA,IB,IV,IHEM,INC,MP,IN,IM,IP,II
     :     ,NWW,IDL,IGL
      REAL*8 AWT
C     
C     
C     USE ITYPE TO DEFINE TRANSFORM TYPE AND SYMMETRY LABELS.
C     ISPAR IS SYMMETRY OF SPECTRAL FIELD    = 0 FOR D,T,SP ETC.
C     = 1 FOR Z.
C     IGPAR IS SYMMETRY OF FOURIER FIELD: SAME AS ISPAR UNLESS 
C     TRANSFORM INVOLVES A D/DY.
C     
      NWW=1+(MM-1)/MOCT
      IDL=(MG+2)/2
      IGL=IDL*NHEM
C     
      IF (ITYPE.LE.0.OR.ITYPE.GE.5) THEN
         WRITE(6,'(/'' ***ABORT : HANAL CALLED WITH TYPE ='',I5)') 
     :        ITYPE
         STOP
      ENDIF
      IALP=(ITYPE+1)/2
      ISPAR=MOD(ITYPE,2)
      IGPAR=ISPAR
      IF (IALP.EQ.2) IGPAR=1-ISPAR
C     
C     FOR A GLOBAL RUN, SUM AND DIFFERENCE THE COMPLETE FOURIER
C     TRANSFORMS AT THE NORTHERN AND SOUTHERN LATITUDE ROWS TO GIVE
C     THE EVEN AND ODD CONTRIBUTIONS.
C     SEPARATE CODE FOR EACH SYMMETRY:
C     IGPAR=0 : EVEN (IA) TO PRECEDE ODD (IB).
C     IGPAR=0 : ODD (IA) TO PRECEDE EVEN (IB).
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
                  GV(IA)=.5*(TEMP+GV(IB))
                  GV(IB)=.5*(TEMP-GV(IB))
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
                  GV(IA)=.5*(TEMP-GV(IB))
                  GV(IB)=.5*(TEMP+GV(IB))
               END DO
            END DO
         ENDIF
      ENDIF
C     
C     SET UP APPROPRIATE GAUSSIAN WEIGHT FOR CURRENT LATITUDE.
C     DEPENDS ON TRANSFORM TYPE.
C     ASSUMES JH IN /LEGAU/ CONTAINS LATITUDE COUNTER FROM CALLING 
C     LOOP.
C     
      IF (IALP.EQ.1) AWT=AW(JH)*CS(JH)
      IF (IALP.EQ.2) AWT=-AW(JH)
C     
C     CALCULATE POLY ARRAY IN VECTOR LOOP BEFORE MAIN TRANSFORM.
C     
      IF (IALP.EQ.1) THEN
         DO IHEM=1,NHEM
            INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
            IA=INC-1
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=AWT*ALP(IA,JH)
            ENDDO
         ENDDO
      ELSEIF (IALP.EQ.2) THEN
         DO IHEM=1,NHEM
            INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
            IA=INC-1
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=AWT*DALP(IA,JH)
            ENDDO
         ENDDO
      ELSEIF (IALP.EQ.3) THEN
         DO IHEM=1,NHEM
            INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
            IA=INC-1
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=AWT*RLP(IA,JH)
            ENDDO
         ENDDO
      ELSEIF (IALP.EQ.4) THEN
         DO IHEM=1,NHEM
            INC=(IHEM-1)*(1-ISPAR)+(2-IHEM)*ISPAR
            IA=INC-1
            DO IP=1,NWJ2
               IA=IA+2
               POLY(IP,IHEM)=AWT*RDLP(IA,JH)
            ENDDO
         ENDDO
      ENDIF
C     
C     PERFORM DIRECT LEGENDRE TRANSFORM FROM THE EVEN AND ODD
C     PARTS OF THE FOURIER TRANSFORMS TO SPECTRAL SPACE.
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
                  SV(IP+IV*NWJ2)=SV(IP+IV*NWJ2) + 
     :                 POLY(IP+NWJ2,1)*GV(IM+IV*IDL)
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
                  SV(IP+IV*NWJ2)=SV(IP+IV*NWJ2) + 
     :                 POLY(IP+NWJ2,II)*GV(IM+IV*IDL)
               END DO
            END DO
         END DO
      ENDIF
C     
      RETURN
      END
