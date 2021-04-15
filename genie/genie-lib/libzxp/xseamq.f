      SUBROUTINE XSEAMQ(X1,Y1,SL1,X2,Y2,SL2,A1,A2,B,C)
C Fit points (X1,Y1) and(X2,Y2) with slopes SL1,SL2 at the corresponding
C seamed quadratics so that y=c+(b+a*(x-xc))*(x-xc) where xc=(x1+x2)/2
C and a=a1 for (x2-x1)*(x-xc)<0.0 ,a=a2 for (x2-x1)*(x-xc)>=0.0
C
      REAL H
      REAL X2
      REAL X1
      REAL HH
      REAL TL
      REAL Y1
      REAL SL1
      REAL TR
      REAL Y2
      REAL SL2
      REAL C
      REAL B
      REAL A1
      REAL A2
C
      H=(X2-X1)*0.5
      HH=H*H
      TL=Y1+0.5*H*SL1
      TR=Y2-0.5*H*SL2
      C =0.5*(TL+TR)
      B =(TR-TL)/H
      A1=(Y1-1.5*TL+0.5*TR)/HH
      A2=(Y2-1.5*TR+0.5*TL)/HH
      RETURN
      END
