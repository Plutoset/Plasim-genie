C
      subroutine xcontc(zg,zr,zi,iwrk,x,y,md,mg,jg,c1,c2)
C
C   This is the 'real' version of ZCONTC.F
C   Fill in colour between two contour values. developed by zunjun zhang
C   at Reading university, england. jan, 1988.
C   Modified by Shian-Jiann Lin at University of Oklahoma, Mar. 3,1990.
C   external routine called: zfilln(x,y,np)
C   Polygon is defined by (x(i),y(i),i=1,np)
C
      INTEGER MD
      INTEGER MG
      REAL DR
      REAL CV
      REAL DI
      INTEGER MGP
      REAL C1
      REAL C2
      REAL CV1
      REAL CV2
      INTEGER J
      INTEGER JG
      REAL HMX
      REAL ZG
      REAL HMN
      INTEGER I
      REAL A
      REAL B
      INTEGER NP
      REAL X
      REAL ZR
      REAL Y
      REAL ZI
      INTEGER IMG
      INTEGER JJ
      INTEGER IWRK
      INTEGER I1
      INTEGER J1
      INTEGER I2
      INTEGER J2
      INTEGER I3
      INTEGER J3
      INTEGER I4
      INTEGER J4
      INTEGER ISIG
      REAL H1
      REAL H2
      REAL H3
      REAL H4
      INTEGER ISW
      INTEGER I10
      INTEGER J10
      INTEGER I40
      INTEGER J40
      INTEGER J0
      INTEGER KORNER
      INTEGER INI
      INTEGER INJ
      REAL CVC
      REAL H5
      INTEGER ISA
      REAL ZBR
      REAL ZBI
C
      dimension zg(md,*),iwrk(mg,*),x(*),y(*)
     +         ,Zr(md,*),Zi(md,*)
*           d(p1,p2,b1,b2)=b1+(cv-p1)*(b2-b1)/(p2-p1)
            dr(p1,p2,b1r,b2r)=b1r+(cv-p1)*(b2r-b1r)/(p2-p1)
            di(p1,p2,b1i,b2i)=b1i+(cv-p1)*(b2i-b1i)/(p2-p1)
      mgp=mg+1
c      cv1=min(c1,c2)
c      cv2=max(c1,c2)
      if(c1.gt.c2) then
            cv1=c2
            cv2=c1
      else
            cv1=c1
            cv2=c2
      endif
      do 3 j=1,jg-1
c      hmn=min(zg(1,j),zg(1,j+1))
c      hmx=max(zg(1,j),zg(1,j+1))
C The following scheme should be more efficient! (S.-J. Lin)
      hmx=zg(1,j)
      hmn=zg(1,j+1)
      if(hmn.gt.hmx) then
            hmx=hmn
            hmn=zg(1,j)
      endif
C
      do 50 i=2,mg
C Only 3 comparisons are needed!
c     hmn=min(hmn,zg(i,j),zg(i,j+1))
c50     hmx=max(hmx,zg(i,j),zg(i,j+1))
      a=zg(i,j)
      b=zg(i,j+1)
      if(a.gt.b) then
        if(a.gt.hmx) hmx=a
        if(b.lt.hmn) hmn=b
      else
        if(b.gt.hmx) hmx=b
        if(a.lt.hmn) hmn=a
      endif
50    continue
      if(hmn.ge.cv1.and.hmx.le.cv2) then
        np=1
*          za=z(1,j)
*          x(np)= real(za)
*          y(np)=aimag(za)
      x(np)=Zr(1,j)
      y(np)=Zi(1,j)
        do 51 i=2,mg
*          if(z(i,j).ne.za) then
      if(Zr(i,j).ne.x(np).or.Zi(i,j).ne.y(np)) then
            np=np+1
*              za=z(i,j)
            x(np)= Zr(i,j)
            y(np)= Zi(i,j)
*              x(np)= real(za)
*              y(np)=aimag(za)
        endif
   51     continue
        do 52 i=1,mg
*          if(z(mg-i+1,j+1).ne.za) then
      img=mg-i+1
      if(Zr(img,j+1).ne.x(np).or.Zi(img,j+1).ne.y(np)) then
            np=np+1
*              za=z(mg-i+1,j+1)
*              x(np)= real(za)
*              y(np)=aimag(za)
            x(np) = Zr(img,j+1)
            y(np) = Zi(img,j+1)
        endif
   52     continue
        call zfilln(x,y,np)
      elseif(hmn.gt.cv2.or.hmx.lt.cv1) then
        goto 3
      endif
      do 4 jj=1,4
      do 4 i=1,mg
    4 iwrk(i,jj)=0
      do 2 i=1,mg-1
      i1=i
      j1=j
      i2=i+1
      j2=j
      i3=i+1
      j3=j+1
      i4=i
      j4=j+1
      isig= 1
   29 cv=0.5*(1+isig)*cv1+0.5*(isig-1)*cv2
      h1=zg(i1,j1)*isig
      h2=zg(i2,j2)*isig
      h3=zg(i3,j3)*isig
      h4=zg(i4,j4)*isig
      if(h1-cv)31,36,36
   31 if(h2-cv)32,34,34
   32 if(h3-cv)33,35,35
   33 if(h4-cv)46,42,42
   34 if(h3-cv)44,35,35
   35 if(h4-cv)43,42,42
   36 if(h2-cv)41,37,37
   37 if(h3-cv)44,38,38
   38 if(h4-cv)43,46,46
   41 isw=1
      i10=i2
      j10=j2
      i40=i1
      j40=j1
      goto 45
   42 isw=2
      i10=i1
      j10=j1
      i40=i4
      j40=j4
      goto 45
   43 isw=3
      i10=i4
      j10=j4
      i40=i3
      j40=j3
      goto 45
   44 isw=4
      i10=i3
      j10=j3
      i40=i2
      j40=j2
      goto 45
   46 if(isig.eq.1) then
       if(h1.lt.cv ) goto 2
       isig=-1
       goto 29
      endif
      goto 2
   45 j0=j+isig-2
      if(iwrk(i10,j10-j0).eq.1.and.iwrk(i40,j40-j0).eq.1) goto 2
      i1=i10
      j1=j10
      i4=i40
      j4=j40
      cv=0.5*(1+isig)*cv1+0.5*(isig-1)*cv2
      h1=zg(i1,j1)*isig
      h4=zg(i4,j4)*isig
*      z1= z(i1,j1)
*      z4= z(i4,j4)
*      za=d(h4,h1,z4,z1)

      np=1
*      x(np)=real(za)
*      y(np)=aimag(za)
      x(np)=dr(h4,h1,Zr(i4,j4),Zr(i1,j1))
      y(np)=di(h4,h1,Zi(i4,j4),Zi(i1,j1))
C
  101 i2=i1+mod(isw-1,2)*(1-2*((isw-1)/2))
      j2=j1+mod(isw  ,2)*(1-2*((isw-1)/2))
      i3=i4+mod(isw-1,2)*(1-2*((isw-1)/2))
      j3=j4+mod(isw  ,2)*(1-2*((isw-1)/2))
      if(i2.eq.0.or.i3.eq.0.or.i2.eq.mgp.or.i3.eq.mgp)goto 103
      if(j2.eq.j-1.or.j3.eq.j-1.or.j2.eq.j+2.or.j3.eq.j+2)goto 103
      goto 104
  103 isw=mod(isw+1,4)+1
      korner=0
  112 ini=mod(isw  ,2)*(1-2*(mod(isw,4)/2))
      inj=mod(isw+1,2)*(1-2*(mod(isw,4)/2))
      cvc=0.5*(isig+1)*cv2+0.5*(isig-1)*cv1
      h4=zg(i4,j4)*isig
      if(korner.eq.0.and.h4.gt.cvc) then
        isig=-isig
        cv=0.5*(1+isig)*cv1+0.5*(isig-1)*cv2
        i4=i1
        j4=j1
        i1=i4+ini
        j1=j4+inj
        h1=zg(i1,j1)*isig
        h4=zg(i4,j4)*isig
*          z1= z(i1,j1)
*          z4= z(i4,j4)
*          za=d(h4,h1,z4,z1)
        iwrk(i1,j1-j-isig+2)=1
        iwrk(i4,j4-j-isig+2)=1
        np=np+1
*          x(np)=real(za)
*          y(np)=aimag(za)
      x(np)=dr(h4,h1,Zr(i4,j4),Zr(i1,j1))
      y(np)=di(h4,h1,Zi(i4,j4),Zi(i1,j1))
        if (i1.ne.i10.or.j1.ne.j10.or.i4.ne.i40.or.j4.ne.j40) goto 101
        goto 100
      else
        i1=i4
        j1=j4
        np=np+1
*          x(np)=real(z(i1,j1))
*          y(np)=aimag(z(i1,j1))
      x(np)=Zr(i1,j1)
      y(np)=Zi(i1,j1)
      endif
  111 i4=i1
      j4=j1
      i1=i4+ini
      j1=j4+inj
      if(i1.eq.0.or.i1.eq.mgp.or.j1.eq.j-1.or.j1.eq.j+2)goto 113
      goto 114
  113 isw=mod(isw+2,4)+1
      korner=1
      goto 112
  114 h1=zg(i1,j1)*isig
      if(h1.gt.cv2*(1+isig)*0.5+cv1*(isig-1)*0.5) then
        isig=-isig
        h1=-h1
        cv=0.5*(1+isig)*cv1+0.5*(isig-1)*cv2
      elseif(h1-cv.ge.0.) then
        np=np+1
*          x(np)=real(z(i1,j1))
*          y(np)=aimag(z(i1,j1))
      x(np)=Zr(i1,j1)
      y(np)=Zi(i1,j1)
        goto 111
      endif
      h4=zg(i4,j4)*isig
*      z1= z(i1,j1)
*      z4= z(i4,j4)
*      za=d(h4,h1,z4,z1)
      iwrk(i1,j1-j-isig+2)=1
      iwrk(i4,j4-j-isig+2)=1
      np=np+1
*      x(np)=real(za)
*      y(np)=aimag(za)
      x(np)=dr(h4,h1,Zr(i4,j4),Zr(i1,j1))
      y(np)=di(h4,h1,Zi(i4,j4),Zi(i1,j1))
      if (i1.ne.i10.or.j1.ne.j10.or.i4.ne.i40.or.j4.ne.j40) goto 101
      goto 100
  104 h1=zg(i1,j1)*isig
      h2=zg(i2,j2)*isig
      h3=zg(i3,j3)*isig
      h4=zg(i4,j4)*isig
      h5=0.25*(h1+h2+h3+h4)
*      z1= z(i1,j1)
*      z2= z(i2,j2)
*      z3= z(i3,j3)
*      z4= z(i4,j4)
      if(h1-cv) 11, 2,15
   11 if(h2-cv) 12,13,13
   12 if(h3-cv) 23,22,22
   13 if(h3-cv) 14,21,21
   14 if(h5-cv) 23,21,21
   15 if(h2-cv) 16,16,18
   16 if(h3-cv) 21,21,17
   17 if(h5-cv) 21,21,23
   18 if(h3-cv) 22,22,23
   21 isa=1
*      zb=d(h1,h2,z1,z2)
      zbr=dr(h1,h2,Zr(i1,j1),Zr(i2,j2))
      zbi=di(h1,h2,Zi(i1,j1),Zi(i2,j2))
c
      i4=i2
      j4=j2
      goto 30
   22 isa=2
*      zb=d(h2,h3,z2,z3)
      zbr=dr(h2,h3,Zr(i2,j2),Zr(i3,j3))
      zbi=di(h2,h3,Zi(i2,j2),Zi(i3,j3))
c
      i1=i2
      j1=j2
      i4=i3
      j4=j3
      goto 30
   23 isa=3
*      zb=d(h3,h4,z3,z4)
      zbr=dr(h3,h4,Zr(i3,j3),Zr(i4,j4))
      zbi=di(h3,h4,Zi(i3,j3),Zi(i4,j4))
C
      i1=i3
      j1=j3
*   30 if(zb.ne.za) then
30      if(zbr.ne.x(np).or.zbi.ne.y(np)) then
        np=np+1
*          x(np)=real(zb)
*          y(np)=aimag(zb)
      x(np)=zbr
      y(np)=zbi
      endif
      iwrk(i1,j1-j-isig+2)=1
      iwrk(i4,j4-j-isig+2)=1
*      za=zb
      isw=mod(isw-isa+5,4)+1
      if (i1.ne.i10.or.j1.ne.j10.or.i4.ne.i40.or.j4.ne.j40) goto 101
  100 call zfilln(x,y,np-1)
    2 continue
    3 continue
      return
      end
