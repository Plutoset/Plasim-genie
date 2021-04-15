      subroutine xcfill(z,x,y,iw,xw,yw,md,m,n,cl,ncl,mode)
C
c Produce a colour or gray filling map by calling Xcontc.
C
      INTEGER MD
      INTEGER LIMZF
      REAL ZFMIN
      REAL ZFMAX
      INTEGER MODE
      REAL ZMAX1
      REAL Z
      REAL ZMIN1
      INTEGER J
      INTEGER N
      INTEGER I
      INTEGER M
      INTEGER NCL1
      INTEGER NCL
      REAL CL
      INTEGER K
      INTEGER KK
      INTEGER ITHICK
      REAL CL1
      REAL CL2
      REAL GRAY
      REAL X
      REAL Y
      INTEGER IW
      REAL XW
      REAL YW
      REAL ZFMA
      REAL ZFMI
C
      dimension z(md,*),x(md,*),y(md,*),iw(md,*),xw(*),yw(*),cl(1)
      save limzf, zfmax, zfmin
      data limzf,zfmin,zfmax /0,-999.0, 999.0/

      if(mode.eq.0) return
      zmax1=z(1,1)
      zmin1=zmax1
      do 1 j=1,n
      do 1 i=1,m
      zmax1=max(z(i,j),zmax1)
      zmin1=min(z(i,j),zmin1)
1     continue

      ncl1=ncl
      if(limzf.ne.0) then
      if(zfmin.ne.-999.0)then
      if(zfmin.ge.cl(ncl1))then
        ncl1=1
        return
      elseif(zfmin.lt.cl(1)) then
        do 14 k=ncl1,1,-1
14        cl(k+1)=cl(k)
        cl(1)=zmin1
        ncl1=ncl1+1
        goto 12
      endif
      do 10 k=1,ncl1-1
        if(cl(k).le.zfmin.and.cl(k+1).gt.zfmin) then
          cl(1)=zfmin
          ncl1=ncl1-k+1
          do 11 kk=2,ncl1
11            cl(kk)=cl(k+kk-1)
          goto 12
        endif
10      continue
      endif
12      if(zfmax.ne.999.0)then
      if(zfmax.le.cl(1))then
        ncl1=1
        return
      elseif(zfmax.gt.cl(ncl1)) then
        ncl1=ncl1+1
        cl(ncl1)=zmax1
        goto 22
      endif
      do 20 k=1,ncl1-1
        if(cl(k).lt.zfmax.and.cl(k+1).ge.zfmax) then
          cl(k+1)=zfmax
          ncl1=k+1
          goto 22
        endif
20      continue
      endif
22      continue
      endif
      if((limzf.eq.0.or.zfmin.eq.-999.0).and.(zmin1.lt.cl(1)))then
      do 32 k=ncl1,1,-1
32        cl(k+1)=cl(k)
      cl(1)=zmin1
      ncl1=ncl1+1
      endif
      if((limzf.eq.0.or.zfmax.eq.999.0).and.(zmax1.gt.cl(ncl1)))then
      ncl1=ncl1+1
      cl(ncl1)=zmax1
      endif

      call xqthik(ithick)
      call xthick(0)
      do 50 k=1,ncl1-1
      cl1=cl(k)
      cl2=cl(k+1)
C
C set gray degree:
C if mode>0, colour varies from white to black for lower to higher
C            contour values.
C if mode<0, the order of gray filling is reveresed, i.e. black for
C            minimum, white for maximum.
C
      if(mode.gt.0) gray=(ncl1-k-0.5)/(ncl1-1)
      if(mode.lt.0) gray=(k-0.5)/(ncl1-1)
C        call PSgray(gray)
      call xcontc(z,x,y,iw,xw,yw,md,m,n,cl1,cl2)
 50   continue
      call xthick(ithick)
C      call PSgray(0.0)
      return

      entry xcflim(zfmi, zfma)
      limzf=1
      zfmax=zfma
      zfmin=zfmi
      return
      end
