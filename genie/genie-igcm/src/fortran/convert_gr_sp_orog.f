      SUBROUTINE convert_gr_sp_orog(change_flag)
      implicit none
C     
C     
c     for iga:
#include "param1.cmn"
      include 'param2.cmn'
c     for gs:
      include 'spectr.cmn'
c     for filename:
      include 'files.cmn'
c     for lorog:
      include 'blank.cmn'
c     for gsg:
      include 'orog_g.cmn'
c     for alat:
      include 'legau.cmn'


C     
      integer i,j,ihem,iof,jj,ii,ij
  
      integer change_flag

      integer mmmax
      PARAMETER(MMMAX=42)
      REAL FILT(0:MMMAX+1)
      integer mgmax,jggmax,nmx1,nmx2
      PARAMETER(MGMAX=2160,JGGMAX=1080)
      REAL FIELD(MGMAX*JGGMAX)
      PARAMETER(NMX1=60000,nmx2=6000)
      REAL GSGI(NMX1)
      COMPLEX ZI(NMX2)
      integer maxcoordsdef
      PARAMETER(MAXCOORDSDEF=2440)
      real alati(maxcoordsdef+1)

c     for the filter....
      real afil
      integer mfil
      parameter(afil=0.1)
      parameter(mfil=21)

c     for normalisation....
      real norm

      integer ifirst
      data ifirst/1/     

      if (change_flag.eq.1) then
C 
C     
      if (lorog) then

c     Well, we only technically need to do this once, at initialisation.
c       This is better, because we don't want to be reading files all the 
c       way through a long run.

         if (ifirst.eq.1) then
         print*,' Opening ',fname_orog_spec(1:ifname_orog_spec)
         call read1d_comp_nc(fname_orog_spec(1:ifname_orog_spec),
     :        'gs', iga, gs)
         ifirst=0
         endif

      else

c     Now, we need to do the spectral transformation

      norm=radea*radea*ww*ww/ga

      do I=1,IGA
         GS(I)=(0.0,0.0)
      enddo

      do j=1,jg
         do ihem=1,nhem
            iof=mgpp*(ihem-1)
            if (ihem.eq.1) then
               jj=j
            else
               jj=2*jg+1-j
            end if
            do i=1,mg
               field(i+(jj-1)*mg)=gsg(i+iof,j)
            end do
         end do
      end do

      II=0
      DO JH=1,JG
        IJ=(JH-1)*(MG+2)*NHEM
        DO I=1,MG
          II=II+1
          IJ=IJ+1
          GSGI(IJ)=FIELD(II)
        ENDDO
      ENDDO
      DO JH=JG,1,-1
        IJ=(JH-1)*(MG+2)*NHEM + (MG+2)
        DO I=1,MG
          II=II+1
          IJ=IJ+1
          GSGI(IJ)=FIELD(II)
        ENDDO
      ENDDO

      alati(1:jg)=alat(1:jg)
      zi(:)=0.0

c     OK, up to this point I reproduce myspec.f with 
c       the same input file....

      CALL INISPEC(GSGI,ZI,ALATI,MG,JG,NHEM,
     :                     NN,MM,MOCT,NWJ2,1)


c      However, here there is a small difference in 
c        the output, zi....

      call filter(filt,mmmax,afil,mfil)
      CALL MYFILT(ZI,NN,MM,MOCT,NHEM,FILT,mmmax)

c        ....which carries on to here.
c      print*,zi(1:260)
c      stop
c     
      gs(1:nwj2)=zi(nwj2+1:nwj2*nhem)/norm
      gs(nwj2+1:nwj2*nhem)=zi(1:nwj2)/norm

      endif  ! End if(lorog) else
      endif  ! End if(change_flag)

      return
      END
c     

