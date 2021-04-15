c
      subroutine drawbox(iptyp,rar,izoom)

      IMPLICIT NONE

      INTEGER IPTYP,IZOOM
      REAL    RAR

      INTEGER L,J

c
      if (iptyp.eq.0) then
         call zststl(0)
         call xmap(-0.95,0.95,-1.05,0.45)
         call zpenup(-2.*rar,-1.0*rar)
         call zpendn( 2.*rar,-1.0*rar)
         call zpendn( 2.*rar,     rar)
         call zpendn(-2.*rar,     rar)
         call zpendn(-2.*rar,-1.0*rar)
      else if (iptyp.eq.11) then
         call zststl(0)
         call xmap(-1.5,1.5,-0.4,1.2)
         call xlcol(8,4,4,0)
         do l=1,6
            call zpenup(-1.0,0.2*(l-1))
            call zpendn( 1.0,0.2*(l-1))
         end do
         do j=1,7
            call zpenup(-1.0+(j-1)/3.0,0.0)
            call zpendn(-1.0+(j-1)/3.0,1.0)
         end do
         call xlcol(1,4,4,0)
         call zpenup(-1.0,0.0)
         call zpendn(-1.0,1.0)
         call zpendn( 1.0,1.0)
         call zpendn( 1.0,0.0)
         call zpendn(-1.0,0.0)
      else if (iptyp.eq.15) then
         call zststl(0)
         call xmap(-1.5,1.5,-0.4,1.2)
         call xlcol(8,4,4,0)
         do l=1,6
            call zpenup(-1.0,0.2*(l-1))
            call zpendn( 1.0,0.2*(l-1))
         end do
         do j=1,7
            call zpenup(-1.0+(j-1)/3.0,0.0)
            call zpendn(-1.0+(j-1)/3.0,1.0)
         end do
         call xlcol(1,4,4,0)
         call zpenup(-1.0,0.0)
         call zpendn(-1.0,1.0)
         call zpendn( 1.0,1.0)
         call zpendn( 1.0,0.0)
         call zpendn(-1.0,0.0)
      else
         call xmap(-0.95,0.95,-0.85,0.65)
         call zststl(iabs(iptyp))
         if (iptyp.eq.4) then
            call zstcen(10.0,90.0)
            call zsthta(30.0)
         else if (iptyp.eq.-4) then
            call zstcen(10.0,-90.0)
            if (izoom.eq.0) then
               call zsthta(-30.0)
            else
               call zsthta(-15.0)
            end if
         else
            call zstcen(0.,0.)
         end if
         call xlcol(8,4,4,0)
         call zxyref
         call xlcol(1,4,4,0)
         call zoutln
      end if
c
      return
      end
