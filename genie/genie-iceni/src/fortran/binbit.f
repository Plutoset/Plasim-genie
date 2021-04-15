c     ***** functions required to calculate binary bit of decimal number
      real*8 function binbit(dec, bit)
      integer i, dec, rem, bit, tmp
      real*8 pow2
      rem = dec
      do i=31,(bit+1),-1
         tmp = rem/pow2(i)
         if (tmp.ge.1.) then
            rem = rem - pow2(i)
         endif
      enddo
      tmp = rem/pow2(bit)
      binbit = tmp
      return
      end

      real*8 function pow2(n)
      integer ans, n, i
      ans = 1
      do i=1,n
         ans = 2*ans
      enddo
      pow2 = ans
      return
      end
