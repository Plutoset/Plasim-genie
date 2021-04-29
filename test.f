c Data are stored in ASCII

c All values are integers
c Pressures are stored as hectapascals (hPa) * 100
c In the uninterpolated product (hadslp2.0_acts.asc), missing 
c data values are stored as -999.9 * 100


c The day (set "1" for this dataset), month and year are stored at the start 
c of each month.

c Data Array (72x37)
c Item (1,1) stores the value for the 5-deg-area centred at 180W and 90N
c Item (72,37) stores the value for the 5-deg-area centred at 175E and 90S
c (Ignore areas beyond 90-deg latitude).

c           ----- ----- 
c          |     |     |
c          | YR  | MON |
c          |_____|_____|__________________________________
c      90N |(1,1)                                         |
c          |                                              |
c          |                                              |
c          |                                              |
c          |                                              |
c          |                                              |
c      Equ |(1,19)                                        |
c          |                                              |
c          |                                              |
c          |                                              |
c          |                                              |
c          |                                              |
c      90S |(1,37)_________________________________(72,37)|
c           180W                 0                    180E


	program read_hadslp 

c	this program reads in the HadSLP2  
c	pressure data set (bulk ascii version). 
c	the data is 5 x 5 degree, from 90 deg north to 90 
c	deg south and from 180 deg east to 535 deg.  It is 
c	monthly data from 1850-2004.

	implicit none

	integer nyr
        parameter (nyr = 155)

	integer nlat, nlon
        parameter(nlon=72,nlat=37)

	integer n_month
	parameter (n_month=12)

	integer idata(nlon,nlat)
	integer ilat, ilon
        integer iyear, imonth
        real gd(nlon,nlat,n_month,nyr)   ! input bulk array
	real xlat(nlat), xlon(nlon)

	integer HEADER(2)

c----------------------------------------------------------------------
	PRINT*,'YEAR: '
	open(10,file='hadslp2r.asc',
     &     status='old',form='formatted')
	PRINT*,'YEAR: '
	do iyear=1,nyr
	  do imonth=1,n_month
	    read(10,'(2i7)',err=888,end=999)header

	    PRINT*,'YEAR: ',HEADER(1),' MONTH: ',HEADER(2)

	    read(10,'(72i8)',err=888,end=999)idata

            do ilat=1,37
              do ilon=1,72
                gd(ilon,ilat,imonth,iyear)=(idata(ilon,ilat)*0.01)
              enddo
            enddo

	  enddo
	enddo
	go to 300

888	stop 'error'
999	stop'eof'

300	write(6,*)'have read in bulk file ok'

c------------------------------------------------------------------
c	define longitudes

	do ilon=1, nlon
 	  xlon(ilon)=180.+(ilon-1)*5.
	enddo 

c	define latitudes

	do ilat=1, nlat
	  xlat(ilat)=95.-ilat*5.
	enddo

c-------------------------------------------------------------------------




	end program read_hadslp