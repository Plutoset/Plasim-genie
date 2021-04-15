#include "libgrx.h"

void xdrawbitmap(int x,int y,int w,int h,char far *bmp,int pitch,int start,long fg,long bg,int nowin)
{
	w += x; h += y;
	bmp += (uint)start >> 3;
	start &= 7;
	do {
	    uchar far *bitp = (uchar far *)bmp;
	    uchar bits = *bitp;
	    uchar mask = 0x80 >> start;
	    int	  xx = x;
	    do {
	        if (nowin==0) SetPixel(xx,y,(bits & mask) ? fg : bg); 
		if((mask >>= 1) == 0) bits = *++bitp,mask = 0x80;
	    } while(++xx != w);
	    bmp += pitch;
	} while(++y != h);
}

