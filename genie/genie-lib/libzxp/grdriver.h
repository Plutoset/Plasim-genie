/**
 ** GRDRIVER.H ---- utilities for frame buffer and video card drivers
 **
 ** Copyright (c) 1995 Csaba Biegl, 820 Stirrup Dr, Nashville, TN 37221
 ** [e-mail: csaba@vuse.vanderbilt.edu] See "doc/copying.cb" for details.
 **/

#ifndef __GRDRIVER_H_INCLUDED__
#define __GRDRIVER_H_INCLUDED__

#ifndef __GRX20_H_INCLUDED__
#include "grx20.h"
#endif

/*
 * Video mode flag bits (in the 'GrVideoModeExt' structure)
 */
#define GR_VMODEF_LINEAR	1	/* uses linear memory mapping */
#define GR_VMODEF_ACCEL		2	/* it is an accelerated mode */
#define GR_VMODEF_FAST_SVGA8	4	/* use faster mixed linear-planar access */

extern GrFrameDriver
/*
 * Standard frame drivers in GRX
 */
_GrFrameDriverHERC1,			/* Hercules mono */
_GrFrameDriverEGAVGA1,			/* EGA VGA mono */
_GrFrameDriverEGA4,			/* EGA 16 color */
_GrFrameDriverSVGA4,			/* (Super) VGA 16 color */
_GrFrameDriverSVGA8,			/* (Super) VGA 256 color */
_GrFrameDriverVGA8X,			/* VGA 256 color mode X */
_GrFrameDriverSVGA16,			/* Super VGA 32768/65536 color */
_GrFrameDriverSVGA24,			/* Super VGA 16M color */
_GrFrameDriverSVGA32L,			/* Super VGA 16M color padded #1 */
_GrFrameDriverSVGA32H,			/* Super VGA 16M color padded #2 */
_GrFrameDriverRAM1,			/* mono */
_GrFrameDriverRAM4,			/* 16 color planar */
_GrFrameDriverRAM8,			/* 256 color */
_GrFrameDriverRAM16,			/* 32768/65536 color */
_GrFrameDriverRAM24,			/* 16M color */
_GrFrameDriverRAM32L,			/* 16M color padded #1 */
_GrFrameDriverRAM32H,			/* 16M color padded #2 */
_GrFrameDriverRAM3x8,			/* 16M color planar (image mode) */
/*
 * This is a NULL-terminated table of frame driver descriptor pointers. Users
 * can provide their own table with only the desired (or additional) drivers.
 * Otherwise the table from the GRX library is linked, which includes ALL
 * currently available drivers (i.e. the ones above).
 */
*_GrFrameDriverTable[];


extern GrVideoDriver
/*
 * Standard video drivers in GRX
 */
_GrVideoDriverHERC,			/* Hercules driver */
_GrVideoDriverSTDEGA,			/* standard EGA driver */
_GrVideoDriverSTDVGA,			/* standard VGA driver */
_GrVideoDriverVESA,			/* generic VESA Super VGA driver */
_GrVideoDriverATI28800,			/* ATI 28800 chipset driver */
_GrVideoDriverET4000,			/* Tseng ET4000 driver */
_GrVideoDriverCL5426,			/* Cirrus 5426 driver */
_GrVideoDriverMACH64,			/* ATI MACH64 driver */
_GrVideoDriverSVGALIB,			/* Linux SVGALIB interface */
/*
 * This is a NULL-terminated table of video driver descriptor pointers. Users
 * can provide their own table with only the desired (or additional) drivers.
 * Otherwise the table from the GRX library is linked, which includes ALL
 * currently available drivers (i.e. the ones above).
 */
*_GrVideoDriverTable[];

/*
 * Frame driver utility functions
 */
void _GrFrDrvGenericBitBlt(GrFrame *dst,int dx,int dy,GrFrame *src,int x,int y,int w,int h,long op);
void _GrFrDrvPackedBitBltR2R(GrFrame *dst,int dx,int dy,GrFrame *src,int x,int y,int w,int h,long op);
void _GrFrDrvPackedBitBltR2V(GrFrame *dst,int dx,int dy,GrFrame *src,int x,int y,int w,int h,long op);
void _GrFrDrvPackedBitBltV2R(GrFrame *dst,int dx,int dy,GrFrame *src,int x,int y,int w,int h,long op);
void _GrFrDrvPackedBitBltV2V(GrFrame *dst,int dx,int dy,GrFrame *src,int x,int y,int w,int h,long op);

/*
 * Video driver utility functions
 */
int  _GrViDrvDetectEGAVGA(void);
int  _GrViDrvDetectVGA(void);
int  _GrViDrvDetectEGA(void);
int  _GrViDrvInitEGAVGA(char *options);
void _GrViDrvResetEGAVGA(void);
int  _GrViDrvGetCurrentEGAVGAmode(void);
int  _GrViDrvSetEGAVGAmode(GrVideoMode *mp,int noclear);
int  _GrViDrvSetEGAVGAcustomTextMode(GrVideoMode *mp,int noclear);
void _GrViDrvLoadColorEGA4(int c,int r,int g,int b);
void _GrViDrvLoadColorVGA4(int c,int r,int g,int b);
void _GrViDrvLoadColorVGA8(int c,int r,int g,int b);
int  _GrViDrvVESAsetVirtualSize(GrVideoMode *md,int w,int h,GrVideoMode *result);
int  _GrViDrvVESAvirtualScroll(GrVideoMode *md,int x,int y,int result[2]);

/*
 * Commonly used video driver data structures
 */
extern GrVideoModeExt _GrViDrvEGAVGAtextModeExt;
extern GrVideoModeExt _GrViDrvEGAVGAcustomTextModeExt;

#endif /* whole file */

