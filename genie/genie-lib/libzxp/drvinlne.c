/**
 ** DRVINLNE.C ---- inline functions related to drivers
 **
 ** Copyright (c) 1995 Csaba Biegl, 820 Stirrup Dr, Nashville, TN 37221
 ** [e-mail: csaba@vuse.vanderbilt.edu] See "doc/copying.cb" for details.
 **/

#include "libgrx.h"

GrGraphicsMode (GrCurrentMode)(void)
{
	return(GrCurrentMode());
}

GrVideoAdapter (GrAdapterType)(void)
{
	return(GrAdapterType());
}

GrFrameMode (GrCurrentFrameMode)(void)
{
	return(GrCurrentFrameMode());
}

GrFrameMode (GrScreenFrameMode)(void)
{
	return(GrScreenFrameMode());
}

GrFrameMode (GrCoreFrameMode)(void)
{
	return(GrCoreFrameMode());
}

const GrVideoDriver *(GrCurrentVideoDriver)(void)
{
	return(GrCurrentVideoDriver());
}

const GrVideoMode *(GrCurrentVideoMode)(void)
{
	return(GrCurrentVideoMode());
}

const GrVideoMode *(GrVirtualVideoMode)(void)
{
	return(GrVirtualVideoMode());
}

const GrFrameDriver *(GrCurrentFrameDriver)(void)
{
	return(GrCurrentFrameDriver());
}

const GrFrameDriver *(GrScreenFrameDriver)(void)
{
	return(GrScreenFrameDriver());
}

int (GrScreenX)(void)
{
	return(GrScreenX());
}

int (GrScreenY)(void)
{
	return(GrScreenY());
}

int (GrVirtualX)(void)
{
	return(GrVirtualX());
}

int (GrVirtualY)(void)
{
	return(GrVirtualY());
}

int (GrViewportX)(void)
{
	return(GrViewportX());
}

int (GrViewportY)(void)
{
	return(GrViewportY());
}

int (GrScreenIsVirtual)(void)
{
	return(GrScreenIsVirtual());
}

int (GrNumPlanes)(void)
{
	return(GrNumPlanes());
}

int (GrLineOffset)(int width)
{
	return(GrLineOffset(width));
}

long (GrPlaneSize)(int w,int h)
{
	return(GrPlaneSize(w,h));
}

long (GrContextSize)(int w,int h)
{
	return(GrContextSize(w,h));
}

