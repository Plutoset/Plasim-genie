/**
 ** DRVINFO.C ---- the driver info data structure
 **
 ** Copyright (c) 1995 Csaba Biegl, 820 Stirrup Dr, Nashville, TN 37221
 ** [e-mail: csaba@vuse.vanderbilt.edu] See "doc/copying.cb" for details.
 **/

#include "grdriver.h"
#include "libgrx.h"

#undef GrDriverInfo

static long dummyframefn(void);

const
struct _GR_driverInfo * const GrDriverInfo = &_GrDriverInfo;
struct _GR_driverInfo _GrDriverInfo = {
    NULL,				/* video driver */
    &DRVINFO->actmode,			/* current video mode pointer */
    {					/* current video mode struct */
	FALSE,				/* present */
	4,				/* bpp */
	80,25,				/* geometry */
	3,				/* BIOS mode */
	160,				/* lineoffset */
	0,				/* private */
    },
    {					/* current frame driver */
	GR_frameUndef,			/* frame mode */
	GR_frameUndef,			/* compatible RAM frame mode */
	FALSE,				/* onscreen */
	1,				/* line width alignment */
	1,				/* number of planes */
	0,				/* bits per pixel */
	0L,				/* max plane size the code can handle */
	NULL,
	(long (*)(GrFrame*,int,int))				    dummyframefn,
	(void (*)(int,int,long))				    dummyframefn,
	(void (*)(int,int,int,int,long))			    dummyframefn,
	(void (*)(int,int,int,long))				    dummyframefn,
	(void (*)(int,int,int,long))				    dummyframefn,
	(void (*)(int,int,int,int,long))			    dummyframefn,
	(void (*)(int,int,int,int,char*,int,int,long,long))	    dummyframefn,
	(void (*)(int,int,int,char,long,long))			    dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn
    },
    {					/* screen frame driver */
	GR_frameUndef,			/* frame mode */
	GR_frameUndef,			/* compatible RAM frame mode */
	FALSE,				/* onscreen */
	1,				/* line width alignment */
	1,				/* number of planes */
	0,				/* bits per pixel */
	0L,				/* max plane size the code can handle */
	NULL,
	(long (*)(GrFrame*,int,int))				    dummyframefn,
	(void (*)(int,int,long))				    dummyframefn,
	(void (*)(int,int,int,int,long))			    dummyframefn,
	(void (*)(int,int,int,long))				    dummyframefn,
	(void (*)(int,int,int,long))				    dummyframefn,
	(void (*)(int,int,int,int,long))			    dummyframefn,
	(void (*)(int,int,int,int,char*,int,int,long,long))	    dummyframefn,
	(void (*)(int,int,int,char,long,long))			    dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn
    },
    {					/* dummy text mode frame driver */
	GR_frameText,			/* frame mode */
	GR_frameUndef,			/* compatible RAM frame mode */
	TRUE,				/* onscreen */
	1,				/* line width alignment */
	1,				/* number of planes */
	16,				/* bits per pixel */
	0L,				/* max plane size the code can handle */
	NULL,
	(long (*)(GrFrame*,int,int))				    dummyframefn,
	(void (*)(int,int,long))				    dummyframefn,
	(void (*)(int,int,int,int,long))			    dummyframefn,
	(void (*)(int,int,int,long))				    dummyframefn,
	(void (*)(int,int,int,long))				    dummyframefn,
	(void (*)(int,int,int,int,long))			    dummyframefn,
	(void (*)(int,int,int,int,char*,int,int,long,long))	    dummyframefn,
	(void (*)(int,int,int,char,long,long))			    dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn,
	(void (*)(GrFrame*,int,int,GrFrame*,int,int,int,int,long))  dummyframefn
    },
    GR_default_text,				/* current mode code */
    80,25,					/* default text size */
    640,480,					/* default graphics size */
    16L,16L,					/* default txt and gr colors */
    0,0,					/* virtual position */
    TRUE,					/* exit upon errors */
    TRUE,					/* restore startup mode */
    FALSE,					/* split banks */
    (-1),					/* current bank */
    NULL,					/* mode set hook */
    (void (*)(int)    )_GrDummyFunction,	/* banking func */
    (void (*)(int,int))_GrDummyFunction		/* split banking func */
};

static long dummyframefn(void)
{
  /*	if(DRVINFO->errsfatal) {
	    _GrCloseVideoDriver();
	    fprintf(stderr,
		"GRX Error: graphics operation attempted %s\n",
		(DRVINFO->fdriver.mode == GR_frameText) ? "in text mode" : "before mode set"
	    );
	    exit(1);
	    }*/
	return(GrNOCOLOR);
}

void _GrDummyFunction(void)
{
	return;
}

