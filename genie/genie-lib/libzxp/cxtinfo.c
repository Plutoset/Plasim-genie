/**
 ** CXTINFO.C ---- the context info data structure
 **
 ** Copyright (c) 1995 Csaba Biegl, 820 Stirrup Dr, Nashville, TN 37221
 ** [e-mail: csaba@vuse.vanderbilt.edu] See "doc/copying.cb" for details.
 **/

#include "libgrx.h"
#undef	 GrContextInfo

const
struct _GR_contextInfo * const GrContextInfo = &_GrContextInfo;
struct _GR_contextInfo _GrContextInfo = {
    {					/* the current context */
	{				/* frame */
	    {				/* frame start addresses */
              0,0,0,0
	    },
	    0,				/* selector */
	    TRUE,			/* onscreen */
	    0,				/* memflags */
	    0,				/* lineoffset */
	    &DRVINFO->tdriver		/* frame driver */
	},
	NULL				/* root */
    },
    {					/* the screen context */
	{				/* frame */
	    {				/* frame start addresses */
0,0,0,0
	    },
	    0,				/* selector */
	    TRUE,			/* onscreen */
	    0,				/* memflags */
	    0,				/* lineoffset */
	    &DRVINFO->tdriver		/* frame driver */
	},
	NULL				/* root */
    }
};


