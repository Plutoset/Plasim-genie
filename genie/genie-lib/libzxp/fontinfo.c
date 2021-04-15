/**
 ** FONTINFO.C ---- font path global data
 **
 ** Copyright (c) 1995 Csaba Biegl, 820 Stirrup Dr, Nashville, TN 37221
 ** [e-mail: csaba@vuse.vanderbilt.edu] See "doc/copying.cb" for details.
 **/
#include "grfontdv.h"
#include "libgrx.h"

struct _GR_fontFileInfo _GrFontFileInfo = {
    (-1),			/* number of path entries */
    NULL,			/* the path entries */
    NULL,			/* the list of available fonts */
};

