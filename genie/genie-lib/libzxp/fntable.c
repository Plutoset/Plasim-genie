/**
 ** FNTABLE.C ---- a table of available font drivers
 **
 ** Copyright (c) 1995 Csaba Biegl, 820 Stirrup Dr, Nashville, TN 37221
 ** [e-mail: csaba@vuse.vanderbilt.edu] See "doc/copying.cb" for details.
 **/

#include "grfontdv.h"
#include "libgrx.h"

GrFontDriver *_GrFontDriverTable[] = {
    &_GrFontDriverGRX,
    NULL
};

