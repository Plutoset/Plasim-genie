/**
 ** VTABLE.C ---- a table of available video drivers
 **
 ** Copyright (c) 1995 Csaba Biegl, 820 Stirrup Dr, Nashville, TN 37221
 ** [e-mail: csaba@vuse.vanderbilt.edu] See "doc/copying.cb" for details.
 **/

#include "grdriver.h"
#include "libgrx.h"

GrVideoDriver *_GrVideoDriverTable[] = {
#ifdef __MSDOS__
    &_GrVideoDriverSTDEGA,
    &_GrVideoDriverSTDVGA,
    &_GrVideoDriverVESA,
    &_GrVideoDriverATI28800,
    &_GrVideoDriverET4000,
    &_GrVideoDriverCL5426,
    &_GrVideoDriverMACH64,
#endif
    NULL
};

