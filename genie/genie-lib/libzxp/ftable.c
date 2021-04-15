/**
 ** FTABLE.C ---- a table of available frame drivers
 **
 ** Copyright (c) 1995 Csaba Biegl, 820 Stirrup Dr, Nashville, TN 37221
 ** [e-mail: csaba@vuse.vanderbilt.edu] See "doc/copying.cb" for details.
 **/

#include "grdriver.h"
#include "libgrx.h"

GrFrameDriver *_GrFrameDriverTable[] = {
    &_GrFrameDriverHERC1,
    &_GrFrameDriverEGAVGA1,
    &_GrFrameDriverEGA4,
    &_GrFrameDriverSVGA4,
    &_GrFrameDriverSVGA8,
    &_GrFrameDriverVGA8X,
    &_GrFrameDriverSVGA16,
    &_GrFrameDriverSVGA24,
    &_GrFrameDriverSVGA32L,
    &_GrFrameDriverSVGA32H,
    &_GrFrameDriverRAM1,
    &_GrFrameDriverRAM4,
    &_GrFrameDriverRAM8,
    &_GrFrameDriverRAM16,
    &_GrFrameDriverRAM24,
    &_GrFrameDriverRAM32L,
    &_GrFrameDriverRAM32H,
    &_GrFrameDriverRAM3x8,
    NULL
};

