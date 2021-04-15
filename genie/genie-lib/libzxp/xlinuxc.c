#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/timeb.h>
#include <stdlib.h>
#include "getarg.h"
#include "gif_lib.h"

#ifdef F2C
  #define FILEATTR my_fileattr__
  #define MYDATE my_date__
  #define GETLOGG1 my_getlogg__
  #define GIFMAKE my_gifmaker__
#else
  #define FILEATTR my_fileattr_
  #define MYDATE my_date_
  #define GETLOGG1 my_getlogg_
  #define GIFMAKE my_gifmaker_
#endif

static int itest;
struct stat buff;
#define PROGRAM_NAME	"Raw2Gif"

static int HandleGifError(GifFileType *GifFile);

void FILEATTR(name,s1time,size)
   char *name;
   char *s1time;
   off_t *size;
{
   char *xxx;
   for (xxx=name; (*xxx != ' ') && (*xxx != 0) ; xxx++) ;
   *xxx=0;

   itest=stat(name,&buff);
   strcpy(s1time,ctime(&buff.st_mtime));
   *size=buff.st_size;
}

void MYDATE(s1time)
   char *s1time;
{
   struct timeb tp;
   ftime(&tp);
   strcpy(s1time,ctime(&tp.time));
}

void GETLOGG1(name)
   char *name;
{
   char *xxx;
   /*   printf(" in getlogg a \n "); */
   xxx=(char *)cuserid(NULL);
   /*   printf(" in getlogg b \n "); */
   if (xxx) {
   strncpy(name,xxx,8);
   } else {
     strncpy(name,"anon",5);
}
   /*   printf(" in getlogg c \n "); */
}

void GIFMAKE(colours,ncols,nxsize,nysize,RawGif,gif_filename)
     int *ncols,*nxsize,*nysize;
     unsigned char colours[];
     GifPixelType RawGif[];
     char *gif_filename;

{
    int ImageWidth, ImageHeight, ColorMapSize, icount;
    GifColorType *ColorMap;
    static int BitsPerPixelArray[] = { 2, 4 ,8, 16, 32, 64, 128, 256 };
    int i, j, BitsPerPixel,GifFileExistence;
    static GifPixelType *ScanLine;
    GifFileType *GifFile;

    char *xxx;
    for (xxx=gif_filename; (*xxx != ' ') && (*xxx != 0) ; xxx++) ;
    *xxx=0;

	if ((ColorMap = (GifColorType *)
	                malloc(sizeof(GifColorType) * 255))  /* Biggest map. */
	    == NULL) {
	    GIF_MESSAGE("Failed to allocate bitmap, aborted.");
	    exit(3);
	}

        for (ColorMapSize=0 ; ColorMapSize<*ncols ; ColorMapSize++ )
          {
	    ColorMap[ColorMapSize].Red = colours[3*ColorMapSize];
	    ColorMap[ColorMapSize].Green = colours[1+3*ColorMapSize];
	    ColorMap[ColorMapSize].Blue = colours[2+3*ColorMapSize];
   	  }
          ColorMapSize=*ncols;
          ImageWidth=*nxsize;
          ImageHeight=*nysize;

    for (BitsPerPixel = 0;
	 BitsPerPixel < 8 && BitsPerPixelArray[BitsPerPixel] != ColorMapSize;
	 BitsPerPixel++);
    if (++BitsPerPixel > 8) {
	GIF_MESSAGE("Number of color map is NOT power of 2 up to 256.");
	exit(3);
    }

    if ((ScanLine = (GifPixelType *) malloc(sizeof(GifPixelType) * ImageWidth))
								== NULL) {
	GIF_MESSAGE("Failed to allocate scan line, aborted.");
	exit(3);
    }
    GifFileExistence=0;
    if ((GifFile = EGifOpenFileName(gif_filename,GifFileExistence)) == NULL) {	   /* Gif to stdout. */
	free((char *) ScanLine);
        HandleGifError(GifFile);
        printf(" Error in EGifOPenFileName ");
	return;
    }

    if (EGifPutScreenDesc(GifFile, ImageWidth, ImageHeight, BitsPerPixel,
			  0, BitsPerPixel, ColorMap) == GIF_ERROR) {
	free((char *) ScanLine);
        HandleGifError(GifFile);
        printf(" Error in EGifPutScreenDesc ");
	return;
    }

    if (EGifPutImageDesc(GifFile, 0, 0, ImageWidth, ImageHeight, FALSE, 1,
			 NULL) == GIF_ERROR) {
	free((char *) ScanLine);
        HandleGifError(GifFile);
        printf(" Error in EGifPutImageDesc ");
	return;
    }

    icount=0;
    for (i = 0; i < ImageHeight; i++) {
	for (j = 0; j < ImageWidth; j++)
           {
            icount=icount+1;
            ScanLine[j]=RawGif[icount];
	    if (ScanLine[j] >= ColorMapSize)
		GIF_MESSAGE("Warning: RAW data color > maximum color map entry.");
           }
	if (EGifPutLine(GifFile, ScanLine, ImageWidth) == GIF_ERROR) {
	    free((char *) ScanLine);
            HandleGifError(GifFile);
            printf(" Error in EGifPutline ");
	    return;
	}
    }

    if (EGifCloseFile(GifFile) == GIF_ERROR) {
	free((char *) ScanLine);
        HandleGifError(GifFile);
        printf(" Error in EGifCloseFile ");
	return;
    }

    free((char *) ScanLine);
    return;
}

/******************************************************************************
* Handle last GIF error. Try to close the file and free all allocated memory. *
******************************************************************************/
static int HandleGifError(GifFileType *GifFile)
{
    int i = GifLastError();

    if (EGifCloseFile(GifFile) == GIF_ERROR) {
	GifLastError();
    }
    return i;
}
