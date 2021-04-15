/*
   Was once part of the POV-RAY raytracing package (X/Linux version), now
   very much modified to provide a simple Xlib based line and polygon 
   drawing routine set for use with the program QPLOT. Very little of the
   original code remains, save for the window initialization routine.
   
   Large portions original by me, Robin Glover (R.W.Glover@reading.ac.uk)
   23/4/1995
   
   All routines inteded to be called from FORTRAN must have pointer 
   arguments and have __ appended to names (certainly for f2c fortran...)
   Also should have lower case names (again f2c). Identified by starting
   x_. Runs on Linux and X11R6, should run on most things I suppose
   
   */
/*
 * Here are some routines I wrote which implement +d option on unix computers
 * running X-windows. For now, only black and white output is supported. If I
 * get access to a computer with a color monitor, I'll probably add support
 * for colors to my routines.
 * 
 * In future I'll probably add some more dithering methods. I have tested these
 * routines on SUN 3 and SUN 4. I'm using the tvtwm window manager.
 * 
 * If you have some suggestions to my source code or if you change anything,
 * please let me know. I can be reached at the following address: Marek
 * Rzewuski, Avstikkeren 11, 1156 Oslo 11, Norway or marekr@ifi.uio.no on
 * Internet.
 */

/*
 * 91-07-29 Allan H. Wax (Wax.OSBU_South@Xerox.COM) Hacked this module so it
 * can be used with either a colour or B&W screen. Made all variables local
 * to this routine (no need for more globals if no one ever uses them). Fixed
 * bug in refresh routine. Use dither from colorx.c. Use xpov.icon instead of
 * theIcon.
 */

#ifndef LINUX
#ifndef SOLARIS
#ifndef CRAY
#ifndef SGI
#ifndef CYGWIN
#error Must define one of LINUX,SOLARIS,CRAY,SGI or CYGWIN in makefile
#endif
#endif
#endif
#endif
#endif

#ifndef CYGWIN
#include <sys/ipc.h>
#include <sys/shm.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <math.h>
#include <signal.h>
#include <unistd.h>
#include "qplot.bit"
#include "grx20.h"
#ifdef SOLARIS
  #include <errno.h>
  #include <thread.h>
  #include <libgen.h>
#endif
#ifdef CRAY
  #include <errno.h>
  #include <libgen.h>
#endif
#ifdef SGI
  #include <sys/types.h>
  #include <sys/prctl.h>
#endif
#ifdef LINUX_CLONE
  #include <linux/sched.h>
#endif


/*#define WISHX "/usr/local/bin/wishx"
#define XQPLOTTCL "/export/glacier/glacier-01/robin/tmp/xqplot.tcl"*/

#ifdef F2C
  #define X_FILLPOLY x_fillpoly__
  #define X_LINETO x_lineto__
  #define X_DISPLAYFLUSH x_displayflush__
  #define X_SETLINESTYLE x_setlinestyle__
  #define X_CLEAR x_clear__
  #define X_DISPLAYINIT x_displayinit__
  #define X_DISPLAYDONE x_displaydone__
  #define X_POLYLINE x_polyline__
  #define X_GETPIXEL x_getpixel__
  #define X_FREECOLOURS x_freecolours__
  #define X_UPDATEPAGE x_updatepage__
  #define X_DISPLAYPAGE x_displaypage__
  #define X_GRABAREA x_grabarea__
  #define XWISH_INIT xwish_init__
  #define CLOSEDOWN closedown_
  #define X_CURINI x_curini__
  #define X_CURCLS x_curcls__
  #define X_CURSR x_cursr__
  #define X_FNT x_fnt__
  #define QPLOT_MAIN qplot_main__
  #define X_GETCOLOURS x_getcolours__
  #define X_SETCOLOURS x_setcolours__
  #define X_SETPIXEL x_setpixel__
  #define X_MYSIZES x_mysizes__
  #define X_CHANGE_FONT x_change_font__
  #define X_CHANGE_FONT1 x_change_font1__
  #define X_CHANGE_FONT2 x_change_font2__
#else
  #ifdef CRAY
  #define X_FILLPOLY X_FILLPOLY 
  #define X_LINETO X_LINETO 
  #define X_DISPLAYFLUSH X_DISPLAYFLUSH 
  #define X_SETLINESTYLE X_SETLINESTYLE 
  #define X_CLEAR X_CLEAR
  #define X_DISPLAYINIT X_DISPLAYINIT 
  #define X_DISPLAYDONE X_DISPLAYDONE 
  #define X_POLYLINE X_POLYLINE 
  #define X_GETPIXEL X_GETPIXEL 
  #define X_FREECOLOURS X_FREECOLOURS 
  #define X_UPDATEPAGE X_UPDATEPAGE 
  #define X_DISPLAYPAGE X_DISPLAYPAGE 
  #define X_GRABAREA X_GRABAREA 
  #define XWISH_INIT XWISH_INIT 
  #define X_CURSR X_CURSR 
  #define X_FNT x_fnt
  #define QPLOT_MAIN QPLOT_MAIN
  #define X_GETCOLOURS X_GETCOLOURS 
  #define X_SETCOLOURS X_SETCOLOURS 
  #define X_SETPIXEL X_SETPIXEL 
  #define X_MYSIZES x_mysizes
  #define X_CHANGE_FONT x_change_font
  #define X_CHANGE_FONT1 x_change_font1
  #define X_CHANGE_FONT2 x_change_font2
  #else
  #define X_FILLPOLY x_fillpoly_
  #define X_LINETO x_lineto_
  #define X_DISPLAYFLUSH x_displayflush_
  #define X_SETLINESTYLE x_setlinestyle_
  #define X_CLEAR x_clear_
  #define X_DISPLAYINIT x_displayinit_
  #define X_DISPLAYDONE x_displaydone_
  #define X_POLYLINE x_polyline_
  #define X_GETPIXEL x_getpixel_
  #define X_FREECOLOURS x_freecolours_
  #define X_UPDATEPAGE x_updatepage_
  #define X_DISPLAYPAGE x_displaypage_
  #define X_GRABAREA x_grabarea_ 
  #define XWISH_INIT xwish_init_
  #define CLOSEDOWN closedown_
  #define X_CURINI x_curini_
  #define X_CURCLS x_curcls_
  #define X_CURSR x_cursr_
  #define X_FNT x_fnt_
  #define QPLOT_MAIN qplot_main_
  #define X_GETCOLOURS x_getcolours_
  #define X_SETCOLOURS x_setcolours_
  #define X_SETPIXEL x_setpixel_
  #define X_MYSIZES x_mysizes_
  #define X_CHANGE_FONT x_change_font_
  #define X_CHANGE_FONT1 x_change_font1_
  #define X_CHANGE_FONT2 x_change_font2_
  #endif
#endif

#ifdef STANDALONE
   #ifdef F2C
      #define WININI1 winini1_
   #else
      #ifdef CRAY
          #define WININI1 winini1
      #else
          #define WININI1 winini1_
      #endif
   #endif
#else
  #define MAIN main
  #ifdef PGI
     #define MAIN MAIN_
  #endif
#endif  

#define MAXPOINTS 4000
#define 	BORDER_WIDTH	2
#define	EV_MASK	(ButtonPressMask | \
		 KeyPressMask	 | \
		 ExposureMask    | \
		 StructureNotifyMask)
#define MAX_COLOURS_EVER 256
#define MAX_POINTS 100

static Display *myDisplay;
static Display *newDisplay;
static int      myScreen;
static int      myDepth;
static Visual  *myVisual;
static unsigned long myBlackPixel;
static unsigned long myWhitePixel;
static long     myCurrentColour;
static unsigned int myCurrentLineWidth;
static int      myCurrentLineStyle;
static int      myCurrentOnDots;
static int      myCurrentOffDots;
static XEvent   myEvent;
static Window   myWindow, openWindow();
static GC       myGC;
static Pixmap   myPixmap[2];
static int      *updatepage,*displaypage;
static int      NoDisplay;

static char    *display_name;
static char    *display_none = ":0.0";
static int     disp_wide, disp_high;
static float   oblate;
static int     nowin;
static int     xx1,yy1;
static char    *data;
static XGCValues gcvalues;
static unsigned long     colorpixels[MAX_COLOURS_EVER];
static int      num_cols;
static Bool     iscolor;
static int 	screen_open = 0;
static int      Window_X_Size,Window_Y_Size;
static int      Last_X_Pos = 0;
static int      Last_Y_Pos = 0;
static int      Display_init = 0;
static char    *DISPENV="DISPLAY";
static char    progid[21];
static int     colourcount;
static XPoint  pointlist[MAX_POINTS];
static unsigned long pointcol=0xDEADBEEF;
static int     pointcount=0;
static XButtonEvent myev,myev2;
static GrTextOption opt;
static GrFont *fsm,*fsr;
static initialise_font;

#ifdef LINUX
static char *shmptr;
static int shmid;
#endif


static int child1=0,child2=0,child3=0;


extern void QPLOT_MAIN(char *progid);

#ifndef STANDALONE
void MAIN__()
{
}
#endif

void displayfont(int *ix,int *iy,GrFont *font,char *text,int *len,int *ifontcol,int *ivert)
{
	int ww,hh;

	opt.txo_font   = font;
        if (*ivert==0) {
	opt.txo_xalign = GR_ALIGN_LEFT;
	opt.txo_yalign = GR_ALIGN_TOP;
	opt.txo_direct = GR_TEXT_RIGHT;
        } else {
	  if (*ivert==1) {
	opt.txo_xalign = GR_ALIGN_LEFT;
	opt.txo_yalign = GR_ALIGN_TOP;
	opt.txo_direct = GR_TEXT_DOWN;
	  } else {
	opt.txo_xalign = GR_ALIGN_LEFT;
	opt.txo_yalign = GR_ALIGN_TOP;
	opt.txo_direct = GR_TEXT_UP;
	}
        }
	opt.txo_fgcolor.v = *ifontcol;
	opt.txo_bgcolor.v = 0;
       	ww = GrStringWidth(text,*len,&opt);
	hh = GrStringHeight(text,*len,&opt);
        if (*ivert==0) {
        *iy=*iy-hh;
        } else {
        }
	GrDrawString(text,*len,*ix,*iy,&opt,nowin);
}

void X_MYSIZES (ix,iy,ifn1,ifn2)
   int *ix,*iy,*ifn1,*ifn2;

{
   *ix=xx1;
   *iy=yy1;

	opt.txo_font   = fsm;
	opt.txo_xalign = GR_ALIGN_LEFT;
	opt.txo_yalign = GR_ALIGN_TOP;
	opt.txo_direct	  = GR_TEXT_RIGHT;
	opt.txo_fgcolor.v = 1;
	opt.txo_bgcolor.v = 0;

   *ifn1=GrStringHeight("A",1,&opt);

	opt.txo_font   = fsr;
	opt.txo_xalign = GR_ALIGN_LEFT;
	opt.txo_yalign = GR_ALIGN_TOP;
	opt.txo_direct	  = GR_TEXT_RIGHT;
	opt.txo_fgcolor.v = 1;
	opt.txo_bgcolor.v = 0;

   *ifn2=GrStringHeight("A",1,&opt);
}

void X_FNT(ipos,ilen,ix,iy,ifail,strwrt,ifonttype,ifontcol,ivert)
   int *ipos,*ilen,*ix,*iy,*ifail,*ifonttype,*ifontcol,*ivert;
   char *strwrt;
{
  if (*ifonttype==0) {
   displayfont(ix,iy,fsm,strwrt,ilen,ifontcol,ivert); 
  } else {
   displayfont(ix,iy,fsr,strwrt,ilen,ifontcol,ivert); 
  }
   ifail=0;
}


void Emergency_Closedown(sigcode)
     int sigcode;
{
  int result;
  #ifdef DEBUG
  fprintf(stderr,"Recieved Signal %d\n",sigcode);
  #endif
  fprintf(stderr,"Emergency exit!...");
  /* sleep for a couple of seconds, to give everything a chance to exit 
     cleanly */
  sleep(1);
  if (child1>0) {
#ifdef DEBUG
    fprintf(stderr,"Terminating first child ... (pid=%d) ... ",child1);
#endif
    result=kill(child1,SIGTERM);
#ifdef DEBUG
    fprintf(stderr,"Result was %d\n",result);
#endif
  }
  if (child2>0) {
#ifdef DEBUG
    fprintf(stderr,"Terminating second child ... (pid=%d) ... ",child2);
#endif
    result=kill(child2,SIGTERM);
#ifdef DEBUG
    fprintf(stderr,"Result was %d\n",result);
#endif
  }
  if (child3>0) {
#ifdef DEBUG
    fprintf(stderr,"Terminating third child ... (pid=%d) ... ",child3);
#endif
    result=kill(child3,SIGTERM);
#ifdef DEBUG
    fprintf(stderr,"Result was %d\n",result);
#endif
  }
  sleep(1);
  if (child1>0) {
#ifdef DEBUG
    fprintf(stderr,"Killing first child ... ");
#endif
    result=kill(child1,SIGKILL);
#ifdef DEBUG
    fprintf(stderr,"Result was %d\n",result);
#endif
  }
  if (child2>0) {
#ifdef DEBUG
    fprintf(stderr,"Killing second child ... ");
#endif
    result=kill(child2,SIGKILL);
#ifdef DEBUG
    fprintf(stderr,"Result was %d \n",result);
#endif
  }
  if (child3>0) {
#ifdef DEBUG
    fprintf(stderr,"Killing third child ... ");
#endif
    result=kill(child3,SIGKILL);
#ifdef DEBUG
    fprintf(stderr,"Result was %d\n",result);
#endif
  }
  fprintf(stderr,"done.\n");
#ifdef SOLARIS
/*  kill(0,SIGTERM);*/
#endif
  exit(0);
}


void Signal_Handler(sigcode)
     int sigcode;
{
  int result;

  fprintf(stderr,"Cleaning up...");

#ifndef SOLARIS
  if (child2>0) {
#ifdef DEBUG
    fprintf(stderr,"Killing second child ... ");
#endif
    result=kill(child2,SIGKILL);
#ifdef DEBUG
    fprintf(stderr,"Result was %d %d %d %d \n",result,child1,child2,child3);
    fprintf(stderr,"Result was %d\n",result);
#endif
  }
#endif
  signal(SIGALRM,Emergency_Closedown);
  alarm(3);

#ifdef DEBUG
    fprintf(stderr," Going OK past alarm(3) %d %d %d \n \n \n",child1,child2,child3);
#endif

  if (child1>0) {
#ifdef SGI
    waitpid(child1,NULL,0);
#else 
#ifdef SOLARIS
    waitpid(child1,NULL,0);
#else
    wait(child1,NULL,0);
#endif
#endif
  }

#ifdef DEBUG
    fprintf(stderr," Going OK past child1 %d %d %d \n",child1,child2,child3);
#endif

#ifndef SOLARIS
#ifndef CYGWIN
  if (child2>0) {
#ifdef SGI
    waitpid(child2,NULL,0);
#else
    wait(child2,NULL,0);
#endif
  }

#ifdef DEBUG
    fprintf(stderr," Going OK past wait 2 %d %d \n",child1,child2,child3);
#endif

  if (child3>0) {
#ifdef SGI
    waitpid(child3,NULL,0);
#else
    wait(child3,NULL,0);
#endif
  }
#endif
#endif

#ifdef DEBUG
    fprintf(stderr," Going OK past wait 3 %d %d %d \n",child1,child2,child3);
#endif

  alarm(0);
  fprintf(stderr,"done.\n");

#ifdef SOLARIS
  signal(SIGTERM,SIG_DFL);
  kill(0,SIGTERM);
#endif

  exit(0);
}

void exec_error (int result, int error_number)
{
  fprintf(stderr,"Exec failed in Xqplot\n");
  fprintf(stderr,"Result was %d, Error Number was %d\n",result,error_number);
  fflush(stderr);
  exit(1);
}


void start_qplot_main(void *arg)

{
  /* This should never return...*/
  #ifndef STANDALONE
      QPLOT_MAIN(progid);
  #endif
  _exit(1);
}

void start_watcher(void *arg)
{

#ifdef LINUX
  struct sigaction  act,oldact;
  sigset_t siggs;
#endif

#ifdef LINUX
  act.sa_handler=Signal_Handler;
  sigemptyset(&siggs);
  act.sa_mask=siggs;
  act.sa_flags=SA_NOCLDSTOP;
  sigaction(SIGCHLD,&act,&oldact);
#else
  signal(SIGCHLD,Signal_Handler);
#endif

  signal(SIGHUP,Signal_Handler);
  signal(SIGINT,Signal_Handler);
  signal(SIGQUIT,Signal_Handler);
  signal(SIGABRT,Signal_Handler);
  signal(SIGUSR1,Signal_Handler);
  signal(SIGUSR2,Signal_Handler);
  signal(SIGTERM,Signal_Handler);
  signal(SIGALRM,Signal_Handler);
  while(1) { pause(); }
}

void CLOSEDOWN (void)

{
  /*
  int ppid;
  ppid=getppid();
  kill(ppid,SIGTERM);
  */
}


#ifdef STANDALONE
   void REDUNDANT(int argc, char **argv)
#else
   void MAIN(int argc, char **argv)
#endif

{
  int pipefds[2],opipefds[2],result,errno;
  char cmd[100],term[1],minusf[3],path[100];
  char *t;
  char *bname;
  void *arg;
#ifdef LINUX
  struct sigaction  act,oldact;
  sigset_t siggs;
  int ierr;
#endif
#ifdef SOLARIS
  thread_t tid;
#endif

#ifdef CYGWIN
  bname=(char *)argv[0];
#else
  bname=(char *)basename(argv[0]);
#endif
  strcpy(progid,bname);

#ifdef LINUX
  shmid=shmget(IPC_PRIVATE,100,0x1ff);
  shmptr=shmat(shmid,0,0);
  shmctl(shmid,IPC_RMID,NULL);
  updatepage=(int *)shmptr;
  displaypage=updatepage++;
#else
  updatepage=malloc(sizeof(int));
  displaypage=malloc(sizeof(int));
#endif

  if (strncmp(bname,"xqplot",6)==0) {

#ifdef LINUX
    act.sa_handler=Signal_Handler;
    sigemptyset(&siggs);
    act.sa_mask=siggs;
    act.sa_flags=SA_NOCLDSTOP;
    sigaction(SIGCHLD,&act,&oldact);
#else
    signal(SIGCHLD,Signal_Handler);
#endif

    /* open two pipes for transfer of data */
    result=pipe(pipefds);
    result=pipe(opipefds);
    
    /* fork off process for wishx */
#ifdef LINUX
    child1=vfork();
#else
    child1=fork();
#endif
    if (child1==0) {
      /* duplicate file descriptors for pipes and attach to stdin/stdout */
      result=dup2(pipefds[1],STDOUT_FILENO);
      close(pipefds[0]);
      close(pipefds[1]);
      result=dup2(opipefds[0],STDIN_FILENO);
      close(opipefds[0]);
      close(opipefds[1]);

      strcpy(cmd,WISHX);
      strcpy(minusf,"-f");
      strcpy(path,XQPLOTTCL);
      strcpy(term,"");
      /* exec wishx with options */

      if (argc==0) {
	result=execl(cmd,minusf,path,NULL);
	exec_error(result,errno);
      }
      if (argc==1) {
	result=execl(cmd,minusf,path,argv[1],NULL);
	exec_error(result,errno);
      }
      if (argc==2) {
	result=execl(cmd,minusf,path,argv[1],argv[2],NULL);
	exec_error(result,errno);
      }
      result=execl(cmd,minusf,path,argv[1],argv[2],argv[3],NULL);
      exec_error(result,errno);
    }
    
    /* duplicate file descriptors and attach to stdin/stdout (other way round!)*/
    result=dup2(pipefds[0],STDIN_FILENO);
    close(pipefds[0]);
    close(pipefds[1]);
    result=dup2(opipefds[1],STDOUT_FILENO);
    close(opipefds[0]);
    close(opipefds[1]);
    
  } 


  /* fork off third child, will sit in pause loop waiting for termination
     of one of the other children */
  
#if defined SGI
  child3=sproc(start_qplot_main,PR_SADDR);
  start_watcher(arg);
  exit(0);
#elif defined SOLARIS
  thr_create(NULL,NULL,start_watcher,arg,NULL,&tid);
  child3=-1;
#endif
  

  /* This should never return...*/
  start_qplot_main(arg);
  
  /* But, just in case it does */
  _exit(1);
  
}


void WININI1()

{
  int pipefds[2],opipefds[2],result,errno;
  char cmd[100],term[1],minusf[3],path[100];
  char *t;
  char *bname;
  void *arg;
#ifdef LINUX
  struct sigaction  act,oldact;
  sigset_t siggs;
  int ierr;
#endif
#ifdef SOLARIS
  thread_t tid;
#endif

  bname=(char *)"qplot7";
  strcpy(progid,bname);

#ifdef LINUX
  shmid=shmget(IPC_PRIVATE,100,0x1ff);
  shmptr=shmat(shmid,0,0);
  shmctl(shmid,IPC_RMID,NULL);
  updatepage=(int *)shmptr;
  displaypage=updatepage++;
#else
  updatepage=malloc(sizeof(int));
  displaypage=malloc(sizeof(int));
#endif


  /* fork off third child, will sit in pause loop waiting for termination
     of one of the other children */
  
#if defined SGI
  child3=sproc(start_qplot_main,PR_SADDR);
  start_watcher(arg);
  exit(0);
#elif defined SOLARIS
  thr_create(NULL,NULL,start_watcher,arg,NULL,&tid);
  child3=-1;
#endif
  
#ifdef F2C
   xdevic_();
#elif defined CRAY
   xdevic();
#elif defined SOLARIS
   xdevic_();
#elif defined PGI
   xdevic_();
#elif defined IFC
   xdevic_();
#else
   xdevic__();
#endif
}

#ifdef IFC
void flush_(nfile)
     int *nfile;
{
}
#endif

void ColourInit()
{
  Pixmap          icon;
  XSizeHints      hints;
  long            size, i;
  XSetWindowAttributes attrib;
  unsigned long mycolour;

/* Can we open the named display ? if not, bomb out */

  if ((myDisplay = XOpenDisplay(display_name)) == NULL) {
    fprintf(stderr, "Can't open X server %s.\n",
	    XDisplayName(display_name));
    exit(1);
  }

/* Yep, open, get screen and note that it's open */

  myScreen = DefaultScreen(myDisplay);
  screen_open=1;

  /* Get the bit depth of the screen */

  myDepth=DefaultDepth(myDisplay,myScreen);

  /*  printf ("Bit Depth is %d\n",myDepth);*/

  myVisual=DefaultVisual(myDisplay,myScreen);

  /*    myVisual->class=3;    */
  /*    printf ("Visual class is %d \n",myVisual->class); */
  
/* screen dimensions */

  disp_wide = DisplayWidth(myDisplay, myScreen);
  disp_high = DisplayHeight(myDisplay, myScreen);

/* Create a window to display in */

  if (NoDisplay==0) {
    
    myWindow = XCreateSimpleWindow(myDisplay,
				   RootWindow(myDisplay, myScreen),
				   0,0, /* Hint at position */
				   Window_X_Size,
				   Window_Y_Size, 2 /* border width */ ,
				   WhitePixel(myDisplay, myScreen),
				   BlackPixel(myDisplay, myScreen));
  

/* create a graphics context for drawing */

    gcvalues.function = GXcopy;
    myGC = XCreateGC(myDisplay, myWindow, GCFunction, &gcvalues);

/* create a pixmap to represent the window off screen */
  
    myPixmap[0] = XCreatePixmap(myDisplay, myWindow,
				Window_X_Size, Window_Y_Size, myDepth);
    myPixmap[1] = XCreatePixmap(myDisplay, myWindow,
				Window_X_Size, Window_Y_Size, myDepth);
    
    *displaypage=0;


/* make a cute icon */

    icon = XCreateBitmapFromData(myDisplay, myWindow, qplot_bits, qplot_width,
				 qplot_height);
    
    /* give a few clues about myself */
    
    hints.flags = PPosition | PSize | PMinSize;
    hints.x = 0;
    hints.y = 0;
    hints.width = Window_X_Size;
    hints.height = Window_Y_Size;
    hints.min_width = 32;
    hints.min_height = 32;
    
    /* Set window name etc etc etc */
    
    XSetStandardProperties(myDisplay, myWindow,
			   "QPlot Output Window", "Output Window",
			   icon, NULL, 0,&hints);
    
    XMapWindow(myDisplay, myWindow);

    XSelectInput(myDisplay, myWindow, ButtonPressMask|ButtonReleaseMask);


  } else {
    myPixmap[0] = XCreatePixmap(myDisplay, DefaultRootWindow(myDisplay),
				Window_X_Size, Window_Y_Size, myDepth);
    myPixmap[1] = XCreatePixmap(myDisplay, DefaultRootWindow(myDisplay),
				Window_X_Size, Window_Y_Size, myDepth);
    gcvalues.function = GXcopy;
    myGC = XCreateGC(myDisplay, myPixmap[0], GCFunction, &gcvalues);
    *displaypage=-1;
  }
  *updatepage=0;

/* Get Black and White Colours */
  
  myWhitePixel=WhitePixel(myDisplay,myScreen);
  myBlackPixel=BlackPixel(myDisplay,myScreen);

  /* Clear the screen... */

  mycolour=myBlackPixel;
  XSetForeground(myDisplay,myGC,mycolour);
  if (NoDisplay==0) {
    XFillRectangle(myDisplay,myWindow,myGC,0,0,Window_X_Size,
		   Window_Y_Size); 
  }
  XFillRectangle(myDisplay,myPixmap[0],myGC,0,0,Window_X_Size,
		 Window_Y_Size);
  XFillRectangle(myDisplay,myPixmap[1],myGC,0,0,Window_X_Size,
		 Window_Y_Size);

/* Set initial Foreground as Black */

  XSetForeground(myDisplay,myGC,myBlackPixel);
  myCurrentColour=myBlackPixel;

/* And Background as Black */

  XSetBackground(myDisplay,myGC,myBlackPixel);



/* Make sure that it's all happened */

  XFlush(myDisplay);

}

void ColourClose()
{
  XFreePixmap(myDisplay, myPixmap[0]);
  XFreePixmap(myDisplay, myPixmap[1]);
  if (NoDisplay==0) {
    XDestroyWindow(myDisplay, myWindow);
  }
  XCloseDisplay(myDisplay);
}


static void getcolours(ncols)
     int ncols;
{
  Bool contig;
  Colormap cmap;
  unsigned long planes_return[1];
  unsigned int nplanes;
  unsigned int npixels;
  int i;
  XColor tmp;

  if (ncols > MAX_COLOURS_EVER) 
    {
      fprintf(stderr,"%d colours requested but hard limit is %d.\n",ncols,MAX_COLOURS_EVER);
      exit(1);
    }

  cmap=DefaultColormap(myDisplay, myScreen);

  if (((myVisual->class)&1)==1) {

    contig=0;
    nplanes=0;
    npixels=ncols;
    
    if (!XAllocColorCells(myDisplay,cmap,contig,planes_return,
			  nplanes,colorpixels,npixels)) 
      {
        fprintf(stderr,"Was unable to get %d colours, exiting.\n",ncols);
        for (npixels=1; npixels < ncols; npixels++)
	  {
            if (XAllocColorCells(myDisplay,cmap,contig,planes_return,
		      	          nplanes,colorpixels,npixels)) 
	    
               {
                 fprintf(stderr,"Was able to get %d colours. \n",npixels);
               }
	  } 
        fprintf(stderr,"Was unable to get %d colours, exiting.\n",ncols);
 	exit(1);
      }
    colourcount=ncols;
  } else {
    if (myDepth<15) {
      fprintf(stderr,"Cannot work with a colormap that isn't either dynamically\n");
      fprintf(stderr,"changeable or at least 15bpp. Sorry.\n");
      exit(1);
    }
    for (i=0; i < ncols ; i++ ) 
      {
	tmp.red = 0;
	tmp.green = 0;
	tmp.blue = 0;
	tmp.flags = DoRed | DoGreen | DoBlue ;
	if (!XAllocColor(myDisplay,cmap,&tmp)) {
	  fprintf(stderr,"Couldn't allocate colour %d of %d \n",i,ncols);
	  exit(1);
	}
	colorpixels[i] = tmp.pixel;
      }
    colourcount=ncols;
  }
  
}

static void setcolours(colarray)
     unsigned short colarray[256*3];
{
  Colormap        cmap;
  int      i;
  XColor   tmp;
  int retcode;

  cmap = DefaultColormap(myDisplay, myScreen);


  if ((myVisual->class&1)==1) {
  
    for (i=0; i < colourcount ; i++ ) 
      {
	tmp.red = colarray[i*3];
	tmp.green = colarray[i*3+1];
	tmp.blue = colarray[i*3+2];
	tmp.flags = DoRed | DoGreen | DoBlue ;
	tmp.pixel=colorpixels[i];
	XStoreColor(myDisplay,cmap,&tmp);
	
      }

  } else {

    XFreeColors(myDisplay,cmap,colorpixels,colourcount,0);
    /*    printf("XFreeColors returned %d \n",retcode);*/
    for (i=0; i < colourcount ; i++ ) {
      tmp.red = colarray[i*3];
      tmp.green = colarray[i*3+1];
      tmp.blue = colarray[i*3+2];
      tmp.flags = DoRed | DoGreen | DoBlue ;
      if (!XAllocColor(myDisplay,cmap,&tmp)) {
	fprintf(stderr,"Couldn't allocate colour %d of %d \n",i,colourcount);
	exit(1);
      }
      colorpixels[i] = tmp.pixel;
      /*      printf("%d %d %d %d %d\n",i,tmp.red,tmp.green,tmp.blue,tmp.pixel);*/
    }
  }
}

static void freecolours()
{
  Colormap cmap;
  int i;
  unsigned long pixels[MAX_COLOURS_EVER];
  unsigned long planes;

  cmap = DefaultColormap(myDisplay, myScreen);
  XFreeColors(myDisplay,cmap,colorpixels,colourcount,0);
  colourcount=0;

}

void SigTerm_Handler(sigcode)
     int sigcode;
/* Handle death of child process cleanly */
{
  XCloseDisplay(newDisplay);
  _exit(0);
}

void CheckExposed()
/* Simple Routine to check if part of the window has become exposed
   checks each time that a draw routine is made, and copies across 
   entire area if necessary. This is run as a child process of the 
   original qplot program... Opens its own connection to the Xserver to
   prevent problems, checks for update once a second */
{
  XExposeEvent report;

  newDisplay=XOpenDisplay(display_name);
  XSelectInput(newDisplay, myWindow, ExposureMask);

  while (1) {
    XWindowEvent(newDisplay,myWindow,ExposureMask,(XEvent*) &report);
    if (report.type!=14) {
      if (report.count==0)
      XCopyArea(newDisplay,myPixmap[*displaypage],myWindow,myGC,0,0,Window_X_Size,
		Window_Y_Size,0,0);
    }
  }
}

void CEChild(void *arg)
{
  signal(SIGTERM,SigTerm_Handler);
  CheckExposed(); 
  exit(0);
}



void ColourLine(x1,y1,x2,y2,colour)
     int x1,x2,y1,y2;
     unsigned char colour;
/* Draws a line after setting the colour if neccessary */
{
  unsigned long mycolour;
  mycolour=colorpixels[colour];
  if (myCurrentColour!=mycolour)
    { XSetForeground(myDisplay,myGC,mycolour); 
      myCurrentColour=mycolour ; }
  if (*updatepage == *displaypage)
    XDrawLine(myDisplay,myWindow,myGC,x1,y1,x2,y2);
  XDrawLine(myDisplay,myPixmap[*updatepage],myGC,x1,y1,x2,y2);
  /*    XDrawLine(myDisplay,myWindow,myGC,x1,y1,x2,y2);
  XDrawLine(myDisplay,myPixmap[*updatepage],myGC,x1,y1,x2,y2);*/
}

void WhiteLine(x1,y1,x2,y2)
     int x1,x2,y1,y2;
/* draws a white line ! */
{
  unsigned long mycolour;
  mycolour=myWhitePixel;
  if (myCurrentColour!=mycolour)
    { XSetForeground(myDisplay,myGC,myWhitePixel);
      myCurrentColour=mycolour; }
  if (*updatepage == *displaypage)
    XDrawLine(myDisplay,myWindow,myGC,x1,y1,x2,y2);
  XDrawLine(myDisplay,myPixmap[*updatepage],myGC,x1,y1,x2,y2);
}

void MoveTo(x,y)
     int x,y;
/* Lifts pen off the paper and moves it */
{
  Last_X_Pos=x;
  Last_Y_Pos=y;
}

void WhiteLineTo(x,y)
     int x,y;
/* Draws a white line relative to last position */
{
  unsigned long mycolour;
  mycolour=myWhitePixel;
  if (myCurrentColour!=mycolour)
    { XSetForeground(myDisplay,myGC,myWhitePixel);
      myCurrentColour=mycolour; }
  if (*updatepage == *displaypage)
    XDrawLine(myDisplay,myWindow,myGC,Last_X_Pos,Last_Y_Pos,x,y);
  XDrawLine(myDisplay,myPixmap[*updatepage],myGC,Last_X_Pos,Last_Y_Pos,x,y);
  Last_X_Pos=x;
  Last_Y_Pos=y;
}

void ColourLineTo(x,y,colour)
     int x,y,colour;
/* Draws a colour line relative to last position */
{
  unsigned long mycolour;
  mycolour=colorpixels[colour];
  if (myCurrentColour!=mycolour)
    { XSetForeground(myDisplay,myGC,mycolour); 
      myCurrentColour=mycolour ; }
  if (*updatepage == *displaypage)
    XDrawLine(myDisplay,myWindow,myGC,Last_X_Pos,Last_Y_Pos,x,y);
  XDrawLine(myDisplay,myPixmap[*updatepage],myGC,Last_X_Pos,Last_Y_Pos,x,y);
  Last_X_Pos=x;
  Last_Y_Pos=y;
}

void flush_points() {
  if (myCurrentColour!=pointcol)
    { XSetForeground(myDisplay,myGC,pointcol); 
      myCurrentColour=pointcol ; 
    }
  if (*updatepage == *displaypage)
    XDrawPoints(myDisplay,myWindow,myGC,pointlist,pointcount,CoordModeOrigin);
  XDrawPoints(myDisplay,myPixmap[*updatepage],myGC,pointlist,pointcount,CoordModeOrigin);
  pointcount=0;
}
  

void SetPixel(x,y,colour)
     int x,y,colour;

{
  unsigned long mycolour;
  mycolour=colorpixels[colour];
  if (mycolour!=pointcol) {
    if(pointcount) {flush_points();}
    pointcol=mycolour;
  }
  pointlist[pointcount].x=x;
  pointlist[pointcount].y=y;
  pointcount++;
  if (pointcount==MAX_POINTS) {
    flush_points();
  }
}

/*
   ----------------------------------------------------------------------

Above this point, all subroutines should be internal to xwindows.c, 
below this point, any subroutine may be called by an external fortran
program, the uppercase names *are* importent, they're #defined at the
top to get either 1 or 2 _'s on the end for sun fortran or f2c

   ----------------------------------------------------------------------

*/

void X_CHANGE_FONT(ifn1,itype)
   int *ifn1,itype;

{
  char font[100];

  /* New code for loading fonts */
    
  strcpy(font,XFONT);

  if (*ifn1==0) strcat(font,"/cour11.fnt");
  if (*ifn1==1) strcat(font,"/cour11b.fnt");
  if (*ifn1==2) strcat(font,"/cour11bi.fnt");
  if (*ifn1==3) strcat(font,"/cour11i.fnt");
  if (*ifn1==4) strcat(font,"/cour12.fnt");
  if (*ifn1==5) strcat(font,"/cour12b.fnt");
  if (*ifn1==6) strcat(font,"/cour12bi.fnt");
  if (*ifn1==7) strcat(font,"/cour12i.fnt");
  if (*ifn1==8) strcat(font,"/cour14.fnt");
  if (*ifn1==9) strcat(font,"/cour14b.fnt");
  if (*ifn1==10) strcat(font,"/cour14bi.fnt");
  if (*ifn1==11) strcat(font,"/cour14i.fnt");
  if (*ifn1==12) strcat(font,"/cour16.fnt");
  if (*ifn1==13) strcat(font,"/cour16b.fnt");
  if (*ifn1==14) strcat(font,"/cour16bi.fnt");
  if (*ifn1==15) strcat(font,"/cour16i.fnt");
  if (*ifn1==16) strcat(font,"/cour20.fnt");
  if (*ifn1==17) strcat(font,"/cour20b.fnt");
  if (*ifn1==18) strcat(font,"/cour20bi.fnt");
  if (*ifn1==19) strcat(font,"/cour20i.fnt");
  if (*ifn1==20) strcat(font,"/cour25.fnt");
  if (*ifn1==21) strcat(font,"/cour25b.fnt");
  if (*ifn1==22) strcat(font,"/cour25bi.fnt");
  if (*ifn1==23) strcat(font,"/cour25i.fnt");
  if (*ifn1==24) strcat(font,"/cour34.fnt");
  if (*ifn1==25) strcat(font,"/cour34b.fnt");
  if (*ifn1==26) strcat(font,"/cour34bi.fnt");
  if (*ifn1==27) strcat(font,"/cour34i.fnt");
  if (*ifn1==28) strcat(font,"/helv11.fnt");
  if (*ifn1==29) strcat(font,"/helv11b.fnt");
  if (*ifn1==30) strcat(font,"/helv11bi.fnt");
  if (*ifn1==31) strcat(font,"/helv11i.fnt");
  if (*ifn1==32) strcat(font,"/helv13.fnt");
  if (*ifn1==33) strcat(font,"/helv13b.fnt");
  if (*ifn1==34) strcat(font,"/helv13bi.fnt");
  if (*ifn1==35) strcat(font,"/helv13i.fnt");
  if (*ifn1==36) strcat(font,"/helv15.fnt");
  if (*ifn1==37) strcat(font,"/helv15b.fnt");
  if (*ifn1==38) strcat(font,"/helv15bi.fnt");
  if (*ifn1==39) strcat(font,"/helv15i.fnt");
  if (*ifn1==40) strcat(font,"/helv17.fnt");
  if (*ifn1==41) strcat(font,"/helv17b.fnt");
  if (*ifn1==42) strcat(font,"/helv17bi.fnt");
  if (*ifn1==43) strcat(font,"/helv17i.fnt");
  if (*ifn1==44) strcat(font,"/helv22.fnt"); 
  if (*ifn1==45) strcat(font,"/helv22b.fnt");
  if (*ifn1==46) strcat(font,"/helv22bi.fnt");
  if (*ifn1==47) strcat(font,"/helv22i.fnt");
  if (*ifn1==48) strcat(font,"/helv29.fnt");
  if (*ifn1==49) strcat(font,"/helv29b.fnt");
  if (*ifn1==50) strcat(font,"/helv29bi.fnt");
  if (*ifn1==51) strcat(font,"/helv29i.fnt");
  if (*ifn1==52) strcat(font,"/helv38.fnt");
  if (*ifn1==53) strcat(font,"/helv38b.fnt");
  if (*ifn1==54) strcat(font,"/helv38bi.fnt");
  if (*ifn1==55) strcat(font,"/helv38i.fnt");
  if (*ifn1==56) strcat(font,"/pc6x14.fnt");
  if (*ifn1==57) strcat(font,"/pc6x8.fnt");
  if (*ifn1==58) strcat(font,"/pc8x14.fnt");
  if (*ifn1==59) strcat(font,"/pc8x14t.fnt");
  if (*ifn1==60) strcat(font,"/pc8x16.fnt");
  if (*ifn1==61) strcat(font,"/pc8x8.fnt");
  if (*ifn1==62) strcat(font,"/pc8x8t.fnt");
  if (*ifn1==63) strcat(font,"/symb11.fnt");
  if (*ifn1==64) strcat(font,"/symb14.fnt");
  if (*ifn1==65) strcat(font,"/symb16.fnt");
  if (*ifn1==66) strcat(font,"/symb20.fnt");
  if (*ifn1==67) strcat(font,"/symb25.fnt");
  if (*ifn1==68) strcat(font,"/symb32.fnt");
  if (*ifn1==69) strcat(font,"/symb34.fnt");
  if (*ifn1==70) strcat(font,"/tms11.fnt");
  if (*ifn1==71) strcat(font,"/tms11b.fnt");
  if (*ifn1==72) strcat(font,"/tms11bi.fnt");
  if (*ifn1==73) strcat(font,"/tms11i.fnt");
  if (*ifn1==74) strcat(font,"/tms13.fnt");
  if (*ifn1==75) strcat(font,"/tms13b.fnt");
  if (*ifn1==76) strcat(font,"/tms13bi.fnt");
  if (*ifn1==77) strcat(font,"/tms13i.fnt");
  if (*ifn1==78) strcat(font,"/tms15.fnt");
  if (*ifn1==79) strcat(font,"/tms15b.fnt");
  if (*ifn1==80) strcat(font,"/tms15bi.fnt");
  if (*ifn1==81) strcat(font,"/tms15i.fnt");
  if (*ifn1==82) strcat(font,"/tms18.fnt");
  if (*ifn1==83) strcat(font,"/tms18b.fnt");
  if (*ifn1==84) strcat(font,"/tms18bi.fnt");
  if (*ifn1==85) strcat(font,"/tms18i.fnt");
  if (*ifn1==86) strcat(font,"/tms22.fnt");
  if (*ifn1==87) strcat(font,"/tms22b.fnt");
  if (*ifn1==88) strcat(font,"/tms22bi.fnt");
  if (*ifn1==89) strcat(font,"/tms22i.fnt");
  if (*ifn1==90) strcat(font,"/tms29.fnt");
  if (*ifn1==91) strcat(font,"/tms29b.fnt");
  if (*ifn1==92) strcat(font,"/tms29bi.fnt");
  if (*ifn1==93) strcat(font,"/tms29i.fnt");
  if (*ifn1==94) strcat(font,"/tms38.fnt");
  if (*ifn1==95) strcat(font,"/tms38b.fnt");
  if (*ifn1==96) strcat(font,"/tms38bi.fnt");
  if (*ifn1==97) strcat(font,"/tms38i.fnt");
  if (*ifn1==98) strcat(font,"/xm10x17.fnt");
  if (*ifn1==99) strcat(font,"/xm10x17b.fnt");
  if (*ifn1==100) strcat(font,"/xm10x20.fnt");
  if (*ifn1==101) strcat(font,"/xm10x20b.fnt");
  if (*ifn1==102) strcat(font,"/xm12x20.fnt");
  if (*ifn1==103) strcat(font,"/xm12x20b.fnt");
  if (*ifn1==104) strcat(font,"/xm16x25.fnt");
  if (*ifn1==105) strcat(font,"/xm16x25b.fnt");
  if (*ifn1==106) strcat(font,"/xm16x25i.fnt");
  if (*ifn1==107) strcat(font,"/xm4x5.fnt");
  if (*ifn1==108) strcat(font,"/xm4x6.fnt");
  if (*ifn1==109) strcat(font,"/xm5x8.fnt");
  if (*ifn1==110) strcat(font,"/xm6x10.fnt");
  if (*ifn1==111) strcat(font,"/xm6x10b.fnt");
  if (*ifn1==112) strcat(font,"/xm6x12.fnt");
  if (*ifn1==113) strcat(font,"/xm6x12b.fnt");
  if (*ifn1==114) strcat(font,"/xm6x12i.fnt");
  if (*ifn1==115) strcat(font,"/xm7x13.fnt");
  if (*ifn1==116) strcat(font,"/xm7x13b.fnt");
  if (*ifn1==117) strcat(font,"/xm8x12.fnt");
  if (*ifn1==118) strcat(font,"/xm8x12b.fnt");
  if (*ifn1==119) strcat(font,"/xm8x16.fnt");
  if (*ifn1==120) strcat(font,"/xm8x16b.fnt");
  if (*ifn1==121) strcat(font,"/xm8x16i.fnt");
  if (*ifn1==122) strcat(font,"/xm9x15.fnt");
  if (*ifn1==123) strcat(font,"/xm9x15b.fnt");
  if (*ifn1>123) {
     printf(" incorrect font choice ");
     exit(1);
  }

  if (itype==1) {
     if (initialise_font==1) GrUnloadFont(fsm);
     fsm=GrLoadFont(font); 
  }
  if (itype==2) {
     if (initialise_font==1) GrUnloadFont(fsr);
     fsr=GrLoadFont(font); 
  }
}

void X_CHANGE_FONT1(ifn1)
   int *ifn1;
{
   X_CHANGE_FONT(ifn1,1);
}

void X_CHANGE_FONT2(ifn1)
   int *ifn1;
{
   X_CHANGE_FONT(ifn1,2);
}

void X_DISPLAYINIT(xx,yy,obl,ifn1,ifn2)
     int *xx,*yy,*ifn1,*ifn2;
     float *obl;
/* Routine called externally to initialize display of X-Windows graphics
   xx,yy indicate size of display area to request */
     /*  NOTE : if both xx and yy are negative, output will only occur
	 to pixmaps, allowing use as a backend for a web page */
{
  void *arg;
#ifdef SOLARIS
  thread_t tid;
#endif

  NoDisplay=0;
  xx1=*xx;
  yy1=*yy;
  oblate=*obl;
  nowin=0;
  if (*obl<0.0) {
     oblate=-*obl;
     nowin=1;
     NoDisplay=1;
  } 


/* Get name of DISPLAY environment variable */

  display_name=getenv(DISPENV);
  if (display_name == NULL) {
    display_name=display_none;
  }

  if ((*xx<0)&&(*yy<0)) {
    *xx=-*xx;
    *yy=-*yy;
    NoDisplay=1;
  }
    
  Window_X_Size=*xx;
  Window_Y_Size=*yy;

  memset(&opt,0,sizeof(opt));
  
  initialise_font=0;   
  X_CHANGE_FONT(ifn1,1);
  X_CHANGE_FONT(ifn2,2);
  initialise_font=1;   

  if (nowin==0) ColourInit();
  Display_init=1;
/* 
   Now fork off the process to update the dislay
   Pity we can't rename it as 'qplot helper' or something
   fork off second child process that will keep the display refreshed 
*/
  if (NoDisplay==0) {
    
#if defined SGI
    child2=sproc(CEChild,PR_SADDR);
    /*    child2=-1;*/
#elif defined SOLARIS
    thr_create(NULL,NULL,CEChild,arg,NULL,&tid);
    child2=-1;
#elif defined LINUX_CLONE
    child2=clone(0,SIGCLD);
#else
    child2=fork();
#endif
    
    if (child2==0) {
      signal(SIGTERM,SigTerm_Handler);
      CheckExposed(); 
      exit(0);
    }

    /* fork off third child, will sit in pause loop waiting for termination
       of one of the other children */
    
#if defined LINUX_CLONE
    child3=clone(0,SIGCLD);
#elif defined LINUX
    child3=fork();
#endif

#ifdef LINUX
    if (child3!=0) {
      start_watcher(arg);
    }
#endif

#ifdef CYGWIN
    child3=fork();
    if (child3!=0) {
      start_watcher(arg);
    }
#endif

#ifdef CRAY
    child3=fork();
    if (child3!=0) {
      start_watcher(arg);
    }
#endif


    
  }
  
}

void X_GETCOLOURS(ncols)
     int *ncols;
{
  if (nowin==0) getcolours(*ncols);
}

void X_SETCOLOURS(colarray)
     unsigned short colarray[256*3];
{
  if (nowin==0) setcolours(colarray);
}


void X_FREECOLOURS()
{
  /*  Colormap cmap;
      cmap = DefaultColormap(myDisplay, myScreen);
      XFreeColors(myDisplay,cmap,colorpixels,num_cols,0);
  */
  if (nowin==0) freecolours();
}


void X_DISPLAYDONE()
{
  if (nowin==0) {if (screen_open) ColourClose ();}
  /*  kill (my_child_id,SIGTERM);*/
}

void X_DISPLAYFLUSH()
{
  if (nowin==0) {
  flush_points();
  XFlush(myDisplay);}
}

void X_CURINI(ifail)
  int *ifail;
{
  /*  fprintf(stderr,"in curini\n");*/

  *ifail=0;
}

void X_CURCLS()
{
  /*  fprintf(stderr,"in curcls\n");*/
}

void X_CURSR(xp,yp,key)
   float *xp,*yp;
   int *key;
{
  int ix,iy,ibut;
  float xt,yt;

  /*  fprintf(stderr,"in cursr\n");
  fprintf(stderr,"%f %f %d\n",*xp,*yp,*key);
  fprintf(stderr,"%p %p %p\n",xp,yp,key);
  */

  XWindowEvent(myDisplay,myWindow,ButtonPressMask,(XEvent*) &myev);
  XWindowEvent(myDisplay,myWindow,ButtonReleaseMask,(XEvent*) &myev2);

  ix=myev.x;
  iy=myev.y;
  switch (myev.button) {
  case Button1:
    ibut=1;
    break;
  case Button2:
    ibut=3;
    break;
  case Button3:
    ibut=2;
    break;
  }
  /*  fprintf(stderr,"ix %d iy %d \n",ix,iy);*/

  xt= ((float) ix) / ((float) xx1);
  xt= xt / oblate;
  yt= ((float) iy) / ((float) yy1);
  yt= 1.0 - yt;

  /*  fprintf(stderr,"x %f y %f %d\n",xt,yt,sizeof(yt));*/

  *xp=xt;
  *yp=yt;
  *key=ibut;
}

void X_FILLPOLY (np,X,Y,col)
     int *np,*col;
     int X[MAXPOINTS];
     int Y[MAXPOINTS];
{

  XPoint points[MAXPOINTS];
  int i;
  unsigned long mycolour;

  if (nowin==0) {
  mycolour=colorpixels[*col];
  if (*col==-2) mycolour=myWhitePixel;
  if (myCurrentColour!=mycolour)
    { XSetForeground(myDisplay,myGC,mycolour); 
      myCurrentColour=mycolour ; }

  if (*np>MAXPOINTS)
    { fprintf(stderr, "Too many points in polygon") ; 
      exit(1) ;
    }
  
  for (i=0 ; i<*np ; i++ )
    { points[i].x=X[i];
      points[i].y=Y[i]; 
    }
  if (*updatepage == *displaypage)
    XFillPolygon(myDisplay,myWindow,myGC,points,*np,Complex,CoordModeOrigin);
  XFillPolygon(myDisplay,myPixmap[*updatepage],myGC,points,*np,Complex,CoordModeOrigin);
  }
}

void X_POLYLINE (np,X,Y,col)
     int *np,*col;
     int X[MAXPOINTS];
     int Y[MAXPOINTS];
{

  XPoint points[MAXPOINTS];
  int i;
  unsigned long mycolour;

  if (nowin==0) {
  mycolour=colorpixels[*col];
  if (*col==-2) mycolour=myWhitePixel;
  if (myCurrentColour!=mycolour)
    { XSetForeground(myDisplay,myGC,mycolour); 
      myCurrentColour=mycolour ; }

  if (*np>MAXPOINTS)
    { fprintf(stderr, "Too many points in polyline") ; 
      exit(1) ;
    }

  for (i=0 ; i<*np ; i++ )
    { points[i].x=X[i];
      points[i].y=Y[i]; 
    }
  if (*updatepage == *displaypage)
    XDrawLines(myDisplay,myWindow,myGC,points,*np,CoordModeOrigin);
  XDrawLines(myDisplay,myPixmap[*updatepage],myGC,points,*np,CoordModeOrigin);
  }
}

void X_GETPIXEL (ix,iy,col)
     int *ix,*iy,*col;
{
}

void X_SETPIXEL(ix,iy,col)
     int *ix,*iy,*col;
{
  if (nowin==0) SetPixel(*ix,*iy,*col);
}


void X_LINETO(x,y,col)
     int *x,*y;
     int *col;
{
  /*  fprintf(stderr,"Colour : %d to x %d to y %d \n",*col,*x,*y); */
  if (nowin==0) {
  if (*col==-1) 
    { MoveTo(*x,*y); }
  else if (*col==-2) 
    { WhiteLineTo(*x,*y); }
   else 
    { ColourLineTo(*x,*y,*col); } 
  }
}

void X_SETLINESTYLE(thick,style,ondots1,offdots1,ondots2,offdots2)
     int *thick,*style,*ondots1,*offdots1,*ondots2,*offdots2;
/* Setup line style for drawing
   thickness in pixels
   style 0= solid, 1=dotted/dashed
   ondots length of on section of dotted line
   offdots length of off section of dotted line
*/
{
  int NewLineStyle;
  unsigned int NewLineWidth;
  char dash_list[4];

/*  printf(" %d %d %d %d \n",*thick,*style,*ondots,*offdots); */
  if (nowin==0) {
  NewLineWidth=*thick;
  if (*style==0) 
    { NewLineStyle=LineSolid ;}
  else
    { NewLineStyle=LineOnOffDash ;}
  
  if ((NewLineStyle!=myCurrentLineStyle)||(NewLineWidth!=myCurrentLineWidth))
      { XSetLineAttributes(myDisplay,myGC,NewLineWidth,NewLineStyle,CapButt,
			   JoinMiter);
	myCurrentLineStyle=NewLineStyle;
	myCurrentLineWidth=NewLineWidth;
      }

  if (NewLineStyle==LineOnOffDash) 
    { dash_list[0]=*ondots1;
      dash_list[1]=*offdots1;
      dash_list[2]=*ondots2;
      dash_list[3]=*offdots2;
      XSetDashes(myDisplay,myGC,0,dash_list,4);
    }
  }
}
  
void X_CLEAR()
/* Blank Display Area to allow a further plot */
{
  unsigned long mycolour;
  if (nowin==0) {
  mycolour=myBlackPixel;
  myCurrentColour=mycolour;
  XSetForeground(myDisplay,myGC,mycolour);
  if (*updatepage == *displaypage)
    XFillRectangle(myDisplay,myWindow,myGC,0,0,Window_X_Size,
		   Window_Y_Size);
  XFillRectangle(myDisplay,myPixmap[*updatepage],myGC,0,0,Window_X_Size,
		 Window_Y_Size);
  XFlush(myDisplay);
  }
}

void X_UPDATEPAGE(page)
     int *page;
{
  *updatepage=*page;
}


void X_DISPLAYPAGE(page)
     int *page;
{
  if (nowin==0) {
  if ((Display_init == 1)&&(NoDisplay==0))
  {  *displaypage=*page;
     XCopyArea(myDisplay,myPixmap[*displaypage],myWindow,myGC,0,0,Window_X_Size,
	    Window_Y_Size,0,0);
     XFlush(myDisplay); }
}
}


void X_GRABAREA(x1,y1,x2,y2,image)
     int *x1,*x2,*y1,*y2;
     int *image;
{
  XImage *myimage;
  int i,j,k;
  int dpix,page;
  unsigned long pixel;

  page=*displaypage;
  if (NoDisplay==1) {
    page=*updatepage;
  }
  
  myimage=XGetImage(myDisplay,myPixmap[page],*x1,*y1,*x2-*x1+1,
                    *y2-*y1+1,-1,XYPixmap);

  for (j=*y1 ; j<=*y2 ; j++ )
    for (i=*x1 ; i<=*x2 ; i++ )
      {
	pixel=XGetPixel(myimage,i,j);
	if (pixel == myWhitePixel) 
	  {dpix=-1;} 
	else
	  if (pixel == myBlackPixel) 
	    {dpix=-2;}
	  else
	    for (k=0;k<colourcount;k++)
	      if (colorpixels[k]==pixel) dpix=k;
        *image=dpix;
	image++;
      }


  XDestroyImage(myimage);

}
  
void beep_()
/* Leave this in, it fixes the need for a beep in zxplotpc */
{
}
