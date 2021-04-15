#include "grx20.h"
#include "libgrx.h"
#include "clipping.h"

void GrDrawString(void *text,int length,int x,int y,GrTextOption *opt,int nowin)
{
	GrColorTableP fgcp = opt->txo_fgcolor.p;
	GrColorTableP bgcp = opt->txo_bgcolor.p;
	long fgcv = opt->txo_fgcolor.v;
	long bgcv = opt->txo_bgcolor.v;
	int  undl = (fgcv & GR_UNDERLINE_TEXT) ? 1 : 0;
	GrFont *f = opt->txo_font;
	int rotat = GR_TEXT_IS_VERTICAL(opt->txo_direct) ? ~0 : 0;
	int dxpre = 0,dypre = 0,dxpost = 0,dypost = 0;
	int ww,hh,x1,y1,x2,y2;
	int type,step;
	if(!f) return;
	x1 = GrFontStringWidth(f,text,length,opt->txo_chrtype);
	y1 = f->h.height;
	if(!x1) return;
	ww = (x1 & ~rotat) | (y1 &  rotat);
	hh = (x1 &  rotat) | (y1 & ~rotat);
	switch(opt->txo_xalign) {
	  case GR_ALIGN_RIGHT:
	    x -= ww - 1;
	    break;
	  case GR_ALIGN_CENTER:
	    x -= (ww >> 1);
	    break;
	}
	switch(opt->txo_yalign) {
	  case GR_ALIGN_BASELINE:
	    if(opt->txo_direct == GR_TEXT_DEFAULT) y -= f->h.baseline;
	    break;
	  case GR_ALIGN_BOTTOM:
	    y -= hh - 1;
	    break;
	  case GR_ALIGN_CENTER:
	    y -= (hh >> 1);
	    break;
	}
	switch(opt->txo_direct) {
	  case GR_TEXT_DOWN:
	    dypost = ~0;
	    break;
	  case GR_TEXT_LEFT:
	    dxpre = ~0;
	    x += ww;
	    break;
	  case GR_TEXT_UP:
	    dypre = ~0;
	    y += hh;
	    break;
	  default:
	    dxpost = ~0;
	    break;
	}
	type = opt->txo_chrtype;
	step = GR_TEXTCHR_SIZE(type);
	while(--length >= 0) {
	    int  chr = GR_TEXTSTR_CODE(text,type);
	    int  attr,xx,yy,cw,ch;
	    char far *bmp;
	    if(type == GR_ATTR_TEXT) {
		attr = GR_TEXTSTR_ATTR(text,GR_ATTR_TEXT);
		fgcv = GR_CTABLE_COLOR(fgcp,GR_ATTR_FGCOLOR(attr));
		bgcv = GR_CTABLE_COLOR(bgcp,GR_ATTR_BGCOLOR(attr));
		undl = GR_ATTR_UNDERLINE(attr);
	    }
	    text = (void *)((char *)(text) + step);
	    x1 = GrFontCharWidth(f,chr);
	    y1 = f->h.height;
	    cw = (x1 & ~rotat) | (y1 &  rotat);
	    ch = (x1 &  rotat) | (y1 & ~rotat);
	    x1 = (x -= (cw & dxpre));
	    y1 = (y -= (ch & dypre));
	    x2 = (xx = x1) + cw - 1;
	    y2 = (yy = y1) + ch - 1;
	    x += (cw & dxpost);
	    y += (ch & dypost);
	    bmp = GrFontCharAuxBmp(f,chr,opt->txo_direct,undl);
	    if(bmp) {
	        xdrawbitmap(
		(x1 + CURC->gc_xoffset),
		(y1 + CURC->gc_yoffset),
		(x2 - x1 + 1),
		(y2 - y1 + 1),
		bmp,
		((cw + 7) >> 3),
		((x1 - xx) + ((y1 - yy) * ((cw + 7) & ~7))),
		fgcv,bgcv,nowin
	    );
	    }
	    else (*FDRV->drawblock)(
		(x1 + CURC->gc_xoffset),
		(y1 + CURC->gc_yoffset),
		(x2 - x1 + 1),
		(y2 - y1 + 1),
		bgcv
	    );
	}
}

