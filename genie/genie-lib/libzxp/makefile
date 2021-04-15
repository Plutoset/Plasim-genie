#
# $Id: makefile 6303 2011-03-30 14:01:52Z gethin-williams $
#
#    This is the make file for the unix (Linux) version
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(LIBZXPPREC)

#
# This next bit copied from the goldstein makefile. 
#
.SUFFIXES: .o .f

.f.o:
	$(F77) $(LOCALFLAGS) -c $*.f

.f:
	$(F77) $(LOCALFLAGS) -o $@ $<

#
LIBNAME = libzxp
OBJECTS= \
zvectc.o zrcnta.o zcstln.o zvecta.o zrcntr.o zoutln.o zcstfl.o \
zcstll.o zcstch.o zgcnta.o dzgcnt.o zcont1.o zcontc.o zcontb.o \
zconta.o zlabof.o zgcntr.o xclear.o zxyref.o zprojc.o xrcnta.o \
zsfplt.o zsf001.o zrcntb.o xrcntr.o xvects.o xhatch.o xhatcy.o \
xhatcx.o xgraph.o xcurve.o xcfont.o xcfill.o xconts.o xchlen.o \
xvectk.o xclimt.o xcaptn.o xbordr.o xaxes.o xspace.o xvectr.o \
vidlin.o vidini.o vframe.o dport.o xbpsc.o xdevic.o xzcnta.o \
xrnumb.o xinumb.o xconta.o xaxsca.o xaxisy.o xaxisx.o dcolor.o \
zfud.o zcontr.o zbuf.o xsinx.o xroute.o xthick.o xvectu.o \
xcvmtd.o xpspac.o zpenup.o zpendn.o xlbint.o xintsy.o xichar.o \
xhat01.o xgrpup.o xepsc.o xdspac.o xzcntr.o xminit.o xcseta.o \
xcontr.o xcontc.o xcontd.o xclevl.o xrch.o xich.o xchlj.o \
xcharr.o xvlimt.o xcharl.o xfpsc.o xbox.o xaxinc.o xarrow.o \
vwrite.o vline.o vclear.o initcol.o inigra.o zcontm.o zfilln.o \
xqpspc.o xpscrm.o xpntsd.o xmap.o xmpsc.o zdrawc.o zcline.o \
xcurup.o xcsetd.o xcsetc.o xcsetb.o zpltch.o xlpnup.o xcharc.o \
zfillnn.o xzrpsc.o xseamq.o xquadr.o xpenup.o xpendn.o xmrefp.o \
xlinvt.o xchmag.o xleter.o ssslll.o xtpnup.o xcpnup.o xtpndn.o \
xpoint.o xcpndn.o imins.o imaxs.o xchdec.o icleng.o xzpsc.o \
gifcap.o colini.o xrbrok.o xbrokn.o dovron.o xtrans.o xdecod.o xflpsc.o \
xdtpsc.o xdhpsc.o xlcol.o xctran.o xchrst.o xbufps.o xbkpsc.o \
ppenup.o xprojc.o xobset.o xmrset.o xltrny.o xltrnx.o xdrset.o \
xclpsc.o xudraw.o ixvmap.o ymap.o xvmap.o xpdpsc.o xcol.o \
sssooo.o iymap.o xprepn.o xlab.o dskip.o savepc.o saveps.o \
vidpix.o vidras.o xaxnmg.o xchlin.o xchobl.o xcmixl.o xcpsc.o \
xcurdv.o xdhtch.o xppsc.o xprjon.o zjinit.o zlabon.o zpostn.o \
zstct1.o zstct2.o zstcth.o zstctz.o setscreenmode.o egapalette.o vgapalette.o \
rcsdef.o rshade.o gwbox.o gcona.o gconaa.o gconda.o geocol.o \
geowid.o rclass.o gcnr2q.o gcnr2s.o rtxcol.o gconco.o gcnr2v.o \
gvport.o gclopt.o gsmth.o gintp1.o gcursr.o gcurky.o gcoscl.o \
xpsbln.o zsavln.o xnear.o xdist.o dzgcnr.o gifswap.o disfnt.o zpscol.o \
xqplots.o xlinux1.o xlinuxc.o \
	bitblt.o	\
	drwinlne.o	\
	ftable.o	\
	fdv_grx.o	\
	fntable.o	\
	cxtinfo.o	\
	cxtinlne.o	\
	drvinfo.o	\
	drvinlne.o	\
	hooks.o		\
	setdrvr.o	\
	buildaux.o	\
	buildfnt.o	\
	convfont.o	\
	dfltfont.o	\
	drawstrg.o	\
	dumptext.o	\
	fntinlne.o	\
	fontinfo.o	\
	fontpath.o	\
	loadfont.o	\
	propwdt.o	\
	unloadfn.o	\
        xdrwbit.o	\
	vtable.o	\
dev2gif.o egif_lib.o dgif_lib.o gif_hash.o \
gif_err.o quantize.o qprintf.o getarg.o



OTHER_FILES=makefile ../../genie-main/makefile.arc

$(LIBNAME).a: $(OBJECTS) $(OTHER_FILES)
	ar rcv $(LIBNAME).a $(OBJECTS)

.PHONY: clean tags
clean : 
	\rm -rf *.o *.a *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

zvectc.o : zvectc.f
zrcnta.o : zrcnta.f
zcstln.o : zcstln.f
zvecta.o : zvecta.f
zrcntr.o : zrcntr.f
zoutln.o : zoutln.f
zcstfl.o : zcstfl.f
zcstll.o : zcstll.f
zcstch.o : zcstch.f
zgcnta.o : zgcnta.f
dzgcnt.o : dzgcnt.f
zcont1.o : zcont1.f
zcontc.o : zcontc.f
zcontb.o : zcontb.f
zconta.o : zconta.f
zlabof.o : zlabof.f
zgcntr.o : zgcntr.f
xclear.o : xclear.f uniras.inc
xpsbln.o : xpsbln.f vpsc.inc colours.inc
zxyref.o : zxyref.f
zprojc.o : zprojc.f
xrcnta.o : xrcnta.f
zsfplt.o : zsfplt.f
zsf001.o : zsf001.f
zrcntb.o : zrcntb.f
xrcntr.o : xrcntr.f
disfnt.o : disfnt.f
xvects.o : xvects.f
xhatch.o : xhatch.f
xhatcy.o : xhatcy.f
xhatcx.o : xhatcx.f
xgraph.o : xgraph.f
xcurve.o : xcurve.f
xcfont.o : xcfont.f
xcfill.o : xcfill.f
xconts.o : xconts.f
xchlen.o : xchlen.f
xvectk.o : xvectk.f
xclimt.o : xclimt.f
xcaptn.o : xcaptn.f
xbordr.o : xbordr.f
xaxes.o : xaxes.f
xspace.o : xspace.f
xvectr.o : xvectr.f
vidlin.o : vidlin.f cframe.inc uniras.inc
vidini.o : vidini.f cframe.inc colours.inc uniras.inc
vframe.o : vframe.f
dport.o : dport.f colours.inc vpsc.inc mappings.inc
xbpsc.o : xbpsc.f colours.inc vpsc.inc
xdevic.o : xdevic.f f77pc.inc mappings.inc cframe.inc uniras.inc
xzcnta.o : xzcnta.f
xrnumb.o : xrnumb.f
xinumb.o : xinumb.f
xconta.o : xconta.f
xaxsca.o : xaxsca.f
xaxisy.o : xaxisy.f
xaxisx.o : xaxisx.f
dcolor.o : dcolor.f colours.inc mappings.inc
zfud.o : zfud.f
zcontr.o : zcontr.f
zbuf.o : zbuf.f
xsinx.o : xsinx.f
xroute.o : xroute.f
xthick.o : xthick.f
xvectu.o : xvectu.f
xcvmtd.o : xcvmtd.f
xpspac.o : xpspac.f
zpenup.o : zpenup.f
zpendn.o : zpendn.f
xlbint.o : xlbint.f
xintsy.o : xintsy.f
xichar.o : xichar.f
xhat01.o : xhat01.f
xgrpup.o : xgrpup.f
xepsc.o : xepsc.f vpsc.inc
xdspac.o : xdspac.f uniras.inc
xzcntr.o : xzcntr.f
xminit.o : xminit.f colours.inc
xcseta.o : xcseta.f
xcontr.o : xcontr.f
xcontc.o : xcontc.f
xcontd.o : xcontd.f
xclevl.o : xclevl.f
xrch.o : xrch.f
xich.o : xich.f
xchlj.o : xchlj.f
xcharr.o : xcharr.f
xvlimt.o : xvlimt.f
xcharl.o : xcharl.f
xfpsc.o : xfpsc.f vpsc.inc
xbox.o : xbox.f
xaxinc.o : xaxinc.f
xarrow.o : xarrow.f
vwrite.o : vwrite.f colours.inc cframe.inc
vline.o : vline.f cframe.inc
vclear.o : vclear.f cframe.inc
initcol.o : initcol.f
inigra.o : inigra.f colours.inc mappings.inc f77pc.inc
zcontm.o : zcontm.f
zfilln.o : zfilln.f
xqpspc.o : xqpspc.f
xpscrm.o : xpscrm.f vpsc.inc
xpntsd.o : xpntsd.f
xmap.o : xmap.f
xmpsc.o : xmpsc.f vpsc.inc
zdrawc.o : zdrawc.f
zcline.o : zcline.f
xcurup.o : xcurup.f
xcsetd.o : xcsetd.f
xcsetc.o : xcsetc.f
xcsetb.o : xcsetb.f
zpltch.o : zpltch.f
xlpnup.o : xlpnup.f
xcharc.o : xcharc.f
zfillnn.o : zfillnn.f colours.inc mappings.inc uniras.inc
xzrpsc.o : xzrpsc.f colours.inc vpsc.inc mappings.inc
xseamq.o : xseamq.f
xquadr.o : xquadr.f
xpenup.o : xpenup.f
xpendn.o : xpendn.f
xmrefp.o : xmrefp.f
xlinvt.o : xlinvt.f
xchmag.o : xchmag.f
xleter.o : xleter.f
ssslll.o : ssslll.f mappings.inc
xtpnup.o : xtpnup.f
xcpnup.o : xcpnup.f
xtpndn.o : xtpndn.f
xpoint.o : xpoint.f
xcpndn.o : xcpndn.f
imins.o : imins.f
imaxs.o : imaxs.f
xchdec.o : xchdec.f
icleng.o : icleng.f
xzpsc.o : xzpsc.f colours.inc vpsc.inc mappings.inc
colini.o : colini.f colours.inc mappings.inc
xrbrok.o : xrbrok.f
xbrokn.o : xbrokn.f
dovron.o : dovron.f uniras.inc mappings.inc
xtrans.o : xtrans.f
xdecod.o : xdecod.f
xflpsc.o : xflpsc.f vpsc.inc
xdtpsc.o : xdtpsc.f vpsc.inc
xdhpsc.o : xdhpsc.f vpsc.inc
xlcol.o : xlcol.f mappings.inc colours.inc f77pc.inc uniras.inc
xctran.o : xctran.f
xchrst.o : xchrst.f
xbufps.o : xbufps.f vpsc.inc
xbkpsc.o : xbkpsc.f vpsc.inc
ppenup.o : ppenup.f mappings.inc uniras.inc
xprojc.o : xprojc.f
xobset.o : xobset.f
xmrset.o : xmrset.f
xltrny.o : xltrny.f
xltrnx.o : xltrnx.f
xdrset.o : xdrset.f
xclpsc.o : xclpsc.f vpsc.inc
xudraw.o : xudraw.f mappings.inc f77pc.inc
ixvmap.o : ixvmap.f mappings.inc
ymap.o : ymap.f mappings.inc
xvmap.o : xvmap.f mappings.inc
xpdpsc.o : xpdpsc.f vpsc.inc
xcol.o : xcol.f
sssooo.o : sssooo.f mappings.inc
iymap.o : iymap.f mappings.inc
xprepn.o : xprepn.f
xlab.o : xlab.f
dskip.o : dskip.f mappings.inc
savepc.o : savepc.f mappings.inc
saveps.o : saveps.f mappings.inc
vidpix.o : vidpix.f cframe.inc
vidras.o : vidras.f uniras.inc cframe.inc
xaxnmg.o : xaxnmg.f
xchlin.o : xchlin.f
xchobl.o : xchobl.f
xcmixl.o : xcmixl.f
xcpsc.o : xcpsc.f
xcurdv.o : xcurdv.f
xdhtch.o : xdhtch.f
xppsc.o : xppsc.f
xprjon.o : xprjon.f
zjinit.o : zjinit.f
zlabon.o : zlabon.f
zpostn.o : zpostn.f
zstct1.o : zstct1.f
zstct2.o : zstct2.f
zstcth.o : zstcth.f
zstctz.o : zstctz.f
setscreenmode.o : setscreenmode.f
egapalette.o : egapalette.f
vgapalette.o : vgapalette.f
rcsdef.o : rcsdef.f
rshade.o : rshade.f
gwbox.o : gwbox.f
gcona.o : gcona.f
gconaa.o : gconaa.f
gconda.o : gconda.f
geocol.o : geocol.f
geowid.o : geowid.f
rclass.o : rclass.f
gcnr2q.o : gcnr2q.f
gcnr2s.o : gcnr2s.f
rtxcol.o : rtxcol.f
gconco.o : gconco.f
gcnr2v.o : gcnr2v.f
gvport.o : gvport.f
gclopt.o : gclopt.f
gsmth.o : gsmth.f
gintp1.o : gintp1.f
gcursr.o : gcursr.f
gcurky.o : gcurky.f
gcoscl.o : gcoscl.f
zsavln.o : zsavln.f
xnear.o : xnear.f
xdist.o : xdist.f
dzgcnr.o : dzgcnr.f
gifswap.o : gifswap.f
zpscol.o : zpscol.f

xqplots.o : xqplots.c qplot.bit grx20.h
	$(CC) -c $(CCFLAGS) $(LOCFLAGS) -DSTANDALONE -o xqplots.o xqplots.c
xlinux1.o : xlinux1.f
xlinuxc.o : xlinuxc.c getarg.h gif_lib.h

bitblt.o: bitblt.c libgrx.h grx20.h \
  clipping.h arith.h
drwinlne.o: drwinlne.c libgrx.h grx20.h
ftable.o: ftable.c grdriver.h \
  grx20.h libgrx.h
fdv_grx.o: fdv_grx.c grfontdv.h \
  grx20.h libgrx.h allocate.h \
  fdv_grx.h
fntable.o: fntable.c grfontdv.h \
  grx20.h libgrx.h
cxtinfo.o: cxtinfo.c libgrx.h grx20.h 
cxtinlne.o: cxtinlne.c libgrx.h grx20.h
drvinfo.o: drvinfo.c grdriver.h grx20.h \
  libgrx.h
drvinlne.o: drvinlne.c libgrx.h grx20.h
hooks.o: hooks.c libgrx.h grx20.h
setdrvr.o: setdrvr.c grdriver.h grx20.h \
  libgrx.h arith.h memfill.h \
  mempeek.h memcopy.h
buildaux.o: buildaux.c libgrx.h grx20.h \
  allocate.h arith.h memfill.h \
  mempeek.h memcopy.h
buildfnt.o: buildfnt.c grfontdv.h grx20.h \
  libgrx.h allocate.h alloca.h \
  arith.h memfill.h \
  mempeek.h memcopy.h
convfont.o: convfont.c grfontdv.h grx20.h \
  libgrx.h arith.h memcopy.h 
dfltfont.o: dfltfont.c libgrx.h grx20.h
drawstrg.o: drawstrg.c libgrx.h grx20.h \
  clipping.h arith.h
dumptext.o: dumptext.c libgrx.h grx20.h \
  arith.h
fntinlne.o: fntinlne.c libgrx.h grx20.h
fontinfo.o: fontinfo.c grfontdv.h grx20.h \
  libgrx.h
fontpath.o: fontpath.c grfontdv.h grx20.h \
  libgrx.h allocate.h alloca.h \
  arith.h
loadfont.o: loadfont.c grfontdv.h grx20.h \
  libgrx.h
propwdt.o: propwdt.c libgrx.h grx20.h
unloadfn.o: unloadfn.c libgrx.h grx20.h \
  allocate.h arith.h
xdrwbit.o: xdrwbit.c grx20.h libgrx.h
vtable.o: vtable.c grdriver.h \
  grx20.h libgrx.h

dev2gif.o   : dev2gif.c gif_lib.h
egif_lib.o  : egif_lib.c gif_lib.h gif_hash.h
dgif_lib.o  : dgif_lib.c gif_lib.h gif_hash.h
gif_hash.o  : gif_hash.c gif_lib.h gif_hash.h
gif_err.o   : gif_err.c gif_lib.h
quantize.o  : quantize.c gif_lib.h
qprintf.o   : qprintf.c gif_lib.h
getarg.o    : getarg.c getarg.h
