#
# $Id: makefile 6300 2011-03-30 11:18:41Z gethin-williams $
#
# For the number of levels:
# (this same technique should really be used for the 
#   precision arguments as well, rather than cluttering makefile.arc)
#
ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(IGCMATMOSPREC) $(BOUNDS_FLAGS) \
            $(FPPFLAGS) $(DOPTS) $(LOC_INC_FLAGS)
#
#
ROOT_DIR = ../../../genie-main

INCLUDE_FILES = balan.cmn bats.cmn blank.cmn comfft.cmn comgrm.cmn \
		cpiers.cmn gridp2.cmn gridp3.cmn gridp.cmn gridpp.cmn \
		gridss.cmn climatologies.cmn \
		files.cmn legau.cmn means.cmn netdata.cmn \
		orog_g.cmn outcon.cmn param1.cmn param2.cmn parray.cmn \
		physca.cmn polyno.cmn ptendz.cmn qstabs.cmn \
		radht.cmn radlw.cmn restij.cmn restor.cmn spectr.cmn \
		stats.cmn xtrap.cmn fluxes.cmn igcm_prec.cmn igcm_rough.cmn \
		igcm_flux.cmn igcm_nlevs.cmn igcm_vdiff.cmn \
		igcm_cloud.cmn igcm_radn.cmn gridpf.cmn extra.cmn \
		igcm_orbit.cmn precision.inc tendencies.cmn rough.cmn \
                butterfly.cmn varalb.cmn highres.cmn flux_adjust.cmn


OTHER_FILES=../../../genie-main/makefile.arc makefile

OBJS1 = abort.$(OBJ_EXT) accum_means.$(OBJ_EXT) end_atmos.$(OBJ_EXT) balanc.$(OBJ_EXT) \
	calndr_igcm.$(OBJ_EXT) cbadj.$(OBJ_EXT) cbcon.$(OBJ_EXT) cldtran.$(OBJ_EXT) colamt.$(OBJ_EXT) consearch.$(OBJ_EXT) \
	contour_graphics.$(OBJ_EXT) convec.$(OBJ_EXT) convec_tiedtke.$(OBJ_EXT) cubm.$(OBJ_EXT) \
	cudif.$(OBJ_EXT) danalv.$(OBJ_EXT) dedd.$(OBJ_EXT) dgrmlt.$(OBJ_EXT) difuse.$(OBJ_EXT) dlsgcr.$(OBJ_EXT) \
	drawbox.$(OBJ_EXT) dryadj.$(OBJ_EXT) dstep.$(OBJ_EXT) end_netcdf.$(OBJ_EXT) \
	energy.$(OBJ_EXT) finish_means.$(OBJ_EXT) \
	gascnt.$(OBJ_EXT) gassearch.$(OBJ_EXT) h2oflux.$(OBJ_EXT) haltran.$(OBJ_EXT) \
	hanal1.$(OBJ_EXT) hanal.$(OBJ_EXT) hanalv.$(OBJ_EXT) hexp1.$(OBJ_EXT) hexp.$(OBJ_EXT) hexpv.$(OBJ_EXT) ictrac.$(OBJ_EXT) \
	inibal.$(OBJ_EXT) ini_checks.$(OBJ_EXT) \
	inigau.$(OBJ_EXT) inimeans.$(OBJ_EXT) ini_netcdf.$(OBJ_EXT) \
	iniphys.$(OBJ_EXT) iniqs.$(OBJ_EXT) inires.$(OBJ_EXT) iniresij.$(OBJ_EXT) iniset.$(OBJ_EXT) inisi.$(OBJ_EXT) \
	inisp.$(OBJ_EXT) inistr.$(OBJ_EXT) inisurf.$(OBJ_EXT) \
	interpf.$(OBJ_EXT) irrad.$(OBJ_EXT) \
	lgndre.$(OBJ_EXT) linsearch.$(OBJ_EXT) lscrn.$(OBJ_EXT) ltddia.$(OBJ_EXT) ltd.$(OBJ_EXT) ltidt.$(OBJ_EXT) lti.$(OBJ_EXT) \
	mascor.$(OBJ_EXT) matinv.$(OBJ_EXT) \
	maxmin.$(OBJ_EXT) mgrmlt.$(OBJ_EXT) netout2.$(OBJ_EXT) nikosrad.$(OBJ_EXT) noise.$(OBJ_EXT) o3interp.$(OBJ_EXT) \
	planck.$(OBJ_EXT) plotfields2.$(OBJ_EXT) plotfields.$(OBJ_EXT) pqsat.$(OBJ_EXT) pvcr.$(OBJ_EXT) radn.$(OBJ_EXT) \
	radsw.$(OBJ_EXT) setcv.$(OBJ_EXT) setres.$(OBJ_EXT) \
	settee.$(OBJ_EXT) setup_nc.$(OBJ_EXT) setzt.$(OBJ_EXT) sfct.$(OBJ_EXT) sigma2.$(OBJ_EXT) smflux.$(OBJ_EXT) solang.$(OBJ_EXT) \
	spdel2.$(OBJ_EXT) surfm.$(OBJ_EXT) svp.$(OBJ_EXT) sw.$(OBJ_EXT) swtt.$(OBJ_EXT) tbal.$(OBJ_EXT) tstep.$(OBJ_EXT) \
	vdiff.$(OBJ_EXT) netout1.$(OBJ_EXT) intpr.$(OBJ_EXT) inter.$(OBJ_EXT) \
	write_netcdf_std.$(OBJ_EXT) write_netcdf_igcm.$(OBJ_EXT) \
	phy_convmf.$(OBJ_EXT) convert_gr_sp_orog.$(OBJ_EXT) \
        inispec.$(OBJ_EXT) iniset_lib.$(OBJ_EXT) inifft.$(OBJ_EXT) inigau_lib.$(OBJ_EXT) inigs.$(OBJ_EXT) \
        rffti.$(OBJ_EXT) lgndre_lib.$(OBJ_EXT) fft.$(OBJ_EXT) eim.$(OBJ_EXT) hexp_lib.$(OBJ_EXT) hanal_lib.$(OBJ_EXT) \
        rffti1.$(OBJ_EXT) rfftb.$(OBJ_EXT) rfftf.$(OBJ_EXT) rfftf1.$(OBJ_EXT) rfftb1.$(OBJ_EXT) \
        radbg.$(OBJ_EXT) radb5.$(OBJ_EXT) radb4.$(OBJ_EXT) radb3.$(OBJ_EXT) radb2.$(OBJ_EXT) \
        radfg.$(OBJ_EXT) radf5.$(OBJ_EXT) radf4.$(OBJ_EXT) radf3.$(OBJ_EXT) radf2.$(OBJ_EXT) \
        gwtlt1_lib.$(OBJ_EXT) \
        filter.$(OBJ_EXT) myfilt.$(OBJ_EXT) \
        convert_free_blayer.$(OBJ_EXT) \
        restructure.$(OBJ_EXT) writenc_restart.$(OBJ_EXT) writenc_histg1.$(OBJ_EXT) writenc_hists.$(OBJ_EXT) writenc_restor.$(OBJ_EXT)  

OBJS2 = igcm3_adiab.$(OBJ_EXT) \
	igcm3_diab.$(OBJ_EXT)

OBJS3 = initialise_igcmsurf.$(OBJ_EXT) \
	igcm_land_blayer.$(OBJ_EXT) \
	igcm_ocean_blayer.$(OBJ_EXT) \
	write_landsurf_restart.$(OBJ_EXT) \
	write_oceansurf_restart.$(OBJ_EXT) \
	igcm_land_surflux.$(OBJ_EXT) \
	igcm_ocean_surflux.$(OBJ_EXT) \
	igcm_landsurface_1.$(OBJ_EXT) \
	igcm_surflux.$(OBJ_EXT) \
	igcm_landsurface_2.$(OBJ_EXT) \
	pqsat_prec.$(OBJ_EXT) \
	igcm_blayer_new.$(OBJ_EXT) \
	igcm_land_runoff.$(OBJ_EXT)

OBJS4 = initialise_atmos.$(OBJ_EXT)

OBJECTS = $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4)

all: $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: all clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.dep *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

# Pattern rules to make object files
# One rule for each compilation permutation
$(OBJS1): %.$(OBJ_EXT): %.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $< $(OBJ_FLAG)$@

$(OBJS2): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(DOPTS) $(GENIEPRECISIONOPTS) \
	$< $(OBJ_FLAG)$@

$(OBJS3): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(GENIEPRECISIONOPTS) $< $(OBJ_FLAG)$@

$(OBJS4): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(GENIEPRECISIONOPTS) $(LIBUTIL1PRECISIONOPTS) \
	$< $(OBJ_FLAG)$@

# Rules to create '.dep' files from sources
# We want make to halt if $(PYTHON) is not found
# A failing 'which' command coupled with a -e shell invocation
# provides the trigger to halt the compilation. 
MAKE_DEPS = @ $(SHELL) -ec 'which $(PYTHON) > /dev/null; $(PYTHON) $(ROOT_DIR)/finc.py $< | sed '\''s/$*\.$(OBJ_EXT)/& $@/g'\'' > $@'

%.dep: %.f
	$(MAKE_DEPS)

%.dep: %.F
	$(MAKE_DEPS)

# Include the '.dep' files
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),tags)
-include $(OBJECTS:.$(OBJ_EXT)=.dep)
endif
endif
