#
# $Id: makefile 4197 2008-05-13 17:05:25Z gethin-williams $
#
LOC_INC_FLAGS=$(MOD_INC_FLAG)../../../genie-main
LOCALFLAGS=$(FFLAGS) $(LANDPRECOPT) $(BOUNDS_FLAGS) $(LOC_INC_FLAGS)
##
.SUFFIXES : .$(OBJ_EXT) .f .F .f90
##
THISFL=./makefile
##
OBJECTS=phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) \
	land_diags.$(OBJ_EXT) land_netcdf.$(OBJ_EXT) land_restart.$(OBJ_EXT) \
	land_runoff.$(OBJ_EXT) land_accounting.$(OBJ_EXT) genie_land_surflux.$(OBJ_EXT) \
	calc_z0_gbm.$(OBJ_EXT) calc_ra_tile.$(OBJ_EXT) calc_daylength.$(OBJ_EXT) \
	calc_albedos.$(OBJ_EXT) canopy.$(OBJ_EXT) compete.$(OBJ_EXT) decay.$(OBJ_EXT) \
	growth.$(OBJ_EXT) tstep_land.$(OBJ_EXT) tstep_triffid.$(OBJ_EXT) \
	tstep_phenology.$(OBJ_EXT) land_state.$(OBJ_EXT) leaf_c3.$(OBJ_EXT) \
	leaf_c4.$(OBJ_EXT) leaf_lit.$(OBJ_EXT) lotka.$(OBJ_EXT) microbe.$(OBJ_EXT) \
	energy_balance.$(OBJ_EXT) pft_sparm.$(OBJ_EXT) phenol.$(OBJ_EXT) qsat.$(OBJ_EXT) \
	raero.$(OBJ_EXT) sf_stom.$(OBJ_EXT) soilcarb.$(OBJ_EXT) triffid.$(OBJ_EXT) \
	vegcarb.$(OBJ_EXT) initialise_land.$(OBJ_EXT) end_land.$(OBJ_EXT) \
	cancap8a.$(OBJ_EXT) land_embm_combine.$(OBJ_EXT) land_general_utils.$(OBJ_EXT) \
	update_landice.$(OBJ_EXT)

INCLUDE_FILES=
MODMODS=phys_const.$(MODEXT) land_const.$(MODEXT) land_var.$(MODEXT) \
        land_diags.$(MODEXT) land_netcdf.$(MODEXT) land_restart.$(MODEXT) \
        land_accounting.$(MODEXT)

all : $(OBJECTS) $(THISFL) $(INCLUDE_FILES)

.PHONY : clean tags
clean:
	\rm -f *.$(OBJ_EXT) *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

# GENIE-land dependencies
phys_const.$(OBJ_EXT) : phys_const.f90 $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) phys_const.f90
land_const.$(OBJ_EXT) : land_const.f90 $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_const.f90
land_var.$(OBJ_EXT) : land_var.f90 land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_var.f90
land_diags.$(OBJ_EXT) : land_diags.f90 phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_diags.f90
land_netcdf.$(OBJ_EXT) : land_netcdf.f90 land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) land_diags.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_netcdf.f90
land_restart.$(OBJ_EXT) : land_restart.f90 land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) land_diags.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_restart.f90
land_runoff.$(OBJ_EXT) : land_runoff.f90 land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL)  
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_runoff.f90
land_accounting.$(OBJ_EXT) : land_accounting.f90 phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL)  
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_accounting.f90
genie_land_surflux.$(OBJ_EXT) : genie_land_surflux.f90 phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) land_diags.$(OBJ_EXT) land_netcdf.$(OBJ_EXT) land_restart.$(OBJ_EXT) land_accounting.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) genie_land_surflux.f90
calc_z0_gbm.$(OBJ_EXT) : calc_z0_gbm.f90 land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL)  
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) calc_z0_gbm.f90
calc_ra_tile.$(OBJ_EXT) : calc_ra_tile.f90 phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL)  
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) calc_ra_tile.f90
calc_daylength.$(OBJ_EXT) : calc_daylength.f90 phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT)  $(THISFL)  
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) calc_daylength.f90
calc_albedos.$(OBJ_EXT) : calc_albedos.f90 phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) $(THISFL)  
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) calc_albedos.f90
canopy.$(OBJ_EXT) : canopy.f phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) canopy.f
compete.$(OBJ_EXT) : compete.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) compete.f
decay.$(OBJ_EXT) : decay.f  land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) decay.f
growth.$(OBJ_EXT) : growth.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) growth.f
tstep_land.$(OBJ_EXT) : tstep_land.f phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) tstep_land.f
tstep_triffid.$(OBJ_EXT) : tstep_triffid.f90 land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) tstep_triffid.f90
tstep_phenology.$(OBJ_EXT) : tstep_phenology.f90 land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) tstep_phenology.f90
land_state.$(OBJ_EXT) : land_state.f90 phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_state.f90
leaf_c3.$(OBJ_EXT) : leaf_c3.f phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) leaf_c3.f
leaf_c4.$(OBJ_EXT) : leaf_c4.f phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) leaf_c4.f
leaf_lit.$(OBJ_EXT) : leaf_lit.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) leaf_lit.f
lotka.$(OBJ_EXT) : lotka.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) lotka.f 
microbe.$(OBJ_EXT) : microbe.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) microbe.f
energy_balance.$(OBJ_EXT) : energy_balance.f phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) energy_balance.f
pft_sparm.$(OBJ_EXT) : pft_sparm.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) pft_sparm.f
phenol.$(OBJ_EXT) : phenol.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) phenol.f
qsat.$(OBJ_EXT) : qsat.f phys_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) qsat.f
raero.$(OBJ_EXT) : raero.f $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) raero.f
sf_stom.$(OBJ_EXT) : sf_stom.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) sf_stom.f
soilcarb.$(OBJ_EXT) : soilcarb.f $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) soilcarb.f
triffid.$(OBJ_EXT) : triffid.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) triffid.f
vegcarb.$(OBJ_EXT) : vegcarb.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) vegcarb.f
initialise_land.$(OBJ_EXT) : initialise_land.f90 phys_const.$(OBJ_EXT) land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) land_diags.$(OBJ_EXT) land_netcdf.$(OBJ_EXT) land_restart.$(OBJ_EXT) land_runoff.$(OBJ_EXT) land_accounting.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) initialise_land.f90
end_land.$(OBJ_EXT) : end_land.f90 $(THISFL)  
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) end_land.f90
cancap8a.$(OBJ_EXT) : cancap8a.f land_const.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) cancap8a.f
land_embm_combine.$(OBJ_EXT) : land_embm_combine.f90 land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_embm_combine.f90
land_general_utils.$(OBJ_EXT) : land_general_utils.f90 $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) land_general_utils.f90
update_landice.$(OBJ_EXT) : update_landice.f90 land_const.$(OBJ_EXT) land_var.$(OBJ_EXT) $(THISFL) 
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) update_landice.f90
