.SUFFIXES: .o .f .f90


.F.o:
	$(FC) $(FLAGS) $(DOPTS) -c $*.F
.f.o:
	$(FC) $(FLAGS) -c $*.f
.f:
	$(FC) $(FLAGS) -o $@ $<
.f90.o:
	$(FC) $(FLAGS) -c $*.f90
.f90:
	$(FC) $(FLAGS) -o $@ $<

CG = ../genie-cgoldstein/
BGM = ../genie-biogem/src/fortran/
SGM = ../genie-sedgem/
GLT = ../genie-gemlite/
LND = ../genie-land/
SLD = ../genie-simpleland/
ACM = ../genie-atchem/
GMN = ../genie-main/



OBJS =	$(CG)co.o $(CG)diag.o $(CG)diag2.o $(CG)diagend.o $(CG)diagopsi.o \
	$(CG)diagosc.o $(CG)drgset.o $(CG)inm.o $(CG)invert.o $(CG)island.o \
	$(CG)mains.o $(CG)outm.o $(CG)jbar.o $(CG)ubarsolv.o $(CG)velc.o \
	$(CG)wind.o $(CG)surflux.o $(CG)diaga.o $(CG)diag3.o $(CG)gseto.o \
	$(CG)gseta.o $(CG)tstepa.o $(CG)tstepo.o $(CG)tstipa.o $(CG)tstipo.o \
	$(CG)tstepsic.o $(CG)readroff.o $(CG)radfor.o

OBJS_CB = $(OBJS) $(GMN)gem_cmn.o $(GMN)gem_util.o $(GMN)gem_carbchem.o \
	$(ACM)atchem_lib.o $(ACM)atchem_box.o \
	$(ACM)atchem_data.o $(ACM)atchem_main.o \
	$(GMN)cpl_flux_atchem.o $(GMN)cpl_comp_atchem.o \
	$(BGM)biogem_lib.o $(BGM)biogem_box.o \
	$(BGM)biogem_data.o $(BGM)biogem_main.o

OBJS_CM = $(OBJS) $(SLD)annav_diags.o $(SLD)carbon.o \
	$(SLD)carbt_diags.o $(SLD)field_interp.o $(SLD)in_ents.o \
	$(SLD)ocean_alb.o $(SLD)out_ents.o \
	$(SLD)physt_diags.o $(SLD)screen_diags.o $(SLD)sealevel.o \
	$(SLD)setup_emissions.o $(SLD)setup_ents.o $(SLD)greenland_melt.o

OBJS_CBM = $(OBJS_CB) $(SLD)annav_diags.o $(SLD)carbon.o \
	$(SLD)carbt_diags.o $(SLD)field_interp.o $(SLD)in_ents.o \
	$(SLD)ocean_alb.o $(SLD)out_ents.o \
	$(SLD)physt_diags.o $(SLD)screen_diags.o $(SLD)sealevel.o \
	$(SLD)setup_emissions.o $(SLD)setup_ents.o $(SLD)greenland_melt.o

OBJS_CBS = $(OBJS_CB) $(SGM)sedgem_lib.o $(SGM)sedgem_box.o \
	$(SGM)sedgem_data.o $(SGM)sedgem_main.o \
	$(GMN)cpl_flux_sedgem.o $(GMN)cpl_comp_sedgem.o

OBJS_CBST = $(OBJS_CBS) $(GLT)gemlite_lib.o $(GLT)gemlite_box.o \
	$(GLT)gemlite_data.o $(GLT)gemlite_main.o

OBJS_L = $(LND)land_coupler.o $(LND)canopy.o $(LND)growth.o \
	$(LND)leaf_c4.o $(LND)lotka.o $(LND)pft_sparm.o $(LND)raero.o \
	$(LND)swrad.o $(LND)compete.o $(LND)land.o $(LND)leaf_c3.o \
	$(LND)microbe.o $(LND)phenol.o $(LND)sf_stom.o $(LND)triffid.o \
	$(LND)decay.o $(LND)land_state.o $(LND)leaf_lit.o $(LND)penmon.o \
	$(LND)qsat.o $(LND)soilcarb.o $(LND)vegcarb.o $(LND)inml.o \
	$(LND)outml.o $(LND)diagend_land.o $(LND)gsetl.o $(LND)diag_land.o

OBJS_CBL = $(OBJS_CB) $(OBJS_L)

OBJS_CBSTL = $(OBJS_CBST) $(OBJS_L)

OBJS_CBSL = $(OBJS_CBS) $(OBJS_L)

OBJS_CL = $(OBJS) $(OBJS_L)

FBJS =	$(CG)co.f $(CG)diag.f $(CG)diag2.f $(CG)diagend.F $(CG)diagopsi.f \
	$(CG)diagosc.F $(CG)drgset.f $(CG)inm.f $(CG)invert.f $(CG)island.f \
	$(CG)mains.F $(CG)outm.f $(CG)jbar.f $(CG)ubarsolv.f $(CG)velc.f \
	$(CG)wind.f $(CG)surflux.F $(CG)diaga.f $(CG)diag3.f $(CG)gseto.F \
	$(CG)gseta.F $(CG)tstepa.f $(CG)tstepo.F $(CG)tstipa.f $(CG)tstipo.F \
	$(CG)tstepsic.f $(CG)readroff.f $(CG)radfor.F

FBJS_CB = $(FBJS) $(GMN)gem_cmn.f90 $(GMN)gem_util.f90 $(GMN)gem_carbchem.f90 \
	$(ACM)atchem_lib.f90 $(ACM)atchem_box.f90 \
	$(ACM)atchem_data.f90 $(ACM)atchem_main.f90 \
	$(GMN)cpl_flux_atchem.f90 $(GMN)cpl_comp_atchem.f90 \
	$(BGM)biogem_lib.f90 $(BGM)biogem_box.f90 \
	$(BGM)biogem_data.f90 $(BGM)biogem_main.f90

FBJS_CM = $(FBJS) $(SLD)annav_diags.F $(SLD)carbon.F \
	$(SLD)carbt_diags.f $(SLD)field_interp.f $(SLD)in_ents.f \
	$(SLD)ocean_alb.f $(SLD)out_ents.f \
	$(SLD)physt_diags.f $(SLD)screen_diags.f $(SLD)sealevel.F \
	$(SLD)setup_emissions.f $(SLD)setup_ents.F $(SLD)greenland_melt.F

FBJS_CBM = $(FBJS_CB) $(SLD)annav_diags.F $(SLD)carbon.F \
	$(SLD)carbt_diags.f $(SLD)field_interp.f $(SLD)in_ents.f \
	$(SLD)ocean_alb.f $(SLD)out_ents.f \
	$(SLD)physt_diags.f $(SLD)screen_diags.f $(SLD)sealevel.F \
	$(SLD)setup_emissions.f $(SLD)setup_ents.F $(SLD)greenland_melt.F

FBJS_CBS = $(FBJS_CB) $(SGM)sedgem_lib.f90  $(SGM)sedgem_box.f90 \
	$(SGM)sedgem_data.f90 $(SGM)sedgem_main.f90 \
	$(GMN)cpl_flux_sedgem.f90 $(GMN)cpl_comp_sedgem.f90

FBJS_CBST = $(FBJS_CBS) $(GLT)gemlite_lib.f90 $(GLT)gemlite_box.f90 \
	$(GLT)gemlite_data.f90 $(GLT)gemlite_main.f90

FBJS_L = $(LND)land_coupler.F $(LND)canopy.f $(LND)growth.f \
	$(LND)leaf_c4.f $(LND)lotka.f $(LND)pft_sparm.f $(LND)raero.f \
	$(LND)swrad.f $(LND)compete.f $(LND)land.f $(LND)leaf_c3.f \
	$(LND)microbe.f $(LND)phenol.f $(LND)sf_stom.f $(LND)triffid.f \
	$(LND)decay.f $(LND)land_state.f $(LND)leaf_lit.f $(LND)penmon.f \
	$(LND)qsat.f $(LND)soilcarb.f $(LND)vegcarb.f $(LND)inml.f \
	$(LND)outml.f $(LND)diagend_land.f $(LND)gsetl.f $(LND)diag_land.f

FBJS_CBL = $(FBJS_CB) $(FBJS_L)

FBJS_CBSTL = $(FBJS_CBST) $(FBJS_L)

FBJS_CBSL = $(FBJS_CBS) $(FBJS_L) 

FBJS_CL = $(FBJS) $(FBJS_L)

## DOPTS = -Ddisle2 (2 islands) -Ddimpo (implicit ocean timestep) -Ddiso (isoneutral diffusion) -Ddimpa (implicit atm. timestep) -Dbiogem (ocean biogeochemisty) -Dsedgem (sediment biogeochemistry) -Dgemlite (geochemistry lite) -Ddosc (seasonal insolation) -Datchem (atmospheric chemistry)

#### fortran compiler command name
#FC = f90
#FC = ifort
FC = pgf90


#### f90 COMPILER FLAGS
FLAGS = -r8 -O
#FLAGS = -xtypemap=real:64 -O
#FLAGS = -Ofast=ip27 -r8
#FLAGS = +FPVZOuiD +T +O3 +autodblpad
#FLAGS = -real_size 64 -O
#FLAGS = +FPVZOuiD +T -g

#### lf95 COMPILER FLAGS
#FLAGS = --ap --chk --chkglobal -g --lst --pca --sav --info --xref --maxfatals 5 --dbl --f95 --tpp
#FLAGS = --ap -g --lst --pca --sav --info --xref --maxfatals 5 --dbl --f95 --tpp
#FLAGS = --dbl -O --ntrace --tpp


DOPTS = -Ddimpa -Ddiso -Ddosc
DOPTS_CB = $(DOPTS) -Dbiogem -Datchem
DOPTS_CMV = $(DOPTS) -Dents -Dfixedveg
DOPTS_CMVD = $(DOPTS) -Dents -Dfixedveg -Dhfoutput
DOPTS_CM = $(DOPTS) -Dents
DOPTS_CMD = $(DOPTS) -Dents -Dhfoutput
DOPTS_CMI = $(DOPTS) -Dents -Dicemelt
DOPTS_CBM = $(DOPTS_CB) -Dents
DOPTS_CBMI = $(DOPTS_CB) -Dents -Dicemelt
DOPTS_CBS = $(DOPTS_CB) -Dsedgem
DOPTS_CBST = $(DOPTS_CBS) -Dgemlite
DOPTS_CL = $(DOPTS) -Ddland
DOPTS_CBL = $(DOPTS_CB) -Ddland
DOPTS_CBSL = $(DOPTS_CBS) -Ddland
DOPTS_CBSTL = $(DOPTS_CBST) -Ddland

CMN = var.cmn Makefile


goldstein: $(OBJS) $(CMN)
	$(FC) $(FLAGS) $(OBJS) -o goldstein

cb_goldstein: $(FBJS_CB) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CB) $(FBJS_CB) -o cb_goldstein
cm_goldstein: $(FBJS_CM) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CM) $(FBJS_CM) -o cm_goldstein
cmd_goldstein:  $(FBJS_CM) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CMD) $(FBJS_CM) -o cmd_goldstein
cmv_goldstein: $(FBJS_CM) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CMV) $(FBJS_CM) -o cmv_goldstein
cmvd_goldstein: $(FBJS_CM) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CMVD) $(FBJS_CM) -o cmvd_goldstein
cmi_goldstein: $(FBJS_CM) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CMI) $(FBJS_CM) -o cmi_goldstein
cbm_goldstein: $(FBJS_CBM) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CBM) $(FBJS_CBM) -o cbm_goldstein
cbmi_goldstein: $(FBJS_CBM) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CBMI) $(FBJS_CBM) -o cbmi_goldstein
cbs_goldstein: $(FBJS_CBS) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CBS) $(FBJS_CBS) -o cbs_goldstein
cbst_goldstein: $(FBJS_CBST) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CBST) $(FBJS_CBST) -o cbst_goldstein
cl_goldstein: $(FBJS_CL) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CL) $(FBJS_CL) -o cl_goldstein
cbl_goldstein: $(FBJS_CBL) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CBL) $(FBJS_CBL) -o cbl_goldstein
cbsl_goldstein: $(FBJS_CBSL) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CBSL) $(FBJS_CBSL) -o cbsl_goldstein
cbstl_goldstein: $(FBJS_CBSTL) $(CMN)
	$(FC) $(FLAGS) $(DOPTS_CBSTL) $(FBJS_CBSTL) -o cbstl_goldstein


clean:
	rm -f *.o *.mod *.f~ *.F~ *.f90~ *goldstein

# dependencies

$(CG)co.o: $(CG)co.f $(CMN)
$(CG)diag.o: $(CG)diag.f $(CMN)
$(CG)diag2.o: $(CG)diag2.f $(CMN)
$(CG)diag3.o: $(CG)diag3.f $(CMN)
$(CG)diaga.o: $(CG)diaga.f $(CMN)
$(CG)diagend.o: $(CG)diagend.F $(CMN)
$(CG)diagopsi.o: $(CG)diagopsi.f $(CMN)
$(CG)diagosc.o: $(CG)diagosc.F $(CMN)
$(CG)drgset.o: $(CG)drgset.f $(CMN)
$(CG)inm.o: $(CG)inm.f $(CMN)
$(CG)invert.o: $(CG)invert.f $(CMN)
$(CG)island.o: $(CG)island.f $(CMN)
$(CG)mains.o: $(CG)mains.F $(CMN)
$(CG)outm.o: $(CG)outm.f $(CMN)
$(CG)jbar.o: $(CG)jbar.f $(CMN)
$(CG)ubarsolv.o: $(CG)ubarsolv.f $(CMN)
$(CG)velc.o: $(CG)velc.f $(CMN)
$(CG)wind.o: $(CG)wind.f $(CMN)
$(CG)surflux.o: $(CG)surflux.F $(CMN)
$(CG)gseto.o: $(CG)gseto.F $(CMN)
$(CG)gseta.o: $(CG)gseta.F $(CMN)
$(CG)tstepa.o: $(CG)tstepa.f $(CMN)
$(CG)tstepo.o: $(CG)tstepo.F $(CMN)
$(CG)tstipa.o: $(CG)tstipa.f $(CMN)
$(CG)tstipo.o: $(CG)tstipo.F $(CMN)
$(CG)tstepsic.o: $(CG)tstepsic.f $(CMN)
$(CG)readroff.o: $(CG)readroff.f $(CMN)
$(CG)radfor.o: $(CG)radfor.F $(CMN)
