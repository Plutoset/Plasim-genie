
#
# Preprocess input datasets which have previously been downloaded to
# ../download/ to facilitate the use of the datasets by the model
#
# This makefile requires NetCDF Operators (NCO;
# http://nco.sourceforge.net) to be installed on the system.
#

# NCO commands
NCKS=ncks
NCWA=ncwa
NCAP2=ncap2
NCECAT=ncecat
NCRENAME=ncrename

PREPROCESSED_ANNUAL_DATASETS_NCEP-DOE_R2_surface=NCEP-DOE_Reanalysis_2_ltaa_wind_stress.nc
PREPROCESSED_ANNUAL_DATASETS_NCEP-DOE_R2_1000mb=NCEP-DOE_Reanalysis_2_ltaa_1000mb_wind_speed.nc
PREPROCESSED_MONTHLY_DATASETS_NCEP-DOE_R2_surface=NCEP-DOE_Reanalysis_2_ltma_wind_stress.nc
PREPROCESSED_MONTHLY_DATASETS_NCEP-DOE_R2_1000mb=NCEP-DOE_Reanalysis_2_ltma_1000mb_wind_speed.nc
# Note, the datasets listed for each variable are assumed to use the
# same spatial grid
DATASETS_NCEP-DOE_R2_surface=uflx.sfc.mon.mean.nc vflx.sfc.mon.mean.nc
DATASETS_NCEP-DOE_R2_pressure=uwnd.mon.mean.nc vwnd.mon.mean.nc

vpath %.mon.mean.nc ../download

PREPROCESSED_DATASETS=$(PREPROCESSED_ANNUAL_DATASETS_NCEP-DOE_R2_surface) $(PREPROCESSED_ANNUAL_DATASETS_NCEP-DOE_R2_1000mb) $(PREPROCESSED_MONTHLY_DATASETS_NCEP-DOE_R2_surface) $(PREPROCESSED_MONTHLY_DATASETS_NCEP-DOE_R2_1000mb)

all: $(PREPROCESSED_DATASETS)

.PHONY: all cleanall

cleanall:
	\rm -f $(PREPROCESSED_DATASETS)

# Preprocess previously downloaded datasets of monthly
# ocean-to-atmosphere momentum fluxes from the NCEP/DOE 2 Reanalysis
# [Kanamitsu et al., 2002] by computing long-term annual averages
# (ltaa) and long-term montly averages (ltma) over 31 years from 1979
# to 2009, removing obsolete dimension level, and collating the
# resulting two fields into one file. The preprocessing uses monthly
# fields derived from the NCEP/DOE 2 Ranalysis data provided by the
# NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their Web site at
# http://www.esrl.noaa.gov/psd/.
#
# Reference:
#
# Kanamitsu M., W. Ebisuzaki, J. Woollen, S-K Yang, J.J. Hnilo,
# M. Fiorino, and G. L. Potter (2002), NCEP-DOE AMIP-II Reanalysis
# (R-2), Bul. Atmos. Met. Soc., 1631-1643.
#
$(PREPROCESSED_ANNUAL_DATASETS_NCEP-DOE_R2_surface): $(DATASETS_NCEP-DOE_R2_surface)
# i) compute length of months in hours using the boundaries of the time
# axis
# ii) restrict data to only include 372 months (1979 to the end of 2009)
# iii) compute weighted average using length of months as weights
# iv) collate variables into one file
	\rm -f ./$@
	for file in $^; do \
	    filename=`basename $${file}`; \
	    varname=$${filename%%.*}; \
	    $(NCAP2) -O -s "nhour[time]=time_bnds(:,1)-time_bnds(:,0)" $$file ./$@.tmp; \
	    $(NCKS) -O -d time,0,371 ./$@.tmp ./$@.tmp; \
	    $(NCWA) -O -a time -v $$varname -w nhour ./$@.tmp ./$@.tmp; \
	    $(NCKS) -A -v $$varname ./$@.tmp ./$@; \
	    \rm ./$@.tmp; \
	done
$(PREPROCESSED_MONTHLY_DATASETS_NCEP-DOE_R2_surface): $(DATASETS_NCEP-DOE_R2_surface)
# i) compute length of months in hours using the boundaries of the time
# axis
# ii) restrict data to only include 372 months (1979 to the end of 2009)
# iii) compute weighted average using length of months as weights for each month
# iv) compute average length of each month
# v) collate variables into one file
	\rm -f ./$@
	for file in $^; do \
	    filename=`basename $${file}`; \
	    varname=$${filename%%.*}; \
	    $(NCAP2) -O -s "nhour[time]=time_bnds(:,1)-time_bnds(:,0)" $$file ./$@.tmp; \
	    $(NCKS) -O -d time,0,371 ./$@.tmp ./$@.tmp; \
	    for month in 00 01 02 03 04 05 06 07 08 09 10 11; do \
		$(NCWA) -O -a time -d time,$$month,,12 -v $$varname -w nhour ./$@.tmp ./$@.$$month.tmp; \
		$(NCWA) -O -a time -d time,$$month,,12 -v nhour ./$@.tmp ./$@.dt_$$month.tmp; \
		$(NCRENAME) -O -v nhour,dt_$$varname ./$@.dt_$$month.tmp; \
	    done; \
	    $(NCECAT) -A -v $$varname ./$@.??.tmp ./$@; \
	    $(NCECAT) -A -v dt_$$varname ./$@.dt_??.tmp ./$@; \
	    \rm ./$@.tmp ./$@.??.tmp ./$@.dt_??.tmp; \
	done

# Preprocess previously downloaded datasets of monthly zonal and
# meridional components of the wind fields on various pressure levels
# from the NCEP/DOE 2 Reanalysis [Kanamitsu et al., 2002] by computing
# long-term annual averages (ltaa) and long-term montly averages
# (ltma) over 31 years from 1979 to 2009, selecting the 1000mb layer
# only, and collating the resulting two fields into one file. The
# preprocessing uses monthly fields derived from the NCEP/DOE 2
# Ranalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado,
# USA, from their Web site at http://www.esrl.noaa.gov/psd/.
#
# Reference:
#
# Kanamitsu M., W. Ebisuzaki, J. Woollen, S-K Yang, J.J. Hnilo,
# M. Fiorino, and G. L. Potter (2002), NCEP-DOE AMIP-II Reanalysis
# (R-2), Bul. Atmos. Met. Soc., 1631-1643.
#
$(PREPROCESSED_ANNUAL_DATASETS_NCEP-DOE_R2_1000mb): $(DATASETS_NCEP-DOE_R2_pressure)
# i) compute length of months in hours using the boundaries of the time
# axis
# ii) restrict data to only include 372 months (1979 to the end of 2009)
# iii) extract 1000mb layer
# iv) compute weighted average using length of months as weights
# v) collate variables into one file
	\rm -f ./$@
	for file in $^; do \
	    filename=`basename $${file}`; \
	    varname=$${filename%%.*}; \
	    $(NCAP2) -O -s "nhour[time]=time_bnds(:,1)-time_bnds(:,0)" $$file ./$@.tmp; \
	    $(NCKS) -O -d time,0,371 ./$@.tmp ./$@.tmp; \
	    $(NCKS) -O -d level,0,0 ./$@.tmp ./$@.tmp; \
	    $(NCWA) -O -a level ./$@.tmp ./$@.tmp; \
	    $(NCWA) -O -a time -v $$varname -w nhour ./$@.tmp ./$@.tmp; \
	    $(NCKS) -A -v $$varname ./$@.tmp ./$@; \
	    \rm ./$@.tmp; \
	done
$(PREPROCESSED_MONTHLY_DATASETS_NCEP-DOE_R2_1000mb): $(DATASETS_NCEP-DOE_R2_pressure)
# i) compute length of months in hours using the boundaries of the time
# axis
# ii) restrict data to only include 372 months (1979 to the end of 2009)
# iii) extract 1000mb layer
# iv) compute weighted average using length of months as weights for each month
# v) compute average length of each month
# vi) collate variables into one file
	\rm -f ./$@
	for file in $^; do \
	    filename=`basename $${file}`; \
	    varname=$${filename%%.*}; \
	    $(NCAP2) -O -s "nhour[time]=time_bnds(:,1)-time_bnds(:,0)" $$file ./$@.tmp; \
	    $(NCKS) -O -d time,0,371 ./$@.tmp ./$@.tmp; \
	    $(NCKS) -O -d level,0,0 ./$@.tmp ./$@.tmp; \
	    $(NCWA) -O -a level ./$@.tmp ./$@.tmp; \
	    for month in 00 01 02 03 04 05 06 07 08 09 10 11; do \
		$(NCWA) -O -a time -d time,$$month,,12 -v $$varname -w nhour ./$@.tmp ./$@.$$month.tmp; \
		$(NCWA) -O -a time -d time,$$month,,12 -v nhour ./$@.tmp ./$@.dt_$$month.tmp; \
		$(NCRENAME) -O -v nhour,dt_$$varname ./$@.dt_$$month.tmp; \
	    done; \
	    $(NCECAT) -A -v $$varname ./$@.??.tmp ./$@; \
	    $(NCECAT) -A -v dt_$$varname ./$@.dt_??.tmp ./$@; \
	    \rm ./$@.tmp ./$@.??.tmp ./$@.dt_??.tmp; \
	done
