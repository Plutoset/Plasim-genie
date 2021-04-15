# ================== TESTING RULES ===================
# create datum files which are ASSUMED TO BE GOOD
# for the purposes of the tests RUN PRIOR TO CHECKIN
# only re-run 'assumedgood' for a known reason
old_assumedgood_vanilla : all
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
old_assumedgood_ig_go_sl : all
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_ig_go_sl_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
old_assumedgood_ig_go_gs : all
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_ig_go_gs_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
old_assumedgood_ig_fi_fi_ml : all
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_ig_fi_fi_ml_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
old_assumedgood_ig_go_gs_ml : all
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_ig_go_gs_ml_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"

# This runs all the short assumedgood targets
old_assumedgood : old_assumedgood_vanilla \
              old_assumedgood_ig_go_sl \
              old_assumedgood_ig_go_gs \
              old_assumedgood_ig_fi_fi_ml \
              old_assumedgood_ig_go_gs_ml

old_assumedgood_eb_go_gs :
	$(MAKE) cleanall
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_eb_go_gs_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall
old_assumedgood_eb_go_gs_ac_bg :
	$(MAKE) cleanall
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_eb_go_gs_ac_bg_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall
old_assumedgood_ig_fi_fi_glim :
	$(MAKE) cleanall
	\rm -rf $(GENIE_ROOT)/genie-icesheet/mod
	$(MAKE) FLAG_GLIMMER=ON
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_ig_fi_fi_glim_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall
old_assumedgood_r8r8 :
	$(MAKE) cleanall
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_r8r8_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall	
old_assumedgood_r8r4 :
	$(MAKE) cleanall
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_r8r4_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall	
old_assumedgood_r4r8 :
	$(MAKE) cleanall
	./genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_r4r8_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall
old_assumedgood_ig_go72_gs72_ml :
	$(MAKE) cleanall
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_ig_go72_gs72_ml_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall
old_assumedgood_ig_go6432_sl_gaalbedo :
	$(MAKE) cleanall
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_ig_go6432_sl_gaalbedo_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall
old_assumedgood_ig_fi_fi_l22 :
	$(MAKE) cleanall
	./old_genie_example.job \
		-r \
		-f $(CONFIG_DIR)/genie_ig_fi_fi_l22_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)"

# This runs all the long assumedgood targets
old_assumedgoodlong : old_assumedgood_eb_go_gs \
		  old_assumedgood_r8r8 \
                  old_assumedgood_r8r4 \
                  old_assumedgood_r4r8 \
                  old_assumedgood_ig_go72_gs72_ml \
                  old_assumedgood_ig_go6432_sl_gaalbedo \
                  old_assumedgood_ig_fi_fi_l22	

define OLD_SHORT_TEST_ACTIONS
./old_genie_example.job \
-t -z \
-f $(CONFIG_DIR)/genie_test.config \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" > test.out;
./old_genie_example.job \
-t -z \
-f $(CONFIG_DIR)/genie_ig_go_sl_test.config \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> test.out;
./old_genie_example.job \
-t -z \
-f $(CONFIG_DIR)/genie_ig_go_gs_test.config \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> test.out;
./old_genie_example.job \
-t -z \
-f $(CONFIG_DIR)/genie_ig_fi_fi_ml_test.config \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> test.out;
./old_genie_example.job \
-t -z \
-f $(CONFIG_DIR)/genie_ig_go_gs_ml_test.config \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> test.out;
endef

# GENIEr4, IGCMr4, GENIENX=36, GENIENY=36
# NB now compares against a "knowngood" file held in CVS
define OLD_EBGOGS_ACTIONS
$(MAKE) cleanall;
./old_genie_example.job \
-t -k \
-f $(CONFIG_DIR)/genie_eb_go_gs_test.config \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" > testebgogs.out;
endef

# NB compares against a "knowngood" file held in CVS
define OLD_BIOGEM_ACTIONS
$(MAKE) cleanall;
./old_genie_example.job \
-t -k \
-f $(CONFIG_DIR)/genie_eb_go_gs_ac_bg_test.config \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> testbiogem.out;
endef

define OLD_GLIMMER_ACTIONS
$(MAKE) cleanall;
\rm -rf $(GENIE_ROOT)/genie-icesheet/mod;
$(MAKE) FLAG_GLIMMER=ON;
./old_genie_example.job \
-t \
-f $(CONFIG_DIR)/genie_ig_fi_fi_glim_test.config \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" > testglimmer.out;
endef

# run the (faster) tests
old_test : all
	@ echo "**** TESTING - SHORTRUN ****"
# GENIEr4, IGCMr4
	$(OLD_SHORT_TEST_ACTIONS)
	@ grep -B1 "**TEST" test.out;

# ebgogs test separately
old_testebgogs :
	@ echo "**** TESTING - EBGOGS ****"
	$(OLD_EBGOGS_ACTIONS)
	@ grep -B1 "**TEST" testebgogs.out;

# ebgogs/biogem test separately
old_testbiogem :
	@ echo "**** TESTING - BIOGEM ****"
	$(OLD_BIOGEM_ACTIONS)
	@ grep -B1 "**TEST" testbiogem.out;

old_testglimmer :
	@ echo "**** TESTING - GLIMMER ****"
	$(OLD_GLIMMER_ACTIONS)
	@ grep -B1 "**TEST" testglimmer.out;

# slower tests
old_testlong :
# call ebgogs test first
	$(OLD_EBGOGS_ACTIONS)
	$(MAKE) cleanall
# then the biogem test
	$(OLD_BIOGEM_ACTIONS)
	$(MAKE) cleanall
# glimmer test net
	$(OLD_GLIMMER_ACTIONS)
	$(MAKE) cleanall
# GENIEr8, IGCMr4
	./old_genie_example.job \
		-t \
		-f $(CONFIG_DIR)/genie_r8r4_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" > testlong.out

	$(MAKE) cleanall
# GENIEr4, IGCMr8
	./old_genie_example.job \
		-t \
		-f $(CONFIG_DIR)/genie_r4r8_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out

	$(MAKE) cleanall
# GENIEr8, IGCMr8
	./old_genie_example.job \
		-t \
		-f $(CONFIG_DIR)/genie_r8r8_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go_sl_checkfluxes_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go_sl_ml_checkfluxes_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go_gs_checkfluxes_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go_gs_ml_checkfluxes_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-z -f $(CONFIG_DIR)/genie_ig_go_sl_restartmake_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go_sl_restartread_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-z -f $(CONFIG_DIR)/genie_ig_go_gs_restartmake_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go_gs_restartread_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-z -f $(CONFIG_DIR)/genie_ig_go_gs_ml_restartmake_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go_gs_ml_restartread_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z -k \
		-f $(CONFIG_DIR)/genie_na_go_ni_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out;

	$(MAKE) cleanall
# GENIEr8, IGCMr8, IGCM 22
	./old_genie_example.job \
		-t \
		-f $(CONFIG_DIR)/genie_ig_fi_fi_l22_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-f $(CONFIG_DIR)/genie_ig_go_sl_restartmake_l22_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go_sl_restartread_l22_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out

	$(MAKE) cleanall
# GENIEr8, IGCMr8, GOLDSTEIN 72
	./old_genie_example.job \
		-t \
		-f $(CONFIG_DIR)/genie_ig_go72_gs72_ml_checkfluxes_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-z -f $(CONFIG_DIR)/genie_ig_go72_gs72_ml_restartmake_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-z -t \
		-f $(CONFIG_DIR)/genie_ig_go72_gs72_ml_restartread_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-z -t \
		-f $(CONFIG_DIR)/genie_ig_go72_gs72_ml_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out

	$(MAKE) cleanall
# GENIEr8, IGCMr8, GOLDSTEIN 6432
	./old_genie_example.job \
		-t \
		-f $(CONFIG_DIR)/genie_ig_go6432_sl_checkfluxes_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-z -f $(CONFIG_DIR)/genie_ig_go6432_sl_restartmake_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go6432_sl_restartread_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_go6432_sl_gaalbedo_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out

	$(MAKE) cleanall
# GENIEr4, IGCMr4
	./old_genie_example.job \
		-f $(CONFIG_DIR)/genie_ig_fi_fi_restartmake_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_fi_fi_restartread_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-z -f $(CONFIG_DIR)/genie_ig_fi_fi_ml_restartmake_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
	./old_genie_example.job \
		-t -z \
		-f $(CONFIG_DIR)/genie_ig_fi_fi_ml_restartread_test.config \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-g $(RUNTIME_ROOT) \
		-m "$(MAKEFLAGS)" >> testlong.out
# *****************************************************************
# SOMEONE NEEDS TO FIND OUT WHY THIS TEST FAILS!!!!
# It fails early so it shouldn't be too hard - I expect it's
#   something to do with the ocean or seaice.
#	./old_genie_example.job \
#		-f $(CONFIG_DIR)/genie_ig_sl_sl_restartmake_test.config \
#		-o $(OUT_DIR) \
#		-c $(GENIE_ROOT) \
#		-g $(RUNTIME_ROOT) \
#		-m "$(MAKEFLAGS)" >> testlong.out
#	./old_genie_example.job \
#		-t \
#		-f $(CONFIG_DIR)/genie_ig_sl_sl_restartread_test.config \
#		-o $(OUT_DIR) \
#		-c $(GENIE_ROOT) \
#		-g $(RUNTIME_ROOT) \
#		-m "$(MAKEFLAGS)" >> testlong.out
# *****************************************************************
	$(OLD_SHORT_TEST_ACTIONS)
	@ echo "**** TESTING - SHORTRUN ****"
	@ grep -B1 "**TEST" test.out;
	@ echo "**** TESTING - LONGRUN ****"
	@ grep -B1 "**TEST" testebgogs.out;
	@ grep -B1 "**TEST" testbiogem.out;
	@ grep -B1 "**TEST" testglimmer.out;
	@ grep -B1 "**TEST" testlong.out;
