# ================== TESTING RULES ===================
# create datum files which are ASSUMED TO BE GOOD
# for the purposes of the tests RUN PRIOR TO CHECKIN
# only re-run 'assumedgood' for a known reason
#assumedgood_vanilla : all
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# assumedgood_ig_go_sl : all
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/ig_go_sl_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# assumedgood_ig_go_gs : all
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/ig_go_gs_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# assumedgood_ig_fi_fi_ml :
# 	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/ig_fi_fi_ml_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# 	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# assumedgood_ig_go_gs_ml :
# 	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/ig_go_gs_ml_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# 	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall	

# # This runs all the short assumedgood targets
# assumedgood : assumedgood_vanilla \
#               assumedgood_ig_go_sl \
#               assumedgood_ig_go_gs

# # and those for 'testmoses'..
# assumedgoodmoses : assumedgood_ig_fi_fi_ml \
#                    assumedgood_ig_go_gs_ml

assumedgood_eb_go_gs :
	$(MAKE) cleanall
	./genie.job \
		-r \
		-f $(CONFIG_DIR)/eb_go_gs_test.xml \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall
assumedgood_eb_go_gs_ac_bg :
	$(MAKE) cleanall
	./genie.job \
		-r \
		-f $(CONFIG_DIR)/eb_go_gs_ac_bg_test.xml \
		-o $(OUT_DIR) \
		-c $(GENIE_ROOT) \
		-m "$(MAKEFLAGS)"
	$(MAKE) cleanall
# assumedgood_ig_fi_fi_glim :
# 	$(MAKE) cleanall
# 	\rm -rf $(GENIE_ROOT)/genie-icesheet/mod
# 	$(MAKE) FLAG_GLIMMER=ON
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/ig_fi_fi_glim_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# 	$(MAKE) cleanall
# assumedgood_r8r8 :
# 	$(MAKE) cleanall
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/r8r8_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# 	$(MAKE) cleanall	
# assumedgood_r8r4 :
# 	$(MAKE) cleanall
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/r8r4_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# 	$(MAKE) cleanall	
# assumedgood_r4r8 :
# 	$(MAKE) cleanall
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/r4r8_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# 	$(MAKE) cleanall
# assumedgood_ig_go72_gs72_ml :
# 	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/ig_go72_gs72_ml_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# 	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# assumedgood_ig_go6432_sl_gaalbedo :
# 	$(MAKE) cleanall
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/ig_go6432_sl_gaalbedo_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-m "$(MAKEFLAGS)"
# 	$(MAKE) cleanall
# assumedgood_ig_fi_fi_l22 :
# 	$(MAKE) cleanall
# 	./genie.job \
# 		-r \
# 		-f $(CONFIG_DIR)/ig_fi_fi_l22_test.xml \
# 		-o $(OUT_DIR) \
# 		-c $(GENIE_ROOT) \
# 		-g $(RUNTIME_ROOT) \
# 		-m "$(MAKEFLAGS)"

# # This runs all the long assumedgood targets
# assumedgoodlong : assumedgood_eb_go_gs \
# 		  assumedgood_r8r8 \
#                   assumedgood_r8r4 \
#                   assumedgood_r4r8 \
#                   assumedgood_ig_go72_gs72_ml \
#                   assumedgood_ig_go6432_sl_gaalbedo \
#                   assumedgood_ig_fi_fi_l22	

# define SHORT_TEST_ACTIONS
# ./genie.job \
# -t \
# -f $(CONFIG_DIR)/test.xml \
# -o $(OUT_DIR) \
# -c $(GENIE_ROOT) \
# -g $(RUNTIME_ROOT) \
# -m "$(MAKEFLAGS)" > test.out;
# ./genie.job \
# -t -z \
# -f $(CONFIG_DIR)/ig_go_sl_test.xml \
# -o $(OUT_DIR) \
# -c $(GENIE_ROOT) \
# -g $(RUNTIME_ROOT) \
# -m "$(MAKEFLAGS)" >> test.out;
# ./genie.job \
# -t -z \
# -f $(CONFIG_DIR)/ig_go_gs_test.xml \
# -o $(OUT_DIR) \
# -c $(GENIE_ROOT) \
# -g $(RUNTIME_ROOT) \
# -m "$(MAKEFLAGS)" >> test.out;
# endef

# define MOSES_ACTIONS
# $(MAKE) FLAG_MOSESTRIFFID=ON cleanall;
# ./genie.job \
# -t \
# -f $(CONFIG_DIR)/ig_fi_fi_ml_test.xml \
# -o $(OUT_DIR) \
# -c $(GENIE_ROOT) \
# -g $(RUNTIME_ROOT) \
# -m "$(MAKEFLAGS)" > testmoses.out;
# ./genie.job \
# -t -z \
# -f $(CONFIG_DIR)/ig_go_gs_ml_test.xml \
# -o $(OUT_DIR) \
# -c $(GENIE_ROOT) \
# -g $(RUNTIME_ROOT) \
# -m "$(MAKEFLAGS)" >> testmoses.out;
# endef

# GENIEr4, IGCMr4, GENIENX=36, GENIENY=36
# NB now compares against a "knowngood" file held in CVS
define EBGOGS_ACTIONS
$(MAKE) cleanall;
./genie.job \
-t -k \
-f $(CONFIG_DIR)/eb_go_gs_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" > testebgogs.out;
./genie.job \
-f $(CONFIG_DIR)/eb_go_gs_restartmake_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> testebgogs.out
./genie.job \
-t -z \
-f $(CONFIG_DIR)/eb_go_gs_restartread_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" >> testebgogs.out
endef

# NB compares against a "knowngood" file held in CVS
define BIOGEM_ACTIONS
$(MAKE) cleanall;
./genie.job \
-t -k \
-f $(CONFIG_DIR)/eb_go_gs_ac_bg_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" > testbiogem.out;
endef

# define GLIMMER_ACTIONS
# $(MAKE) cleanall;
# \rm -rf $(GENIE_ROOT)/genie-icesheet/mod;
# ./genie.job \
# -t \
# -f $(CONFIG_DIR)/ig_fi_fi_glim_test.xml \
# -o $(OUT_DIR) \
# -c $(GENIE_ROOT) \
# -g $(RUNTIME_ROOT) \
# -m "$(MAKEFLAGS)" > testglimmer.out;
# endef

# NB now compares against a "knowngood" file held in SVN
define ENTS_ACTIONS
$(MAKE) cleanall;
./genie.job \
-t -k \
-f $(CONFIG_DIR)/eb_go_gs_el_test.xml \
-o $(OUT_DIR) \
-c $(GENIE_ROOT) \
-g $(RUNTIME_ROOT) \
-m "$(MAKEFLAGS)" > testents.out;
endef

# run the (faster) tests
#test : 
#	@ echo "**** TESTING - SHORTRUN ****"
# GENIEr4, IGCMr4
#	$(SHORT_TEST_ACTIONS)
#	@ grep -B1 "**TEST" test.out;

# ebgogs test separately
testebgogs :
	@ echo "**** TESTING - EBGOGS ****"
	$(EBGOGS_ACTIONS)
	@ grep -B1 "**TEST" testebgogs.out;

# ebgogs/biogem test separately
testbiogem :
	@ echo "**** TESTING - BIOGEM ****"
	$(BIOGEM_ACTIONS)
	@ grep -B1 "**TEST" testbiogem.out;

#testglimmer :
#	@ echo "**** TESTING - GLIMMER ****"
#	$(GLIMMER_ACTIONS)
#	@ grep -B1 "**TEST" testglimmer.out;

#testmoses :
#	@ echo "**** TESTING - MOSESTRIFFID ****"
#	$(MOSES_ACTIONS)
#	@ grep -B1 "**TEST" testmoses.out;

# ebgogssl test separately
testents :
	@ echo "**** TESTING - ENTS ****"
	$(ENTS_ACTIONS)
	@ grep -B1 "**TEST" testents.out;



# slower tests
testlong :
#	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
	$(MAKE) cleanall
# IGCM based short tests
#	$(SHORT_TEST_ACTIONS)
#	$(MAKE) cleanall
# and with moses
#	$(MOSES_ACTIONS)
#	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# call ebgogs test
	$(EBGOGS_ACTIONS)
	$(MAKE) cleanall
# then the biogem test
	$(BIOGEM_ACTIONS)
	$(MAKE) cleanall
# then the ents test
	$(ENTS_ACTIONS)
	$(MAKE) cleanall
# glimmer test next
#	$(GLIMMER_ACTIONS)
#	$(MAKE) cleanall
# GENIEr8, IGCMr4
#	./genie.job \
#		-t \
#		-f $(CONFIG_DIR)/r8r4_test.xml \
#		-o $(OUT_DIR) \
#		-c $(GENIE_ROOT) \
#		-g $(RUNTIME_ROOT) \
#		-m "$(MAKEFLAGS)" > testlong.out

#	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# GENIEr4, IGCMr8
#	./genie.job \
#		-t \
#		-f $(CONFIG_DIR)/r4r8_test.xml \
#		-o $(OUT_DIR) \
#		-c $(GENIE_ROOT) \
#		-g $(RUNTIME_ROOT) \
#		-m "$(MAKEFLAGS)" >> testlong.out

#	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# GENIEr8, IGCMr8
	# ./genie.job \
	# 	-t \
	# 	-f $(CONFIG_DIR)/r8r8_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go_sl_checkfluxes_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go_sl_ml_checkfluxes_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go_gs_checkfluxes_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go_gs_ml_checkfluxes_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-z -f $(CONFIG_DIR)/ig_go_sl_restartmake_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go_sl_restartread_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-z -f $(CONFIG_DIR)/ig_go_gs_restartmake_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go_gs_restartread_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-z -f $(CONFIG_DIR)/ig_go_gs_ml_restartmake_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go_gs_ml_restartread_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z -k \
	# 	-f $(CONFIG_DIR)/na_go_ni_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out;

#	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# GENIEr8, IGCMr8, IGCM 22
	# ./genie.job \
	# 	-t \
	# 	-f $(CONFIG_DIR)/ig_fi_fi_l22_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-f $(CONFIG_DIR)/ig_go_sl_restartmake_l22_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go_sl_restartread_l22_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out

#	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# GENIEr8, IGCMr8, GOLDSTEIN 72
	# ./genie.job \
	# 	-t \
	# 	-f $(CONFIG_DIR)/ig_go72_gs72_ml_checkfluxes_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-z -f $(CONFIG_DIR)/ig_go72_gs72_ml_restartmake_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-z -t \
	# 	-f $(CONFIG_DIR)/ig_go72_gs72_ml_restartread_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-z -t \
	# 	-f $(CONFIG_DIR)/ig_go72_gs72_ml_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out

#	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# GENIEr8, IGCMr8, GOLDSTEIN 6432
	# ./genie.job \
	# 	-t \
	# 	-f $(CONFIG_DIR)/ig_go6432_sl_checkfluxes_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-z -f $(CONFIG_DIR)/ig_go6432_sl_restartmake_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go6432_sl_restartread_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_go6432_sl_gaalbedo_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out

#	$(MAKE) FLAG_MOSESTRIFFID=ON cleanall
# GENIEr8, IGCMr8 - NB these tests used to be r4r4, but they did not pass
# on a 64bit machine.  This may be due to the doubles written in the
# restart files.
	# ./genie.job \
	# 	-f $(CONFIG_DIR)/ig_fi_fi_restartmake_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_fi_fi_restartread_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-z -f $(CONFIG_DIR)/ig_fi_fi_ml_restartmake_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
	# ./genie.job \
	# 	-t -z \
	# 	-f $(CONFIG_DIR)/ig_fi_fi_ml_restartread_test.xml \
	# 	-o $(OUT_DIR) \
	# 	-c $(GENIE_ROOT) \
	# 	-g $(RUNTIME_ROOT) \
	# 	-m "$(MAKEFLAGS)" >> testlong.out
# *****************************************************************
# SOMEONE NEEDS TO FIND OUT WHY THIS TEST FAILS!!!!
# It fails early so it shouldn't be too hard - I expect it's
#   something to do with the ocean or seaice.
#	./genie_example.job \
#		-f $(CONFIG_DIR)/ig_sl_sl_restartmake_test.xml \
#		-o $(OUT_DIR) \
#		-c $(GENIE_ROOT) \
#		-g $(RUNTIME_ROOT) \
#		-m "$(MAKEFLAGS)" >> testlong.out
#	./genie_example.job \
#		-t \
#		-f $(CONFIG_DIR)/ig_sl_sl_restartread_test.xml \
#		-o $(OUT_DIR) \
#		-c $(GENIE_ROOT) \
#		-g $(RUNTIME_ROOT) \
#		-m "$(MAKEFLAGS)" >> testlong.out
# *****************************************************************
#	@ echo "**** TESTING - TEST ****"
#	@ grep -B1 "**TEST" test.out;
#	@ echo "**** TESTING - TESTMOSES ****"
#	@ grep -B1 "**TEST" testmoses.out;
	@ echo "**** TESTING - TESTEBGOGS ****"
	@ grep -B1 "**TEST" testebgogs.out;
	@ echo "**** TESTING - TESTBIOGEM ****"
	@ grep -B1 "**TEST" testbiogem.out;
	@ echo "**** TESTING - TESTENTS ****"
	@ grep -B1 "**TEST" testents.out;
#	@ echo "**** TESTING - TESTGLIMMER ****"
#	@ grep -B1 "**TEST" testglimmer.out;
#	@ echo "**** TESTING - LONGRUN ****"
#	@ grep -B1 "**TEST" testlong.out;

