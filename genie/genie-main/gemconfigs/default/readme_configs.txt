CONFIGURATIONS
--------------

worap1_NSGA_NSGA: 8-level IFT closed using worap1.k1; NSGA2 calibration of climate; NSGA2 calibration of PO4 and ALK cycling in the ocean
worap2_NSGA_NSGA: 16-level IFT closed using worap2.k1; NSGA2 calibration of climate; NSGA2 calibration of PO4 and ALK cycling in the ocean
worap2_NSGA_NSGA_b: 16-level IFT closed using worap2.k1; NSGA2 calibration of climate; NSGA2 calibration of PO4 and ALK cycling in the ocean;
                    BUT altered Redfield ratios (note that the marine carbon cycle is not re-calibrated with the altered Redfield ratios)
EXPERIMENTS
-----------

genie_eb_go_gs_ac_bg_itfclsd_08l.spin_worap1_NSGA_NSGA: 10,001 year spin-up using worap1_NSGA_NSGA
genie_eb_go_gs_ac_bg_itfclsd_16l.spin_worap2_NSGA_NSGA: 10,001 year spin-up using worap2_NSGA_NSGA
genie_eb_go_gs_ac_bg_itfclsd_16l.spin_worap2_NSGA_NSGA_b: 10,001 year spin-up using worap2_NSGA_NSGA_b
genie_eb_go_gs_ac_bg_itfclsd_08l.anth_worap1_NSGA_NSGA: year 0 -> year 2001 anthropogenic transient from spin-up; worap1_NSGA_NSGA
genie_eb_go_gs_ac_bg_itfclsd_16l.anth_worap2_NSGA_NSGA: year 0 -> year 2001 anthropogenic transient from spin-up; worap2_NSGA_NSGA
genie_eb_go_gs_ac_bg_itfclsd_16l.anth_worap2_NSGA_NSGA_b: year 0 -> year 2001 anthropogenic transient from spin-up; worap2_NSGA_NSGA_b

