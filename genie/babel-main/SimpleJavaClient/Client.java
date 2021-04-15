
class Client
{  
        public static void main(String args[])
	{

//************************SETUP BELOW. SHOULD MATCH THE SERVER SETUP IN 'genie_example.job' and 'genie_control.f90'**************

	int ilon_ocn = 36;
	int ilat_ocn = 36;

    int ilon_atm = 36;
    int ilat_atm = 36;

	int ilon_sic = 36;
	int ilat_sic = 36;

	int inl1_ocn = 8;
	int inl2_ocn = 9;
	
	int gen_maxnyr = 720;
	int intrac_ocn = 11;
	
	int kocn_loop = 5;
	int katm_loop = 1;
	int ksic_loop = 5;
	
	int koverall_total_bbl = 200;

        int istep_ocn_bbl = 0;
        int istep_atm_bbl = 0;
        int istep_sic_bbl = 0;

//*********************************************VARIABLES FOR OCEAN + EMBM**********************************************************

	sidl.Double.Array1.Holder alon1_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_ocn, true));
	sidl.Double.Array1.Holder alat1_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn, true));
	sidl.Double.Array1.Holder alon2_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_ocn, true));
	sidl.Double.Array1.Holder alat2_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn, true));
	sidl.Double.Array1.Holder alon3_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_ocn, true));
	sidl.Double.Array1.Holder alat3_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn, true));


        sidl.Double.Array1.Holder alon1_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_atm, true));
        sidl.Double.Array1.Holder alat1_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_atm, true));
        sidl.Double.Array1.Holder alon2_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_atm, true));
        sidl.Double.Array1.Holder alat2_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_atm, true));
        sidl.Double.Array1.Holder alon3_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_atm, true));
        sidl.Double.Array1.Holder alat3_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_atm, true));

        sidl.Double.Array1.Holder alon1_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder alat1_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder alon2_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder alat2_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder alon3_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder alat3_sic_bbl = new sidl.Double.Array1.Holder();

	sidl.Double.Array1.Holder aboxedge1_lon_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_ocn + 1, true));
	sidl.Double.Array1.Holder aboxedge1_lat_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn + 1, true));
	sidl.Double.Array1.Holder aboxedge2_lon_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_ocn + 1, true));
	sidl.Double.Array1.Holder aboxedge2_lat_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn + 1, true));
	sidl.Double.Array1.Holder aboxedge3_lon_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_ocn + 1, true));
	sidl.Double.Array1.Holder aboxedge3_lat_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn + 1, true));

        sidl.Double.Array1.Holder aboxedge1_lon_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder aboxedge1_lat_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder aboxedge2_lon_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder aboxedge2_lat_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder aboxedge3_lon_sic_bbl = new sidl.Double.Array1.Holder();
        sidl.Double.Array1.Holder aboxedge3_lat_sic_bbl = new sidl.Double.Array1.Holder();
	
	sidl.Double.Array1.Holder depth1_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(inl1_ocn, true));
	sidl.Double.Array1.Holder depth2_ocn_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(inl2_ocn, true));
    
	sidl.Integer.Array2.Holder ilandmask1_ocn_bbl = new sidl.Integer.Array2.Holder(new sidl.Integer.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Integer.Array2.Holder ilandmask2_ocn_bbl = new sidl.Integer.Array2.Holder(new sidl.Integer.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Integer.Array2.Holder ilandmask3_ocn_bbl = new sidl.Integer.Array2.Holder(new sidl.Integer.Array2(ilon_ocn, ilat_ocn, true));

        sidl.Integer.Array2.Holder ilandmask1_sic_bbl = new sidl.Integer.Array2.Holder();
        sidl.Integer.Array2.Holder ilandmask2_sic_bbl = new sidl.Integer.Array2.Holder();
        sidl.Integer.Array2.Holder ilandmask3_sic_bbl = new sidl.Integer.Array2.Holder();

	sidl.Double.Array2.Holder tstar_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder sstar_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder ustar_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder vstar_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder albedo_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	
	sidl.Integer.Array1.Holder ias_out_bbl = new sidl.Integer.Array1.Holder(new sidl.Integer.Array1(ilat_ocn, true));
	sidl.Integer.Array1.Holder iaf_out_bbl = new sidl.Integer.Array1.Holder(new sidl.Integer.Array1(ilat_ocn, true));
	sidl.Integer.Array1.Holder ips_out_bbl = new sidl.Integer.Array1.Holder(new sidl.Integer.Array1(ilat_ocn, true));
	sidl.Integer.Array1.Holder ipf_out_bbl = new sidl.Integer.Array1.Holder(new sidl.Integer.Array1(ilat_ocn, true));

	sidl.Integer.Holder jsf_out_bbl =  new sidl.Integer.Holder();

	boolean lrestart_genie_bbl = false;

	sidl.Double.Array2.Holder latent_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder sensible_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder netsolar_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder netlong_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder conductflux_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder evap_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder precip_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder runoff_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder waterflux_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder ocean_stressx2_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder ocean_stressy2_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder ocean_stressx3_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Array2.Holder ocean_stressy3_ocn_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
	sidl.Double.Holder test_energy_ocean_bbl = new sidl.Double.Holder();
	sidl.Double.Holder test_water_ocean_bbl = new sidl.Double.Holder();
	int koverall_bbl = 1;

//*********************************************VARIABLES FOR EMBM**********************************************************

    sidl.Double.Array1.Holder aboxedge1_lon_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_atm + 1, true));
    sidl.Double.Array1.Holder aboxedge1_lat_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_atm + 1, true));
    sidl.Double.Array1.Holder aboxedge2_lon_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_atm + 1, true));
    sidl.Double.Array1.Holder aboxedge2_lat_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_atm + 1, true));
    sidl.Double.Array1.Holder aboxedge3_lon_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilon_atm + 1, true));
    sidl.Double.Array1.Holder aboxedge3_lat_atm_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_atm + 1, true));
    
    sidl.Integer.Array2.Holder ilandmask1_atm_bbl = new sidl.Integer.Array2.Holder(new sidl.Integer.Array2(ilon_atm, ilat_atm, true));
    sidl.Integer.Array2.Holder ilandmask2_atm_bbl = new sidl.Integer.Array2.Holder(new sidl.Integer.Array2(ilon_atm, ilat_atm, true));
    sidl.Integer.Array2.Holder ilandmask3_atm_bbl = new sidl.Integer.Array2.Holder(new sidl.Integer.Array2(ilon_atm, ilat_atm, true));
    
    sidl.Double.Array2.Holder tstar_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    sidl.Double.Array2.Holder surf_qstar_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    sidl.Double.Array2.Holder co2_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    sidl.Double.Holder atmos_dt_tim_bbl = new sidl.Double.Holder(0.0);

    sidl.Double.Array2.Holder surf_latent_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    sidl.Double.Array2.Holder netsolar_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    sidl.Double.Array2.Holder netlong_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    sidl.Double.Array2.Holder precip_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    sidl.Double.Array2.Holder atmos_lowestlh_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    
//******************************************VARIABLES FOR SEAICE***********************************************************
    
    sidl.Double.Array2.Holder hght_sic_bbl = new sidl.Double.Array2.Holder();
    sidl.Double.Array2.Holder frac_sic_bbl = new sidl.Double.Array2.Holder();
    sidl.Double.Array2.Holder temp_sic_bbl = new sidl.Double.Array2.Holder();
    sidl.Double.Array2.Holder albd_sic_bbl = new sidl.Double.Array2.Holder();
    sidl.Double.Holder test_energy_seaice_bbl = new sidl.Double.Holder();
     
    sidl.Double.Holder test_water_seaice_bbl = new sidl.Double.Holder();

    sidl.Double.Array2.Holder dhght_sic_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_sic, ilat_sic, true));
    sidl.Double.Array2.Holder dfrac_sic_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_sic, ilat_sic, true));
    
//******************************************VARIABLES FOR LAND***********************************************************
    
    
    sidl.Double.Array2.Holder surf_sensible_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));
    sidl.Double.Array2.Holder evap_atm_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_atm, ilat_atm, true));

    sidl.Double.Array2 land_sensibleinst_atm_bbl = new sidl.Double.Array2(ilon_atm, ilat_atm, true);
    sidl.Double.Array2 land_evap_atm_bbl = new sidl.Double.Array2(ilon_atm, ilat_atm, true);
    sidl.Double.Array2 land_runoff_atm_bbl = new sidl.Double.Array2(ilon_atm, ilat_atm, true);

//******************************************VARIABLES FOR BIOGEM***********************************************************
    
    sidl.Double.Holder go_solconst_bbl = new sidl.Double.Holder();    
    sidl.Double.Array2.Holder go_solfor_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilat_ocn, gen_maxnyr, true));
    sidl.Double.Array3.Holder go_tau_bbl = new sidl.Double.Array3.Holder(new sidl.Double.Array3(2, ilon_ocn, ilat_ocn, true));

    sidl.Integer.Holder go_npstp_bbl = new sidl.Integer.Holder();
    sidl.Integer.Holder go_iwstp_bbl = new sidl.Integer.Holder();
    sidl.Integer.Holder go_itstp_bbl = new sidl.Integer.Holder();
    sidl.Integer.Holder go_ianav_bbl = new sidl.Integer.Holder();

    sidl.Double.Holder go_saln0_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_rhoair_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_cd_bbl = new sidl.Double.Holder();    

    sidl.Double.Array1.Holder go_ds_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn, true));

    sidl.Double.Holder go_dphi_bbl = new sidl.Double.Holder();    
 
    sidl.Integer.Array1.Holder go_ips_bbl = new sidl.Integer.Array1.Holder(new sidl.Integer.Array1(ilat_ocn, true));
    sidl.Integer.Array1.Holder go_ipf_bbl = new sidl.Integer.Array1.Holder(new sidl.Integer.Array1(ilat_ocn, true));

    sidl.Double.Holder bg_usc_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_rsc_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_tsc_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_dsc_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_fsc_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_gsc_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_rh0sc_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_rhosc_bbl = new sidl.Double.Holder();    
    sidl.Double.Holder go_cpsc_bbl = new sidl.Double.Holder();    

    sidl.Integer.Array2.Holder go_k1_bbl = new sidl.Integer.Array2.Holder(new sidl.Integer.Array2(ilon_ocn, ilat_ocn, true));
    
    sidl.Integer.Holder go_lmax_bbl = new sidl.Integer.Holder();
 
    sidl.Double.Array1.Holder go_dz_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(inl1_ocn, true));
    sidl.Double.Array1.Holder go_dza_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(inl1_ocn, true));

    sidl.Integer.Array1.Holder go_ias_bbl = new sidl.Integer.Array1.Holder(new sidl.Integer.Array1(ilat_ocn, true));
    sidl.Integer.Array1.Holder go_iaf_bbl = new sidl.Integer.Array1.Holder(new sidl.Integer.Array1(ilat_ocn, true));

    sidl.Integer.Holder go_jsf_bbl = new sidl.Integer.Holder();

    sidl.Double.Array1.Holder go_c_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn + 1, true));
    sidl.Double.Array1.Holder go_cv_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn + 1, true));
    sidl.Double.Array1.Holder go_s_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn + 1, true));
    sidl.Double.Array1.Holder go_sv_bbl = new sidl.Double.Array1.Holder(new sidl.Double.Array1(ilat_ocn + 1, true));

    sidl.Double.Array4.Holder go_ts_bbl = new sidl.Double.Array4.Holder(new sidl.Double.Array4(intrac_ocn, ilon_ocn, ilat_ocn, inl1_ocn, true));
    sidl.Double.Array4.Holder go_ts1_bbl = new sidl.Double.Array4.Holder(new sidl.Double.Array4(intrac_ocn, ilon_ocn, ilat_ocn, inl1_ocn, true));
    
    sidl.String.Holder go_lin_bbl = new sidl.String.Holder();
    sidl.String.Holder go_lout_bbl = new sidl.String.Holder();
    
    sidl.Double.Array2.Holder go_cost_bbl = new sidl.Double.Array2.Holder(new sidl.Double.Array2(ilon_ocn, ilat_ocn, true));
    sidl.Double.Array4.Holder go_u_bbl = new sidl.Double.Array4.Holder(new sidl.Double.Array4(3,ilon_ocn,ilat_ocn,inl1_ocn, true));
    sidl.String.Holder go_ext_bbl = new sidl.String.Holder();
    
	try
	{
//*********************************************INSTANTIATIONS**************************************************************
		System.out.println("Instantiations!");
		
		ocean.goldstein goldstein_callee = new ocean.goldstein();
		atmosphere.embm embm_callee = new atmosphere.embm();
		seaice.goldseaice goldseaice_callee =  new seaice.goldseaice();

		System.out.println("Begin JAVA DIRECT CLIENT");

//*********************************************INITIALISATIONS************************************************************


		goldstein_callee.initialise(alon1_ocn_bbl, alat1_ocn_bbl, alon2_ocn_bbl, alat2_ocn_bbl, alon3_ocn_bbl, alat3_ocn_bbl, aboxedge1_lon_ocn_bbl, aboxedge1_lat_ocn_bbl, aboxedge2_lon_ocn_bbl, aboxedge2_lat_ocn_bbl, aboxedge3_lon_ocn_bbl, aboxedge3_lat_ocn_bbl, depth1_ocn_bbl,  depth2_ocn_bbl, ilandmask1_ocn_bbl, ilandmask2_ocn_bbl, ilandmask3_ocn_bbl, koverall_total_bbl, tstar_ocn_bbl, sstar_ocn_bbl,  ustar_ocn_bbl, vstar_ocn_bbl, albedo_ocn_bbl, ias_out_bbl, iaf_out_bbl, ips_out_bbl, ipf_out_bbl, jsf_out_bbl, lrestart_genie_bbl, go_npstp_bbl, go_iwstp_bbl, go_itstp_bbl, go_ianav_bbl, go_saln0_bbl, go_rhoair_bbl, go_cd_bbl, go_ds_bbl, go_dphi_bbl, go_ips_bbl, go_ipf_bbl, bg_usc_bbl, go_rsc_bbl, go_tsc_bbl, go_dsc_bbl, go_fsc_bbl, go_gsc_bbl, go_rh0sc_bbl, go_rhosc_bbl, go_cpsc_bbl, go_k1_bbl, go_lmax_bbl, go_dz_bbl, go_dza_bbl, go_ias_bbl, go_iaf_bbl, go_jsf_bbl, go_c_bbl, go_cv_bbl, go_s_bbl, go_sv_bbl, go_ts_bbl, go_ts1_bbl, go_lin_bbl, go_lout_bbl);
		
		System.out.println("In CLIENT AFTER INITIALISING GOLDSTEIN");


		embm_callee.initialise(alon1_atm_bbl, alat1_atm_bbl, alon2_atm_bbl, alat2_atm_bbl, alon3_atm_bbl,   alat3_atm_bbl, aboxedge1_lon_atm_bbl, aboxedge1_lat_atm_bbl, aboxedge2_lon_atm_bbl, aboxedge2_lat_atm_bbl, aboxedge3_lon_atm_bbl, aboxedge3_lat_atm_bbl, ilandmask1_atm_bbl, ilandmask2_atm_bbl, ilandmask3_atm_bbl, ias_out_bbl.get(), iaf_out_bbl.get(), ips_out_bbl.get(), ipf_out_bbl.get(), tstar_ocn_bbl.get(), koverall_total_bbl, co2_atm_bbl, ocean_stressx2_ocn_bbl, ocean_stressy2_ocn_bbl, ocean_stressx3_ocn_bbl, ocean_stressy3_ocn_bbl, tstar_atm_bbl, surf_qstar_atm_bbl, atmos_dt_tim_bbl, go_solconst_bbl);
		
		System.out.println("In CLIENT AFTER INITIALISING EMBM");

		goldseaice_callee.initialise(alon1_sic_bbl, alat1_sic_bbl, alon2_sic_bbl, alat2_sic_bbl, alon3_sic_bbl, alat3_sic_bbl, aboxedge1_lon_sic_bbl, aboxedge1_lat_sic_bbl, aboxedge2_lon_sic_bbl, aboxedge2_lat_sic_bbl, aboxedge3_lon_sic_bbl, aboxedge3_lat_sic_bbl, ilandmask1_sic_bbl,  ilandmask2_sic_bbl, ilandmask3_sic_bbl, koverall_total_bbl, hght_sic_bbl, frac_sic_bbl, temp_sic_bbl, albd_sic_bbl, test_energy_seaice_bbl);

		System.out.println("In CLIENT AFTER INITIALISING GOLDSTEIN SEAICE");

//*********************************************MAIN RUNS****************************************************************
          for ( koverall_bbl=1; koverall_bbl <= koverall_total_bbl; koverall_bbl++ ) {
//                System.out.println("koverall: " + koverall);

                if (koverall_bbl%8640 == 0) { 
                   System.out.println("koverall = " + koverall_bbl + "(of" + koverall_total_bbl + ")");
		}
                
		if (koverall_bbl%kocn_loop == 1) { 

		   istep_ocn_bbl++;

		   System.out.println(" In CLIENT CALLING SURFLUX");

                   embm_callee.run_surflux(istep_ocn_bbl, tstar_ocn_bbl.get(), sstar_ocn_bbl.get(), tstar_atm_bbl.get(), surf_qstar_atm_bbl.get(), hght_sic_bbl.get(), frac_sic_bbl.get(), temp_sic_bbl.get(), albd_sic_bbl.get(), ocean_stressx2_ocn_bbl.get(), ocean_stressy2_ocn_bbl.get(), ocean_stressx3_ocn_bbl.get(), ocean_stressy3_ocn_bbl.get(), co2_atm_bbl.get(), albedo_ocn_bbl, latent_ocn_bbl, sensible_ocn_bbl, netsolar_ocn_bbl, netlong_ocn_bbl, evap_ocn_bbl, precip_ocn_bbl, runoff_ocn_bbl, surf_latent_atm_bbl, surf_sensible_atm_bbl, netsolar_atm_bbl, netlong_atm_bbl, evap_atm_bbl, precip_atm_bbl, dhght_sic_bbl, dfrac_sic_bbl, atmos_lowestlh_atm_bbl, go_solfor_bbl, go_tau_bbl);
   
		}

		if (koverall_bbl%katm_loop == 0) { 
		   istep_atm_bbl++;
		
		   System.out.println(" In CLIENT CALLING EMBM");

                 embm_callee.run(istep_atm_bbl, surf_latent_atm_bbl.get(), surf_sensible_atm_bbl.get(), netsolar_atm_bbl.get(), netlong_atm_bbl.get(), evap_atm_bbl.get(), precip_atm_bbl.get(), ocean_stressx2_ocn_bbl, ocean_stressy2_ocn_bbl, ocean_stressx3_ocn_bbl, ocean_stressy3_ocn_bbl, tstar_atm_bbl, surf_qstar_atm_bbl, koverall_bbl);

		}

		if (koverall_bbl%ksic_loop == 0) { 
		   istep_sic_bbl++;

		   System.out.println(" In CLIENT CALLING GOLDSTEIN SEAICE");

		   goldseaice_callee.run(istep_sic_bbl, dhght_sic_bbl.get(), dfrac_sic_bbl.get(), ustar_ocn_bbl.get(), vstar_ocn_bbl.get(), hght_sic_bbl, frac_sic_bbl, temp_sic_bbl.get(), albd_sic_bbl.get(), waterflux_ocn_bbl, conductflux_ocn_bbl, test_energy_seaice_bbl, test_water_seaice_bbl, koverall_bbl);
		}

		if (koverall_bbl%kocn_loop == 0) { 

		   if (koverall_bbl%8640 == 0) { 
		      System.out.println(" calling the goldstein ocean model");
		   }

		   System.out.println(" In CLIENT CALLING GOLDSTEIN");

		   goldstein_callee.run(istep_ocn_bbl, latent_ocn_bbl.get(), sensible_ocn_bbl.get(), netsolar_ocn_bbl.get(),  netlong_ocn_bbl.get(), conductflux_ocn_bbl.get(), evap_ocn_bbl.get(), precip_ocn_bbl.get(), runoff_ocn_bbl.get(), waterflux_ocn_bbl.get(), ocean_stressx2_ocn_bbl.get(), ocean_stressy2_ocn_bbl.get(), ocean_stressx3_ocn_bbl.get(), ocean_stressy3_ocn_bbl.get(), tstar_ocn_bbl, sstar_ocn_bbl, ustar_ocn_bbl, vstar_ocn_bbl, albedo_ocn_bbl, test_energy_ocean_bbl, test_water_ocean_bbl, koverall_bbl, go_ts_bbl, go_ts1_bbl, go_cost_bbl, go_u_bbl, go_ext_bbl);
		}

          }
          System.out.println("Finally!");

//*********************************************FINALISATIONS*************************************************************


//*******************************************END OF MAIN PROGRAM*********************************************************

		System.out.println("\nEnd");
	} catch (java.lang.Exception ex){
		System.out.println("Error!!!" + ex);
		}
	}
	
	static void print(sidl.Double.Array1 arr){
	
	  for(int i=0; i < arr.length();  i=i+1){
	    System.out.println("i=" + i + " value:" + arr.get(i));
	    
	  }
	  }

	static void print(sidl.Double.Array2 arr){
	
	  for(int i=0; i < arr.length(0);  i=i+1){
		  for(int j=0; j < arr.length(1);  j=j+1){
	             System.out.println("i,j=" + i + j+ " value:" + arr.get(j,i) + " ");
	  	  }
	  }
	}

	static void print(sidl.Integer.Array1 arr){
	
	  for(int i=0; i < arr.length();  i=i+1){
		System.out.print(arr.get(i) + " ");
	  }
	  System.out.println();
	}

	static void print(sidl.Double.Array4 arr){
	
	  for(int i=0; i < arr.length(0);  i=i+1){
		  for(int j=0; j < arr.length(1);  j=j+1){
			  for(int k=0; k < arr.length(2);  k=k+1){
				  for(int l=0; l < arr.length(3);  l=l+1){
	             System.out.print(arr.get(i,j,k,l) + " ");
				}
			}
	  	  }
	  }
	}
	
}
 
