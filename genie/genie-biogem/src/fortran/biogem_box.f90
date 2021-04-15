! *************************************************************************************************
! biogem_box.f90
! C-GOLDSTEIn/BioGeM
! MISCELLANEOUS MECHANICS OF THE SYSTEM
! *************************************************************************************************


MODULE biogem_box


  use gem_carbchem
  USE biogem_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! OCEAN-ATMOSPHERE EXCHANGE
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SOLUBILITY COEFFICIENT
  subroutine sub_calc_solconst(dum_i,dum_j)
    ! dummy arguments
    integer,INTENT(in)::dum_i,dum_j
    ! local variables
    integer::l,ia
    REAL::loc_T,loc_rT,loc_Tr100,loc_S
    real::loc_rho
    ! calculate local constants
    ! NOTE: pressure in units of (bar) (1 m depth approx = 1 dbar pressure)
    ! NOTE: temperature in K
    ! NOTE: restrict valid T,S range for empirical fit
    loc_T     = ocn(io_T,dum_i,dum_j,n_k)
    loc_S     = ocn(io_S,dum_i,dum_j,n_k)
    ! ### THESE VALUES NEED TO BE REPLACED WITH THE ACTUAL LITERATURE VALUES RAHTER THAN MY GUESS ... ;) ######################### !
    ! ### CURRENTLY, THEY TAKE THE SAME CONSERVATIVE LIMITS AS THE Mehrbach K1, K2 CONSTANTS] #################################### !
    if (loc_T <  const_zeroC +  2.0)  loc_T = const_zeroC +  2.0
    if (loc_T > (const_zeroC + 35.0)) loc_T = const_zeroC + 35.0
    if (loc_S < 26.0) loc_S = 26.0
    if (loc_S > 43.0) loc_S = 43.0
    ! ############################################################################################################################ !
    loc_rT    = 1.0/loc_T
    loc_Tr100 = loc_T/100.0
    loc_rho   = phys_ocn(ipo_rho,dum_i,dum_j,n_k)
    ! calculate Solubility Coefficients (mol/(kg atm))
    ! NOTE: for CO2 and N2O, the soluability coefficient is in units of mol/(kg atm)
    !       rather than as a Bunsen Solubility Coefficient (see Wanninkohf [1992])
    !       => convert units for others
    ! NOTE: for CFC-11 and CFC-12, the soluability coefficient is in units of mol/(kg atm)
    !       rather than as a Bunsen Solubility Coefficient (see Wanninkohf [1992])
    !       (actaully, it is not really this simple and K should be corrected for water vapour pressure and lame things like that)
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       IF (atm_type(ia) == 1) then
          SELECT CASE (ia)
          CASE (ia_pCO2,ia_pN2O)
             ocnatm_airsea_solconst(ia,dum_i,dum_j) = EXP( &
                  & par_bunsen_coef(1,ia) + par_bunsen_coef(2,ia)*(100*loc_rT) + par_bunsen_coef(3,ia)*LOG(loc_Tr100) + &
                  & loc_S*(par_bunsen_coef(4,ia) + par_bunsen_coef(5,ia)*(loc_Tr100) + par_bunsen_coef(6,ia)*(loc_Tr100)**2) &
                  &  )
          CASE (ia_pCFC11,ia_pCFC12)
             ocnatm_airsea_solconst(ia,dum_i,dum_j) = EXP( &
                  & par_bunsen_coef(1,ia) + par_bunsen_coef(2,ia)*(100*loc_rT) + par_bunsen_coef(3,ia)*LOG(loc_Tr100) + &
                  & loc_S*(par_bunsen_coef(4,ia) + par_bunsen_coef(5,ia)*(loc_Tr100) + par_bunsen_coef(6,ia)*(loc_Tr100)**2) &
                  &  )
          CASE default
             ocnatm_airsea_solconst(ia,dum_i,dum_j) = EXP( &
                  & par_bunsen_coef(1,ia) + par_bunsen_coef(2,ia)*(100*loc_rT) + par_bunsen_coef(3,ia)*LOG(loc_Tr100) + &
                  & loc_S*(par_bunsen_coef(4,ia) + par_bunsen_coef(5,ia)*(loc_Tr100) + par_bunsen_coef(6,ia)*(loc_Tr100)**2) &
                  &  )/ &
                  & (loc_rho*const_V)
          END SELECT
       end if
    end do
  end subroutine sub_calc_solconst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE WIND SPEED FROM WIND STRESS
  function fun_calc_u()
    ! result variable
    REAL,dimension(n_i,n_j)::fun_calc_u ! units of (m s-1)
    ! local variables
    integer::i,j
    real::tv,tv2,tv3
    ! calculate wind speed from wind stress - see: gseta.F:
    ! usurf(i,j) = sqrt((sqrt(tv**2 + tv2**2))*rh0sc*dsc*usc*fsc/(rhoair*cd*scf))
    fun_calc_u(:,:) = 0.0
    do j=1,n_j
       tv3 = 0.0
       do i=1,n_i
          if (i == 1) then
             tv = (phys_ocnatm(ipoa_tau_u,i,j) + phys_ocnatm(ipoa_tau_u,n_i,j))/2.0
          else
             tv = (phys_ocnatm(ipoa_tau_u,i,j) + phys_ocnatm(ipoa_tau_u,i - 1,j))/2.0
          endif
          if (j == 1) then
             tv2 = phys_ocnatm(ipoa_tau_v,i,j)/2.0
          else
             tv2 = (phys_ocnatm(ipoa_tau_v,i,j) + phys_ocnatm(ipoa_tau_v,i,j - 1))/2.0
          endif
          fun_calc_u(i,j) = sqrt((sqrt(tv**2 + tv2**2))/(goldstein_rhoair*goldstein_cd))
          tv3 = tv3 + fun_calc_u(i,j)
       enddo
       do i=1,n_i
          if ((j < 2) .OR. (j > (n_j - 1))) fun_calc_u(i,j) = tv3/real(n_i)
       enddo
    enddo
  END function fun_calc_u
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE PISTON VELOCITY
  SUBROUTINE sub_calc_pv(dum_i,dum_j)
    ! dummy arguments
    INTEGER::dum_i,dum_j
    ! local variables
    integer::l,ia
    REAL::loc_Sc
    REAL::loc_TC,loc_TC2,loc_TC3
    real::loc_u2
    ! set local variables
    ! temperature powers
    ! NOTE: temeprature must be converted to the correct units (degrees C)
    ! NOTE: valid temperature range is 0 - 30 C for the Schmidt number empirical fit - see: Wanninkhof et al. [1992]
    loc_TC  = ocn(io_T,dum_i,dum_j,n_k) - const_zeroC
    if (loc_TC <  0.0) loc_TC =  0.0 
    if (loc_TC > 30.0) loc_TC = 30.0 
    loc_TC2 = loc_TC*loc_TC
    loc_TC3 = loc_TC2*loc_TC
    ! wind speed^2
    loc_u2 = phys_ocnatm(ipoa_u,dum_i,dum_j)**2
    !  calculate piston velocity
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       IF (atm_type(ia) == 1) then
          ! calculate gas transfer Schmidt number
          loc_Sc =                           &
               & par_Sc_coef(1,ia)         - &
               & par_Sc_coef(2,ia)*loc_TC  + &
               & par_Sc_coef(3,ia)*loc_TC2 - &
               & par_Sc_coef(4,ia)*loc_TC3
          ! calculate CO2 gas transfer velocity (piston velocity)
          ! NOTE: from Wanninkhof [1992] equation 1/3
          ! NOTE: convert from units of (cm hr-1) to (m yr-1)
          ! NOTE: pre-calculate 1.0/660 (= 1.515E-3)
          ocnatm_airsea_pv(ia,dum_i,dum_j) = conv_cm_m*conv_yr_hr*par_gastransfer_a*loc_u2*(loc_Sc*1.515E-3)**(-0.5)
       end if
    end do
  END SUBROUTINE sub_calc_pv
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE AIR-SEA GAS EXCHANGE
  FUNCTION fun_calc_ocnatm_flux(dum_i,dum_j,dum_atm,dum_dt)
    ! result variable
    REAL,dimension(n_atm)::fun_calc_ocnatm_flux ! units of (mol yr-1)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    REAL,dimension(n_atm),INTENT(in)::dum_atm
    REAL,INTENT(in)::dum_dt
    ! local variables
    integer::l,ia,io
    REAL,dimension(n_atm)::loc_focnatm,loc_fatmocn
    real::loc_alpha_k,loc_alpha_alpha
    real::loc_alpha_sa,loc_alpha_as
    real::loc_rho
    real::loc_TC
    real::loc_r13C_ocn,loc_r14C_ocn
    real::loc_r13C_atm,loc_r14C_atm
    real::loc_R_atm,loc_R_ocn
    real::loc_ocn,loc_atm
    real::loc_A
    real::loc_r_dflux_deqm
    real::loc_buff
    REAL,dimension(n_atm)::loc_dflux
    REAL,dimension(n_ocn)::loc_deqm

    ! *** INITIALIZE VARIABLES ***
    loc_focnatm(:) = 0.0
    loc_fatmocn(:) = 0.0
    loc_rho = phys_ocn(ipo_rho,dum_i,dum_j,n_k)
    loc_TC = ocn(io_T,dum_i,dum_j,n_k) - const_zeroC
    ! area available for air-sea gas transfer
    loc_A = (1.0 - phys_ocnatm(ipoa_seaice,dum_i,dum_j))*phys_ocnatm(ipoa_A,dum_i,dum_j)

    ! *** calculate air-sea gas exchange fluxes ***
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       if (.NOT. ocnatm_airsea_eqm(ia)) then
          ! set corresponding ocean tracer
          ! NOTE: assume that there is a one-to-one mapping from atm tracers to ocn tracers,
          !       with the corresponding ocean tracer index given in the i=1 index position of the conv_atm_ocn_i array
          io = conv_atm_ocn_i(1,ia)
          SELECT CASE (atm_type(ia))
          CASE (1)
             ! calculate bulk gas exchange
             ! set local ocean and atmosphere tracer variables
             ! NOTE: check for special case of CO2
             !       -> there are 3 dissolved components associated with CO2(g) (CO2, HCO3, CO3)
             !          but only CO2(aq) is relevant to air-sea gas exchange)
             !       -> also, don't restrict air-sea gas exchange on the basis of estimated equilibrium with CO2(aq)
             !          because the buffering provided by eqm with HCO3 and CO3 is not taken into account
             !          => set a relative buffering factor for CO2, 
             !             chosen to ensure numerical stability yet not unduely restrict air-sea CO2 exchange
             ! NOTE: local atmospheric tracer value has Bunsen Solubility Coefficient implicit in its value 
             loc_atm = ocnatm_airsea_solconst(ia,dum_i,dum_j)*dum_atm(ia)
             if (io == io_DIC) then
                loc_ocn = carb(ic_conc_CO2,dum_i,dum_j,n_k)
                ! calculate limitation of air-sea exchange of CO2 based on Revelle factor (see: Zeebe and Wolf-Gladwor [2001])
                ! NOTE: RF0 = d[CO2]/[CO2] / dDIC/DIC
                !       => d[CO2]/dDIC = RF0 * [CO2]/DIC
                !          => loc_buff = 1.0 / (RF0 * [CO2]/DIC)
                ! NOTE: the new factor is not infinitely different from the previous code of ([CO2] + [CO3]) / [CO2]
                loc_buff = 1.0/( carb(ic_RF0,dum_i,dum_j,n_k)*carb(ic_conc_CO2,dum_i,dum_j,n_k)/ocn(io_DIC,dum_i,dum_j,n_k) )
             else
                loc_ocn = ocn(io,dum_i,dum_j,n_k)
                loc_buff = 1.0
             end if
             ! make sure nothing 'nasty' can happen if a tracer has a -ve concentration
             ! (if shouldn't really have in the first place, but, well ...) 
             if (loc_ocn < const_real_nullsmall) loc_ocn = 0.0
             if (loc_atm < const_real_nullsmall) loc_atm = 0.0
             ! calculate gas exchange fluxes ocn->atm and atm->ocn
             ! NOTE: units of (mol yr-1)
             ! NOTE: the solubility coefficient must be converted to units of mol/(kg atm) from a Bunsen Solubility Coefficient
             loc_focnatm(ia) = ocnatm_airsea_pv(ia,dum_i,dum_j)*loc_A*loc_rho*loc_ocn
             loc_fatmocn(ia) = ocnatm_airsea_pv(ia,dum_i,dum_j)*loc_A*loc_rho*loc_atm
             ! check for 'excessive' gas transfer (i.e., with the potential to lead to numerical instability)
             ! => rescale the fluxes to that the ocean surface is brought exactly into equilibrium
             ! NOTE: in the case of DIC, only CO2(aq) is considered
             ! NOTE: no account is taken of molar changes due to ocean circulation and biological activity in making this check
             ! calculate the molar magnitude of ocean deficit or surfit w.r.t. the atmosphere
             loc_deqm(io) = phys_ocn(ipo_dD,dum_i,dum_j,n_k)*phys_ocnatm(ipoa_A,dum_i,dum_j)*loc_rho*abs(loc_atm - loc_buff*loc_ocn)
             ! calculate the molar transfer that would normally then be applied
             loc_dflux(ia) = dum_dt*abs(loc_focnatm(ia) - loc_fatmocn(ia))
             ! ensure that molar transfer does not exceed the current disequilibrium
             ! (i.e., ensure that a +ve disequilibrium is not turned into a larger -ve disequilibrium at the next time-step)
             If (loc_deqm(io) > const_real_nullsmall) then
                loc_r_dflux_deqm = loc_dflux(ia)/loc_deqm(io)
                if (loc_r_dflux_deqm > par_airsea_r_dflux_deqm_max) then
                   loc_focnatm(ia) = (par_airsea_r_dflux_deqm_max/loc_r_dflux_deqm)*loc_focnatm(ia)
                   loc_fatmocn(ia) = (par_airsea_r_dflux_deqm_max/loc_r_dflux_deqm)*loc_fatmocn(ia)
                   IF (ctrl_debug_reportwarnings) then
                      print*,'WARNING: excessive air-sea flux of ',trim(string_atm(ia)), &
                           & ' prevented at (',fun_conv_num_char_n(2,dum_i),',',fun_conv_num_char_n(2,dum_j),')'
                   end IF
                end if
             end If
          case default
             ! calculate derived isotopic exchange
             ! NOTE: assume that associated bulk tracer flux has already been calculated (i.e., earlier during this routine)
             ! NOTE: convert r (13C/(13C + 12C)) ratio to R (13C/12C) before applying fractionation factor alpha
             ! NOTE: assume that the total mass of C is approximately equal to 12C + 13C
             ! NOTE: for 14C, the standard is already in the form: 14C/C
             SELECT CASE (ia)
             CASE (ia_pCO2_13C)
                ! isotopic fluxes - 13C
                loc_r13C_atm = dum_atm(ia_pCO2_13C)/dum_atm(ia_pCO2)
                loc_r13C_ocn = carbisor(ici_CO2_r13C,dum_i,dum_j,n_k)
                loc_R_atm = loc_r13C_atm/(1.0 - loc_r13C_atm)
                loc_R_ocn = loc_r13C_ocn/(1.0 - loc_r13C_ocn)
                ! overall 13C fractionationscheme following Marchal et al. [1998]
                ! NOTE: fractionation factors taken from Zhang et al. [1995]
                ! NOTE: some notation borrowed from Yamahaka and Tajika [1996]
                ! kinetic fractionation
                loc_alpha_k = 0.99912
                ! air-sea equilibrium fractionation between aqueous CO2 and gaseous CO2
                loc_alpha_alpha = 0.99869 + 4.9E-6*loc_TC
                ! overall fractionation factors in both directions
                loc_alpha_as = loc_alpha_alpha*loc_alpha_k
                loc_alpha_sa = loc_alpha_k
                ! calculate fluxes
                loc_fatmocn(ia_pCO2_13C) = (loc_alpha_as*loc_R_atm/(1.0 + loc_alpha_as*loc_R_atm))*loc_fatmocn(ia_pCO2)
                loc_focnatm(ia_pCO2_13C) = (loc_alpha_sa*loc_R_ocn/(1.0 + loc_alpha_sa*loc_R_ocn))*loc_focnatm(ia_pCO2)
             CASE (ia_pCO2_14C)
                ! isotopic fluxes - 14C
                loc_r14C_atm = dum_atm(ia_pCO2_14C)/dum_atm(ia_pCO2)
                loc_r14C_ocn = carbisor(ici_CO2_r14C,dum_i,dum_j,n_k)
                loc_R_atm = loc_r14C_atm/(1.0 - loc_r14C_atm)
                loc_R_ocn = loc_r14C_ocn/(1.0 - loc_r14C_ocn)
                loc_fatmocn(ia_pCO2_14C) = (loc_alpha_as**2*loc_R_atm/(1.0 + loc_alpha_as**2*loc_R_atm))*loc_fatmocn(ia_pCO2)
                loc_focnatm(ia_pCO2_14C) = (loc_alpha_sa**2*loc_R_ocn/(1.0 + loc_alpha_sa**2*loc_R_ocn))*loc_focnatm(ia_pCO2)
             CASE (ia_pCH4_13C)
                if (dum_atm(ia_pCH4) > const_real_nullsmall) then
                   loc_r13C_atm = dum_atm(ia_pCH4_13C)/dum_atm(ia_pCH4)
                   loc_fatmocn(ia_pCH4_13C) = loc_r13C_atm*loc_fatmocn(ia_pCH4)
                end if
                if (ocn(io_CH4,dum_i,dum_j,n_k) > const_real_nullsmall) then
                   loc_r13C_ocn = ocn(io_CH4_13C,dum_i,dum_j,n_k)/ocn(io_CH4,dum_i,dum_j,n_k)
                   loc_focnatm(ia_pCH4_13C) = loc_r13C_ocn*loc_focnatm(ia_pCH4)
                end if
             CASE (ia_pCH4_14C)
                if (dum_atm(ia_pCH4) > const_real_nullsmall) then
                   loc_r14C_atm = dum_atm(ia_pCH4_14C)/dum_atm(ia_pCH4)
                   loc_fatmocn(ia_pCH4_14C) = loc_r14C_atm*loc_fatmocn(ia_pCH4)
                end if
                if (ocn(io_CH4,dum_i,dum_j,n_k) > const_real_nullsmall) then
                   loc_r14C_ocn = ocn(io_CH4_14C,dum_i,dum_j,n_k)/ocn(io_CH4,dum_i,dum_j,n_k)
                   loc_focnatm(ia_pCH4_14C) = loc_r14C_ocn*loc_focnatm(ia_pCH4)
                end if
             case default          
                ! ### INSERT CODE TO DEAL WITH ADDITIONAL ISOTOPES ############################################################### !
                ! 
                ! ################################################################################################################ !
             end SELECT
          end SELECT
          ! calculate net gas transfer and set results variable
          fun_calc_ocnatm_flux(ia) = loc_focnatm(ia) - loc_fatmocn(ia)
       end if
    end do

  END FUNCTION fun_calc_ocnatm_flux
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WATER-COLUMN TRANSFORMATION PROCESSES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE BIOLOGICAL PRODUCTIVITY
  SUBROUTINE sub_calc_bio(dum_i,dum_j,dum_k1,dum_dt)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    INTEGER,INTENT(in)::dum_k1
    real,intent(in)::dum_dt
    select case (par_bio_prodopt)
    case (                         &
         & '1N1T_PO4restore',      &
         & '1N1T_PO4restoreLL',    &
         & '1N1T_PO4MM',           &
         & '2N1T_PO4MM_SiO2',      &
         & '1N1T_PO4MM_Cd',        &
         & '2N2T_PO4MM_NO3',       &
         & 'Payal_Cd',             &
         & 'bio_PFe',              &
         & 'bio_PFe_OCMIP2',       &
         & 'bio_PFeSi',            &
         & 'bio_PFeSi_Ridgwell02', &
         & 'bio_POCflux'           &
         & )
       ! biologically induced (mass balance) schemes
       call sub_calc_bio_uptake(dum_i,dum_j,dum_k1,dum_dt)
    case default
       ! NOTHING
    end select
  end SUBROUTINE sub_calc_bio
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE BIOLOGICAL TRACER UPTAKE AT THE SURFACE OCEAN -- biologically induced (mass balance) schemes
  ! NOTE: assume complete homogenization over mixed layer
  !       => calculate uptaked based on tracer concentrations in surface layer only
  SUBROUTINE sub_calc_bio_uptake(dum_i,dum_j,dum_k1,dum_dt)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    INTEGER,INTENT(in)::dum_k1
    real,intent(in)::dum_dt
    ! local variables
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real::loc_dPO4
    real::loc_dPO4_1,loc_dPO4_2
    real::loc_frac_N2fix
    real::loc_ficefree,loc_intI,loc_kI,loc_kT
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real::loc_Kq
    real::loc_delta_Corg,loc_delta_CaCO3
    real::loc_alpha
    real::loc_r15N
    real::loc_r30Si
    real::loc_r114Cd
    real::loc_r7Li
    real::loc_R
    real::loc_PO4,loc_Cd,loc_FeT,loc_SiO2
    real::loc_kPO4,loc_kFe,loc_kFe_sp,loc_kFe_nsp,loc_kCd,loc_kSiO2
    real::loc_TC
    real::loc_bio_red_DOMfrac
    real::loc_bio_red_POC_POFe_1,loc_bio_red_POC_POFe_2
    real::loc_d13C_DIC_Corg_ef
    integer::loc_k_mld

    ! *** INITIALIZE VARIABLES ***
    loc_Cd = 0.0
    loc_dPO4_1 = 0.0
    loc_dPO4_2 = 0.0
    loc_frac_N2fix = 0.0 
    loc_bio_uptake(:,:) = 0.0
    loc_k_mld = dum_k1

    ! *** CALCULATE MIXED LAYER PROPERTIES ***
    ! NOTE: MLD is stored as a POSITIVE depth below the surface
    ! k limit
    DO k=n_k,1,-1
       If (phys_ocn(ipo_Dbot,dum_i,dum_j,k) >= phys_ocnatm(ipoa_mld,dum_i,dum_j)) then
          loc_k_mld = k
          exit
       end If
    end DO

    ! *** CALCULATE LOCAL NUTRIENT CONCENTRATIONS & LIMITATIONS ***
    if (ocn_select(io_PO4)) then
       loc_PO4 = ocn(io_PO4,dum_i,dum_j,n_k)
       loc_kPO4 = loc_PO4/(loc_PO4 + par_bio_c0_PO4)
    end if
    if (ocn_select(io_Fe)) then
       loc_FeT = ocn(io_Fe,dum_i,dum_j,n_k) + ocn(io_FeL,dum_i,dum_j,n_k)
       loc_kFe = loc_FeT/(loc_FeT + par_bio_c0_Fe)
       loc_kFe_sp = loc_FeT/(loc_FeT + par_bio_c0_Fe_sp)
       loc_kFe_nsp = loc_FeT/(loc_FeT + par_bio_c0_Fe_nsp)
    end if
    if (ocn_select(io_Cd)) then
       loc_Cd = ocn(io_Cd,dum_i,dum_j,n_k)
       loc_kCd = loc_Cd/(loc_Cd + par_bio_c0_Cd)
    end if
    if (ocn_select(io_SiO2)) then
       loc_SiO2 = ocn(io_SiO2,dum_i,dum_j,n_k)
       loc_kSiO2 = loc_SiO2/(loc_SiO2 + par_bio_c0_SiO2)
    end if

    ! *** CALCULATE PRODUCTIVITY MODIFIERS ***
    ! fractional ice-free coverage
    loc_ficefree = (1.0 - phys_ocnatm(ipoa_seaice,dum_i,dum_j))
    ! insolation modifier
    ! ### EDIT ADD AND/OR EXTEND BIOLOGICAL OPTIONS ############################################################################## !
    select case (par_bio_prodopt)
    case (                      &
         & '1N1T_PO4restoreLL', &
         & '1N1T_PO4MM',        &
         & '2N1T_PO4MM_SiO2',   &
         & '1N1T_PO4MM_Cd',     &
         & '2N2T_PO4MM_NO3'     &
         & )
       loc_kI = phys_ocnatm(ipoa_solfor,dum_i,dum_j)/phys_solar_constant
    case (                        &
         & 'Payal_Cd',            &
         & 'bio_PFe',             &
         & 'bio_PFe_OCMIP2',      &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       ! calculate integrated insolation over depth of entire mixed layer
       ! => assume e-folding depth of 20 m (set in <par_bio_eI>) [Doney et al., 2006] (i.e., as in OCMIP-2 definition)
       !    i.e.: I(int) = [(-20)*e(-thickness/20) - (-20)*e(0/20)]*I(incident)/thickness
       ! NOTE: slight deviation from OCMIP-2, as much as it is possible to understand the text in Doney et al. [2006] ... ;)
       loc_intI = (-par_bio_I_eL)*(exp(-phys_ocn(ipo_Dbot,dum_i,dum_j,loc_k_mld)/par_bio_I_eL) - 1.0)* &
            & phys_ocnatm(ipoa_fxsw,dum_i,dum_j)/phys_ocn(ipo_dbot,dum_i,dum_j,loc_k_mld)
       loc_kI = loc_intI/(loc_intI + par_bio_c0_I)
    case default
       loc_kI = 0.0
    end select
    ! ############################################################################################################################ !
    ! temperature
    loc_TC = ocn(io_T,dum_i,dum_j,n_k) - const_zeroC
    SELECT CASE (par_bio_prodopt)
    case (                        &
         & 'bio_PFe',             &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       loc_kT = par_bio_kT0*exp(loc_TC/par_bio_kT_eT)
    case (           &
         & 'bio_PFe_OCMIP2' &
         & )
       loc_kT = (loc_TC + 2.0)/(loc_TC + 10.0)
    case default
       loc_kT = 0.0
    end SELECT

    ! *** SET DOM FRACTION ***
    SELECT CASE (par_bio_prodopt)
    case (                        &
         & 'bio_PFe',             &
         & 'bio_PFeSi'            &
         & )
       loc_bio_red_DOMfrac = (1.0 - 0.5/(par_bio_kT0*exp(loc_TC/par_bio_kT_eT)))*par_bio_red_DOMfrac
    case default
       loc_bio_red_DOMfrac = par_bio_red_DOMfrac
    end SELECT

    ! *** ADJUST PARTICULATE COMPOSITION 'REDFIELD' RATIOS ***
    ! CaCO3:POC 'rain ratio'
    ! NOTE: a correction is made for the fact that a proportion of the POM is transformed into DOM,
    !      whereas the initially calculated CaCO3 and opal fluxes do not change
    !      => re-scale CaCO3 and opal ratios so that the prescribed export ratio value better reflects final export composition
    if (sed_select(is_CaCO3)) then
       if (ctrl_force_CaCO3toPOCrainratio) then
          bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (1.0 - loc_bio_red_DOMfrac)*par_bio_CaCO3toPOCrainratio(dum_i,dum_j)
       else
          if (carb(ic_ohm_cal,dum_i,dum_j,n_k) > 1.0) then
             bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = (1.0 - loc_bio_red_DOMfrac)*par_bio_red_POC_CaCO3* &
                  & (carb(ic_ohm_cal,dum_i,dum_j,n_k) - 1.0)**par_bio_red_POC_CaCO3_pP
          else
             bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = 0.0
          end if
       end if
    end if
    ! If no H4SiO3 limitation is considered in the biological production, modify Si:C uptake according to H4SiO4 depletion
    ! (NOTE: this is just to ensure that SiO2 does not drop below zero - it does not consistute true nutrient limitation)
    ! For H4SiO4-limited biological production, adjust Si:C uptake for Fe replete conditions (Ridgwell et al. [2002], GBC)
    if (sed_select(is_opal)) then
       SELECT CASE (par_bio_prodopt)
       case (                        &
            & 'bio_PFeSi',           &
            & 'bio_PFeSi_Ridgwell02' &
            & )
          if (ocn(io_SiO2,dum_i,dum_j,n_k) > const_real_nullsmall) then
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = (1.0 - loc_bio_red_DOMfrac)*par_bio_red_POC_opal* &
                  & ((par_bio_c0_Fe_sp/(loc_FeT+par_part_red_FeTmin))+1.0)
          else
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = 0.0
          end if
       case default
          if (ocn(io_SiO2,dum_i,dum_j,n_k) > const_real_nullsmall) then
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = (1.0 - loc_bio_red_DOMfrac)*par_bio_red_POC_opal*loc_kSiO2
          else
             bio_part_red(is_POC,is_opal,dum_i,dum_j) = 0.0
          end if
       end SELECT
    end if
    ! modify Fe:C cellular quotient according to Fe limitation
    ! NOTE: following Ridgwell [2001] (mean parameter values from diatom and coccolithophorid parameterizations)
    ! NOTE: default uniform Fe:C ratio has already been set during model initialization
    ! NOTE: for options 'bio_PFeSi*' this has no effect, as below two different ratios will be used for sicliceous
    ! and non-siliceous phytoplankton. Finally an appropriate mixture of these two ratios (according to the ratio
    ! of siliceous and non-siliceous phytoplankton produced) will be used to update the value computed here.
    ! KEY: par_bio_FetoC_pP == power in [FeT] dependent Fe:C ratio equation [Ridgwell, 2001] (-0.4225)
    !      par_bio_FetoC_K  == scaling in [FeT] dependent Fe:C ratio equation [Ridgwell, 2001] (103684.0)
    !      par_bio_FetoC_C  == constant in [FeT] dependent Fe:C ratio equation [Ridgwell, 2001] (0.0)
    if (sed_select(is_POFe)) then
       if (.NOT. ctrl_bio_red_fixedFetoC) then
          if (loc_FeT > par_part_red_FeTmin) then
             bio_part_red(is_POC,is_POFe,dum_i,dum_j) = 1.0/ &
                  & MIN(par_part_red_FetoCmax,(par_bio_FetoC_C + par_bio_FetoC_K*(1.0E9*loc_FeT)**(par_bio_FetoC_pP)))
          else
             bio_part_red(is_POC,is_POFe,dum_i,dum_j) = 1.0/par_part_red_FetoCmax
          end if
       end if
    end if
    ! TRACE METALS
    ! Cd
    if (ocn_select(io_Cd)) then
       if (loc_Cd > const_real_nullsmall) then
          if (ctrl_force_POCdtoPOCrainratio) then
             ! (1) take POCd:POC ratio from prescribed 2-D field
             bio_part_red(is_POC,is_POCd,dum_i,dum_j) = par_bio_POCdtoPOCrainratio(dum_i,dum_j)
          elseif (ctrl_bio_red_CdtoC_Felim) then
             ! (2) calculate POCd:POC ratio according to nutrient limitation
             !     assume enhanced Cd uptake when Fe nutrient limitation dominates other nutrient limitations
             !     .AND. there is Fe limitation in the first place
             !     NOTE: simply take nutrient 'limitation' as being defined by an ambient conc. less than the half-sat conc.
             if ((loc_kFe < 0.5) .AND. ((loc_kPO4 - loc_kFe) > const_real_nullsmall)) then
                bio_part_red(is_POC,is_POCd,dum_i,dum_j) = par_bio_red_CdtoC_Felim_max                
             else
                bio_part_red(is_POC,is_POCd,dum_i,dum_j) = par_bio_red_CdtoC_Felim_min
             end if
          else
             ! (3) default scheme
             !     NOTE: the default scheme has two components;
             !           par_bio_red_POC_POCd is a parameter for defining a fixed cellular C:Cd (Cd/C) ratio
             !           the remainder of the parameterization is the Elderfield and Rickaby [2000] partition coefficient model
             if (ctrl_force_Cd_alpha) par_bio_red_POC_POCd_alpha = par_bio_Cd_alpha(dum_i,dum_j)
             if (loc_PO4 > const_real_nullsmall) then
                bio_part_red(is_POC,is_POCd,dum_i,dum_j) = par_bio_red_POC_POCd + par_bio_red_POC_POCd_alpha* &
                     & loc_Cd/(bio_part_red(is_POP,is_POC,dum_i,dum_j)*loc_PO4)
             else
                bio_part_red(is_POC,is_POCd,dum_i,dum_j) = 0.0
             end if
          end if
       else
          bio_part_red(is_POC,is_POCd,dum_i,dum_j) = 0.0
       end if
    end if
    if (ocn_select(io_Cd) .AND. ocn_select(io_Ca)) then
!!$       bio_part_red(is_CaCO3,is_CdCO3,dum_i,dum_j) = par_bio_red_CaCO3_CdCO3 + par_bio_red_CaCO3_CdCO3_alpha* &
!!$            & ocn(io_Cd,dum_i,dum_j,n_k)/ocn(io_Ca,dum_i,dum_j,n_k)
    end if
    ! Li
    if (ocn_select(io_Li) .AND. ocn_select(io_Ca)) then
       bio_part_red(is_CaCO3,is_LiCO3,dum_i,dum_j) = par_bio_red_CaCO3_LiCO3 + par_bio_red_CaCO3_LiCO3_alpha* &
            & ocn(io_Li,dum_i,dum_j,n_k)/ocn(io_Ca,dum_i,dum_j,n_k)
    end if

    ! *** CALCULATE PO4 DEPLETION ***
    ! NOTE: production is calculated as the concentration of newly-formed particulate material in the surface ocean layer
    !       that occurs within any single time step
    !       i.e., loc_dPO4 is in units of (mol kg-1)
    ! ### EDIT ADD AND/OR EXTEND BIOLOGICAL OPTIONS ############################################################################## !
    SELECT CASE (par_bio_prodopt)
    CASE ('NONE')
       ! nought going on ('abiological')
       loc_dPO4 = 0.0
    CASE ( &
         & '1N1T_PO4restore' &
         & )
       ! 1 x nutrient, 1 x 'taxa': PO4 restoring
       ! NOTE: filter for positive productivity; indicated by a negative value of <dum_docn_restore>
       !       (i.e., predicted model nutrient concentrations are higher than the restoring target)
       if (force_restore_docn_nuts(io_PO4) < -const_real_nullsmall) then
          loc_dPO4 = &
               loc_ficefree*(-force_restore_docn_nuts(io_PO4))
       else
          loc_dPO4 = 0.0
       end if
    CASE (                     &
         & '1N1T_PO4restoreLL' &
         & )
       ! 1 x nutrient, 1 x 'taxa': PO4 restoring + light limitation
       ! NOTE: filter for positive productivity; indicated by a negative value of <dum_docn_restore>
       !       (i.e., predicted model nutrient concentrations are higher than the restoring target)
       if (force_restore_docn_nuts(io_PO4) < -const_real_nullsmall) then
          loc_dPO4 = &
               & loc_ficefree* &
               & loc_kI* &
               & (-force_restore_docn_nuts(io_PO4))
       else
          loc_dPO4 = 0.0
       end if
    CASE (                    &
         & '1N1T_PO4MM',      &
         & '2N1T_PO4MM_SiO2', &
         & '1N1T_PO4MM_Cd'    &
         & )
       ! 1 x nutrient, 1 x 'taxa': PO4 Michaelis-Menton
       if (loc_PO4 > const_real_nullsmall) then
          loc_dPO4 = &
               & dum_dt* &
               & loc_ficefree* &
               & loc_kI* &
               & loc_kPO4* &
               & par_bio_k0_PO4
       else
          loc_dPO4 = 0.0
       end if
    CASE (            &
         & 'Payal_Cd' &
         & )
       ! Parekh et al. [2005] scheme
       if (loc_PO4 > const_real_nullsmall .AND. loc_FeT > const_real_nullsmall) then
          loc_dPO4 = &
               & dum_dt* &
               & loc_ficefree* &
               & loc_kI* &
               & min(loc_kPO4,loc_kFe)* &
               & par_bio_k0_PO4
       else
          loc_dPO4 = 0.0
       end if
    CASE (                  &
         & 'bio_PFe',       &
         & 'bio_PFe_OCMIP2' &
         & )
       ! structure of uptake parameterization after Doney et al. [2006]
       ! NOTE: the scaling for MLD > the compensation depth in Doney et al. [2006] is implicitly account for
       !       by the creation of organic matter throughout the MLD layersmax
       !      (the explicit equivalent would be to add the term: max(1.0,phys_ocnatm(ipoa_mld,dum_i,dum_j)/par_bio_zc)
       if (loc_PO4 > const_real_nullsmall .AND. loc_FeT > const_real_nullsmall) then
          loc_dPO4 =                                                                                                    &
               & dum_dt*                                                                                                & 
               & loc_ficefree*                                                                                          &
               & loc_kT*                                                                                                &
               & min(loc_kPO4,loc_kFe)*                                                                                 &
               & loc_kI*                                                                                                &
               & min(loc_PO4,bio_part_red(is_POC,is_POP,dum_i,dum_j)*loc_FeT/bio_part_red(is_POC,is_POFe,dum_i,dum_j))/ &
               & par_bio_tau
       else
          loc_dPO4 = 0.0
       end if
    CASE (                        &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       ! PO4 and H4SiO4 Michaelis-Menton + iron + light + temperature limitation for siliceous and non-siliceous species
       ! The structure of the uptake parameterization follows Doney et al. (2006, J. Climate), but in contrast to the
       ! formulation by Doney et al. (2006, J. Climate), the uptake is computed separately for siliceous and non-siliceous
       ! phytoplankton following Ridgwell et al. (2002, Global Biogeochem. Cycles)

       ! Parameterisations for POFe:POC ratios follows Ridgwell (2001, Ph.D. thesis)
       if (.NOT. ctrl_bio_red_fixedFetoC) then
          if (loc_FeT > par_part_red_FeTmin) then
             loc_bio_red_POC_POFe_1 = 1.0/ &
                  & MIN(333000.0,15000.0 + 115623.0*(1.0E9*(loc_FeT - par_part_red_FeTmin))**(-0.65))
             loc_bio_red_POC_POFe_2 = 1.0/ &
                  & MIN(333000.0,20000.0 + 31805.0*(1.0E9*(loc_FeT - par_part_red_FeTmin))**(-0.65))
          else
             loc_bio_red_POC_POFe_1 = 1.0/333000.0
             loc_bio_red_POC_POFe_2 = 1.0/333000.0
          end if
       else
          loc_bio_red_POC_POFe_1 = bio_part_red(is_POC,is_POFe,dum_i,dum_j)
          loc_bio_red_POC_POFe_2 = bio_part_red(is_POC,is_POFe,dum_i,dum_j)
       end if

       ! loc_dPO4_1 is phosphate depletion according to siliceous species productivity, loc_dPO4_2 is phosphate depletion
       ! according to non-siliceous species productivity
       ! loc_dPO4=loc_dPO4_1+loc_dPO4_2 is the total phosphate depletion
       if (loc_PO4 > const_real_nullsmall .AND. loc_FeT > const_real_nullsmall .AND. loc_SiO2 > const_real_nullsmall) then
          loc_dPO4_1 =                                                                                                  &
               & dum_dt*                                                                                                & 
               & loc_ficefree*                                                                                          &
               & loc_kT*                                                                                                &
               & min(loc_kPO4,loc_kFe_sp,loc_kSiO2)*                                                                    &
               & loc_kI
          if (par_bio_prodopt == 'bio_PFeSi_Ridgwell02') then
             loc_dPO4_1 = loc_dPO4_1*                                                                                   &
                  par_bio_k0_PO4*par_bio_relprod_sp
          else
             loc_dPO4_1 = loc_dPO4_1*                                                                                   &
                  & min(loc_PO4,                                                                                        &
                  &     bio_part_red(is_POC,is_POP,dum_i,dum_j)*loc_FeT/loc_bio_red_POC_POFe_1,                         &
                  &     bio_part_red(is_POC,is_POP,dum_i,dum_j)*loc_SiO2/bio_part_red(is_POC,is_opal,dum_i,dum_j))*     &
                  &     par_bio_relprod_sp/par_bio_tau
          endif
       else
          loc_dPO4_1 = 0.0
       end if

       if (loc_PO4 > const_real_nullsmall .AND. loc_FeT > const_real_nullsmall) then
          loc_dPO4_2 =                                                                                                  &
               & dum_dt*                                                                                                & 
               & loc_ficefree*                                                                                          &
               & loc_kT*                                                                                                &
               & min(loc_kPO4,loc_kFe_nsp)*                                                                             &
               & loc_kI
          if (par_bio_prodopt == 'bio_PFeSi_Ridgwell02') then
             loc_dPO4_2 = loc_dPO4_2*                                                                                   &
                  & par_bio_k0_PO4*(1.0-par_bio_relprod_sp)
          else
             loc_dPO4_2 = loc_dPO4_2*                                                                                   &
                  & min(loc_PO4,bio_part_red(is_POC,is_POP,dum_i,dum_j)*loc_FeT/loc_bio_red_POC_POFe_2)*                &
                  & (1.0-par_bio_relprod_sp)/par_bio_tau
          endif
       else
          loc_dPO4_2 = 0.0
       end if

       loc_dPO4 = loc_dPO4_1 + loc_dPO4_2

       ! Adjust "Redfield" ratios to take into account differential production of siliceous and non-siliceous species
       ! No differential sp/nsp behaviour for POC
       ! No differential sp/nsp behaviour for PON
       ! differential sp/nsp behaviour of POCd
       if (ocn_select(io_Cd)) then
          if (ctrl_bio_red_CdtoC_Felim) then
             if ((loc_Cd > const_real_nullsmall) .AND. (loc_dPO4 > const_real_nullsmall)) then
                if ((loc_kFe_sp < 0.5) .AND. ((loc_kPO4 - loc_kFe_sp) > const_real_nullsmall)) then
                   bio_part_red(is_POC,is_POCd,dum_i,dum_j) = loc_dPO4_1*par_bio_red_CdtoC_Felim_max/loc_dPO4
                else
                   bio_part_red(is_POC,is_POCd,dum_i,dum_j) = loc_dPO4_1*par_bio_red_CdtoC_Felim_min/loc_dPO4
                end if
                if ((loc_kFe_nsp < 0.5) .AND. ((loc_kPO4 - loc_kFe_nsp) > const_real_nullsmall)) then
                   bio_part_red(is_POC,is_POCd,dum_i,dum_j) = bio_part_red(is_POC,is_POCd,dum_i,dum_j)+ &
                        loc_dPO4_2*par_bio_red_CdtoC_Felim_max/loc_dPO4
                else
                   bio_part_red(is_POC,is_POCd,dum_i,dum_j) = bio_part_red(is_POC,is_POCd,dum_i,dum_j)+ &
                        loc_dPO4_2*par_bio_red_CdtoC_Felim_min/loc_dPO4
                end if
             else
                bio_part_red(is_POC,is_POCd,dum_i,dum_j) = 0.0
             endif
          endif
       endif
       ! differentail sp/nsp behaviour of POFe
       if (sed_select(is_POFe)) then
          if (.NOT. ctrl_bio_red_fixedFetoC) then
             if (loc_dPO4 > const_real_nullsmall) then
                bio_part_red(is_POC,is_POFe,dum_i,dum_j) = &
                     & (loc_dPO4_1*loc_bio_red_POC_POFe_1+loc_dPO4_2*loc_bio_red_POC_POFe_2)/loc_dPO4
             else
                bio_part_red(is_POC,is_POFe,dum_i,dum_j) = 0.
             endif
          end if
       end if
       ! only siliceous phytoplanction is responsible for opal flux (Ridgwell et al., 2002, Global Biogeochem. Cycles)
       if (loc_dPO4 > const_real_nullsmall) then
          bio_part_red(is_POC,is_opal,dum_i,dum_j) = bio_part_red(is_POC,is_opal,dum_i,dum_j)*loc_dPO4_1/loc_dPO4
       end if
       ! only non-siliceous phytoplankton is responsible for CaCO3 flux (Ridgwell et al., 2002, Global Biogechem. Cycles)
       if (loc_dPO4 > const_real_nullsmall) then
          bio_part_red(is_POC,is_CaCO3,dum_i,dum_j) = bio_part_red(is_POC,is_CaCO3,dum_i,dum_j)*loc_dPO4_2/loc_dPO4
       end if
    CASE ( &
         & 'bio_POCflux' &
         & )
       ! prescribed POC flux
       ! NOTE: ignore sea-ice cover
       ! NOTE: force_restore_docn_nuts has a negative sign for POP formation ...
       if ((-force_restore_docn_nuts(io_PO4)) < ocn(io_PO4,dum_i,dum_j,n_k)) then
          loc_dPO4 = -force_restore_docn_nuts(io_PO4)
       else
          loc_dPO4 = 0.0
       end if
    CASE ( &
         & '2N2T_PO4MM_NO3' &
         & )
       ! 2 x nutrient, 2 x 'taxa': PO4, NO3 Michaelis-Menton
       ! calculate PO4 depletion; loc_dPO4_1 is non-Nfixer productivity, loc_dPO4_2 is N-fixer productivity
       ! (after Fennel et al., 2005)
       if ((ocn(io_PO4,dum_i,dum_j,n_k) > const_real_nullsmall) .and. &
            & (ocn(io_NO3,dum_i,dum_j,n_k) + ocn(io_NH4,dum_i,dum_j,n_k) > const_real_nullsmall)) then
          loc_dPO4_1 = &
               & dum_dt* &
               & loc_ficefree* &
               & loc_kI* &
               & min(loc_kPO4, &
               &     (ocn(io_NO3,dum_i,dum_j,n_k) + ocn(io_NH4,dum_i,dum_j,n_k))/ &
               &     (par_bio_c0_N + ocn(io_NO3,dum_i,dum_j,n_k) + ocn(io_NH4,dum_i,dum_j,n_k))) * &
               &     par_bio_mu1*ocn(io_PO4,dum_i,dum_j,n_k) 
          ! Need to add productivity from nitrogen fixation if conditions are right
          if ((ocn(io_NO3,dum_i,dum_j,n_k) + ocn(io_NH4,dum_i,dum_j,n_k) < par_bio_N2fixthresh) .and. &
               & ((ocn(io_NO3,dum_i,dum_j,n_k) + ocn(io_NH4,dum_i,dum_j,n_k))/ocn(io_PO4,dum_i,dum_j,n_k) & 
               & <  par_bio_red_POP_PON)) then 
             loc_dPO4_2 = &
                  & dum_dt* &
                  & loc_ficefree* &
                  & loc_kI* &
                  & par_bio_mu2*ocn(io_PO4,dum_i,dum_j,n_k)* &
                  & loc_kPO4
          else 
             loc_dPO4_2 = 0.0
          endif
       else
          loc_dPO4_1 = 0.0
          loc_dPO4_2 = 0.0
       end if
       ! calculate total production (= PO4 uptate)
       loc_dPO4 = loc_dPO4_1 + loc_dPO4_2
       ! calculate fraction of total production supported by N2 fixation
       if(loc_dPO4 > const_real_nullsmall) loc_frac_N2fix = loc_dPO4_2/loc_dPO4
    end select
    ! ############################################################################################################################ !

    ! *** CALCULATE ISOTOPIC FRACTIONATION ***
    ! NOTE: implement isotopic fraction as a 'Redfield' field ratio (populate array <bio_part_red>)
    ! NOTE: *** REMEMBER to convert r (13C/(13C + 12C)) ratio to R (13C/12C) before applying fractionation factor alpha **********
    !       *** and then convert back to little r again to create the isotopic 'Redfield ratio' **********************************
    ! NOTE: assume that the total mass of C is approximately equal to 12C + 13C
    ! NOTE: assume for 14C, the standard is already in the form: 14C/C
    ! NOTE: the array <carbisor> represents its isotopic ratio as little 'r'
    ! NOTE: T-dependent fractionation for calcite following Mook [1986]
    ! NOTE: CaCO3 fractionation w.r.t. HCO3-
    SELECT CASE (par_bio_prodopt)
    CASE (                        &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       if (loc_dPO4 > const_real_nullsmall) then
          loc_d13C_DIC_Corg_ef = (loc_dPO4_1*par_d13C_DIC_Corg_ef_sp + loc_dPO4_2*par_d13C_DIC_Corg_ef_nsp)/loc_dPO4
       else
          loc_d13C_DIC_Corg_ef = par_d13C_DIC_Corg_ef
       endif
    case default
       loc_d13C_DIC_Corg_ef = par_d13C_DIC_Corg_ef
    end select
    if (sed_select(is_POC_13C)) then
       ! calculate the 13C/12C fractionation between DIC and POC
       loc_Kq = const_d13C_DIC_Corg_Q2_c + const_d13C_DIC_Corg_Q2_x*ocn(io_T,dum_i,dum_j,n_k) + &
            & const_d13C_DIC_Corg_Q2_x2*ocn(io_T,dum_i,dum_j,n_k)**2
       loc_delta_Corg = -loc_d13C_DIC_Corg_ef + &
            & (loc_d13C_DIC_Corg_ef - const_d13C_DIC_Corg_ed)*loc_Kq/carb(ic_conc_CO2,dum_i,dum_j,n_k)
       loc_alpha = 1.0 + loc_delta_Corg/1000.0
       loc_R = carbisor(ici_CO2_r13C,dum_i,dum_j,n_k)/(1.0 - carbisor(ici_CO2_r13C,dum_i,dum_j,n_k))
       bio_part_red(is_POC,is_POC_13C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    if (sed_select(is_CaCO3_13C)) then
       ! calculate 13C/12C fractionation between DIC and CaCO3
       loc_delta_CaCO3 = 15.10 - 4232.0/ocn(io_T,dum_i,dum_j,n_k)
       loc_alpha = 1.0 + loc_delta_CaCO3/1000.0
       loc_R = carbisor(ici_HCO3_r13C,dum_i,dum_j,n_k)/(1.0 - carbisor(ici_HCO3_r13C,dum_i,dum_j,n_k))
       bio_part_red(is_CaCO3,is_CaCO3_13C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    if (sed_select(is_POC_14C)) then
       ! calculate the 14C/C fractionation between DIC and POC
       loc_alpha = 1.0 + 2.0*loc_delta_Corg/1000.0
       loc_R = carbisor(ici_CO2_r14C,dum_i,dum_j,n_k)/(1.0 - carbisor(ici_CO2_r14C,dum_i,dum_j,n_k))
       bio_part_red(is_POC,is_POC_14C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    if (sed_select(is_CaCO3_14C)) then
       ! calculate 14C/C fractionation between DIC and CaCO3
       loc_alpha = 1.0 + 2.0*loc_delta_CaCO3/1000.0
       loc_R = carbisor(ici_HCO3_r14C,dum_i,dum_j,n_k)/(1.0 - carbisor(ici_HCO3_r14C,dum_i,dum_j,n_k))
       bio_part_red(is_CaCO3,is_CaCO3_14C,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    if (sed_select(is_PON_15N)) then
       ! calculate the 15N/14N fractionation between NO3 and PON
       ! NOTE: ASSUME NO FRACTIONATION
       ! NOTE; check first for non-zero nitrate concentration to prevent potential unpleasantness ...
       if (ocn(io_NO3,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r15N = ocn(io_NO3_15N,dum_i,dum_j,n_k)/ocn(io_NO3,dum_i,dum_j,n_k)
       else
          loc_r15N = 0.0
       end if
!!$       loc_delta_Corg = -5.0
!!$       loc_alpha = 1.0 + loc_delta_Corg/1000.0
       loc_alpha = 0.0
       bio_part_red(is_PON,is_PON_15N,dum_i,dum_j) = loc_alpha*loc_r15N
    end if
    if (sed_select(is_opal_30Si)) then
       if (ocn(io_SiO2,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r30Si = ocn(io_SiO2_30Si,dum_i,dum_j,n_k)/ocn(io_SiO2,dum_i,dum_j,n_k)
       else
          loc_r30Si = 0.0
       endif
       ! Fractionation of 30Si during opal formation by diatoms
       loc_alpha = 1.0 + par_d30Si_opal_epsilon/1000.0
       loc_R = loc_r30Si/(1.0 - loc_r30Si)
       bio_part_red(is_opal,is_opal_30Si,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    if (sed_select(is_POCd_114Cd)) then
       ! calculate 114/???Cd fractionation between Cd and POCd
       ! ****************************
       ! d114Cd = 1.0006
       ! mean ocean d114Cd = 0.4 o/oo
       ! ****************************
       if (ocn(io_Cd,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r114Cd = ocn(io_Cd_114Cd,dum_i,dum_j,n_k)/ocn(io_Cd,dum_i,dum_j,n_k)
       else
          loc_r114Cd = 0.0
       end if
       loc_alpha = 1.0 + par_d114Cd_POCd_epsilon/1000.0
       loc_R = loc_r114Cd/(1.0 - loc_r114Cd)
       bio_part_red(is_POCd,is_POCd_114Cd,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if
    if (sed_select(is_LiCO3_7Li)) then
       ! calculate 7/6Li fractionation between Li and LiCO3
       if (ocn(io_Li,dum_i,dum_j,n_k) > const_real_nullsmall) then
          loc_r7Li = ocn(io_Li_7Li,dum_i,dum_j,n_k)/ocn(io_Li,dum_i,dum_j,n_k)
       else
          loc_r7Li = 0.0
       end if
       loc_alpha = 1.0 + par_d7Li_LiCO3_epsilon/1000.0
       loc_R = loc_r7Li/(1.0 - loc_r7Li)
       bio_part_red(is_LiCO3,is_LiCO3_7Li,dum_i,dum_j) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)
    end if

    ! ### INSERT CODE TO DEAL WITH ADDITIONAL ISOTOPES ########################################################################### !
    ! 
    ! ############################################################################################################################ !


    ! *** CALCULATE ORGANIC CARBON PRODUCTION ***
    ! calculate export in currency of particulate carbon (rather than PO4)
    ! NOTE: put everything into particulate form initially, but re-scale later to account for DOM export
    bio_part(is_POC,dum_i,dum_j,loc_k_mld:n_k) = bio_part_red(is_POP,is_POC,dum_i,dum_j)*loc_dPO4


    
    
    ! NOTE: scavenging is handled elsewhere
    
    
    ! *** CALCULATE ASSOCIATED BULK EXPORT ***
    ! calculate any associated 'Redfield' and isotopic tracer components
    ! NOTE: treat <is_POM_recfrac> as a special case, and set to default value (as set in <biogem_config.par>)
    ! NOTE: sed_select(is) == 1,2 are the bulk biogenic particulates (POC,PON,POP,POFe,PO2,CaCO3,opal)
    ! NOTE: sed_select(is) == 11,12,13,14 are the isotopic particulates (POC_13C,POC_14C,PON_15N,CaCO3_13C,CaCO3_14C,CaCO3_18O)
    !       that can be related directly to the relevant bulk particulate by the defined dependency == sed_dep(is)
    !       (see gem_config_sed.par)
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       select case (sed_type(is))
       case (par_sed_type_bio)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(is_POC,is,dum_i,dum_j)*bio_part(is_POC,dum_i,dum_j,loc_k_mld:n_k)
       end select
    end do

    ! *** CALCULATE ASSOCIATED TRACE ELEMENT EXPORT ***
    ! 
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       select case (sed_type(is))
       case (par_sed_type_POM)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(is_POC,is,dum_i,dum_j)*bio_part(is_POC,dum_i,dum_j,loc_k_mld:n_k)
       case (par_sed_type_CaCO3)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(is_CaCO3,is,dum_i,dum_j)*bio_part(is_CaCO3,dum_i,dum_j,loc_k_mld:n_k)
       case (par_sed_type_opal)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(is_opal,is,dum_i,dum_j)*bio_part(is_opal,dum_i,dum_j,loc_k_mld:n_k)
       end select
    end do

    ! *** CALCULATE ASSOCIATED ISOTOPIC EXPORT ***
    ! 
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       select case (sed_type(is))
       case (11:20)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & bio_part_red(sed_dep(is),is,dum_i,dum_j)*bio_part(sed_dep(is),dum_i,dum_j,loc_k_mld:n_k)
       end select
    end do




    ! *** CALCULATE INORGANIC UPDATE ***
    ! convert particulate sediment tracer indexed array concentrations to (dissolved) tracer indexed array
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_tot_i = conv_sed_ocn_i(0,is)
       do loc_i=1,loc_tot_i
          io = conv_sed_ocn_i(loc_i,is)
          loc_bio_uptake(io,loc_k_mld:n_k) = loc_bio_uptake(io,loc_k_mld:n_k) + &
               & conv_sed_ocn(io,is)*bio_part(is,dum_i,dum_j,loc_k_mld:n_k)
       end do
    end DO

    ! *** ADJUST DISSOLVED CONSTITUENT UPTAKE FOR NON-STANDARD PRODUCTIVITY ***
    ! the stoichiometries for productivity based on different forms of nitrogen are as follows:
    ! (1) using NH4: 106 mol O2 is produced for 106 mol C fixed, or a ratio of 1
    !     [implemented as 138 mol O2 released MINUS 2 x 16 mol NH4 (16 mol N is fixed for 106 mol C)]
    ! (2) using NO3: 138 mol O2 is produced for 106 mol C fixed, or a ratio of 138/106
    !     (this is the default ratio is embedded in conv_sed_ocn(io,is))
    ! (3) using N2: 118 mol O2  is produced for 106 mol C, or a ratio of 118/106
    !     [implemented as 138 mol O2 released MINUS 2.5 x 16/2 mol N2 (16 mol N is fixed for 106 mol C)]
    SELECT CASE (par_bio_prodopt)
    CASE ( &
         & '2N2T_PO4MM_NO3' &
         & )
       ! adjust default biological tracer uptake stoichiometry due to N2 fixation (replacing some NO3 consumption)
       loc_bio_uptake(io_N2,loc_k_mld:n_k) = loc_bio_uptake(io_N2,loc_k_mld:n_k) + &
            & 0.5*loc_frac_N2fix*loc_bio_uptake(io_NO3,loc_k_mld:n_k)
       loc_bio_uptake(io_O2,loc_k_mld:n_k) = loc_bio_uptake(io_O2,loc_k_mld:n_k) + &
            & 2.5*loc_bio_uptake(io_N2,loc_k_mld:n_k)
       loc_bio_uptake(io_ALK,loc_k_mld:n_k) = loc_bio_uptake(io_ALK,loc_k_mld:n_k) + &
            & 1.0*loc_frac_N2fix*loc_bio_uptake(io_NO3,loc_k_mld:n_k)
       loc_bio_uptake(io_NO3,loc_k_mld:n_k) = (1.0 - loc_frac_N2fix)*loc_bio_uptake(io_NO3,loc_k_mld:n_k)
       ! adjust default biological tracer uptake stoichiometry due to NH4 consumption (replacing some NO3 consumption)
       loc_bio_uptake(io_NH4,loc_k_mld:n_k) = loc_bio_uptake(io_NH4,loc_k_mld:n_k) +  &
            & (1.0 - loc_frac_N2fix)*loc_bio_uptake(io_NO3,loc_k_mld:n_k)* &
            & ocn(io_NH4,dum_i,dum_j,loc_k_mld:n_k)/(ocn(io_NH4,dum_i,dum_j,loc_k_mld:n_k) + ocn(io_NO3,dum_i,dum_j,loc_k_mld:n_k))
       loc_bio_uptake(io_O2,loc_k_mld:n_k) = loc_bio_uptake(io_O2,loc_k_mld:n_k) + &
            & 2.0*loc_bio_uptake(io_NH4,loc_k_mld:n_k)
       loc_bio_uptake(io_ALK,loc_k_mld:n_k) = loc_bio_uptake(io_ALK,loc_k_mld:n_k) + &
            & 2.0*loc_bio_uptake(io_NH4,loc_k_mld:n_k)
       loc_bio_uptake(io_NO3,loc_k_mld:n_k) = loc_bio_uptake(io_NO3,loc_k_mld:n_k) - &
            & loc_bio_uptake(io_NH4,loc_k_mld:n_k)
!!$       ! adjust for S phototrophy
!!$       ! Apparently ... the sulfur bacteria do this:  2H2O + H2S + 2CO2 --> SO4 + 2CH2O + 2H+
!!$       ! NOTE: O2 has to be removed because organic carbon fixation has already been calculated (above)
!!$       !       and furthermore has assumed to have occurred photosynthetically
!!$       !       => this O2 has to be removed so that productivity is transferred to being via the S phototrophy mechanism
!!$       if (ocn(io_H2S,dum_i,dum_j,n_k) > const_real_nullsmall) then
!!$          loc_bio_uptake(io_H2S) = 0.5*bio_part(is_POC,dum_i,dum_j,n_k)
!!$          if (loc_bio_uptake(io_H2S) > ocn(io_H2S,dum_i,dum_j,n_k)) then
!!$             loc_bio_uptake(io_H2S) = ocn(io_H2S,dum_i,dum_j,n_k)
!!$          end if
!!$          loc_bio_uptake(io_SO4) = -loc_bio_uptake(io_H2S)
!!$          loc_bio_uptake(io_ALK) = loc_bio_uptake(io_ALK) + 2.0*loc_bio_uptake(io_H2S)
!!$          loc_bio_uptake(io_O2)  = loc_bio_uptake(io_O2)  + 2.0*loc_bio_uptake(io_H2S)
!!$       end if
    END select

    ! *** RE-SCALE FOR DISSOLVED ORGANIC MATTER PRODUCTION ***
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_tot_i = conv_POM_DOM_i(0,is)
       do loc_i=1,loc_tot_i
          io = conv_POM_DOM_i(loc_i,is)
          bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) = bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) + &
               & loc_bio_red_DOMfrac*bio_part(is,dum_i,dum_j,loc_k_mld:n_k)
          bio_part(is,dum_i,dum_j,loc_k_mld:n_k) = &
               & (1.0 - loc_bio_red_DOMfrac)*bio_part(is,dum_i,dum_j,loc_k_mld:n_k)
       end do
    end do

    ! *** INITIAL PARTICULATE FRACTION PARTITIONING ***
    ! set partitioning between differently remineralized particulate fluxes
    ! NOTE: this code should ideally be replaced by a generic algorithm
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    if (ctrl_bio_remin_POC_ballast) then
       DO k=n_k,loc_k_mld,-1
          if (bio_part(is_POC,dum_i,dum_j,k) > const_real_nullsmall) then
             bio_part(is_POC_frac2,dum_i,dum_j,k) =                               &
                  & (                                                             &
                  &   par_bio_remin_ballast_kc*bio_part(is_CaCO3,dum_i,dum_j,k) + &
                  &   par_bio_remin_ballast_ko*bio_part(is_opal,dum_i,dum_j,k) +  &
                  &   par_bio_remin_ballast_kl*bio_part(is_det,dum_i,dum_j,k)     &
                  & )                                                             &
                  & /bio_part(is_POC,dum_i,dum_j,k)
          else
             bio_part(is_POC_frac2,dum_i,dum_j,k) = 0.0
          end if
          if (bio_part(is_POC_frac2,dum_i,dum_j,k) > 1.0) bio_part(is_POC_frac2,dum_i,dum_j,k) = 1.0
       end DO
    else
       bio_part(is_POC_frac2,dum_i,dum_j,loc_k_mld:n_k) = par_bio_remin_POC_frac2
    end if
    bio_part(is_CaCO3_frac2,dum_i,dum_j,loc_k_mld:n_k) = par_bio_remin_CaCO3_frac2
    bio_part(is_opal_frac2,dum_i,dum_j,loc_k_mld:n_k)  = par_bio_remin_opal_frac2
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ! *** DIRECTLY CREATE PLANTON TRACERS ***
    if (sed_select(is_CaCO3) .AND. sed_select(is_foram_p_13C)) then
       ! calculate 13C/12C fractionation between DIC and CaCO3
       loc_delta_CaCO3 = 0.0
       loc_alpha = 1.0 + loc_delta_CaCO3/1000.0
       loc_R = carbisor(ici_HCO3_r13C,dum_i,dum_j,n_k)/(1.0 - carbisor(ici_HCO3_r13C,dum_i,dum_j,n_k))
       bio_part(is_foram_p_13C,dum_i,dum_j,loc_k_mld:n_k) = &
            & (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*bio_part(is_CaCO3,dum_i,dum_j,loc_k_mld:n_k)
    end if

    ! *** Fe SCAVENGING ***
    ! calculate scavenging of Fe from water column by newly formed particulates
    if (ocn_select(io_Fe)) then
       DO k=n_k,loc_k_mld,-1
          if (ocn(io_Fe,dum_i,dum_j,k) > const_real_nullsmall) then
             call sub_calc_scav_Fe( &
                  & dum_dt, &
                  & ocn(io_Fe,dum_i,dum_j,k), &
                  & bio_part(:,dum_i,dum_j,k), &
                  & bio_remin(:,dum_i,dum_j,k) &
                  & )
          end if
       end DO
    end if

    ! *** WRITE DATA ***
    ! set modification of tracer concentrations
    ! NOTE: depletion of dissolved species as a result of biological productivity is implimented as negative remineralization
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) = bio_remin(io,dum_i,dum_j,loc_k_mld:n_k) - loc_bio_uptake(io,loc_k_mld:n_k)
    end do
    ! record diagnostics
    ! NOTE: scale productivity modifiers by tiem-step to get correct average
    ! ### NOTE ################################################################################################################### !
    ! need to adjust (units?) diagnostics consistent with MLD changes(?)
    ! ############################################################################################################################ !
    diag_bio(idiag_bio_dPO4,dum_i,dum_j) = loc_dPO4
    SELECT CASE (par_bio_prodopt)
    CASE ( &
         & '2N2T_PO4MM_NO3' &
         & )
       diag_bio(idiag_bio_dPO4_1,dum_i,dum_j)     = loc_dPO4_1
       diag_bio(idiag_bio_dPO4_2,dum_i,dum_j)     = loc_dPO4_2
       diag_bio(idiag_bio_N2fixation,dum_i,dum_j) = loc_bio_uptake(io_N2,n_k)
       diag_bio(idiag_bio_NH4assim,dum_i,dum_j)   = loc_bio_uptake(io_NH4,n_k)
    case (                  &
         & 'bio_PFe',       &
         & 'bio_PFe_OCMIP2' &
         & )
       diag_bio(idiag_bio_kT,dum_i,dum_j)      = dum_dt*loc_kT
       diag_bio(idiag_bio_kI,dum_i,dum_j)      = dum_dt*loc_kI
       diag_bio(idiag_bio_kN,dum_i,dum_j)      = dum_dt*min(loc_kPO4,loc_kFe)
       diag_bio(idiag_bio_DOMfrac,dum_i,dum_j) = dum_dt*loc_bio_red_DOMfrac
    case (                        &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       diag_bio(idiag_bio_kT,dum_i,dum_j)      = dum_dt*loc_kT
       diag_bio(idiag_bio_kI,dum_i,dum_j)      = dum_dt*loc_kI
       diag_bio(idiag_bio_kN,dum_i,dum_j)      = dum_dt*min(loc_kPO4,loc_kFe)
       diag_bio(idiag_bio_dPO4_1,dum_i,dum_j)     = loc_dPO4_1
       diag_bio(idiag_bio_dPO4_2,dum_i,dum_j)     = loc_dPO4_2
       diag_bio(idiag_bio_DOMfrac,dum_i,dum_j) = dum_dt*loc_bio_red_DOMfrac
    end SELECT

  end SUBROUTINE sub_calc_bio_uptake
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE ABIOTIC TRACER UPTAKE AT THE SURFACE OCEAN
  SUBROUTINE sub_calc_bio_uptake_abio(dum_i,dum_j,dum_k1,dum_dt)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dt
    ! local variables
    INTEGER::k,l,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_ocn,n_k)::loc_bio_uptake
    real,dimension(n_sed,n_k)::loc_bio_part
    real::loc_delta_CaCO3
    real::loc_alpha
    real::loc_R
    integer::loc_kmax

    ! *** INITIALIZE VARIABLES ***
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_uptake(io,:) = 0.0
    end do
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       loc_bio_uptake(is,:) = 0.0
    end DO
    ! restrict abiotic precipitation to the surface if requested
    if (ctrl_bio_CaCO3precip_sur) then
       loc_kmax = n_k
    else
       loc_kmax = dum_k1
    end if
    
    ! *** CALCULATE CaCO3 PRECIPITATION ***
    DO k=n_k,dum_k1,-1
       ! re-calculate carbonate dissociation constants
       CALL sub_calc_carbconst(                 &
            & phys_ocn(ipo_Dmid,dum_i,dum_j,k), &
            & ocn(io_T,dum_i,dum_j,k),          &
            & ocn(io_S,dum_i,dum_j,k),          &
            & carbconst(:,dum_i,dum_j,k)        &
            & )
       ! adjust carbonate constants
       if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
          call sub_adj_carbconst(           &
               & ocn(io_Ca,dum_i,dum_j,k),  &
               & ocn(io_Mg,dum_i,dum_j,k),  &
               & carbconst(:,dum_i,dum_j,k) &
               & )
       end if
       ! re-estimate Ca and borate concentrations from salinity (if not selected and therefore explicitly treated)
       IF (.NOT. ocn_select(io_Ca))  ocn(io_Ca,dum_i,dum_j,k)  = fun_calc_Ca(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_B))   ocn(io_B,dum_i,dum_j,k)   = fun_calc_Btot(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_SO4)) ocn(io_SO4,dum_i,dum_j,k) = fun_calc_SO4tot(ocn(io_S,dum_i,dum_j,k))
       IF (.NOT. ocn_select(io_F))   ocn(io_F,dum_i,dum_j,k)   = fun_calc_Ftot(ocn(io_S,dum_i,dum_j,k))
       ! re-calculate surface ocean carbonate chemistry
       CALL sub_calc_carb(             &
            & ocn(io_DIC,dum_i,dum_j,k),  &
            & ocn(io_ALK,dum_i,dum_j,k),  &
            & ocn(io_Ca,dum_i,dum_j,k),   &
            & ocn(io_PO4,dum_i,dum_j,k),  &
            & ocn(io_SiO2,dum_i,dum_j,k), &
            & ocn(io_B,dum_i,dum_j,k),    &
            & ocn(io_SO4,dum_i,dum_j,k),  &
            & ocn(io_F,dum_i,dum_j,k),    &
            & ocn(io_H2S,dum_i,dum_j,k),  &
            & ocn(io_NH4,dum_i,dum_j,k),  &
            & carbconst(:,dum_i,dum_j,k), & 
            & carb(:,dum_i,dum_j,k),      &  
            & carbalk(:,dum_i,dum_j,k)    & 
            & )
       if (carb(ic_ohm_cal,dum_i,dum_j,k) > 1.0) then
          loc_bio_part(is_CaCO3,k) = dum_dt*par_bio_CaCO3precip_sf*(carb(ic_ohm_cal,dum_i,dum_j,k) - 1.0)**par_bio_CaCO3precip_exp
       else
          loc_bio_part(is_CaCO3,k) = 0.0
       end if
       if (sed_select(is_CaCO3_13C)) then
          ! re-calculate carbonate system isotopic properties
          if (ocn_select(io_DIC_13C)) then
             call sub_calc_carb_r13C(           &
                  & ocn(io_T,dum_i,dum_j,k),       &
                  & ocn(io_DIC,dum_i,dum_j,k),     &
                  & ocn(io_DIC_13C,dum_i,dum_j,k), &
                  & carb(:,dum_i,dum_j,k),         &
                  & carbisor(:,dum_i,dum_j,k)      &
                  & )
          end IF
          ! calculate 13C/12C fractionation between DIC and CaCO3
          ! NOTE: T-dependent fractionation for calcite following Mook [1986]
          ! NOTE: CaCO3 fractionation w.r.t. HCO3-
          loc_delta_CaCO3 = 15.10 - 4232.0/ocn(io_T,dum_i,dum_j,k)
          loc_alpha = 1.0 + loc_delta_CaCO3/1000.0
          loc_R = carbisor(ici_HCO3_r13C,dum_i,dum_j,k)/(1.0 - carbisor(ici_HCO3_r13C,dum_i,dum_j,k))
          loc_bio_part(is_CaCO3_13C,k) = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_bio_part(is_CaCO3,k)        
       end if
       ! convert particulate sediment tracer indexed array concentrations to (dissolved) tracer indexed array
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_tot_i = conv_sed_ocn_i(0,is)
          do loc_i=1,loc_tot_i
             io = conv_sed_ocn_i(loc_i,is)
             loc_bio_uptake(io,k) = loc_bio_uptake(io,k) + conv_sed_ocn(io,is)*loc_bio_part(is,k)
          end do
       end DO
    end DO
    
    ! *** SET MODIFICATION OF TRACER CONCENTRATIONS ***
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) - loc_bio_uptake(io,:)
    end do

    ! *** SET MODIFICATION OF PARTICULATE CONCENTRATIONS ***
    DO l=3,n_l_sed
       is = conv_iselected_is(l)
       bio_part(is,dum_i,dum_j,:) = bio_part(is,dum_i,dum_j,:) + loc_bio_part(is,:)
    end DO

  end SUBROUTINE sub_calc_bio_uptake_abio
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Fe SPECIATION
  SUBROUTINE sub_calc_geochem_Fe(dum_i,dum_j,dum_k1,dum_focnFe)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,dimension(n_k),INTENT(in)::dum_focnFe
    ! local variables
    INTEGER::k
    real::loc_Fe,loc_FeL,loc_L
    real::loc_FeT,loc_LT
    real,DIMENSION(2)::loc_roots

    ! *** CALCULATE Fe SPECIATION THROUGHOUT THE WATER COLUMN***
    DO k=n_k,dum_k1,-1
       ! initialize variables
       loc_Fe  = ocn(io_Fe,dum_i,dum_j,k) + bio_remin(io_Fe,dum_i,dum_j,k) + dum_focnFe(k)
       loc_FeL = ocn(io_FeL,dum_i,dum_j,k) + bio_remin(io_FeL,dum_i,dum_j,k)
       loc_L   = ocn(io_L,dum_i,dum_j,k) + bio_remin(io_L,dum_i,dum_j,k)
       loc_FeT = loc_FeL + loc_Fe
       loc_LT  = loc_FeL + loc_L
       ! solve Fe speciation equation:
       ! K = FeL / (Fe*L) (e.g. see: Parekth et al. [2005])
       ! => FeL = Fe*L*K
       !    conservation relations:
       !    FeL + Fe = FeT => Fe = FeT - FeL
       !    FeL + L  = LT  => L  = LT  - FeL
       !    substitute:
       !    FeL = (FeT - FeL)*(LT - FeL)*K
       !    => FeL/K = FeT*LT + FeL^2 - LT*FeL - FeT*FeL
       !    => FeL/K = FeL^2 - (LT + FeT)*FeL + FeT*LT
       !    => 1.0*FeL^2 - (LT + FeT + 1.0/K)*FeL + FeT*LT = 0.0
       !       solve as: ax2 + bx + c = 0.0
       !                 where x = FeL
       loc_roots(:) = fun_quad_root(1.0,-(loc_LT + loc_FeT + 1.0/par_K_FeL),loc_FeT*loc_LT)
       ! filter returned roots
       if (maxval(loc_roots(:)) < const_real_nullsmall) then
          IF (ctrl_audit) THEN
             CALL sub_report_error( &
                  & 'biogem_box.f90','sub_calc_geochem_Fe', &
                  & 'No REAL root in Fe speciation calculation (or maybe zero ...).'// &
                  & ' / Data: dum_i,dum_j,k,loc_FeL(OLD),loc_Fe(OLD),loc_L(OLD),loc_FeT(OLD),loc_LT(OLD),', &
                  & 'SOD THIS FOR A GAME OF SOLDIERS: calculation abondoned ...', &
                  & (/real(dum_i),real(dum_j),real(k),loc_FeL,loc_Fe,loc_L,loc_FeT,loc_LT/),.false. &
                  & )
             error_stop = .FALSE.
          end IF
       elseif ((minval(loc_roots(:)) > loc_FeT) .AND. (minval(loc_roots(:)) > loc_LT)) then
          IF (ctrl_audit) THEN
             CALL sub_report_error( &
                  & 'biogem_box.f90','sub_calc_geochem_Fe', &
                  & 'No solution to Fe speciation calculation possible ... :('// &
                  & ' / Data: dum_i,dum_j,k,loc_FeL(OLD),loc_Fe(OLD),loc_L(OLD),loc_FeT(OLD),loc_LT(OLD),', &
                  & 'SOD THIS FOR A GAME OF SOLDIERS: calculation abondoned ...', &
                  & (/real(dum_i),real(dum_j),real(k),loc_FeL,loc_Fe,loc_L,loc_FeT,loc_LT/),.false. &
                  & )
             error_stop = .FALSE.
          end IF
       else
          if (minval(loc_roots(:)) < const_real_nullsmall) then
             loc_FeL = maxval(loc_roots(:))
          else
             loc_FeL = minval(loc_roots(:))
          end if
          loc_Fe  = loc_FeT - loc_FeL
          loc_L   = loc_LT - loc_FeL
       end if
       ! re-calculate reminerlization arrays to give rise to calculated Fe speciation
       ! NOTE: subtract <dum_focnFe> again because it is added subsequently in the main BIOGEM loop through <locijk_focn>
       bio_remin(io_Fe,dum_i,dum_j,k)  = loc_Fe - ocn(io_Fe,dum_i,dum_j,k) - dum_focnFe(k)
       bio_remin(io_FeL,dum_i,dum_j,k) = loc_FeL - ocn(io_FeL,dum_i,dum_j,k)
       bio_remin(io_L,dum_i,dum_j,k)   = loc_L - ocn(io_L,dum_i,dum_j,k)
    end DO

  end SUBROUTINE sub_calc_geochem_Fe
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! OXIDATION OF HYDROGEN SULPHIDE
  SUBROUTINE sub_calc_geochem_oxidize_H2S(dum_i,dum_j,dum_k1,dum_dtyr)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! local variables
    integer::l,io,k
    real::loc_potO2cap,loc_r34S
    real::loc_H2S_oxidation_const,loc_H2S_oxidation
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    ! change units of H2S oxidation constant from mM-2 hr-1 to M-2 yr-1
    ! and convert from O2 consumption units to H2S units (i.e., divide by 2)
    loc_H2S_oxidation_const = 0.5*const_oxidation_coeff_H2S/conv_hr_yr/(conv_mmol_mol)**2
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** OXIDIZE H2S ***
    ! look for some H2S and see if it can be instantaneously oxidized (using O2; if there is any!)
    ! H2S + 2O2 -> SO4 + 2H
    DO k=n_k,dum_k1,-1
       ! calculate potential oxidation capacity
       loc_potO2cap = ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k)
       if ((ocn(io_H2S,dum_i,dum_j,k) > const_real_nullsmall) .AND. (loc_potO2cap > const_real_nullsmall)) then
          ! calculate H2S oxidation, and cap value at H2S concentration if necessary
          loc_H2S_oxidation = dum_dtyr*loc_H2S_oxidation_const*ocn(io_H2S,dum_i,dum_j,k)*loc_potO2cap**2
          if (loc_H2S_oxidation > ocn(io_H2S,dum_i,dum_j,k)) then
             loc_H2S_oxidation = ocn(io_H2S,dum_i,dum_j,k)
          end if
          ! calculate isotopic ratio
          loc_r34S = ocn(io_H2S_34S,dum_i,dum_j,k)/ocn(io_H2S,dum_i,dum_j,k)
          if (loc_H2S_oxidation <= 0.5*loc_potO2cap) then
             ! complete H2S oxidation (no S fractionation)
             loc_bio_remin(io_H2S,k) = -loc_H2S_oxidation
             loc_bio_remin(io_SO4,k) = loc_H2S_oxidation
             loc_bio_remin(io_O2,k)  = -2.0*loc_H2S_oxidation
             loc_bio_remin(io_ALK,k) = -2.0*loc_H2S_oxidation
             loc_bio_remin(io_H2S_34S,k) = -loc_r34S*loc_H2S_oxidation
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*loc_H2S_oxidation
          else
             ! partial H2S oxidation (=> S isotope Rayleigh fractionation)
             loc_bio_remin(io_H2S,k) = -0.5*loc_potO2cap
             loc_bio_remin(io_SO4,k) = 0.5*loc_potO2cap
             loc_bio_remin(io_O2,k)  = -loc_potO2cap
             loc_bio_remin(io_ALK,k) = -loc_potO2cap
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO S FRACTIONATION ########################################################## !
             loc_bio_remin(io_H2S_34S,k) = -loc_r34S*0.5*loc_potO2cap
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*0.5*loc_potO2cap
             ! ################################################################################################################### !
          end if
       end if
    end DO

    ! *** WRITE GLOBAL ARRAY DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do

  end SUBROUTINE sub_calc_geochem_oxidize_H2S
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE THE OXIDATION OF HN4
  SUBROUTINE sub_calc_bio_remin_oxidize_NH4(dum_i,dum_j,dum_k1,dum_dtyr)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! local variables
    integer::l,io,k
    real::loc_potO2cap
    real::loc_NH4_oxidation_const,loc_NH4_oxidation,loc_NH4_halfsat
    real::loc_r15N
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    loc_NH4_oxidation_const = 0.16666666 !(yr-1; Fennel et al, 2005)
    loc_NH4_halfsat = 2.0E-05 !(mol kg-1; Fennel et al., 2005)

    ! *** OXIDIZE NH4 ***
    ! look for some NH4 and see if it can be instantaneously oxidized (using O2; if there is any!)
    ! NH4+ + 2O2 -> NO3- + 2H+ + H2O
    DO k=n_k,dum_k1,-1
       ! calculate potential oxidation capacity
       loc_potO2cap = ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k)
       if ((ocn(io_NH4,dum_i,dum_j,k) > const_real_nullsmall) .AND. (loc_potO2cap > const_real_nullsmall)) then
          ! calculate NH4 oxidation, and cap value at NH4 concentration if necessary
          loc_NH4_oxidation = dum_dtyr*loc_NH4_oxidation_const*ocn(io_NH4,dum_i,dum_j,k)*loc_potO2cap / &
               & (loc_NH4_halfsat + loc_potO2cap)
          If (loc_NH4_oxidation > ocn(io_NH4,dum_i,dum_j,k)) loc_NH4_oxidation = ocn(io_NH4,dum_i,dum_j,k)
          ! calculate isotopic ratio
          loc_r15N = ocn(io_NH4_15N,dum_i,dum_j,k)/ocn(io_NH4,dum_i,dum_j,k)
          if (loc_NH4_oxidation <= 0.5*loc_potO2cap) then
             ! complete NH4 oxidation (no N fractionation)
             loc_bio_remin(io_NH4,k) = -loc_NH4_oxidation
             loc_bio_remin(io_NO3,k) = loc_NH4_oxidation
             loc_bio_remin(io_O2,k)  = -2.0*loc_NH4_oxidation
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k)
             loc_bio_remin(io_NH4_15N,k) = -loc_r15N*loc_NH4_oxidation
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_NH4_oxidation
          else
             ! partial NH4 oxidation (=> S isotope Rayleigh fractionation)
             loc_bio_remin(io_NH4,k) = -0.5*loc_potO2cap
             loc_bio_remin(io_NO3,k) = 0.5*loc_potO2cap
             loc_bio_remin(io_O2,k)  = -loc_potO2cap
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO N FRACTIONATION ########################################################## !
             loc_bio_remin(io_NH4_15N,k) = -loc_r15N*0.5*loc_potO2cap
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*0.5*loc_potO2cap
             ! ################################################################################################################### !
          end if
       end if
    end DO

    ! *** WRITE DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! record diagnostics (mol kg-1)
    diag_geochem(idiag_geochem_ammox_dNH4,dum_i,dum_j,:) = loc_bio_remin(io_NH4,:)
    diag_geochem(idiag_geochem_ammox_dNO3,dum_i,dum_j,:) = loc_bio_remin(io_NO3,:)

  end SUBROUTINE sub_calc_bio_remin_oxidize_NH4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WATER COLUMN REMINERALIZATION OF METHANE
  SUBROUTINE sub_calc_bio_remin_oxidize_CH4(dum_i,dum_j,dum_k1,dum_dtyr)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! local variables
    integer::l,io,k
    real::loc_potO2cap
    real::loc_CH4
    real::loc_r13C,loc_r14C
    real::loc_frac
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** OXIDIZE CH4 ***
    ! look for some CH4 and see if it can be oxidized
    ! allow different rate constant depending on availability of O2 or not
    ! CH4 + 2O2 -> CO2 + 2H2O
    DO k=n_k,dum_k1,-1
       ! calculate potential oxidation capacity
       loc_potO2cap = ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k)
       if ((ocn(io_CH4,dum_i,dum_j,k) > const_real_nullsmall) .AND. (loc_potO2cap > const_real_nullsmall)) then
          ! calculate CH4 oxidation
          ! NOTE: units of par_bio_remin_CH4rate == per year
          loc_frac = dum_dtyr*par_bio_remin_CH4rate
          if (loc_frac > 1.0) loc_frac = 1.0
          loc_CH4 = loc_frac*ocn(io_CH4,dum_i,dum_j,k)
          ! calculate isotopic ratio
          loc_r13C = ocn(io_CH4_13C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          loc_r14C = ocn(io_CH4_14C,dum_i,dum_j,k)/ocn(io_CH4,dum_i,dum_j,k)
          if (loc_CH4 <= 0.5*loc_potO2cap) then
             ! complete CH4 oxidation (no C fractionation)
             loc_bio_remin(io_CH4,k) = -loc_CH4
             loc_bio_remin(io_DIC,k) = loc_CH4
             loc_bio_remin(io_O2,k)  = -2.0*loc_CH4
             loc_bio_remin(io_CH4_13C,k) = -loc_r13C*loc_CH4
             loc_bio_remin(io_CH4_14C,k) = -loc_r14C*loc_CH4
             loc_bio_remin(io_DIC_13C,k) = loc_r13C*loc_CH4
             loc_bio_remin(io_DIC_14C,k) = loc_r14C*loc_CH4
          else
             ! partial CH4 oxidation (=> C isotope Rayleigh fractionation)
             loc_bio_remin(io_CH4,k) = -0.5*loc_potO2cap
             loc_bio_remin(io_DIC,k) = 0.5*loc_potO2cap
             loc_bio_remin(io_O2,k)  = -loc_potO2cap
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO C FRACTIONATION ########################################################## !
             loc_bio_remin(io_CH4_13C,k) = -loc_r13C*0.5*loc_potO2cap
             loc_bio_remin(io_CH4_14C,k) = -loc_r14C*0.5*loc_potO2cap
             loc_bio_remin(io_DIC_13C,k) = loc_r13C*0.5*loc_potO2cap
             loc_bio_remin(io_DIC_14C,k) = loc_r14C*0.5*loc_potO2cap
             ! ################################################################################################################### !
          end if
       end if
    end DO

    ! *** WRITE DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! record diagnostics (mol kg-1)
    diag_geochem(idiag_geochem_dCH4,dum_i,dum_j,:) = loc_bio_remin(io_CH4,:)

  end SUBROUTINE sub_calc_bio_remin_oxidize_CH4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! WATER COLUMN REMINSERALTZAION OF DISSOLVED ORGANIC MATTER
  SUBROUTINE sub_calc_bio_remin_DOM(dum_i,dum_j,dum_k1,dum_dtyr)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1                             ! 
    real,intent(in)::dum_dtyr                                          ! 
    ! local variables                                                  ! 
    integer::l,io,is,k                                                 ! 
    integer::loc_i,loc_tot_i                                           ! 
    real::loc_potO2cap,loc_O2demand,loc_bio_remin_DOMratio             ! 
    real,dimension(n_ocn,n_k)::loc_bio_remin                           ! 
    real,dimension(n_sed,n_k)::loc_bio_part                            ! 
    real::loc_DOMlifetime                                              ! 

    ! *** INITIALIZE VARIABLES ***
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do
    ! initialize particulate tracer arrays
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_bio_part(is,:) = 0.0
    end do

    ! *** REMINERALIZE DISSOLVED ORGANIC MATTER ***
    ! NOTE: the new algorithm converts the fraction of DOM marked to be remineralized first into POM before applying the
    !       'usual' generic conversion of sed -> ocn tracers, so as to avoid the need for 'special cases'
    !       (such as of the link between DON and ALK, or POP and ALK)
    DO k=n_k,dum_k1,-1
       ! calculate DOm lifetime modifiers
       SELECT CASE (par_bio_prodopt)
       case (                        &
            & 'bio_PFe',             &
            & 'bio_PFeSi',           &
            & 'bio_PFeSi_Ridgwell02' &
            & )
!!$          loc_DOMlifetime = par_bio_remin_DOMlifetime/(0.59*exp(0.0633*(ocn(io_T,dum_i,dum_j,k) - const_zeroC)))
          loc_DOMlifetime = par_bio_remin_DOMlifetime
       case default
          loc_DOMlifetime = par_bio_remin_DOMlifetime
       end SELECT
       ! calculate potential oxidation capacity
       loc_potO2cap = fun_potO2cap(ocn_select(:),ocn(:,dum_i,dum_j,k),bio_remin(:,dum_i,dum_j,k))
       If (ocn(io_DOM_C,dum_i,dum_j,k) > const_real_nullsmall) then
          ! check that the DOM lifetime is no shorter than the time-step and modify fraction remineralized accordingly
          if (loc_DOMlifetime > dum_dtyr) then
             loc_bio_remin_DOMratio = dum_dtyr/loc_DOMlifetime
          else
             loc_bio_remin_DOMratio = 1.0
          end if
          ! calculate potential oxidation capacity
          loc_O2demand = conv_sed_ocn(io_O2,is_POC)*conv_DOM_POM(is_POC,io_DOM_C)* &
               & loc_bio_remin_DOMratio*ocn(io_DOM_C,dum_i,dum_j,k)
          ! compare with potential oxygen availability and modify fraction remineralized accordingly
          if ((loc_O2demand > loc_potO2cap) .AND. (loc_O2demand > const_real_nullsmall)) then
             loc_bio_remin_DOMratio = (loc_potO2cap/loc_O2demand)*loc_bio_remin_DOMratio         
          end if
          ! remineralize dissolved organic matter and add released dissolved inorganic tracers to local remin array
          DO l=3,n_l_ocn
             io = conv_iselected_io(l)
             loc_tot_i = conv_DOM_POM_i(0,io)
             do loc_i=1,loc_tot_i
                is = conv_DOM_POM_i(loc_i,io)
                loc_bio_part(is,k)  = loc_bio_remin_DOMratio*conv_DOM_POM(is,io)*ocn(io,dum_i,dum_j,k)
                loc_bio_remin(io,k) = -loc_bio_part(is,k)
             end do
          end do
          DO l=1,n_l_sed
             is = conv_iselected_is(l)
             loc_tot_i = conv_sed_ocn_i(0,is)
             do loc_i=1,loc_tot_i
                io = conv_sed_ocn_i(loc_i,is)
                loc_bio_remin(io,k) = loc_bio_remin(io,k) + conv_sed_ocn(io,is)*loc_bio_part(is,k)
             end do
          end DO
       end if

    end DO

    ! *** WRITE GLOBAL ARRAY DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do

  end SUBROUTINE sub_calc_bio_remin_DOM
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE WATER COLUMN TRANSFORMATIONS INVOLVING (MOSTLY BIOGENIC) PARTICULATE MATTER
  ! NOTE: also include scavenging in this subroutine
  SUBROUTINE sub_calc_bio_remin(dum_i,dum_j,dum_k1,dum_dtyr)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    real,intent(in)::dum_dtyr
    ! local variables
    integer::l,io,is
    INTEGER::k,kk,loc_bio_remin_min_k
    integer::loc_i,loc_tot_i
    real::loc_potO2cap,loc_O2demand
    real::loc_T,loc_SiO2                                               ! 
    real::loc_Si_eq,loc_u                                              ! 
    real::loc_bio_remin_dD
    real::loc_bio_remin_max_D
    real::loc_bio_remin_layerratio
    real::loc_bio_remin_sinkingrate                                    ! 
    real::loc_bio_remin_dt                                             ! layer residence time (in years)
    real::loc_bio_remin_POC_frac1,loc_bio_remin_POC_frac2
    real::loc_bio_part_POC_ratio
    real::loc_bio_remin_CaCO3_frac1,loc_bio_remin_CaCO3_frac2
    real::loc_bio_part_CaCO3_ratio
    real::loc_bio_remin_opal_frac1,loc_bio_remin_opal_frac2
    real::loc_bio_part_opal_ratio
    real,dimension(n_sed,n_k)::loc_bio_part_TMP
    real,dimension(n_sed,n_k)::loc_bio_part_OLD
    real,dimension(n_sed,n_k)::loc_bio_part
    real,dimension(n_ocn,n_k)::loc_bio_remin
    real,dimension(n_sed,n_k)::loc_bio_settle
    real::loc_scav_sinkingvel

    ! ### USER-DEFINABLE OPTIONS ################################################################################################# !
    ! NOTE: settings not included in the run-time configuration files for clarity
    par_bio_remin_opal_K = 0.019/conv_d_yr ! opal particulate base dissolution rate (d-1 -> yr-1) [Ridgwell, 2001]
    ! ############################################################################################################################ !

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    loc_bio_remin_sinkingrate = par_bio_remin_sinkingrate
    ! initialize particulate tracer arrays
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       ! copy particulate tracer field to temporary array and reset value of particulate tracer field
       loc_bio_part_OLD(is,:) = bio_part(is,dum_i,dum_j,:)
       bio_part(is,dum_i,dum_j,:)   = 0.0
       ! initialize local particulate tracer field and settling flux arrays
       loc_bio_part(is,:) = 0.0
       loc_bio_settle(is,:) = 0.0
    end do
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! *** k WATER-COLUMN LOOP START ***
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    DO k=n_k,dum_k1,-1
       ! find some particulates in the water column
       If ( &
            & ( &
            &   loc_bio_part_OLD(is_POC,k) + loc_bio_part_OLD(is_CaCO3,k) + &
            &   loc_bio_part_OLD(is_opal,k) + loc_bio_part_OLD(is_det,k) &
            & ) > const_real_nullsmall) then
          ! if the identified particulate material is already residing in the bottom-most ocean layer, flag as sediment flux
          If (k == dum_k1) then
             loc_bio_remin_min_k = dum_k1 - 1
          else
             ! determine the deepest layer that sinking material can reach within the current time-step
             ! NOTE: do this regardless of whether a fixed remineralization profile is selected, or whether 
             !       remineralization is calculated as a function of residence time in an ocena layer
             !       (and ambient environmental conditions)
             ! NOTE: trap the situation where the depth of the sediment surface is surpassed
             ! NOTE: start loop from the layer lying below the one in which the identified particulate material resides
             loc_bio_remin_min_k = dum_k1 - 1
             loc_bio_remin_max_D = phys_ocn(ipo_Dbot,dum_i,dum_j,k) + dum_dtyr*loc_bio_remin_sinkingrate
             do kk=k-1,dum_k1,-1
                If (phys_ocn(ipo_Dbot,dum_i,dum_j,kk) > loc_bio_remin_max_D) then
                   loc_bio_remin_min_k = kk
                   exit
                end if
             end do
          end if

          ! zero local (temporary) particulate field array, and seed value at location in water column identified
          DO l=1,n_l_sed
             is = conv_iselected_is(l)
             loc_bio_part_TMP(is,:) = 0.0
             loc_bio_part_TMP(is,k) = loc_bio_part_OLD(is,k)
          end do

          ! >>>>>>>>>>>>>>>>>>>>>>>>>
          ! *** kk SUB-LOOP START ***
          ! >>>>>>>>>>>>>>>>>>>>>>>>>

          ! for each of the three (POC, CaCO3, and opal) primary remineralizable species (if selected),
          ! loop down remineralization column identified previously;
          ! (1) calculating the fractional remineralization in each layer, moving the particulate remainder to the layer below
          ! (2) calculate tracer remineralization from particulate supply from layer above
          ! (3) update particulate tracer field for current layer
          ! then, if the sediments are reached, calculate sediment flux
          ! NOTE: the particulate tracer field is in units of mol kg-1, following the (dissolved) ocean tracers, and as a result,
          !       corrections must be made for changes in ocean layer thickness
          do kk=k-1,loc_bio_remin_min_k,-1
             ! test to see whether the ocean bottom has been reached
             If (kk >= dum_k1) then
                ! calculate ratio of layer thicknesses
                ! (used to convert particulate matter concentrations as particulates settle through the water column
                !  comprising layers of non-uniform thickness)
                loc_bio_remin_layerratio = phys_ocn(ipo_dD,dum_i,dum_j,kk+1)/phys_ocn(ipo_dD,dum_i,dum_j,kk)
                loc_bio_remin_dD = phys_ocn(ipo_dD,dum_i,dum_j,kk)
                ! calculate residence time (yr) of particulates in ocean layer (from layer thickness and sinking speed)
                ! NOTE: sinking rate has units of (m yr-1) (converted from parameter file input units)
                if (loc_bio_remin_sinkingrate > const_real_nullsmall) loc_bio_remin_dt = loc_bio_remin_dD/loc_bio_remin_sinkingrate

                ! *** Calculate fractional change in particulate fluxes ***
                ! carbonate
                if (sed_select(is_CaCO3)) then
                   If (.NOT. ctrl_bio_remin_CaCO3_fixed) then
                      ! calculate residence time in each ocean layer
                      ! ### INSERT CODE ########################################################################################## !
                      ! 
                      ! ########################################################################################################## !
                   else
                      ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                      ! if both reminerilization lengths have been set to zero,
                      ! then under undersaturated conditions assume that all CaCO3 dissolves 
                      ! NOTE: requires that saturation state at the grid point has already been solved for (if not updated)
                      if (par_bio_remin_CaCO3_eL1 < const_real_nullsmall .AND. par_bio_remin_CaCO3_eL2 < const_real_nullsmall) then
                         if (carb(ic_ohm_cal,dum_i,dum_j,kk) < 1.0) then
                            loc_bio_remin_CaCO3_frac1 = 1.0
                            loc_bio_remin_CaCO3_frac2 = 1.0
                         else
                            loc_bio_remin_CaCO3_frac1 = 0.0
                            loc_bio_remin_CaCO3_frac2 = 0.0
                         end if
                      else
                         loc_bio_remin_CaCO3_frac1 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_CaCO3_eL1))
                         loc_bio_remin_CaCO3_frac2 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_CaCO3_eL2))
                      end if
                      ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                   endif
                   ! calculate the ratio of particulate tracer between layers
                   loc_bio_part_CaCO3_ratio = 1.0 - &
                        & ( &
                        &   (1.0 - loc_bio_part_TMP(is_CaCO3_frac2,kk+1))*loc_bio_remin_CaCO3_frac1 + &
                        &   loc_bio_part_TMP(is_CaCO3_frac2,kk+1)*loc_bio_remin_CaCO3_frac2 &
                        & )
                   ! calculate change in partitioning between different fractions
                   is = is_CaCO3_frac2
                   if (loc_bio_part_TMP(is,kk+1) > const_real_nullsmall) then
                      loc_bio_part_TMP(is,kk) = &
                           & (1.0 - loc_bio_remin_CaCO3_frac2)*loc_bio_part_TMP(is,kk+1)/loc_bio_part_CaCO3_ratio
                   else
                      loc_bio_part_TMP(is,kk) = 0.0
                   end if
                end if
                ! opal
                if (sed_select(is_opal)) then
                   If (.NOT. ctrl_bio_remin_opal_fixed) then
                      ! set local variables - temperature (K) and silicic acid concentration (mol kg-1)
                      loc_T     = ocn(io_T,dum_i,dum_j,kk)
                      loc_SiO2  = ocn(io_SiO2,dum_i,dum_j,kk)
                      ! calculate opal equilibrium H4SiO4 saturation concentration
                      loc_Si_eq = conv_umol_mol*10.0**(6.44 - 968.0/loc_T)
                      ! calculate degree of opal undersatruation
                      loc_u     = (loc_Si_eq - loc_SiO2)/loc_Si_eq
                      IF (loc_u > const_real_one)       loc_u = 1.0
                      IF (loc_u < const_real_nullsmall) loc_u = 0.0
                      ! calculate opal fractional dissolution
                      ! NOTE: for now, assume that both opal 'fractionas' behave identically
                      loc_bio_remin_opal_frac1 =                                             &
                           & loc_bio_remin_dt*par_bio_remin_opal_K*                          &
                           & (1.0/0.71)*                                                     &
                           & (                                                               &
                           &   (0.16*(1.0 + (loc_T - const_zeroC)/15.0)*loc_u) +             &
                           &   (0.55*((1.0 + (loc_T - const_zeroC)/400.0)**4.0*loc_u)**9.25) &
                           & )
                      if (loc_bio_remin_opal_frac1 > const_real_one) loc_bio_remin_opal_frac1 = 1.0
                      loc_bio_remin_opal_frac2 = loc_bio_remin_opal_frac1
                   else
                      loc_bio_remin_opal_frac1 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_opal_eL1))
                      loc_bio_remin_opal_frac2 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_opal_eL2))
                   endif
                   ! calculate the ratio of particulate tracer between layers
                   loc_bio_part_opal_ratio = 1.0 - &
                        & ( &
                        &   (1.0 - loc_bio_part_TMP(is_opal_frac2,kk+1))*loc_bio_remin_opal_frac1 + &
                        &   loc_bio_part_TMP(is_opal_frac2,kk+1)*loc_bio_remin_opal_frac2 &
                        & )
                   ! calculate change in partitioning between different fractions
                   is = is_opal_frac2
                   if (loc_bio_part_TMP(is,kk+1) > const_real_nullsmall) then
                      loc_bio_part_TMP(is,kk) = &
                           & (1.0 - loc_bio_remin_opal_frac2)*loc_bio_part_TMP(is,kk+1)/loc_bio_part_opal_ratio
                   else
                      loc_bio_part_TMP(is,kk) = 0.0
                   end if
                end if
                ! particulate organic matter
                if (sed_select(is_POC)) then
                   If (.NOT. ctrl_bio_remin_POC_fixed) then
                      ! calculate residence time in each ocean layer
                      ! ### INSERT CODE ######################################################################################### !
                      ! 
                      ! ######################################################################################################### !
                      ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                      if (ctrl_bio_remin_POC_ballast) then
                         loc_bio_remin_POC_frac1 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_POC_eL1))
                         if (loc_bio_part_TMP(is_POC_frac2,kk+1)*loc_bio_part_TMP(is_POC,kk+1) > const_real_nullsmall) then
                            loc_bio_remin_POC_frac2 = 1.0 -                                                              &
                                 & (                                                                                     &
                                 &   loc_bio_part_CaCO3_ratio*par_bio_remin_ballast_kc*loc_bio_part_TMP(is_CaCO3,kk+1) + &
                                 &   loc_bio_part_opal_ratio*par_bio_remin_ballast_ko*loc_bio_part_TMP(is_opal,kk+1) +   &
                                 &   1.0*par_bio_remin_ballast_kl*loc_bio_part_TMP(is_det,kk+1)                          &
                                 & ) &
                                 & / &
                                 & ( &
                                 &   par_bio_remin_ballast_kc*loc_bio_part_TMP(is_CaCO3,kk+1) + &
                                 &   par_bio_remin_ballast_ko*loc_bio_part_TMP(is_opal,kk+1) +   &
                                 &   par_bio_remin_ballast_kl*loc_bio_part_TMP(is_det,kk+1)  &
                                 & )
                         else
                            loc_bio_remin_POC_frac2 = 0.0
                         end if
                      else
                         ! ### INSERT CODE ###################################################################################### !
                         ! 
                         ! ###################################################################################################### !
                      end if
                      ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                   else
                      loc_bio_remin_POC_frac1 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_POC_eL1))
                      loc_bio_remin_POC_frac2 = (1.0 - EXP(-loc_bio_remin_dD/par_bio_remin_POC_eL2))
                   endif
                   ! calculate the ratio of particulate tracer between layers
                   loc_bio_part_POC_ratio = 1.0 - &
                        & ( &
                        &   (1.0 - loc_bio_part_TMP(is_POC_frac2,kk+1))*loc_bio_remin_POC_frac1 + &
                        &   loc_bio_part_TMP(is_POC_frac2,kk+1)*loc_bio_remin_POC_frac2 &
                        & )
                   ! calculate potential oxidation capacity
                   loc_potO2cap = fun_potO2cap(ocn_select(:),ocn(:,dum_i,dum_j,kk),bio_remin(:,dum_i,dum_j,kk))
                   ! compare with potential oxygen availability and modify fraction remineralized accordingly
                   loc_O2demand = conv_sed_ocn(io_O2,is_POC)* &
                        & loc_bio_remin_layerratio*loc_bio_part_POC_ratio*loc_bio_part_TMP(is_POC,kk+1)
                   ! decrease effective remin fraction to take into account depletion of electon acceptors
                   if ((loc_O2demand > loc_potO2cap) .AND. (loc_O2demand > const_real_nullsmall)) then
                      loc_bio_part_POC_ratio = (loc_potO2cap/loc_O2demand)*loc_bio_part_POC_ratio
                   end if
                   ! calculate change in partitioning between different fractions
                   is = is_POC_frac2
                   if (loc_bio_part_TMP(is,kk+1) > const_real_nullsmall) then
                      loc_bio_part_TMP(is,kk) =  &
                           & (1.0 - loc_bio_remin_POC_frac2)*loc_bio_part_TMP(is,kk+1)/loc_bio_part_POC_ratio
                   else
                      loc_bio_part_TMP(is,kk) = 0.0
                   end if
                end if

                ! *** Calculate particle concentrations in layer below ***
                ! calculate local (temporary) particulate tracer concentration;
                ! (a) take the particulate concentration in the layer above
                ! (b) modify it by the remineralization ratio to take into account dissolution loss, and
                ! (c) convert to the equivalent particulate concentration in the new (lower) layer
                ! NOTE: NO fractionation (in elemental composition or isotopic ratio) is currently assumed
                !       => additional CASE statements are required to deal with specific fractionation cases
                ! NOTE: adjust fraction of scavenged material that can be returned (par_scav_fremin)
                !       => e.g., par_scav_fremin = 1.0 will result in
                !          scavanged material being returned in the same proportion as remineralization of the scavenger
                DO l=1,n_l_sed
                   is = conv_iselected_is(l)
                   if ( &
                        & (sed_dep(is) == is_POC) .OR. &
                        & (sed_type(is) == par_sed_type_POM) .OR. &
                        & (sed_type(sed_dep(is)) == is_POC) &
                        & ) then
                      ! particulate organic matter (plus elemental components, and particle-reactive scavenged elements)
                      if (sed_type(is) == par_sed_type_scavenged) then
                         loc_bio_part_TMP(is,kk) = loc_bio_part_TMP(is,kk+1)* &
                              & loc_bio_remin_layerratio*(1.0 - par_scav_fremin*(1.0 - loc_bio_part_POC_ratio))
                      else
                         loc_bio_part_TMP(is,kk) = loc_bio_part_TMP(is,kk+1)* &
                              & loc_bio_remin_layerratio*loc_bio_part_POC_ratio
                      end if
                   else if ( &
                        & (sed_dep(is) == is_CaCO3) .OR. &
                        & (sed_type(is) == par_sed_type_CaCO3) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_CaCO3) &
                        & ) then
                      ! carbonate (plus elemental components, and particle-reactive scavenged elements)
                      if (sed_type(is) == par_sed_type_scavenged) then
                         loc_bio_part_TMP(is,kk) = loc_bio_part_TMP(is,kk+1)* &
                              & loc_bio_remin_layerratio*(1.0 - par_scav_fremin*(1.0 - loc_bio_part_CaCO3_ratio))
                      else
                         loc_bio_part_TMP(is,kk) = loc_bio_part_TMP(is,kk+1)* &
                              & loc_bio_remin_layerratio*loc_bio_part_CaCO3_ratio
                      end if
                   else if ( &
                        & (sed_dep(is) == is_opal) .OR. &
                        & (sed_type(is) == par_sed_type_opal) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_opal) &
                        & ) then
                      ! opal (plus elemental components, and particle-reactive scavenged elements)
                      if (sed_type(is) == par_sed_type_scavenged) then
                         loc_bio_part_TMP(is,kk) = loc_bio_part_TMP(is,kk+1)* &
                              & loc_bio_remin_layerratio*(1.0 - par_scav_fremin*(1.0 - loc_bio_part_opal_ratio))
                      else
                         loc_bio_part_TMP(is,kk) = loc_bio_part_TMP(is,kk+1)* &             
                              & loc_bio_remin_layerratio*loc_bio_part_opal_ratio
                      endif
                   else if ( &
                        & (sed_dep(is) == is_det) .OR. &
                        & (sed_type(is) == par_sed_type_det) .OR. &
                        & (sed_type(sed_dep(is)) == par_sed_type_det) &
                        & ) then
                      ! 
                      loc_bio_part_TMP(is,kk) = loc_bio_part_TMP(is,kk+1)* &
                           & loc_bio_remin_layerratio
                   end if
                end do

                ! *** Calculate increase in tracer concentrations due to particle remineralization ***
                ! add 'missing' (remineralized) particulate sediment tracers to respective remineralization array components
                ! NOTE: ensure the particulate concentration in the upper layer is scaled w.r.t. 
                !       the difference in relative layer thickness
                !
                ! *** NOTE: par_sed_type_indepsinking ***
                !     Ignore changes to loc_bio_remin here if the sediment
                !     tracer is of type 'par_sed_type_indepsinking', as such tracers
                !     are subject to their independent sinking scheme and are thus
                !     not transported in accord with the particles they are associated to.
                !
                DO l=1,n_l_sed
                   is = conv_iselected_is(l)
                   if (sed_type(is).ne.par_sed_type_indepsinking) then
                      loc_tot_i = conv_sed_ocn_i(0,is)
                      do loc_i=1,loc_tot_i
                         io = conv_sed_ocn_i(loc_i,is)
                         loc_bio_remin(io,kk) = loc_bio_remin(io,kk) + conv_sed_ocn(io,is)* &
                              & (loc_bio_remin_layerratio*loc_bio_part_TMP(is,kk+1) - loc_bio_part_TMP(is,kk))
                      end do
                   endif
                end DO

                ! *** Scavenge Fe from water column ***
                ! NOTE: Fe scavenging must be called AFTER particulates have been 'moved' to the next layer down
                !       - they are assumed to start at the BASE of the originating layer,
                !         which is why they are not scavenged from level (kk+1)
                if (ocn_select(io_Fe)) then
                   if (ocn(io_Fe,dum_i,dum_j,kk) > const_real_nullsmall) then
                      call sub_calc_scav_Fe( &
                           & loc_bio_remin_dt, &
                           & ocn(io_Fe,dum_i,dum_j,kk), &
                           & loc_bio_part_TMP(:,kk), &
                           & loc_bio_remin(:,kk) &
                           & )
                   end if
                end if

             end If
          end do

          ! <<<<<<<<<<<<<<<<<<<<<<<
          ! *** kk SUB-LOOP END ***
          ! <<<<<<<<<<<<<<<<<<<<<<<

          ! *** UPDATE PARTICULATE MATTER INFORMATION ***
          ! update local ocean particulate tracer field - store residual particulate tracer at the point of 
          ! the deepest level reached
          ! NOTE: do not store if the sediment surface is reached
          !
          ! *** NOTE: par_sed_type_indepsinking ***
          !     Ignore changes to loc_bio_part here if the sediment
          !     tracer is of type 'par_sed_type_indepsinking', as such tracers
          !     are subject to their independent sinking scheme and are thus
          !     not transported in accord with the particles they are associated to.
          !
          If (loc_bio_remin_min_k >= dum_k1) then
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                if (sed_type(is).ne.par_sed_type_indepsinking) then
                   loc_bio_part(is,loc_bio_remin_min_k) = loc_bio_part(is,loc_bio_remin_min_k) + &
                        & loc_bio_part_TMP(is,loc_bio_remin_min_k)
                endif
             end do
          end if
          ! record particulate fluxes at base of each layer (units of; mol per time-step)
          ! NOTE: implicitly includes sedimentation flux (kk=dum_k1)
          ! NOTE: exclude meaningless 
          !
          ! *** NOTE: par_sed_type_indepsinking ***
          !     Reset loc_bio_settle here if the sediment tracer is of type
          !     'par_sed_type_indepsinking', as such tracers are subject to their
          !     independent sinking scheme and are thus not transported in accord
          !     with the particles they are associated to.
          !
          do kk=k,loc_bio_remin_min_k+1,-1
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                SELECT CASE (sed_type(is))
                case (par_sed_type_frac)
                   loc_bio_settle(is,kk) = loc_bio_settle(is,kk) + &
                        & loc_bio_part_TMP(is,kk)
                case (par_sed_type_indepsinking)
                   loc_bio_settle(is,kk) = 0.0
                case default
                   loc_bio_settle(is,kk) = loc_bio_settle(is,kk) + &
                        & phys_ocn(ipo_M,dum_i,dum_j,kk)*loc_bio_part_TMP(is,kk)
                end SELECT
             end do
          end do

       end If

       ! *** independent sinking scheme for 230Th and 231Pa ***
       do l=1,n_l_sed
          is=conv_iselected_is(l)
          if (sed_type(is).eq.par_sed_type_indepsinking) then
             loc_scav_sinkingvel = 0.0
             if ((is.eq.is_POM_230Th).or.(is.eq.is_CaCO3_230Th) &
                  & .or.(is.eq.is_opal_230Th).or.(is.eq.is_det_230Th)) then
                loc_scav_sinkingvel = par_scav_230Th_indepsinkingvel
             endif
             if ((is.eq.is_POM_231Pa).or.(is.eq.is_CaCO3_231Pa) &
                  & .or.(is.eq.is_opal_231Pa).or.(is.eq.is_det_231Pa)) then
                loc_scav_sinkingvel = par_scav_231Pa_indepsinkingvel
             endif
             if (k.gt.dum_k1) then
                if ((dum_dtyr*loc_scav_sinkingvel).ge.(phys_ocn(ipo_dD,dum_i,dum_j,k-1))) then
                   CALL sub_report_error( &
                        & 'biogem_box','calc_bio_remin', &
                        & 'Independent sinking velocity for 230Th or 231Pa is too large to comply with CFL criterium', &
                        & 'STOPPING', &
                        & (/const_real_null/),.TRUE. &
                        & )
                endif
             endif
             loc_bio_settle(is,k) = dum_dtyr * loc_scav_sinkingvel * &
                  & loc_bio_part_OLD(is,k) * phys_ocn(ipo_M,dum_i,dum_j,k) / phys_ocn(ipo_dD,dum_i,dum_j,k)
             loc_tot_i = conv_sed_ocn_i(0,is)
             do loc_i=1,loc_tot_i
                io = conv_sed_ocn_i(loc_i,is)
                loc_bio_remin(io,k) = loc_bio_remin(io,k) - loc_bio_settle(is,k)*phys_ocn(ipo_rM,dum_i,dum_j,k)
             enddo
             loc_bio_part(is,k) = loc_bio_part_OLD(is,k) + loc_bio_part(is,k) - loc_bio_settle(is,k)*phys_ocn(ipo_rM,dum_i,dum_j,k)
             if (k.gt.dum_k1) then
                loc_bio_part(is,k-1) = loc_bio_part(is,k-1) + loc_bio_settle(is,k)*phys_ocn(ipo_rM,dum_i,dum_j,k-1)
                do loc_i=1,loc_tot_i
                   io = conv_sed_ocn_i(loc_i,is)
                   loc_bio_remin(io,k-1) = loc_bio_remin(io,k-1) + loc_bio_settle(is,k)*phys_ocn(ipo_rM,dum_i,dum_j,k-1)
                enddo
             endif
          endif
       enddo
    end do

    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ! *** k WATER-COLUMN LOOP END ***
    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    ! *** WRITE GLOBAL ARRAY DATA ***
    ! NOTE: because sub_calc_bio_remin is the first call in the sequence of events (in biogem_main),
    !       data arrays are over-written rather than incremented
    ! write ocean tracer field and settling flux arrays (global array)
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       bio_part(is,dum_i,dum_j,:)   = loc_bio_part(is,:)
       bio_settle(is,dum_i,dum_j,:) = loc_bio_settle(is,:)
    end do
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do

  END SUBROUTINE sub_calc_bio_remin
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONVERT NO3 RELEASED DURING AEROBIC OXIDATION TO NH4
  SUBROUTINE sub_calc_bio_remin_NO3toNH4(dum_i,dum_j,dum_k1)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    ! local variables
    integer::l,io,k
    real::loc_potO2
    real::loc_NO3,loc_r15N
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** CONVERT REMINERLAIZED NO3 -> NH4 ***
    ! subroutine to correct for the default complete oxidation of organic N to NO3 under aerobic conditions
    ! assume reverse of NH4 oxidation reaction:
    ! NO3- + 2H+ + H2O -> NH4+ + 2O2
    DO k=n_k,dum_k1,-1
       ! set local [NO3]
       loc_NO3 = bio_remin(io_NO3,dum_i,dum_j,k)
       ! calculate potential oxygen
       loc_potO2 = ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k) + 2.0*loc_NO3
       if ((bio_remin(io_NO3,dum_i,dum_j,k) > const_real_nullsmall) .AND. (loc_potO2 > const_real_nullsmall)) then
          ! restrict NO3 pool if necessary
          if (loc_potO2 < 2.0*loc_NO3) loc_NO3 = loc_potO2/2.0
          ! calculate isotopic ratio
          loc_r15N = bio_remin(io_NO3_15N,dum_i,dum_j,k)/bio_remin(io_NO3,dum_i,dum_j,k)
          ! convert NO3 -> NH4
          loc_bio_remin(io_O2,k)  = 2.0*loc_NO3
          loc_bio_remin(io_NH4,k) = loc_NO3
          loc_bio_remin(io_NO3,k) = -loc_NO3
          loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k)
          loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_bio_remin(io_NO3,k)
          loc_bio_remin(io_NH4_15N,k) = loc_r15N*loc_bio_remin(io_NH4,k)
       end if
    end DO

    ! *** WRITE DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do

  end SUBROUTINE sub_calc_bio_remin_NO3toNH4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE NITRATE REDUCTION ARRISING FROM THE ANAEROBIC OXIDATION OF ORGANIC MATTER
  SUBROUTINE sub_calc_bio_remin_reduce_NO3(dum_i,dum_j,dum_k1)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j,dum_k1
    ! local variables
    integer::l,io,k
    real::loc_potO2def
    real::loc_NO3,loc_r15N
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize local variables
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** NITRATE REDUCTION ***
    ! INTRO
    ! look for oxygen deficits; carry out nitrate reduction to remove the O2 deficit
    ! STOCHIOMETRY
    ! NOTE: reaction stoichiometry assumes OM = C106H263O110N16P
    !       and 138 mol O2 are required to oxidize each mol of OM under aerobic conditions
    ! Overall: 106(CH2O)16(NH3)H3PO4 + 84.8 NO3- + 84.8 H+ -> 106 CO2 + 42.4 N2 + 16 NH3 + H3PO4 + 148.4 H2O
    ! for which oxygen is provided by: 2 NO3- + 2 H+ -> N2 + 5/2 O2 + H2O
    ! and thus where 84.8 arises from 2/2.5 * 106
    ! => 84.8 mole of NO3 are required for every 106 moles of O2 deficit,
    !    BUT 138 mol O2 has implicitly been consumed and must be returned!
    !    => par_bio_red_O2_NO3 = 84.8/138.0
    ! NOTE: the implicit aerobic oxidation of OM has created 16 mol NO3 for each mol OM,
    !       which would otherwise have to be subtracted!
    !       EXCEPT, the <ocn> array has not yet been updated with the remin release (and hence NO3 addition)
    ! NOTE: according to Sarmiento and Gruber [2006]:
    !       OM + 104 HNO3 -> 106 CO2 + 60 N2 + H3PO4 + 138 H2O
    !       where OM = C106H175O42N16P
    DO k=n_k,dum_k1,-1
       ! calculate potential oxygen availability
       loc_potO2def = -(ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k)) + par_bio_remin_denitrO2thresh
       ! if oxygen deficit exists, go do something useful about it
       if ((loc_potO2def > const_real_nullsmall) .AND. (ocn(io_NO3,dum_i,dum_j,k) > const_real_nullsmall)) then
          ! set local NO3 concentration
          loc_NO3 = ocn(io_NO3,dum_i,dum_j,k)
          ! calculate isotopic ratio
          loc_r15N = ocn(io_NO3_15N,dum_i,dum_j,k)/ocn(io_NO3,dum_i,dum_j,k)
          ! test for full vs. partial NO3 utilization
!!$          if (loc_potO2def < (loc_NO3 - par_bio_red_POP_PON*loc_potO2def/(-par_bio_red_POP_PO2))/par_bio_red_O2_NO3) then
          if (loc_potO2def < loc_NO3/par_bio_red_O2_NO3) then
             ! partial NO3 utilization (=> N isotope Rayleigh fractionation can occur) NB *** need to include fractionation!!!
             loc_bio_remin(io_N2,k)  = 0.5*par_bio_red_O2_NO3*loc_potO2def
             loc_bio_remin(io_O2,k)  = loc_potO2def
             loc_bio_remin(io_NH4,k) = par_bio_red_POP_PON*loc_potO2def/(-par_bio_red_POP_PO2)
             loc_bio_remin(io_NO3,k) = -par_bio_red_O2_NO3*loc_potO2def - loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k)
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_bio_remin(io_NO3,k)
             loc_bio_remin(io_N2_15N,k)  = loc_r15N*loc_bio_remin(io_N2,k)
             loc_bio_remin(io_NH4_15N,k) = loc_r15N*loc_bio_remin(io_NH4,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO N FRACTIONATION ########################################################## !
             ! 
             ! ################################################################################################################### !
          else
             ! complete NO3 utilization (no N fractionation)
             loc_bio_remin(io_N2,k)  = 0.5*loc_NO3
             loc_bio_remin(io_O2,k)  = loc_NO3/par_bio_red_O2_NO3
             loc_bio_remin(io_NH4,k) = par_bio_red_POP_PON*loc_bio_remin(io_O2,k)/(-par_bio_red_POP_PO2)
             loc_bio_remin(io_NO3,k) = -loc_NO3 - loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k)
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_bio_remin(io_NO3,k)
             loc_bio_remin(io_N2_15N,k)  = loc_r15N*loc_bio_remin(io_N2,k)
             loc_bio_remin(io_NH4_15N,k) = loc_r15N*loc_bio_remin(io_NH4,k)
          end if
       end if
    end DO

    ! *** WRITE DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! record diagnostics (mol kg-1)
    diag_geochem(idiag_geochem_Nredct_dN2,dum_i,dum_j,:)  = loc_bio_remin(io_N2,:)
    diag_geochem(idiag_geochem_Nredct_dNH4,dum_i,dum_j,:) = loc_bio_remin(io_NH4,:)

  end SUBROUTINE sub_calc_bio_remin_reduce_NO3
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SULPHATE REDUCTION ARRISING FROM THE ANAEROBIC OXIDATION OF ORGANIC MATTER
  SUBROUTINE sub_calc_bio_remin_reduce_SO4(dum_i,dum_j,dum_k1)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    integer,intent(in)::dum_k1
    ! local variables
    integer::l,io
    INTEGER::k
    real::loc_potO2def
    real::loc_SO4,loc_r34S,loc_r15N
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** SULPHATE REDUCTION ***
    ! INTRO
    ! look for oxygen deficits; carry out sulphate reduction to remove the O2 deficit
    ! ***
    ! STOCHIOMETRY
    ! NOTE: reaction stoichiometry assumes OM = C106H263O110N16P
    !       and 138 mol O2 are required to oxidize each mol of OM under aerobic conditions
    ! Overall: (CH2O)106(NH3)16H3PO4 + 53 SO42- + 106 H+ -> 106 CO2 + 53 H2S + 16 NH3 + H3PO4 + 106 H2O
    ! for which oxygen is provided by: 2H+ + SO4 -> H2S + 2O2
    ! => 53 mole of SO4 are required for every 106 moles of O2 deficit
    !    BUT 138 mol O2 has implicitly been consumed and must be returned!
    !    => par_bio_red_O2_H2SO4 = 53.0/138.0
    ! NOTE: don't forget, the implicit aerobic oxidation of OM has created 16 mol HNO3 for each mol OM
    !       => this must be subtracted!
    ! NOTE: according to Sarmiento and Gruber [2006]:
    !       OM + 59 H2SO4 -> 106 CO2 + 59 H2S + 16 NH3 + H3PO4 + 62 H2O
    !       where OM = C106H175O42N16P
    ! ***
    DO k=n_k,dum_k1,-1
       ! calculate potential oxygen deficit
       loc_potO2def = -(ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k))
       ! if oxygen deficit exists, go do something useful about it
       If ((loc_potO2def > const_real_nullsmall) .AND. (ocn(io_SO4,dum_i,dum_j,k) > const_real_nullsmall)) then
          ! set local SO4 concentration
          loc_SO4 = ocn(io_SO4,dum_i,dum_j,k)
          ! calculate isotopic ratio
          loc_r34S = ocn(io_SO4_34S,dum_i,dum_j,k)/ocn(io_SO4,dum_i,dum_j,k)
          loc_r15N = bio_remin(io_NO3_15N,dum_i,dum_j,k)/bio_remin(io_NO3,dum_i,dum_j,k)
          ! test for full vs. partial SO4 utilization
          if (loc_potO2def < loc_SO4/par_bio_red_O2_H2SO4) then
             ! partial SO4 utilization (=> S isotope Rayleigh fractionation can occur)
             loc_bio_remin(io_SO4,k) = -par_bio_red_O2_H2SO4*loc_potO2def
             loc_bio_remin(io_H2S,k) = par_bio_red_O2_H2SO4*loc_potO2def
             loc_bio_remin(io_O2,k)  = loc_potO2def
             loc_bio_remin(io_NH4,k) = par_bio_red_POP_PON*loc_potO2def/(-par_bio_red_POP_PO2)
             loc_bio_remin(io_NO3,k) = -loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k) - 2.0*loc_bio_remin(io_SO4,k)
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*loc_bio_remin(io_SO4,k)
             loc_bio_remin(io_H2S_34S,k) = loc_r34S*loc_bio_remin(io_H2S,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO S FRACTIONATION ########################################################## !
             ! 
             ! ################################################################################################################### !
             loc_bio_remin(io_NH4_15N,k) = loc_r15N*loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_bio_remin(io_NO3,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO N FRACTIONATION ########################################################## !
             ! 
             ! ################################################################################################################### !
          else
             ! complete SO4 utilization (no S fractionation)
             loc_bio_remin(io_SO4,k) = -loc_SO4
             loc_bio_remin(io_H2S,k) = loc_SO4
             loc_bio_remin(io_O2,k)  = loc_SO4/par_bio_red_O2_H2SO4
             loc_bio_remin(io_NH4,k) = par_bio_red_POP_PON*loc_bio_remin(io_O2,k)/(-par_bio_red_POP_PO2)
             loc_bio_remin(io_NO3,k) = -loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_ALK,k) = loc_bio_remin(io_NH4,k) - loc_bio_remin(io_NO3,k) - 2.0*loc_bio_remin(io_SO4,k)
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*loc_bio_remin(io_SO4,k)
             loc_bio_remin(io_H2S_34S,k) = loc_r34S*loc_bio_remin(io_H2S,k)
             loc_bio_remin(io_NH4_15N,k) = loc_r15N*loc_bio_remin(io_NH4,k)
             loc_bio_remin(io_NO3_15N,k) = loc_r15N*loc_bio_remin(io_NO3,k)
          end if
       end if
    end do

    ! *** WRITE DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! record diagnostics (mol kg-1)
    diag_geochem(idiag_geochem_Sredct_dH2S,dum_i,dum_j,:) = loc_bio_remin(io_H2S,:)
    diag_geochem(idiag_geochem_Sredct_dNH4,dum_i,dum_j,:) = loc_bio_remin(io_NH4,:)

  end SUBROUTINE sub_calc_bio_remin_reduce_SO4
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SULPHATE REDUCTION ARRISING FROM THE ANAEROBIC OXIDATION OF ORGANIC MATTER - SIMPLE VERSION!
  SUBROUTINE sub_calc_bio_remin_reduce_SO4_SIMPLE(dum_i,dum_j,dum_k1)
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    integer,intent(in)::dum_k1
    ! local variables
    integer::l,io
    INTEGER::k
    real::loc_potO2def
    real::loc_SO4,loc_r34S
    real,dimension(n_ocn,n_k)::loc_bio_remin

    ! *** INITIALIZE VARIABLES ***
    ! initialize remineralization tracer arrays
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_bio_remin(io,:) = 0.0
    end do

    ! *** SULPHATE REDUCTION ***
    ! INTRO
    ! look for oxygen deficits; carry out sulphate reduction to remove the O2 deficit
    ! ***
    ! STOCHIOMETRY
    ! 2H+ + SO4 -> H2S + 2O2
    ! ***
    DO k=n_k,dum_k1,-1
       ! calculate potential oxygen deficit
       loc_potO2def = -(ocn(io_O2,dum_i,dum_j,k) + bio_remin(io_O2,dum_i,dum_j,k))
       ! if oxygen deficit exists, go do something useful about it
       If ((loc_potO2def > const_real_nullsmall) .AND. (ocn(io_SO4,dum_i,dum_j,k) > const_real_nullsmall)) then
          ! set local SO4 concentration
          loc_SO4 = ocn(io_SO4,dum_i,dum_j,k)
          ! calculate isotopic ratio
          loc_r34S = ocn(io_SO4_34S,dum_i,dum_j,k)/ocn(io_SO4,dum_i,dum_j,k)
          if (loc_potO2def < 2.0*loc_SO4) then
             ! partial SO4 utilization (=> S isotope Rayleigh fractionation can occur)
             loc_bio_remin(io_SO4,k) = -0.5*loc_potO2def
             loc_bio_remin(io_H2S,k) = 0.5*loc_potO2def
             loc_bio_remin(io_O2,k)  = loc_potO2def
             loc_bio_remin(io_ALK,k) = -2.0*loc_bio_remin(io_SO4,k)
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*loc_bio_remin(io_SO4,k)
             loc_bio_remin(io_H2S_34S,k) = loc_r34S*loc_bio_remin(io_H2S,k)
             ! ### INSERT ALTERNATIVE CODE FOR NON-ZERO S FRACTIONATION ########################################################## !
             ! 
             ! ################################################################################################################### !
          else
             ! complete SO4 utilization (no S fractionation)
             loc_bio_remin(io_SO4,k) = -loc_SO4
             loc_bio_remin(io_H2S,k) = loc_SO4
             loc_bio_remin(io_O2,k)  = 2.0*loc_SO4
             loc_bio_remin(io_ALK,k) = -2.0*loc_bio_remin(io_SO4,k)
             loc_bio_remin(io_SO4_34S,k) = loc_r34S*loc_bio_remin(io_SO4,k)
             loc_bio_remin(io_H2S_34S,k) = loc_r34S*loc_bio_remin(io_H2S,k)
          end if
       end if
    end do

    ! *** WRITE DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
    ! record diagnostics (mol kg-1)
    diag_geochem(idiag_geochem_Sredct_dH2S,dum_i,dum_j,:) = loc_bio_remin(io_H2S,:)

  end SUBROUTINE sub_calc_bio_remin_reduce_SO4_SIMPLE
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate Fe scavenging
  SUBROUTINE sub_calc_scav_Fe(dum_dtyr,dum_ocn_Fe,dum_bio_part,dum_bio_remin)
    ! dummy arguments
    REAL,INTENT(in)::dum_dtyr
    REAL,INTENT(in)::dum_ocn_Fe
    real,dimension(n_sed),INTENT(inout)::dum_bio_part
    real,dimension(n_ocn),INTENT(inout)::dum_bio_remin
    ! local variables
    real::loc_scav_Fe_k_POC,loc_scav_Fe_k_CaCO3,loc_scav_Fe_k_opal,loc_scav_Fe_k_det
    real::loc_scav_Fe_k_tot
    real::loc_scav_dFe_tot
    real::loc_part_den_POC,loc_part_den_CaCO3,loc_part_den_opal,loc_part_den_det
    real::loc_part_den_tot

    ! *** Calculate Fe scavenging ***
    ! NOTE: residence time in each ocean layer must be estimated for the Parekh et al. [2005] model
    ! NOTE: Dutkiewicz et al. [2005] scavenging rate par_scav_Fe_Ks has been converted to units of (yr-1)
    ! NOTE: scavening from surface ocean is NOT calculated here ...
    if (sed_select(is_POM_Fe)) then 
       loc_part_den_POC = conv_g_mg*conv_POC_mol_g*dum_bio_part(is_POC)/conv_kg_l
    else
       loc_part_den_POC = 0.0
    end if
    if (sed_select(is_CaCO3_Fe)) then 
       loc_part_den_CaCO3 = conv_g_mg*conv_cal_mol_g*dum_bio_part(is_CaCO3)/conv_kg_l
    else
       loc_part_den_CaCO3 = 0.0
    end if
    if (sed_select(is_opal_Fe)) then 
       loc_part_den_opal  = conv_g_mg*conv_opal_mol_g*dum_bio_part(is_opal)/conv_kg_l
    else
       loc_part_den_opal  = 0.0
    end if
    if (sed_select(is_det_Fe)) then 
       loc_part_den_det   = conv_g_mg*conv_det_mol_g*dum_bio_part(is_det)/conv_kg_l
    else
       loc_part_den_det   = 0.0
    end if
    loc_part_den_tot = loc_part_den_POC + loc_part_den_CaCO3 + loc_part_den_opal + loc_part_den_det 
    if (loc_part_den_tot > const_real_nullsmall) then
       if (ctrl_bio_Fe_fixedKscav) then
          ! calculate scavenging following Dutkiewicz et al. [2005]
          ! net scavenging rate
          loc_scav_Fe_k_tot = par_scav_Fe_ks
          ! particle-specific scavenging rates
          loc_scav_Fe_k_POC = (loc_part_den_POC/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_CaCO3 = (loc_part_den_CaCO3/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_opal = (loc_part_den_opal/loc_part_den_tot)*loc_scav_Fe_k_tot
          loc_scav_Fe_k_det = (loc_part_den_det/loc_part_den_tot)*loc_scav_Fe_k_tot
          ! calculate total Fe scavenged
          loc_scav_dFe_tot = dum_dtyr*loc_scav_Fe_k_tot*dum_ocn_Fe
       else
          ! calculate scavenging following Parekh et al. [2005]
          ! particle-specific scavenging rates
          loc_scav_Fe_k_POC   = par_scav_Fe_sf_POC*par_scav_Fe_k0*loc_part_den_POC**par_scav_Fe_exp
          loc_scav_Fe_k_CaCO3 = par_scav_Fe_sf_CaCO3*par_scav_Fe_k0*loc_part_den_CaCO3**par_scav_Fe_exp
          loc_scav_Fe_k_opal  = par_scav_Fe_sf_opal*par_scav_Fe_k0*loc_part_den_opal**par_scav_Fe_exp
          loc_scav_Fe_k_det   = par_scav_Fe_sf_det*par_scav_Fe_k0*loc_part_den_det**par_scav_Fe_exp
          ! net scavenging rate
          loc_scav_Fe_k_tot = loc_scav_Fe_k_POC + loc_scav_Fe_k_CaCO3 + loc_scav_Fe_k_opal + loc_scav_Fe_k_det
          ! calculate total Fe scavenged
          loc_scav_dFe_tot = dum_dtyr*loc_scav_Fe_k_tot*dum_ocn_Fe
       end if
       ! calculate Fe scavenged by particulates
       ! and update local remineralization array to take into account the removal of Fe from solution
       if (loc_scav_Fe_k_tot > const_real_nullsmall) then
          dum_bio_part(is_POM_Fe)   = dum_bio_part(is_POM_Fe) + (loc_scav_Fe_k_POC/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          dum_bio_part(is_CaCO3_Fe) = dum_bio_part(is_CaCO3_Fe) + (loc_scav_Fe_k_CaCO3/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          dum_bio_part(is_opal_Fe)  = dum_bio_part(is_opal_Fe) + (loc_scav_Fe_k_opal/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          dum_bio_part(is_det_Fe)   = dum_bio_part(is_det_Fe) + (loc_scav_Fe_k_det/loc_scav_Fe_k_tot)*loc_scav_dFe_tot
          dum_bio_remin(io_Fe) = dum_bio_remin(io_Fe) - loc_scav_dFe_tot
       end if
    end if

  end SUBROUTINE sub_calc_scav_Fe
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! Calculate 230Th partitioning for equilibrium scavenging
  SUBROUTINE sub_calc_equilscav_230Th(dum_dtyr,dum_i,dum_j,dum_k1)
    ! dummy arguments
    REAL,INTENT(in)::dum_dtyr
    integer::dum_i,dum_j,dum_k1
    ! local variables
    real::loc_part_ratden_POC,loc_part_ratden_CaCO3,loc_part_ratden_opal,loc_part_ratden_det,loc_part_ratden_tot
    real::loc_dis_230Th
    integer::loc_k
    real::loc_conv
    real::loc_scav_sinkingvel
    real::loc_bio_settle_POC,loc_bio_settle_CaCO3,loc_bio_settle_opal,loc_bio_settle_det
 
    loc_conv=conv_m3_kg
    loc_scav_sinkingvel = par_scav_230Th_indepsinkingvel

    ! *** Calculate 230Th partitioning for equilibrium scavenging ***
    ! Kd = (CTh_p/M_p)/(CTh_d/M_water) [((mol/l) / (kg/l)) / ((mol/l) / (kg/l))]
    ! ratden: M_p/M_water
    do loc_k = dum_k1,n_k
       if (ctrl_force_scav_fpart_POC) then
          loc_bio_settle_POC = par_scav_fpart_POC(dum_i,dum_j,loc_k)*phys_ocn(ipo_A,dum_i,dum_j,loc_k)*dum_dtyr
       else
          loc_bio_settle_POC = bio_settle(is_POC,dum_i,dum_j,loc_k)
       endif
       if (ctrl_force_scav_fpart_CaCO3) then
          loc_bio_settle_CaCO3 = par_scav_fpart_CaCO3(dum_i,dum_j,loc_k)*phys_ocn(ipo_A,dum_i,dum_j,loc_k)*dum_dtyr
       else
          loc_bio_settle_CaCO3 = bio_settle(is_CaCO3,dum_i,dum_j,loc_k)
       endif
       if (ctrl_force_scav_fpart_opal) then
          loc_bio_settle_opal = par_scav_fpart_opal(dum_i,dum_j,loc_k)*phys_ocn(ipo_A,dum_i,dum_j,loc_k)*dum_dtyr
       else
          loc_bio_settle_opal = bio_settle(is_opal,dum_i,dum_j,loc_k)
       endif
       if (ctrl_force_scav_fpart_det) then
          loc_bio_settle_det = par_scav_fpart_det(dum_i,dum_j,loc_k)*phys_ocn(ipo_A,dum_i,dum_j,loc_k)*dum_dtyr
       else
          loc_bio_settle_det = bio_settle(is_det,dum_i,dum_j,loc_k)
       endif
       if (sed_select(is_POM_230Th)) then
          loc_part_ratden_POC = conv_g_kg * conv_POC_mol_g * loc_bio_settle_POC * &
               & phys_ocn(ipo_rA,dum_i,dum_j,loc_k) / (dum_dtyr * loc_scav_sinkingvel * loc_conv  )
          ! kg m^{-3} / kg m^{-3}, density(CaCO3)/density(fluid)
       else
          loc_part_ratden_POC = 0.0
       end if
       if (sed_select(is_CaCO3_230Th)) then
          loc_part_ratden_CaCO3 = conv_g_kg * conv_cal_mol_g * loc_bio_settle_CaCO3 * &
               & phys_ocn(ipo_rA,dum_i,dum_j,loc_k) / (dum_dtyr * loc_scav_sinkingvel * loc_conv  )
          ! kg m^{-3} / kg m^{-3}, density(CaCO3)/density(fluid) 
       else
          loc_part_ratden_CaCO3 = 0.0
       End if
       if (sed_select(is_opal_230Th)) then
          loc_part_ratden_opal = conv_g_kg * conv_opal_mol_g * loc_bio_settle_opal * &
               & phys_ocn(ipo_rA,dum_i,dum_j,loc_k) / (dum_dtyr * loc_scav_sinkingvel * loc_conv  )
          ! kg m^{-3} / kg m^{-3}, density(opal)/density(fluid) 
       else
          loc_part_ratden_opal = 0.0
       end if
       if (sed_select(is_det_230Th)) then
          loc_part_ratden_det = conv_g_kg * conv_det_mol_g * loc_bio_settle_det * &
               & phys_ocn(ipo_rA,dum_i,dum_j,loc_k) / (dum_dtyr * loc_scav_sinkingvel * loc_conv  )
          ! kg m^{-3} / kg m^{-3}, density(det)/density(fluid) 
       else
          loc_part_ratden_det = 0.0
       end if
       loc_part_ratden_tot = loc_part_ratden_POC + loc_part_ratden_CaCO3 + loc_part_ratden_opal + loc_part_ratden_det
       if (ocn(io_230Th,dum_i,dum_j,loc_k) < const_real_zero) then
          CALL sub_report_error('biogem_box','calc_equilscav_230Th', &
               & '(ocean) tracer inventory of 230Th below zero at (i,j,k), ignoring equilibrium scavenging', &
               & 'CONTINUING', &
               & (/real(dum_i),real(dum_j),real(loc_k)/),.FALSE. &
               & )
       endif
       if ((loc_part_ratden_tot > const_real_nullsmall).and.(ocn(io_230Th,dum_i,dum_j,loc_k) > const_real_zero)) then
          SELECT CASE (par_scav_230Th_scavopt)
          CASE ('equilibrium')
             ! calculate equilibrium scavenging following Siddall et al. (2005)
             ! Assume ALL 230Th is stored in ocn(io_230Th,:,:,:) (not only the
             ! dissolved form), partition here temporarily into particulate
             ! species and dissolved form
             ! NOTE: ocn(io_230Th,:,:,:) remains unaffected by this
                loc_dis_230Th = ocn(io_230Th,dum_i,dum_j,loc_k) / &
                     & (par_scav_230Th_kPOC * loc_part_ratden_POC &
                     & + par_scav_230Th_kCaCO3 * loc_part_ratden_CaCO3 &
                     & + par_scav_230Th_kopal * loc_part_ratden_opal &
                     & + par_scav_230Th_kdet * loc_part_ratden_det &
                     & + 1.0)
                bio_part(is_POM_230Th,dum_i,dum_j,loc_k) = loc_dis_230Th * par_scav_230Th_kPOC * loc_part_ratden_POC
                bio_part(is_CaCO3_230Th,dum_i,dum_j,loc_k) = loc_dis_230Th * par_scav_230Th_kCaCO3 * loc_part_ratden_CaCO3 
                bio_part(is_opal_230Th,dum_i,dum_j,loc_k) = loc_dis_230Th * par_scav_230Th_kopal * loc_part_ratden_opal 
                bio_part(is_det_230Th,dum_i,dum_j,loc_k) = loc_dis_230Th * par_scav_230Th_kdet * loc_part_ratden_det
          CASE default
             ! Do nothing for non-equilibrium scavenging here
          END SELECT
       else
          SELECT CASE (par_scav_230Th_scavopt)
          CASE ('equilibrium')
             bio_part(is_POM_230Th,dum_i,dum_j,loc_k) = 0.
             bio_part(is_CaCO3_230Th,dum_i,dum_j,loc_k) = 0.
             bio_part(is_opal_230Th,dum_i,dum_j,loc_k) = 0.
             bio_part(is_det_230Th,dum_i,dum_j,loc_k) = 0.
          CASE default
             ! Do nothing for non-equilibrium scavenging here
          END SELECT
       end if
    enddo
  end SUBROUTINE sub_calc_equilscav_230Th
  ! ****************************************************************************************************************************** !
 
  ! ****************************************************************************************************************************** !
  ! Calculate 231Pa partitioning for equilibrium scavenging
  SUBROUTINE sub_calc_equilscav_231Pa(dum_dtyr,dum_i,dum_j,dum_k1)
    ! dummy arguments
    REAL,INTENT(in)::dum_dtyr
    integer::dum_i,dum_j,dum_k1
    ! local variables
    real::loc_part_ratden_POC,loc_part_ratden_CaCO3,loc_part_ratden_opal,loc_part_ratden_det,loc_part_ratden_tot
    real::loc_dis_231Pa
    integer::loc_k
    real::loc_conv
    real::loc_scav_sinkingvel
    real::loc_bio_settle_POC,loc_bio_settle_CaCO3,loc_bio_settle_opal,loc_bio_settle_det

    loc_conv=conv_m3_kg
    loc_scav_sinkingvel = par_scav_231Pa_indepsinkingvel

    ! *** Calculate 231Pa partitioning for equilibrium scavenging ***
    ! Kd = (CTh_p/M_p)/(CTh_d/M_water) [((mol/l) / (kg/l)) / ((mol/l) / (kg/l))]
    ! ratden: M_p/M_water
    do loc_k = dum_k1,n_k
       if (ctrl_force_scav_fpart_POC) then
          loc_bio_settle_POC = par_scav_fpart_POC(dum_i,dum_j,loc_k)*phys_ocn(ipo_A,dum_i,dum_j,loc_k)*dum_dtyr
       else
          loc_bio_settle_POC = bio_settle(is_POC,dum_i,dum_j,loc_k)
       endif
       if (ctrl_force_scav_fpart_CaCO3) then
          loc_bio_settle_CaCO3 = par_scav_fpart_CaCO3(dum_i,dum_j,loc_k)*phys_ocn(ipo_A,dum_i,dum_j,loc_k)*dum_dtyr
       else
          loc_bio_settle_CaCO3 = bio_settle(is_CaCO3,dum_i,dum_j,loc_k)
       endif
       if (ctrl_force_scav_fpart_opal) then
          loc_bio_settle_opal = par_scav_fpart_opal(dum_i,dum_j,loc_k)*phys_ocn(ipo_A,dum_i,dum_j,loc_k)*dum_dtyr
       else
          loc_bio_settle_opal = bio_settle(is_opal,dum_i,dum_j,loc_k)
       endif
       if (ctrl_force_scav_fpart_det) then
          loc_bio_settle_det = par_scav_fpart_det(dum_i,dum_j,loc_k)*phys_ocn(ipo_A,dum_i,dum_j,loc_k)*dum_dtyr
       else
          loc_bio_settle_det = bio_settle(is_det,dum_i,dum_j,loc_k)
       endif
       if (sed_select(is_POM_231Pa)) then
          loc_part_ratden_POC = conv_g_kg * conv_POC_mol_g * loc_bio_settle_POC * &
               & phys_ocn(ipo_rA,dum_i,dum_j,loc_k) / (dum_dtyr * loc_scav_sinkingvel * loc_conv  )
          ! kg m^{-3} / kg m^{-3}, density(CaCO3)/density(fluid)
       else
          loc_part_ratden_POC = 0.0
       end if
       if (sed_select(is_CaCO3_231Pa)) then
          loc_part_ratden_CaCO3 = conv_g_kg * conv_cal_mol_g * loc_bio_settle_CaCO3 * &
               & phys_ocn(ipo_rA,dum_i,dum_j,loc_k) / (dum_dtyr * loc_scav_sinkingvel * loc_conv  )
          ! kg m^{-3} / kg m^{-3}, density(CaCO3)/density(fluid) 
       else
          loc_part_ratden_CaCO3 = 0.0
       End if
       if (sed_select(is_opal_231Pa)) then
          loc_part_ratden_opal = conv_g_kg * conv_opal_mol_g * loc_bio_settle_opal * &
               & phys_ocn(ipo_rA,dum_i,dum_j,loc_k) / (dum_dtyr * loc_scav_sinkingvel * loc_conv  )
          ! kg m^{-3} / kg m^{-3}, density(opal)/density(fluid) 
       else
          loc_part_ratden_opal = 0.0
       end if
       if (sed_select(is_det_231Pa)) then
          loc_part_ratden_det = conv_g_kg * conv_det_mol_g * loc_bio_settle_det * &
               & phys_ocn(ipo_rA,dum_i,dum_j,loc_k) / (dum_dtyr * loc_scav_sinkingvel * loc_conv  )
          ! kg m^{-3} / kg m^{-3}, density(det)/density(fluid) 
       else
          loc_part_ratden_det = 0.0
       end if
       loc_part_ratden_tot = loc_part_ratden_POC + loc_part_ratden_CaCO3 + loc_part_ratden_opal + loc_part_ratden_det
       if (ocn(io_231Pa,dum_i,dum_j,loc_k) < const_real_zero) then
          CALL sub_report_error('biogem_box','calc_equilscav_231Pa', &
               & '(ocean) tracer inventory of 231Pa below zero at (i,j,k), ignoring equilibrium scavenging', &
               & 'CONTINUING', &
               & (/real(dum_i),real(dum_j),real(loc_k)/),.FALSE. &
               & )
       endif
       if ((loc_part_ratden_tot > const_real_nullsmall).and.(ocn(io_231Pa,dum_i,dum_j,loc_k) > const_real_zero)) then
          SELECT CASE (par_scav_231Pa_scavopt)
          CASE ('equilibrium')
             ! calculate equilibrium scavenging following Siddall et al. (2005)
             ! Assume ALL 231Pa is stored in ocn(io_231Pa,:,:,:) (not only the
             ! dissolved form), partition here temporarily into particulate
             ! species and dissolved form
             ! NOTE: ocn(io_231Pa,:,:,:) remains unaffected by this
                loc_dis_231Pa = ocn(io_231Pa,dum_i,dum_j,loc_k) / &
                     & (par_scav_231Pa_kPOC * loc_part_ratden_POC &
                     & + par_scav_231Pa_kCaCO3 * loc_part_ratden_CaCO3 &
                     & + par_scav_231Pa_kopal * loc_part_ratden_opal &
                     & + par_scav_231Pa_kdet * loc_part_ratden_det &
                     & + 1.0)
                bio_part(is_POM_231Pa,dum_i,dum_j,loc_k) = loc_dis_231Pa * par_scav_231Pa_kPOC * loc_part_ratden_POC
                bio_part(is_CaCO3_231Pa,dum_i,dum_j,loc_k) = loc_dis_231Pa * par_scav_231Pa_kCaCO3 * loc_part_ratden_CaCO3 
                bio_part(is_opal_231Pa,dum_i,dum_j,loc_k) = loc_dis_231Pa * par_scav_231Pa_kopal * loc_part_ratden_opal 
                bio_part(is_det_231Pa,dum_i,dum_j,loc_k) = loc_dis_231Pa * par_scav_231Pa_kdet * loc_part_ratden_det
          CASE default
             ! Do nothing for non-equilibrium scavenging here
          END SELECT
       else
          SELECT CASE (par_scav_231Pa_scavopt)
          CASE ('equilibrium')
             bio_part(is_POM_231Pa,dum_i,dum_j,loc_k) = 0.
             bio_part(is_CaCO3_231Pa,dum_i,dum_j,loc_k) = 0.
             bio_part(is_opal_231Pa,dum_i,dum_j,loc_k) = 0.
             bio_part(is_det_231Pa,dum_i,dum_j,loc_k) = 0.
          CASE default
             ! Do nothing for non-equilibrium scavenging here
          END SELECT
       end if
    enddo
  end SUBROUTINE sub_calc_equilscav_231Pa
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! Calculate 230Th partitioning for equilibrium scavenging
  SUBROUTINE sub_calc_scav_230Th(dum_k1)
    ! dummy arguments
    integer::dum_k1
    ! local variables
    integer::loc_k

    do loc_k = dum_k1,n_k
       SELECT CASE (par_scav_230Th_scavopt)
       CASE ('equilibrium')
          ! Equilibrium scavenging following Siddall et al. (2005)
          ! Nothing to do here
       CASE default
          ! INSERT CODE FOR NON-EQUILIBRIUM SCAVENGING HERE
       END SELECT
    enddo

  end SUBROUTINE sub_calc_scav_230Th
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! Calculate 231Pa partitioning for equilibrium scavenging
  SUBROUTINE sub_calc_scav_231Pa(dum_k1)
    ! dummy arguments
    integer::dum_k1
    ! local variables
    integer::loc_k

    do loc_k = dum_k1,n_k
       SELECT CASE (par_scav_231Pa_scavopt)
       CASE ('equilibrium')
          ! Equilibrium scavenging following Siddall et al. (2005)
          ! Nothing to do here
       CASE default
          ! INSERT CODE FOR NON-EQUILIBRIUM SCAVENGING HERE
       END SELECT
    enddo

  end SUBROUTINE sub_calc_scav_231Pa
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! CORRECT SPURIOUS NEGATIVE [H2S]
  SUBROUTINE sub_calc_bio_remin_fix_H2S(loc_ocn)
    ! dummy arguments
    real,INTENT(inout),dimension(n_ocn)::loc_ocn
    ! fix [H2S]
    if (loc_ocn(io_H2S) < const_real_zero) then
       loc_ocn(io_SO4) = loc_ocn(io_SO4) + loc_ocn(io_H2S)
       loc_ocn(io_O2)  = loc_ocn(io_O2) - 2.0*loc_ocn(io_H2S)
       loc_ocn(io_ALK) = loc_ocn(io_ALK) - 2.0*loc_ocn(io_H2S)
       loc_ocn(io_H2S) = 0.0
    end if
    ! also fix [O2]
    if (loc_ocn(io_O2) < const_real_zero) then
       loc_ocn(io_SO4) = loc_ocn(io_SO4) + 0.5*loc_ocn(io_O2)
       loc_ocn(io_ALK) = loc_ocn(io_ALK) - loc_ocn(io_O2)
       loc_ocn(io_H2S) = loc_ocn(io_H2S) - 0.5*loc_ocn(io_O2)
       loc_ocn(io_O2)  = 0.0
    end if
  end SUBROUTINE sub_calc_bio_remin_fix_H2S
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CORRECT SPURIOUS NEGATIVE [NH4]
  SUBROUTINE sub_calc_bio_remin_fix_NH4(loc_ocn)
    ! dummy arguments
    real,INTENT(inout),dimension(n_ocn)::loc_ocn
    ! fix [NH4]
    if (loc_ocn(io_NH4) < const_real_zero) then
       loc_ocn(io_NO3) = loc_ocn(io_NO3) + loc_ocn(io_NH4)
       loc_ocn(io_O2)  = loc_ocn(io_O2) - 2.0*loc_ocn(io_NH4)
       loc_ocn(io_ALK) = loc_ocn(io_ALK) - 2.0*loc_ocn(io_NH4)
       loc_ocn(io_NH4) = 0.0
    end if
  end SUBROUTINE sub_calc_bio_remin_fix_NH4
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! PRODUCTION OF DECAY CHAIN ISOTOPES
  !
  ! Temporary version with implicit 230Th and 231Pa production, to be replaced by generic code that calculates production
  ! from the decay of the corresponding parent tracer
  !
  ! sea water 234U/238U ratio 234R=6.258e-5 (+/- 0.012e-5) (Chen et al., 1986)
  ! sea water 238U/235U ratio 1/235R=137.89 (+/- 0.15) (Chen et al., 1986)
  ! mean uranium concentration in sea water normalised to 35o/oo: C_U=3.238 ng/g (Chen et al, 1986)
  ! Reference:
  ! J.H. Chen, R.L. Edwards, G.J. Wasserburg (1986), "238U, 234U and 232Th in seawater", Earth Planet. Sci. Lett., 80, pp. 241-251
   SUBROUTINE sub_calc_geochem_decay_chain(dum_focn,dum_i,dum_j,dum_k1)
    ! dummy arguments
    real,intent(inout),dimension(n_ocn,n_k)::dum_focn
    integer,intent(in)::dum_i,dum_j,dum_k1
    ! local variables
    integer::k
    real,dimension(n_ocn,n_k)::loc_focn
    real,parameter::const_234R = 6.258e-5  ! 234U-to-238U ratio
    real,parameter::const_234M = 234.      ! g/mol
    real,parameter::const_235R = 1./137.89 ! 235U-to-238U ratio
    real,parameter::const_235M = 235.      ! g/mol
    real,parameter::const_238R = 1.        ! 238U-to-238U ratio
    real,parameter::const_238M = 238.      ! g/mol
    real,parameter::const_CU = 3.238e-6    ! g/kg
    real,parameter::const_U=const_CU*(const_234R+const_235R+const_238R)/(const_234M*const_234R+const_235M* &
         & const_235R+const_238M*const_238R)  ! mol/kg
    real,parameter::const_234U=const_234R*const_U/(const_234R+const_235R+const_238R) ! mol/kg
    real,parameter::const_235U=const_235R*const_U/(const_234R+const_235R+const_238R) ! mol/kg
    ! linearise decay, good approximation if const_lambda_* much bigger than timestep
    ! this enables to calculate loc_f* as constants here (which otherwise would depend on the length of the time step
    real,parameter::const_f234U=-const_lambda_234U*const_234U ! mol/(kg*yr)
    real,parameter::const_f235U=-const_lambda_235U*const_235U ! mol/(kg*yr)
    ! The resulting production activity ratio is
    ! const_lambda_231Pa*const_f235U/(const_lambda_230Th*const_f234U) = 9.34371952334926148E-002
    !
    if (ocn_select(io_230Th).or.ocn_select(io_231Pa)) then
       do k=dum_k1,n_k
          if (ocn_select(io_230Th)) then
             loc_focn(io_230Th,k) = -1.*phys_ocn(ipo_M,dum_i,dum_j,k)*const_f234U*ocn(io_S,dum_i,dum_j,k)/35. ! mol/yr
          endif
          if (ocn_select(io_231Pa)) then
             ! Assume newly produced 231Th completely decays within one timestep (half-life time of 231Th is ~1day)
             loc_focn(io_231Pa,k) = -1.*phys_ocn(ipo_M,dum_i,dum_j,k)*const_f235U*ocn(io_S,dum_i,dum_j,k)/35. ! mol/yr
          endif
          dum_focn(io_230Th,k)=dum_focn(io_230Th,k)+loc_focn(io_230Th,k) ! mol/yr
          dum_focn(io_231Pa,k)=dum_focn(io_231Pa,k)+loc_focn(io_231Pa,k) ! mol/yr
!          ! debug output: production activity rates
!          print *, const_lambda_230Th*loc_focn(io_230Th,k)*6.022e23/phys_ocn(ipo_V,dum_i,dum_j,k)/365.25/24/60, &
!            & const_lambda_231Pa*loc_focn(io_231Pa,k)*6.022e23/phys_ocn(ipo_V,dum_i,dum_j,k)/365.25/24/60 ! dpm/(m^3*yr)
       enddo
    endif
!    ! debug output: 231Pa/230Th production activity ratio
!    print *, const_lambda_231Pa*const_f235U/(const_lambda_230Th*const_f234U)
  end SUBROUTINE sub_calc_geochem_decay_chain
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CORRECT SPURIOUS NEGATIVE [H2S]
  SUBROUTINE sub_calc_geochem_daughter()
    !
  end SUBROUTINE sub_calc_geochem_daughter
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CORRECT SPURIOUS NEGATIVE [NH4]
  SUBROUTINE sub_calc_misc_brinerejection(dum_dtyr,dum_i,dum_j,dum_fT,dum_fS)
    ! dummy arguments
    real,intent(in)::dum_dtyr                                      ! 
    INTEGER,INTENT(in)::dum_i,dum_j                                ! 
    real,INTENT(inout)::dum_fT,dum_fS                              ! 
    ! local variables
    integer::l,io                                                  ! 
    integer::loc_k1                                                ! local topography
    real::loc_dV,loc_rM,loc_frac                                   ! 
    real,dimension(n_ocn,n_k)::loc_bio_remin                       !

    ! *** BLAH ***
    ! set local constants
    loc_k1 = goldstein_k1(dum_i,dum_j)
    loc_dV = phys_ocnatm(ipoa_seaice_dV,dum_i,dum_j)
    loc_rM = phys_ocn(ipo_M,dum_i,dum_j,n_k)/phys_ocn(ipo_M,dum_i,dum_j,loc_k1)
    ! initialize variables
    dum_fT = 0.0
    dum_fS = 0.0
    loc_bio_remin(:,:) = 0.0

    ! *** BLAH ***
    ! carry out brine transfer from surface to depth
    if ((loc_dV > const_real_nullsmall) .AND. (dum_j <= par_misc_brinerejection_jmax)) then
       ! calculate fractional volume transfer of tracers from surface to benthic cell
       loc_frac = par_misc_brinerejection_frac* &
            & (const_rho_seaice/phys_ocn(ipo_rho,dum_i,dum_j,n_k))*(loc_dV/phys_ocn(ipo_V,dum_i,dum_j,n_k))
       ! calculate T,S fluxes
       dum_fT = 0.0
       dum_fS = loc_frac*ocn(io_S,dum_i,dum_j,n_k)*phys_ocn(ipo_M,dum_i,dum_j,n_k)/dum_dtyr
       if (ctrl_misc_brinerejection_bgc) then
          ! calculate biogeochem tracer concentration changes
          DO l=3,n_l_ocn
             io = conv_iselected_io(l)
             loc_bio_remin(io,n_k)    = -loc_frac*ocn(io,dum_i,dum_j,n_k)
             loc_bio_remin(io,loc_k1) = -loc_rM*loc_bio_remin(io,n_k)
          end DO
       end if
    end if

    ! *** WRITE DATA ***
    ! write ocean tracer remineralization field (global array)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       bio_remin(io,dum_i,dum_j,:) = bio_remin(io,dum_i,dum_j,:) + loc_bio_remin(io,:)
    end do
!!$    ! record diagnostics
!!$    diag_geochem(idiag_geochem_ammox_dNH4,dum_i,dum_j,:) = loc_bio_remin(io_NH4,:)
!!$    diag_geochem(idiag_geochem_ammox_dNO3,dum_i,dum_j,:) = loc_bio_remin(io_NO3,:)

  end SUBROUTINE sub_calc_misc_brinerejection
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FORCING FUNCTION ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Updating environment at the current (BioGeM) model time w.r.t. a defined signal function
  SUBROUTINE sub_update_sig(dum_t,dum_sig,dum_sig_i,dum_x)
    ! dummy arguments
    REAL, INTENT(in)::dum_t
    REAL,INTENT(in),DIMENSION(n_data_max)::dum_sig
    INTEGER,INTENT(inout),DIMENSION(2)::dum_sig_i
    REAL, INTENT(out)::dum_x
    ! update forcing signal indices (if required) and carry put linear interpolation
    ! NOTE: t(1) is the lower age bounding point, t(2) is the upper age bounding point
    IF (dum_sig_i(1) > 1) THEN
       IF (dum_t < dum_sig(dum_sig_i(1))) THEN
          DO
             dum_sig_i(1) = dum_sig_i(1) - 1
             IF (dum_t > dum_sig(dum_sig_i(1))) THEN
                ! found correct index - exit loop
                EXIT
             ELSEIF (dum_sig_i(1) == 1) THEN
                EXIT
             END IF
          END DO
       END IF
    END IF
    IF (dum_sig_i(2) > 1) THEN
       IF (dum_t < dum_sig(dum_sig_i(2))) THEN
          DO
             dum_sig_i(2) = dum_sig_i(2) - 1
             IF (dum_t >= dum_sig(dum_sig_i(2))) THEN
                ! come too far - add one back to index(2) and exit loop
                dum_sig_i(2) = dum_sig_i(2) + 1
                EXIT
             ELSEIF (dum_sig_i(2) == 1) THEN
                EXIT
             END IF
          END DO
       END IF
    END IF
    ! calculate relative position of current time w.r.t. upper and lower bounding points of the signal function
    ! NOTE: if upper and lower bounding points are identical 
    !       (i.e., if current time is outside of maximum or minimum signal time values)
    !       avoid divide-by-zero problems and assume a value of 0.5
    IF (ABS(dum_sig(dum_sig_i(2)) - dum_sig(dum_sig_i(1))) > const_real_nullsmall) THEN
       dum_x = (dum_sig(dum_sig_i(2)) - dum_t)/(dum_sig(dum_sig_i(2)) - dum_sig(dum_sig_i(1)))
    ELSE
       dum_x = 0.5
    ENDIF
  END SUBROUTINE sub_update_sig
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Update ocean restoring forcing function value
  SUBROUTINE sub_update_force_restore_ocn(dum_t,dum_io)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_io
    ! local variables
    INTEGER::i,j,k
    REAL::loc_x
    REAL::loc_force_restore_ocn
    real::loc_tot,loc_standard
    ! calculate new forcing time series values
    CALL sub_update_sig(dum_t,force_restore_ocn_sig(dum_io,1,:),force_restore_ocn_sig_i(dum_io,:),loc_x)
    force_restore_ocn_sig_x(dum_io) = &
         & (1 - loc_x)*force_restore_ocn_sig(dum_io,2,force_restore_ocn_sig_i(dum_io,2)) + &
         & loc_x*force_restore_ocn_sig(dum_io,2,force_restore_ocn_sig_i(dum_io,1))
    ! *** update prescribed (restoring) boundary conditions ***
    ! NOTE: use different <k> limits for the ocean restoring forcing loop (to enable surface-only forcing to be implemented)
    ! NOTE: flux forcings are in units of mol a-1
    DO i=1,n_i
       DO j=1,n_j
          DO k=force_restore_ocn_k1(dum_io,i,j),n_k
             loc_force_restore_ocn = &
                  & force_restore_ocn_I(dum_io,i,j,k) + &
                  & force_restore_ocn_sig_x(dum_io)*(force_restore_ocn_II(dum_io,i,j,k) - force_restore_ocn_I(dum_io,i,j,k))
             SELECT CASE (ocn_type(dum_io))
             CASE (0,1)
                force_restore_ocn(dum_io,i,j,k) = loc_force_restore_ocn
             case (11:20)
                loc_tot  = force_restore_ocn(ocn_dep(dum_io),i,j,k)
                loc_standard = const_standards(ocn_type(dum_io))
                force_restore_ocn(dum_io,i,j,k) = fun_calc_isotope_fraction(loc_force_restore_ocn,loc_standard)*loc_tot
             END SELECT
          END DO
       END DO
    END DO
  END SUBROUTINE sub_update_force_restore_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Update ocean flux forcing function value
  SUBROUTINE sub_update_force_flux_ocn(dum_t,dum_io)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_io
    ! local variables
    INTEGER::i,j,k
    REAL::loc_x
    REAL::loc_force_flux_ocn_tot
    REAL::loc_force_flux_ocn_rtot
    REAL::loc_force_flux_ocn
    real::loc_tot,loc_standard
    ! calculate new forcing time series values
    CALL sub_update_sig(dum_t,force_flux_ocn_sig(dum_io,1,:),force_flux_ocn_sig_i(dum_io,:),loc_x)
    force_flux_ocn_sig_x(dum_io) = &
         & (1 - loc_x)*force_flux_ocn_sig(dum_io,2,force_flux_ocn_sig_i(dum_io,2)) + &
         & loc_x*force_flux_ocn_sig(dum_io,2,force_flux_ocn_sig_i(dum_io,1))
    ! *** update flux boundary conditions ***
    ! NOTE: use different <k> limits for the ocean restoring forcing loop (to enable surface-only forcing to be implemented)
    ! NOTE: flux forcings are in units of mol yr-1
    loc_force_flux_ocn_tot = 0.0
    DO i=1,n_i
       DO j=1,n_j
          DO k=force_flux_ocn_k1(dum_io,i,j),n_k
             loc_force_flux_ocn = &
                  & force_flux_ocn_I(dum_io,i,j,k) + &
                  & force_flux_ocn_sig_x(dum_io)*(force_flux_ocn_II(dum_io,i,j,k) - force_flux_ocn_I(dum_io,i,j,k))
             SELECT CASE (ocn_type(dum_io))
             CASE (0,1)
                force_flux_ocn(dum_io,i,j,k) = loc_force_flux_ocn
                loc_force_flux_ocn_tot = loc_force_flux_ocn_tot + loc_force_flux_ocn
             case (11:20)
                loc_tot  = force_flux_ocn(ocn_dep(dum_io),i,j,k)
                loc_standard = const_standards(ocn_type(dum_io))
                force_flux_ocn(dum_io,i,j,k) = fun_calc_isotope_fraction(loc_force_flux_ocn,loc_standard)*loc_tot
             END SELECT
          END DO
       END DO
    END DO
    ! normalize flux forcings (if selected) so that the total flux is equal to the magnitude (at the current time step) 
    ! defined in the forcing signal file
    IF (force_flux_ocn_scale(dum_io)) THEN
       if (abs(loc_force_flux_ocn_tot) > const_real_nullsmall) then
          loc_force_flux_ocn_rtot = 1.0/loc_force_flux_ocn_tot
       else
          loc_force_flux_ocn_rtot = 0.0
       end if
       DO i=1,n_i
          DO j=1,n_j
             DO k=force_flux_ocn_k1(dum_io,i,j),n_k
                SELECT CASE (ocn_type(dum_io))
                CASE (0,1)
                   force_flux_ocn(dum_io,i,j,k) = force_flux_ocn_sig_x(dum_io)*force_flux_ocn(dum_io,i,j,k)*loc_force_flux_ocn_rtot
                end SELECT
             END DO
          END DO
       END DO
    END IF
  END SUBROUTINE sub_update_force_flux_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Update atmosphere tracer restoring forcing function value
  SUBROUTINE sub_update_force_restore_atm(dum_t,dum_ia)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_ia
    ! local variables
    INTEGER::i,j
    REAL::loc_x
    REAL::loc_force_restore_atm
    real::loc_tot,loc_standard
    ! calculate new atmosphere forcing time series values
    CALL sub_update_sig(dum_t,force_restore_atm_sig(dum_ia,1,:),force_restore_atm_sig_i(dum_ia,:),loc_x)
    force_restore_atm_sig_x(dum_ia) = &
         & (1 - loc_x)*force_restore_atm_sig(dum_ia,2,force_restore_atm_sig_i(dum_ia,2)) + &
         & loc_x*force_restore_atm_sig(dum_ia,2,force_restore_atm_sig_i(dum_ia,1))
    ! update prescribed (restoring) boundary conditions
    DO i=1,n_i
       DO j=1,n_j
          loc_force_restore_atm =  &
               & force_restore_atm_I(dum_ia,i,j) + &
               & force_restore_atm_sig_x(dum_ia)*(force_restore_atm_II(dum_ia,i,j) - force_restore_atm_I(dum_ia,i,j))
          SELECT CASE (atm_type(dum_ia))
          CASE (1)
             force_restore_atm(dum_ia,i,j) = loc_force_restore_atm
          case (11:20)
             loc_tot  = force_restore_atm(atm_dep(dum_ia),i,j)
             loc_standard = const_standards(atm_type(dum_ia))
             force_restore_atm(dum_ia,i,j) = fun_calc_isotope_fraction(loc_force_restore_atm,loc_standard)*loc_tot
          END SELECT
       END DO
    END DO
  END SUBROUTINE sub_update_force_restore_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Update atmosphere tracer flux forcing function value
  SUBROUTINE sub_update_force_flux_atm(dum_t,dum_ia)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_ia
    ! local variables
    INTEGER::i,j
    REAL::loc_x
    REAL::loc_force_flux_atm_tot
    REAL::loc_force_flux_atm_rtot
    REAL::loc_force_flux_atm
    real::loc_tot,loc_standard
    ! calculate new atmosphere forcing time series values
    CALL sub_update_sig(dum_t,force_flux_atm_sig(dum_ia,1,:),force_flux_atm_sig_i(dum_ia,:),loc_x)
    force_flux_atm_sig_x(dum_ia) = &
         & (1 - loc_x)*force_flux_atm_sig(dum_ia,2,force_flux_atm_sig_i(dum_ia,2)) + &
         & loc_x*force_flux_atm_sig(dum_ia,2,force_flux_atm_sig_i(dum_ia,1))
    ! update flux boundary conditions
    ! NOTE: flux forcings are in units of mol yr-1
    loc_force_flux_atm_tot = 0.0
    DO i=1,n_i
       DO j=1,n_j
          loc_force_flux_atm = &
               & force_flux_atm_I(dum_ia,i,j) + &
               & force_flux_atm_sig_x(dum_ia)*(force_flux_atm_II(dum_ia,i,j) - force_flux_atm_I(dum_ia,i,j))
          SELECT CASE (atm_type(dum_ia))
          CASE (1)
             force_flux_atm(dum_ia,i,j) = loc_force_flux_atm
             loc_force_flux_atm_tot = loc_force_flux_atm_tot + loc_force_flux_atm
          case (11:20)
             loc_tot  = force_flux_atm(atm_dep(dum_ia),i,j)
             loc_standard = const_standards(atm_type(dum_ia))
             force_flux_atm(dum_ia,i,j) = fun_calc_isotope_fraction(loc_force_flux_atm,loc_standard)*loc_tot
          END SELECT
       END DO
    END DO
    ! normalize flux forcings (if selected) so that the total flux is equal to the magnitude (at the current time step) 
    ! defined in the forcing signal file
    ! NOTE: only re-scale type 1 atmosphere tracers -
    !       the isotopic tracers will be automatically normalized because they are related directly to the total flux,
    !       and when this subroutine call is made for an isotopic tracer,
    !       it has already been called to deal with the related bulk tracer where the normalization is done
    IF (force_flux_atm_scale(dum_ia)) THEN
       if (abs(loc_force_flux_atm_tot) > const_real_nullsmall) then
          loc_force_flux_atm_rtot = 1.0/loc_force_flux_atm_tot
       else
          loc_force_flux_atm_rtot = 0.0
       end if
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (atm_type(dum_ia))
             CASE (1)
                force_flux_atm(dum_ia,i,j) = force_flux_atm(dum_ia,i,j)*force_flux_atm_sig_x(dum_ia)*loc_force_flux_atm_rtot
             END SELECT
          END DO
       END DO
    END IF
  END SUBROUTINE sub_update_force_flux_atm
  ! ****************************************************************************************************************************** !


!!$  ! *** update sediment tracer restoring forcing function value ***
!!$  ! <<< GENERIC >>>
!!$  SUBROUTINE sub_update_force_restore_sed(dum_t,dum_is)
!!$    ! dummy arguments
!!$    REAL,INTENT(in)::dum_t
!!$    INTEGER,INTENT(in)::dum_is
!!$    ! local variables
!!$    INTEGER::i,j
!!$    REAL::loc_x
!!$    ! calculate new sediment tracer forcing time series values
!!$    CALL sub_update_sig(dum_t,force_restore_sed_sig(dum_is,1,:),force_restore_sed_sig_i(dum_is,:),loc_x)
!!$    force_restore_sed_sig_x(dum_is) = &
!!$         & (1 - loc_x)*force_restore_sed_sig(dum_is,2,force_restore_sed_sig_i(dum_is,2)) + &
!!$         & loc_x*force_restore_sed_sig(dum_is,2,force_restore_sed_sig_i(dum_is,1))
!!$    ! update prescribed (restoring) boundary conditions
!!$    DO i=1,n_i
!!$       DO j=1,n_j
!!$          force_restore_sed(dum_is,i,j) = &
!!$               & force_restore_sed_I(dum_is,i,j) + &
!!$               & force_restore_sed_sig_x(dum_is)*(force_restore_sed_II(dum_is,i,j) - force_restore_sed_I(dum_is,i,j))
!!$       END DO
!!$    END DO
!!$  END SUBROUTINE sub_update_force_restore_sed


  ! ****************************************************************************************************************************** !
  ! Update sediment tracer flux forcing function value
  SUBROUTINE sub_update_force_flux_sed(dum_t,dum_is)
    ! dummy arguments
    REAL,INTENT(in)::dum_t
    INTEGER,INTENT(in)::dum_is
    ! local variables
    INTEGER::i,j
    REAL::loc_x
    REAL::loc_force_flux_sed_tot
    REAL::loc_force_flux_sed_rtot
    REAL::loc_force_flux_sed
    real::loc_tot,loc_standard
    ! calculate new sediment tracer forcing time series values
    CALL sub_update_sig(dum_t,force_flux_sed_sig(dum_is,1,:),force_flux_sed_sig_i(dum_is,:),loc_x)
    force_flux_sed_sig_x(dum_is) = &
         & (1 - loc_x)*force_flux_sed_sig(dum_is,2,force_flux_sed_sig_i(dum_is,2)) + &
         & loc_x*force_flux_sed_sig(dum_is,2,force_flux_sed_sig_i(dum_is,1))
    ! update flux boundary conditions
    ! NOTE: flux forcings are in units of mol yr-1
    loc_force_flux_sed_tot = 0.0
    DO i=1,n_i
       DO j=1,n_j
          loc_force_flux_sed = &
               & force_flux_sed_I(dum_is,i,j) + &
               & force_flux_sed_sig_x(dum_is)*(force_flux_sed_II(dum_is,i,j) - force_flux_sed_I(dum_is,i,j))
          SELECT CASE (sed_type(dum_is))
          CASE (par_sed_type_bio,par_sed_type_abio,par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det)
             force_flux_sed(dum_is,i,j) = loc_force_flux_sed
             loc_force_flux_sed_tot = loc_force_flux_sed_tot + loc_force_flux_sed
          case (11:20)
             loc_tot  = force_flux_sed(sed_dep(dum_is),i,j)
             loc_standard = const_standards(sed_type(dum_is))
             force_flux_sed(dum_is,i,j) = fun_calc_isotope_fraction(loc_force_flux_sed,loc_standard)*loc_tot
          END SELECT
       END DO
    END DO
    ! normalize flux forcings (if selected) so that the total flux is equal to the magnitude (at the current time step) 
    ! defined in the forcing signal file
    IF (force_flux_sed_scale(dum_is)) THEN
       if (abs(loc_force_flux_sed_tot) > const_real_nullsmall) then
          loc_force_flux_sed_rtot = 1.0/loc_force_flux_sed_tot
       else
          loc_force_flux_sed_rtot = 0.0
       end if
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(dum_is))
             CASE (par_sed_type_bio)
                force_flux_sed(dum_is,i,j) = force_flux_sed(dum_is,i,j)*force_flux_sed_sig_x(dum_is)*loc_force_flux_sed_rtot
             end SELECT
          END DO
       END DO
    END IF
  END SUBROUTINE sub_update_force_flux_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INVENTORY AUDIT ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Calculate ocean tracer inventory
  FUNCTION fun_calc_ocn_tot()
    ! result variable
    REAL,dimension(n_ocn)::fun_calc_ocn_tot
    ! local variables
    INTEGER::l,i,j,k,io,is
    integer::loc_i,loc_tot_i
    real,dimension(n_sed,n_i,n_j,n_k)::loc_bio_part
    real,dimension(n_ocn,n_i,n_j,n_k)::loc_bio_part_ocn
    real,dimension(n_ocn,n_i,n_j,n_k)::loc_ocn
    real,dimension(n_ocn,n_i,n_j,n_k)::loc_ocn_tot
    ! set local variables
    loc_bio_part(:,:,:,:)     = 0.0
    loc_bio_part_ocn(:,:,:,:) = 0.0
    loc_ocn(:,:,:,:)          = 0.0
    loc_ocn_tot(:,:,:,:)      = 0.0
    ! set default result
    fun_calc_ocn_tot(:) = 0.0
    ! convert particulate sediment and dissolved organic matter tracer concentrations to (dissolved) tracers
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             loc_ocn(:,i,j,k) = ocn(:,i,j,k)
             loc_bio_part(:,i,j,k) = bio_part(:,i,j,k)
             DO l=3,n_l_ocn
                io = conv_iselected_io(l)
                loc_tot_i = conv_DOM_POM_i(0,io)
                do loc_i=1,loc_tot_i
                   is = conv_DOM_POM_i(loc_i,io)
                   loc_bio_part(is,i,j,k)  = loc_bio_part(is,i,j,k) + conv_DOM_POM(is,io)*loc_ocn(io,i,j,k)
                   loc_ocn(io,i,j,k) = 0.0
                end do
             end do
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                loc_tot_i = conv_sed_ocn_i(0,is)
                do loc_i=1,loc_tot_i
                   io = conv_sed_ocn_i(loc_i,is)
                   loc_bio_part_ocn(io,i,j,k) = loc_bio_part_ocn(io,i,j,k) + conv_sed_ocn(io,is)*loc_bio_part(is,i,j,k)
                end do
             end DO
          end DO
       end DO
    end DO
    ! determine ocean tracer inventory (mol)
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       loc_ocn_tot(io,:,:,:) = loc_ocn(io,:,:,:) + loc_bio_part_ocn(io,:,:,:)
       fun_calc_ocn_tot(io) = sum(phys_ocn(ipo_M,:,:,:)*loc_ocn_tot(io,:,:,:))
    end do
    fun_calc_ocn_tot(:) = fun_audit_combinetracer(fun_calc_ocn_tot(:))
  END function fun_calc_ocn_tot
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Carry out updated tracer audit
  SUBROUTINE sub_audit_update()
    ! local variables
    INTEGER::l,io
    REAL,dimension(n_ocn)::loc_audit_ocn_relerr
    ! set local variables
    loc_audit_ocn_relerr(:)   = 0.0
    ! calculate inventory drift
    audit_ocn_new(:) = fun_calc_ocn_tot()
    ! adjust ocean tracer inventory change (audit_ocn_delta) to combine different forms of the same element
    audit_ocn_delta(:) = fun_audit_combinetracer(audit_ocn_delta(:))
    ! calculate relative change in tracer inventories
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       if (abs(audit_ocn_new(io)) > const_real_nullsmall) then
          loc_audit_ocn_relerr(io) = (audit_ocn_new(io) - (audit_ocn_old(io) + audit_ocn_delta(io)))/audit_ocn_new(io)
       else
          loc_audit_ocn_relerr(io) = 0.0
       end if
    end DO
    ! compare current (ocean) tracer inventory with estimate since last audit,
    ! upate maximum error encountered, and report error if relative change exceeds pre-defined threshold
    ! NOTE: do not report 14C (ocn 'type' 12), because it decays and is 'lost' in mass terms ...
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       SELECT CASE (ocn_type(io))
       CASE (1,11:20)
          IF (ABS(loc_audit_ocn_relerr(io)) > par_misc_audit_relerr) THEN
             CALL sub_report_error('biogem_box','audit_update', &
                  & '(ocean) tracer inventory drift: new(top)/old(middle)/expected(bottom)): '//TRIM(string_ocn(io)), &
                  & 'n/a', &
                  & (/ &
                  &   audit_ocn_new(io), &
                  &   audit_ocn_old(io), &
                  &   (audit_ocn_old(io) + audit_ocn_delta(io)) &
                  & /), &
                  & ctrl_audit_fatal)
          ENDIF
          audit_ocn_old(io) = audit_ocn_new(io)
          audit_ocn_delta(io) = 0.0
       end SELECT
    END DO
  END SUBROUTINE sub_audit_update
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Combine different forms of the same element
  function fun_audit_combinetracer(dum_ocn)
    ! result variable
    real,dimension(n_ocn)::fun_audit_combinetracer
    ! dummy arguments
    REAL,dimension(n_ocn),INTENT(in)::dum_ocn
    ! initialze result variable
    fun_audit_combinetracer(:) = dum_ocn(:)
    ! adjust ocean tracer inventory change (audit_ocn_delta) to combine different forms of the same element
    ! NOTE: combine transformable tracer pairs when testing for drift;
    !       NO3 + N2 (N2 -> NO3 during nitrogen fixation, and NO3 -> N2 during denitrification))
    !       CO2 + CH4
    !       SO4 + H2S
    ! NOTE: ignore O2 component in oxidizing CH4->CO2 for now ...
    ! NOTE: adjust ALK for H2S (assumed created via sulphate reduction and thus ocean ALK increase)
    ! NOTE: subtract 2.0 x NH4 from O2 potential inventory to take into account virtual O2 liberation during ammoniam oxidation:
    !       NH4+ + 2O2 -> NO3- + 2H+ + H2O
    ! NOTE: subtract 2.0 x N2 from O2 potential inventory to take into account virtual O2 liberation during denitrification:
    !       2 NO3- + 2 H+ -> N2 + 5/2 O2 + H2O
    if (ocn_select(io_CH4)) then
       fun_audit_combinetracer(io_DIC) = fun_audit_combinetracer(io_DIC) + dum_ocn(io_CH4)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  - 2.0*dum_ocn(io_CH4)
    end if
    fun_audit_combinetracer(io_CH4) = 0.0
    if (ocn_select(io_CH4_13C)) then
       fun_audit_combinetracer(io_DIC_13C) = fun_audit_combinetracer(io_DIC_13C) + dum_ocn(io_CH4_13C)
    end if
    fun_audit_combinetracer(io_CH4_13C) = 0.0
    if (ocn_select(io_NO3)) then
       fun_audit_combinetracer(io_ALK) = fun_audit_combinetracer(io_ALK) + dum_ocn(io_NO3)
    end if
    if (ocn_select(io_N2O)) then
       fun_audit_combinetracer(io_NO3) = fun_audit_combinetracer(io_NO3) + 2.0*dum_ocn(io_N2O)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  + 0.5*dum_ocn(io_N2O)
    end if
    fun_audit_combinetracer(io_N2O) = 0.0
    if (ocn_select(io_N2)) then
       fun_audit_combinetracer(io_NO3) = fun_audit_combinetracer(io_NO3) + 2.0*dum_ocn(io_N2)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  - 2.5*dum_ocn(io_N2)
    end if
    fun_audit_combinetracer(io_N2)  = 0.0
    if (ocn_select(io_NH4)) then
       fun_audit_combinetracer(io_NO3) = fun_audit_combinetracer(io_NO3) + dum_ocn(io_NH4)
       fun_audit_combinetracer(io_ALK) = fun_audit_combinetracer(io_ALK) - dum_ocn(io_NH4)
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  - 2.0*dum_ocn(io_NH4)
    end if
    fun_audit_combinetracer(io_NH4) = 0.0
    if (ocn_select(io_SO4)) then
       fun_audit_combinetracer(io_O2)  = fun_audit_combinetracer(io_O2)  + 2.0*dum_ocn(io_SO4)
    end if
    if (ocn_select(io_H2S)) then
       fun_audit_combinetracer(io_ALK) = fun_audit_combinetracer(io_ALK) - 2.0*dum_ocn(io_H2S)
       fun_audit_combinetracer(io_SO4) = fun_audit_combinetracer(io_SO4) + dum_ocn(io_H2S)
    end if
    fun_audit_combinetracer(io_H2S) = 0.0
    if (ocn_select(io_Fe)) then
       fun_audit_combinetracer(io_Fe)  = fun_audit_combinetracer(io_Fe)  + dum_ocn(io_FeL)
    end if
    if (ocn_select(io_L)) then
       fun_audit_combinetracer(io_L)   = fun_audit_combinetracer(io_L)   + dum_ocn(io_FeL)
    end if
    fun_audit_combinetracer(io_FeL) = 0.0
    ! #### INSERT CODE FOR FURTHER SPECIAL CASES ################################################################################# !
    ! 
    ! ############################################################################################################################ !
  end function fun_audit_combinetracer
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! MISCELLANEOUS ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Copy tracer array
  SUBROUTINE sub_biogem_copy_ocntots(dum_ts,dum_ts1)
    USE biogem_lib
    ! dummy arguments
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(inout)::dum_ts  ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(inout)::dum_ts1 ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
    ! local variables
    INTEGER::i,j,k,l,io
    real::loc_ocn_mean_S,loc_ocn_tot_V
    ! initialize local variables
    ! NOTE: this is a fudge to avoid compiler warnings for unused variables ...
    !       (the variables in question are only used depending on a specific compile-time option)
    loc_ocn_mean_S = 0.0
    loc_ocn_tot_V  = 0.0
    if (ctrl_misc_Snorm) then
       ! [SALINITY NORMALIZED SCHEME]
       ! calculate total ocean mass
       loc_ocn_tot_V = sum(phys_ocn(ipo_V,:,:,:))
       ! calculate mean ocean salinity
       loc_ocn_mean_S = SUM(ocn(io_S,:,:,:)*phys_ocn(ipo_V,:,:,:))/loc_ocn_tot_V
       ! copy GOLDSTEIn <ts> array values from the relevant <ocn> array of BioGeM
       ! NOTE: leave T (index 1) and S (index 2) well alone ;-)
       ! NOTE: no offset (array: <tstoocn_offset()>) required for biogeochem-only tracers
       ! NOTE: normalize by relative salinity deviation from ocean mean
       DO i=1,n_i
          DO j=1,n_j
             DO k=goldstein_k1(i,j),n_k
                DO l=3,n_l_ocn
                   io = conv_iselected_io(l)
                   dum_ts(l,i,j,k)  = ocn(io,i,j,k)*(loc_ocn_mean_S/ocn(io_S,i,j,k))
                   dum_ts1(l,i,j,k) = dum_ts(l,i,j,k)
                end do
             END DO
          END DO
       END DO
    else
       ! [NON-SALINITY NORMALIZED SCHEME]
       DO i=1,n_i
          DO j=1,n_j
             DO k=goldstein_k1(i,j),n_k
                DO l=3,n_l_ocn
                   io = conv_iselected_io(l)
                   dum_ts(l,i,j,k)  = ocn(io,i,j,k)
                   dum_ts1(l,i,j,k) = dum_ts(l,i,j,k)
                end do
             END DO
          END DO
       END DO
    end if
  END SUBROUTINE sub_biogem_copy_ocntots
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Copy tracer array
  SUBROUTINE sub_biogem_copy_tstoocn(dum_ts)
    USE biogem_lib
    ! dummy arguments
    REAL,DIMENSION(intrac_ocn,n_i,n_j,n_k),INTENT(in)::dum_ts ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
    ! local variables
    INTEGER::i,j,k
    ! copy BioGeM <ocn> array values from the relevant <ts> (or <ts1>) array of GOLDSTEIN
    ! NOTE: restrict to T and S
    DO i=1,n_i
       DO j=1,n_j
          DO k=1,n_k
             IF (k >= goldstein_k1(i,j)) THEN
                ocn(io_T,i,j,k) = dum_ts(1,i,j,k) + tstoocn_offset(1)
                ocn(io_S,i,j,k) = dum_ts(2,i,j,k) + tstoocn_offset(2)
             END IF
          END DO
       END DO
    END DO
  END SUBROUTINE sub_biogem_copy_tstoocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! Copy of GOLDSTEIn overturning streamfunction calculation
  SUBROUTINE sub_calc_psi(dum_u,dum_opsi,dum_opsia,dum_opsip,dum_zpsi,dum_opsia_minmax,dum_opsip_minmax)
    ! dummy arguments
    REAL,INTENT(in),DIMENSION(3,n_i,n_j,n_k)::dum_u
    REAL,INTENT(out),DIMENSION(0:n_j,0:n_k)::dum_opsi,dum_opsia,dum_opsip,dum_zpsi
    REAL,INTENT(out),DIMENSION(2)::dum_opsia_minmax,dum_opsip_minmax
    ! local variables
    INTEGER::i,j,k
    REAL::loc_ominp,loc_omaxp
    REAL::loc_omina,loc_omaxa
    REAL,DIMENSION(n_j,n_k)::loc_ou,loc_zu
    REAL,DIMENSION(0:n_j,0:n_k)::loc_opsi,loc_opsia,loc_opsip,loc_zpsi
    ! Calculate meridional overturning streamfunction opsi on C grid only
    loc_opsi(:,:)  = 0.0
    loc_opsia(:,:) = 0.0
    loc_opsip(:,:) = 0.0
    DO j=1,n_j-1
       DO k=1,n_k-1
          loc_ou(j,k) = 0.0
          DO i=1,n_i
             loc_ou(j,k) = loc_ou(j,k) + goldstein_cv(j)*dum_u(2,i,j,k)*goldstein_dphi
          END DO
          loc_opsi(j,k) = loc_opsi(j,k-1) - goldstein_dz(k)*loc_ou(j,k)
       END DO
    END DO
    ! Pacific overturning streamfunction
    loc_ominp = 0.0
    loc_omaxp = 0.0
    DO j=goldstein_jsf+1,n_j-1
       DO k=1,n_k-1
          loc_ou(j,k) = 0.0
          DO i=goldstein_ips(j),goldstein_ipf(j)
             loc_ou(j,k) = loc_ou(j,k) + goldstein_cv(j)*dum_u(2,i,j,k)*goldstein_dphi
          ENDDO
          loc_opsip(j,k) = loc_opsip(j,k-1) - goldstein_dz(k)*loc_ou(j,k)
          IF(loc_opsip(j,k) < loc_ominp) loc_ominp = loc_opsip(j,k)
          IF(loc_opsip(j,k) > loc_omaxp) loc_omaxp = loc_opsip(j,k)
       ENDDO
    ENDDO
    dum_opsip_minmax(1) = loc_ominp
    dum_opsip_minmax(2) = loc_omaxp
    ! Atlantic overturning streamfunction
    ! NOTE: Atlantic calculation hacked so that only the deeper 1/2 of the maximum is calculated
    loc_omina = 0.0
    loc_omaxa = 0.0
    DO j=goldstein_jsf+1,n_j-1
       DO k=1,n_k-1
          loc_ou(j,k) = 0.0
          DO i=goldstein_ias(j),goldstein_iaf(j)
             loc_ou(j,k) = loc_ou(j,k) + goldstein_cv(j)*dum_u(2,i,j,k)*goldstein_dphi
          ENDDO
          loc_opsia(j,k) = loc_opsia(j,k-1) - goldstein_dz(k)*loc_ou(j,k)
          IF((loc_opsia(j,k) < loc_omina) .AND. (k <= n_k/2)) loc_omina = loc_opsia(j,k)
          IF((loc_opsia(j,k) > loc_omaxa) .AND. (k <= n_k/2)) loc_omaxa = loc_opsia(j,k)
       ENDDO
    ENDDO
    dum_opsia_minmax(1) = loc_omina
    dum_opsia_minmax(2) = loc_omaxa
    !
    loc_zpsi(:,:) = 0.0
    DO i=1,n_i-1
       DO k=1,n_k-1
          loc_zu(i,k) = 0
          DO j=1,n_j
             loc_zu(i,k) = loc_zu(i,k) + dum_u(1,i,j,k)/goldstein_c(j)*goldstein_ds
          ENDDO
          loc_zpsi(i,k) = loc_zpsi(i,k-1) - goldstein_dz(k)*loc_zu(i,k)
       ENDDO
    ENDDO
    ! set results arrays
    dum_opsi(1:n_j,1:n_k)  = loc_opsi(1:n_j,1:n_k)
    dum_opsia(1:n_j,1:n_k) = loc_opsia(1:n_j,1:n_k)
    dum_opsip(1:n_j,1:n_k) = loc_opsip(1:n_j,1:n_k)
    dum_zpsi(1:n_j,1:n_k)  = loc_zpsi(1:n_j,1:n_k)
  END SUBROUTINE sub_calc_psi
  ! ****************************************************************************************************************************** !


END MODULE biogem_box

! Reference:
! M. Siddall, G.M. Henderson, N.R. Edwards, M. Frank, S.A. Mueller, T.F. Stocker, F. Joos (2005),
! "231Pa/230Th fractionation by ocean transport, biogenic particle flux and particle type",
! Earth Planet. Sci. Lett., 237, pp. 135-155
