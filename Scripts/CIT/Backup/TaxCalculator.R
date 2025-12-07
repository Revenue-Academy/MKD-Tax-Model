
library(readxl)
cit_simulation_parameters_raw <- read_excel("CIT-Parameters.xlsx")

cit_simulation_parameters_updated<-cit_simulation_parameters_raw



library(readr)
dt_cit <- read_csv("Data/CIT/cit_data_macedonia.csv")


growth_factors<-read_csv("Data/CIT/growfactors_cit_macedonia.csv")



SimulationYear<-2021

weights_cit<-read_csv("Data/CIT/cit_weights_macedonia.csv")



# -------------------------------------------------------------------------


setDTthreads(threads = 8)

get_param_fun <- function(params_dt, param_name) {
  params_dt[Parameters == param_name, Value]
}

            base_year <- unique(dt_cit$Year)[1]
            end_year <- base_year + 5
            
            
            simulation_year <- SimulationYear  # Year from slider
            forecast_horizon <- seq(base_year, end_year)
            scenario_years<-forecast_horizon
            
            # Define the scenarios
            scenarios <- c("t0", "t1", "t2", "t3", "t4","t5")
            
            # Simulation parameters must be in data.table
            cit_simulation_parameters_raw <- cit_simulation_parameters_raw %>% data.table()
            cit_simulation_parameters_updated <- cit_simulation_parameters_updated %>% data.table()


# 1. Tax Calculation Function -------------------------------------------------------
start.time <- proc.time()
tax_calc_fun <- function(dt_scn, params_dt) {
  CITrate <- get_param_fun(params_dt, "CITrate")
  toggle_voluntary_pension_fund <- get_param_fun(params_dt, "toggle_voluntary_pension_fund")
  toggle_insurance_premiums <- get_param_fun(params_dt, "toggle_insurance_premiums")
  toggle_insurance_premiums_management_bodies <- get_param_fun(params_dt, "toggle_insurance_premiums_management_bodies")
  toggle_10_fiscal_systems <- get_param_fun(params_dt, "toggle_10_fiscal_systems")
  toggle_sport_donation <- get_param_fun(params_dt, "toggle_sport_donation")
  toggle_TIDZ <- get_param_fun(params_dt, "toggle_TIDZ")
  toggle_dividends_capitals_r <- get_param_fun(params_dt, "toggle_dividends_capitals_r")
  toggle_reinvested_profits <- get_param_fun(params_dt, "toggle_reinvested_profits")

  
# I. ESTIMATION TAX LIABILITY FOR INCOME FROM LABOR --------------
                      
                      
                      
                      ## 1) ue_voluntary_pension_fund_r_calc (no function)
                      ## If `toggle_voluntary_pension_fund` is 0/1:
                      dt_scn[, ue_voluntary_pension_fund_r_calc :=
                               ue_voluntary_pension_fund_r * toggle_voluntary_pension_fund]            
                      
                      
                      # 2) If toggle_insurance_premiums is 0/1:
                      dt_scn[, ue_insurance_premiums_r_calc :=
                               ue_insurance_premiums_r * toggle_insurance_premiums]
                      
                      
                      # 3) If toggle_insurance_premiums_management_bodies is 0/1:
                      dt_scn[, ue_insurance_premiums_management_bodies_r_calc :=
                               ue_insurance_premiums_management_bodies_r * toggle_insurance_premiums_management_bodies]
                      
                      
                      # 4) ue

                      
                      ue_cols <- c(
                        "ue_not_related_bus_ent_r",
                        "ue_allowances_above_specified_amount_r",
                        "ue_allowances_employee_exp_not_specified_r",
                        "ue_meals_transportation_r",
                        "ue_hotel_not_documented_r",
                        "ue_sustenance_costs_night_time_r",
                        "ue_management_authorities_over_amount_r",
                        "ue_voluntary_pension_fund_r_calc",
                        "ue_insurance_premiums_r_calc",
                        "ue_volunteers_public_works_r",
                        "ue_hidden_payment_profits_r",
                        "ue_deficits_not_caused_emergencies_r",
                        "ue_representational_costs_r",
                        "ue_donation_sponsorship_5pct_r",
                        "ue_sport_r",
                        "ue_int_loans_not_used_applied_business_r",
                        "ue_insurance_premiums_management_bodies_r_calc",
                        "ue_withholding_taxes_third_parties_r",
                        "ue_tax_fines_penalty_enforcement_r",
                        "ue_scholarship_r",
                        "ue_shinkage_waste_breakage_r",
                        "ue_writeoff_non_collected_claims_r",
                        "ue_net_amount_income_social_insurancecontributions_r",
                        "ue_practical_training_students_r",
                        "ue_cost_training_students_r",
                        "ue_depreciation_rev_tang_intan_assets_r",
                        "ue_depreciation_tangible_intangible_assests_higher_r",
                        "ue_present_value_PRO_r",
                        "ue_uncollected_receivables_r",
                        "ue_correction_unpaid_demand_r",
                        "ue_positive_difference_expenditures_r",
                        "ue_pos_diff_inc_arm_length_principle_r",
                        "ue_interest_loans_credit_instituion_r",
                        "ue_interest_penalty_credit_institutions_r",
                        "ue_interest_loans_nonresidential_20pct_capital_r",
                        "ue_other_adjustments_r"
                      )
                      
                      # SAME sum in both Python branches; apply when either condition is TRUE
                      dt_scn[, ue_total_calc :=
                               fifelse(
                                 (toggle_TIDZ == 1 & special_tax_treatment == 0) |
                                   (toggle_TIDZ == 0 & special_tax_treatment == 2),
                                 rowSums(.SD, na.rm = TRUE),
                                 NA_real_   # or 0 if you prefer: replace NA_real_ with 0
                               ),
                             .SDcols = ue_cols]
                      
                      
"
III. Estimation of Tax base (I + II) (AOP 01 + AOP02)

AOP*         Descriptions
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

AOP01	Financial results in balance income sheet
AOP02	Non-deductable expenses for tax purpose (AOP03 to AOP29)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*) Field of tax return

"         
                      
# 6) TAX BASE: tax_base_calc

dt_scn[, tax_base_calc := financial_results_r + ue_total_calc]                  
                      
                      
                      
                      
"
IV. Reduction of the tax base (AOP 42+ AOP 43+ AOP 44+ AOP 45+ AOP 46+ AOP 47+ AOP 48)

AOP*         Descriptions
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

AOP42	The amount of the billed demand for which in the previous period has increased the given basis
AOP43	The amount of the returned part of the loan for which there was an increase in the tax base in the previous tax periods
AOP44	Amount of amortization costs above the amount calculated by applying the amortization rates determined by the nomenclature of depreciation funds and the annual amortization rates for the above
AOP45	The amount of the unpaid compensations above the amounts determined in Article 9 paragraph (1) points 2), 3-b), 4), 5), 5-a) and 6) of the Law on Profit Tax, for which in the previous period was carried out increase in the tax base
AOP46	Dividends realized by participation in the capital of another taxpayer, taxed with tax on profit and payment
AOP47	Part of the loss reduced for unrecognized expenses, carried over from the previous year
AOP48	Amount of investments made from profits (reinvested)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*) Field of tax return

"         
                      
                      
                      
# 7) REINVESTED PROFITS: rb_reinvested_profits_calc

dt_scn[, rb_reinvested_profits_calc :=
         rb_reinvested_profits_r * toggle_reinvested_profits]
                      
                      
# 8) DIVIDENDS (CAPITALS): rb_dividends_capitals_r_calc              

# If toggle_dividends_capitals_r is 0/1:
dt_scn[, rb_dividends_capitals_r_calc :=
         rb_dividends_capitals_r * toggle_dividends_capitals_r]

# 9) TAX REDUCTIONS (SUM OF COMPONENTS): rb_tax_reductions_calc

dt_scn[, rb_tax_reductions_calc :=
         rowSums(.SD, na.rm = TRUE),
       .SDcols = c(
         "rb_amount_collected_claims_tax_base_increased_r",
         "rb_repaid_part_loan_tb_increased_r",
         "rb_amount_depreciation_costs_r",
         "rb_unpaid_allowances_r",
         "rb_dividends_capitals_r_calc",
         "rb_part_loss_previous_years_r",
         "rb_reinvested_profits_calc"
       )]


"
V. Tax base after deductions (III- IV) (AOP 40 - AOP 41)

AOP*         Descriptions
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
III. Estimation of Tax base 
IV. Deduction of tax base (AOP 42+ AOP 43+ AOP 44+ AOP 45+ AOP 46+ AOP 47+ AOP 48)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*) Field of tax return

"
#(net taxable income )


# 10) TAX BASE AFTER REDUCTIONS: tb_ar_deductions_calc
# =========================
# If tax_base_calc < 0 → 0, else → tax_base_calc - rb_tax_reductions_calc
dt_scn[, tb_ar_deductions_calc :=
         fifelse(tax_base_calc < 0, 0, tax_base_calc - rb_tax_reductions_calc)]


# 11) NET TAX BASE: Net_tax_base

# Direct assignment per Python: Net_tax_base = tb_ar_deductions_calc
dt_scn[, Net_tax_base := tb_ar_deductions_calc]


                      
"
VI. Calculated corporate income tax  (V x Tax rate) 

AOP*         Descriptions
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
V. Calculated corporate income tax (V x 10%)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*) Field of tax return

"

# =========================
# 12) CORPORATE INCOME TAX (CIT): tb_ar_income_CIT_calc
# =========================

dt_scn[, tb_ar_income_CIT_calc := tb_ar_deductions_calc * CITrate]


"
VII. Reduction of calculated corporate income tax (AOP 52+AOP 53+AOP 54+AOP 55)

AOP*         Descriptions
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AOP52	Reduction of the tax by the value of the purchased and deployed up to 10 fiscal systems or cash transaction registration systems
AOP53	Amount of the tax contained in taxed incomes / profits abroad (withholding tax) up to the prescribed rate
AOP54	Tax paid by the subsidiary abroad for the profit included in the income of the parent legal entity in the Republic of Macedonia, but not more than the amount of the tax using the prescribed rate in the Law on CIT
AOP55	Amount of the calculated tax deduction for a given donation determined in accordance with Article 30-a of the Law on CIT

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*) Field of tax return

"

# 13) CASH PAY SYSTEMS (10% fiscal systems): tb_ar_cash_pay_systems_calc
# =========================
# If toggle_10_fiscal_systems is 0/1:
dt_scn[, tb_ar_cash_pay_systems_calc :=
         tb_ar_cash_pay_systems_r * toggle_10_fiscal_systems]


# 14) SPORT DONATION RELIEF: tb_ar_tax_relief_donation_r_calc
# =========================
# If toggle_sport_donation is 0/1:
dt_scn[, tb_ar_tax_relief_donation_r_calc :=
         tb_ar_tax_relief_donation_r * toggle_sport_donation]


# =========================
# 15) TAX CREDITS (SUM): tb_ar_deductions_calculated_calc
# =========================
dt_scn[, tb_ar_deductions_calculated_calc :=
         rowSums(.SD, na.rm = TRUE),
       .SDcols = c(
         "tb_ar_cash_pay_systems_calc",
         "tb_ar_tax_gains_abroad_r",
         "tb_ar_tax_paid_branch_abroad_r",
         "tb_ar_tax_relief_donation_r_calc"
       )]



"
VIII. Calculated tax after reduction  (VI-VII) (ova ne treba)
AOP*         Descriptions
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
VI. Calculated corporate income tax  (V x Tax rate) 
VII. Deductions of calculated CIT (AOP 52 + AOP 53 + AOP 54 + AOP 55)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*) Field of tax return

"

# =========================
# 16) CIT LIABILITY AFTER CREDITS: tb_ar_calculated_tax_after_reduction_calc
# =========================
# If (CIT - credits) < 0 → 0, else → CIT - credits
dt_scn[, tb_ar_calculated_tax_after_reduction_calc :=
         fifelse(tb_ar_income_CIT_calc - tb_ar_deductions_calculated_calc < 0,
                 0,
                 tb_ar_income_CIT_calc - tb_ar_deductions_calculated_calc)]

# (Optional NA-safe variant)
# dt_scn[, tb_ar_calculated_tax_after_reduction_calc := {
#   diff <- fcoalesce(tb_ar_income_CIT_calc, 0) - fcoalesce(tb_ar_deductions_calculated_calc, 0)
#   fifelse(diff < 0, 0, diff)
# }]


"
AOP 59 Amount for additional payment / overpaid amount (AOP 56-AOP 57-AOP 58) (ova ne treba sproed RK)

AOP*         Descriptions
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AOP 56 VIII. Calculated tax after reduction  (VI-VII)
AOP 57 Paid advance tax on profits for the current period
AOP 58 Amount of overpaid income tax carried over from previous tax periods
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*) Field of tax return

"


# =========================
# 17) AMOUNT TO PAY / OVERPAID: tb_ar_tax_overpaid_calc
# =========================
# Formula: calculated_tax_after_reduction - advance_tax_paid - overpaid_prev_periods
dt_scn[, tb_ar_tax_overpaid_calc :=
         tb_ar_calculated_tax_after_reduction_calc -
         tb_ar_paid_advance_tax_profits_r -
         tb_ar_over_paid_previous_tax_periods_r]


# =========================
# 18) CARRIED-FORWARD LOSSES: CF_losses
# =========================
# Direct translation: CF_losses = rb_part_loss_previous_years_r
dt_scn[, CF_losses := rb_part_loss_previous_years_r]



# =========================
# 19) LOSS CARRY-FORWARD (TEST STUB): newloss1..newloss10 = 0
# =========================

"not implemented !!!"
# Straight translation of the test function’s effect: set all newloss* to 0
dt_scn[, paste0("newloss", 1:10) := 0]


# =========================
# 20) NET TAX BASE (BEHAVIOR STUB): Net_tax_base_behavior = 0
# =========================


"not implemented !!!"
# Direct translation of the provided function’s effect: set to 0
dt_scn[, Net_tax_base_behavior := 0]

    

# =========================
# 21) TOTAL TAX LIABILITY (CIT): citax
# =========================
# Direct translation of current Python logic: citax = tb_ar_calculated_tax_after_reduction_calc
dt_scn[, citax := tb_ar_calculated_tax_after_reduction_calc]
                      
                      
# =========================
# NET TAXABLE INCOME AFTER REDUCTIONS: tb_ar_deductions_calc
# =========================
# If tax_base_calc < 0 → 0, else → tax_base_calc - rb_tax_reductions_calc
dt_scn[, tb_ar_deductions_calc :=
         fifelse(tax_base_calc < 0, 0, tax_base_calc - rb_tax_reductions_calc)]


                      
#   # 1.Calculation of tax base wages ----------------------------------------------------
#                       dt_scn[, tax_base_w := {
#                                               personal_allowance_new <- g_total_personal_allowance_l * sf_per_allowance
#                                               tax_base_wages1 <- pmax(g_Wages_l - total_ssc - personal_allowance_new, 0)
#                                               tax_base_wages_diplomatic_consular_l <- g_WagesDiplomaticConsular_l - d_WagesDiplomaticConsular_l
#                                               tax_base_wages1 + tax_base_wages_diplomatic_consular_l
#                                             }]
#       
#   # 2.Calculation of tax base for income of the basis of sale of own agricultural products ------------------------------------------------------------------------
#                       dt_scn[, tax_base_agr := g_AgriculturalProductsOwn_l - (g_AgriculturalProductsOwn_l * rate_ded_income_agr_l)]
#                       dt_scn[, pit_tax_agr := tax_base_agr*rate1] 
#   # 3.Copyright Income Artistic Photography  ------------------------------------------
#                      dt_scn[, tax_base_CopyrightIncomeArtisticPhotography_l := 
#                                                                              g_CopyrightIncomeArtisticPhotography_l - 
#                                                                              (g_CopyrightIncomeArtisticPhotography_l * rate_deductions_CopyrightIncomeArtisticPhotography_l)
#                                                                     ]
#                       
#                       dt_scn[, pit_tax_CopyrightIncomeArtisticPhotography_l := tax_base_CopyrightIncomeArtisticPhotography_l*rate1]
#   # 4.Copyright Income Music Ballet  -------------------------------------
#                       dt_scn[, tax_base_CopyrightIncomeMusicBallet_l := 
#                                g_CopyrightIncomeMusicBallet_l - 
#                                (g_CopyrightIncomeMusicBallet_l * rate_deductions_CopyrightIncomeMusicBallet_l)
#                       ]
#                       dt_scn[, pit_tax_CopyrightIncomeMusicBallet_l := tax_base_CopyrightIncomeMusicBallet_l*rate1] 
#   # 5.Copyright Income Paintings)---------------
#                       dt_scn[, tax_base_CopyrightIncomePaintingsSculptural_l := 
#                                g_CopyrightIncomePaintingsSculptural_l - 
#                                (g_CopyrightIncomePaintingsSculptural_l * rate_deductions_CopyrightIncomePaintingsSculptural_l)
#                       ]
#                       
#                       dt_scn[, pit_tax_CopyrightIncomePaintingsSculptural_l := tax_base_CopyrightIncomePaintingsSculptural_l*rate1]
# 
#   # 6.Copyright Income Translations Lectures ----------------------------------------------------------------------
#                       dt_scn[, tax_base_CopyrightIncomeTranslationsLectures_l := 
#                                g_CopyrightIncomeTranslationsLectures_l - 
#                                (g_CopyrightIncomeTranslationsLectures_l * rate_deductions_CopyrightIncomeTranslationsLectures_l)
#                       ]
#                     
#                       dt_scn[, pit_tax_CopyrightIncomeTranslationsLectures_l := tax_base_CopyrightIncomeTranslationsLectures_l*rate1] 
#   
#   # 7.Successor or holder of the copyrights and related rights ----------------------------------------------------------------------
#                       dt_scn[, tax_base_CopyrightIncomeSuccessor_l := 
#                                g_CopyrightIncomeSuccessor_l - 
#                                (g_CopyrightIncomeSuccessor_l * rate_deductions_CopyrightIncomeSuccessor_l)
#                       ]  
#                       
#                       dt_scn[, pit_tax_CopyrightIncomeSuccessor_l := tax_base_CopyrightIncomeSuccessor_l*rate1] 
#   
#   # 8. Work Income ----------------------------------------------------------------------
#                       dt_scn[, tax_base_WorkIncome_l := 
#                                g_WorkIncome_l - 
#                                (d_WorkIncome_l * sf_ded_work_l)
#                       ]
#                       
#                       dt_scn[, pit_tax_WorkIncome_l := tax_base_WorkIncome_l*rate1] 
# 
#   # 9.Total tax base OTHER INCOME from labor (not include wages )----------------------------------------------------------------------
#                       dt_scn[, tax_base_other := pmax(
#                                                       tax_base_agr + 
#                                                       tax_base_CopyrightIncomeArtisticPhotography_l + 
#                                                       tax_base_CopyrightIncomeMusicBallet_l + 
#                                                       tax_base_CopyrightIncomePaintingsSculptural_l + 
#                                                       tax_base_CopyrightIncomeTranslationsLectures_l + 
#                                                       tax_base_WorkIncome_l + 
#                                                       g_TemporaryContracts_l + 
#                                                       g_AgriculturalProducts_l + 
#                                                       g_IndependentActivity_l + 
#                                                       tax_base_CopyrightIncomeSuccessor_l, 
#                                                       0)]
#         
#   # 10. Calculation for PIT for income for labor -------------------------------------------
# 
#                    dt_scn[, tti_w_I := tax_base_w + tax_base_other]
#                            
#                             
#                     dt_scn[, pit_w := 
#                                      (rate1 * pmin(tti_w_I, tbrk1) +
#                                         rate2 * pmin(tbrk2 - tbrk1, pmax(0, tti_w_I - tbrk1)) +
#                                         rate3 * pmin(tbrk3 - tbrk2, pmax(0, tti_w_I - tbrk2)) +
#                                         rate4 * pmax(0, tti_w_I - tbrk3))
#                             ]
#                             
# 
#                     
# # II. ESTIMATION TAX LIABILITY FOR INCOME FROM CAPITAL ---------------
#    # 1. Estimation of tax base for capital incomes without deductions (prescribed cost) --------
#                     dt_scn[, tax_base_IndustrialPropertyRights_c := 
#                              g_IndustrialPropertyRights_c- 
#                              (g_IndustrialPropertyRights_c* rate_deductions_IndustrialPropertyRights_c)
#                     ]
#                     
#                     dt_scn[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c*capital_income_rate_c] 
# 
#    # 2.Income on the basis of lease -----------------------------------------------
#                     dt_scn[, tax_base_Lease_c := 
#                              g_Lease_c- 
#                              (g_Lease_c* rate_deductions_Lease_c)
#                     ]
#             
#                     dt_scn[, pit_tax_Lease_c := tax_base_Lease_c*capital_income_rate_c]
#           
#           
#    # 3. Income on the basis of lease of equipped residential and business premises-------------------------------
#                   dt_scn[, tax_base_LeaseBusiness_c := 
#                            g_LeaseBusiness_c- 
#                            (g_LeaseBusiness_c* rate_deductions_LeaseBusiness_c)
#                   ]
#           
#                   dt_scn[, pit_LeaseBusiness_c := tax_base_LeaseBusiness_c*capital_income_rate_c] 
#           
#           
#    # 4. Solid waste -------------------------------------------------------------
#                   dt_scn[, tax_base_SolidWaste_c := 
#                            g_SolidWaste_c- 
#                            (g_SolidWaste_c* rate_deductions_SolidWaste_c)
#                   ]
#                   
#                   dt_scn[, pit_SolidWaste_c := tax_base_SolidWaste_c*capital_income_rate_c] 
#           
#    # 5. Games of Chance 
#                   
#         # 5.1 General ----------------------------------- --------   
#                   dt_scn[, tax_base_GamesofChanceGeneral_c := 
#                            g_GamesofChanceGeneral_c
#                            
#                   ]
#                   
#                   dt_scn[, pit_GamesofChanceGeneral_c := tax_base_GamesofChanceGeneral_c*capital_income_rate_g] 
#                   
# 
#       # 5.2 Specific----------------------------------- --------
#                 dt_scn[, tax_base_GamesofChanceSpecific_c := 
#                          g_GamesofChanceSpecific_c- 
#                          (d_GamesofChanceSpecific_c* sf_ded_games_s)
#                 ]
#                 
#                 dt_scn[, pit_GamesofChanceSpecific_c := tax_base_GamesofChanceSpecific_c*capital_income_rate_g] 
#            
#                 
#     # 5.3 Betting shop --------------------------------------------------------
#           dt_scn[, tax_base_GamesofChanceBettingShop_c :=
#                    g_GamesofChanceBettingShop_c-(d_GamesofChanceBettingShop_c * sf_ded_betting_h)
#           ]
#           
#           dt_scn[, pit_GamesofChanceBettingShop_c := tax_base_GamesofChanceBettingShop_c*capital_income_rate_g] 
#           
#           
#     # 6. Industrial Property Rights Successor ---------------------------------------
#             dt_scn[, tax_base_IndustrialPropertyRightsSuccessor_c := 
#                      g_IndustrialPropertyRightsSuccessor_c]
#             
#             dt_scn[, pit_IndustrialPropertyRightsSuccessor_c := tax_base_IndustrialPropertyRightsSuccessor_c*capital_income_rate_c] 
#               
#     # 7.  Income from insurance---------------
#             dt_scn[, tax_base_Insurance_c := 
#                      g_Insurance_c]
#             
#             dt_scn[, pit_Insurance_c := tax_base_Insurance_c*capital_income_rate_c] 
#             
#             
#     # 8.  Income from Interests---------------
#             dt_scn[, tax_base_Interests_c := 
#                      g_Interests_c]
#             
#             dt_scn[, pit_Interests_c := tax_base_Interests_c*capital_income_rate_c] 
#     # 9.  Other Income  ---------------  
#             
#             dt_scn[, tax_base_OtherIncome_c := 
#                      g_OtherIncome_c]
#             
#             dt_scn[, pit_OtherIncome_c := tax_base_OtherIncome_c*capital_income_rate_c] 
#     # 10.  Sublease ---------------  
#            dt_scn[, tax_base_Sublease_c := 
#                      g_Sublease_c
#                       ]
#             
#             dt_scn[, pit_Sublease_c := tax_base_Sublease_c*capital_income_rate_c] 
#             
#   
#     # 11. CapitalIncome_c -------------------------------------------------------
#             dt_scn[, tax_base_CapitalIncome_c := 
#                      g_CapitalIncome_c
#                    
#             ]
#             
#             dt_scn[, pit_CapitalIncome_c := tax_base_CapitalIncome_c*capital_income_rate_c] 
#     
# 
#     # 11a. ---------------------------------------------------------------------
#             # dt_scn[, tax_base_deposit_c :=
#             #                                 gross_int_t_dep
#             # 
#             # ]
#             # 
#             # dt_scn[, pit_Int_t_dep_c := tax_base_deposit_c*capital_income_rate_c]
# 
#             
#             
#             
#             
#   # 12. Total tax base based on capital income --------------------------------
#         # 12.1 Calculation for total tax base from capital income OTHER THAN games of chance----------------------
#             dt_scn[, tti_c_a := 
#                               g_IndustrialPropertyRightsSuccessor_c + g_Insurance_c + g_Interests_c + g_OtherIncome_c + g_Sublease_c +
#                               tax_base_IndustrialPropertyRights_c + tax_base_Lease_c + tax_base_LeaseBusiness_c + tax_base_SolidWaste_c +
#                               tax_base_Insurance_c + g_CapitalIncome_c
#                               #+tax_base_deposit_c
#                    ]
# 
#        
#         # 12.2 Calculation for total tax base from capital income ONLY from games of chance (15%)----------------
# 
#           dt_scn[, tti_c_g :=
#                                     tax_base_GamesofChanceGeneral_c+
#                                     tax_base_GamesofChanceSpecific_c +
#                                       tax_base_GamesofChanceBettingShop_c
#                                       
#                                   ]
# 
#             
#           
# 
#             
# 
#   
#         # 12.3 Total PIT on the base of income from capital----------------------
#       
# 
#             dt_scn[, pit_c :=pit_tax_IndustrialPropertyRights_c+
#                                                               pit_tax_Lease_c+
#                                                               pit_LeaseBusiness_c+
#                                                               pit_SolidWaste_c+
#                                                               pit_GamesofChanceSpecific_c+
#                                                               pit_GamesofChanceBettingShop_c+
#                                                               pit_IndustrialPropertyRightsSuccessor_c+
#                                                               pit_Insurance_c+
#                                                               pit_Interests_c+
#                                                               pit_OtherIncome_c+
#                                                               pit_Sublease_c+
#                                                               pit_CapitalIncome_c # +
#                                                               #pit_Int_t_dep_c
#                    ]
# 
# 
#          
            
            
  # III. ESTIMATION TOTAL PIT --------------------------------       
  # Total PIT ------------------------------------------------------
          
            # dt_scn[, total_taxbase := tti_c_g + tti_c_a + tti_w_I]
            # 
            # dt_scn[, pitax := pit_w + pit_c]
            # 
            # dt_scn[, total_net := g_total_gross-(total_ssc+g_total_personal_allowance_l+pitax)]
            # 
            
            
}    
# 2. Helper to Retrieve Growth Factors for Each Variable -------------------------------
                vars_to_grow <- c("financial_results_r", "ue_total_calc", "ue_not_related_bus_ent_r",
                                  "ue_allowances_above_specified_amount_r", "ue_allowances_employee_exp_not_specified_r",
                                  "ue_meals_transportation_r", "ue_hotel_not_documented_r", "ue_sustenance_costs_night_time_r",
                                  "ue_management_authorities_over_amount_r", "ue_voluntary_pension_fund_r",
                                  "ue_insurance_premiums_r", "ue_volunteers_public_works_r", "ue_hidden_payment_profits_r",
                                  "ue_deficits_not_caused_emergencies_r", "ue_representational_costs_r",
                                  "ue_donation_sponsorship_5pct_r", "ue_sport_r",
                                  "ue_int_loans_not_used_applied_business_r", "ue_insurance_premiums_management_bodies_r",
                                  "ue_withholding_taxes_third_parties_r", "ue_tax_fines_penalty_enforcement_r",
                                  "ue_scholarship_r", "ue_shinkage_waste_breakage_r", "ue_writeoff_non_collected_claims_r",
                                  "ue_net_amount_income_social_insurancecontributions_r", "ue_practical_training_students_r",
                                  "ue_cost_training_students_r", "ue_depreciation_rev_tang_intan_assets_r",
                                  "ue_depreciation_tangible_intangible_assests_higher_r", "ue_present_value_PRO_r",
                                  "ue_uncollected_receivables_r", "ue_correction_unpaid_demand_r",
                                  "ue_positive_difference_expenditures_r", "ue_pos_diff_inc_arm_length_principle_r",
                                  "ue_interest_loans_credit_instituion_r", "ue_interest_penalty_credit_institutions_r",
                                  "ue_interest_loans_nonresidential_20pct_capital_r", "ue_other_adjustments_r",
                                  "tax_base_calc", "rb_tax_reductions_calc", "rb_amount_collected_claims_tax_base_increased_r",
                                  "rb_repaid_part_loan_tb_increased_r", "rb_amount_depreciation_costs_r", "rb_unpaid_allowances_r",
                                  "rb_dividends_capitals_r", "rb_part_loss_previous_years_r", "rb_reinvested_profits_r",
                                  "tb_ar_deductions_calc", "tb_ar_income_CIT_calc", "tb_ar_deductions_calculated_calc",
                                  "tb_ar_cash_pay_systems_r", "tb_ar_tax_gains_abroad_r", "tb_ar_tax_paid_branch_abroad_r",
                                  "tb_ar_tax_relief_donation_r", "tb_ar_calculated_tax_after_reduction_calc",
                                  "tb_ar_paid_advance_tax_profits_r", "tb_ar_over_paid_previous_tax_periods_r",
                                  "tb_ar_tax_overpaid_calc", "sd_reinvestment_profit_r", "sd_loss_previous_year_r",
                                  "sd_realized_loss_r", "sd_transferred_unused_part_tax_r",
                                  "sd_transferred_unused_part_tax_aborad_r", "sd_realized_income_r",
                                  "sd_total_cost_donation_used_r", "sd_total_cost_donation_not_used_r",
                                  "sd_total_cost_sponsorships_used_r", "sd_total_cost_sponsorships_not_used_r",
                                  "sd_total_cost_donation_sport_r")

                
                get_growth_factor_row <- function(scenario) {
                  gf_row <- growth_factors[scenarios == scenario]
                  out <- numeric(length(vars_to_grow))
                  names(out) <- vars_to_grow
                  
                  for (v in vars_to_grow) {
                    gf_col <- sub("_adjusted", "", v)  
                    out[v] <- gf_row[[gf_col]]
                  }
                  return(out)
                }

# 3. Business as usual  ------------------------------------------------------
          
          CIT_BU_list <- list()
          
          # Start from baseline
          dt_scn_BU <- copy(dt_cit)
          
          for (s in scenarios) {
            
            # 1) Retrieve scenario growth factors
            gf_values <- get_growth_factor_row(s)
            
            # 2) Multiply each variable by gf_values[v] * weights[[s]]
            for (v in vars_to_grow) {
              dt_scn_BU[, (v) := get(v) * gf_values[v] * weights_cit[[s]]]
            }
            
            # 3) Row-wise tax logic
            tax_calc_fun(dt_scn_BU, cit_simulation_parameters_raw)
            
            # 4) ADD a 'weight' column that references weights[[s]]
            dt_scn_BU[, weight := weights_cit[[s]]]
            
            # 5) Store in CIT_BU_list
            CIT_BU_list[[s]] <- copy(dt_scn_BU)
          }

# 4. Simulation --------------------------------------------------------------
          start_index <- match(SimulationYear, scenario_years) 
          
          CIT_SIM_list <- list()

          if (start_index > 1) {
            for (i in seq_len(start_index - 1)) {
              s_early <- scenarios[i]
              CIT_SIM_list[[s_early]] <- copy(CIT_BU_list[[s_early]])
            }
          }
          
          # 2) Determine the starting data for re-simulation
          if (start_index == 1) {
            # SimulationYear=2021 => start from original dt_cit
            dt_scn_SIM <- copy(dt_cit)
          } else {
            # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
            prev_scenario <- scenarios[start_index - 1]
            dt_scn_SIM <- copy(CIT_BU_list[[prev_scenario]])
          }
          
          # 3) Chain from scenario index = start_index .. 5
          for (i in seq(from = start_index, to = length(scenarios))) {
            s <- scenarios[i]
            
            gf_values <- get_growth_factor_row(s)
            
            # Multiply each variable by growth factor * row-weight for scenario s
            for (v in vars_to_grow) {
              dt_scn_SIM[, (v) := get(v) * gf_values[v] * weights_cit[[s]]]
            }
            
            # Run row-wise calculations with updated parameters
            tax_calc_fun(dt_scn_SIM, cit_simulation_parameters_updated)
            
            # **Add a 'weight' column** with the row-specific weights_cit for scenario s
            dt_scn_SIM[, weight := weights_cit[[s]]]
            
            # Store final data in CIT_SIM_list
            CIT_SIM_list[[s]] <- copy(dt_scn_SIM)
          }
          
          message("Block 2 (CIT_SIM_list) done, including early years from CIT_BU_list, plus 'weight' column.\n")
          message("All done!\n")
          
         # rm(dt_scn_BU, dt_scn_SIM)

      # 5. Aggregation of simulated data -----------------------------------------------------
          
          summarize_PIT_fun_dt <- function(PIT_list, suffix) {
            # 1) Loop (via lapply) over each named data.table in the list
            # 2) Sum columns matching regex ^(calc|cit)
            # 3) Collect results into one data.table
            summary_list <- lapply(names(PIT_list), function(scenario_name) {
              dt <- PIT_list[[scenario_name]]
              
              # Select columns starting with "calc" or "cit", and sum them
              # .SDcols = patterns("^(calc|cit)") picks columns with those prefixes
              sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = patterns("^(calc|cit)")]
              
              # Add scenario name as a column
              sums_dt[, scenarios := scenario_name]
              
              # Make 'scenarios' the first column
              setcolorder(sums_dt, c("scenarios", setdiff(names(sums_dt), "scenarios")))
              sums_dt
            })
            
            # Combine all scenario summaries into one data.table
            result_dt <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
            
            # Append 'suffix' to every column except 'scenarios'
            old_names <- setdiff(names(result_dt), "scenarios")
            new_names <- paste0(old_names, suffix)
            setnames(result_dt, old_names, new_names)
            
            # Convert to data.frame if you want the same final type as your original code
            result_df <- as.data.frame(result_dt)
            
            return(result_df)
          }
          
          
      
      # Function to sum the specified columns in the list and store the results in a data frame

          summary_SIM <- summarize_PIT_fun_dt(CIT_SIM_list, "_sim")
          summary_BU  <- summarize_PIT_fun_dt(CIT_BU_list, "_bu")
          
      
      
      merged_CIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_CIT_BU_SIM$year <- as.character(forecast_horizon)
      merged_CIT_BU_SIM <- merged_CIT_BU_SIM[, c("year", names(merged_CIT_BU_SIM)[-length(merged_CIT_BU_SIM)])]
      
      numeric_columns <- sapply(merged_CIT_BU_SIM, is.numeric)
      merged_CIT_BU_SIM[, numeric_columns] <- merged_CIT_BU_SIM[, numeric_columns] / 1e06



# 6. Decile ------------------------------------------------------------------
       
      
      # calc_weighted_groups_in_one_pass <- function(DT, inc_col = "g_total_gross", w_col = "weight") {
      #   # 1. Keep track of original row order so we can restore it after sorting
      #   DT[, row_id__tmp := .I]
      #   
      #   # 2. Sort by income (use setorderv for a character column name)
      #   setorderv(DT, inc_col)
      #   
      #   # 3. Compute the cumulative sum of weight
      #   #    (handle NA weights as 0, adjust if you prefer a different approach)
      #   DT[, w_cumsum__tmp := cumsum(fifelse(is.na(get(w_col)), 0, get(w_col)))]
      #   
      #   # 4. Get the total weight
      #   total_w <- DT[.N, w_cumsum__tmp]
      #   
      #   # 5. Define breakpoints for deciles (10 groups) and centiles (100 groups)
      #   decile_breaks  <- seq(0, total_w, length.out = 11)   # 11 points => 10 intervals
      #   centile_breaks <- seq(0, total_w, length.out = 101)  # 101 points => 100 intervals
      #   
      #   # 6. Assign decile_group and centile_group
      #   DT[, decile_group  := findInterval(w_cumsum__tmp, decile_breaks,  rightmost.closed = TRUE)]
      #   DT[, centile_group := findInterval(w_cumsum__tmp, centile_breaks, rightmost.closed = TRUE)]
      #   
      #   # 7. Ensure the top boundary doesn't exceed the number of groups
      #   DT[, decile_group  := pmin(decile_group,  10)]
      #   DT[, centile_group := pmin(centile_group, 100)]
      #   
      #   # 8. Restore original row order
      #   setorder(DT, row_id__tmp)
      #   
      #   # 9. Clean up temporary columns
      #   DT[, c("row_id__tmp", "w_cumsum__tmp") := NULL]
      #   
      #   # Modifies DT in place, so no return() needed
      #   invisible(DT)
      # }
      # 
      # # -------------------------------------------------------------------
      # # Loop over lists in data.tables
      # # -------------------------------------------------------------------
      # for (i in seq_along(CIT_BU_list)) {
      #   calc_weighted_groups_in_one_pass(
      #     DT      = CIT_BU_list[[i]],
      #     inc_col = "g_total_gross",
      #     w_col   = "weight"
      #   )
      # }
      # 
      # 
      # for (i in seq_along(CIT_BU_list)) {
      #   calc_weighted_groups_in_one_pass(
      #     DT      = CIT_SIM_list[[i]],
      #     inc_col = "g_total_gross",
      #     w_col   = "weight"
      #   )
      # }
      # 
      # 
      
      




                      # Convert data for presentation in GUI
                      cit_summary_df <- merged_CIT_BU_SIM %>%
                        pivot_longer(cols = -year, 
                                     names_to = c("variable", ".value"), 
                                     names_pattern = "(.*)_(bu|sim)")
                      
                      # Calculate the difference between _sim and _bu columns
                      cit_summary_df <- cit_summary_df %>%
                        mutate(difference = sim - bu)
                      
                      
                      cit_summary_df <- cit_summary_df %>%
                        mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
                        filter(variable=='citax')
                      
                      # Arrange the columns
                      cit_summary_df <- cit_summary_df %>%
                                  select(year, bu, sim, difference)%>%
                                  dplyr::rename(
                                    "Current law (LCU Mil)"="bu",
                                    "Simulation (LCU Mil)"="sim",
                                    "Fiscal impact (LCU Mil)"="difference",
                                  )
                      
                      
                      MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
                      
                      cit_summary_df<-left_join(cit_summary_df,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
                        select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
                        dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                                       `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                                       `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
                        dplyr::select(-c(Nominal_GDP))
                      
                      
                      cit_summary_df <- as.data.table(cit_summary_df)
                      
                      
                      
                      print(merged_CIT_BU_SIM)
                      
                      end.time <- proc.time()
                      save.time <- end.time - start.time
                      cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")



                      
                      
                    