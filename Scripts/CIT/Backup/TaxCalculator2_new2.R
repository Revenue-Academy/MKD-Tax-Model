library(data.table)
library(dplyr)
library(tidyr)

setDTthreads(threads = 8)

# Ensure parameter tables are data.table
cit_simulation_parameters_raw     <- data.table(cit_simulation_parameters_raw)
cit_simulation_parameters_updated <- data.table(cit_simulation_parameters_updated)

#-------------------------------------------------------------------------------
# 1. TIME / SCENARIOS: from parameter table years
#-------------------------------------------------------------------------------
param_years      <- sort(unique(cit_simulation_parameters_raw$Year))
base_year        <- min(param_years)
end_year         <- max(param_years)                  # e.g. 2021–2027
forecast_horizon <- seq(base_year, end_year)
scenario_years   <- forecast_horizon                  # just a clearer name

# Scenario labels t0, t1, t2, ...
scenarios <- paste0("t", seq_along(scenario_years) - 1L)

# Simulation year from slider
simulation_year <- SimulationYear

#-------------------------------------------------------------------------------
# 2. Helper: get parameter value for a given name and year
#-------------------------------------------------------------------------------
get_param_fun <- function(params_dt, param_name, year) {
  val <- params_dt[Parameters == param_name & Year == year, Value]
  if (!length(val)) {
    stop(sprintf("Parameter '%s' for year %s not found in parameter table.", 
                 param_name, year))
  }
  val[1]
}

start.time <- proc.time()

#-------------------------------------------------------------------------------
# 3. TAX CALCULATION FUNCTION (uses year-specific parameters)
#-------------------------------------------------------------------------------
tax_calc_fun <- function(dt_scn, params_dt, year) {
  
  # ---- Read parameters for this year ----
  CITrate                                     <- get_param_fun(params_dt, "CITrate", year)
  toggle_voluntary_pension_fund               <- get_param_fun(params_dt, "toggle_voluntary_pension_fund", year)
  toggle_insurance_premiums                   <- get_param_fun(params_dt, "toggle_insurance_premiums", year)
  toggle_insurance_premiums_management_bodies <- get_param_fun(params_dt, "toggle_insurance_premiums_management_bodies", year)
  toggle_10_fiscal_systems                    <- get_param_fun(params_dt, "toggle_10_fiscal_systems", year)
  toggle_sport_donation                       <- get_param_fun(params_dt, "toggle_sport_donation", year)
  toggle_TIDZ                                 <- get_param_fun(params_dt, "toggle_TIDZ", year)
  toggle_dividends_capitals_r                 <- get_param_fun(params_dt, "toggle_dividends_capitals_r", year)
  toggle_reinvested_profits                   <- get_param_fun(params_dt, "toggle_reinvested_profits", year)
  
  #--------------------------------------------------------------------
  # I. ESTIMATION – UE PART
  #--------------------------------------------------------------------
  dt_scn[, ue_voluntary_pension_fund_r_calc :=
           ue_voluntary_pension_fund_r * toggle_voluntary_pension_fund]
  
  dt_scn[, ue_insurance_premiums_r_calc :=
           ue_insurance_premiums_r * toggle_insurance_premiums]
  
  dt_scn[, ue_insurance_premiums_management_bodies_r_calc :=
           ue_insurance_premiums_management_bodies_r * 
           toggle_insurance_premiums_management_bodies]
  
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
  
  use_cols <- intersect(ue_cols, names(dt_scn))
  
  dt_scn[, ue_total_calc := {
    if (length(use_cols)) {
      numSD <- lapply(.SD, function(x) suppressWarnings(as.numeric(x)))
      rowSums(as.data.frame(numSD), na.rm = TRUE)
    } else {
      rep(0, .N)
    }
  }, .SDcols = use_cols]
  
  #--------------------------------------------------------------------
  # III. Tax base (financial_results_r + ue_total_calc)
  #--------------------------------------------------------------------
  dt_scn[, tax_base_calc := financial_results_r + ue_total_calc]
  
  #--------------------------------------------------------------------
  # IV. Reductions of tax base
  #--------------------------------------------------------------------
  dt_scn[, rb_reinvested_profits_calc :=
           rb_reinvested_profits_r * toggle_reinvested_profits]
  
  dt_scn[, rb_dividends_capitals_r_calc :=
           rb_dividends_capitals_r * toggle_dividends_capitals_r]
  
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
  
  dt_scn[, tb_ar_deductions_calc :=
           fifelse(tax_base_calc < 0, 0, tax_base_calc - rb_tax_reductions_calc)]
  
  dt_scn[, Net_tax_base := tb_ar_deductions_calc]
  
  #--------------------------------------------------------------------
  # VI. Calculated CIT
  #--------------------------------------------------------------------
  dt_scn[, tb_ar_income_CIT_calc := tb_ar_deductions_calc * CITrate]
  
  #--------------------------------------------------------------------
  # VII. Deductions of calculated CIT
  #--------------------------------------------------------------------
  dt_scn[, tb_ar_cash_pay_systems_calc :=
           tb_ar_cash_pay_systems_r * toggle_10_fiscal_systems]
  
  dt_scn[, tb_ar_tax_relief_donation_r_calc :=
           tb_ar_tax_relief_donation_r * toggle_sport_donation]
  
  dt_scn[, tb_ar_deductions_calculated_calc :=
           rowSums(.SD, na.rm = TRUE),
         .SDcols = c(
           "tb_ar_cash_pay_systems_calc",
           "tb_ar_tax_gains_abroad_r",
           "tb_ar_tax_paid_branch_abroad_r",
           "tb_ar_tax_relief_donation_r_calc"
         )]
  
  dt_scn[, tb_ar_calculated_tax_after_reduction_calc :=
           fifelse(tb_ar_income_CIT_calc - tb_ar_deductions_calculated_calc < 0,
                   0,
                   tb_ar_income_CIT_calc - tb_ar_deductions_calculated_calc)]
  
  #--------------------------------------------------------------------
  # Overpaid / CF losses / stubs
  #--------------------------------------------------------------------
  dt_scn[, tb_ar_tax_overpaid_calc :=
           tb_ar_calculated_tax_after_reduction_calc -
           tb_ar_paid_advance_tax_profits_r -
           tb_ar_over_paid_previous_tax_periods_r]
  
  dt_scn[, CF_losses := rb_part_loss_previous_years_r]
  
  dt_scn[, paste0("newloss", 1:10) := 0]
  
  dt_scn[, Net_tax_base_behavior := 0]
  
  dt_scn[, citax := tb_ar_calculated_tax_after_reduction_calc]
  
  dt_scn[, tb_ar_deductions_calc :=
           fifelse(tax_base_calc < 0, 0, tax_base_calc - rb_tax_reductions_calc)]
  
  invisible(NULL)
}

#-------------------------------------------------------------------------------
# 4. GROWTH FACTORS: variables and robust helper
#-------------------------------------------------------------------------------
vars_to_grow <- c(
  "financial_results_r", "ue_total_calc", "ue_not_related_bus_ent_r",
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
  "sd_total_cost_donation_sport_r"
)

get_growth_factor_row <- function(scenario) {
  
  # 1) figure out which row in growth_factors corresponds to this scenario
  if ("scenario" %in% names(growth_factors)) {
    idx <- which(growth_factors$scenario == scenario)
  } else if ("scenarios" %in% names(growth_factors)) {
    idx <- which(growth_factors$scenarios == scenario)
  } else {
    # fallback: align by position / global 'scenarios' vector
    idx <- which(scenarios == scenario)
  }
  
  if (length(idx) == 0L) {
    stop(sprintf("Scenario '%s' not found in growth_factors.", scenario))
  }
  if (length(idx) > 1L) {
    stop(sprintf("Scenario '%s' appears multiple times in growth_factors.", scenario))
  }
  if (idx > nrow(growth_factors)) {
    stop(sprintf("Index %d for scenario '%s' exceeds nrow(growth_factors) = %d.",
                 idx, scenario, nrow(growth_factors)))
  }
  
  # 2) extract that row (works for data.table and data.frame)
  if (inherits(growth_factors, "data.table")) {
    gf_row <- growth_factors[idx]
  } else {
    gf_row <- growth_factors[idx, , drop = FALSE]
  }
  
  # 3) fill output vector
  out <- setNames(numeric(length(vars_to_grow)), vars_to_grow)
  
  for (v in vars_to_grow) {
    gf_col <- sub("_adjusted", "", v)
    
    if (!gf_col %in% names(gf_row)) {
      # no explicit growth factor for this variable → assume 1 (no change)
      out[v] <- 1
    } else {
      val <- gf_row[[gf_col]][1]
      if (length(val) == 0L || is.na(val)) {
        out[v] <- 1
      } else {
        out[v] <- val
      }
    }
  }
  
  out
}

#-------------------------------------------------------------------------------
# 5. BUSINESS-AS-USUAL (BU) PATH
#-------------------------------------------------------------------------------
CIT_BU_list <- list()
dt_scn_BU   <- copy(dt_cit)

for (i in seq_along(scenarios)) {
  s             <- scenarios[i]
  scenario_year <- scenario_years[i]
  
  gf_values <- get_growth_factor_row(s)
  
  # robust weight: if missing, default to 1
  w <- weights_cit[[s]]
  if (is.null(w) || length(w) == 0L) {
    w <- 1
  }
  
  # Apply growth factors and weights
  for (v in vars_to_grow) {
    if (v %in% names(dt_scn_BU)) {
      dt_scn_BU[, (v) := get(v) * gf_values[v] * w]
    }
  }
  
  # Tax logic with year-specific "raw" parameters
  tax_calc_fun(dt_scn_BU, cit_simulation_parameters_raw, scenario_year)
  
  dt_scn_BU[, weight := w]
  
  CIT_BU_list[[s]] <- copy(dt_scn_BU)
}

#-------------------------------------------------------------------------------
# 6. SIMULATION PATH (policy change from SimulationYear onward)
#-------------------------------------------------------------------------------
start_index <- match(simulation_year, scenario_years)
if (is.na(start_index)) stop("SimulationYear not found in scenario_years.")

CIT_SIM_list <- list()

# 1) Years before SimulationYear = same as BU
if (start_index > 1L) {
  for (i in seq_len(start_index - 1L)) {
    s_early <- scenarios[i]
    CIT_SIM_list[[s_early]] <- copy(CIT_BU_list[[s_early]])
  }
}

# 2) Starting micro data
if (start_index == 1L) {
  dt_scn_SIM <- copy(dt_cit)
} else {
  prev_scenario <- scenarios[start_index - 1L]
  dt_scn_SIM    <- copy(CIT_BU_list[[prev_scenario]])
}

# 3) From SimulationYear onwards: re-run with updated parameters
for (i in seq(from = start_index, to = length(scenarios))) {
  s             <- scenarios[i]
  scenario_year <- scenario_years[i]
  
  gf_values <- get_growth_factor_row(s)
  
  # robust weight here as well
  w <- weights_cit[[s]]
  if (is.null(w) || length(w) == 0L) {
    w <- 1
  }
  
  for (v in vars_to_grow) {
    if (v %in% names(dt_scn_SIM)) {
      dt_scn_SIM[, (v) := get(v) * gf_values[v] * w]
    }
  }
  
  tax_calc_fun(dt_scn_SIM, cit_simulation_parameters_updated, scenario_year)
  
  dt_scn_SIM[, weight := w]
  
  CIT_SIM_list[[s]] <- copy(dt_scn_SIM)
}

message("Block 2 (CIT_SIM_list) done, including early years from CIT_BU_list, plus 'weight' column.")
message("All done!\n")

#-------------------------------------------------------------------------------
# 7. AGGREGATION BY SCENARIO
#-------------------------------------------------------------------------------
summarize_CIT_fun_dt <- function(CIT_list, suffix) {
  
  summary_list <- lapply(names(CIT_list), function(scenario_name) {
    dt <- CIT_list[[scenario_name]]
    
    sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE),
                  .SDcols = patterns("^(calc|cit)")]
    
    sums_dt[, scenarios := scenario_name]
    setcolorder(sums_dt, c("scenarios", setdiff(names(sums_dt), "scenarios")))
    sums_dt
  })
  
  result_dt <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
  
  old_names <- setdiff(names(result_dt), "scenarios")
  new_names <- paste0(old_names, suffix)
  setnames(result_dt, old_names, new_names)
  
  as.data.frame(result_dt)
}

summary_SIM <- summarize_CIT_fun_dt(CIT_SIM_list, "_sim")
summary_BU  <- summarize_CIT_fun_dt(CIT_BU_list,  "_bu")

merged_CIT_BU_SIM      <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
merged_CIT_BU_SIM$year <- as.character(forecast_horizon)
merged_CIT_BU_SIM      <- merged_CIT_BU_SIM[, c("year", names(merged_CIT_BU_SIM)[-length(merged_CIT_BU_SIM)])]

numeric_columns <- sapply(merged_CIT_BU_SIM, is.numeric)
merged_CIT_BU_SIM[, numeric_columns] <- merged_CIT_BU_SIM[, numeric_columns] / 1e06

#-------------------------------------------------------------------------------
# 8. GUI SUMMARY AND % OF GDP
#-------------------------------------------------------------------------------
cit_summary_df <- merged_CIT_BU_SIM %>%
  pivot_longer(
    cols         = -year,
    names_to     = c("variable", ".value"),
    names_pattern = "(.*)_(bu|sim)"
  ) %>%
  mutate(difference = sim - bu) %>%
  mutate(across(c(bu, sim, difference), ~ round(., 1))) %>%
  filter(variable == "citax") %>%
  select(year, bu, sim, difference) %>%
  dplyr::rename(
    "Current law (LCU Mil)"   = bu,
    "Simulation (LCU Mil)"    = sim,
    "Fiscal impact (LCU Mil)" = difference
  )

MACRO_FISCAL_INDICATORS$Year <- as.character(MACRO_FISCAL_INDICATORS$Year)

cit_summary_df <- left_join(
  cit_summary_df,
  MACRO_FISCAL_INDICATORS,
  by = c("year" = "Year"))%>%
  select(
    year,
    "Current law (LCU Mil)",
    "Simulation (LCU Mil)",
    "Fiscal impact (LCU Mil)",
    Nominal_GDP
  ) %>%
  dplyr::mutate(
    `Current law (Pct of GDP)`   = round(`Current law (LCU Mil)`   / Nominal_GDP * 100, 2),
    `Simulation (Pct of GDP)`    = round(`Simulation (LCU Mil)`    / Nominal_GDP * 100, 2),
    `Fiscal impact (Pct of GDP)` = round(`Fiscal impact (LCU Mil)` / Nominal_GDP * 100, 2)
  ) %>%
  dplyr::select(-Nominal_GDP)

cit_summary_df <- as.data.table(cit_summary_df)

print(merged_CIT_BU_SIM)

end.time  <- proc.time()
save.time <- end.time - start.time
cat("\n Number of minutes running:", save.time[3] / 60, "\n\n")


"Brojkite se provereni i korespondiraat so originalnite podatoci"


                 
                      
                    