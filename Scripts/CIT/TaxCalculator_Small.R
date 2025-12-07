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
# 3. TAX CALCULATION FUNCTION (uses year-specific parameters, NA-safe)
#-------------------------------------------------------------------------------
tax_calc_fun_small <- function(dt_scn, params_dt, year) {
  
  # ---- Read parameter for this year ----
  CITrateSmall_2 <- get_param_fun(params_dt, "CITrateSmall_2", year)
  
  # NA-safe turnover:
  # 1) Coerce core_income / other_income / extra_income to numeric
  # 2) Sum row-wise with na.rm = TRUE (NA treated as 0)
  dt_scn[, turnover := {
    numSD <- lapply(.SD, function(x) suppressWarnings(as.numeric(x)))
    rowSums(as.data.frame(numSD), na.rm = TRUE)
  }, .SDcols = c("core_income", "other_income", "extra_income")]
  
  # CIT: if turnover is numeric, multiplication is NA-safe already.
  # (Only NA would be from CITrateSmall_2 itself, which we expect to exist.)
  dt_scn[, citax_turnover := turnover * CITrateSmall_2]
  
  invisible(NULL)
}

#-------------------------------------------------------------------------------
# 4. GROWTH FACTORS: variables and robust helper
#-------------------------------------------------------------------------------
vars_to_grow <- c(
  "core_income",
  "other_income",
  "extra_income"
)

get_growth_factor_row <- function(scenario) {
  
  # 1) figure out which row in growth_factors_small corresponds to this scenario
  if ("scenario" %in% names(growth_factors_small)) {
    idx <- which(growth_factors_small$scenario == scenario)
  } else if ("scenarios" %in% names(growth_factors_small)) {
    idx <- which(growth_factors_small$scenarios == scenario)
  } else {
    # fallback: align by position / global 'scenarios' vector
    idx <- which(scenarios == scenario)
  }
  
  if (length(idx) == 0L) {
    stop(sprintf("Scenario '%s' not found in growth_factors_small.", scenario))
  }
  if (length(idx) > 1L) {
    stop(sprintf("Scenario '%s' appears multiple times in growth_factors_small.", scenario))
  }
  if (idx > nrow(growth_factors_small)) {
    stop(sprintf("Index %d for scenario '%s' exceeds nrow(growth_factors_small) = %d.",
                 idx, scenario, nrow(growth_factors_small)))
  }
  
  # 2) extract that row (works for data.table and data.frame)
  if (inherits(growth_factors_small, "data.table")) {
    gf_row <- growth_factors_small[idx]
  } else {
    gf_row <- growth_factors_small[idx, , drop = FALSE]
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
# 5. BUSINESS-AS-USUAL (BU) PATH - SMALL
#-------------------------------------------------------------------------------
CIT_BU_list_small <- list()
dt_scn_BU_small   <- copy(dt_cit_small)

for (i in seq_along(scenarios)) {
  s             <- scenarios[i]
  scenario_year <- scenario_years[i]
  
  gf_values <- get_growth_factor_row(s)
  
  # robust weight: if missing, default to 1
  w <- if (exists("weights_cit_small", inherits = TRUE)) weights_cit_small[[s]] else NULL
  if (is.null(w) || length(w) == 0L) {
    w <- 1
  }
  
  # Apply growth factors and weights
  for (v in vars_to_grow) {
    if (v %in% names(dt_scn_BU_small)) {
      dt_scn_BU_small[, (v) := get(v) * gf_values[v] * w]
    }
  }
  
  # Tax logic with year-specific "raw" parameters (NA-safe)
  tax_calc_fun_small(dt_scn_BU_small, cit_simulation_parameters_raw, scenario_year)
  
  dt_scn_BU_small[, weight := w]
  
  CIT_BU_list_small[[s]] <- copy(dt_scn_BU_small)
}

#-------------------------------------------------------------------------------
# 6. SIMULATION PATH (policy change from SimulationYear onward) - SMALL
#-------------------------------------------------------------------------------
start_index <- match(simulation_year, scenario_years)
if (is.na(start_index)) stop("SimulationYear not found in scenario_years.")

CIT_SIM_list_small <- list()

# 1) Years before SimulationYear = same as BU
if (start_index > 1L) {
  for (i in seq_len(start_index - 1L)) {
    s_early <- scenarios[i]
    CIT_SIM_list_small[[s_early]] <- copy(CIT_BU_list_small[[s_early]])
  }
}

# 2) Starting micro data
if (start_index == 1L) {
  dt_scn_SIM_small <- copy(dt_cit_small)
} else {
  prev_scenario    <- scenarios[start_index - 1L]
  dt_scn_SIM_small <- copy(CIT_BU_list_small[[prev_scenario]])
}

# 3) From SimulationYear onwards: re-run with updated parameters
for (i in seq(from = start_index, to = length(scenarios))) {
  s             <- scenarios[i]
  scenario_year <- scenario_years[i]
  
  gf_values <- get_growth_factor_row(s)
  
  # robust weight here as well
  w <- if (exists("weights_cit_small", inherits = TRUE)) weights_cit_small[[s]] else NULL
  if (is.null(w) || length(w) == 0L) {
    w <- 1
  }
  
  for (v in vars_to_grow) {
    if (v %in% names(dt_scn_SIM_small)) {
      dt_scn_SIM_small[, (v) := get(v) * gf_values[v] * w]
    }
  }
  
  tax_calc_fun_small(dt_scn_SIM_small, cit_simulation_parameters_updated, scenario_year)
  
  dt_scn_SIM_small[, weight := w]
  
  CIT_SIM_list_small[[s]] <- copy(dt_scn_SIM_small)
}

message("Block 2 (CIT_SIM_list_small) done, including early years from CIT_BU_list_small, plus 'weight' column.")
message("All done!\n")

#-------------------------------------------------------------------------------
# 7. AGGREGATION BY SCENARIO - SMALL
#-------------------------------------------------------------------------------
summarize_CIT_fun_dt_small <- function(CIT_list, suffix) {
  
  summary_list <- lapply(names(CIT_list), function(scenario_name) {
    dt <- CIT_list[[scenario_name]]
    
    # sum columns starting with "cit" (we only need citax here)
    sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE),
                  .SDcols = patterns("^cit")]
    
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

summary_SIM_small <- summarize_CIT_fun_dt_small(CIT_SIM_list_small, "_sim")
summary_BU_small  <- summarize_CIT_fun_dt_small(CIT_BU_list_small,  "_bu")

merged_CIT_BU_SIM_small      <- merge(summary_BU_small, summary_SIM_small, by = "scenarios", all = TRUE)
merged_CIT_BU_SIM_small$year <- as.character(forecast_horizon)
merged_CIT_BU_SIM_small      <- merged_CIT_BU_SIM_small[, c("year", names(merged_CIT_BU_SIM_small)[-length(merged_CIT_BU_SIM_small)])]

numeric_columns <- sapply(merged_CIT_BU_SIM_small, is.numeric)
merged_CIT_BU_SIM_small[, numeric_columns] <- merged_CIT_BU_SIM_small[, numeric_columns] / 1e06

#-------------------------------------------------------------------------------
# 8. GUI SUMMARY AND % OF GDP - SMALL
#-------------------------------------------------------------------------------
cit_summary_df_small <- merged_CIT_BU_SIM_small %>%
  pivot_longer(
    cols         = -year,
    names_to     = c("variable", ".value"),
    names_pattern = "(.*)_(bu|sim)"
  ) %>%
  mutate(difference = sim - bu) %>%
  mutate(across(c(bu, sim, difference), ~ round(., 1))) %>%
  filter(variable == "citax_turnover") %>%
  select(year, bu, sim, difference) %>%
  dplyr::rename(
    "Current law (LCU Mil)"   = bu,
    "Simulation (LCU Mil)"    = sim,
    "Fiscal impact (LCU Mil)" = difference
  )

MACRO_FISCAL_INDICATORS$Year <- as.character(MACRO_FISCAL_INDICATORS$Year)

cit_summary_df_small <- left_join(
                  cit_summary_df_small,
                  MACRO_FISCAL_INDICATORS,
                  by = c("year" = "Year")
                ) %>%
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

cit_summary_df_small <- as.data.table(cit_summary_df_small)

print(merged_CIT_BU_SIM_small)

end.time  <- proc.time()
save.time <- end.time - start.time
cat("\n Number of minutes running:", save.time[3] / 60, "\n\n")
