# ============================================================
# PIT_SIM_list_calculation.R
# ------------------------------------------------------------
# Purpose:
#   Build PIT_SIM_list (reform path), merge with baseline
#   results from PIT_BU_list, and produce GUI-ready summary.
# Assumes the following are already in the global environment:
#   - dt, PIT_BU_list, summary_BU, growth_factors, weights_pit
#   - vars_to_grow, get_growth_factor_row(), tax_calc_fun()
#   - summarize_PIT_fun_dt(), SimulationYear, forecast_horizon_pit
#   - scenarios, MACRO_FISCAL_INDICATORS
# ============================================================

#library(data.table)

#pit_simulation_parameters_raw<-pit_simulation_parameters_raw

start.time <- proc.time()

pit_simulation_parameters_updated<-pit_simulation_parameters_updated%>%data.table()

# Safety checks ---------------------------------------------------------------
required_objs <- c("dt","PIT_BU_list","summary_BU","growth_factors","weights_pit",
                   "vars_to_grow","SimulationYear","forecast_horizon_pit","scenarios",
                   "MACRO_FISCAL_INDICATORS")
missing_objs <- required_objs[!vapply(required_objs, exists, logical(1))]
if (length(missing_objs)) {
  stop("Missing required objects in environment: ", paste(missing_objs, collapse = ", "))
}

# STEP 1: Setup ---------------------------------------------------------------
start_index <- match(SimulationYear, forecast_horizon_pit)
if (is.na(start_index)) stop("SimulationYear is not in forecast_horizon_pit.")

# Ensure PIT_BU_list follows the same scenarios order
if (!identical(names(PIT_BU_list), scenarios)) {
  # Reorder if possible
  PIT_BU_list <- PIT_BU_list[scenarios]
}

PIT_SIM_list <- list()

# STEP 2: Copy early (pre-simulation) years from baseline ---------------------
if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    PIT_SIM_list[[s_early]] <- copy(PIT_BU_list[[s_early]])
  }
}

# Determine starting data for re-simulation
if (start_index == 1) {
  dt_scn_SIM <- copy(dt)
} else {
  prev_scenario <- scenarios[start_index - 1]
  dt_scn_SIM <- copy(PIT_BU_list[[prev_scenario]])
}

# STEP 3: Run Simulation from selected year onwards ---------------------------
for (i in seq(from = start_index, to = length(scenarios))) {
  s <- scenarios[i]
  gf_values <- get_growth_factor_row(s)
  
  # Apply growth * weight for scenario s
  for (v in vars_to_grow) {
    dt_scn_SIM[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
  }
  
  # Row-wise tax logic with UPDATED parameters
  tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated)
  
  # Add scenario-specific weights (row-level)
  dt_scn_SIM[, weight := weights_pit[[s]]]
  
  # Save
  PIT_SIM_list[[s]] <- copy(dt_scn_SIM)
}

message("PIT_SIM_list calculation complete.")

# STEP 4: Summaries and Merge with Baseline -----------------------------------
summary_SIM <- summarize_PIT_fun_dt(PIT_SIM_list, "_sim")

# Merge baseline and simulation summaries
merged_PIT_BU_SIM <- merge(as.data.table(summary_BU),
                           as.data.table(summary_SIM),
                           by = "scenarios", all = TRUE)

# Attach years as character, preserving scenario order
merged_PIT_BU_SIM <- merged_PIT_BU_SIM[match(scenarios, merged_PIT_BU_SIM$scenarios)]
merged_PIT_BU_SIM[, year := as.character(forecast_horizon_pit)]
setcolorder(merged_PIT_BU_SIM, c("year", setdiff(names(merged_PIT_BU_SIM), "year")))

# Convert numeric columns to millions
num_cols <- names(merged_PIT_BU_SIM)[vapply(merged_PIT_BU_SIM, is.numeric, logical(1))]
# exclude no columns - convert all numerics
merged_PIT_BU_SIM[, (num_cols) := lapply(.SD, function(x) x / 1e6), .SDcols = num_cols]

# STEP 5: Build GUI table for PIT only (data.table approach) ------------------
# We avoid pivot_longer by selecting just the PIT columns directly.

# 5.1 Identify PIT columns
pit_bu_col  <- "pitax_bu"
pit_sim_col <- "pitax_sim"
if (!all(c(pit_bu_col, pit_sim_col) %in% names(merged_PIT_BU_SIM))) {
  stop("Expected columns 'pitax_bu' and 'pitax_sim' not found in merged_PIT_BU_SIM.")
}

# 5.2 Construct the summary table
pit_summary_df <- merged_PIT_BU_SIM[, .(
  year,
  bu = round(get(pit_bu_col), 1),
  sim = round(get(pit_sim_col), 1)
)]
pit_summary_df[, difference := round(sim - bu, 1)]

# 5.3 Rename columns (data.table::setnames)
data.table::setnames(
  pit_summary_df,
  old = c("bu", "sim", "difference"),
  new = c("Current law (LCU Mil)", "Simulation (LCU Mil)", "Fiscal impact (LCU Mil)")
)

# STEP 6: Merge with MACRO_FISCAL_INDICATORS and compute % of GDP -------------
MACRO_FISCAL_INDICATORS <- as.data.table(MACRO_FISCAL_INDICATORS)
MACRO_FISCAL_INDICATORS[, Year := as.character(Year)]

pit_summary_df <- merge(
  pit_summary_df,
  MACRO_FISCAL_INDICATORS[, .(Year, Nominal_GDP)],
  by.x = "year", by.y = "Year",
  all.x = TRUE
)

# Compute percentages of GDP
pit_summary_df[, `Current law (Pct of GDP)` := round(`Current law (LCU Mil)` / Nominal_GDP * 100, 2)]
pit_summary_df[, `Simulation (Pct of GDP)`  := round(`Simulation (LCU Mil)`  / Nominal_GDP * 100, 2)]
pit_summary_df[, `Fiscal impact (Pct of GDP)` := round(`Fiscal impact (LCU Mil)` / Nominal_GDP * 100, 2)]

# Drop Nominal_GDP if not needed further
pit_summary_df[, Nominal_GDP := NULL]

# Convert to data.table explicitly (already is), keep for clarity
pit_summary_df <- as.data.table(pit_summary_df)

# Optional: message summaries
message("Merged baseline and simulation summaries complete.")
message("Constructed PIT GUI table (pit_summary_df).")


end.time <- proc.time()
save.time <- end.time - start.time
cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")


# ============================================================
# OUTPUTS now available in the environment:
#   - PIT_SIM_list
#   - summary_SIM
#   - merged_PIT_BU_SIM
#   - pit_summary_df
# ============================================================
