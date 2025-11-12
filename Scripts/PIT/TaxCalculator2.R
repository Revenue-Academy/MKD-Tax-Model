# ============================================================
# PIT_SIM_list_calculation.R
# ------------------------------------------------------------
# Purpose:
#   Build PIT_SIM_list (reform), merge with baseline (PIT_BU_list),
#   compute decile/centile groups for BU & SIM, and produce GUI summary.
# Assumes the following exist in the global environment:
#   - dt, PIT_BU_list, summary_BU, growth_factors, weights_pit
#   - vars_to_grow, get_growth_factor_row(), tax_calc_fun()
#   - summarize_PIT_fun_dt(), SimulationYear, forecast_horizon_pit
#   - scenarios, MACRO_FISCAL_INDICATORS
# ============================================================

#library(data.table)
# setDTthreads(threads = 8L)  # uncomment if you want to pin threads

start.time <- proc.time()

# Ensure updated parameters are a data.table (no pipes)
pit_simulation_parameters_updated <- as.data.table(pit_simulation_parameters_updated)
simulation_year <- SimulationYear  # Year from slider
# -------------------------- Safety checks -----------------------------------
required_objs <- c("dt","PIT_BU_list","summary_BU","growth_factors","weights_pit",
                   "vars_to_grow","SimulationYear","forecast_horizon_pit","scenarios",
                   "MACRO_FISCAL_INDICATORS","get_growth_factor_row","tax_calc_fun")
missing_objs <- required_objs[!vapply(required_objs, exists, logical(1))]
if (length(missing_objs)) {
  stop("Missing required objects in environment: ", paste(missing_objs, collapse = ", "))
}

# Provide summarize_PIT_fun_dt if it wasn't sourced (keeps script robust)
if (!exists("summarize_PIT_fun_dt")) {
  summarize_PIT_fun_dt <- function(PIT_list, suffix) {
    summary_list <- lapply(names(PIT_list), function(scenario_name) {
      dt_local <- PIT_list[[scenario_name]]
      sums_dt <- dt_local[, lapply(.SD, sum, na.rm = TRUE), .SDcols = patterns("^(calc|pit)")]
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
}

# ----------------------------- Setup ----------------------------------------
start_index <- match(SimulationYear, forecast_horizon_pit)
if (is.na(start_index)) stop("SimulationYear is not in forecast_horizon_pit.")

# Ensure PIT_BU_list follows the same scenarios order
if (!identical(names(PIT_BU_list), scenarios)) {
  PIT_BU_list <- PIT_BU_list[scenarios]
}

PIT_SIM_list <- list()

# --------- Copy early (pre-simulation) years from baseline ------------------
if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    PIT_SIM_list[[s_early]] <- copy(PIT_BU_list[[s_early]])
  }
}

# --------------- Determine starting data for re-simulation -------------------
if (start_index == 1) {
  dt_scn_SIM <- copy(dt)
} else {
  prev_scenario <- scenarios[start_index - 1]
  dt_scn_SIM <- copy(PIT_BU_list[[prev_scenario]])
}

# ---------- Run Simulation from selected year onwards (reform path) ----------
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

# ----------------- Decile / Centile grouping (BU and SIM) -------------------
# calc_weighted_groups_in_one_pass <- function(DT, inc_col = "g_total_gross", w_col = "weight") {
#   DT[, row_id__tmp := .I]
#   setorderv(DT, inc_col)
#   
#   DT[, w_cumsum__tmp := cumsum(fifelse(is.na(get(w_col)), 0, get(w_col)))]
#   total_w <- DT[.N, w_cumsum__tmp]
#   
#   decile_breaks  <- seq(0, total_w, length.out = 11)
#   centile_breaks <- seq(0, total_w, length.out = 101)
#   
#   DT[, decile_group  := findInterval(w_cumsum__tmp,  decile_breaks,  rightmost.closed = TRUE)]
#   DT[, centile_group := findInterval(w_cumsum__tmp, centile_breaks, rightmost.closed = TRUE)]
#   
#   DT[, decile_group  := pmin(decile_group,  10)]
#   DT[, centile_group := pmin(centile_group, 100)]
#   
#   setorder(DT, row_id__tmp)
#   DT[, c("row_id__tmp", "w_cumsum__tmp") := NULL]
#   invisible(DT)
# }
# 
# # Apply to BU
# for (i in seq_along(PIT_BU_list)) {
#   calc_weighted_groups_in_one_pass(
#     DT      = PIT_BU_list[[i]],
#     inc_col = "g_total_gross",
#     w_col   = "weight"
#   )
# }
# # Apply to SIM  (note: iterate over PIT_SIM_list, not PIT_BU_list)
# for (i in seq_along(PIT_SIM_list)) {
#   calc_weighted_groups_in_one_pass(
#     DT      = PIT_SIM_list[[i]],
#     inc_col = "g_total_gross",
#     w_col   = "weight"
#   )
# }

# -------------------- Summaries and merge with baseline ----------------------
summary_SIM <- summarize_PIT_fun_dt(PIT_SIM_list, "_sim")

merged_PIT_BU_SIM <- merge(as.data.table(summary_BU),
                           as.data.table(summary_SIM),
                           by = "scenarios", all = TRUE)

# Attach years as character, preserving scenario order
merged_PIT_BU_SIM <- merged_PIT_BU_SIM[match(scenarios, merged_PIT_BU_SIM$scenarios)]
merged_PIT_BU_SIM[, year := as.character(forecast_horizon_pit)]
setcolorder(merged_PIT_BU_SIM, c("year", setdiff(names(merged_PIT_BU_SIM), "year")))

# Convert numeric columns to millions
num_cols <- names(merged_PIT_BU_SIM)[vapply(merged_PIT_BU_SIM, is.numeric, logical(1))]
merged_PIT_BU_SIM[, (num_cols) := lapply(.SD, function(x) x / 1e6), .SDcols = num_cols]

# ---------------- GUI table for PIT only (data.table approach) ---------------
pit_bu_col  <- "pitax_bu"
pit_sim_col <- "pitax_sim"
if (!all(c(pit_bu_col, pit_sim_col) %in% names(merged_PIT_BU_SIM))) {
  stop("Expected columns 'pitax_bu' and 'pitax_sim' not found in merged_PIT_BU_SIM.")
}

pit_summary_df <- merged_PIT_BU_SIM[, .(
  year,
  bu  = round(get(pit_bu_col), 1),
  sim = round(get(pit_sim_col), 1)
)]
pit_summary_df[, difference := round(sim - bu, 1)]

data.table::setnames(
  pit_summary_df,
  old = c("bu", "sim", "difference"),
  new = c("Current law (LCU Mil)", "Simulation (LCU Mil)", "Fiscal impact (LCU Mil)")
)

# ------------- Merge with MACRO_FISCAL_INDICATORS, compute % of GDP ----------
MACRO_FISCAL_INDICATORS <- as.data.table(MACRO_FISCAL_INDICATORS)
MACRO_FISCAL_INDICATORS[, Year := as.character(Year)]

pit_summary_df <- merge(
  pit_summary_df,
  MACRO_FISCAL_INDICATORS[, .(Year, Nominal_GDP)],
  by.x = "year", by.y = "Year",
  all.x = TRUE
)

pit_summary_df[, `Current law (Pct of GDP)`   := round(`Current law (LCU Mil)`   / Nominal_GDP * 100, 2)]
pit_summary_df[, `Simulation (Pct of GDP)`    := round(`Simulation (LCU Mil)`    / Nominal_GDP * 100, 2)]
pit_summary_df[, `Fiscal impact (Pct of GDP)` := round(`Fiscal impact (LCU Mil)` / Nominal_GDP * 100, 2)]
pit_summary_df[, Nominal_GDP := NULL]

pit_summary_df <- as.data.table(pit_summary_df)

message("Merged baseline and simulation summaries complete.")
message("Constructed PIT GUI table (pit_summary_df).")

end.time <- proc.time()
save.time <- end.time - start.time
cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")

# ============================================================
# OUTPUTS now available in the environment:
#   - PIT_SIM_list            (with decile_group / centile_group)
#   - summary_SIM
#   - merged_PIT_BU_SIM
#   - pit_summary_df
# ============================================================
