#day 3 training

setDTthreads(threads = 8)

get_param_fun <- function(params_dt, param_name) {
  params_dt[Parameters == param_name, Value]
}

            base_year <- unique(dt$Year)[1]
            end_year <- base_year + 5
            
            
            simulation_year <- SimulationYear  # Year from slider
            forecast_horizon_pit <- seq(base_year, end_year)
            scenario_years<-forecast_horizon_pit
            
            # Define the scenarios
            scenarios <- c("t0", "t1", "t2", "t3", "t4","t5")
            
            # Simulation parameters must be in data.table
            pit_simulation_parameters_raw <- pit_simulation_parameters_raw %>% data.table()
            pit_simulation_parameters_updated <- pit_simulation_parameters_updated %>% data.table()


# 1. Tax Calculation Function -------------------------------------------------------
start.time <- proc.time()
tax_calc_fun <- function(dt_scn, params_dt) {
                      rate1 <- get_param_fun(params_dt, "rate1")
                      rate2 <- get_param_fun(params_dt, "rate2")
                      rate3 <- get_param_fun(params_dt, "rate3")
                      rate4 <- get_param_fun(params_dt, "rate4")
                      tbrk1 <- get_param_fun(params_dt, "tbrk1")
                      tbrk2 <- get_param_fun(params_dt, "tbrk2")
                      tbrk3 <- get_param_fun(params_dt, "tbrk3")
                      tbrk4 <- get_param_fun(params_dt, "tbrk4")
                      rate_ded_income_agr_l <- get_param_fun(params_dt, "rate_ded_income_agr_l")
                      rate_deductions_CopyrightIncomePaintingsSculptural_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomePaintingsSculptural_l")
                      rate_deductions_CopyrightIncomeArtisticPhotography_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomeArtisticPhotography_l")
                      rate_deductions_CopyrightIncomeMusicBallet_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomeMusicBallet_l")
                      rate_deductions_CopyrightIncomeTranslationsLectures_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomeTranslationsLectures_l")
                      rate_deductions_IndustrialPropertyRights_c <- get_param_fun(params_dt, "rate_deductions_IndustrialPropertyRights_c")
                      rate_deductions_CopyrightIncomeSuccessor_l <- get_param_fun(params_dt, "rate_deductions_CopyrightIncomeSuccessor_l")
                      rate_deductions_Lease_c <- get_param_fun(params_dt, "rate_deductions_Lease_c")
                      rate_deductions_LeaseBusiness_c <- get_param_fun(params_dt, "rate_deductions_LeaseBusiness_c")
                      rate_deductions_SolidWaste_c <- get_param_fun(params_dt, "rate_deductions_SolidWaste_c")
                      sf_ded_work_l <- get_param_fun(params_dt, "sf_ded_work_l")
                      sf_ded_games_s <- get_param_fun(params_dt, "sf_ded_games_s")
                      sf_ded_betting_h <- get_param_fun(params_dt, "sf_ded_betting_h")
                      sf_per_allowance <- get_param_fun(params_dt, "sf_per_allowance")
                      capital_income_rate_g<- get_param_fun(params_dt, "capital_income_rate_g")
                      capital_income_rate_c<- get_param_fun(params_dt, "capital_income_rate_c")
  
# I. ESTIMATION TAX LIABILITY FOR INCOME FROM LABOR --------------
  # 1.Calculation of tax base wages ----------------------------------------------------
                      dt_scn[, tax_base_w := {
                                              personal_allowance_new <- g_total_personal_allowance_l * sf_per_allowance
                                              tax_base_wages1 <- pmax(g_Wages_l - total_ssc - personal_allowance_new, 0)
                                              tax_base_wages_diplomatic_consular_l <- g_WagesDiplomaticConsular_l - d_WagesDiplomaticConsular_l
                                              tax_base_wages1 + tax_base_wages_diplomatic_consular_l
                                            }]
      
  # 2.Calculation of tax base for income of the basis of sale of own agricultural products ------------------------------------------------------------------------
                      dt_scn[, tax_base_agr := g_AgriculturalProductsOwn_l - (g_AgriculturalProductsOwn_l * rate_ded_income_agr_l)]
                      dt_scn[, pit_tax_agr := tax_base_agr*rate1] 
  # 3.Copyright Income Artistic Photography  ------------------------------------------
                     dt_scn[, tax_base_CopyrightIncomeArtisticPhotography_l := 
                                                                             g_CopyrightIncomeArtisticPhotography_l - 
                                                                             (g_CopyrightIncomeArtisticPhotography_l * rate_deductions_CopyrightIncomeArtisticPhotography_l)
                                                                    ]
                      
                      dt_scn[, pit_tax_CopyrightIncomeArtisticPhotography_l := tax_base_CopyrightIncomeArtisticPhotography_l*rate1]
  # 4.Copyright Income Music Ballet  -------------------------------------
                      dt_scn[, tax_base_CopyrightIncomeMusicBallet_l := 
                               g_CopyrightIncomeMusicBallet_l - 
                               (g_CopyrightIncomeMusicBallet_l * rate_deductions_CopyrightIncomeMusicBallet_l)
                      ]
                      dt_scn[, pit_tax_CopyrightIncomeMusicBallet_l := tax_base_CopyrightIncomeMusicBallet_l*rate1] 
  # 5.Copyright Income Paintings)---------------
                      dt_scn[, tax_base_CopyrightIncomePaintingsSculptural_l := 
                               g_CopyrightIncomePaintingsSculptural_l - 
                               (g_CopyrightIncomePaintingsSculptural_l * rate_deductions_CopyrightIncomePaintingsSculptural_l)
                      ]
                      
                      dt_scn[, pit_tax_CopyrightIncomePaintingsSculptural_l := tax_base_CopyrightIncomePaintingsSculptural_l*rate1]

  # 6.Copyright Income Translations Lectures ----------------------------------------------------------------------
                      dt_scn[, tax_base_CopyrightIncomeTranslationsLectures_l := 
                               g_CopyrightIncomeTranslationsLectures_l - 
                               (g_CopyrightIncomeTranslationsLectures_l * rate_deductions_CopyrightIncomeTranslationsLectures_l)
                      ]
                    
                      dt_scn[, pit_tax_CopyrightIncomeTranslationsLectures_l := tax_base_CopyrightIncomeTranslationsLectures_l*rate1] 
  
  # 7.Successor or holder of the copyrights and related rights ----------------------------------------------------------------------
                      dt_scn[, tax_base_CopyrightIncomeSuccessor_l := 
                               g_CopyrightIncomeSuccessor_l - 
                               (g_CopyrightIncomeSuccessor_l * rate_deductions_CopyrightIncomeSuccessor_l)
                      ]  
                      
                      dt_scn[, pit_tax_CopyrightIncomeSuccessor_l := tax_base_CopyrightIncomeSuccessor_l*rate1] 
  
  # 8. Work Income ----------------------------------------------------------------------
                      dt_scn[, tax_base_WorkIncome_l := 
                               g_WorkIncome_l - 
                               (d_WorkIncome_l * sf_ded_work_l)
                      ]
                      
                      dt_scn[, pit_tax_WorkIncome_l := tax_base_WorkIncome_l*rate1] 

  # 9.Total tax base OTHER INCOME from labor (not include wages )----------------------------------------------------------------------
                      dt_scn[, tax_base_other := pmax(
                                                      tax_base_agr + 
                                                      tax_base_CopyrightIncomeArtisticPhotography_l + 
                                                      tax_base_CopyrightIncomeMusicBallet_l + 
                                                      tax_base_CopyrightIncomePaintingsSculptural_l + 
                                                      tax_base_CopyrightIncomeTranslationsLectures_l + 
                                                      tax_base_WorkIncome_l + 
                                                      g_TemporaryContracts_l + 
                                                      g_AgriculturalProducts_l + 
                                                      g_IndependentActivity_l + 
                                                      tax_base_CopyrightIncomeSuccessor_l, 
                                                      0)]
        
  # 10. Calculation for PIT for income for labor -------------------------------------------

                   dt_scn[, tti_w_I := tax_base_w + tax_base_other]
                           
                            
                    dt_scn[, pit_w := 
                                     (rate1 * pmin(tti_w_I, tbrk1) +
                                        rate2 * pmin(tbrk2 - tbrk1, pmax(0, tti_w_I - tbrk1)) +
                                        rate3 * pmin(tbrk3 - tbrk2, pmax(0, tti_w_I - tbrk2)) +
                                        rate4 * pmax(0, tti_w_I - tbrk3))
                            ]
                            

                    
# II. ESTIMATION TAX LIABILITY FOR INCOME FROM CAPITAL ---------------
   # 1. Estimation of tax base for capital incomes without deductions (prescribed cost) --------
                    dt_scn[, tax_base_IndustrialPropertyRights_c := 
                             g_IndustrialPropertyRights_c- 
                             (g_IndustrialPropertyRights_c* rate_deductions_IndustrialPropertyRights_c)
                    ]
                    
                    dt_scn[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c*capital_income_rate_c] 

   # 2.Income on the basis of lease -----------------------------------------------
                    dt_scn[, tax_base_Lease_c := 
                             g_Lease_c- 
                             (g_Lease_c* rate_deductions_Lease_c)
                    ]
            
                    dt_scn[, pit_tax_Lease_c := tax_base_Lease_c*capital_income_rate_c]
          
          
   # 3. Income on the basis of lease of equipped residential and business premises-------------------------------
                  dt_scn[, tax_base_LeaseBusiness_c := 
                           g_LeaseBusiness_c- 
                           (g_LeaseBusiness_c* rate_deductions_LeaseBusiness_c)
                  ]
          
                  dt_scn[, pit_LeaseBusiness_c := tax_base_LeaseBusiness_c*capital_income_rate_c] 
          
          
   # 4. Solid waste -------------------------------------------------------------
                  dt_scn[, tax_base_SolidWaste_c := 
                           g_SolidWaste_c- 
                           (g_SolidWaste_c* rate_deductions_SolidWaste_c)
                  ]
                  
                  dt_scn[, pit_SolidWaste_c := tax_base_SolidWaste_c*capital_income_rate_c] 
          
   # 5. Games of Chance 
                  
        # 5.1 General ----------------------------------- --------   
                  dt_scn[, tax_base_GamesofChanceGeneral_c := 
                           g_GamesofChanceGeneral_c
                           
                  ]
                  
                  dt_scn[, pit_GamesofChanceGeneral_c := tax_base_GamesofChanceGeneral_c*capital_income_rate_g] 
                  

      # 5.2 Specific----------------------------------- --------
                dt_scn[, tax_base_GamesofChanceSpecific_c := 
                         g_GamesofChanceSpecific_c- 
                         (d_GamesofChanceSpecific_c* sf_ded_games_s)
                ]
                
                dt_scn[, pit_GamesofChanceSpecific_c := tax_base_GamesofChanceSpecific_c*capital_income_rate_g] 
           
                
    # 5.3 Betting shop --------------------------------------------------------
          dt_scn[, tax_base_GamesofChanceBettingShop_c :=
                   g_GamesofChanceBettingShop_c-(d_GamesofChanceBettingShop_c * sf_ded_betting_h)
          ]
          
          dt_scn[, pit_GamesofChanceBettingShop_c := tax_base_GamesofChanceBettingShop_c*capital_income_rate_g] 
          
          
    # 6. Industrial Property Rights Successor ---------------------------------------
            dt_scn[, tax_base_IndustrialPropertyRightsSuccessor_c := 
                     g_IndustrialPropertyRightsSuccessor_c]
            
            dt_scn[, pit_IndustrialPropertyRightsSuccessor_c := tax_base_IndustrialPropertyRightsSuccessor_c*capital_income_rate_c] 
              
    # 7.  Income from insurance---------------
            dt_scn[, tax_base_Insurance_c := 
                     g_Insurance_c]
            
            dt_scn[, pit_Insurance_c := tax_base_Insurance_c*capital_income_rate_c] 
            
            
    # 8.  Income from Interests---------------
            dt_scn[, tax_base_Interests_c := 
                     g_Interests_c]
            
            dt_scn[, pit_Interests_c := tax_base_Interests_c*capital_income_rate_c] 
    # 9.  Other Income  ---------------  
            
            dt_scn[, tax_base_OtherIncome_c := 
                     g_OtherIncome_c]
            
            dt_scn[, pit_OtherIncome_c := tax_base_OtherIncome_c*capital_income_rate_c] 
    # 10.  Sublease ---------------  
           dt_scn[, tax_base_Sublease_c := 
                     g_Sublease_c
                      ]
            
            dt_scn[, pit_Sublease_c := tax_base_Sublease_c*capital_income_rate_c] 
            
  
    # 11. CapitalIncome_c -------------------------------------------------------
            dt_scn[, tax_base_CapitalIncome_c := 
                     g_CapitalIncome_c
                   
            ]
            
            dt_scn[, pit_CapitalIncome_c := tax_base_CapitalIncome_c*capital_income_rate_c] 
    

    # 11a. ---------------------------------------------------------------------
            # dt_scn[, tax_base_deposit_c :=
            #                                 gross_int_t_dep
            # 
            # ]
            # 
            # dt_scn[, pit_Int_t_dep_c := tax_base_deposit_c*capital_income_rate_c]

            
            
            
            
  # 12. Total tax base based on capital income --------------------------------
        # 12.1 Calculation for total tax base from capital income OTHER THAN games of chance----------------------
            dt_scn[, tti_c_a := 
                              g_IndustrialPropertyRightsSuccessor_c + g_Insurance_c + g_Interests_c + g_OtherIncome_c + g_Sublease_c +
                              tax_base_IndustrialPropertyRights_c + tax_base_Lease_c + tax_base_LeaseBusiness_c + tax_base_SolidWaste_c +
                              tax_base_Insurance_c + g_CapitalIncome_c
                              #+tax_base_deposit_c
                   ]

       
        # 12.2 Calculation for total tax base from capital income ONLY from games of chance (15%)----------------

          dt_scn[, tti_c_g :=
                                    tax_base_GamesofChanceGeneral_c+
                                    tax_base_GamesofChanceSpecific_c +
                                      tax_base_GamesofChanceBettingShop_c
                                      
                                  ]

            
          

            

  
        # 12.3 Total PIT on the base of income from capital----------------------
      

            dt_scn[, pit_c :=pit_tax_IndustrialPropertyRights_c+
                                                              pit_tax_Lease_c+
                                                              pit_LeaseBusiness_c+
                                                              pit_SolidWaste_c+
                                                              pit_GamesofChanceSpecific_c+
                                                              pit_GamesofChanceBettingShop_c+
                                                              pit_IndustrialPropertyRightsSuccessor_c+
                                                              pit_Insurance_c+
                                                              pit_Interests_c+
                                                              pit_OtherIncome_c+
                                                              pit_Sublease_c+
                                                              pit_CapitalIncome_c # +
                                                              #pit_Int_t_dep_c
                   ]


         
            
            
  # III. ESTIMATION TOTAL PIT --------------------------------       
  # Total PIT ------------------------------------------------------
          
            dt_scn[, total_taxbase := tti_c_g + tti_c_a + tti_w_I]
            
            dt_scn[, pitax := pit_w + pit_c]
            
            dt_scn[, total_net := g_total_gross-(total_ssc+g_total_personal_allowance_l+pitax)]
            
            
            
}    
# 2. Helper to Retrieve Growth Factors for Each Variable -------------------------------
                vars_to_grow <- c(
                                  "g_Wages_l",
                                  "g_WagesDiplomaticConsular_l", 
                                  "g_TemporaryContracts_l", 
                                  "g_AgriculturalProductsOwn_l", 
                                  "g_AgriculturalProducts_l", 
                                  "g_total_personal_allowance_l",
                                  "g_IndependentActivity_l", 
                                  "g_CopyrightIncomeArtisticPhotography_l", 
                                  "g_CopyrightIncomeMusicBallet_l", 
                                  "g_CopyrightIncomePaintingsSculptural_l", 
                                  "g_CopyrightIncomeSuccessor_l", 
                                  "g_CopyrightIncomeTranslationsLectures_l", 
                                  "g_WorkIncome_l", 
                                  "g_CapitalIncome_c", 
                                  "g_IndustrialPropertyRights_c", 
                                  "g_IndustrialPropertyRightsSuccessor_c", 
                                  "g_Insurance_c", 
                                  "g_Interests_c", 
                                  "g_Lease_c", 
                                  "g_LeaseBusiness_c", 
                                  "g_Sublease_c", 
                                  "g_SolidWaste_c", 
                                  "g_GamesofChanceSpecific_c", 
                                  "g_GamesofChanceGeneral_c", 
                                  "g_GamesofChanceBettingShop_c", 
                                  "g_OtherIncome_c",
                                  "total_net",
                                  "g_total_gross",
                                  "total_ssc" 
                                  #"gross_int_t_dep"
                              )
                
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
          
          PIT_BU_list <- list()
          
          # Start from baseline
          dt_scn_BU <- copy(dt)
          
          for (s in scenarios) {
            
            # 1) Retrieve scenario growth factors
            gf_values <- get_growth_factor_row(s)
            
            # 2) Multiply each variable by gf_values[v] * weights[[s]]
            for (v in vars_to_grow) {
              dt_scn_BU[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
            }
            
            # 3) Row-wise tax logic
            tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
            
            # 4) ADD a 'weight' column that references weights[[s]]
            dt_scn_BU[, weight := weights_pit[[s]]]
            
            # 5) Store in PIT_BU_list
            PIT_BU_list[[s]] <- copy(dt_scn_BU)
          }

# 4. Simulation --------------------------------------------------------------
          start_index <- match(SimulationYear, scenario_years) 
          
          PIT_SIM_list <- list()

          if (start_index > 1) {
            for (i in seq_len(start_index - 1)) {
              s_early <- scenarios[i]
              PIT_SIM_list[[s_early]] <- copy(PIT_BU_list[[s_early]])
            }
          }
          
          # 2) Determine the starting data for re-simulation
          if (start_index == 1) {
            # SimulationYear=2021 => start from original dt
            dt_scn_SIM <- copy(dt)
          } else {
            # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
            prev_scenario <- scenarios[start_index - 1]
            dt_scn_SIM <- copy(PIT_BU_list[[prev_scenario]])
          }
          
          # 3) Chain from scenario index = start_index .. 5
          for (i in seq(from = start_index, to = length(scenarios))) {
            s <- scenarios[i]
            
            gf_values <- get_growth_factor_row(s)
            
            # Multiply each variable by growth factor * row-weight for scenario s
            for (v in vars_to_grow) {
              dt_scn_SIM[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
            }
            
            # Run row-wise calculations with updated parameters
            tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated)
            
            # **Add a 'weight' column** with the row-specific weights_pit for scenario s
            dt_scn_SIM[, weight := weights_pit[[s]]]
            
            # Store final data in PIT_SIM_list
            PIT_SIM_list[[s]] <- copy(dt_scn_SIM)
          }
          
          message("Block 2 (PIT_SIM_list) done, including early years from PIT_BU_list, plus 'weight' column.\n")
          message("All done!\n")
          
         # rm(dt_scn_BU, dt_scn_SIM)

      # 5. Aggregation of simulated data -----------------------------------------------------
          
          summarize_PIT_fun_dt <- function(PIT_list, suffix) {
            # 1) Loop (via lapply) over each named data.table in the list
            # 2) Sum columns matching regex ^(calc|pit)
            # 3) Collect results into one data.table
            summary_list <- lapply(names(PIT_list), function(scenario_name) {
              dt <- PIT_list[[scenario_name]]
              
              # Select columns starting with "calc" or "pit", and sum them
              # .SDcols = patterns("^(calc|pit)") picks columns with those prefixes
              sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = patterns("^(calc|pit)")]
              
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

          summary_SIM <- summarize_PIT_fun_dt(PIT_SIM_list, "_sim")
          summary_BU  <- summarize_PIT_fun_dt(PIT_BU_list, "_bu")
          
      
      
      merged_PIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_PIT_BU_SIM$year <- as.character(forecast_horizon_pit)
      merged_PIT_BU_SIM <- merged_PIT_BU_SIM[, c("year", names(merged_PIT_BU_SIM)[-length(merged_PIT_BU_SIM)])]
      
      numeric_columns <- sapply(merged_PIT_BU_SIM, is.numeric)
      merged_PIT_BU_SIM[, numeric_columns] <- merged_PIT_BU_SIM[, numeric_columns] / 1e06



# 6. Decile ------------------------------------------------------------------
       
      
      calc_weighted_groups_in_one_pass <- function(DT, inc_col = "g_total_gross", w_col = "weight") {
        # 1. Keep track of original row order so we can restore it after sorting
        DT[, row_id__tmp := .I]
        
        # 2. Sort by income (use setorderv for a character column name)
        setorderv(DT, inc_col)
        
        # 3. Compute the cumulative sum of weight
        #    (handle NA weights as 0, adjust if you prefer a different approach)
        DT[, w_cumsum__tmp := cumsum(fifelse(is.na(get(w_col)), 0, get(w_col)))]
        
        # 4. Get the total weight
        total_w <- DT[.N, w_cumsum__tmp]
        
        # 5. Define breakpoints for deciles (10 groups) and centiles (100 groups)
        decile_breaks  <- seq(0, total_w, length.out = 11)   # 11 points => 10 intervals
        centile_breaks <- seq(0, total_w, length.out = 101)  # 101 points => 100 intervals
        
        # 6. Assign decile_group and centile_group
        DT[, decile_group  := findInterval(w_cumsum__tmp, decile_breaks,  rightmost.closed = TRUE)]
        DT[, centile_group := findInterval(w_cumsum__tmp, centile_breaks, rightmost.closed = TRUE)]
        
        # 7. Ensure the top boundary doesn't exceed the number of groups
        DT[, decile_group  := pmin(decile_group,  10)]
        DT[, centile_group := pmin(centile_group, 100)]
        
        # 8. Restore original row order
        setorder(DT, row_id__tmp)
        
        # 9. Clean up temporary columns
        DT[, c("row_id__tmp", "w_cumsum__tmp") := NULL]
        
        # Modifies DT in place, so no return() needed
        invisible(DT)
      }
      
      # -------------------------------------------------------------------
      # Loop over lists in data.tables
      # -------------------------------------------------------------------
      for (i in seq_along(PIT_BU_list)) {
        calc_weighted_groups_in_one_pass(
          DT      = PIT_BU_list[[i]],
          inc_col = "g_total_gross",
          w_col   = "weight"
        )
      }
      
      
      for (i in seq_along(PIT_BU_list)) {
        calc_weighted_groups_in_one_pass(
          DT      = PIT_SIM_list[[i]],
          inc_col = "g_total_gross",
          w_col   = "weight"
        )
      }
      
      
      
      




                      # Convert data for presentation in GUI
                      pit_summary_df <- merged_PIT_BU_SIM %>%
                        pivot_longer(cols = -year, 
                                     names_to = c("variable", ".value"), 
                                     names_pattern = "(.*)_(bu|sim)")
                      
                      # Calculate the difference between _sim and _bu columns
                      pit_summary_df <- pit_summary_df %>%
                        mutate(difference = sim - bu)
                      
                      
                      pit_summary_df <- pit_summary_df %>%
                        mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
                        filter(variable=='pitax')
                      
                      # Arrange the columns
                      pit_summary_df <- pit_summary_df %>%
                                  select(year, bu, sim, difference)%>%
                                  dplyr::rename(
                                    "Current law (LCU Mil)"="bu",
                                    "Simulation (LCU Mil)"="sim",
                                    "Fiscal impact (LCU Mil)"="difference",
                                  )
                      
                      
                      MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
                      
                      pit_summary_df<-left_join(pit_summary_df,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
                        select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
                        dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                                       `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                                       `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
                        dplyr::select(-c(Nominal_GDP))
                      
                      
                      pit_summary_df <- as.data.table(pit_summary_df)
                      
                      
                      
                      print(merged_PIT_BU_SIM)
                      
                      end.time <- proc.time()
                      save.time <- end.time - start.time
                      cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")



                    