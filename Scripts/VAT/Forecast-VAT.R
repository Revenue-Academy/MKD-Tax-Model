'Revenue forecast'
end_year<-tail(growfactors_vat$Year, 1)
forecast_horizon <- seq(base_year_VAT, end_year)

# I. Forecasting next years with disaggregated data CPA ----------------------------------------------------------------------
          # 1.Business as usual -----------------------------------------------------
          
          vat_nace_cpa_tbl_bu<-CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev
          
          vat_nace_cpa_tbl_bu <- vat_nace_cpa_tbl_bu %>%
            dplyr::mutate(Scenario = "Baseline")
          
          vat_nace_cpa_tbl_bu$year <- base_year_VAT
          
          # Create a data frame for all combinations of years and PRODUCT_INDUSTRY_CODE
          forecast_bu_cpa <- expand.grid(
            year = forecast_horizon,
            PRODUCT_INDUSTRY_CODE = vat_nace_cpa_tbl_bu$PRODUCT_INDUSTRY_CODE
          )
          
          # Merge with grow factors for each year
          forecast_bu_cpa <- merge(forecast_bu_cpa, growfactors_vat, by.x = "year", by.y = "Year", all.x = TRUE)
          
          # Add the initial values from vat_nace_cpa_tbl_bu, including additional columns
          forecast_bu_cpa <- merge(
            forecast_bu_cpa,
            vat_nace_cpa_tbl_bu[, c(
              "PRODUCT_INDUSTRY_CODE", "PRODUCT_INDUSTRY_NAME", 
              "Total_Revenues_from_Intermediate_Inputs", "Final_Demand_HH", 
              "Final_Demand_NPISH", "Final_Demand_Government", "Final_Demand_Total"
            )],
            by = "PRODUCT_INDUSTRY_CODE", all.x = TRUE
          )
          
          # Arrange rows by PRODUCT_INDUSTRY_CODE and then year
          forecast_bu_cpa <- forecast_bu_cpa[order(
            forecast_bu_cpa$PRODUCT_INDUSTRY_CODE, forecast_bu_cpa$year
          ), ]
          
          # Initialize a column for sequential calculations
          forecast_bu_cpa$Grossed_Values <- NA
          
          # Rename Final_Demand_Total to Values for calculations
          forecast_bu_cpa <- forecast_bu_cpa %>%
            dplyr::rename("Values" = "Final_Demand_Total")
          
          # Perform sequential multiplication for each description
          for (desc in unique(forecast_bu_cpa$PRODUCT_INDUSTRY_CODE)) {
            # Filter rows for the current description
            desc_rows <- forecast_bu_cpa$PRODUCT_INDUSTRY_CODE == desc
            
            # Get indices for rows
            indices <- which(desc_rows)
            
            # Ensure there are valid indices
            if (length(indices) == 0) next
            
            # Set the initial value for the first year
            forecast_bu_cpa$Grossed_Values[indices[1]] <- forecast_bu_cpa$Values[indices[1]] * forecast_bu_cpa$grow_factor[indices[1]]
            
            # Loop through subsequent years and apply the previous year's grossed value
            for (i in 2:length(indices)) {
              forecast_bu_cpa$Grossed_Values[indices[i]] <- forecast_bu_cpa$Grossed_Values[indices[i - 1]] * forecast_bu_cpa$grow_factor[indices[i]]
            }
          }
          
          # Reset the row names to ascending numbers
          rownames(forecast_bu_cpa) <- NULL
          
          # Add scenario label
          forecast_bu_cpa$scenario <- "Baseline"
          
          # Select relevant columns and rename for final output
          forecast_bu_cpa <- forecast_bu_cpa %>%
            dplyr::select(
              year, PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, 
              Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, 
              Final_Demand_NPISH, Final_Demand_Government, Grossed_Values, scenario
            ) %>%
            dplyr::rename("value" = "Grossed_Values")
          
          # View(forecast_bu_cpa) # Uncomment to view the resulting table
          
          
          # 2.Simulation -----------------------------------------------------------
          
          vat_nace_cpa_tbl_sim<-CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev
          
          vat_nace_cpa_tbl_sim <- vat_nace_cpa_tbl_sim %>%
            dplyr::mutate(Scenario = "Simulation")
          
          vat_nace_cpa_tbl_sim$year <- base_year_VAT
          
          
          # Create a data frame for all combinations of years and PRODUCT_INDUSTRY_NAME
          forecast_sim_cpa <- expand.grid(
            year = forecast_horizon,
            PRODUCT_INDUSTRY_NAME = vat_nace_cpa_tbl_sim$PRODUCT_INDUSTRY_NAME
          )
          
          # Merge with grow factors for each year
          forecast_sim_cpa <- merge(forecast_sim_cpa, growfactors_vat, by.x = "year", by.y = "Year", all.x = TRUE)
          
          # Add initial values from vat_nace_cpa_tbl_sim, including additional columns
          forecast_sim_cpa <- merge(
            forecast_sim_cpa,
            vat_nace_cpa_tbl_sim[, c(
              "PRODUCT_INDUSTRY_CODE", "PRODUCT_INDUSTRY_NAME", 
              "Total_Revenues_from_Intermediate_Inputs", "Final_Demand_HH", 
              "Final_Demand_NPISH", "Final_Demand_Government", "Final_Demand_Total"
            )],
            by = "PRODUCT_INDUSTRY_NAME", all.x = TRUE
          )
          
          # Arrange rows by PRODUCT_INDUSTRY_NAME and year
          forecast_sim_cpa <- forecast_sim_cpa[order(forecast_sim_cpa$PRODUCT_INDUSTRY_NAME, forecast_sim_cpa$year), ]
          
          # Initialize a column for sequential calculations
          forecast_sim_cpa$Grossed_Values <- NA
          
          # # Perform sequential calculations using BU values before SimulationYear
          # for (desc in unique(forecast_sim_cpa$PRODUCT_INDUSTRY_NAME)) {
          #   desc_rows <- forecast_sim_cpa$PRODUCT_INDUSTRY_NAME == desc
          #   indices <- which(desc_rows)
          #   
          #   for (i in seq_along(indices)) {
          #     current_index <- indices[i]
          #     current_year <- forecast_sim_cpa$year[current_index]
          #     
          #     if (current_year < SimulationYear) {
          #       # Use values from Business as Usual for years before SimulationYear
          #       forecast_sim_cpa$Grossed_Values[current_index] <- forecast_bu_cpa %>%
          #         dplyr::filter(PRODUCT_INDUSTRY_NAME == desc, year == current_year) %>%
          #         dplyr::pull(value)
          #     } else if (current_year == SimulationYear) {
          #       # Start simulation logic from SimulationYear
          #       forecast_sim_cpa$Grossed_Values[current_index] <- forecast_sim_cpa$Final_Demand_Total[current_index] * forecast_sim_cpa$grow_factor[current_index]
          #     } else {
          #       # Continue simulation logic for years after SimulationYear
          #       previous_index <- indices[i - 1]
          #       forecast_sim_cpa$Grossed_Values[current_index] <- forecast_sim_cpa$Grossed_Values[previous_index] * forecast_sim_cpa$grow_factor[current_index]
          #     }
          #   }
          # }
          
          ##
          # Perform sequential calculations for each description
          # Perform sequential calculations for each PRODUCT_INDUSTRY_NAME
          for (desc in unique(forecast_sim_cpa$PRODUCT_INDUSTRY_NAME)) {
            desc_rows <- forecast_sim_cpa$PRODUCT_INDUSTRY_NAME == desc
            indices <- which(desc_rows)
            
            for (i in seq_along(indices)) {
              current_index <- indices[i]
              
              if (i == 1) {
                # For the first year, use the initial value from Final_Demand_Total and apply grow_factor
                forecast_sim_cpa$Grossed_Values[current_index] <- forecast_sim_cpa$Final_Demand_Total[current_index] * forecast_sim_cpa$grow_factor[current_index]
              } else {
                # For subsequent years, apply the grow factor sequentially
                previous_index <- indices[i - 1]
                forecast_sim_cpa$Grossed_Values[current_index] <- forecast_sim_cpa$Grossed_Values[previous_index] * forecast_sim_cpa$grow_factor[current_index]
              }
            }
          }
          
          
          
          # Reset row names
          rownames(forecast_sim_cpa) <- NULL
          
          # Add scenario label
          forecast_sim_cpa$scenario <- "Simulation"
          
          # Select relevant columns and rename for final output
          forecast_sim_cpa <- forecast_sim_cpa %>%
            dplyr::select(
              year, PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, 
              Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, 
              Final_Demand_NPISH, Final_Demand_Government, Grossed_Values, scenario
            ) %>%
            dplyr::rename("value" = "Grossed_Values")
          
          # View(forecast_sim_cpa) # Uncomment to view the resulting table
          
          
          # 3.Combined --------------------------------------------------------------------
          forecast_combined_cpa<-rbind(forecast_bu_cpa,forecast_sim_cpa)
          
          forecast_combined_cpa<-forecast_combined_cpa%>%
            dplyr::rename("Businesses_VAT"="Total_Revenues_from_Intermediate_Inputs",
                          "Households_VAT"="Final_Demand_HH",
                          "NPISH_VAT"="Final_Demand_NPISH",
                          "Goverment_VAT"="Final_Demand_Government",
                          "Total_VAT"="value")
          
          
          #View(forecast_combined_cpa)
          
          
          
# II. Forecasting next years with aggregate data ---------------------------------------------------------------------
          # 1. Business as usual -----------------------------------------------------
          
          MainResultsVATFinal_BU_forecast <- MainResultsVATFinal_BU %>%
            dplyr::filter(Descriptions %in% c('Benchmark VAT', 'Uncalibrated VAT', 'Calibrated VAT', 'Total VAT Gap', 'Policy Gap', 'Compliance Gap'))
          
          MainResultsVATFinal_BU_forecast$year <- base_year_VAT
          
          # Create a data frame for all combinations of years and descriptions
          forecast_bu <- expand.grid(
            year = forecast_horizon,
            Descriptions = MainResultsVATFinal_BU_forecast$Descriptions
          )
          
          # Merge with grow factors for each year
          forecast_bu <- merge(forecast_bu, growfactors_vat, by.x = "year", by.y = "Year", all.x = TRUE)
          
          # Add the initial values
          forecast_bu <- merge(forecast_bu, MainResultsVATFinal_BU_forecast[, c("Descriptions", "Values")], by = "Descriptions", all.x = TRUE)
          
          # Arrange rows by Descriptions and year
          forecast_bu <- forecast_bu[order(
            factor(forecast_bu$Descriptions, levels = c("Benchmark VAT", "Uncalibrated VAT", "Calibrated VAT", "Total VAT Gap", "Policy Gap", "Compliance Gap")),
            forecast_bu$year
          ), ]
          
          # Initialize a column for sequential calculations
          forecast_bu$Grossed_Values <- NA
          
          # Perform sequential calculations for each description
          for (desc in unique(forecast_bu$Descriptions)) {
            desc_rows <- forecast_bu$Descriptions == desc
            indices <- which(desc_rows)
            
            for (i in seq_along(indices)) {
              if (i == 1) {
                # First year: Apply the initial value and grow factor
                forecast_bu$Grossed_Values[indices[i]] <- forecast_bu$Values[indices[i]] * forecast_bu$grow_factor[indices[i]]
              } else {
                # Subsequent years: Multiply by the grow factor
                forecast_bu$Grossed_Values[indices[i]] <- forecast_bu$Grossed_Values[indices[i - 1]] * forecast_bu$grow_factor[indices[i]]
              }
            }
          }
          
          forecast_bu$scenario <- "Baseline"
          forecast_bu <- forecast_bu %>%
            dplyr::select(year, Descriptions, Grossed_Values, scenario) %>%
            dplyr::rename("value" = "Grossed_Values")
          
          # 2. Simulation -----------------------------------------------------------
                # # 
                MainResultsVATFinal_SIM_forecast <- MainResultsVATFinal_SIM %>%
                  dplyr::filter(Descriptions %in% c('Benchmark VAT', 'Uncalibrated VAT', 'Calibrated VAT', 'Total VAT Gap', 'Policy Gap', 'Compliance Gap'))

                MainResultsVATFinal_SIM_forecast$year <- base_year_VAT

                # Create a data frame for all combinations of years and descriptions
                forecast_sim <- expand.grid(
                  year = forecast_horizon,
                  Descriptions = MainResultsVATFinal_SIM_forecast$Descriptions
                )

                # Merge with grow factors for each year
                forecast_sim <- merge(forecast_sim, growfactors_vat, by.x = "year", by.y = "Year", all.x = TRUE)

                # Add the initial values
                forecast_sim <- merge(forecast_sim, MainResultsVATFinal_SIM_forecast[, c("Descriptions", "Values")], by = "Descriptions", all.x = TRUE)

                # Arrange rows by Descriptions and year
                forecast_sim <- forecast_sim[order(
                  factor(forecast_sim$Descriptions, levels = c("Benchmark VAT", "Uncalibrated VAT", "Calibrated VAT", "Total VAT Gap", "Policy Gap", "Compliance Gap")),
                  forecast_sim$year
                ), ]

                # Initialize a column for sequential calculations
                forecast_sim$Grossed_Values <- NA


                # Perform sequential calculations for each description - POSLEDNA VERZIJA STO NE MENUVA GODINI !!
                for (desc in unique(forecast_sim$Descriptions)) {
                  desc_rows <- forecast_sim$Descriptions == desc
                  indices <- which(desc_rows)

                  for (i in seq_along(indices)) {
                    current_index <- indices[i]

                    if (i == 1) {
                      # For the first year, use the initial value from the input data
                      forecast_sim$Grossed_Values[current_index] <- forecast_sim$Values[current_index] * forecast_sim$grow_factor[current_index]
                    } else {
                      # For subsequent years, apply the grow factor sequentially
                      previous_index <- indices[i - 1]
                      forecast_sim$Grossed_Values[current_index] <- forecast_sim$Grossed_Values[previous_index] * forecast_sim$grow_factor[current_index]
                    }
                  }
                }


                forecast_sim$scenario <- "Simulation"
                forecast_sim <- forecast_sim %>%
                  dplyr::select(year, Descriptions, Grossed_Values, scenario) %>%
                  dplyr::rename("value" = "Grossed_Values")


          # 3. Combined -------------------------------------------------------------
          
          forecast_combined_agg <- rbind(forecast_bu, forecast_sim)
          
                # Split data into Baseline and Simulation
                baseline_data <- forecast_combined_agg %>%
                  filter(scenario == "Baseline")
                
                simulation_data <- forecast_combined_agg %>%
                  filter(scenario == "Simulation")
                
                # Perform the replacement for Simulation data
                simulation_data <- simulation_data %>%
                  left_join(
                    baseline_data %>%
                      select(year, Descriptions, baseline_value = value), 
                    by = c("year", "Descriptions")
                  ) %>%
                  mutate(
                    value = ifelse(
                      year < SimulationYear,  # Replace only for years before SimulationYear
                      baseline_value,         # Use the corresponding Baseline value
                      value                   # Keep the original Simulation value otherwise
                    )
                  ) %>%
                  select(-baseline_value)  # Remove the temporary Baseline column
                
                # Combine the updated Simulation data with the original Baseline data
                forecast_combined_agg <- bind_rows(
                  baseline_data,
                  simulation_data
                )
                
                

# III.Preparation of data for table in GUI ------------------------------------

    # 1.Preparation of table with revenues -------------------------------------

                forecast_combined_agg_tbl<-forecast_combined_agg%>%
                              dplyr::filter(Descriptions=='Calibrated VAT')
                  
                
                # Pivot the data to wide format
                forecast_combined_agg_tbl_wide_raw <- forecast_combined_agg_tbl %>%
                                            pivot_wider(
                                              id_cols = year,                # Keep the 'year' column as is
                                              names_from = c(scenario, Descriptions), # Combine 'scenario' and 'Descriptions' for new column names
                                              values_from = value            # Fill the new columns with the 'value' data
                                            )%>%
                                            data.table()
                
              
                # Rename columns
                forecast_combined_agg_tbl_wide_raw <- forecast_combined_agg_tbl_wide_raw %>%
                                    dplyr::rename(
                                      `Current Law (Million LCU)` = `Baseline_Calibrated VAT`,
                                      `Simulation (Million LCU)` = `Simulation_Calibrated VAT`
                                    )%>%
                                    data.table()
                
                # Add a new column for "Fiscal Impact (LCU Mil)"
                forecast_combined_agg_tbl_wide_raw <- forecast_combined_agg_tbl_wide_raw %>%
                                      dplyr::mutate(`Fiscal Impact (Million LCU)` = `Simulation (Million LCU)` - `Current Law (Million LCU)`)%>%
                                      data.table()
                  
                
                forecast_combined_agg_tbl_wide_raw <- forecast_combined_agg_tbl_wide_raw %>%
                                         dplyr::mutate(across(-year, ~ round(., 1)))
                
                
                forecast_combined_agg_tbl_wide<-left_join(forecast_combined_agg_tbl_wide_raw,macro_fiscal_indicators_vat,by=c("year"="Year"))
                
              
                forecast_combined_agg_tbl_wide<-forecast_combined_agg_tbl_wide%>%
                        dplyr::mutate('Current Law (Pct of GDP)'=round((`Current Law (Million LCU)`/Nominal_GDP)*100,2),
                                      'Simulation (Pct of GDP)'=round((`Simulation (Million LCU)`/Nominal_GDP)*100,2),
                                      'Fiscal Impact (Pct of GDP)'=round((`Fiscal Impact (Million LCU)`/Nominal_GDP)*100,2))%>%
                        dplyr::select(-c(Nominal_GDP,Nominal_VAT_NET))
                

# 2. Preparation of Tax Expenditure Table ------------------------------------------

                forecast_TE_raw<-forecast_combined_agg%>%
                          dplyr::filter(Descriptions=='Policy Gap')%>%
                          dplyr::filter(scenario=="Simulation")
                

                forecast_TE_tbl<-left_join(forecast_TE_raw,macro_fiscal_indicators_vat,by=c("year"="Year"))%>%
                            dplyr::mutate('Tax Expenditures (Pct of GDP)'=round((value/Nominal_GDP)*100,2)
                                          )%>%
                            dplyr::select(-c(Nominal_GDP,Nominal_VAT_NET,Descriptions,scenario))%>%
                            dplyr::rename("Tax Expenditures (Million LCU)"="value")%>%
                            dplyr::mutate('Tax Expenditures (Million LCU)'=round(`Tax Expenditures (Million LCU)`,1))%>%
                            #dplyr::filter(year==2020)
                            dplyr::filter(year==base_year_VAT)
                            data.table()
                
                
                              
                
             # View(forecast_combined_agg_tbl_wide)
          
          # test<-forecast_combined_agg%>%
          #   dplyr::filter(Descriptions=="Calibrated VAT")
          # 
          # # %>%
          # #   dplyr::filter(year=='2022')
          # 
          # 
          # View(test)
          
          
# IV.Preparation of charts ---------------------------------------------------

# Define the custom order for PRODUCT_INDUSTRY_CODE
custom_order <- c(
                      "01", "02", "03", "B", "10-12", "13-15", "16", "17", "18", "19", "20", 
                      "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31-32", 
                      "33", "35", "36", "37-39", "F", "45", "46", "47", "49", "50", "51", "52", 
                      "53", "I", "58", "59-60", "61", "62-63", "64", "65", "66", "68B", "68А", 
                      "69-70", "71", "72", "73", "74-75", "77", "78", "79", "80-82", "84", 
                      "85", "86", "87-88", "90-92", "93", "94", "95", "96", "T"
                    )

# Filter and arrange the data based on the custom order
forecast_combined_cpa_selected <- forecast_combined_cpa %>%
                  dplyr::filter(year == SimulationYear) %>%
                  dplyr::mutate(PRODUCT_INDUSTRY_CODE = factor(PRODUCT_INDUSTRY_CODE, levels = custom_order)) %>%
                  dplyr::arrange(PRODUCT_INDUSTRY_CODE)



print("Script Forecast is Done !")     

