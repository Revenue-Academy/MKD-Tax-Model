'Install packages and importing of data
                                          '



'Step 1. Set your local path to the  model'
rm(list = ls())

path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/MKD-Tax-Model" ##<---PATH


# I.INSTALLING REQUIRED PACKAGES AND SETTING PATH  -------------------------------------------------
          '1.Library installation'

                  list.of.packages <- c(
                                          "shiny",
                                          "shinydashboard",
                                          "shinyjs",
                                          "shinyWidgets",
                                          "DT",
                                          "ineq",
                                          "data.table",
                                          "readxl",
                                          "fontawesome",
                                          "flexdashboard",
                                          "tidyverse",
                                          "plyr",
                                          "shinycssloaders",
                                          "future",
                                          "promises",
                                          "plotly",
                                          "stringr",
                                          "reshape2",
                                          "base64enc",
                                          "parallel",
                                          "purrr",
                                          "tidyr",
                                          "RColorBrewer",
                                          "Hmisc",
                                          "openxlsx",
                                          "sm",
                                          "ks",
                                          "kableExtra" 
                                        )


          new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
          if(length(new.packages)) install.packages(new.packages)



# Additional installation -------------------------------------------------


        # install.packages("https://cran.r-project.org/src/contrib/Archive/IC2/IC2_1.0-1.tar.gz",
        #                     repos = NULL, type = "source", method = "wininet")
        # 
        # 
        # 
        # install.packages(
        #   "https://cran.r-project.org/src/contrib/Archive/rccmisc/rccmisc_0.3.7.tar.gz",
        #   repos = NULL,
        #   type = "source",
        #   method = "wininet"
        # )
        
        
        
          
        library(tidyverse)
        library(readxl)
        library(reshape2)
        library(data.table)
        library(plyr)
        library(readxl)

# II.IMPORT DATA -----------------------------------------------------------------

      # 1.PIT -------------------------------------------------------------------


        path2 <- paste0(path1, "/Data/PIT")
        setwd(path2)
        getwd()
        
                         
                          
                          #dt<-read_csv("mpin_epdd_nace_final334.csv")%>%data.table()
                          dt<-read_csv("mpin_epdd_nace_final.csv")%>%data.table()
                           # dt<-read_csv("pit_synthetic.csv")%>%data.table()
                          
                          
                          
                          
                          MACRO_FISCAL_INDICATORS<-read_excel("macro_indicators.xlsx")
                          
                          # 2.Growth Factors & Scenario Mapping
                          
                          
                          growth_factors<-read.csv("growfactors_pit_mkd.csv")%>%data.table()
                          
                         
                          # 3.Weights

                          
                          
                          n <- NROW(dt)
                          
                          same_weight<-1
                          
                          #same_weight<-56  ### #<--- Fiscal concul
                          
                          weights_pit <- data.table(
                                                    t0 = rep(same_weight, n),
                                                    t1 = rep(1, n),
                                                    t2 = rep(1, n),
                                                    t3 = rep(1, n),
                                                    t4 = rep(1, n),
                                                    t5 = rep(1, n)
                                                  )
                          rm(n)
                          
                          
                          
                  
    # NACE NAMES
    df_nace_names<-structure(list(section = c("A", "B", "C", "D", "E", "F", "G", 
                                              "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
                                              "U", "Other"), description = c("Agriculture, forestry and fishing", 
                                                                             "Mining and quarrying", "Manufacturing", "Electricity, gas, steam and air conditioning supply", 
                                                                             "Water supply; sewerage; waste managment and remediation activities", 
                                                                             "Construction", "Wholesale and retail trade; repair of motor vehicles and motorcycles", 
                                                                             "Transporting and storage", "Accommodation and food service activities", 
                                                                             "Information and communication", "Financial and insurance activities", 
                                                                             "Real estate activities", "Professional, scientific and technical activities", 
                                                                             "Administrative and support service activities", "Public administration and defence; compulsory social security", 
                                                                             "Education", "Human health and social work activities", "Arts, entertainment and recreation", 
                                                                             "Other services activities", "Activities of households as employers; undifferentiated goods - and services - producing activities of households for own use", 
                                                                             "Activities of extraterritorial organisations and bodies", "Other"
                                              )), row.names = c(NA, -22L), class = c("tbl_df", "tbl", "data.frame"
                                              ))
    

    

      # 2. Calculation of Business as Usual -------------------------------------
    # 
    # # ============================================================
    # # PIT_BU_list_calculation.R
    # # ------------------------------------------------------------
    # # Purpose:
    # #   Generate baseline (Business-as-Usual) PIT results (PIT_BU_list)
    # #   including all growth factor applications and tax calculations.
    # # ============================================================
    # 
    # 
    # pit_simulation_parameters_raw<-read_excel("PIT-Parameters.xlsx")%>%
    #   data.table()
    # 
    # # --- Load required libraries ---
    # library(data.table)
    # library(dplyr)
    # 
    # # ============================================================
    # # STEP 1: Helper Functions
    # # ============================================================
    # 
    # # Retrieve parameter by name
    # get_param_fun <- function(params_dt, param_name) {
    #   params_dt[Parameters == param_name, Value]
    # }
    # 
    # # PIT tax calculation function
    # tax_calc_fun <- function(dt_scn, params_dt) {
    #   # --- Extract key parameters ---
    #   rate1 <- get_param_fun(params_dt, "rate1")
    #   rate2 <- get_param_fun(params_dt, "rate2")
    #   rate3 <- get_param_fun(params_dt, "rate3")
    #   rate4 <- get_param_fun(params_dt, "rate4")
    #   tbrk1 <- get_param_fun(params_dt, "tbrk1")
    #   tbrk2 <- get_param_fun(params_dt, "tbrk2")
    #   tbrk3 <- get_param_fun(params_dt, "tbrk3")
    #   tbrk4 <- get_param_fun(params_dt, "tbrk4")
    #   
    #   rate_ded_income_agr_l <- get_param_fun(params_dt, "rate_ded_income_agr_l")
    #   rate_deductions_IndustrialPropertyRights_c <- get_param_fun(params_dt, "rate_deductions_IndustrialPropertyRights_c")
    #   sf_ded_work_l <- get_param_fun(params_dt, "sf_ded_work_l")
    #   sf_ded_games_s <- get_param_fun(params_dt, "sf_ded_games_s")
    #   sf_ded_betting_h <- get_param_fun(params_dt, "sf_ded_betting_h")
    #   sf_per_allowance <- get_param_fun(params_dt, "sf_per_allowance")
    #   capital_income_rate_g <- get_param_fun(params_dt, "capital_income_rate_g")
    #   capital_income_rate_c <- get_param_fun(params_dt, "capital_income_rate_c")
    #   
    #   # --- I. Tax on income from labor ---
    #   dt_scn[, tax_base_w := pmax(g_Wages_l - total_ssc - (g_total_personal_allowance_l * sf_per_allowance), 0)]
    #   dt_scn[, pit_w := (rate1 * pmin(tax_base_w, tbrk1) +
    #                        rate2 * pmin(tbrk2 - tbrk1, pmax(0, tax_base_w - tbrk1)) +
    #                        rate3 * pmin(tbrk3 - tbrk2, pmax(0, tax_base_w - tbrk2)) +
    #                        rate4 * pmax(0, tax_base_w - tbrk3))]
    #   
    #   # --- II. Tax on income from capital ---
    #   dt_scn[, tax_base_IndustrialPropertyRights_c := g_IndustrialPropertyRights_c * (1 - rate_deductions_IndustrialPropertyRights_c)]
    #   dt_scn[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c * capital_income_rate_c]
    #   
    #   # --- Total PIT ---
    #   dt_scn[, pitax := pit_w + pit_tax_IndustrialPropertyRights_c]
    #   dt_scn[, total_net := g_total_gross - (total_ssc + g_total_personal_allowance_l + pitax)]
    #   
    #   
    #   
    # }
    # 
    # 
    # 
    # 
    # 
    # # Retrieve growth factors for each scenario
    # vars_to_grow <- c(
    #   "g_Wages_l", "g_total_personal_allowance_l", "g_IndustrialPropertyRights_c",
    #   "g_total_gross", "total_ssc"
    # )
    # 
    # get_growth_factor_row <- function(scenario) {
    #   gf_row <- growth_factors[scenarios == scenario]
    #   out <- numeric(length(vars_to_grow))
    #   names(out) <- vars_to_grow
    #   for (v in vars_to_grow) {
    #     gf_col <- sub("_adjusted", "", v)
    #     out[v] <- gf_row[[gf_col]]
    #   }
    #   return(out)
    # }
    # 
    # # ============================================================
    # # STEP 2: Define Scenarios and Years
    # # ============================================================
    # 
    # base_year <- unique(dt$Year)[1]
    # end_year <- base_year + 5
    # forecast_horizon <- seq(base_year, end_year)
    # scenarios <- c("t0", "t1", "t2", "t3", "t4", "t5")
    # 
    # # ============================================================
    # # STEP 3: Generate PIT_BU_list
    # # ============================================================
    # 
    # PIT_BU_list <- list()
    # dt_scn_BU <- copy(dt)
    # 
    # for (s in scenarios) {
    #   gf_values <- get_growth_factor_row(s)
    #   
    #   # Apply growth and weights
    #   for (v in vars_to_grow) {
    #     dt_scn_BU[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
    #   }
    #   
    #   # Calculate PIT for this scenario
    #   tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
    #   
    #   # Add scenario weight
    #   dt_scn_BU[, weight := weights_pit[[s]]]
    #   
    #   # Save scenario data
    #   PIT_BU_list[[s]] <- copy(dt_scn_BU)
    # }
    # 
    # message("PIT_BU_list baseline calculation complete.\n")
    # 
    # # ============================================================
    # # STEP 4: Summarize PIT_BU_list
    # # ============================================================
    # 
    # summarize_PIT_fun_dt <- function(PIT_list, suffix) {
    #   summary_list <- lapply(names(PIT_list), function(scenario_name) {
    #     dt <- PIT_list[[scenario_name]]
    #     sums_dt <- dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = patterns("^(calc|pit)")]
    #     sums_dt[, scenarios := scenario_name]
    #     setcolorder(sums_dt, c("scenarios", setdiff(names(sums_dt), "scenarios")))
    #     sums_dt
    #   })
    #   result_dt <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)
    #   old_names <- setdiff(names(result_dt), "scenarios")
    #   new_names <- paste0(old_names, suffix)
    #   setnames(result_dt, old_names, new_names)
    #   as.data.frame(result_dt)
    # }
    # 
    # summary_BU <- summarize_PIT_fun_dt(PIT_BU_list, "_bu")
    # message("Baseline summary (summary_BU) generated.\n")
    # 
    # # ============================================================
    # # DONE
    # # ============================================================
    # # PIT_BU_list and summary_BU are now available in the environment.
    # 
    
    # ============================================================
    # PIT_BU_list_calculation.R  - FULL VERSION
    # ------------------------------------------------------------
    # Reads PIT data & parameters, defines ALL helper funcs,
    # runs full baseline (Business-as-Usual) across scenarios,
    # and builds PIT_BU_list + summary_BU.
    # ============================================================
    
    # ---- Setup & IO -------------------------------------------------------------
    library(data.table)
    library(readr)
    library(readxl)
    
    setDTthreads(threads = 8L)
    
    # Adjust path1 before this block or ensure it's already set in env
    path2 <- paste0(path1, "/Data/PIT")
    setwd(path2)
    
    # Data
    dt <- read_csv("mpin_epdd_nace_final.csv") %>% data.table()
    
    
    # Assumes: dt is a data.table and has column g_total_gross
    
    # --- Decile (1..10) and Percentile (1..100) groups, unweighted ---
    dt[, decile_group := fifelse(
      is.na(g_total_gross),
      NA_integer_,
      pmin(10L, as.integer(ceiling(10 * data.table::frank(g_total_gross, ties.method = "average")/.N)))
    )]
    
    dt[, centile_group := fifelse(
      is.na(g_total_gross),
      NA_integer_,
      pmin(100L, as.integer(ceiling(100 * data.table::frank(g_total_gross, ties.method = "average")/.N)))
    )]
    
    
    
    
    
    
    
    
    
    MACRO_FISCAL_INDICATORS <- read_excel("macro_indicators.xlsx")
    
    # Growth factors & weights
    growth_factors <- read.csv("growfactors_pit_mkd.csv") %>% data.table()
    
    n <- NROW(dt)
    same_weight <- 1L
    weights_pit <- data.table(
      t0 = rep(same_weight, n),
      t1 = rep(1, n),
      t2 = rep(1, n),
      t3 = rep(1, n),
      t4 = rep(1, n),
      t5 = rep(1, n)
    )
    rm(n)
    
    # Parameters
    pit_simulation_parameters_raw <- read_excel("PIT-Parameters.xlsx") %>% data.table()
    
    # ---- Helper: parameter fetch ------------------------------------------------
    get_param_fun <- function(params_dt, param_name) {
      params_dt[Parameters == param_name, Value]
    }
    
    # ---- Years & Scenarios ------------------------------------------------------
    base_year <- unique(dt$Year)[1]
    end_year  <- base_year + 5
    #forecast_horizon <- seq(base_year, end_year)
    forecast_horizon_pit <- seq(base_year, end_year)
    scenarios <- c("t0","t1","t2","t3","t4","t5")
    

   # start.time <- proc.time()
    
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
      capital_income_rate_g <- get_param_fun(params_dt, "capital_income_rate_g")
      capital_income_rate_c <- get_param_fun(params_dt, "capital_income_rate_c")
      
      # I. INCOME FROM LABOR ------------------------------------------------------
      # 1. Wages base
      dt_scn[, tax_base_w := {
        personal_allowance_new <- g_total_personal_allowance_l * sf_per_allowance
        tax_base_wages1 <- pmax(g_Wages_l - total_ssc - personal_allowance_new, 0)
        tax_base_wages_diplomatic_consular_l <- g_WagesDiplomaticConsular_l - d_WagesDiplomaticConsular_l
        tax_base_wages1 + tax_base_wages_diplomatic_consular_l
      }]
      
      # 2. Own agricultural products
      dt_scn[, tax_base_agr := g_AgriculturalProductsOwn_l - (g_AgriculturalProductsOwn_l * rate_ded_income_agr_l)]
      dt_scn[, pit_tax_agr := tax_base_agr * rate1]
      
      # 3. Copyright Artistic Photography
      dt_scn[, tax_base_CopyrightIncomeArtisticPhotography_l :=
               g_CopyrightIncomeArtisticPhotography_l -
               (g_CopyrightIncomeArtisticPhotography_l * rate_deductions_CopyrightIncomeArtisticPhotography_l)]
      dt_scn[, pit_tax_CopyrightIncomeArtisticPhotography_l := tax_base_CopyrightIncomeArtisticPhotography_l * rate1]
      
      # 4. Copyright Music/Ballet
      dt_scn[, tax_base_CopyrightIncomeMusicBallet_l :=
               g_CopyrightIncomeMusicBallet_l -
               (g_CopyrightIncomeMusicBallet_l * rate_deductions_CopyrightIncomeMusicBallet_l)]
      dt_scn[, pit_tax_CopyrightIncomeMusicBallet_l := tax_base_CopyrightIncomeMusicBallet_l * rate1]
      
      # 5. Copyright Paintings/Sculptural
      dt_scn[, tax_base_CopyrightIncomePaintingsSculptural_l :=
               g_CopyrightIncomePaintingsSculptural_l -
               (g_CopyrightIncomePaintingsSculptural_l * rate_deductions_CopyrightIncomePaintingsSculptural_l)]
      dt_scn[, pit_tax_CopyrightIncomePaintingsSculptural_l := tax_base_CopyrightIncomePaintingsSculptural_l * rate1]
      
      # 6. Copyright Translations/Lectures
      dt_scn[, tax_base_CopyrightIncomeTranslationsLectures_l :=
               g_CopyrightIncomeTranslationsLectures_l -
               (g_CopyrightIncomeTranslationsLectures_l * rate_deductions_CopyrightIncomeTranslationsLectures_l)]
      dt_scn[, pit_tax_CopyrightIncomeTranslationsLectures_l := tax_base_CopyrightIncomeTranslationsLectures_l * rate1]
      
      # 7. Copyright Successor
      dt_scn[, tax_base_CopyrightIncomeSuccessor_l :=
               g_CopyrightIncomeSuccessor_l -
               (g_CopyrightIncomeSuccessor_l * rate_deductions_CopyrightIncomeSuccessor_l)]
      dt_scn[, pit_tax_CopyrightIncomeSuccessor_l := tax_base_CopyrightIncomeSuccessor_l * rate1]
      
      # 8. Work Income
      dt_scn[, tax_base_WorkIncome_l := g_WorkIncome_l - (d_WorkIncome_l * sf_ded_work_l)]
      dt_scn[, pit_tax_WorkIncome_l := tax_base_WorkIncome_l * rate1]
      
      # 9. OTHER income from labor (excl. wages)
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
        0
      )]
      
      # 10. PIT on labor
      dt_scn[, tti_w_I := tax_base_w + tax_base_other]
      dt_scn[, pit_w :=
               (rate1 * pmin(tti_w_I, tbrk1) +
                  rate2 * pmin(tbrk2 - tbrk1, pmax(0, tti_w_I - tbrk1)) +
                  rate3 * pmin(tbrk3 - tbrk2, pmax(0, tti_w_I - tbrk2)) +
                  rate4 * pmax(0, tti_w_I - tbrk3))]
      
      # II. INCOME FROM CAPITAL ---------------------------------------------------
      # 1. IPR (prescribed costs)
      dt_scn[, tax_base_IndustrialPropertyRights_c :=
               g_IndustrialPropertyRights_c - (g_IndustrialPropertyRights_c * rate_deductions_IndustrialPropertyRights_c)]
      dt_scn[, pit_tax_IndustrialPropertyRights_c := tax_base_IndustrialPropertyRights_c * capital_income_rate_c]
      
      # 2. Lease
      dt_scn[, tax_base_Lease_c := g_Lease_c - (g_Lease_c * rate_deductions_Lease_c)]
      dt_scn[, pit_tax_Lease_c := tax_base_Lease_c * capital_income_rate_c]
      
      # 3. Lease of equipped premises
      dt_scn[, tax_base_LeaseBusiness_c := g_LeaseBusiness_c - (g_LeaseBusiness_c * rate_deductions_LeaseBusiness_c)]
      dt_scn[, pit_LeaseBusiness_c := tax_base_LeaseBusiness_c * capital_income_rate_c]
      
      # 4. Solid waste
      dt_scn[, tax_base_SolidWaste_c := g_SolidWaste_c - (g_SolidWaste_c * rate_deductions_SolidWaste_c)]
      dt_scn[, pit_SolidWaste_c := tax_base_SolidWaste_c * capital_income_rate_c]
      
      # 5. Games of chance
      # 5.1 General
      dt_scn[, tax_base_GamesofChanceGeneral_c := g_GamesofChanceGeneral_c]
      dt_scn[, pit_GamesofChanceGeneral_c := tax_base_GamesofChanceGeneral_c * capital_income_rate_g]
      
      # 5.2 Specific
      dt_scn[, tax_base_GamesofChanceSpecific_c := g_GamesofChanceSpecific_c - (d_GamesofChanceSpecific_c * sf_ded_games_s)]
      dt_scn[, pit_GamesofChanceSpecific_c := tax_base_GamesofChanceSpecific_c * capital_income_rate_g]
      
      # 5.3 Betting shop
      dt_scn[, tax_base_GamesofChanceBettingShop_c := g_GamesofChanceBettingShop_c - (d_GamesofChanceBettingShop_c * sf_ded_betting_h)]
      dt_scn[, pit_GamesofChanceBettingShop_c := tax_base_GamesofChanceBettingShop_c * capital_income_rate_g]
      
      # 6. IPR Successor
      dt_scn[, tax_base_IndustrialPropertyRightsSuccessor_c := g_IndustrialPropertyRightsSuccessor_c]
      dt_scn[, pit_IndustrialPropertyRightsSuccessor_c := tax_base_IndustrialPropertyRightsSuccessor_c * capital_income_rate_c]
      
      # 7. Insurance
      dt_scn[, tax_base_Insurance_c := g_Insurance_c]
      dt_scn[, pit_Insurance_c := tax_base_Insurance_c * capital_income_rate_c]
      
      # 8. Interests
      dt_scn[, tax_base_Interests_c := g_Interests_c]
      dt_scn[, pit_Interests_c := tax_base_Interests_c * capital_income_rate_c]
      
      # 9. Other income
      dt_scn[, tax_base_OtherIncome_c := g_OtherIncome_c]
      dt_scn[, pit_OtherIncome_c := tax_base_OtherIncome_c * capital_income_rate_c]
      
      # 10. Sublease
      dt_scn[, tax_base_Sublease_c := g_Sublease_c]
      dt_scn[, pit_Sublease_c := tax_base_Sublease_c * capital_income_rate_c]
      
      # 11. CapitalIncome
      dt_scn[, tax_base_CapitalIncome_c := g_CapitalIncome_c]
      dt_scn[, pit_CapitalIncome_c := tax_base_CapitalIncome_c * capital_income_rate_c]
      
      # 11a. (commented in original)
      # dt_scn[, tax_base_deposit_c := gross_int_t_dep]
      # dt_scn[, pit_Int_t_dep_c := tax_base_deposit_c * capital_income_rate_c]
      
      # 12. Total capital tax base
      # 12.1 OTHER than games of chance
      dt_scn[, tti_c_a :=
               g_IndustrialPropertyRightsSuccessor_c + g_Insurance_c + g_Interests_c + g_OtherIncome_c + g_Sublease_c +
               tax_base_IndustrialPropertyRights_c + tax_base_Lease_c + tax_base_LeaseBusiness_c + tax_base_SolidWaste_c +
               tax_base_Insurance_c + g_CapitalIncome_c
             # + tax_base_deposit_c
      ]
      
      # 12.2 ONLY games of chance (15%)
      dt_scn[, tti_c_g := tax_base_GamesofChanceGeneral_c + tax_base_GamesofChanceSpecific_c + tax_base_GamesofChanceBettingShop_c]
      
      # 12.3 Total PIT on capital
      dt_scn[, pit_c :=
               pit_tax_IndustrialPropertyRights_c +
               pit_tax_Lease_c +
               pit_LeaseBusiness_c +
               pit_SolidWaste_c +
               pit_GamesofChanceSpecific_c +
               pit_GamesofChanceBettingShop_c +
               pit_IndustrialPropertyRightsSuccessor_c +
               pit_Insurance_c +
               pit_Interests_c +
               pit_OtherIncome_c +
               pit_Sublease_c +
               pit_CapitalIncome_c
             # + pit_Int_t_dep_c
      ]
      
      # III. TOTAL PIT ------------------------------------------------------------
      dt_scn[, total_taxbase := tti_c_g + tti_c_a + tti_w_I]
      dt_scn[, pitax := pit_w + pit_c]
      dt_scn[, total_net := g_total_gross - (total_ssc + g_total_personal_allowance_l + pitax)]
    }
    
    # ---- Helper to Retrieve Growth Factors (FULL vars_to_grow) ------------------
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
      # "gross_int_t_dep"
    )
    
    get_growth_factor_row <- function(scenario) {
      gf_row <- growth_factors[scenarios == scenario]
      out <- numeric(length(vars_to_grow))
      names(out) <- vars_to_grow
      for (v in vars_to_grow) {
        gf_col <- sub("_adjusted", "", v)
        out[v] <- gf_row[[gf_col]]
      }
      out
    }
    
    # ---- BUSINESS AS USUAL (PIT_BU_list) ----------------------------------------
    PIT_BU_list <- list()
    dt_scn_BU <- copy(dt)
    
    for (s in scenarios) {
      gf_values <- get_growth_factor_row(s)
      
      # Apply growth and weights per variable
      for (v in vars_to_grow) {
        dt_scn_BU[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
      }
      
      # Row-wise tax logic with RAW params
      tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
      
      # Weight column for this scenario
      dt_scn_BU[, weight := weights_pit[[s]]]
      
      # Store scenario table
      PIT_BU_list[[s]] <- copy(dt_scn_BU)
    }
    
    message("PIT_BU_list baseline calculation complete.\n")
    
    # ---- Summarize BU -----------------------------------------------------------
    summarize_PIT_fun_dt <- function(PIT_list, suffix) {
      summary_list <- lapply(names(PIT_list), function(scenario_name) {
        dtx <- PIT_list[[scenario_name]]
        sums_dt <- dtx[, lapply(.SD, sum, na.rm = TRUE), .SDcols = patterns("^(calc|pit)")]
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
    
    summary_BU <- summarize_PIT_fun_dt(PIT_BU_list, "_bu")
    message("Baseline summary (summary_BU) generated.\n")
    
    # # ---- Timing -----------------------------------------------------------------
    # end.time <- proc.time()
    # save.time <- end.time - start.time
    # cat("\n Number of minutes running:", save.time[3] / 60, "\n\n")
    # 
    # Done: PIT_BU_list and summary_BU in env
    # ============================================================================
    
    
    
    

      
# 2.VAT ---------------------------------------------------------------------

    'DATA PREPROCESSING MODULE          
                       '
    
    base_year_VAT<-2022 # <-This is the same year as the year from which the data originates.
    max_time_horizon<-base_year_VAT+5
    time_horizon<-seq(base_year_VAT,max_time_horizon)
    

    path3 <- paste0(path1, "/Data/VAT")
    setwd(path3)
    getwd()
    
    # 1. DEFINE FUNCTIONS ----
    
    #  The function creates an ntile group vector:
    qgroup = function(numvec, n, na.rm=TRUE){
      qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm)  
      out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
      return(out)
    }
    
    #  to extract only English names from SUTs
    trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
    
    input_output_matrix_to_long_data <- function(matrix){
      
      matrix <- matrix %>%
        dplyr::filter(...2 != "NA")
      
      colnames(matrix) <- matrix[1,]
      
      data <- matrix[c(-1,-2), c(-1,-2)] %>%
        as.matrix() %>%               
        reshape2::melt()              
      
      ## -------------------------
      
      product_industry_name <- matrix[[2]][c(-1,-2)]
      product_industry_code <- matrix[[1]][c(-1,-2)]
      industry_code <- matrix[2, c(-1,-2)] %>% as.character()
      
      data$Var1 <- rep(product_industry_name, time = length(industry_code))
      
      data <- data %>% 
        dplyr::rename(PRODUCT_INDUSTRY_NAME = Var1,
                      INDUSTRY_NAME = Var2)
      
      data$PRODUCT_INDUSTRY_CODE <- rep(product_industry_code, time = length(industry_code))
      data$INDUSTRY_CODE <- rep(industry_code, each = length(product_industry_code))
      
      data <- data %>% 
        dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value)
      
      data$PRODUCT_INDUSTRY_NAME <- gsub("^.*\\/", "", data$PRODUCT_INDUSTRY_NAME) %>% trim()
      data$INDUSTRY_NAME <- gsub("^.*\\/", "", data$INDUSTRY_NAME) %>% trim()
      
      data$value <- as.numeric(as.character(data$value))
      
      return(data)
    }
    
    
    
    # 2. RAW DATA IMPORT AND PREPROCESS  ----- 
    
    

    # CPA_TAX_PROP_SELECTED_WITH_RATES_RAW <- read_excel(
    #                                                   file.path(path3, "VAT-Data-Template.xlsx"),
    #                                                    sheet = "CPA_VAT_RATES_old"
    # )
    # 
    
    # RATES_BY_CPA<-read_excel("VAT-Data-Template.xlsx", 
    #                          sheet = "CPA_VAT_RATES")
    # 
    
    RATES_BY_CPA <- read_excel(
                                        file.path(path3, "VAT-Data-Template.xlsx"),
                                        sheet = "CPA_VAT_RATES"
                                      )
    
    
    # New
    
    
    # taxable_proportions_raw <- read_excel(
    #   file.path(path3, "TaxableProportions-5.xlsx")
    # )
    
    taxable_proportion_bu <- read_excel(
                      file.path(path3, "VAT-Data-Template.xlsx"),
                sheet = "Taxable_proportions_BU"
                )
    
    
    
  
    # Initialize empty lists to store the tables
    CPA_TAXABLE_PROPORTIONS_BU_list <- list()
    CPA_TAXABLE_PROPORTIONS_SIM_list <- list()
    
    # Created empty dataframe
    CPA_TAXABLE_PROPORTIONS_BASELINE <- data.frame(
                                                  PRODUCT_INDUSTRY_CODE = character(64),
                                                  PRODUCT_INDUSTRY_NAME = character(64),
                                                  Current_Policy_Exempt = numeric(64),
                                                  Current_Policy_Reduced_Rate = numeric(64),
                                                  Current_Policy_Fully_Taxable = numeric(64),
                                                  PreferentialVATRate_1 = numeric(64),
                                                  PreferentialVATRate_2 = numeric(64),
                                                  StandardVATRate = numeric(64),
                                                  stringsAsFactors = FALSE
                                                )
    
    
    
    CPA_TAXABLE_PROPORTIONS_BASELINE_BU<-CPA_TAXABLE_PROPORTIONS_BASELINE
    
    
    ' 
                    In this section data are imported from five files:
                    
                    VAT_Model_v9.16a2.xlsx
                    TaxableProportions-4a.xlsx
                    MACRO_FISCAL_INDICATORS.xlsx
                    Data4_hbs2020.xlsx  <---HBS DATA
                    NACE_SUT_table.xlsx
                    '
    
    # Name of the version of model
    #version_vat_model<-c("VAT_Model_v9.16a2.xlsx")
   # version_vat_model<-c("VAT_Model_v9.17a3-1.xlsx")
    version_vat_model<-c("SUT_DATA.xlsx")
    

    
    macro_fiscal_raw <- read_excel(
      file.path(path3, "MACRO_FISCAL_INDICATORS.xlsx"),
      sheet = "MediumTermForecast"
    )
    
    
    
 
    
    
    
    # 2.1 SUTs ------------------------------------
    
    SUPPLY_raw <- read_excel(version_vat_model, sheet = "Supply", col_names = F)[c(-1,-2,-3,-4),] %>%
      input_output_matrix_to_long_data()
    
    "Each value from Use_Purchaser are imported here"
    USE_PURCHASER_raw <- read_excel(version_vat_model, sheet = "Use_Purchaser", col_names = F)[c(-1,-2,-3,-4),] %>%
      input_output_matrix_to_long_data()
    
    USE_VAT_raw <- read_excel(version_vat_model, sheet = "Use_VAT", col_names = F)[c(-1,-2,-3,-4),] %>%
      input_output_matrix_to_long_data()
    
    USE_BASIC_raw <- read_excel(version_vat_model, sheet = "Use_Basic", col_names = F)[c(-1,-2,-3,-4),] %>%
      input_output_matrix_to_long_data()
    
    
    # 2.2 COICOP table ------------------------------------------------------------
    # Please import VAT rates that are available at the moment of producing of VAT COICOP
    vat_bu_rate_preferential1<-0.05
    vat_bu_rate_preferential2<-0.05
    vat_bu_rate_standard<-0.18
    RC_prc_of_Constructions_and_construction_works = 0.3
    vat_rate_on_residential_construction = 0.05
    
    
    
    # # Create empty data frame 
    # CPA_BASE_TAXABLE_PROPORTION<-data.frame(Base=numeric())
    
    
    ## Data from COICOP table
    VAT_COICOP_NAMES <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1:2),c(2)]
    VAT_COICOP_NAMES<-VAT_COICOP_NAMES[1:178,1]
    
    
    VAT_COICOP_FC <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1:2),c(4:9)]
    VAT_COICOP_FC<-VAT_COICOP_FC[1:178,1:6]
    
    
    VAT_COICOP_FINAL_RAW<-cbind(VAT_COICOP_NAMES,VAT_COICOP_FC)
    
    VAT_COICOP_FINAL_RAW<-VAT_COICOP_FINAL_RAW%>%
      dplyr:: rename(c("COICOP_Descriptions"= "...2",
                       "FC"="...4",
                       "EX"= "...5",
                       "Reduced_Rate_5"="...6",
                       "Standard_Rate_18"= "...7",
                       "VAT_Revenue_5"="...8",
                       "VAT_Revenue_18"="...9"
                       
      ))
    
    # Select NACE industries on four digits for calculation  
    
    VAT_COICOP_FINAL_RAW<-subset(VAT_COICOP_FINAL_RAW, grepl("^\\d{2}\\.\\d\\.\\d\\s+", COICOP_Descriptions))
    
    # Extract NACE codes
    VAT_COICOP_FINAL_RAW$Four_digits<-substr(VAT_COICOP_FINAL_RAW$COICOP_Descriptions, 1, 6)
    VAT_COICOP_FINAL_RAW$Two_digits<-substr(VAT_COICOP_FINAL_RAW$COICOP_Descriptions, 1, 2)
    VAT_COICOP_FINAL_RAW[is.na(VAT_COICOP_FINAL_RAW)] <- 0
    VAT_COICOP_FINAL_RAW$EX<-as.numeric(VAT_COICOP_FINAL_RAW$EX)
    VAT_COICOP_FINAL_RAW$VAT_Revenue_5<-as.numeric(VAT_COICOP_FINAL_RAW$VAT_Revenue_5)
    
    
    # Input raw concordance table
    ConcordanceVAT_COICOP_CPA <- read_excel(version_vat_model, sheet = "Concordance", col_names = T)
    
    
    
    # Merging table <--- This table will be used in GUI NEW 1.6.2024
    VAT_COICOP_FINAL<-left_join(VAT_COICOP_FINAL_RAW,ConcordanceVAT_COICOP_CPA,by = c("COICOP_Descriptions"))
    
    ConcordanceVAT_COICOP_CPA<-ConcordanceVAT_COICOP_CPA %>% filter(!is.na(Four_digits))
    ConcordanceVAT_COICOP_CPA<-ConcordanceVAT_COICOP_CPA %>% filter(!is.na(CPA_CODE))
    
    
    # # Adjustment of CPA codes with Concordance table
    # COICOP <- read_excel(version_vat_model, sheet = "COICOP", col_names = F)[-c(1,2),-c(1:19)]
    # COICOP[1,1] <- "PRODUCT_INDUSTRY_CODE"
    # COICOP[1,5] <- "Negative"
    # 
    # colnames(COICOP) <- c("PRODUCT_INDUSTRY_CODE", "Base",  "Exempt_Levels", "Reduced_Rate_Levels", "Negative", "Exempt_Adjustment", "Reduced_Rate_Adjustment", 
    #                       "Exempt_Levels_2", "Reduced_Rate_Levels_2",
    #                       "Exempt_Raw_perc", "Reduced_Rate_Raw_perc", "Exempt_Capped_perc", "Reduced_Rate_Capped_perc")
    # 
    # COICOP <- COICOP[-c(1, 66:nrow(COICOP)),]
    # 
    # COICOP <- COICOP %>%
    #   dplyr::arrange(PRODUCT_INDUSTRY_CODE)
    # 
    
    
    # 2.5 Concordance table NACE_SUT ----------------------------------
    
    #NACE_SUT_table <- read_excel("NACE_SUT_table.xlsx", sheet = "NACE")
    #CPA_COICOP_CONCORDANCE <- read_excel("NACE_SUT_table.xlsx", sheet = "CPA_COICOP_CONCORDANCE")
    
    
    CPA_COICOP_CONCORDANCE <- read_excel(
      file.path(path3, "VAT-Data-Template.xlsx"),
      sheet = "CPA_COICOP_CONCORDANCE"
    )
    
    # 2.6 HBS  ----------------------------------------
    # # Import data
    # data4_hbs <- read_excel("Data4_hbs2020.xlsx")
    # 
    # # Setting columns names
    # data4_hbs<-data4_hbs%>%
    #   dplyr::select(-c('kvartal','Year'))
    # 
    # colnames(data4_hbs)<-c("number_hh","01","02","03","04","05","06","07","08","09","10","11","12","Consumption_own")
    # 
    # # Preparing data for merging 
    # data4_hbs_long<-data4_hbs%>%
    #   pivot_longer(!number_hh, names_to = "COICOP_section", values_to = "Expenditures")
    # 
    # weight_hbs <- read_excel("Weight_hbs2020.xls")
    # 
    
    # 2.7 MACRO-FISCAL INDICATORS ---------------------------------------------
    #MACRO_FISCAL_INDICATORS <- read_excel("MACRO_FISCAL_INDICATORS.xlsx")
    
    macro_fiscal_indicators_vat<-read_excel("MACRO_FISCAL_INDICATORS.xlsx")
    
    # FinalConsumption <- read_excel("MACRO_FISCAL_INDICATORS.xlsx", 
    #                                sheet = "FinalConsumption")
    
    # 3. INSERT TAXABLE PROPORTIONS SIMULATION PARAMETERS ---------------------------------------------
  
    
    
    # taxable_proportion_bu <- taxable_proportions_raw %>%
    #   dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
    #                 Simulated_Policy_Reduced_Rate = ifelse(is.na(PreferentialVATRate_1), Current_Policy_Reduced_Rate, PreferentialVATRate_1),
    #                 Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
    
    
    taxable_proportion_bu <- taxable_proportion_bu %>%
      dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
                    Simulated_Policy_Reduced_Rate = ifelse(is.na(PreferentialVATRate_1), Current_Policy_Reduced_Rate, PreferentialVATRate_1),
                    Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
    
    
    ### NEW 22/12/2024
    
    #CPA_TAXABLE_PROPORTIONS_BU<-read_excel("~/Models/Tax-Modeling-Toolkit/VAT_TaxableProportions.xlsx")
    
    
    # CPA_TAXABLE_PROPORTIONS_BU <- read_excel(
    #                                               file.path(path3, "VAT_TaxableProportions.xlsx")
    #                                             )
    
    
    # CPA_TAXABLE_PROPORTIONS_BU <- read_excel(
    #   file.path(path3, "VAT_TaxableProportions.xlsx")
    # )
    # 
    
    CPA_TAXABLE_PROPORTIONS_BU<- read_excel(
      file.path(path3, "VAT-Data-Template.xlsx"),
      sheet = "Taxable_proportions_BU"
    )
    
    ### NEW 27/12/2024
    
   # growfactors_vat <- read.csv("~/Models/Tax-Modeling-Toolkit/Data/VAT/growfactors_vat.csv")
    
    
    growfactors_vat <- read.csv(
      file.path(path3, "growfactors_vat.csv")
    )
    
        
# III. SAVE DATA IN R ENVIRONMENT (RDS FILE) --------------------------------------------------------
   
    gc(TRUE)             
                  setwd(path1)
                  save.image(file=".RData") 
          
                  
