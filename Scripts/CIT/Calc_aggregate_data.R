
'Preparation of percentiles and decile'

# I. Large companies ---------------------------------------------------------

                        # 0. Functions for calculation -----------------------------------------------
extract_filtered_re_df_fun <- function(CIT_BU_list, forecast_horizon, simulation_year,
                                       filter_positive = FALSE) {
  if (!simulation_year %in% forecast_horizon) {
    stop("The specified simulation year is not in the forecast horizons.")
  }
  index <- which(forecast_horizon == simulation_year)

  CIT_BU_simulation_year_df <- CIT_BU_list[[index]]
  
  # Define the columns to keep.
  columns_to_keep <- c("id_n",
                       "nace",
                       "financial_results_r",
                       "tb_ar_deductions_calc",
                       "rb_tax_reductions_calc",
                       "citax"
                        )
  
  # Check for missing columns and issue a warning if any are not found.
  missing_columns <- setdiff(columns_to_keep, colnames(CIT_BU_simulation_year_df))
  if (length(missing_columns) > 0) {
    warning("The following columns are missing in the data frame: ",
            paste(missing_columns, collapse = ", "))
  }
  
  # Filter the data.table to keep only the specified columns.
  CIT_BU_simulation_year_df <- CIT_BU_simulation_year_df[, ..columns_to_keep, with = FALSE]
  
  if (filter_positive) {
    condition <- Reduce("&", lapply(columns_to_keep, function(col) {
      if (is.numeric(CIT_BU_simulation_year_df[[col]])) {
        CIT_BU_simulation_year_df[[col]] > 0
      } else {
        rep(TRUE, nrow(CIT_BU_simulation_year_df))
      }
    }))
    CIT_BU_simulation_year_df <- CIT_BU_simulation_year_df[condition]
  }
  
  return(CIT_BU_simulation_year_df)
}




          # 1.BU ----------------------------------------------------------------------
                CIT_BU_sim_year_df <- extract_filtered_re_df_fun(CIT_BU_list, forecast_horizon, simulation_year)
                  
                          # 1.Filtering companies with positive results -----------------------------------------------------------------------
                          
                          CIT_BU_sim_year_pos_res<-CIT_BU_sim_year_df %>%
                                      dplyr::select(id_n,nace,financial_results_r,tb_ar_deductions_calc, citax) %>%
                                      dplyr::filter(financial_results_r>0)
                          
                          
                          
                                  CIT_BU_sim_year_pos_res[, decile_group := fifelse(
                                            is.na(financial_results_r),
                                            NA_integer_,
                                            pmin(10L, as.integer(ceiling(10 * data.table::frank(financial_results_r, ties.method = "average")/.N)))
                                          )]
                                  
                                  CIT_BU_sim_year_pos_res[, percentile_group := fifelse(
                                              is.na(financial_results_r),
                                              NA_integer_,
                                              pmin(100L, as.integer(ceiling(100 * data.table::frank(financial_results_r, ties.method = "average")/.N)))
                                            )]
                                  
                                  
                          
                          # 2.Percentile --------------------------------------------------------------
                                  etr_cit_large_percentile_bu <- CIT_BU_sim_year_pos_res %>%
                                            dplyr::select(financial_results_r,percentile_group,tb_ar_deductions_calc, citax) %>%
                                            dplyr::group_by(percentile_group)%>%
                                            dplyr::summarize(ETR = round((sum(citax, na.rm = TRUE) / sum(tb_ar_deductions_calc, na.rm = TRUE)) * 100, 2))
                          
                          # 3.Decile  -----------------------------------------------------------------
                                  etr_cit_large_decile_bu <- CIT_BU_sim_year_pos_res %>%
                                            dplyr::select(financial_results_r,decile_group,tb_ar_deductions_calc, citax) %>%
                                            dplyr::group_by(decile_group)%>%
                                            dplyr::summarize(ETR = round((sum(citax, na.rm = TRUE) / sum(tb_ar_deductions_calc, na.rm = TRUE)) * 100, 2))
                                  
                          
                          # 4.NACE ------------------------------------------------------------------
                          
                                            etr_cit_large_nace_bu <- CIT_BU_sim_year_pos_res %>%
                                              dplyr::select(financial_results_r,nace,tb_ar_deductions_calc, citax) %>%
                                              dplyr::group_by(nace)%>%
                                              dplyr::summarize(ETR= round((sum(citax, na.rm = TRUE) / sum(tb_ar_deductions_calc, na.rm = TRUE)) * 100, 2))
                                
                          
                                  
                                  
                                  
          # 2.SIM -------------------------------------------------------------------
          
                          CIT_SIM_sim_year_df <- extract_filtered_re_df_fun(CIT_SIM_list, forecast_horizon, simulation_year)
                          
                          # 1.Filtering companies with positive results -----------------------------------------------------------------------
                          
                          CIT_SIM_sim_year_pos_res<-CIT_SIM_sim_year_df %>%
                            dplyr::select(id_n,nace,financial_results_r,tb_ar_deductions_calc, citax) %>%
                            dplyr::filter(financial_results_r>0)
                          
                          
                          
                          CIT_SIM_sim_year_pos_res[, decile_group := fifelse(
                            is.na(financial_results_r),
                            NA_integer_,
                            pmin(10L, as.integer(ceiling(10 * data.table::frank(financial_results_r, ties.method = "average")/.N)))
                          )]
                          
                          CIT_SIM_sim_year_pos_res[, percentile_group := fifelse(
                            is.na(financial_results_r),
                            NA_integer_,
                            pmin(100L, as.integer(ceiling(100 * data.table::frank(financial_results_r, ties.method = "average")/.N)))
                          )]
                          
                          
                          
                          # 2.Percentile --------------------------------------------------------------
                          etr_cit_large_percentile_sim <- CIT_SIM_sim_year_pos_res %>%
                            dplyr::select(financial_results_r,percentile_group,tb_ar_deductions_calc, citax) %>%
                            dplyr::group_by(percentile_group)%>%
                            dplyr::summarize(ETR = round((sum(citax, na.rm = TRUE) / sum(tb_ar_deductions_calc, na.rm = TRUE)) * 100, 2))
                          
                          # 3.Decile  -----------------------------------------------------------------
                          etr_cit_large_decile_sim <- CIT_SIM_sim_year_pos_res %>%
                            dplyr::select(financial_results_r,decile_group,tb_ar_deductions_calc, citax) %>%
                            dplyr::group_by(decile_group)%>%
                            dplyr::summarize(ETR= round((sum(citax, na.rm = TRUE) / sum(tb_ar_deductions_calc, na.rm = TRUE)) * 100, 2))
                          
                          
                          # 4.NACE ------------------------------------------------------------------
                          
                          etr_cit_large_nace_sim <- CIT_SIM_sim_year_pos_res %>%
                            dplyr::select(financial_results_r,nace,tb_ar_deductions_calc, citax) %>%
                            dplyr::group_by(nace)%>%
                            dplyr::summarize(ETR= round((sum(citax, na.rm = TRUE) / sum(tb_ar_deductions_calc, na.rm = TRUE)) * 100, 2))
                          
                         
          # 3.Merging data ----------------------------------------------------------
                        
                          CIT_rev_nace<- merge(CIT_BU_sim_year_pos_res, CIT_SIM_sim_year_pos_res[, c("id_n","citax")], 
                                               by = c("id_n"), 
                                               suffixes = c("_bu", "_sim"))%>%
                                              dplyr::select(nace,financial_results_r,citax_bu,citax_sim)%>%
                                              dplyr::left_join(nace_description,by=c("nace"))%>%
                                              dplyr::select(description,financial_results_r,citax_bu,citax_sim,section)%>%
                                              dplyr::group_by(section,description)%>%
                                              dplyr::summarise(financial_results_r=sum( financial_results_r),
                                                               citax_bu=sum(citax_bu),
                                                               citax_sim=sum(citax_sim)
                                                               )
                                               
                                       
                        
                          
                          percentile_large_companies_percentile <- merge(etr_cit_large_percentile_bu, etr_cit_large_percentile_sim[, c("percentile_group",  "ETR")], 
                                                    by = c("percentile_group"), 
                                                    suffixes = c("_bu", "_sim"))
                                         
                          
                          decile_large_companies_decile <- merge(etr_cit_large_decile_bu, etr_cit_large_decile_sim[, c("decile_group",  "ETR")], 
                                                               by = c("decile_group"), 
                                                               suffixes = c("_bu", "_sim"))
            
                          nace_large_companies_raw <- merge(etr_cit_large_nace_bu, etr_cit_large_nace_sim[, c("nace",  "ETR")], 
                                                                     by = c("nace"), 
                                                                     suffixes = c("_bu", "_sim"))
                          
                          
                          nace_large_companies <- nace_large_companies_raw %>%
                                    left_join(nace_description, by = "nace") %>%
                                    select(-nace) %>%
                                    group_by(section, description) %>%
                                    summarise(
                                      n = n(),
                                      across(c(ETR_bu, ETR_sim), ~ mean(.x, na.rm = TRUE), .names = "avg_{.col}")
                                    ) %>%
                                    ungroup()%>%
                                    select(-("n"))
                                  
                          
                          # DO TUKA !!! DA SE PRODOLZI SO PRAVANJE NA CHARTOVI ZA DASHBOARDITE
                          

                          
# II.Small companies ------------------------------------------------------
                          # 0. Functions for calculation -----------------------------------------------

                          extract_filtered_re_df_fun <- function(CIT_BU_list_small, forecast_horizon, simulation_year,
                                                                 filter_positive = FALSE) {
                            if (!simulation_year %in% forecast_horizon) {
                              stop("The specified simulation year is not in the forecast horizons.")
                            }
                            index <- which(forecast_horizon == simulation_year)
                            
                            CIT_BU_simulation_year_small_df <- CIT_BU_list_small[[index]]
                            
                            # Define the columns to keep.
                            columns_to_keep <- c("id_n",
                                                 "nace",
                                                 "bs_total_income",
                                                 "citax_turnover"
                            )
                            
                            # Check for missing columns and issue a warning if any are not found.
                            missing_columns <- setdiff(columns_to_keep, colnames(CIT_BU_simulation_year_small_df))
                            if (length(missing_columns) > 0) {
                              warning("The following columns are missing in the data frame: ",
                                      paste(missing_columns, collapse = ", "))
                            }
                            
                            # Filter the data.table to keep only the specified columns.
                            CIT_BU_simulation_year_small_df <- CIT_BU_simulation_year_small_df[, ..columns_to_keep, with = FALSE]
                            
                            if (filter_positive) {
                              condition <- Reduce("&", lapply(columns_to_keep, function(col) {
                                if (is.numeric(CIT_BU_simulation_year_small_df[[col]])) {
                                  CIT_BU_simulation_year_small_df[[col]] > 0
                                } else {
                                  rep(TRUE, nrow(CIT_BU_simulation_year_small_df))
                                }
                              }))
                              CIT_BU_simulation_year_small_df <- CIT_BU_simulation_year_small_df[condition]
                            }
                            
                            return(CIT_BU_simulation_year_small_df)
                          }
                          
                          
                          
                          
          # 1.BU ----------------------------------------------------------------------
                          CIT_BU_sim_year_small_df <- extract_filtered_re_df_fun(CIT_BU_list_small, forecast_horizon, simulation_year)
                          
                          # 1.Filtering companies with positive results -----------------------------------------------------------------------
                          
                          CIT_BU_sim_year_pos_res_small<-CIT_BU_sim_year_small_df %>%
                            dplyr::select(id_n,nace,bs_total_income,citax_turnover) %>%
                            dplyr::filter(bs_total_income>0)
                          
                          
                          
                          
                          CIT_BU_sim_year_pos_res_small[, decile_group := fifelse(
                            is.na(bs_total_income),
                            NA_integer_,
                            pmin(10L, as.integer(ceiling(10 * data.table::frank(bs_total_income, ties.method = "average")/.N)))
                          )]
                          
                          CIT_BU_sim_year_pos_res_small[, percentile_group := fifelse(
                            is.na(bs_total_income),
                            NA_integer_,
                            pmin(100L, as.integer(ceiling(100 * data.table::frank(bs_total_income, ties.method = "average")/.N)))
                          )]
                          
                          
                          
                          # 2.Percentile --------------------------------------------------------------
                          etr_cit_percentile_small_bu <- CIT_BU_sim_year_pos_res_small %>%
                            dplyr::select(bs_total_income,percentile_group,citax_turnover) %>%
                            dplyr::group_by(percentile_group)%>%
                            dplyr::summarize(ETR = round((sum(citax_turnover, na.rm = TRUE) / sum(bs_total_income, na.rm = TRUE)) * 100, 2))
                          
                          # 3.Decile  -----------------------------------------------------------------
                          etr_cit_decile_small_bu <- CIT_BU_sim_year_pos_res_small %>%
                            dplyr::select(bs_total_income,decile_group,citax_turnover) %>%
                            dplyr::group_by(decile_group)%>%
                            dplyr::summarize(ETR = round((sum(citax_turnover, na.rm = TRUE) / sum(bs_total_income, na.rm = TRUE)) * 100, 2))
                          
                          
                          # 4.NACE ------------------------------------------------------------------
                          
                          etr_cit_nace_small_bu <- CIT_BU_sim_year_pos_res_small %>%
                            dplyr::select(bs_total_income,nace, citax_turnover) %>%
                            dplyr::group_by(nace)%>%
                            dplyr::summarize(ETR= round((sum(citax_turnover, na.rm = TRUE) / sum(bs_total_income, na.rm = TRUE)) * 100, 2))
                          
                          
                          
                          
                          
         # 2.SIM -------------------------------------------------------------------
                          
                          CIT_SIM_sim_year_small_df <- extract_filtered_re_df_fun(CIT_SIM_list_small, forecast_horizon, simulation_year)
                          
                          # 1.Filtering companies with positive results -----------------------------------------------------------------------
                          
                          CIT_SIM_sim_year_pos_res_small<-CIT_SIM_sim_year_small_df %>%
                            dplyr::select(id_n,nace,bs_total_income,citax_turnover) %>%
                            dplyr::filter(bs_total_income>0)
                          
                          
                          
                          CIT_SIM_sim_year_pos_res_small[, decile_group := fifelse(
                            is.na(bs_total_income),
                            NA_integer_,
                            pmin(10L, as.integer(ceiling(10 * data.table::frank(bs_total_income, ties.method = "average")/.N)))
                          )]
                          
                          CIT_SIM_sim_year_pos_res_small[, percentile_group := fifelse(
                            is.na(bs_total_income),
                            NA_integer_,
                            pmin(100L, as.integer(ceiling(100 * data.table::frank(bs_total_income, ties.method = "average")/.N)))
                          )]
                          
                          
                          
                          # 2.Percentile --------------------------------------------------------------
                          etr_cit_percentile_small_sim <- CIT_SIM_sim_year_pos_res_small %>%
                            dplyr::select(bs_total_income,percentile_group, citax_turnover) %>%
                            dplyr::group_by(percentile_group)%>%
                            dplyr::summarize(ETR = round((sum(citax_turnover, na.rm = TRUE) / sum(bs_total_income, na.rm = TRUE)) * 100, 2))
                          
                          # 3.Decile  -----------------------------------------------------------------
                          etr_cit_decile_small_sim <- CIT_SIM_sim_year_pos_res_small %>%
                            dplyr::select(bs_total_income,decile_group, citax_turnover) %>%
                            dplyr::group_by(decile_group)%>%
                            dplyr::summarize(ETR= round((sum(citax_turnover, na.rm = TRUE) / sum(bs_total_income, na.rm = TRUE)) * 100, 2))
                          
                          
                          # 4.NACE ------------------------------------------------------------------
                          
                          etr_cit_nace_small_sim <- CIT_SIM_sim_year_pos_res_small %>%
                            dplyr::select(bs_total_income,nace, citax_turnover) %>%
                            dplyr::group_by(nace)%>%
                            dplyr::summarize(ETR= round((sum(citax_turnover, na.rm = TRUE) / sum(bs_total_income, na.rm = TRUE)) * 100, 2))
                          
                          
                          # 3.Merging data ----------------------------------------------------------
                          
                          CIT_rev_nace_small<- merge(CIT_BU_sim_year_pos_res_small, CIT_SIM_sim_year_pos_res_small[, c("id_n","citax_turnover")], 
                                                     by = c("id_n"), 
                                                     suffixes = c("_bu", "_sim"))%>%
                            dplyr::select(nace,bs_total_income,citax_turnover_bu,citax_turnover_sim)%>%
                            dplyr::left_join(nace_description,by=c("nace"))%>%
                            dplyr::select(description,bs_total_income,citax_turnover_bu,citax_turnover_sim,section)%>%
                            dplyr::group_by(section,description)%>%
                            dplyr::summarise(bs_total_income=sum(bs_total_income),
                                             citax_turnover_bu=sum(citax_turnover_bu),
                                             citax_turnover_sim=sum(citax_turnover_sim)
                            )
                          
                          
                          
                          
                          percentile_small_companies_percentile <- merge(etr_cit_percentile_small_bu, etr_cit_percentile_small_sim[, c("percentile_group",  "ETR")], 
                                                                         by = c("percentile_group"), 
                                                                         suffixes = c("_bu", "_sim"))
                          
                          
                          decile_small_companies_decile <- merge(etr_cit_decile_small_bu, etr_cit_decile_small_sim[, c("decile_group",  "ETR")], 
                                                                 by = c("decile_group"), 
                                                                 suffixes = c("_bu", "_sim"))
                          
                          nace_companies_raw_small <- merge(etr_cit_nace_small_bu, etr_cit_nace_small_sim[, c("nace",  "ETR")], 
                                                      by = c("nace"), 
                                                      suffixes = c("_bu", "_sim"))
                          
                          
                          nace_companies_small <- nace_companies_raw_small %>%
                            left_join(nace_description, by = "nace") %>%
                            select(-nace) %>%
                            group_by(section, description) %>%
                            summarise(
                              n = n(),
                              across(c(ETR_bu, ETR_sim), ~ mean(.x, na.rm = TRUE), .names = "avg_{.col}")
                            ) %>%
                            ungroup()%>%
                            select(-("n"))
                          
                          
                          

