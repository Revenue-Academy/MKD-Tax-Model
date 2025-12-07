" Strucuture of Gross Income and Revenues"

# I.Function for Dashboard ------------------------------------------------------------------

Structure_GrossIncome_Charts <- function(CIT_rev_nace,CIT_rev_nace_small,forecast_horizon) {

# I.Treemap gross income LARGE -------------------------------------------------------------------------

  treemap_gross_income_large_plt <- plot_ly(
                                          type       = "treemap",
                                          labels     = CIT_rev_nace$section,                          # show SECTION on the treemap
                                          parents    = rep("", nrow(CIT_rev_nace)),                   # all root-level
                                          values     = round(CIT_rev_nace$financial_results_r / 1e09, 1),  # billions
                                          customdata = CIT_rev_nace$description,                      # DESCRIPTION only for hover
                                          textinfo   = "label+value+percent entry",
                                          hovertemplate = paste(
                                            "Section: %{label}<br>",
                                            "Description: %{customdata}<br>",
                                            "Gross income: %{value} billion<br>",
                                            "Share: %{percentEntry:.1%}",
                                            "<extra></extra>"
                                          )
                                        ) %>%
                                layout(
                                    title = list(
                                      text = "Structure of Gross income by Type of Income",
                                      font = list(size = 14)
                                    ),
                                    annotations = list(
                                      list(
                                        x = 0.05,
                                        y = -0.01,
                                        text = "Source: WB staff estimation",
                                        showarrow = FALSE,
                                        xref = 'paper',
                                        yref = 'paper',
                                        xanchor = 'center',
                                        yanchor = 'top',
                                        font = list(size = 10)
                                      )
                                    )
                                  )
                                
                                
 
  
  

# II. Treemap gross income SMALL  --------------------------------

               
  treemap_gross_income_small_plt <- plot_ly(
                                type       = "treemap",
                                labels     = CIT_rev_nace_small$section,                          # show SECTION on the treemap
                                parents    = rep("", nrow(CIT_rev_nace_small)),                   # all root-level
                                values     = round(CIT_rev_nace_small$bs_total_income / 1e09, 1),  # billions
                                customdata = CIT_rev_nace_small$description,                      # DESCRIPTION only for hover
                                textinfo   = "label+value+percent entry",
                                hovertemplate = paste(
                                  "Section: %{label}<br>",
                                  "Description: %{customdata}<br>",
                                  "Gross income: %{value} billion<br>",
                                  "Share: %{percentEntry:.1%}",
                                  "<extra></extra>"
                                )
                              ) %>%
                                layout(
                                  title = list(
                                    text = "Structure of Gross income by Type of Income",
                                    font = list(size = 14)
                                  ),
                                  annotations = list(
                                    list(
                                      x = 0.05,
                                      y = -0.01,
                                      text = "Source: WB staff estimation",
                                      showarrow = FALSE,
                                      xref = 'paper',
                                      yref = 'paper',
                                      xanchor = 'center',
                                      yanchor = 'top',
                                      font = list(size = 10)
                                    )
                                  )
                                )
                                
# III. Treemap gross income Revenues -------------------------------------------------------
    
  treemap_revenues_large_plt <- plot_ly(
                                  type       = "treemap",
                                  labels     = CIT_rev_nace$section,                          # show SECTION on the treemap
                                  parents    = rep("", nrow(CIT_rev_nace)),                   # all root-level
                                  values     = round(CIT_rev_nace$citax_sim / 1e06, 1),  # billions
                                  customdata = CIT_rev_nace$description,                      # DESCRIPTION only for hover
                                  textinfo   = "label+value+percent entry",
                                  hovertemplate = paste(
                                    "Section: %{label}<br>",
                                    "Description: %{customdata}<br>",
                                    "Gross income: %{value} million<br>",
                                    "Share: %{percentEntry:.1%}",
                                    "<extra></extra>"
                                  )
                                ) %>%
                                  layout(
                                    title = list(
                                      text = "Structure of CIT revenue by NACE sectors",
                                      font = list(size = 14)
                                    ),
                                    annotations = list(
                                      list(
                                        x = 0.05,
                                        y = -0.01,
                                        text = "Source: WB staff estimation",
                                        showarrow = FALSE,
                                        xref = 'paper',
                                        yref = 'paper',
                                        xanchor = 'center',
                                        yanchor = 'top',
                                        font = list(size = 10)
                                      )
                                    )
                                  )                
  
                  
                    

# IV. Structure of gross income by NACE sections-------------------------------------------

  treemap_revenues_small_plt <- plot_ly(
                                  type       = "treemap",
                                  labels     = CIT_rev_nace_small$section,                          # show SECTION on the treemap
                                  parents    = rep("", nrow(CIT_rev_nace_small)),                   # all root-level
                                  values     = round(CIT_rev_nace_small$citax_turnover_sim / 1e06, 1),  # billions
                                  customdata = CIT_rev_nace_small$description,                      # DESCRIPTION only for hover
                                  textinfo   = "label+value+percent entry",
                                  hovertemplate = paste(
                                    "Section: %{label}<br>",
                                    "Description: %{customdata}<br>",
                                    "Gross income: %{value} million<br>",
                                    "Share: %{percentEntry:.1%}",
                                    "<extra></extra>"
                                  )
                                ) %>%
                                  layout(
                                    title = list(
                                      text = "Structure of CIT revenue by NACE sectors",
                                      font = list(size = 14)
                                    ),
                                    annotations = list(
                                      list(
                                        x = 0.05,
                                        y = -0.01,
                                        text = "Source: WB staff estimation",
                                        showarrow = FALSE,
                                        xref = 'paper',
                                        yref = 'paper',
                                        xanchor = 'center',
                                        yanchor = 'top',
                                        font = list(size = 10)
                                      )
                                    )
                                  )             
# Export Charts -----------------------------------------------------------
                    list(
                      # Charts
                      treemap_gross_income_large_plt=treemap_gross_income_large_plt,
                      treemap_revenues_large_plt=treemap_revenues_large_plt,
                      treemap_gross_income_small_plt=treemap_gross_income_small_plt,
                      treemap_revenues_small_plt=treemap_revenues_small_plt

                    )
}      