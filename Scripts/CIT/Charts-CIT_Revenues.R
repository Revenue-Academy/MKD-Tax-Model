

Revenue_Charts <- function(merged_PIT_BU_SIM,merged_CIT_BU_SIM_small, forecast_horizon) {
  
  # Chart 1. Comparison of PIT Revenues-LARGE COMPANIES -----------------------------------------------------------------
  CIT_RevenuesTotal_large_plt <- plot_ly(
                                    merged_CIT_BU_SIM,
                                    x = ~year,
                                    y = ~citax_bu*1e06,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                    ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~citax_sim*1e06,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                      ) %>%
                                layout(
                                title = paste("Total CIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
                                              xaxis = list(title = '', tickformat = 'd'),
                                              yaxis = list(title = ' ', rangemode = 'tozero'),
                                              annotations = list(
                                                x = -0.02,
                                                y = -0.1,
                                                text = "Source: WB staff estimation",
                                                showarrow = FALSE,
                                                xref = 'paper',
                                                yref = 'paper',
                                                align = 'left'
                                )
                              )
  
 
  
  
  # Chart 2.  Comparison of PIT Revenues-SMALL COMPANIES   ------------------------- 

  CIT_RevenuesTotal_small_plt <- plot_ly(
                                      merged_CIT_BU_SIM_small,
                                        x = ~year,
                                        y = ~citax_turnover_bu *1e06,
                                        name = "Baseline",
                                        type = 'scatter',
                                        mode = 'lines',
                                        line = list(width = 4, dash = "solid")
                                      ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~citax_turnover_sim*1e06,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                        ) %>%
                                        layout(
                                          title = paste("Total CIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
                                          xaxis = list(title = '', tickformat = 'd'),
                                          yaxis = list(title = ' ', rangemode = 'tozero'),
                                          annotations = list(
                                            x = -0.02,
                                            y = -0.1,
                                            text = "Source: WB staff estimation",
                                            showarrow = FALSE,
                                            xref = 'paper',
                                            yref = 'paper',
                                            align = 'left'
                                          )
                                        )
  # Chart 3. Comparison of CIT Revenues from Labor  ------------------------- 
  
 
  CIT_RevenuesTotal_large_plt <- plot_ly(
                                      merged_CIT_BU_SIM,
                                      x = ~year,
                                      y = ~citax_bu*1e06,
                                      name = "Baseline",
                                      type = 'scatter',
                                      mode = 'lines',
                                      line = list(width = 4, dash = "solid")
                                    ) %>%
                                      add_trace(
                                        x = ~year,
                                        y = ~citax_sim*1e06,
                                        name = 'Simulation',
                                        line = list(width = 4, dash = "dot")
                                      ) %>%
                                      layout(
                                        title = paste("Total CIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
                                        xaxis = list(title = '', tickformat = 'd'),
                                        yaxis = list(title = ' ', rangemode = 'tozero'),
                                        annotations = list(
                                          x = -0.02,
                                          y = -0.1,
                                          text = "Source: WB staff estimation",
                                          showarrow = FALSE,
                                          xref = 'paper',
                                          yref = 'paper',
                                          align = 'left'
                                        )
                                      )

  

  
  # Chart 4. Comparison of CIT Revenues from Wages  ------------------------- 
  
  CIT_RevenuesTotal_small_plt <- plot_ly(
    merged_CIT_BU_SIM,
    x = ~year,
    y = ~citax_bu*1e06,
    name = "Baseline",
    type = 'scatter',
    mode = 'lines',
    line = list(width = 4, dash = "solid")
  ) %>%
    add_trace(
      x = ~year,
      y = ~citax_sim*1e06,
      name = 'Simulation',
      line = list(width = 4, dash = "dot")
    ) %>%
    layout(
      title = paste("Total CIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
      xaxis = list(title = '', tickformat = 'd'),
      yaxis = list(title = ' ', rangemode = 'tozero'),
      annotations = list(
        x = -0.02,
        y = -0.1,
        text = "Source: WB staff estimation",
        showarrow = FALSE,
        xref = 'paper',
        yref = 'paper',
        align = 'left'
      )
    )
  
  
 

  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    CIT_RevenuesTotal_large_plt = CIT_RevenuesTotal_large_plt,
    CIT_RevenuesTotal_small_plt=CIT_RevenuesTotal_small_plt,
    CIT_RevenuesTotal_large_plt = CIT_RevenuesTotal_large_plt,
    CIT_RevenuesTotal_small_plt=CIT_RevenuesTotal_small_plt,
    
    
    # Tables
    merged_CIT_BU_SIM = merged_CIT_BU_SIM
  )
}
