library(plotly)

'ovde za pit_diplomatic_consular_bu da se koregira'


Revenue_Charts <- function(merged_PIT_BU_SIM, forecast_horizon) {
  
  # Chart 1. Comparison of PIT Revenues -----------------------------------------------------------------
  PIT_RevenuesTotal_plt <- plot_ly(
                                    merged_PIT_BU_SIM,
                                    x = ~year,
                                    y = ~pitax_bu,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                    ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~pitax_sim,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                      ) %>%
                              layout(
                                title = paste("Total PIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
 
  
  
  # Chart 2. Comparison of PIT Revenues from Capital  ------------------------- 
  PIT_RevenuesCapital_plt <- plot_ly(
                                    merged_PIT_BU_SIM,
                                    x = ~year,
                                    y = ~pit_c_bu,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                  ) %>%
                                    add_trace(
                                      x = ~year,
                                      y = ~pit_c_sim,
                                      name = 'Simulation',
                                      line = list(width = 4, dash = "dot")
                                    ) %>%
                                    layout(
                                      title = paste("PIT Revenues Capital,", min(forecast_horizon), "-", max(forecast_horizon)),
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

  # Chart 3. Comparison of PIT Revenues from Labor  ------------------------- 
  
  PIT_RevenuesLabor_plt <- plot_ly(
    merged_PIT_BU_SIM,
    x = ~year,
    #y = ~(pit_w_bu+pit_diplomatic_consular_bu+pit_other_income_l_bu),
    y = ~(pit_w_bu),
    name = "Baseline",
    type = 'scatter',
    mode = 'lines',
    line = list(width = 4, dash = "solid")
  ) %>%
    add_trace(
      x = ~year,
      #y = ~(pit_w_sim+pit_diplomatic_consular_sim+pit_other_income_l_sim),
      y = ~(pit_w_sim),
      name = 'Simulation',
      line = list(width = 4, dash = "dot")
    ) %>%
    layout(
      title = paste("PIT Revenues Labor,", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
  
  

  
  # Chart 4. Comparison of PIT Revenues from Wages  ------------------------- 
  
  PIT_RevenuesWages_plt <- plot_ly(
    merged_PIT_BU_SIM,
    x = ~year,
    y = ~pit_w_bu,
    name = "Baseline",
    type = 'scatter',
    mode = 'lines',
    line = list(width = 4, dash = "solid")
  ) %>%
    add_trace(
      x = ~year,
      y = ~pit_w_sim,
      name = 'Simulation',
      line = list(width = 4, dash = "dot")
    ) %>%
    layout(
      title = paste("PIT Revenues Wages,", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
 
  
  
  # Chart 5. Comparison of SSC Revenues -----------------------------------------------------------------
  SSC_RevenuesTotal_plt <- plot_ly(
                                    merged_PIT_BU_SIM,
                                    x = ~year,
                                    y = ~calc_ssc_bu,
                                    name = "Baseline",
                                    type = 'scatter',
                                    #mode = 'lines',
                                    mode = 'lines+markers+text',
                                    line = list(width = 4, dash = "solid")
                                  ) %>%
                                    add_trace(
                                      x = ~year,
                                      y = ~calc_ssc_sim,
                                      name = 'Simulation',
                                      line = list(width = 4, dash = "dot")
                                    ) %>%
                                    layout(
                                      title = paste("SSC Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
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
    PIT_RevenuesTotal_plt = PIT_RevenuesTotal_plt,
    PIT_RevenuesCapital_plt=PIT_RevenuesCapital_plt,
    PIT_RevenuesLabor_plt=PIT_RevenuesLabor_plt,
    PIT_RevenuesWages_plt=PIT_RevenuesWages_plt,
    SSC_RevenuesTotal_plt = SSC_RevenuesTotal_plt,
    
    # Tables
    merged_PIT_BU_SIM = merged_PIT_BU_SIM
  )
}
