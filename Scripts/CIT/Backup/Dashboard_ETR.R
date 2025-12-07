" Strucuture of Gross Income "

"DA SE STAVAT IMINJATA ZA ZA MIKRO VO FUNKCIJATA. SEGA NE SE STAVENI BIDEJKI FUNKCIJATA NE E KREIRANA "


# I.Function for Dashboard ------------------------------------------------------------------

ETR_charts_fun <- function(nace_large_companies,te_labor_capital,nace_pit_summary_te,decile_pit_summary, forecast_horizon) {

# I.Chart ETR LARGE COMPANIES BY NACE SECTIONS -------------------------------------------------------------------------

  # Isto vakvo da se napravi i za mikro-kompani
  # Define custom colors
  custom_colors <- c('#1f77b4', '#ff7f0e')
  
  
  etr_nace_large_plot <- plot_ly(
                              nace_large_companies,
                              x = ~section, 
                              y = ~avg_ETR_bu, 
                              name = 'Baseline',
                              type = 'bar',
                              marker = list(color = custom_colors[1]),
                              text = ~paste0(
                                "section: ", section, "<br>",
                                "description: ", description, "<br>",
                                "ETR-Business as usual: ", round(avg_ETR_bu, 1)
                              ),
                              hoverinfo = "text",        # show only the custom text on hover
                              textposition = "none"      # DO NOT print text on the bars
                            ) %>%
                              add_trace(
                                y = ~avg_ETR_sim, 
                                name = 'Simulation', 
                                marker = list(color = custom_colors[2]),
                                text = ~paste0(
                                  "section: ", section, "<br>",
                                  "description: ", description, "<br>",
                                  "ETR-Simulation: ", round(avg_ETR_sim, 1)
                                ),
                                hoverinfo = "text",      # only text on hover
                                textposition = "none"    # no labels on bars
                              ) %>%
                              layout(
                                title = paste("Effective Tax Rates,", simulation_year),
                                xaxis = list(title = "Sectors", tickmode = 'linear'), 
                                yaxis = list(title = "Pct (%)"),
                                annotations = list(
                                  list(
                                    x = -0.02,
                                    y = -0.1,
                                    text = "Source: WB staff estimation",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    align = 'left'
                                  )
                                )
                              )

# II. Chart ETR small COMPANIES BY NACE SECTIONS --------------------------------

  etr_nace_small_plot <- plot_ly(
                              nace_large_companies,
                              x = ~section, 
                              y = ~avg_ETR_bu, 
                              name = 'Baseline',
                              type = 'bar',
                              marker = list(color = custom_colors[1]),
                              text = ~paste0(
                                "section: ", section, "<br>",
                                "description: ", description, "<br>",
                                "ETR-Business as usual: ", round(avg_ETR_bu, 1)
                              ),
                              hoverinfo = "text",        # show only the custom text on hover
                              textposition = "none"      # DO NOT print text on the bars
                            ) %>%
                              add_trace(
                                y = ~avg_ETR_sim, 
                                name = 'Simulation', 
                                marker = list(color = custom_colors[2]),
                                text = ~paste0(
                                  "section: ", section, "<br>",
                                  "description: ", description, "<br>",
                                  "ETR-Simulation: ", round(avg_ETR_sim, 1)
                                ),
                                hoverinfo = "text",      # only text on hover
                                textposition = "none"    # no labels on bars
                              ) %>%
                              layout(
                                title = paste("Effective Tax Rates,", simulation_year),
                                xaxis = list(title = "Sectors", tickmode = 'linear'), 
                                yaxis = list(title = "Pct (%)"),
                                annotations = list(
                                  list(
                                    x = -0.02,
                                    y = -0.1,
                                    text = "Source: WB staff estimation",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    align = 'left'
                                  )
                                )
                              )
  

# III. Chart ETR large -------------------------------------------------------
    
                    
  dist_centile_groups_large_plot <- plot_ly(percentile_large_companies_percentile, x = ~percentile_group, y = ~ETR_bu, name = "Baseline", type = 'scatter', mode = 'lines',
                                     line = list(width = 4,dash = "solid"))
  dist_centile_groups_large_plot <- dist_centile_groups_large_plot %>% add_trace(y = ~ETR_sim, name = "Simulation", line = list(width = 4,dash = "dash"))%>%
                              layout(
                                title = paste("Effective Tax Rate by Percentile Groups,", simulation_year),
                                xaxis = list(title = 'Percentile'),
                                yaxis = list(title = ' '),
                                annotations = list(
                                  list(
                                    x = -0.02,
                                    y = -0.1,
                                    text = "Source: WB staff estimation",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    align = 'left'
                                  )
                                )
                              )
  
                    
                    

# IV. Chart ETR small -------------------------------------------

  dist_centile_groups_small_plot <- plot_ly(percentile_large_companies_percentile, x = ~percentile_group, y = ~ETR_bu, name = "Baseline", type = 'scatter', mode = 'lines',
                                            line = list(width = 4,dash = "solid"))
  dist_centile_groups_small_plot <- dist_centile_groups_small_plot %>% add_trace(y = ~ETR_sim, name = "Simulation", line = list(width = 4,dash = "dash"))%>%
    layout(
      title = paste("Effective Tax Rate by Percentile Groups,", simulation_year),
      xaxis = list(title = 'Percentile'),
      yaxis = list(title = ' '),
      annotations = list(
        list(
          x = -0.02,
          y = -0.1,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          align = 'left'
        )
      )
    )
  

                    
# Export Charts -----------------------------------------------------------
                    list(
                      # Charts
                      etr_nace_large_plot=etr_nace_large_plot,
                      etr_nace_small_plot=etr_nace_small_plot,
                      dist_centile_groups_large_plot=dist_centile_groups_large_plot,
                      dist_centile_groups_small_plot=dist_centile_groups_small_plot

                    )
}      