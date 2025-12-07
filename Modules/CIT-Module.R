library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(ineq)
library(IC2)
library(data.table)
library(readxl)
library(fontawesome)
library(flexdashboard)
library(tidyverse)
library(plyr)
library(shinycssloaders) 
library(future)
library(promises)
library(plotly)
library(stringr)
library(reshape2)
library(base64enc)
library(parallel)
library(purrr)
library(tidyr)
library(RColorBrewer) 
library(Hmisc)
library(openxlsx)

options(scipen = 999)

# I. UI --------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      uiOutput("headerImage"),
      tags$span(
        "CIT Module",
        style = "flex-grow: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("file-excel")),
      menuItem("Simulation Parameters", icon = icon("list-alt"),
               menuSubItem("Policy Parameters", tabName = "PolicyParameters", icon = icon("edit"))
      ),
      menuItem("Results", icon = icon("magnifying-glass-chart"),
               menuSubItem("CIT regime", tabName = "MainResultsSimulation", icon = icon("gauge")),
               menuSubItem("Turnover regime", tabName = "MainDistributionTables", icon = icon("chart-column")),
               menuSubItem("Tax Contribution", tabName = "MainResultBins", icon = icon("chart-pie"))
      ),
      menuItem("Visualizations", tabName = "CustomsDuties-charts", icon = icon("chart-simple"),
               menuSubItem("Dashboards", tabName = "CIT_Revenues", icon = icon("chart-column"))
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      # INPUT TAB -----------------------------------------------------------
      tabItem(tabName = "input",
              fluidRow(
                column(6,
                       h4("Data Input"),
                       selectInput("inputType", "Data Source",
                                   choices = c("Manual", "Excel File"),
                                   selected = "Excel File"),
                       conditionalPanel(
                         condition = "input.inputType == 'Excel File'",
                         fileInput("fileInput", "Upload Excel File", accept = c(".xlsx")),
                         checkboxInput("hasHeader", "Header", TRUE)
                       ),
                       actionButton("importExcel", "Import Excel Data")
                )
              )
      ),
      
      # POLICY PARAMETERS TAB -----------------------------------------------
      tabItem(tabName = "PolicyParameters",
              fluidRow(
                column(3,
                       # DROPDOWN FOR YEAR (from Excel)
                       selectInput("SimulationYear", "Setting Simulation Year",
                                   choices = c(), selected = NULL),
                       
                       uiOutput("PolicyParameter"),
                       uiOutput("Descriptions_Select"),
                       actionButton("addValuesValue", "Add to Table", style = "float: left; margin-right: 5px;"),
                       actionButton("removeLastRow", "Remove Last Row", style = "float: left; margin-right: 5px;"),
                       actionButton("clearValuesTable", "Clear Table", style = "float: left;")
                ),
                column(3,
                       numericInput("default_Value", "Value", value = 0,
                                    min = 0, step = 0.01)
                )
              ),
              
              div(h4("Selected Simulations Parameters"), style = "text-align: center;"),
              fluidRow(
                column(12,
                       DTOutput("cit_simulation_parameters_updated"),
                       actionButton("calc_Customs_Sim_Button", "Run Simulation", style = "float: right;"),
                       actionButton("savecit_simulation_parameters_updated", "Save Data", style = "float: right;")
                )
              )
      ),
      
      # RESULTS TABS --------------------------------------------------------
      tabItem(
        tabName = "MainResultsSimulation",
        fluidRow(column(12, DTOutput("CIT_SUMMARY_TABLES")))
      ),
      tabItem(
        tabName = "MainDistributionTables",
        fluidRow(column(12, DTOutput("CIT_SUMMARY_TABLES_SMALL")))
      ),
      tabItem(
        tabName = "MainResultBins",
        fluidRow(column(12, DTOutput("BIN_TABLES")))
      ),
      
      # CHARTS TAB ----------------------------------------------------------
      tabItem(
        tabName = "CIT_Revenues",
        fluidRow(
          column(6,
                 selectInput("chartSelectCIT_Revenues", "Select Chart",
                             choices = c(
                               "Structure_Charts",
                               "Revenue_Charts",
                               "Distribution_Charts"
                             ),
                             selected = "Structure_Charts")
          )
        ),
        fluidRow(
          infoBoxOutput("infoBox1", width = 6),
          infoBoxOutput("infoBox2", width = 6)
        ),
        fluidRow(
          column(12, uiOutput("additionalCharts"))
        )
      )
    )
  )
)

# II. SERVER ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Header image ------------------------------------------------------------
  output$headerImage <- renderUI({
    img_data <- base64enc::dataURI(file = "img/WB_pic.png", mime = "image/png")
    tags$img(src = img_data, height = "40px",
             style = "float:left; margin-right:20px;")
  })
  
  
  # Excel data --------------------------------------------------------------
  excelData <- reactiveVal(NULL)
  
  observeEvent(input$importExcel, {
    req(input$fileInput)
    inFile <- input$fileInput
    if (!is.null(inFile)) {
      data <- read_excel(inFile$datapath, col_names = input$hasHeader)
      
      required_cols <- c("PolicyParameter", "Descriptions", "Variables",
                         "AdditionalInfo", "Year", "Parameters", "Value")
      if (!all(required_cols %in% colnames(data))) {
        showModal(modalDialog(
          title = "Error",
          paste0(
            "The Excel file must contain the columns: ",
            paste(paste0("'", required_cols, "'"), collapse = ", ")
          ),
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      data <- data %>%
        mutate(
          Value = as.numeric(gsub("[^0-9.]", "", Value)),
          Year  = as.numeric(gsub("[^0-9.]", "", Year))
        )
      excelData(data)
      assign("cit_simulation_parameters_raw", excelData(), envir = .GlobalEnv)
      cat("Excel data imported successfully\n")
    }
  })
  
  # when excelData is available, fill the SimulationYear dropdown ----------
  observeEvent(excelData(), {
    df <- excelData()
    if (!is.null(df)) {
      yrs <- sort(unique(df$Year))
      yrs <- yrs[!is.na(yrs)]
      if (length(yrs) > 0) {
        updateSelectInput(session, "SimulationYear",
                          choices  = yrs,
                          selected = max(yrs))
      }
    }
  })
  
  # Reactive parameters table -----------------------------------------------
  cit_simulation_parameters_updated <- reactiveVal(data.table(
    PolicyParameter = character(),
    Descriptions    = character(),
    Variables       = character(),
    Value           = numeric(),
    Year            = numeric()
  ))
  
  # Selection UIs -----------------------------------------------------------
  output$PolicyParameter <- renderUI({
    if (!is.null(excelData())) {
      selectInput("PolicyParameter", "Policy Parameter Selection",
                  choices = unique(excelData()$PolicyParameter))
    } else {
      selectInput("PolicyParameter", "Policy Parameter Selection", choices = NULL)
    }
  })
  
  output$Descriptions_Select <- renderUI({
    req(input$PolicyParameter)
    PolicyParameter <- input$PolicyParameter
    if (!is.null(PolicyParameter) && !is.null(excelData())) {
      selectInput("Descriptions_Select", "Description of parameter",
                  choices = unique(
                    excelData()[excelData()$PolicyParameter == PolicyParameter, ]$Descriptions
                  ))
    } else {
      selectInput("Descriptions_Select", "Description of parameter", choices = NULL)
    }
  })
  
  # Helper: row selected by PolicyParameter + Description + Year -----------
  selected_row <- reactive({
    req(excelData(), input$PolicyParameter, input$Descriptions_Select, input$SimulationYear)
    df <- excelData() %>%
      dplyr::filter(
        PolicyParameter == input$PolicyParameter,
        Descriptions    == input$Descriptions_Select,
        Year            == as.numeric(input$SimulationYear)
      )
    if (nrow(df) == 0) return(NULL)
    df[1, ]   # first match for that year
  })
  
  # When Description or Year changes, pre-fill Value from Excel ------------
  observeEvent(list(input$Descriptions_Select, input$SimulationYear), {
    row <- selected_row()
    if (!is.null(row)) {
      updateNumericInput(session, "default_Value", value = row$Value)
      cat("default_Value updated from selected row\n")
    }
  })
  
  # Add / clear / remove rows in parameters table ---------------------------
  observeEvent(input$addValuesValue, {
    row <- selected_row()
    req(row)
    
    newEntry <- data.table(
      PolicyParameter = row$PolicyParameter,
      Descriptions    = row$Descriptions,
      Variables       = row$Variables,
      Value           = input$default_Value,
      Year            = as.numeric(input$SimulationYear)   # <--- USE SELECTED YEAR
    )
    
    # ---- DUPLICATE CHECK: same param + same year only ---------------------
    current_tbl <- cit_simulation_parameters_updated()
    dup <- current_tbl[
      PolicyParameter == newEntry$PolicyParameter &
        Descriptions    == newEntry$Descriptions &
        Year            == newEntry$Year
    ]
    
    if (nrow(dup) > 0) {
      if (any(abs(dup$Value - newEntry$Value) > .Machine$double.eps^0.5)) {
        showModal(modalDialog(
          title = "Duplicate parameter for the same year",
          paste0(
            "A row for parameter '", newEntry$Descriptions,
            "' in year ", newEntry$Year,
            " already exists with value ", dup$Value[1], 
            ".\n\nPlease remove the existing row or keep the same value."
          ),
          easyClose = TRUE,
          footer = NULL
        ))
        cat("Duplicate (different value) detected; new entry not added.\n")
        return()
      } else {
        showModal(modalDialog(
          title = "Parameter already in table",
          paste0(
            "The parameter '", newEntry$Descriptions,
            "' for year ", newEntry$Year,
            " with the same value is already in the table.\n",
            "The row was not added again."
          ),
          easyClose = TRUE,
          footer = NULL
        ))
        cat("Exact duplicate detected; new entry not added.\n")
        return()
      }
    }
    
    cit_simulation_parameters_updated(
      rbind(current_tbl, newEntry)
    )
    cat("New entry added to cit_simulation_parameters_updated:\n")
    print(newEntry)
    
    # RESET VALUE BOX TO BASELINE (Excel) FOR THAT YEAR --------------------
    updateNumericInput(session, "default_Value", value = row$Value)
    cat("default_Value reset to baseline after adding row\n")
  })
  
  # NEW: remove last row button ---------------------------------------------
  observeEvent(input$removeLastRow, {
    tbl <- cit_simulation_parameters_updated()
    if (nrow(tbl) == 0) {
      showModal(modalDialog(
        title = "No rows to remove",
        "The table is currently empty.",
        easyClose = TRUE,
        footer = NULL
      ))
      cat("removeLastRow clicked, but table is empty.\n")
      return()
    }
    
    tbl <- tbl[-nrow(tbl)]
    cit_simulation_parameters_updated(tbl)
    cat("Last row removed from cit_simulation_parameters_updated.\n")
  })
  
  observeEvent(input$clearValuesTable, {
    cit_simulation_parameters_updated(data.table(
      PolicyParameter = character(),
      Descriptions    = character(),
      Variables       = character(),
      Value           = numeric(),
      Year            = numeric()
    ))
    cat("cit_simulation_parameters_updated table cleared\n")
  })
  
  # Save parameters back into raw table -------------------------------------
  observeEvent(input$savecit_simulation_parameters_updated, {
    assign("ValueTableUpdate", cit_simulation_parameters_updated(), envir = .GlobalEnv)
    cat("CIT simulation parameters saved to GlobalEnv as ValueTableUpdate\n")
    
    cit_simulation_parameters_updated_copy <- get("cit_simulation_parameters_raw", envir = .GlobalEnv)
    
    citRateData <- get("ValueTableUpdate", envir = .GlobalEnv)
    if (nrow(citRateData) > 0) {
      for (i in 1:nrow(citRateData)) {
        row_i <- citRateData[i, ]
        # match ALSO on Year, and ONLY update Value
        cit_simulation_parameters_updated_copy[
          cit_simulation_parameters_updated_copy$PolicyParameter == row_i$PolicyParameter &
            cit_simulation_parameters_updated_copy$Descriptions    == row_i$Descriptions &
            cit_simulation_parameters_updated_copy$Variables       == row_i$Variables &
            cit_simulation_parameters_updated_copy$Year            == row_i$Year,
          "Value"
        ] <- row_i$Value
      }
    }
    
    assign("cit_simulation_parameters_updated", cit_simulation_parameters_updated_copy, envir = .GlobalEnv)
    cat("cit_simulation_parameters_updated assigned to GlobalEnv\n")
  })
  
  output$cit_simulation_parameters_updated <- renderDT({
    datatable(cit_simulation_parameters_updated(),
              options = list(dom = 't', paging = FALSE),
              editable = TRUE)
  })
  
  # Simulation results container --------------------------------------------
  reactive_simulation_results <- reactiveVal()
  
  # Run simulation ----------------------------------------------------------
  observeEvent(input$calc_Customs_Sim_Button, {
    if (nrow(cit_simulation_parameters_updated()) == 0 && is.null(excelData())) {
      showModal(modalDialog(
        title = "Error",
        "No parameters have been added to the table or imported from the Excel file. Please select parameters, add them to the table or import from Excel before running the simulation.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    showModal(modalDialog(
      title = "Running Simulation...",
      "Please wait while the simulation is running...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    future({
      source(paste0(path1, "/Scripts/CIT/TaxCalculator_Large.R"))
      source(paste0(path1, "/Scripts/CIT/TaxCalculator_Small.R"))
      source(paste0(path1, "/Scripts/CIT/Calc_aggregate_data.R"))
      #source(paste0(path1, "/Scripts/CIT/Calc-Distribution-Effects.R"))
      # source(paste0(path1, "/Scripts/CIT/Calc-Redistribution-Effects.R"))
      
      list(
        cit_summary_df                 = get("cit_summary_df", envir = .GlobalEnv),
        cit_summary_df_small                 = get("cit_summary_df_small", envir = .GlobalEnv)
        
        #re_effects_final               = get("re_effects_final", envir = .GlobalEnv),
        #cit_decile_distribution_bu_sim = get("cit_decile_distribution_bu_sim", envir = .GlobalEnv),
        #cit_result_bins_sim_sub        = get("cit_result_bins_sim_sub", envir = .GlobalEnv)
      )
    }) %...>% (function(results) {
      removeModal()
      showModal(modalDialog(
        title = "Success",
        "Simulation is done!",
        easyClose = TRUE,
        footer = NULL
      ))
      
      reactive_simulation_results(results)
      updateCharts()
    }) %...!% (function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Error during calculation:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  # RESULTS TABLES ----------------------------------------------------------
  output$CIT_SUMMARY_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$cit_summary_df,
      caption = tags$caption(
        paste("CIT Projections,", min(forecast_horizon), "-", max(forecast_horizon)),
        class = "table-caption-bold"
      ),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'CIT_Projections',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'CIT_Projections',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          )
        ),
        autoWidth  = TRUE,
        escape     = FALSE,
        lengthMenu = list(c(10,25,50,-1), c(10,25,50,"All"))
      ),
      rownames = FALSE
    )
  })
  
  output$CIT_SUMMARY_TABLES_SMALL <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$cit_summary_df_small,
      caption = tags$caption(
        paste("Distribution Tables LCU,", SimulationYear),
        class = "table-caption-bold"
      ),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'DistTable',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'DistTable',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          )
        ),
        autoWidth  = TRUE,
        escape     = FALSE,
        lengthMenu = list(c(10,25,50,-1), c(10,25,50,"All"))
      ),
      rownames = FALSE
    )
  })
  
  output$BIN_TABLES <- renderDT({
    req(reactive_simulation_results())
    datatable(
      reactive_simulation_results()$cit_result_bins_sim_sub,
      caption = tags$caption(
        paste("Structure of CIT liability by income groups, ", simulation_year),
        class = "table-caption-bold"
      ),
      extensions = 'Buttons',
      options = list(
        pageLength = 15,
        dom = 'Blfrtip',
        buttons = list(
          list(
            extend = 'copyHtml5',
            text = 'Copy',
            filename = 'BinTables',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          ),
          list(
            extend = 'csvHtml5',
            text = 'CSV',
            filename = 'BinTables',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          ),
          list(
            extend = 'print',
            text = 'Print',
            exportOptions = list(
              format = list(
                body = JS("function(data, row, column, node) {
                           return $('<div>').html(data).text();
                         }")
              )
            )
          )
        ),
        autoWidth  = TRUE,
        escape     = FALSE,
        lengthMenu = list(c(10,25,50,-1), c(10,25,50,"All"))
      ),
      rownames = FALSE
    )
  })
  
  # CHARTS ------------------------------------------------------------------
  updateCharts <- function() {
    cat("Updating charts after simulation\n")
    chart_type <- isolate(input$chartSelectCIT_Revenues)
    cat("Selected chart type:", chart_type, "\n")
    
    if (is.null(reactive_simulation_results())) {
      cat("reactive_simulation_results is NULL; charts will be updated after running the simulation.\n")
      return(invisible(NULL))
    }
    
    if (exists("merged_CIT_BU_SIM", envir = .GlobalEnv) && exists("forecast_horizon", envir = .GlobalEnv)) {
      merged_CIT_BU_SIM <- get("merged_CIT_BU_SIM", envir = .GlobalEnv)
      forecast_horizon  <- get("forecast_horizon",  envir = .GlobalEnv)
      
      if (chart_type == "Revenue_Charts") {
        source(paste0(path1, "/Scripts/CIT/Charts-CIT_Revenues.R"))
        charts <- Revenue_Charts(merged_CIT_BU_SIM,merged_CIT_BU_SIM_small, range(forecast_horizon))
        
     
        output$infoBox1 <- renderInfoBox({
          infoBox(
            title = " ",          # empty title
            value = " ",          # empty value
            icon  = icon("coins"),
            color = "orange"
          )
        })
        
        output$infoBox2 <- renderInfoBox({
          infoBox(
            title = " ",          # empty title
            value = " ",          # empty value
            icon  = icon("chart-line"),
            color = "light-blue"
          )
        })
        
        
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("CIT_RevenuesTotal_large_plt",  height = "400px")),
              column(6, plotlyOutput("CIT_RevenuesTotal_small_plt",  height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("CIT_RevenuesTotal_large_plt",   height = "400px")),
              column(6, plotlyOutput("CIT_RevenuesTotal_small_plt", height = "400px"))
            )
          )
        })
        
        output$CIT_RevenuesTotal_large_plt   <- renderPlotly({ charts$CIT_RevenuesTotal_large_plt })
        output$CIT_RevenuesTotal_small_plt   <- renderPlotly({ charts$CIT_RevenuesTotal_small_plt })
        output$CIT_RevenuesTotal_large_plt   <- renderPlotly({ charts$CIT_RevenuesTotal_large_plt })
        output$CIT_RevenuesTotal_small_plt <- renderPlotly({ charts$CIT_RevenuesTotal_small_plt })
        
      } else if (chart_type == "Structure_Charts") {
        source(paste0(path1, "/Scripts/CIT/Charts-StructureGrossIncome.R"))
        Charts_structure <- Structure_GrossIncome_Charts(
                                                          CIT_rev_nace,CIT_rev_nace_small
                                                      )
        
     
        output$infoBox1 <- renderInfoBox({
          infoBox(
            title = " ",          # empty title
            value = " ",          # empty value
            icon  = icon("chart-area"),
            color = "orange"
          )
        })
        
        output$infoBox2 <- renderInfoBox({
          infoBox(
            title = " ",          # empty title
            value = " ",          # empty value
            icon  = icon("industry"),
            color = "light-blue"
          )
        })
        
        
        
        
        output$chartOutputCIT <- renderPlotly({ Charts_structure$treemap_gross_income_large_plt })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("treemap_gross_income_large_plt",      height = "400px")),
              column(6, plotlyOutput("treemap_gross_income_small_plt", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("treemap_revenues_large_plt",    height = "400px")),
              column(6, plotlyOutput("treemap_revenues_small_plt",  height = "400px"))
            )
          )
        })
        
        output$treemap_gross_income_large_plt      <- renderPlotly({ Charts_structure$treemap_gross_income_large_plt })
        output$treemap_gross_income_small_plt <- renderPlotly({ Charts_structure$treemap_gross_income_small_plt })
        output$treemap_revenues_large_plt    <- renderPlotly({ Charts_structure$treemap_revenues_large_plt })
        output$treemap_revenues_small_plt  <- renderPlotly({ Charts_structure$treemap_revenues_small_plt })
        
      } else if (chart_type == "Distribution_Charts") {
        source(paste0(path1, "/Scripts/CIT/Charts-ETR.R"))
        charts_dist <- ETR_charts_fun(
                                        nace_large_companies,nace_companies_small, forecast_horizon
                                                                            )

        
        output$infoBox1 <- renderInfoBox({
          infoBox(
            title = " ",          # empty title
            value = " ",          # empty value
            icon  = icon("percent"),
            color = "orange"
          )
        })
        
        output$infoBox2 <- renderInfoBox({
          infoBox(
            title = " ",          # empty title
            value = " ",          # empty value
            icon  = icon("percent"),
            color = "light-blue"
          )
        })
        
        
        output$chartOutputCIT <- renderPlotly({ charts_dist$etr_nace_large_plot })
        
        output$additionalCharts <- renderUI({
          tagList(
            fluidRow(
              column(6, plotlyOutput("etr_nace_large_plot",      height = "400px")),
              column(6, plotlyOutput("etr_nace_small_plot", height = "400px"))
            ),
            fluidRow(
              column(6, plotlyOutput("dist_centile_groups_large_plot",    height = "400px")),
              column(6, plotlyOutput("dist_centile_groups_small_plot",  height = "400px"))
            )
          )
        })
        
        output$etr_nace_large_plot      <- renderPlotly({ charts_dist$etr_nace_large_plot })
        output$etr_nace_small_plot <- renderPlotly({ charts_dist$etr_nace_small_plot })
        output$dist_centile_groups_large_plot    <- renderPlotly({ charts_dist$dist_centile_groups_large_plot })
        output$dist_centile_groups_small_plot  <- renderPlotly({ charts_dist$dist_centile_groups_small_plot })
        
      } 
      
    } else {
      cat("Error: merged_CIT_BU_SIM or forecast_horizon not found in the global environment\n")
    }
  }
  
  observeEvent(input$chartSelectCIT_Revenues, {
    updateCharts()
  })
}

shinyApp(ui = ui, server = server)
