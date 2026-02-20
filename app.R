# ---- Packages ----
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tibble)

# ---- Precomputed data (fast startup) ----
gdp_path <- file.path("data", "gdp_dash.rds")
cbp_path <- file.path("data", "cbp_dash.rds")
if (!file.exists(gdp_path) || !file.exists(cbp_path)) {
  stop("Precomputed data files not found. Please run locally: saveRDS(gdp_dash, 'data/gdp_dash.rds'); saveRDS(cbp_dash, 'data/cbp_dash.rds')")
}
gdp_dash <- readRDS(gdp_path)
cbp_dash <- readRDS(cbp_path)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Florida County Economic Dashboard"),
  tabsetPanel(
    tabPanel(
      "GDP",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "gdp_sector", "Select Sector:",
            choices = sort(unique(gdp_dash$industry_title)),
            selected = "Agriculture, forestry, fishing and hunting"
          ),
          selectizeInput(
            "gdp_county", "Select up to 5 counties:",
            choices = sort(unique(gdp_dash$county)),
            multiple = TRUE,
            options = list(maxItems = 5, placeholder = "Choose counties…")
          ),
          actionButton("clear_gdp", "Clear selections"),
          helpText("Note: Real GDP values are in chained 2017 dollars (BEA CAGDP9). Displayed in millions.")
        ),
        mainPanel(
          plotOutput("gdpPlot", height = "520px"),
          tableOutput("gdpTable"),
          downloadButton("download_gdp", "Download GDP data (CSV)"),
          helpText("Source: U.S. Bureau of Economic Analysis (BEA) Regional API, Table CAGDP9 (Real GDP, chained 2017 dollars). ",
                   "Years with suppressed values are omitted (zeros treated as suppressed).")
        )
      )
    ),
    tabPanel(
      "CBP (Establishments, Employment, Payroll)",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "cbp_industry", "Select Industry:",
            choices = sort(unique(cbp_dash$industry_title)),
            selected = "Agriculture, forestry, fishing and hunting"
          ),
          selectizeInput(
            "cbp_county", "Select up to 5 counties:",
            choices = sort(unique(cbp_dash$county)),
            multiple = TRUE,
            options = list(maxItems = 5, placeholder = "Choose counties…")
          ),
          selectInput(
            "cbp_indicator", "Indicator:",
            choices = c("Establishments" = "ESTAB",
                        "Employees" = "EMP",
                        "Annual payroll ($)" = "PAYANN"),
            selected = "EMP"
          ),
          actionButton("clear_cbp", "Clear selections")
        ),
        mainPanel(
          plotOutput("cbpPlot", height = "520px"),
          tableOutput("cbpTable"),
          downloadButton("download_cbp", "Download CBP data (CSV)"),
          helpText("Source: U.S. Census Bureau, County Business Patterns (CBP) API. ",
                   "Annual payroll (PAYANN) is reported in thousands of dollars; dashboard displays millions. ",
                   "Suppressed values are reported as zero by CBP and are omitted here for Employees and Annual payroll.")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Clear buttons
  observeEvent(input$clear_gdp, {
    updateSelectizeInput(session, "gdp_county", selected = character(0))
  })
  observeEvent(input$clear_cbp, {
    updateSelectizeInput(session, "cbp_county", selected = character(0))
  })
  
  # Dynamic CBP indicator label
  observeEvent(input$cbp_indicator, {
    updateSelectInput(
      session, "cbp_indicator",
      label = if (identical(input$cbp_indicator, "PAYANN")) {
        "Indicator: Annual payroll (Millions $)"
      } else {
        "Indicator:"
      }
    )
  })
  
  # ---- GDP filtered data (suppress zeros) ----
  filtered_gdp <- reactive({
    req(input$gdp_sector)
    if (is.null(input$gdp_county) || length(input$gdp_county) == 0) return(gdp_dash[0, ])
    gdp_dash %>%
      filter(county %in% input$gdp_county,
             industry_title == input$gdp_sector) %>%
      mutate(GDP_millions = dplyr::na_if(GDP_millions, 0))
  })
  
  output$gdpPlot <- renderPlot({
    df <- filtered_gdp()
    if (nrow(df) == 0 || all(is.na(df$GDP_millions))) {
      plot.new(); text(0.5, 0.5, "Select one or more counties to display", cex = 1.3); return()
    }
    ggplot(df, aes(x = Year, y = GDP_millions, color = county, group = county)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE) +
      scale_color_brewer(palette = "Dark2") +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(labels = scales::label_dollar(suffix = "M")) +
      labs(
        title = paste("Real GDP (chained 2017 dollars) for", input$gdp_sector),
        subtitle = "BEA Regional API Table CAGDP9 (suppressed years omitted)",
        x = "Year",
        y = "GDP (Millions of dollars)",
        color = "County"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  output$gdpTable <- renderTable({
    df <- filtered_gdp()
    if (nrow(df) == 0) return(NULL)
    df %>%
      filter(!is.na(GDP_millions)) %>%
      select(County = county, Year, Sector = industry_title, `GDP (Millions)` = GDP_millions) %>%
      arrange(County, Year) %>%
      mutate(`GDP (Millions)` = scales::dollar(`GDP (Millions)`))
  }, striped = TRUE, rownames = FALSE)
  
  # GDP download (CSV)
  output$download_gdp <- downloadHandler(
    filename = function() {
      paste0("gdp_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_gdp() %>%
        filter(!is.na(GDP_millions)) %>%
        select(County = county, Year, Sector = industry_title, GDP_Millions = GDP_millions) %>%
        arrange(County, Year)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ---- CBP filtered data (suppress zeros, convert payroll to millions) ----
  filtered_cbp <- reactive({
    req(input$cbp_industry)
    if (is.null(input$cbp_county) || length(input$cbp_county) == 0) return(cbp_dash[0, ])
    cbp_dash %>%
      filter(industry_title == input$cbp_industry,
             county %in% input$cbp_county) %>%
      mutate(
        EMP      = dplyr::na_if(EMP, 0L),      # suppressed
        PAYANN   = dplyr::na_if(PAYANN, 0),    # suppressed
        PAYANN_M = PAYANN / 1000               # thousands -> millions
      )
  })
  
  output$cbpPlot <- renderPlot({
    df <- filtered_cbp()
    if (nrow(df) == 0 ||
        (identical(input$cbp_indicator, "PAYANN") && all(is.na(df$PAYANN_M))) ||
        (!identical(input$cbp_indicator, "PAYANN") && all(is.na(df[[input$cbp_indicator]])))) {
      plot.new(); text(0.5, 0.5, "Select one or more counties to display", cex = 1.3); return()
    }
    
    if (identical(input$cbp_indicator, "PAYANN")) {
      y_col <- "PAYANN_M"
      ylab  <- "Annual payroll (Millions $)"
      y_scale <- scale_y_continuous(labels = scales::label_dollar(suffix = "M"))
    } else if (identical(input$cbp_indicator, "EMP")) {
      y_col <- "EMP"
      ylab  <- "Employees"
      y_scale <- scale_y_continuous(labels = scales::label_comma())
    } else {
      y_col <- "ESTAB"
      ylab  <- "Establishments"
      y_scale <- scale_y_continuous(labels = scales::label_comma())
    }
    
    ggplot(df, aes(x = as.integer(year), y = .data[[y_col]], color = county, group = county)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE) +
      scale_color_brewer(palette = "Dark2") +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      y_scale +
      labs(
        title = paste(ylab, "for", input$cbp_industry),
        subtitle = if (y_col %in% c("PAYANN_M","EMP")) "Suppressed (zero) values omitted" else NULL,
        x = "Year", y = ylab, color = "County"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  output$cbpTable <- renderTable({
    df <- filtered_cbp()
    if (nrow(df) == 0) return(NULL)
    # Drop suppressed rows for chosen indicator
    if (identical(input$cbp_indicator, "PAYANN")) {
      df <- df %>% filter(!is.na(PAYANN_M))
    } else {
      df <- df %>% filter(!is.na(.data[[input$cbp_indicator]]))
    }
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      select(County = county, Year = year,
             Industry = industry_title,
             Establishments = ESTAB,
             Employees = EMP,
             `Annual payroll (Millions $)` = PAYANN_M) %>%
      arrange(County, Year) %>%
      mutate(
        Establishments = format(Establishments, big.mark = ",", scientific = FALSE),
        Employees      = ifelse(is.na(Employees), NA, format(Employees, big.mark = ",", scientific = FALSE)),
        `Annual payroll (Millions $)` = ifelse(is.na(`Annual payroll (Millions $)`), NA,
                                               scales::dollar(`Annual payroll (Millions $)`))
      )
  }, striped = TRUE, rownames = FALSE)
  
  # CBP download (CSV)
  output$download_cbp <- downloadHandler(
    filename = function() {
      paste0("cbp_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_cbp()
      # For PAYANN, include the converted millions column; otherwise include all
      df_out <- df %>%
        select(County = county, Year = year, Industry = industry_title,
               Establishments = ESTAB, Employees = EMP, Payroll_Millions = PAYANN_M) %>%
        # Omit suppressed rows for the chosen indicator
        {
          if (identical(input$cbp_indicator, "PAYANN")) dplyr::filter(., !is.na(Payroll_Millions))
          else if (identical(input$cbp_indicator, "EMP")) dplyr::filter(., !is.na(Employees))
          else dplyr::filter(., !is.na(Establishments))
        } %>%
        arrange(County, Year)
      write.csv(df_out, file, row.names = FALSE)
    }
  )
}

# ---- Run App ----
shinyApp(ui = ui, server = server)