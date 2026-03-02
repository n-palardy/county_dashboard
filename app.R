# ---- Packages ----
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(tibble)

# ---- Precomputed data (fast startup) ----
gdp_path <- file.path("data", "gdp_dash.rds")
cbp_path <- file.path("data", "cbp_dash.rds")
if (!file.exists(gdp_path) || !file.exists(cbp_path)) {
  stop("Precomputed data files not found. Please run locally: saveRDS(gdp_dash, 'data/gdp_dash.rds'); saveRDS(cbp_dash, 'data/cbp_dash.rds')")
}
gdp_dash <- readRDS(gdp_path)
cbp_dash <- readRDS(cbp_path)

LATEST_GDP_YEAR <- max(gdp_dash$Year, na.rm = TRUE)
LATEST_CBP_YEAR <- max(cbp_dash$year, na.rm = TRUE)

# Labels for the "Choose data" option (cross-sector)
LABEL_GDP <- "Gross Domestic Product (GDP) by industry"
LABEL_CBP <- "Establishments, Employment, and Payroll by industry"

# County Business Patterns exclusions note (displayed directly under graphs)
CBP_EXCLUSION_NOTE <- paste(
  "Note: The County Business Patterns data from the US Census Bureau does not include information from the following industries:",
  "Crop and animal production; Rail transportation; Postal Service;",
  "Insurance and employee benefit funds; Trusts, estates, and agency accounts;",
  "Public schools and colleges; Private households; and Public administration"
)
# ---- UI ----
ui <- fluidPage(
  titlePanel("Florida County Economic Dashboard"),
  tabsetPanel(
    tabPanel(
      "Gross Domestic Product (GDP) Over Time",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "gdp_sector", "Select industry:",
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
          downloadButton("download_gdp", "Download GDP data (CSV)"),
          helpText("Note: Real GDP values are in chained 2017 dollars (BEA CAGDP9). Displayed in millions.")
        ),
        mainPanel(
          plotOutput("gdpPlot", height = "520px"),
          tableOutput("gdpTable"),
          helpText("Source: U.S. Bureau of Economic Analysis (BEA) Regional API, Table CAGDP9 (Real GDP, chained 2017 dollars).",
                   "Years with suppressed values are omitted (zeros treated as suppressed).")
        )
      )
    ),
    
    tabPanel(
      "Establishments, Employment, and Payroll Over Time",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "cbp_industry", "Select industry:",
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
            "cbp_indicator", "Economic indicator:",
            choices = c("Establishments" = "ESTAB",
                        "Employees" = "EMP",
                        "Annual payroll ($)" = "PAYANN"),
            selected = "EMP"
          ),
          actionButton("clear_cbp", "Clear selections"),
          downloadButton("download_cbp", "Download County Business Patterns data (CSV)")
        ),
        mainPanel(
          plotOutput("cbpPlot", height = "520px"),
          helpText(CBP_EXCLUSION_NOTE),  # directly under the graph
          tableOutput("cbpTable"),
          helpText("Source: U.S. Census Bureau, County Business Patterns API.",
                   "Annual payroll (PAYANN) is reported in thousands of dollars; dashboard displays millions.",
                   "Suppressed values are reported as zero by County Business Patterns and are omitted here for Employees and Annual payroll.")
        )
      )
    ),
    
    tabPanel(
      "Cross-sector Comparison of Economic Indicators",
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            "latest_counties",
            "Choose counties (side-by-side comparison):",
            choices = sort(unique(gdp_dash$county)),
            multiple = TRUE,
            options = list(placeholder = "Choose one or more counties…")
          ),
          checkboxInput("latest_labels", "Show value labels", TRUE),
          
          radioButtons(  # Single selector replaces sub-tabs and "choose view"
            "latest_data", "Choose data:",
            choices = c(LABEL_GDP, LABEL_CBP),
            selected = LABEL_GDP
          ),
          
          conditionalPanel(
            condition = sprintf("input.latest_data == '%s'", LABEL_CBP),
            radioButtons(
              "cbp_latest_indicator", "Economic indicator:",
              choices = c("Establishments" = "ESTAB",
                          "Employees" = "EMP",
                          "Annual payroll (Millions $)" = "PAYANN_M"),
              selected = "EMP"
            )
          ),
          
          actionButton("clear_latest", "Clear selections"),
          helpText("Latest GDP year:"), textOutput("gdpLatestYearNote"),
          helpText("Latest County Business Patterns year:"), textOutput("cbpLatestYearNote")
        ),
        mainPanel(
          uiOutput("latest_main")  # Dynamically renders GDP or CBP view based on 'latest_data'
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  # Clear buttons
  observeEvent(input$clear_gdp,     { updateSelectizeInput(session, "gdp_county", selected = character(0)) })
  observeEvent(input$clear_cbp,     { updateSelectizeInput(session, "cbp_county", selected = character(0)) })
  observeEvent(input$clear_latest,  { updateSelectizeInput(session, "latest_counties", selected = character(0)) })
  
  # Dynamic label tweak for time-series CBP
  observeEvent(input$cbp_indicator, {
    updateSelectInput(
      session, "cbp_indicator",
      label = if (identical(input$cbp_indicator, "PAYANN")) "Economic indicator: Annual payroll (Millions $)" else "Economic indicator:"
    )
  })
  
  # ---- GDP time series filtered (suppress zeros) ----
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
  
  output$download_gdp <- downloadHandler(
    filename = function() paste0("gdp_data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- filtered_gdp() %>%
        filter(!is.na(GDP_millions)) %>%
        select(County = county, Year, Sector = industry_title, GDP_Millions = GDP_millions) %>%
        arrange(County, Year)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ---- County Business Patterns time series filtered (suppress zeros, convert payroll) ----
  filtered_cbp <- reactive({
    req(input$cbp_industry)
    if (is.null(input$cbp_county) || length(input$cbp_county) == 0) return(cbp_dash[0, ])
    cbp_dash %>%
      filter(industry_title == input$cbp_industry,
             county %in% input$cbp_county) %>%
      mutate(
        EMP      = dplyr::na_if(EMP, 0L),
        PAYANN   = dplyr::na_if(PAYANN, 0),
        PAYANN_M = PAYANN / 1000
      )
  })
  
  output$cbpPlot <- renderPlot({
    df <- filtered_cbp()
    if (nrow(df) == 0 ||
        (identical(input$cbp_indicator, "PAYANN") && all(is.na(df$PAYANN))) ||
        (!identical(input$cbp_indicator, "PAYANN") && all(is.na(df[[input$cbp_indicator]])))) {
      plot.new(); text(0.5, 0.5, "Select one or more counties to display", cex = 1.3); return()
    }
    if (identical(input$cbp_indicator, "PAYANN")) {
      y_col <- "PAYANN_M"; ylab <- "Annual payroll (Millions $)"; y_scale <- scale_y_continuous(labels = scales::label_dollar(suffix = "M"))
    } else if (identical(input$cbp_indicator, "EMP")) {
      y_col <- "EMP"; ylab <- "Employees"; y_scale <- scale_y_continuous(labels = scales::label_comma())
    } else {
      y_col <- "ESTAB"; ylab <- "Establishments"; y_scale <- scale_y_continuous(labels = scales::label_comma())
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
    if (identical(input$cbp_indicator, "PAYANN")) {
      df <- df %>% filter(!is.na(PAYANN_M))
    } else {
      df <- df %>% filter(!is.na(.data[[input$cbp_indicator]]))
    }
    df %>%
      select(County = county, Year = year, Industry = industry_title,
             Establishments = ESTAB, Employees = EMP, `Annual payroll (Millions $)` = PAYANN_M) %>%
      arrange(County, Year) %>%
      mutate(
        Establishments = format(Establishments, big.mark = ",", scientific = FALSE),
        Employees      = ifelse(is.na(Employees), NA, format(Employees, big.mark = ",", scientific = FALSE)),
        `Annual payroll (Millions $)` = ifelse(is.na(`Annual payroll (Millions $)`), NA, scales::dollar(`Annual payroll (Millions $)`))
      )
  }, striped = TRUE, rownames = FALSE)
  
  output$download_cbp <- downloadHandler(
    filename = function() paste0("county_business_patterns_data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- filtered_cbp() %>%
        select(County = county, Year = year, Industry = industry_title,
               Establishments = ESTAB, Employees = EMP, Payroll_Millions = PAYANN_M) %>%
        {
          if (identical(input$cbp_indicator, "PAYANN")) dplyr::filter(., !is.na(Payroll_Millions))
          else if (identical(input$cbp_indicator, "EMP")) dplyr::filter(., !is.na(Employees))
          else dplyr::filter(., !is.na(Establishments))
        } %>%
        arrange(County, Year)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # ---- Latest pane notes ----
  output$gdpLatestYearNote <- renderText({ paste0(LATEST_GDP_YEAR) })
  output$cbpLatestYearNote <- renderText({ paste0(LATEST_CBP_YEAR) })
  
  # ---- GDP by Industry (Latest Year), side-by-side by county ----
  gdp_latest_compare <- reactive({
    if (is.null(input$latest_counties) || length(input$latest_counties) == 0) {
      return(tibble(industry_title = character(), county = character(), GDP_millions = numeric()))
    }
    df <- gdp_dash %>%
      filter(Year == LATEST_GDP_YEAR,
             county %in% input$latest_counties) %>%
      mutate(GDP_millions = dplyr::na_if(GDP_millions, 0))
    ord <- df %>%
      group_by(industry_title) %>%
      summarise(total = sum(GDP_millions, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(industry_title)
    df %>% mutate(industry_title = factor(industry_title, levels = ord))
  })
  
  # ---- County Business Patterns by Industry (Latest Year), side-by-side by county ----
  cbp_latest_industry <- reactive({
    if (is.null(input$latest_counties) || length(input$latest_counties) == 0) {
      return(tibble(industry_title = character(), county = character(), value = numeric()))
    }
    df <- cbp_dash %>%
      filter(year == LATEST_CBP_YEAR,
             county %in% input$latest_counties) %>%
      mutate(
        EMP      = dplyr::na_if(EMP, 0L),
        PAYANN   = dplyr::na_if(PAYANN, 0),
        PAYANN_M = PAYANN / 1000
      )
    metric <- if (!is.null(input$cbp_latest_indicator)) input$cbp_latest_indicator else "EMP"
    ord <- df %>%
      group_by(industry_title) %>%
      summarise(total = sum(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(industry_title)
    df %>%
      transmute(
        industry_title = factor(industry_title, levels = ord),
        county,
        value = .data[[metric]]
      )
  })
  
  # ---- Dynamic main panel for "Cross-sector Comparison" ----
  output$latest_main <- renderUI({
    if (identical(input$latest_data, LABEL_GDP)) {
      tagList(
        plotOutput("gdpLatestBar", height = "520px"),
        tableOutput("gdpLatestTable"),
        helpText("Units: Millions of chained 2017 dollars (BEA CAGDP9). Bars are per county per industry.")
      )
    } else {
      tagList(
        plotOutput("cbpLatestIndustryBar", height = "520px"),
        helpText(CBP_EXCLUSION_NOTE),  # directly under the CBP-by-industry graph
        tableOutput("cbpLatestIndustryTable"),
        downloadButton("download_cbp_latest_industry", "Download County Business Patterns latest by industry (CSV)"),
        helpText("County Business Patterns by industry shows the latest year per county per industry side-by-side.",
                 "Employees and payroll zeros treated as suppressed (omitted). Payroll displayed in millions.")
      )
    }
  })
  
  # ---- Render latest GDP view ----
  output$gdpLatestBar <- renderPlot({
    req(identical(input$latest_data, LABEL_GDP))
    df <- gdp_latest_compare()
    if (nrow(df) == 0 || all(is.na(df$GDP_millions))) {
      plot.new(); text(0.5, 0.5, "Select one or more counties in the sidebar", cex = 1.3); return()
    }
    p <- ggplot(df, aes(x = industry_title, y = GDP_millions, fill = county)) +
      geom_col(position = position_dodge(width = 0.85), na.rm = TRUE) +
      coord_flip() +
      scale_y_continuous(labels = scales::label_dollar(suffix = "M"), expand = expansion(mult = c(0, 0.05))) +
      scale_fill_brewer(palette = "Dark2") +
      labs(
        title = paste0("Real GDP by Industry, ", LATEST_GDP_YEAR),
        subtitle = "Side-by-side comparison by county",
        x = "Industry",
        y = "GDP (Millions of chained 2017 dollars)",
        fill = "County"
      ) +
      theme_minimal(base_size = 13)
    if (isTRUE(input$latest_labels)) {
      p <- p + geom_text(aes(label = ifelse(is.na(GDP_millions), "", scales::dollar(GDP_millions))),
                         position = position_dodge(width = 0.85), hjust = -0.1, size = 3)
    }
    p
  })
  
  output$gdpLatestTable <- renderTable({
    req(identical(input$latest_data, LABEL_GDP))
    df <- gdp_latest_compare()
    if (nrow(df) == 0) return(NULL)
    df %>%
      arrange(industry_title, county) %>%
      mutate(`GDP (Millions)` = scales::dollar(GDP_millions)) %>%
      select(Industry = industry_title, County = county, `GDP (Millions)`)
  }, striped = TRUE, rownames = FALSE)
  
  # ---- Render latest County Business Patterns-by-industry view ----
  output$cbpLatestIndustryBar <- renderPlot({
    req(identical(input$latest_data, LABEL_CBP))
    df <- cbp_latest_industry()
    if (nrow(df) == 0 || all(is.na(df$value))) {
      plot.new(); text(0.5, 0.5, "Select one or more counties in the sidebar", cex = 1.3); return()
    }
    is_payroll <- identical(input$cbp_latest_indicator, "PAYANN_M")
    y_lab <- if (is_payroll) "Annual payroll (Millions $)" else if (identical(input$cbp_latest_indicator, "EMP")) "Employees" else "Establishments"
    p <- ggplot(df, aes(x = industry_title, y = value, fill = county)) +
      geom_col(position = position_dodge(width = 0.85), na.rm = TRUE) +
      coord_flip() +
      scale_fill_brewer(palette = "Dark2") +
      scale_y_continuous(labels = if (is_payroll) scales::label_dollar(suffix = "M") else scales::label_comma(),
                         expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = paste0("County Business Patterns by Industry, ", LATEST_CBP_YEAR),
        subtitle = y_lab,
        x = "Industry", y = y_lab, fill = "County"
      ) +
      theme_minimal(base_size = 13)
    if (isTRUE(input$latest_labels)) {
      lbl_fun <- if (is_payroll) scales::dollar else scales::comma
      p <- p + geom_text(aes(label = ifelse(is.na(value), "", lbl_fun(value))),
                         position = position_dodge(width = 0.85), hjust = -0.1, size = 3)
    }
    p
  })
  
  output$cbpLatestIndustryTable <- renderTable({
    req(identical(input$latest_data, LABEL_CBP))
    df <- cbp_latest_industry()
    if (nrow(df) == 0) return(NULL)
    is_payroll <- identical(input$cbp_latest_indicator, "PAYANN_M")
    df %>%
      arrange(industry_title, county) %>%
      mutate(Value = if (is_payroll) scales::dollar(value) else scales::comma(value)) %>%
      select(Industry = industry_title, County = county, Value)
  }, striped = TRUE, rownames = FALSE)
  
  output$download_cbp_latest_industry <- downloadHandler(
    filename = function() paste0("county_business_patterns_latest_by_industry_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- cbp_latest_industry() %>%
        arrange(industry_title, county) %>%
        select(Industry = industry_title, County = county, Value = value)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# ---- Run App ----
shinyApp(ui = ui, server = server)