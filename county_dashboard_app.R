library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(tidycensus)
library(shiny)
library(ggplot2)
library(scales)

setwd("C:/Users/n.palardy/OneDrive - University of Florida/UF/UF workspace/R/county_dashboard")

# Load data
gdp <- read_csv("CAGDP9_FL_2001_2023.csv", show_col_types = FALSE)
qcew_2024 <- read_csv("2024.annual.singlefile.csv", show_col_types = FALSE)
qcew_2023 <- read_csv("2023.annual.singlefile.csv", show_col_types = FALSE)
qcew_2022 <- read_csv("2022.annual.singlefile.csv", show_col_types = FALSE)
qcew_2021 <- read_csv("2021.annual.singlefile.csv", show_col_types = FALSE)
qcew_2020 <- read_csv("2020.annual.singlefile.csv", show_col_types = FALSE)
qcew_2019 <- read_csv("2019.annual.singlefile.csv", show_col_types = FALSE)
qcew_2018 <- read_csv("2018.annual.singlefile.csv", show_col_types = FALSE)
qcew_2017 <- read_csv("2017.annual.singlefile.csv", show_col_types = FALSE)
qcew_2016 <- read_csv("2016.annual.singlefile.csv", show_col_types = FALSE)
qcew_2015 <- read_csv("2015.annual.singlefile.csv", show_col_types = FALSE)
qcew_2014 <- read_csv("2014.annual.singlefile.csv", show_col_types = FALSE)
qcew_2013 <- read_csv("2013.annual.singlefile.csv", show_col_types = FALSE)
qcew_2012 <- read_csv("2012.annual.singlefile.csv", show_col_types = FALSE)
qcew_2011 <- read_csv("2011.annual.singlefile.csv", show_col_types = FALSE)
qcew_2010 <- read_csv("2010.annual.singlefile.csv", show_col_types = FALSE)
qcew_2009 <- read_csv("2009.annual.singlefile.csv", show_col_types = FALSE)
qcew_2008 <- read_csv("2008.annual.singlefile.csv", show_col_types = FALSE)
qcew_2007 <- read_csv("2007.annual.singlefile.csv", show_col_types = FALSE)
qcew_2006 <- read_csv("2006.annual.singlefile.csv", show_col_types = FALSE)
qcew_2005 <- read_csv("2005.annual.singlefile.csv", show_col_types = FALSE)
qcew_2004 <- read_csv("2004.annual.singlefile.csv", show_col_types = FALSE)
qcew_2003 <- read_csv("2003.annual.singlefile.csv", show_col_types = FALSE)
qcew_2002 <- read_csv("2002.annual.singlefile.csv", show_col_types = FALSE)
qcew_2001 <- read_csv("2001.annual.singlefile.csv", show_col_types = FALSE)
industry_titles <- read_csv("industry-titles.csv", show_col_types = FALSE)

# Example GDP codes (replace this with unique(gdp$IndustryClassification))
gdp_industry_codes <- c("10", "101", "1011", "1012", "1013", "102", "1021", "1022", "1023", "1024", "1025", "1026", "1027", "1028", "1029", "11")


# Clean and process the data
years <- as.character(2001:2023)

gdp <- gdp %>%
  filter(GeoName != "None", !is.na(GeoName)) %>%
  mutate(across(all_of(years_gdp), as.numeric)) %>%
  pivot_longer(
    cols = all_of(years_gdp),
    names_to = "Year",
    values_to = "GDP"
  ) %>%
  mutate(Year = as.integer(Year))
# Helper function: expand one code string into NAICS codes as character
expand_gdp_code <- function(code) {
  # Handle NA code values directly
  if (is.na(code) || code == "") return(NA_character_)
  parts <- unlist(strsplit(code, ","))
  codes <- unlist(lapply(parts, function(part) {
    if (is.na(part) || part == "") return(NA_character_)
    if (stringr::str_detect(part, "-")) {
      bounds <- as.numeric(strsplit(part, "-")[[1]])
      sprintf("%d", seq(bounds[1], bounds[2]))
    } else {
      part
    }
  }))
  codes
}

# Build lookup table: GDP (id, original_code, expanded NAICS code)
gdp_lookup <- tibble(
  IndustryClassification = gdp_industry_codes
) %>%
  mutate(NAICS = map(IndustryClassification, expand_gdp_code)) %>%
  unnest(NAICS)

library(dplyr)
library(purrr)
library(stringr)

# 1. List all your QCEW annual dataframes in order—newest to oldest or as you wish
qcew_dfs <- list(
  qcew_2024, qcew_2023, qcew_2022, qcew_2021, qcew_2020, qcew_2019, qcew_2018, qcew_2017,
  qcew_2016, qcew_2015, qcew_2014, qcew_2013, qcew_2012, qcew_2011, qcew_2010, qcew_2009,
  qcew_2008, qcew_2007, qcew_2006, qcew_2005, qcew_2004, qcew_2003, qcew_2002, qcew_2001
)

# 2. Define a function to process one QCEW dataframe
process_qcew <- function(df) {
  df %>%
    filter(nchar(area_fips) == 5, substr(area_fips, 1, 2) == "12") %>%
    mutate(
      industry_code = as.character(industry_code)
    ) %>%
    select(
      own_code, agglvl_code, area_fips, size_code, industry_code, year,
      total_annual_wages, annual_avg_emplvl, annual_avg_estabs, annual_avg_wkly_wage
    ) %>%
    left_join(mutate(industry_titles, industry_code = as.character(industry_code)), by = 'industry_code') %>%
    mutate(
      industry_title = str_trim(str_remove_all(industry_title, "[0-9]"))
    ) %>%
    filter(industry_code %in% gdp_lookup$NAICS)
}
# 3. Stack and process all files at once, then summarize
qcew_fl <- map_dfr(qcew_dfs, process_qcew)

summed_fl <- qcew_fl %>%
  group_by(area_fips, size_code, industry_code, industry_title, year) %>%
  summarise(
    total_annual_wages = sum(total_annual_wages, na.rm = TRUE),
    annual_avg_emplvl = sum(annual_avg_emplvl, na.rm = TRUE),
    annual_avg_estabs = sum(annual_avg_estabs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ownership_level = "all ownerships")

# 4. County lookup
county_lookup <- gdp %>%
  select(area_fips = GeoFIPS, county = GeoName) %>%
  distinct()

summed_fl <- summed_fl %>%
  left_join(county_lookup, by = "area_fips") %>%  
  mutate(
    total_annual_wages = na_if(total_annual_wages, 0),
    annual_avg_emplvl  = na_if(annual_avg_emplvl, 0),
    annual_avg_estabs  = na_if(annual_avg_estabs, 0)
  )

# ---- Years ----
years_gdp <- as.character(2001:2023)
years_qcew <- as.character(2001:2024)

# ---- Data import and preprocessing must be completed before app runs ----
# For example, you should already have the following loaded and ready:
# gdp, summed_fl
# With correct columns for each as used below.

ui <- fluidPage(
  titlePanel("Florida County Economic Indicator Dashboard"),
  fluidRow(
    column(6, tags$a(
      href = "https://apps.bea.gov/regional/downloadzip.htm?_gl=1*1qmeqa6*_ga*MTU2NTU0NjM0My4xNzY5MDk2ODk5*_ga_J4698JNNFT*czE3NjkwOTY4OTkkbzEkZzAkdDE3NjkwOTY4OTkkajYwJGwwJGgw",
      target = "_blank",
      class = "btn btn-primary btn-lg",
      "Download BEA Regional Data"
    )),
    column(6, tags$a(
      href = "https://www.bls.gov/cew/downloadable-data-files.htm",
      target = "_blank",
      class = "btn btn-success btn-lg",
      "Download BLS QCEW Data"
    ))
  ),
  tags$br(),
  tabsetPanel(
    tabPanel("GDP",
             sidebarLayout(
               sidebarPanel(
                 selectInput("sector", "Select Sector:",
                             choices = unique(gdp$Description)),
                 selectizeInput("county", "Select up to 5 counties:",
                                choices = unique(gdp$GeoName),
                                multiple = TRUE,
                                options = list(maxItems = 5))
               ),
               mainPanel(
                 plotOutput("gdpPlot", height = "500px"),
                 tableOutput("gdpTable")
               )
             )
    ),
    tabPanel("Employment & Wages",
             sidebarLayout(
               sidebarPanel(
                 selectInput("industry", "Select Industry:",
                             choices = unique(summed_fl$industry_title)),
                 selectizeInput("county_qcew", "Select up to 5 counties:",
                                choices = unique(summed_fl$county),
                                multiple = TRUE,
                                options = list(maxItems = 5)),
                 selectInput("indicator", "Indicator:",
                             choices = c("Total Annual Wages" = "total_annual_wages",
                                         "Average Employment Level" = "annual_avg_emplvl",
                                         "Average Number of Establishments" = "annual_avg_estabs"))
               ),
               mainPanel(
                 plotOutput("emplPlot", height = "500px"),
                 tableOutput("emplTable")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # --- GDP filtered data ---
  filtered_gdp <- reactive({
    req(input$county, input$sector)
    gdp %>%
      filter(GeoName %in% input$county, Description == input$sector)
  })
  
  output$gdpPlot <- renderPlot({
    df_long <- filtered_gdp()
    if (nrow(df_long) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selection", cex = 2)
      return()
    }
    ggplot(df_long, aes(x = Year, y = GDP / 10000, color = GeoName, group = GeoName)) +
      geom_line(size = 1.3) +
      geom_point(size = 3) +
      scale_color_brewer(palette = "Dark2") +
      scale_y_continuous(labels = label_dollar(prefix = "$", suffix = "0K", big.mark = ",", accuracy = 1)) +
      labs(
        title = paste("GDP for", input$sector, "in Selected Counties"),
        x = "Year",
        y = "GDP (10,000s of dollars)",
        color = "County"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$gdpTable <- renderTable({
    df <- filtered_gdp()
    if (nrow(df) == 0) return(NULL)
    out <- df %>%
      select(County = GeoName, Year, Indicator = Description, GDP) %>%
      arrange(County, Year)
    out$GDP <- format(round(as.numeric(out$GDP)), big.mark = ",", scientific = FALSE)
    out
  }, striped = TRUE, rownames = FALSE)
  # --- Employment & Wages filtered data ---
  filtered_empl <- reactive({
    req(input$county_qcew, input$industry, input$indicator)
    summed_fl %>%
      filter(county %in% input$county_qcew,
             industry_title == input$industry)
  })
  
  output$emplPlot <- renderPlot({
    df <- filtered_empl()
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selection", cex = 2)
      return()
    }
    ylabel <- switch(
      input$indicator,
      "total_annual_wages" = "Total Annual Wages ($)",
      "annual_avg_emplvl" = "Average Employment Level",
      "annual_avg_estabs" = "Average Number of Establishments"
    )
    ggplot(df, aes(x = as.integer(year), y = .data[[input$indicator]], color = county, group = county)) +
      geom_line(size = 1.3) +
      geom_point(size = 3) +
      scale_color_brewer(palette = "Dark2") +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_y_continuous(labels = label_comma()) +
      labs(
        title = paste(ylabel, "for", input$industry),
        x = "Year",
        y = ylabel,
        color = "County"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$emplTable <- renderTable({
    df <- filtered_empl()
    if (nrow(df) == 0) return(NULL)
    out <- df %>%
      select(County = county, Year = year,
             `Total Annual Wages` = total_annual_wages,
             `Average Employment Level` = annual_avg_emplvl,
             `Average Number of Establishments` = annual_avg_estabs) %>%
      arrange(County, Year)
    out$`Total Annual Wages` <- format(out$`Total Annual Wages`, big.mark = ",", scientific = FALSE)
    out$`Average Employment Level` <- format(out$`Average Employment Level`, big.mark = ",", scientific = FALSE)
    out$`Average Number of Establishments` <- format(out$`Average Number of Establishments`, big.mark = ",", scientific = FALSE)
    out
  }, striped = TRUE, rownames = FALSE)
}

shinyApp(ui = ui, server = server)