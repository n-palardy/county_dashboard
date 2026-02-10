# ---- Packages ----
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidycensus)
library(httr)
library(jsonlite)
library(purrr)
library(tibble)

setwd("C:/Users/n.palardy/OneDrive - University of Florida/UF/UF workspace/R/county_dashboard_clean")

# =========================
# CBP DATA (already in your script)
# =========================

# Detect the NAICS variable name for a given year (CBP changed over time)
detect_naics_var <- function(year) {
  vars_url <- paste0("https://api.census.gov/data/", year, "/cbp/variables.json")
  res <- httr::GET(vars_url)
  if (res$status_code != 200) return(NA)  # year not available
  vars <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  vnames <- names(vars$variables)
  if ("NAICS2022" %in% vnames) return("NAICS2022")
  if ("NAICS2017" %in% vnames) return("NAICS2017")
  if ("NAICS2012" %in% vnames) return("NAICS2012")
  if ("NAICS2007" %in% vnames) return("NAICS2007")
  NA
}

# Fetch CBP for all Florida counties for one NAICS sector and one year
fetch_cbp_fl_by_naics <- function(year, naics_var, naics_code, sleep_sec = 0.05) {
  url <- paste0(
    "https://api.census.gov/data/", year, "/cbp?",
    "get=ESTAB,EMP,PAYANN,", naics_var,
    "&for=county:*",
    "&in=state:12",
    "&", naics_var, "=", naics_code
  )
  res <- httr::GET(url)
  Sys.sleep(sleep_sec)  # polite throttle
  if (res$status_code != 200) return(NULL)
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  if (grepl('^\\s*\\{\\s*"error"\\s*:', txt)) return(NULL)
  
  dat <- jsonlite::fromJSON(txt)
  if (length(dat) < 2) return(NULL)
  
  headers <- as.character(dat[1, , drop = TRUE])
  rows <- dat[-1, , drop = FALSE]
  df <- tibble::as_tibble(as.data.frame(rows, stringsAsFactors = FALSE), .name_repair = "unique")
  names(df) <- make.unique(headers)
  
  # geography columns are "state" and "county"
  state_col  <- names(df)[grepl("^state$",  names(df), ignore.case = TRUE)][1]
  county_col <- names(df)[grepl("^county$", names(df), ignore.case = TRUE)][1]
  if (is.na(state_col) || is.na(county_col)) return(NULL)
  
  df %>%
    mutate(
      year   = year,
      ESTAB  = suppressWarnings(as.integer(ESTAB)),
      EMP    = suppressWarnings(as.integer(EMP)),
      PAYANN = suppressWarnings(as.numeric(PAYANN)),
      county_fips = paste0(.data[[state_col]], .data[[county_col]]),
      NAICS  = .data[[naics_var]]
    ) %>%
    select(year, county_fips, NAICS, ESTAB, EMP, PAYANN)
}

# Years to probe: 2010 through current year; keep only those with a valid NAICS var
candidate_years <- 2010:as.integer(format(Sys.Date(), "%Y"))
year_vars <- setNames(purrr::map(candidate_years, detect_naics_var), candidate_years)
available_years <- as.integer(names(year_vars[!is.na(unlist(year_vars))]))

# Your requested NAICS sectors
naics_list <- c(
  "11","21","22","23","31-33","42","44-45","48-49",
  "51","52","53","54","55","56","61","62","71","72","81","99"
)

# Pull all years × sectors for Florida counties
cbp_fl_all <- purrr::map_dfr(available_years, function(y) {
  nv <- year_vars[[as.character(y)]]
  purrr::map_dfr(naics_list, ~fetch_cbp_fl_by_naics(y, nv, .x))
})

# NAICS titles
naics_titles <- c(
  "11"   = "Agriculture, forestry, fishing and hunting",
  "21"   = "Mining, quarrying, and oil and gas extraction",
  "22"   = "Utilities",
  "23"   = "Construction",
  "31-33"= "Manufacturing",
  "42"   = "Wholesale trade",
  "44-45"= "Retail trade",
  "48-49"= "Transportation and warehousing",
  "51"   = "Information",
  "52"   = "Finance and insurance",
  "53"   = "Real estate and rental and leasing",
  "54"   = "Professional, scientific, and technical services",
  "55"   = "Management of companies and enterprises",
  "56"   = "Administrative and support and waste management and remediation services",
  "61"   = "Educational services",
  "62"   = "Health care and social assistance",
  "71"   = "Arts, entertainment, and recreation",
  "72"   = "Accommodation and food services",
  "81"   = "Other services (except public administration)",
  "99"   = "Industries not classified"
)

# County names
fl_county_names <- tidycensus::fips_codes %>%
  filter(state_code == "12") %>%
  distinct(county_code, county, .keep_all = TRUE) %>%
  mutate(county_fips = paste0(state_code, county_code)) %>%
  select(county_fips, county)

cbp_dash <- cbp_fl_all %>%
  mutate(
    NAICS = str_trim(NAICS),
    county_fips = as.character(county_fips)
  ) %>%
  left_join(fl_county_names, by = "county_fips") %>%
  mutate(
    county = if_else(is.na(county), paste0("FIPS ", county_fips), county),
    industry_title = recode(NAICS, !!!naics_titles, .default = NA_character_)
  ) %>%
  filter(!is.na(industry_title))

# =========================
# BEA COUNTY GDP (API) — REPLACEMENT
# =========================
# =========================
# Complete working code: BEA county GDP (CAGDP9) for Florida counties
# Builds gdp_dash with county, Year, NAICS sector, Unit, GDP_millions
# =========================

# 0) Set BEA API key for this session (paste your key)
Sys.setenv(BEA_KEY = "3942CEA9-6CA8-4835-86E9-8986C98D15F2")

# 1) Libraries
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(tidycensus)
library(stringr)

# 2) Key
bea_key <- Sys.getenv("BEA_KEY")
stopifnot(nzchar(bea_key))

# 3) Retry/backoff helper
bea_get_with_retries <- function(url, max_tries = 8, base_wait = 1) {
  attempt <- 1
  repeat {
    res <- try(httr::GET(url), silent = TRUE)
    if (inherits(res, "try-error")) {
      Sys.sleep(base_wait * 2^(attempt - 1))
    } else if (res$status_code == 200) {
      return(res)
    } else if (res$status_code %in% c(429, 500, 502, 503, 504)) {
      ra <- httr::headers(res)[["retry-after"]]
      wait <- if (!is.null(ra)) as.numeric(ra) else base_wait * 2^(attempt - 1)
      Sys.sleep(wait)
    } else {
      stop(sprintf("BEA error %s: %s", res$status_code,
                   httr::content(res, as = "text", encoding = "UTF-8")))
    }
    attempt <- attempt + 1
    if (attempt > max_tries) stop("Exceeded max retries for URL: ", url)
  }
}
# 4) Safe BEA GET wrapper (coerces params to character, uses datasetname)
bea_get <- function(params) {
  params$UserID       <- Sys.getenv("BEA_KEY")
  params$ResultFormat <- "JSON"
  if (is.null(params$datasetname) && is.null(params$dataset)) {
    params$datasetname <- "Regional"
  }
  qs <- paste0(
    names(params), "=",
    vapply(params, function(x) URLencode(as.character(x), reserved = TRUE),
           FUN.VALUE = character(1))
  )
  url <- paste0("https://apps.bea.gov/api/data?", paste(qs, collapse = "&"))
  res <- bea_get_with_retries(url)
  out <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  if (!is.null(out$BEAAPI$Error)) {
    return(list(Data = NULL, Note = out$BEAAPI$Error))
  }
  out$BEAAPI$Results
}

# 5) Table and sector mapping (CAGDP9 = real GDP, chained 2017 dollars)
table_name <- "CAGDP9"
naics_to_line <- c(
  "11"=3, "21"=6, "22"=10, "23"=11, "31-33"=12,
  "42"=34, "44-45"=35, "48-49"=36, "51"=45, "52"=50, "53"=51,
  "54"=56, "55"=59, "56"=60, "61"=64, "62"=65, "71"=68, "72"=69, "81"=70, "99"=99
)
line_to_naics <- setNames(names(naics_to_line), as.character(naics_to_line))

# 6) Florida counties (GeoFIPS) and chunking
fl_counties <- tidycensus::fips_codes %>%
  filter(state_code == "12") %>%
  distinct(county_code, .keep_all = TRUE) %>%
  transmute(GeoFIPS = paste0(state_code, county_code)) %>%
  pull(GeoFIPS)

chunk_vec <- function(x, size = 20) split(x, ceiling(seq_along(x) / size))
geo_chunks <- chunk_vec(fl_counties, size = 20)

# 7) Years
years_vec <- 2010:2023
years_csv <- paste(years_vec, collapse = ",")

# 8) Pull data (line-code x chunk) with pacing
raw_df <- purrr::map_dfr(unname(naics_to_line), function(lc) {
  purrr::map_dfr(geo_chunks, function(keys) {
    Sys.sleep(0.5)  # polite pacing per chunk
    res <- bea_get(list(
      method      = "GetData",
      TableName   = table_name,
      LineCode    = lc,
      GeoFIPS     = paste(keys, collapse = ","),
      Year        = years_csv
    ))
    if (is.null(res$Data) || length(res$Data) == 0) return(tibble())
    tibble::as_tibble(res$Data)
  })
})

if (nrow(raw_df) == 0) stop("BEA returned no data. Try reducing chunk size (e.g., 10), increasing delays, or verifying table/years.")
# Case-insensitive column pickers
pick_col_ci <- function(df, candidates) {
  nm <- names(df); low <- tolower(nm)
  for (cand in tolower(candidates)) {
    hit <- which(low == cand)
    if (length(hit)) return(nm[hit[1]])
  }
  NA_character_
}

# Mapping
naics_to_line <- c(
  "11"=3, "21"=6, "22"=10, "23"=11, "31-33"=12,
  "42"=34, "44-45"=35, "48-49"=36, "51"=45, "52"=50, "53"=51,
  "54"=56, "55"=59, "56"=60, "61"=64, "62"=65, "71"=68, "72"=69, "81"=70, "99"=99
)
line_to_naics <- setNames(names(naics_to_line), as.character(naics_to_line))

naics_titles <- c(
  "11"="Agriculture, forestry, fishing and hunting",
  "21"="Mining, quarrying, and oil and gas extraction",
  "22"="Utilities",
  "23"="Construction",
  "31-33"="Manufacturing",
  "42"="Wholesale trade",
  "44-45"="Retail trade",
  "48-49"="Transportation and warehousing",
  "51"="Information",
  "52"="Finance and insurance",
  "53"="Real estate and rental and leasing",
  "54"="Professional, scientific, and technical services",
  "55"="Management of companies and enterprises",
  "56"="Administrative and support and waste management and remediation services",
  "61"="Educational services",
  "62"="Health care and social assistance",
  "71"="Arts, entertainment, and recreation",
  "72"="Accommodation and food services",
  "81"="Other services (except public administration)",
  "99"="Industries not classified"
)

# Case-insensitive pickers you already defined: pick_col_ci(...)

time_col   <- pick_col_ci(raw_df, c("TimePeriod","Year","Time"))
value_col  <- pick_col_ci(raw_df, c("DataValue","Data","Value"))
unit_col   <- pick_col_ci(raw_df, c("Unit","CL_UNIT"))
geo_col    <- pick_col_ci(raw_df, c("GeoFIPS","GeoFips","GeoCode"))
name_col   <- pick_col_ci(raw_df, c("GeoName","Area","Geo"))
code_col   <- pick_col_ci(raw_df, c("Code","IndustryClassification","Industry"))

stopifnot(all(!is.na(c(time_col, value_col, unit_col, geo_col, name_col, code_col))))

# Florida county names
fl_county_names <- tidycensus::fips_codes %>%
  filter(state_code == "12") %>%
  distinct(county_code, county, .keep_all = TRUE) %>%
  mutate(county_fips = paste0(state_code, county_code)) %>%
  select(county_fips, county)

# Build gdp_dash: parse Code like "CAGDP9-3" -> 3 -> NAICS -> title
gdp_dash <- raw_df %>%
  mutate(
    Year           = suppressWarnings(as.integer(.data[[time_col]])),
    county_fips    = .data[[geo_col]],
    Code_str       = as.character(.data[[code_col]]),
    line_from_code = suppressWarnings(as.integer(sub("^.*-", "", Code_str))),
    NAICS          = line_to_naics[as.character(line_from_code)],
    industry_title = naics_titles[NAICS],
    GDP_thousands  = suppressWarnings(as.numeric(gsub(",", "", .data[[value_col]]))),
    Unit           = .data[[unit_col]],
    GeoName_fix    = .data[[name_col]]
  ) %>%
  filter(!is.na(GDP_thousands), Year >= 2010, !is.na(NAICS)) %>%
  left_join(fl_county_names, by = "county_fips") %>%
  mutate(
    county        = coalesce(county, GeoName_fix),
    GDP_millions  = GDP_thousands / 1000
  ) %>%
  select(county, Year, NAICS, industry_title, Unit, GDP_millions)
# Sanity check: if still empty, inspect the NAICS values that came back
if (nrow(gdp_dash) == 0) {
  message("No rows after filtering. Unique NAICS codes returned:\n",
          paste(head(unique(stringr::str_trim(raw_df[[naics_col]])), 30), collapse = ", "))
}

# 14) Preview
glimpse(gdp_dash)

# Complete working Shiny app (assumes gdp_dash and cbp_dash are already created in your environment)
# - GDP: chained 2017 dollars (millions), suppressed (0) values omitted
# - CBP: Annual payroll reported in thousands -> displayed in millions; EMP/PAYANN zeros treated as suppressed and omitted
# - No auto-selected counties; clear buttons provided; dynamic label when PAYANN is selected

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

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
          helpText("Source: U.S. Bureau of Economic Analysis (BEA) Regional API, Table CAGDP9 (Real GDP, chained 2017 dollars).",
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
          helpText("Source: U.S. Census Bureau, County Business Patterns (CBP) API. ",
                   "Annual payroll (PAYANN) is reported in thousands of dollars; dashboard displays millions. ",
                   "Suppressed values are reported as zero by CBP and are omitted here for Employees and Annual payroll.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Clear buttons
  observeEvent(input$clear_gdp, {
    updateSelectizeInput(session, "gdp_county", selected = character(0))
  })
  observeEvent(input$clear_cbp, {
    updateSelectizeInput(session, "cbp_county", selected = character(0))
  })
  
  # Dynamically change the CBP indicator label when PAYANN is selected
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
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(labels = label_dollar(suffix = "M")) +
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
      mutate(`GDP (Millions)` = dollar(`GDP (Millions)`))
  }, striped = TRUE, rownames = FALSE)
  
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
      y_scale <- scale_y_continuous(labels = label_dollar(suffix = "M"))
    } else if (identical(input$cbp_indicator, "EMP")) {
      y_col <- "EMP"
      ylab  <- "Employees"
      y_scale <- scale_y_continuous(labels = label_comma())
    } else {
      y_col <- "ESTAB"
      ylab  <- "Establishments"
      y_scale <- scale_y_continuous(labels = label_comma())
    }
    
    ggplot(df, aes(x = as.integer(year), y = .data[[y_col]], color = county, group = county)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE) +
      scale_color_brewer(palette = "Dark2") +
      scale_x_continuous(breaks = pretty_breaks()) +
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
    
    out <- df %>%
      select(County = county, Year = year,
             Industry = industry_title,
             Establishments = ESTAB,
             Employees = EMP,
             `Annual payroll (Millions $)` = PAYANN_M) %>%
      arrange(County, Year)
    
    out$Establishments <- format(out$Establishments, big.mark = ",", scientific = FALSE)
    out$Employees      <- ifelse(is.na(out$Employees), NA, format(out$Employees, big.mark = ",", scientific = FALSE))
    out$`Annual payroll (Millions $)` <- ifelse(is.na(out$`Annual payroll (Millions $)`), NA,
                                                dollar(out$`Annual payroll (Millions $)`))
    out
  }, striped = TRUE, rownames = FALSE)
}

shinyApp(ui = ui, server = server)