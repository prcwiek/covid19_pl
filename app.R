library(DT)
library(ggthemes)
library(highcharter)
library(lubridate)
library(readxl)
library(shiny)
library(shinyjs)
library(tidyverse)

# Data from ---------------------------------------------------------------
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
download.file("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-12-14.xlsx",
              destfile = "COVID-19-geographic-disbtribution-worldwide-2020-12-14.xlsx")
dx <- read_excel("COVID-19-geographic-disbtribution-worldwide-2020-12-14.xlsx")
#load("others/dx.RData")

# Data from ---------------------------------------------------------------
# https://www.ecdc.europa.eu/en/publications-data/data-national-14-day-notification-rate-covid-19
dw <- readr::read_csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv")
#load("others/dw.RData")

# change date format ------------------------------------------------------
tryCatch(
  expr = {
    dx$dateRep <- as.Date(dx$dateRep, format = "%d/%m/%Y")
  },
  error = function(e){
    message("Data frame is incorrect!")
    print(e)
  },
  finally = {
    stopifnot(TRUE)
  }
)

# list of countries--------------------------------------------------------

cl <- list("Poland" = 1,
           "Austria" = 2,
           "Belgium" = 3,
           "Bulgaria" = 4,
           "Croatia" = 5,
           "Cyprus" = 6,
           "Czechia" = 7,
           "Denmark" = 8,
           "Estonia" = 9,
           "Finland" = 10,
           "France" = 10,
           "Germany" = 11,
           "Greece" = 12,
           "Hungary" = 13,
           "Ireland" = 14,
           "Italy" = 15,
           "Latvia" = 16,
           "Lithuania" = 17,
           "Luxembourg" = 18,
           "Malta" = 19,
           "Netherlands" = 20,
           "Portugal" = 21,
           "Romania" = 22,
           "Slovakia" = 23,
           "Slovenia" = 24,
           "Spain" = 25,
           "Sweden" = 26,
           "United Kingdom" = 27
)

# add countries id to dx --------------------------------------------------
dcl <-  data.frame(countriesAndTerritories = names(cl),
                   id_country = unlist(matrix(cl)))

dcl$countriesAndTerritories <- str_replace_all(dcl$countriesAndTerritories, pattern = "United Kingdom", replacement = "United_Kingdom")

dx <- dx %>%
  left_join(dcl, by = "countriesAndTerritories") %>%
  filter(!is.na(id_country)) %>%
  filter(cases >= 0) %>% 
  mutate(countriesAndTerritories = str_replace_all(countriesAndTerritories, pattern = "United_Kingdom", replacement = "United Kingdom"))

names(dcl) <- c("country", "id_country")

# 2019 population data for appending to dw
dx_population <- dx %>% select(id_country, popData2019)

dw <- dw %>% 
  left_join(dcl, by = "country") %>% 
  mutate(Date = as.Date(paste0(str_sub(year_week, 1, 4), "-01-01")) +
           as.numeric(str_sub(year_week, -2, -1)) * 7 - 
           as.numeric(format(as.Date(paste0(str_sub(year_week, 1, 4), "-01-01")), format = "%w"))) %>% 
  left_join(dx_population, by = "id_country")

names(dw) <- gsub(pattern = "^country$", "Country", names(dw))

rm(dcl)
rm(dx_population)

# colors ------------------------------------------------------------------
dcolors <- c("red", "cyan", "green", "orange", "darkblue",
             "darkgreen", "gold", "pink", "lightgray", "limegreen",
             "magenta", "navyblue", "mistyrose", "orchid", "purple",
             "blue", "yellow", "turquise", "tomato", "sienna",
             "seagreen", "violeta", "mintcream", "gray", "darksalmon",
             "deeppink", "brown", "bisque")

ui <- fluidPage(

  shinyjs::useShinyjs(),
  
  navbarPage(theme = bslib::bs_theme(version = 4, bootswatch = "cerulean", primary = "#FF0018", secondary = "#FF0018"),
             title = "COVID-19 cases in Poland",
             
             tabPanel("Weekly data",
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     dateInput(
                                       "sdate_w",
                                       "Start date:",
                                       value = "2020-03-07",
                                       format = "yyyy-mm-dd",
                                       min = min(dw$Date),
                                       max = max(dw$Date)
                                     ),
                                     dateInput(
                                       "edate_w",
                                       "End date:",
                                       value = max(dw$Date),
                                       format = "yyyy-mm-dd",
                                       min = "2020-03-07",
                                       max = max(dw$Date)
                                     ),
                                     checkboxInput("checkSmooth_w", label = "Smoothed conditional mean", value = FALSE),
                                     checkboxInput("checkConfidenceInterval_w", label = "Show confidence interval", value = FALSE),
                                     checkboxInput("casesp100_w", label = "New cases per 100,000", value = FALSE),
                                     checkboxInput("casespm_w", label = "New cases per million", value = FALSE),
                                     checkboxGroupInput(
                                       "checkCountries_w",
                                       label = h5("Select countries"),
                                       choices = cl,
                                       selected = c(1, 7, 11, 13, 17, 23, 26)
                                     )
                        ),
                        
                        mainPanel(width = 10,
                                  tabsetPanel(id = "tabs_w",
                                              tabPanel("EDPC weekly cases ggplot", plotOutput("new_weekly_cases"),
                                                       h4("Data source:"),
                                                       p("European Centre for Disease Prevention and Control"),
                                                       a(
                                                         "https://www.ecdc.europa.eu/en/publications-data/data-national-14-day-notification-rate-covid-19",
                                                         href = "https://www.ecdc.europa.eu/en/publications-data/data-national-14-day-notification-rate-covid-19"
                                                       )),
                                              tabPanel("ECDPC table", dataTableOutput("covidTable_w"))
                                  )
                        )
                      )
             ),
             
             tabPanel("Daily data",
                      #titlePanel("COVID-19 cases in Poland"),
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     dateInput(
                                       "sdate",
                                       "Start date:",
                                       value = "2020-03-07",
                                       format = "yyyy-mm-dd",
                                       min = "2020-03-07",
                                       max = max(dx$dateRep)
                                     ),
                                     dateInput(
                                       "edate",
                                       "End date:",
                                       value = max(dx$dateRep),
                                       format = "yyyy-mm-dd",
                                       min = min(dx$dateRep),
                                       max = max(dx$dateRep)
                                     ),
                                     checkboxInput("checkSmooth", label = "Smoothed conditional mean", value = FALSE),
                                     checkboxInput("checkConfidenceInterval", label = "Show confidence interval", value = FALSE),
                                     checkboxInput("casesp100", label = "New cases per 100,000", value = FALSE),
                                     checkboxInput("casespm", label = "New cases per million", value = FALSE),
                                     checkboxGroupInput(
                                       "checkCountries",
                                       label = h5("Select countries"),
                                       choices = cl,
                                       selected = c(1, 7, 11, 13, 17, 23, 26)
                                     )
                        ),
                        
                        mainPanel(width = 10,
                                  tabsetPanel(id = "tabs",
                                              tabPanel("ECDPC Highcharts plot", 
                                                       highchartOutput("covid_hc_plot", height = 600),
                                                       h4("Data source:"),
                                                       p("European Centre for Disease Prevention and Control"),
                                                       a("www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                                                         href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"
                                                       ),
                                                       br(),br(),
                                                       p("Data available until  ", max(dx$dateRep)),
                                                       br(),
                                                       p("Prepared with Highcharts"),
                                                       a("www.highcharts.com/",
                                                         href = "https://www.highcharts.com/")),
                                              tabPanel("ECDPC Highcharts bar plot", 
                                                       highchartOutput("covid_hc_barplot", height = 600),
                                                       h4("Data source:"),
                                                       p("European Centre for Disease Prevention and Control"),
                                                       a("www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                                                         href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"
                                                       ),
                                                       br(),br(),
                                                       p("Data available until  ", max(dx$dateRep)),
                                                       br(),
                                                       p("Prepared with Highcharts"),
                                                       a("www.highcharts.com/",
                                                         href = "https://www.highcharts.com/")),
                                              tabPanel("ECDPC table", dataTableOutput("covidTable")),
                                              tabPanel("ECDPC ggplot",
                                                       plotOutput("covidPlot", height = 600),
                                                       h4("Data source:"),
                                                       p("European Centre for Disease Prevention and Control"),
                                                       a(
                                                         "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                                                         href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"
                                                       ),
                                                       br(),br(),
                                                       p("Data available until  ", max(dx$dateRep)))
                                  )
                        )
                    )
  
            )
            
  )
)

server <- function(input, output, session) {
  
  dp <- reactive({
    req(input$sdate, input$edate, input$checkCountries)
    
    dx %>%
      filter(id_country %in% input$checkCountries) %>%
      mutate(
        Date = as.Date(dateRep),
        cases_per_million = round(cases / (popData2019 / 1000000), 0),
        cases_per_100k = round(cases / (popData2019 / 100000), 0),
        Country = countriesAndTerritories
      ) %>%
      select(Date,
             Country,
             cases,
             deaths,
             popData2019,
             cases_per_million,
             cases_per_100k) %>%
      filter(complete.cases(.)) %>%
      arrange(Date) %>%
      group_by(Country) %>%
      summarise(
        Date,
        cases,
        case_sum = cumsum(cases),
        cases_per_million,
        cases_per_100k
      ) %>% 
      filter(Date >= input$sdate & Date <= input$edate) %>% 
      ungroup()
    
  })
  
  dpw <- reactive({
    req(input$sdate_w, input$edate_w, input$checkCountries_w)
    
    dw <- dw %>% 
      filter(indicator == "cases") %>% 
      filter(id_country %in% input$checkCountries_w) %>% 
      select(-id_country) %>% 
      mutate(cases_per_million = round(weekly_count / (popData2019 / 1000000), 0),
             cases_per_100k = round(weekly_count / (popData2019 / 100000), 0)) %>% 
      filter(Date >= input$sdate_w & Date <= input$edate_w)
    
  })
    
  output$covidPlot <- renderPlot({
    if(input$casespm) {
      p <- ggplot(data = dp(),
                  aes(x = Date, y = cases_per_million, color = Country)) +
          geom_line() +
          ylab("New cases per million per day") +
          scale_y_continuous(breaks = round(seq(0, max(dp()$cases_per_million), by = 50), 0), limits = c(0,NA))
    } else if(input$casesp100) {
      p <- ggplot(data = dp(),
                  aes(x = Date, y = cases_per_100k, color = Country)) +
        geom_line() +
        geom_hline(yintercept = 75, color = "black", linetype = "dashed") +
        geom_hline(yintercept = 70, color = "black", linetype = "dashed") +
        annotate("text", x = input$sdate + 2 , y = 70, vjust = -0.5, color = "black",
                 label = "Poland - lockdown limit, 7 days average") +
        geom_hline(yintercept = 50, color = "red", linetype = "dashed") +
        annotate("text", x = input$sdate + 2 , y = 50, vjust = -1, color = "red",
                 label = "Poland - red zone upper limit, 7 days average") +
        geom_hline(yintercept = 25, color = "red", linetype = "dashed") +
        annotate("text", x = input$sdate + 2 , y = 25, vjust = -1, color = "red",
                 label = "Poland - red zone lower limit, 7 days average") +
        geom_hline(yintercept = 10, color = "#ffcc00", linetype = "dashed") +
        annotate("text", x = input$sdate + 2 , y = 10, vjust = -1, color = "#ffcc00",
                 label = "Poland - yellow zone limit, 7 days average") +
        ylab("New cases per 100,000 per day") +
        scale_y_continuous(breaks = round(seq(0, max(dp()$cases_per_100k), by = 10), 0), limits = c(0,NA))
    } else {
      if(max(dp()$cases) - min(dp()$cases) > 4000 & max(dp()$cases) - min(dp()$cases) < 10000) {
        by_ticks = 1000
      } else if(max(dp()$cases) - min(dp()$cases) > 10000) {
        by_ticks = 2500
      } else {
        by_ticks = 250
      }
      p <- ggplot(data = dp(),
                  aes(x = Date, y = cases, color = Country)) + 
          geom_line() + 
          ylab("New cases") +
          scale_y_continuous(breaks = round(seq(min(dp()$cases), max(dp()$cases), by = by_ticks), 0), limits = c(0,NA))
    }
    
    # add common part ---------------------------------------------------------
    if((input$edate - input$sdate) <= 30) {
      p <- p + scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d")
    } else {
      p <- p + scale_x_date(date_breaks = "2 week", date_labels = "%Y-%m")
    }
    p <- p + theme_gdocs() +
      scale_fill_manual(values = dcolors) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      xlab("Date") +
      scale_color_manual(values = dcolors)
      
    if(input$checkSmooth){
      p <- p + geom_smooth(method = lm, formula = y ~ splines::bs(x, 6),
                          se = input$checkConfidenceInterval)
    }
    
    p
    
  })
  
  output$covidTable <- renderDataTable({
      dt <- dp()
      names(dt) <- c("Country", "Date", "New cases", "Sum of all cases per country", 
                     "New cases per million", "New cases per 100,000")
      dt[,5] <- round(dt[,5], 2)
      dt[,6] <- round(dt[,6], 2)
      dt
      }
  )
   

  
  output$covid_hc_plot <- renderHighchart({
    if(input$casespm) {
      dhc <- dp() %>%
        group_by(Country) %>%
        do(dhc = list(
          data = list_parse2(data.frame(.$Date, .$cases_per_million))
        )) %>%
        {map2(.$Country, .$dhc, function(x, y){
          append(list(name = x), y)
        })}
      y_text <- "New cases per million per day"      
    } else if(input$casesp100) {
      dhc <- dp() %>%
        group_by(Country) %>%
        do(dhc = list(
          data = list_parse2(data.frame(.$Date, .$cases_per_100k))
        )) %>%
        {map2(.$Country, .$dhc, function(x, y){
          append(list(name = x), y)
        })}
      y_text <- "New cases per 100,000 per day"      
    } else {
      dhc <- dp() %>%
        group_by(Country) %>%
        do(dhc = list(
          data = list_parse2(data.frame(.$Date, .$cases))
        )) %>%
        {map2(.$Country, .$dhc, function(x, y){
          append(list(name = x), y)
        })}
      y_text <- "New cases"
    }
  
    if(input$casesp100){
      highchart() %>% 
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>% 
        hc_add_series_list(dhc) %>%
        hc_xAxis(categories = unique(dp()$Date)) %>% 
        hc_yAxis(title = list(text = y_text),
                 plotLines = list(list(value = 75, color = "black", width = 2, dashStyle = "shortdash"),
                                  list(value = 70, color = "black", width = 2, dashStyle = "shortdash",
                                       label = list(text = "Poland - lockdown limit, 7 days average",
                                                    style = list(color = "black", fontWeight = "bold"))),
                                  list(value = 50, color = "red", width = 2, dashStyle = "shortdash",
                                       label = list(text = "Poland - red zone upper limit, 7 days average",
                                                    style = list(color = "red", fontWeight = "bold"))),
                                  list(value = 25, color = "red", width = 2, dashStyle = "shortdash",
                                       label = list(text = "Poland - red zone lower limit, 7 days average",
                                                    style = list(color = "red", fontWeight = "bold"))),
                                  list(value = 10, color = "#ffcc00", width = 2, dashStyle = "shortdash",
                                       label = list(text = "Poland - yellow zone limit, 7 days average",
                                                    style = list(color = "#ffcc00", fontWeight = "bold"))))
                 ) %>% 
        hc_tooltip(table = TRUE,
                   sort = TRUE) %>% 
        hc_colors(dcolors)
    } else {
      highchart() %>% 
        hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>% 
        hc_add_series_list(dhc) %>%
        hc_xAxis(categories = unique(dp()$Date)) %>% 
        hc_yAxis(title = list(text = y_text)) %>% 
        hc_tooltip(table = TRUE,
                   sort = TRUE) %>% 
        hc_colors(dcolors)
    }
    
  })

  output$covid_hc_barplot <- renderHighchart({
    
    

    # change summary to months ------------------------------------------------
    if((input$edate - input$sdate) > 30) {
      dbarp <- dp() %>% 
        mutate(nmonth = month(Date),
               nyear = year(Date)) %>% 
        group_by(Country, nmonth, nyear) %>% 
        summarise(case_sum = max(case_sum)) %>% 
        ungroup() %>% 
        mutate(Date = as.Date(make_datetime(year = nyear, month = nmonth, day = 1)))
        hchart(dbarp, 'column', hcaes(x = Date, y = case_sum, group = Country)) %>% 
          hc_xAxis( title = list(text = "<b>Date</b>"), type = "datetime") %>% 
          hc_yAxis(title = list(text = "Sum of all cases per country")) %>%
          hc_colors(dcolors)
        
    } else {
      hchart(dp(), 'column', hcaes(x = Date, y = case_sum, group = Country)) %>% 
        hc_xAxis( title = list(text = "<b>Date</b>"), type = "datetime") %>% 
        hc_yAxis(title = list(text = "Sum of all cases per country")) %>%
        hc_colors(dcolors)
      
    }
  })
    
  # Mon Mar  8 21:27:44 2021 ------------------------------
  # weekly table
  output$covidTable_w <- renderDataTable({
    dt <- dpw()
    # names(dt) <- c("Country", "Date", "New cases", "Sum of all cases per country", 
    #                "New cases per million", "New cases per 100,000")
    # dt[,5] <- round(dt[,5], 2)
    # dt[,6] <- round(dt[,6], 2)
    dt
  }
  )
  
  # Sat Mar 20 20:50:55 2021 ------------------------------
  output$new_weekly_cases <- renderPlot({
    #dw_p <- dpw()
    if(input$casespm_w) {
      p <- ggplot(data = dpw(),
                  aes(x = Date, y = cases_per_million, color = Country)) +
        geom_line() +
        ylab("New weekly cases per million per day") +
        scale_y_continuous(breaks = round(seq(0, max(dpw()$cases_per_million), by = 2000), 0), limits = c(0,NA))
    } else if(input$casesp100_w) {
      p <- ggplot(data = dpw(),
                  aes(x = Date, y = cases_per_100k, color = Country)) +
        geom_line() +
        ylab("New weekly cases per 100,000 per day") +
        scale_y_continuous(breaks = round(seq(0, max(dpw()$cases_per_100k), by = 100), 0), limits = c(0,NA))
    } else {
      p <- ggplot(data = dpw(),
                  aes(x = Date, y = weekly_count, color = Country)) +
        geom_line() +
        ylab("New weekly cases") +
        scale_y_continuous(breaks = round(seq(0, max(dpw()$weekly_count), by = 10000), 0), limits = c(0,NA))
    }
    
    # add common part ---------------------------------------------------------
    if((input$edate_w - input$sdate_w) <= 30) {
      p <- p + scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d")
    } else {
      p <- p + scale_x_date(date_breaks = "2 week", date_labels = "%Y-%m-%d")
    }
    p <- p + theme_gdocs() +
      scale_fill_manual(values = dcolors) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      xlab("Date") +
      scale_color_manual(values = dcolors)
    
    if(input$checkSmooth_w){
      p <- p + geom_smooth(method = lm, formula = y ~ splines::bs(x, 6),
                           se = input$checkConfidenceInterval_w)
    }
    
    p
  })
  

  observe({
    if(input$tabs == "ECDPC Highcharts plot") {
      updateCheckboxInput(session, inputId = "checkSmooth", value = FALSE)
      updateCheckboxInput(session, inputId = "checkConfidenceInterval", value = FALSE)
      shinyjs::disable("checkSmooth")
      shinyjs::disable("checkConfidenceInterval")
      shinyjs::enable("casespm")
      shinyjs::enable("casesp100")
    } else if(input$tabs == "ECDPC ggplot") {
      shinyjs::enable("checkSmooth")
      shinyjs::enable("checkConfidenceInterval")
      shinyjs::enable("casespm")
      shinyjs::enable("casesp100")
    } else if(input$tabs == "ECDPC Highcharts bar plot"){
      updateCheckboxInput(session, inputId = "checkSmooth", value = FALSE)
      updateCheckboxInput(session, inputId = "checkConfidenceInterval", value = FALSE)
      updateCheckboxInput(session, inputId = "casespm", value = FALSE)
      updateCheckboxInput(session, inputId = "casesp100", value = FALSE)
      shinyjs::disable("checkSmooth")
      shinyjs::disable("checkConfidenceInterval")
      shinyjs::disable("casespm")
      shinyjs::disable("casesp100")
    }
  })

  observeEvent(eventExpr = input$casespm, {
    if(input$casespm) {
      updateCheckboxInput(session, inputId = "casesp100", value = FALSE)
    }
  })

  observeEvent(eventExpr = input$casesp100, {
    if(input$casesp100) {
      updateCheckboxInput(session, inputId = "casespm", value = FALSE)
    }
  })
  
    # Disable buttons at start
  shinyjs::disable("checkSmooth")
  shinyjs::disable("checkConfidenceInterval")
  
  # End application after closing a window or tab ---------------------------
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
