library(DT)
library(ggthemes)
library(shiny)
library(tidyverse)

# Data from ---------------------------------------------------------------
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
dx <-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
              na.strings = "",
              fileEncoding = "UTF-8-BOM")

# change date format ------------------------------------------------------
dx$dateRep <- as.Date(dx$dateRep, format = "%d/%m/%Y")


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
dx <- dx %>%
  left_join(dcl, by = "countriesAndTerritories") %>%
  filter(!is.na(id_country)) %>%
  filter(cases >= 0)
rm(dcl)

# colors ------------------------------------------------------------------
dcolors <- c("red", "cyan", "green", "orange", "darkblue",
             "darkgreen", "gold", "pink", "lightgray", "limegreen",
             "magenta", "navyblue", "mistyrose", "orchid", "purple",
             "blue", "yellow", "turquise", "tomato", "sienna",
             "seagreen", "violeta", "mintcream", "gray", "darksalmon",
             "deeppink", "brown", "bisque")

ui <- fluidPage(titlePanel("COVID-19 cases in Poland"),
                sidebarLayout(
                  sidebarPanel(
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
                    checkboxInput("checkRawData", label = "Show raw data", value = TRUE),
                    checkboxInput("checkSmooth", label = "Smoothed conditional mean", value = TRUE),
                    checkboxInput("checkConfidenceInterval", label = "Show confidence interval", value = FALSE),
                    checkboxInput("casespm", label = "New cases per million", value = FALSE),
                    checkboxInput("logScale", label = "Logarithmic scale", value = FALSE),
                    checkboxGroupInput(
                      "checkCountries",
                      label = h4("Select countries"),
                      choices = cl,
                      selected = c(1, 2, 7, 13, 16, 17, 23, 26)
                    )
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("ECDPC plot", plotOutput("covidPlot", height = 600),
                               h4("Data source:"),
                               p("European Centre for Disease Prevention and Control"),
                               a(
                                 "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                                 href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"
                               ),
                               br(),br(),
                               p("Data last updated on : ", max(dx$dateRep))),
                      tabPanel("ECDPC table", dataTableOutput("covidTable"))
                    )
                  )
))

server <- function(input, output, session) {
  dp <- reactive({
    req(input$sdate, input$edate, input$checkCountries)
    
    dx %>%
      filter(id_country %in% input$checkCountries) %>%
      mutate(
        Date = as.Date(dateRep),
        cases_per_million = cases / (popData2018 / 1000000),
        Country = countriesAndTerritories
      ) %>%
      select(Date,
             Country,
             cases,
             deaths,
             popData2018,
             cases_per_million) %>%
      filter(complete.cases(.)) %>%
      arrange(Date) %>%
      group_by(Country) %>%
      summarise(
        Date,
        cases,
        case_sum = cumsum(cases),
        cases_per_million,
        cases_per_million_sum = cumsum(cases_per_million)
      ) %>% 
      filter(Date > input$sdate & Date <= input$edate)
  })
  
  output$covidPlot <- renderPlot({
    if (input$logScale & input$casespm) {
      p <-
        ggplot(data = dp(), 
               aes(x = Date, y = log(cases_per_million), color = Country))
    } else if (!input$logScale & input$casespm) {
      p <-
        ggplot(data = dp(),
               aes(x = Date, y = cases_per_million, color = Country))
    } else if (input$logScale & !input$casespm) {
      p <-
        ggplot(data = dp(), 
               aes(x = Date, y = log(cases), color = Country))
    } else if (!input$logScale & !input$casespm) {
      p <-
        ggplot(data = dp(), 
               aes(x = Date, y = cases, color = Country))
    }

    # add common part ---------------------------------------------------------
    p <- p + scale_x_date(date_breaks = "2 week", date_labels = "%Y-%m") +
      theme_gdocs() +
      scale_fill_manual(values = dcolors) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      xlab("Date") +
      ylab("log(New cases per million per day)") +
      scale_color_manual(values = dcolors)
    
    # additional part for standard plot ---------------------------------------
    if (!input$logScale & !input$casespm) {
      p <- p + scale_y_continuous(breaks = round(seq(
        min(dp()$cases), max(dp()$cases), by = 250), 0),
        limits = c(0,NA))
    }

    if(input$checkRawData) {
      p <- p + geom_line()
    }
    
    if(input$checkSmooth){
      p <- p+ geom_smooth(method = lm, formula = y ~ splines::bs(x, 6),
                          se = input$checkConfidenceInterval)
    }
    
    p
    
  })
  
  output$covidTable <- renderDataTable({
    dt <- dp()
    names(dt) <- c("Country", "Date", "New cases", "Sum of new cases", "New cases per million", "Sum of new cases per million")
    dt[,5] <- round(dt[,5], 2)
    dt[,6] <- round(dt[,6], 2)
    dt
  }, rownames = FALSE)
  
  # End application after closing a window or tab ---------------------------
  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
