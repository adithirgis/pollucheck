library(shiny)
library(Cairo)
library(DT)
library(data.table)
library(dplyr)
library(ggplot2)
library(htmltools)
library(tidyverse)
library(lubridate)
library(zoo)
library(caTools)
library(xts)
library(readr)
library(openair)
library(xlsx)
library(nortest)
library(janitor)
library(recipes)

ui <- fluidPage(
  tags$style('.container-fluid {
                             background-color: #ffece9;
              }'),
  h1("PolluCheck - Analyse open source air quality data"),
  tags$head(
    tags$style(HTML(".well {
                    height : 10vh; overflow-y : auto; font-size : 13px; 
                    background-color: #ffece9;
                    }" ,
                    ".shiny-output-error-validation {
                    color : red; font-size : 14px;
                    }"
    ))),
  sidebarLayout(absolutePanel(fixed = TRUE, draggable = TRUE,
                             conditionalPanel(condition = "input.tabs1 == 3",
                                              tags$hr(),
                                              selectInput("palleInp", "Plot this parameter",
                                                          multiple = FALSE, 
                                                          "Select"),
                                              tags$hr(),
                                              radioButtons("avg_hour3", "Which values",
                                                           c("Hourly" = "hour3",
                                                             "Daily" = "daily3"), 
                                                           selected = "hour1"),
                                              tags$hr(),
                                              actionButton("ts", "Time Series"),
                                              textInput("ts_mt", label = "Edit time series title", 
                                                        value = "Title"),
                                              textInput("ts_y", label = "Edit time series y axis", 
                                                        value = "Pollutant"),
                                              tags$hr(),
                                              actionButton("box", "Monthly Box Plot"),
                                              textInput("box_mt", label = "Edit title", 
                                                        value = "Title"),
                                              textInput("box_y", label = "Edit y axis", 
                                                        value = "Pollutant"),
                                              tags$hr(),
                                              actionButton("diurnal", "Diurnal Plot"),
                                              textInput("diurnal_mt", label = "Edit Diurnal pattern title", 
                                                        value = "Title"),
                                              textInput("diurnal_y", label = "Edit Diurnal pattern y axis", 
                                                        value = "Pollutant"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 5",
                                              tags$hr(),
                                              selectInput("palleInp2", "Plot this parameter",
                                                          multiple = FALSE, "Select"),
                                              tags$hr(),
                                              radioButtons("avg_hour2", "Which values",
                                                           c("Hourly" = "hour2",
                                                             "Daily" = "daily2"), 
                                                           selected = "hour1"),
                                              tags$hr(),
                                              radioButtons("normality", "Normality Test",
                                                           c("Anderson-Darling" = "ad",
                                                             "Shapiro-Wilk" = "sh"), 
                                                           selected = "ad"),
                                              tags$hr(),
                                              actionButton("freq", "Density Plot"),
                                              textInput("freq_mt", label = "Edit Density plot title", 
                                                        value = "Title"),
                                              textInput("freq_x", label = "Edit Density plot x axis", 
                                                        value = "Pollutant"),
                                              tags$hr(),
                                              actionButton("qq", "QQ Plot"),
                                              textInput("qq_mt", label = "Edit QQ plot title", 
                                                        value = "Title"),
                                              textInput("qq_y", label = "Edit QQ plot y axis", 
                                                        value = "Pollutant"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 2",
                                              tags$hr(),
                                              selectInput("avg",
                                                          label = "Averaging period",
                                                          c("None" = "no",
                                                            "Daily Statistics" = "day",
                                                            "Month-Year Statistics" = "month",
                                                            "Monthly Statistics" = "monthly"),
                                                          multiple = FALSE, 
                                                          selected = "None"),
                                              tags$hr(),
                                              downloadButton('download_stats', "Download as csv")),
                             conditionalPanel(condition = "input.tabs1 == 6",
                                              tags$hr(),
                                              selectInput("DepVar", 
                                                          "Dependent Variable", 
                                                          multiple = FALSE, "Select"),
                                              selectInput("InDepVar", 
                                                          "Independent Variable", 
                                                          multiple = FALSE, "Select"),
                                              tags$hr(),
                                              radioButtons("avg_hour1", "Which values",
                                                           c("Hourly" = "hour1",
                                                             "Daily" = "daily1"), 
                                                           selected = "hour1"),
                                              tags$hr(),
                                              actionButton("reg", "Linear Regression Plot"),
                                              textInput("reg_mt", label = "Edit regression title", 
                                                        value = "Title"),
                                              textInput("reg_x", label = "Edit regression x axis", 
                                                        value = "Pollutant"),
                                              textInput("reg_y", label = "Edit regression y axis", 
                                                        value = "Pollutant"),
                                              tags$hr(),
                                              selectInput("DepVar1", 
                                                          "Dependent Variable", 
                                                          multiple = FALSE, "Select"),
                                              selectInput("InDepVar1", 
                                                          "Independent Variable(s)", 
                                                          multiple = TRUE, "Select"),
                                              tags$hr(),
                                              actionButton("mulreg", "Multiple Regression"),),
                             conditionalPanel(condition = "input.tabs1 == 7"),
                             conditionalPanel(condition = "input.tabs1 == 4",
                                              tags$hr(),
                                              selectInput("palleInp1", "Plot this parameter",
                                                          multiple = FALSE, "Select"),
                                              tags$hr(),
                                              radioButtons("avg_hour4", "Which values",
                                                           c("Hourly" = "hour4",
                                                             "Daily" = "daily4"), 
                                                           selected = "hour1"),
                                              tags$hr(),
                                              actionButton("cp", "Calendar Plot"),
                                              textInput("cp_mt", label = "Edit Calendar plot title", 
                                                        value = "Title"),
                                              tags$hr(),
                                              radioButtons("stat_tv", "Statistic to plot the graph",
                                                           c("Median and quantiles" = "median",
                                                             "Mean and 95% confidence intervals" = "mean"), 
                                                           selected = "mean"),
                                              actionButton("tv", "Time Variation graphs"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 1",
                                              tags$hr(),
                                              radioButtons("type", "Data downloaded from",
                                                           choiceNames = list(
                                                             HTML("<a href = 'https://openaq.org/#/countries/IN?_k=5ecycz' target = '_blank'>OpenAQ</a>"), 
                                                             HTML("<a href = 'https://www.airnow.gov/international/us-embassies-and-consulates/#India' target = '_blank'>AirNow - US Embassies</a>"),
                                                             HTML("<a href = 'https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing' target = '_blank'>Pollution Control Board</a>")),
                                                           choiceValues = list("oaq", "an", "cpcb"),
                                                           selected = "cpcb"),
                                              tags$hr(),
                                              conditionalPanel(condition = "input.type == 'cpcb'",
                                              radioButtons("file", "Time resolution",
                                                           c("15 minutes" = "15 min",
                                                             "30 minutes" = "30 min",
                                                             "60 minutes" = "60 min"), 
                                                           selected = "60 min")),
                                              tags$hr(),
                                              fileInput("file1",
                                                        "Add data",
                                                        multiple = TRUE,
                                                        accept = c('.xlsx', '.csv')),
                                              tags$hr(),
                                              checkboxInput('remove_9', 'Remove negative values'),
                                              tags$hr(),
                                              checkboxInput('repeated', 'Remove consecutive repeated measurements'),
                                              tags$hr(),
                                              checkboxInput('exclude', 'Remove outliers based on Mean and Std Dev'),
                                              conditionalPanel(
                                                condition = "input.exclude == true",
                                                numericInput("ey", "Specify a multiple for removing outliers (Mean + X*Std Dev)",
                                                             value = 3)),
                                              tags$hr(),
                                              checkboxInput('percent', 'Completeness of data in a day'),
                                              conditionalPanel(
                                                condition = "input.percent == true",
                                                numericInput("per", "Specify % of data completeness required in a day - 24 hours",
                                                             value = 75)),
                                              tags$hr(),
                                              numericInput("high_number",
                                                           "Remove PM2.5 and PM10 values above",
                                                           value = 9999),
                                              tags$hr(),
                                              radioButtons("avg_hour", "Which values",
                                                           c("Hourly" = "hour",
                                                             "Daily" = "daily"), 
                                                           selected = "hour"),
                                              tags$hr(),
                                              actionButton("hourly", "Show Data"),
                                              downloadButton('download', "Download as csv"),
                                              tags$hr())),
                mainPanel(
                  tags$a(img(src = 'logo.png', align = "right", height = 70,
                             width = 70),
                         href = "https://www.ilklabs.com/", target = "_blank"),
                  tags$head(
                    tags$style(type = 'text/css',
                               ".nav-tabs {
                               font-size: 17px
                               } ")),
                  tabsetPanel(id = "tabs1",
                              tabPanel(value = 1,
                                       title = "File",
                                       dataTableOutput("table1")),
                              tabPanel(
                                value = 2,
                                title = "Summary",
                                dataTableOutput("table")), 
                              tabPanel(
                                value = 3,
                                title = "Plots",
                                plotOutput("plot1", width = 800),
                                plotOutput("plot3", width = 800),
                                plotOutput("plot2", width = 800)),
                              tabPanel(
                                value = 5,
                                title = "Statistics Plots",
                                verbatimTextOutput("normality_test"),
                                plotOutput("plot6", width = 800),
                                plotOutput("plot7", width = 800)),
                              tabPanel(
                                value = 6,
                                title = "Linear Regression",
                                plotOutput("plot8", width = 800), 
                                verbatimTextOutput("RegOut"),
                                verbatimTextOutput(outputId = "IndPrint"),
                                verbatimTextOutput(outputId = "DepPrint")),
                              tabPanel(
                                value = 4,
                                title = "openair package plots",
                                plotOutput("plot5", height = 600),
                                plotOutput("plot4", height = 600)),
                              tabPanel(
                                value = 7,
                                title = "Read Me",
                                textOutput("text")))
  )))


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2, shiny.launch.browser = TRUE)
  
  CPCB_f <- reactive({
    completeFun <- function(data, desiredCols) {
      completeVec <- complete.cases(data[, desiredCols])
      return(data[completeVec, ])
    }
    ### Function to check for Mean+3SD and Mean-3SD; caution needs to have all 
    # the columns without NA values
    LLD <- function(x, y, z, ey) {
      if (is.na(x) || is.nan(x) || is.nan(y) || is.na(y) || is.na(z) || is.nan(z)) {
        return (NA)
      }
      else if (x > (y + (ey * z)) || x < (y - (ey * z))) {
        return (NA)
      }
      else if (is.null(x) || x == '' || is.null(y) || y == '' || is.null(z) || z == '') {
        return (NA)
      } else if(x < (y + (ey * z)) || x > (y - (ey * z)))
      {
        return (x)
      }
    }
    per1 <- input$per
    date_ts <- function(file, time_period) {
      ye <- format(file[1, "date"], format = "%Y")
      x1 <- as.POSIXct(paste0(ye, "-01-01 01:00:00"), 
                       format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
      ye <- format(tail(file$date, n = 3)[1], format = "%Y")
      x2 <- as.POSIXct(paste0(ye, "-12-31 23:00:00"), 
                       format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
      date <- seq(
        from = as.POSIXct(x1, tz = "Asia/Kolkata"),
        to = as.POSIXct(x2, tz = "Asia/Kolkata"),
        by = time_period
      ) 
      date
      return(date)
    }
    if (is.null(input$file1)) {
      return(NULL)
    } else {
      if(input$type == "cpcb") {
      trial <- read.xlsx2(input$file1$datapath, 1, startRow = 17)
      trial$date <- gsub(":00", ":00:00", trial$To.Date, fixed = TRUE)
      ### This folder contains files with different columns on below other so 
      # splitting it based on empty rows after a set of parameters
      trial$tbl_id <- cumsum(!nzchar(trial$date))
      trial <- trial[nzchar(trial$date), ]
      trial[ , c('From.Date', 'To.Date')] <- list(NULL)
      dt_s <- split(trial[, -ncol(trial)], trial$tbl_id)
      
      ### Three dataframes representing different parameters; Also the start and 
      # the end date was used from each file to create a time series dataframe
      PM <- data.frame(dt_s$`0`) %>%
        mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', tz = "Asia/Kolkata"))
      date <- date_ts(PM, input$file)
      tseries_df <- data.frame(date)
      ### Join the three idfferent dataframe into a single one
      site1_join <- left_join(tseries_df, PM, by = "date")
      
      make_df <- function(y, tseries_df) {
        df <- data.frame(y)
        if(!nrow(df))
        {
          df <- tseries_df
        } else {
          names(df) <- as.matrix(df[1, ])
          df <- df[-1, ]
          df[] <- lapply(df, function(x) type.convert(as.character(x)))
          df <- Filter(function(x)! all(is.na(x)), df)
          df <- df %>%
            dplyr::select("date" = `To Date`, everything()) %>%
            mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', 
                                     tz = "Asia/Kolkata"))
        }
      }
      Ben <- make_df(dt_s$`2`, tseries_df)
      Beny <- make_df(dt_s$`4`, tseries_df)
      Bent <- make_df(dt_s$`6`, tseries_df)
      all <- list(site1_join, Ben, Beny, Bent) %>% reduce(left_join, by = "date")
      } 
      site1_join_f1 <- all %>%
        mutate(day = as.Date(date, format = '%Y-%m-%d', tz = "Asia/Kolkata")) %>%
        dplyr::select(date, day, everything())
      
     
      ### PM2.5 and PM10 ratio
      if("PM2.5" %in% colnames(site1_join_f1)) {
        
        site1_join_f1$PM2.5 <- ifelse(as.numeric(as.character(site1_join_f1$PM2.5)) > input$high_number, 
                                      as.numeric(as.character(NA)), as.numeric(as.character(site1_join_f1$PM2.5)))
        
        ### If PM10 values exist then check the rario of PM2.5 / PM10 and if it is 
        # greater than 1 then remove those values
        if("PM10" %in% colnames(site1_join_f1))
        {
          b <- as.numeric(as.character(site1_join_f1$PM10)) > input$high_number
          site1_join_f1$PM10 <- ifelse(b, as.numeric(as.character(NA)), as.numeric(as.character(site1_join_f1$PM10)))
          site1_join_f1$ratio <- as.numeric(as.character(site1_join_f1$PM2.5)) / as.numeric(as.character(site1_join_f1$PM10))
          site1_join_f1$PM2.5 <- ifelse(site1_join_f1$ratio >= 1, as.numeric(as.character(NA)), as.numeric(as.character(site1_join_f1$PM2.5)))
          site1_join_f1$PM10 <- ifelse(site1_join_f1$ratio >= 1, as.numeric(as.character(NA)), as.numeric(as.character(site1_join_f1$PM10)))
        } else {
          site1_join_f1$ratio <- NA
        }
      } else { site1_join_f1 }
      site1_join_f1 <- site1_join_f1 %>%
        dplyr::select(date, day, everything()) %>%
        janitor::remove_empty("cols")
      site1_join_f1
    }
  })
  
  data_joined <- eventReactive(input$hourly, {
    data <- CPCB_f()
    if(input$avg_hour == "daily") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })

  
  
  observe({
    if (is.null(input$file1)) {
      NULL
    } else {
      data_joined <- data_joined()
      data_joined <- data_joined %>%
        dplyr::select(- date, - day)
    }
    updateSelectInput(session, "palleInp", choices = names(data_joined))
    updateSelectInput(session, "palleInp1", choices = names(data_joined))
    updateSelectInput(session, "palleInp2", choices = names(data_joined))
    updateSelectInput(session, "DepVar", choices = names(data_joined))
    updateSelectInput(session, "InDepVar", choices = names(data_joined))
    updateSelectInput(session, "DepVar1", choices = names(data_joined))
    updateSelectInput(session, "InDepVar1", choices = names(data_joined))
  })
  
  output$table1 <- DT::renderDataTable({
    data_joined <- data_joined() 
    cols <- names(data_joined)[3:ncol(data_joined)]
    data_joined[ , cols] <- sapply(X = data_joined[ , cols], 
                                   FUN = function(x) as.numeric(as.character(x)))
    setDT(data_joined)
    data_joined[,(cols) := round(.SD, 2), .SDcols = cols]
    if(input$avg_hour == "daily") {
      data_joined <- data_joined %>%
        mutate(date = as.Date(date, tz = "Asia/Kolkata")) %>%
        dplyr::select(date, everything(), - day)
      datatable(data_joined, options = list("pageLength" = 25)) %>% formatDate(1, "toLocaleDateString")
    } else { datatable(data_joined, options = list("pageLength" = 25)) %>% formatDate(1, "toLocaleString") }
  })
  
  output$download <- downloadHandler(
    filename <- function() {"data.csv"},
    content <- function(fname) {
      data_joined <- data_joined()
      write.csv(data_joined, fname)
    })
  
  
  
  
  
}
## Run app
shinyApp(ui, server)


# runApp(display.mode = "showcase")



