library(shiny)
library(Cairo)
library(openair)
library(tidyverse)
library(dplyr)
library(bslib)
library(forecast)
library(biwavelet)
library(readxl)
library(DT)
library(ggplot2)
library(data.table)
library(janitor)
library(nortest)
library(zoo)



ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "pulse"), 
  h1("PolluCheck - Analyse open source air quality data of India"),
  tags$head(
    tags$style(HTML(".sidebar {
                    height : 10vh; overflow-y : auto; font-size : 13px; 
                    background-color: #ffece9;
                    }" ,
                    ".shiny-output-error-validation {
                    color : red; font-size : 14px;
                    }"
    ))),
  sidebarLayout(position = "left",
                sidebarPanel(width = 3,
                             conditionalPanel(condition = "input.tabs1 == 3",
                                              tags$hr(),
                                              actionButton("da", "Data availability plot"),
                                              tags$hr(),
                                              tags$hr(),
                                              selectInput("palleInp", "Select parameter",
                                                          multiple = FALSE, 
                                                          "Select"),
                                              radioButtons("avg_hour3", "Plotting data",
                                                           c("Hourly average" = "hour3",
                                                             "Daily average" = "daily3"), 
                                                           selected = "hour3"),
                                              tags$hr(),
                                              tags$hr(),
                                              actionButton("ts", "Time-series plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("ts_mt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("ts_y", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$hr(),
                                              tags$hr(),
                                              actionButton("box", "Month and Year box plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("box_mt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("box_y", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$hr(),
                                              tags$hr(),
                                              actionButton("mbox", "Monthly box plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("box_mmt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("box_my", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$hr(),
                                              tags$hr(),
                                              actionButton("boxt", "Vertical bar plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("box_mtt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("box_yt", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$hr(),
                                              tags$hr(),
                                              radioButtons("diurn", "Diurnal plot",
                                                           c("All data" = "all",
                                                             "Monthly" = "mon"), 
                                                           selected = "all"),
                                              radioButtons("diur", "Aggregation method",
                                                           c("Mean and Std Dev" = "mesd",
                                                             "Median and IQR" = "mediq"), 
                                                           selected = "mesd"),
                                              actionButton("diurnal", "Diurnal plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("diurnal_mt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("diurnal_y", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$br(),
                                              downloadButton('download_diurnal', "Download as csv"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 2",
                                              tags$hr(),
                                              selectInput("avg",
                                                          label = "Averaging period",
                                                          c("None" = "no",
                                                            "Daily" = "day",
                                                            "Monthly" = "monthly", 
                                                            "Month and Year" = "month"),
                                                          multiple = FALSE, 
                                                          selected = "None"),
                                              tags$br(),
                                              downloadButton('download_stats', "Download as csv"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 6",
                                              tags$hr(),
                                              radioButtons("avg_hour1", "Plotting data",
                                                           c("Hourly average" = "hour1",
                                                             "Daily average" = "daily1"), 
                                                           selected = "hour1"),
                                              tags$hr(),
                                              tags$hr(),
                                              selectInput("DepVar", 
                                                          "Dependent variable", 
                                                          multiple = FALSE, "Select"),
                                              selectInput("InDepVar", 
                                                          "Independent variable", 
                                                          multiple = FALSE, "Select"),
                                              actionButton("reg", "Univariate regression"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("reg_mt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("reg_x", label = "Edit X axis label", 
                                                        value = "Parameter"),
                                              textInput("reg_y", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$hr(),
                                              tags$hr(),
                                              selectInput("DepVar1", 
                                                          "Dependent variable", 
                                                          multiple = FALSE, "Select"),
                                              selectInput("InDepVar1", 
                                                          "Independent variable(s)", 
                                                          multiple = TRUE, "Select"),
                                              actionButton("mulreg", "Multivariate regression "),
                                              tags$br(),
                                              tags$br(),
                                              textInput("reg_mmt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("reg_mx", label = "Edit X axis label", 
                                                        value = "Fitted"),
                                              textInput("reg_my", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 4",
                                              tags$hr(),
                                              selectInput("palleInp1", "Select parameter",
                                                          multiple = FALSE, "Select"),
                                              tags$hr(),
                                              actionButton("cp", "Calendar Plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("cp_mt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              tags$hr(),
                                              tags$hr(),
                                              radioButtons("stat_tv", "Plot using",
                                                           c("Median and quantiles" = "median",
                                                             "Mean and 95% confidence intervals" = "mean"), 
                                                           selected = "mean"),
                                              actionButton("tv", "Time variation"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 1",
                                              tags$hr(),
                                              radioButtons("type", "Data Source",
                                                           choiceNames = list(
                                                             HTML("<a href = 'https://openaq.org/#/countries/IN?_k=5ecycz' target = '_blank'>OpenAQ</a>"), 
                                                             HTML("<a href = 'https://www.airnow.gov/international/us-embassies-and-consulates/#India' target = '_blank'>AirNow - US Embassies</a>"),
                                                             HTML("<a href = 'https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing' target = '_blank'>Pollution Control Board</a>")),
                                                           choiceValues = list("oaq", "an", "cpcb"),
                                                           selected = "cpcb"),
                                              tags$br(),
                                              test <- a("Input Timezone* (link to supported
                                             timezones)",
                                                        href = "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones",
                                                        style = "font-size:14px; ",
                                                        target = "_blank"),
                                              selectInput("timezone",
                                                          label = "",
                                                          choices = OlsonNames(),
                                                          selected = "Asia/Kolkata"),
                                              tags$hr(),
                                              conditionalPanel(condition = "input.type == 'cpcb'",
                                                               radioButtons("file", "Input data time-resolution",
                                                                            c("15 minutes" = "15 min",
                                                                              "30 minutes" = "30 min",
                                                                              "60 minutes" = "60 min"), 
                                                                            selected = "60 min")),
                                              tags$br(),
                                              fileInput("file1",
                                                        "Upload data file*",
                                                        multiple = TRUE,
                                                        accept = c('.xlsx', '.csv')),
                                              tags$hr(),
                                              checkboxInput('remove_9', 'Remove negative values'),
                                              checkboxInput('repeated', 'Remove duplicate consecutive values'),
                                              checkboxInput('exclude', 'Specify a multiple (x) to remove outliers based on Mean and SD (Mean + x * SD)'),
                                              checkboxInput('log_op', 'Use log values'),
                                              conditionalPanel(
                                                condition = "input.exclude == true",
                                                numericInput("ey", NULL,
                                                             value = 3)),
                                              checkboxInput('percent', 'Specify % of data completeness for computing daily mean values'),
                                              conditionalPanel(
                                                condition = "input.percent == true",
                                                sliderInput("per", NULL,
                                                            value = 75,  min = 35, max = 100)),
                                              numericInput("high_number",
                                                           "Remove PM2.5 and PM10 above",
                                                           value = 9999),
                                              tags$hr(),
                                              radioButtons("avg_hour", "Output aggregation",
                                                           c("Hourly average" = "hour",
                                                             "Daily average" = "daily"), 
                                                           selected = "hour"),
                                              tags$br(),
                                              actionButton("hourly", "Show Data"),
                                              downloadButton('download', "Download as csv"),
                                              tags$hr()), 
                             conditionalPanel(condition = "input.tabs1 == 7",
                                              tags$hr(),
                                              radioButtons("type2", "Data Source",
                                                           choiceNames = list(
                                                             HTML("<a href = 'https://openaq.org/#/countries/IN?_k=5ecycz' target = '_blank'>OpenAQ</a>"), 
                                                             HTML("<a href = 'https://www.airnow.gov/international/us-embassies-and-consulates/#India' target = '_blank'>AirNow - US Embassies</a>"),
                                                             HTML("<a href = 'https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing' target = '_blank'>Pollution Control Board</a>")),
                                                           choiceValues = list("oaq", "an", "cpcb"),
                                                           selected = "cpcb"),
                                              tags$br(),
                                              test <- a("Input Timezone* (link to supported
                                             timezones)",
                                                        href = "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones",
                                                        style = "font-size:14px; ",
                                                        target = "_blank"),
                                              selectInput("timezone1",
                                                          label = "",
                                                          choices = OlsonNames(),
                                                          selected = "Asia/Kolkata"),
                                              tags$hr(),
                                              conditionalPanel(condition = "input.type == 'cpcb'",
                                                               radioButtons("file12", "Input data time resolution",
                                                                            c("15 minutes" = "15 min",
                                                                              "30 minutes" = "30 min",
                                                                              "60 minutes" = "60 min"), 
                                                                            selected = "60 min")),
                                              tags$br(),
                                              fileInput("file2",
                                                        "Upload data",
                                                        multiple = TRUE,
                                                        accept = c('.xlsx', '.csv')),
                                              tags$hr(),
                                              tags$hr(),
                                              selectInput("Para", 
                                                          "Parameter to plot in original data", 
                                                          multiple = FALSE, "Select"),
                                              selectInput("Para1", 
                                                          "Parameter to plot in comparision data", 
                                                          multiple = FALSE, "Select"),
                                              actionButton("plot_values", "Plot time series"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("comp_mt", label = "Edit title of time-series plot", 
                                                        value = "Title"),
                                              textInput("comp_y", label = "Edit Y axis label of time-series plot", 
                                                        value = "Parameter"),
                                              tags$hr(),
                                              tags$hr(),
                                              actionButton("plot_val", "Scatter plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("reg_mt1", label = "Edit title of scatter plot", 
                                                        value = "Title"),
                                              textInput("reg_mx1", label = "Edit X axis of scatter plot", 
                                                        value = "Site 2"),
                                              textInput("reg_my1", label = "Edit Y axis of scatter plot", 
                                                        value = "Site 1"),
                                              tags$hr(),
                                              tags$hr(),
                                              radioButtons("diurn1", "Diurnal plot",
                                                           c("All data" = "all",
                                                             "Monthly" = "mon"), 
                                                           selected = "all"),
                                              radioButtons("diur1", "Aggregation method",
                                                           c("Mean and Std Dev" = "mesd",
                                                             "Median and IQR" = "mediq"), 
                                                           selected = "mesd"),
                                              actionButton("plot_val_di", "Diurnal plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("diurnal_mt1", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("diurnal_y1", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 5",
                                              tags$hr(),
                                              selectInput("palleInp2", "Select parameter",
                                                          multiple = FALSE, "Select"),
                                              radioButtons("avg_hour2", "Plotting data",
                                                           c("Hourly average" = "hour2",
                                                             "Daily average" = "daily2"), 
                                                           selected = "hour2"),
                                              tags$hr(),
                                              tags$hr(),
                                              radioButtons("normality", "Normality Test",
                                                           c("Anderson-Darling" = "ad",
                                                             "Shapiro-Wilk" = "sh"), 
                                                           selected = "ad"),
                                              tags$hr(),
                                              tags$hr(),
                                              actionButton("freq", "Density plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("freq_mt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("freq_x", label = "Edit X axis label", 
                                                        value = "Parameter"),
                                              tags$hr(),
                                              tags$hr(),
                                              actionButton("qq", "QQ plot"),
                                              tags$br(),
                                              tags$br(),
                                              textInput("qq_mt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("qq_y", label = "Edit Y axis label", 
                                                        value = "Parameter"),
                                              tags$hr(),
                                              tags$hr(),
                                              conditionalPanel(condition = 'input.avg_hour2 == "daily2"',
                                                               actionButton("mk", "Trend Analysis"),
                                                               tags$br(),
                                                               tags$br(),
                                                               checkboxInput('avg_month', 'Use monthly averages'),
                                                               tags$hr(),
                                                               tags$hr(),
                                                               actionButton("ta", "Periodicity Analysis"),
                                                               tags$br(),
                                                               tags$br(),
                                                               textInput("title_bi", label = "Edit title of the plot", 
                                                                         value = "Title")),
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
                                title = "Summary Plots",
                                plotOutput("plot16", width = 800),
                                plotOutput("plot1", width = 800),
                                plotOutput("plot3", width = 800),
                                plotOutput("plot9", width = 800),
                                plotOutput("plot10", width = 800),
                                plotOutput("plot2", width = 800)),
                              tabPanel(
                                value = 5,
                                title = "Statistical Plots",
                                verbatimTextOutput("normality_test"),
                                plotOutput("plot6", width = 800),
                                plotOutput("plot7", width = 800),
                                verbatimTextOutput("kendal_test"),
                                plotOutput("plot13", width = 800)),
                              tabPanel(
                                value = 6,
                                title = "Linear Regression",
                                plotOutput("plot8", width = 800), 
                                verbatimTextOutput("RegOut"),
                                plotOutput("plot11", width = 800), 
                                verbatimTextOutput(outputId = "IndPrint"),
                                verbatimTextOutput(outputId = "DepPrint")),
                              tabPanel(value = 7,
                                       title = "Compare",
                                       plotOutput("plot12", height = 600),
                                       plotOutput("plot15", height = 600),
                                       plotOutput("plot17", height = 600)),
                              tabPanel(
                                value = 4,
                                title = "`openair`",
                                plotOutput("plot5", height = 600),
                                plotOutput("plot4", height = 600)),
                              tabPanel(
                                title = "FAQ's",
                                includeMarkdown("include.md")),
                              tabPanel(
                                title = "Disclaimer",
                                helpText("We are not responsible for malfunction or miscalculation or bugs in the code.")))
                )))


server <- function(input, output, session) {
  # bs_themer()
  options(shiny.maxRequestSize = 30*1024^2, shiny.launch.browser = TRUE)
  observeEvent(input$exclude, {
    if (input$exclude) shinyjs::enable(id = "log_op")  
    else shinyjs::disable(id = "log_op") 
  })
  observeEvent(input$type, {
    if (input$type == "cpcb") shinyjs::disable(id = "timezone")  
    else shinyjs::enable(id = "timezone") 
  })
  
  #### Function to remove outliers
  LLD <- function(x, y, z, ey) {
    ifelse(x < (y + (ey * z)) && x > (y - (ey * z)), x, NA)
  }
  
  #### Function to make date time series
  date_ts <- function(file, time_period) {
    ye <- format(file[1, "date"], format = "%Y-%m")
    x1 <- as.POSIXct(paste0(ye, "-01 00:00:00"), 
                     format = '%Y-%m-%d %H:%M:%S', tz = input$timezone)
    ye <- format(tail(file$date, n = 3)[1], format = "%Y-%m-%d")
    x2 <- as.POSIXct(paste0(ye, " 23:00:00"), 
                     format = '%Y-%m-%d %H:%M:%S', tz = input$timezone)
    date <- seq(
      from = as.POSIXct(x1, tz = input$timezone),
      to = as.POSIXct(x2, tz = input$timezone),
      by = time_period
    ) 
    date
    return(date)
  }
  
  #### CPCB data overlaps hence break it and make it into a df using this function 
  #### using date and also making the first column as the column name
  make_df <- function(y, tseries_df) {
    df <- data.frame(y)
    if(!nrow(df))
    {
      df <- tseries_df
    } else {
      df <- df %>%
        select(everything(), -c(new_set, set))
      names(df) <- as.matrix(df[1, ])
      df <- df[-1, ]
      df[] <- lapply(df, function(x) type.convert(as.character(x)))
      df <- base::Filter(function(x)! all(is.na(x)), df)
      df <- df %>%
        dplyr::select("date" = `To Date`, everything()) %>%
        dplyr::mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', 
                                        tz = input$timezone))
    }
  }
  
  
  #### How to assign functions returning two variables
  ':=' <- function(lhs, rhs) {
    frame <- parent.frame()
    lhs <- as.list(substitute(lhs))
    if (length(lhs) > 1)
      lhs <- lhs[-1]
    if (length(lhs) == 1) {
      do.call(`=`, list(lhs[[1]], rhs), envir=frame)
      return(invisible(NULL)) 
    }
    if (is.function(rhs) || is(rhs, 'formula'))
      rhs <- list(rhs)
    if (length(lhs) > length(rhs))
      rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
    for (i in 1:length(lhs))
      do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
    return(invisible(NULL)) 
  }
  
  #### Function to handle cpcb data
  cpcb <- function(sf, sfd) {
    trial <- read_excel(path = sf, skip = 16, col_types = "text") 
    trial$date <- gsub(":00", ":00:00", trial$`To Date`, fixed = TRUE)
    trial <- trial %>%
      mutate(new_set = is.na(`From Date`),
             set     = cumsum(new_set)) %>%
      filter(!is.na(`To Date`))
    trial[ , c('From Date', 'To Date')] <- list(NULL)
    dt_s <- split(trial, trial$set)
    PM <- data.frame(dt_s$`0`) %>%
      dplyr::mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', tz = input$timezone))
    date <- date_ts(PM, sfd)
    tseries_df <- data.frame(date)
    site1_join <- left_join(tseries_df, PM, by = "date") %>%
      select(date, everything(), - c(new_set, set))
    Ben <- make_df(dt_s$`1`, tseries_df)
    Beny <- make_df(dt_s$`2`, tseries_df)
    Bent <- make_df(dt_s$`3`, tseries_df)
    all <- list(site1_join, Ben, Beny, Bent) %>% reduce(left_join, by = "date")
    all <- all %>%
      select(date, everything())
    col_interest <- 2:ncol(all)
    all[ , col_interest] <- sapply(X = all[ , col_interest], 
                                             FUN = function(x) as.numeric(as.character(x)))
    all[ , col_interest] <- sapply(X = all[ , col_interest], 
                                             FUN = function(x) ifelse(x == -999, NA, x))
    return(list(all, date))
  }
  
  #### Function to handle airnow data
  an <- function(sdw) {
    trial <- read.csv(sdw, header = TRUE, sep = ",", 
                      row.names = NULL)
    trial <- trial %>%
      dplyr::select("date" = Date..LT., "PM2.5" = Raw.Conc., "Valid" = QC.Name) %>%
      dplyr::mutate(date  = as.POSIXct(date, format = '%Y-%m-%d %I:%M %p', tz = input$timezone)) %>%
      dplyr::filter(Valid == "Valid")
    trial$Valid <- NULL
    date <- date_ts(trial, "60 min")
    tseries_df <- data.frame(date)
    all <- left_join(tseries_df, trial, by = "date")
    all <- all %>%
      select(date, everything())
    col_interest <- 2:ncol(all)
    all[ , col_interest] <- sapply(X = all[ , col_interest], 
                                   FUN = function(x) as.numeric(as.character(x)))
    all[ , col_interest] <- sapply(X = all[ , col_interest], 
                                   FUN = function(x) ifelse(x == -999, NA, x))
    return(list(all, date))
  }
  
  #### Function to handle openaq data
  openaq <- function(sgf) {
    trial <- read.csv(sgf, header = TRUE, sep = ",", 
                      row.names = NULL)
    trial <- trial %>%
      dplyr::select("date" = local, "parameter" = parameter, "value" = value) %>%
      dplyr::mutate(date  = as.POSIXct(date, format = '%Y-%m-%dT%H:%M:%S+05:30', tz = input$timezone))
    trial <- trial[order(trial$date), ]
    date <- date_ts(trial, "1 min")
    tseries_df <- data.frame(date)
    trial <- trial %>%
      pivot_wider(names_from = parameter, values_from = value)
    all <- left_join(tseries_df, trial, by = "date") 
    all$hour <- lubridate::ceiling_date(all$date, "hour")
    all <- all %>%
      group_by(hour) %>%
      summarise_all(funs(mean), na.rm = TRUE) %>%
      dplyr::select(everything(), - date) %>%
      dplyr::select("date" = hour, everything())
    names(all) <- toupper(names(all))
    all <- all %>%
      dplyr::select(everything(), "date" = DATE)
    if("PM25" %in% colnames(all))
    {
      all <- all %>%
        dplyr::select(date, everything(), "PM2.5" = PM25)
    }
    if("NOX" %in% colnames(all))
    {
      all <- all %>%
        dplyr::select(date, everything(), "NOx" = nox)
    }
    date <- all %>%
      select(date)
    return(list(all, date))
  }
  
  #### Function to remove negative values
  neg <- function(site1_join_f1) {
    col_interest <- 3:ncol(site1_join_f1)
    site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                             FUN = function(x) as.numeric(as.character(x)))
    site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                             FUN = function(x) ifelse(x < 0, NA, x))
    site1_join_f1
  }
  
  #### Function to remove repeated values
  repeat_ed <- function(site1_join_f1) {
    col_interest <- 3:ncol(site1_join_f1)
    site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[, col_interest], 
                                             FUN = function(j)
                                               ifelse(c(FALSE, diff(as.numeric(j), 1, 1) == 0), 
                                                      NA, j))
    site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                             FUN = function(x) 
                                               as.numeric(as.character(x)))
    site1_join_f1
  }
  
  #### Function to remove values higher than a limit
  remov_99 <- function(site1_join_f1, high_number) {
    site1_join_f1$PM2.5 <- ifelse(as.numeric(as.character(site1_join_f1$PM2.5)) > 
                                    as.numeric(as.character(high_number)), 
                                  as.numeric(as.character(NA)), 
                                  as.numeric(as.character(site1_join_f1$PM2.5)))
    site1_join_f1
  }
  
  #### Function to compute PM2.5/PM10 ratio and check it
  ratio <- function(site1_join_f1, high_number) {
    b <- as.numeric(as.character(site1_join_f1$PM10)) > as.numeric(as.character(high_number))
    site1_join_f1$PM10 <- ifelse(b, as.numeric(as.character(NA)), 
                                 as.numeric(as.character(site1_join_f1$PM10)))
    site1_join_f1$ratio <- as.numeric(as.character(site1_join_f1$PM2.5)) / 
      as.numeric(as.character(site1_join_f1$PM10))
    site1_join_f1$PM2.5 <- ifelse(site1_join_f1$ratio >= 1 & !is.na(site1_join_f1$ratio), 
                                  as.numeric(as.character(NA)), 
                                  as.numeric(as.character(site1_join_f1$PM2.5)))
    site1_join_f1$PM10 <- ifelse(site1_join_f1$ratio >= 1 & !is.na(site1_join_f1$ratio), 
                                 as.numeric(as.character(NA)), 
                                 as.numeric(as.character(site1_join_f1$PM10)))
    site1_join_f1
  }
  
  #### Function to remove outliers based on LLD function
  outlier <- function(site1_join_f1, name, date, eq) {
    site1_join_f1 <- site1_join_f1 %>%
      group_by(day) %>%
      dplyr::mutate_all(funs(mean, sd), na.rm = TRUE) %>%
      ungroup() %>%
      dplyr::select(date, day, everything(), -date_mean, -date_sd)
    tseries_df <- site1_join_f1 %>%
      dplyr::select(date)
    for(i in names(name)) {
      data_list <- site1_join_f1 %>% 
        dplyr::select(starts_with(i))
      if(i == "NO") {
        data_list <- data_list %>%
          dplyr::select(- contains(c("NO2", "NOx")))
      } else if (i == "O") {
        data_list <- data_list %>%
          dplyr::select(- contains(c("Ozone")))
      } else {
        data_list
      }
      mean <- paste0(i, "_mean")
      sd <- paste0(i, "_sd")
      x <- as.numeric(as.character(data_list[[i]]))
      x[!is.finite(x)] <- NA
      y <- grep("_mean", colnames(data_list))
      y <- as.numeric(as.character(data_list[[y]]))
      y[!is.finite(y)] <- NA
      z <- grep("_sd", colnames(data_list))
      z <- as.numeric(as.character(data_list[[z]]))
      z[!is.finite(z)] <- NA
      if(!nrow(data_list)){
        NULL 
      } else {
        data_list[[i]] <- mapply(LLD, x, y, z, as.numeric(as.character(eq)))
      }
      tseries_df <- bind_cols(tseries_df, data_list)
    }
    site1_join_f1 <- tseries_df %>%
      dplyr::select(date, everything(), -contains(c("_sd", "_mean")))
    tseries_df <- data.frame(date)
    site1_join_f1 <- left_join(tseries_df, site1_join_f1, by = "date") %>%
      dplyr::mutate(day = as.Date(date, format = '%Y-%m-%d', tz = input$timezone)) 
    site1_join_f1
  }
  
  #### Function to remove values which are incomplete in a day
  compl <- function(name, site1_join_f1, date, fi, per1) {
    tseries_df <- site1_join_f1 %>%
      dplyr::select(date)
    for(i in names(name)){
      data_list <- site1_join_f1 %>% 
        dplyr::select(date, day, starts_with(i))
      if(i == "NO") {
        data_list <- data_list %>%
          dplyr::select(-contains(c("NO2", "NOx")))
      } else if(i == "O") {
        data_list <- data_list %>%
          dplyr::select(-contains(c("Ozone", "O3")))
      } else {
        data_list
      }
      data_list <- data_list %>% 
        group_by(day) %>%
        dplyr::mutate_at(vars(contains(i)), list(no_hour = ~ sum(!is.na(.)))) %>%
        dplyr::select(-contains(c("_sd_no_hour", "_mean_no_hour")))
      old_no <- paste0(i, "_no_hour")
      names(data_list)[names(data_list) == old_no] <- 'no_hour'
      if(fi == "15 min") {
        time_avg = 96
      } else if(fi == "30 min") {
        time_avg = 48
      } else if(fi == "60 min") {
        time_avg = 24
      }
      data_list[[i]] <- ifelse(data_list$no_hour >= ((per1 / 100) * time_avg), data_list[[i]], NA)
      data_list[ , c('date', 'day', 'no_hour')] <- list(NULL)
      tseries_df <- bind_cols(tseries_df, data_list)
    }
    site1_join_f1 <- tseries_df %>%
      dplyr::mutate(day = as.Date(date, format = '%Y-%m-%d', tz = input$timezone)) 
  }
  
  #### Functions to be applied on the data set in a sequence
  data_file <- function(per, type, file_data_path, file_1, file, remove_9, repeated, 
                        percent, ey, exclude, high_number, log_op) {
    if (is.null(file_1)) {
      return(NULL)
    } else {
      if(type == "cpcb") {
        c(all, date) := cpcb(file_data_path, file)
        all <- data.frame(all) 
        date <- data.frame(date)
      } else if(type == "an") {
        c(all, date) := an(file_data_path)
        all <- data.frame(all) 
        date <- data.frame(date)
      } else if (type == "oaq") {
        c(all, date) := openaq(file_data_path)
        all <- data.frame(all) 
        date <- data.frame(date)
      }
      site1_join_f1 <- all %>%
        dplyr::mutate(day = as.Date(date, format = '%Y-%m-%d', tz = input$timezone)) %>%
        dplyr::select(date, day, everything())
      if(remove_9) {
        site1_join_f1 <- neg(site1_join_f1)
      } else { site1_join_f1 }  
      
      if(repeated) {
        site1_join_f1 <- repeat_ed(site1_join_f1)
      } else { site1_join_f1 }  
      
      name <- site1_join_f1 %>%
        dplyr::select(everything(), -day, -date)
      site1_join_f1 <- site1_join_f1 %>%
        dplyr::mutate(ratio = NA) %>%
        dplyr::select(date, day, everything())
      col_interest <- 3:ncol(site1_join_f1)
      if(log_op) {
        site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                                 FUN = function(x) log(x))
      } else { site1_join_f1 } 
      if(exclude) {
        site1_join_f1 <- outlier(site1_join_f1, name, date, ey)
        if(log_op) {
          site1_join_f1 <- site1_join_f1 %>%
            dplyr::select(date, day, everything())
          col_interest <- 3:ncol(site1_join_f1)
          site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                                   FUN = function(x) exp(x))
        }
      } else { site1_join_f1 } 
      
      site1_join_f1 <- site1_join_f1 %>%
        dplyr::select(date, day, everything())
      col_interest <- 3:ncol(site1_join_f1)
      site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                               FUN = function(x) as.numeric(as.character(x)))
      if(percent) {
        site1_join_f1 <- compl(name, site1_join_f1, date, file, per)
      } else { site1_join_f1 }
      
      if("PM2.5" %in% colnames(site1_join_f1)) {
        site1_join_f1 <- remov_99(site1_join_f1, high_number)
        if("PM10" %in% colnames(site1_join_f1))
        {
          site1_join_f1 <- ratio(site1_join_f1, high_number)
        } else {
          site1_join_f1$ratio <- NA
        }
      } else { site1_join_f1 }
      site1_join_f1 <- site1_join_f1 %>%
        dplyr::select(date, day, everything()) %>%
        janitor::remove_empty("cols")
      site1_join_f1
    }
  }
  
  CPCB_f <- reactive({
    if (is.null(input$file1)) {
      data <- data_file(75, "cpcb", "TN_CMN_19.xlsx", "TN_CMN_19.xlsx",
                        "60 min", TRUE, TRUE, TRUE,
                        3, TRUE, 999, FALSE)
    } else {
      data <- data_file(input$per, input$type, input$file1$datapath, input$file1,
                        input$file, input$remove_9, input$repeated, input$percent, 
                        input$ey, input$exclude, input$high_number, input$log_op) 
      }
  })
  Cmp_f <- reactive({
    if (is.null(input$file2)) {
      data <- data_file(75, "cpcb", "TN_CMN_19.xlsx", "TN_CMN_19.xlsx",
                        "60 min", TRUE, TRUE, TRUE,
                        3, TRUE, 999, FALSE)
    } else {
    data <- data_file(input$per, input$type2, input$file2$datapath, input$file2,
                      input$file12, input$remove_9, input$repeated, input$percent, 
                      input$ey, input$exclude, input$high_number, input$log_op)
    }
  })
  
  observeEvent(input$switch_tab, {
    updateTabsetPanel(session, "tabs1", selected = "FAQ's")
    removeModal()
  })
  
  query_modal <- modalDialog(
    title = "What to expect?",
    HTML("First visit here? Dummy data is loaded click on buttons and generate the figures!<br>"),
    footer = tagList(
      actionButton("help_link", "Read Me",
                   onclick = "window.open('https://github.com/adithirgis/OpenSourceAirQualityApp#have-a-look-at-the-app',
                   '_blank')"),
      actionButton("switch_tab", "FAQ's"),
      modalButton("Close")
    ), 
    easyClose = F
  )
  
  # Show the model on start up ...
  showModal(query_modal)
  
  observe({
    if (is.null(input$file1 | input$file2)) {
      data <- CPCB_f()
      data1 <- Cmp_f()
     } else {
      data <- CPCB_f()
      data1 <- Cmp_f()
    }
    data <- data %>% 
      dplyr::select(- date, - day)
    data1 <- data1 %>%
      dplyr::select(- date, - day)
    updateSelectInput(session, "Para", choices = names(data))
    updateSelectInput(session, "Para1", choices = names(data1))
    })
  mess <- function(button, da) {
    data <- CPCB_f()
    if(button == da) {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
  }
  
  data_joined <- eventReactive(input$hourly, {
    data <- mess(input$avg_hour, "daily")
  })
  data_joined_comp <- eventReactive(input$plot_values, {
    data <- CPCB_f()
    data1 <- Cmp_f()
    if(input$avg_hour == "daily") {
      data <- openair::timeAverage(data, avg.time = "day")
      data1 <- openair::timeAverage(data1, avg.time = "day")
    } else { 
      data
      data1 }
    data <- data %>%
      select(date, "Site 1" = input$Para)
    data1 <- data1 %>%
      select(date, "Site 2" = input$Para1)
    all <- full_join(data, data1, by = "date")
  })
  data_scatter_comp <- eventReactive(input$plot_val, {
    data <- CPCB_f()
    data1 <- Cmp_f()
    if(input$avg_hour == "daily") {
      data <- openair::timeAverage(data, avg.time = "day")
      data1 <- openair::timeAverage(data1, avg.time = "day")
    } else { 
      data
      data1 }
    data <- data %>%
      select(date, "Site 1" = input$Para)
    data1 <- data1 %>%
      select(date, "Site 2" = input$Para1)
    all <- full_join(data, data1, by = "date")
  })
  data_diurnal_comp <- eventReactive(input$plot_val_di, {
    data <- CPCB_f()
    data1 <- Cmp_f()
    data <- data %>%
      select(date, "Site 1" = input$Para)
    data1 <- data1 %>%
      select(date, "Site 2" = input$Para1)
    all <- full_join(data, data1, by = "date")
  })
  trend <- function(data, button, avg) {
    date_df <- data.frame(date = date_ts(data, "60 min"))
    data <- data %>%
      distinct(date, .keep_all = TRUE) %>%
      right_join(date_df, by = "date")
    if(button == avg) {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
  }
  output$plot12 <- renderPlot({
    if (is.null(input$file1) | is.null(input$file2)) { 
      all <- data_joined_comp()
      }
    else {
      all <- data_joined_comp()
    }
    all <- all %>%
      pivot_longer(-date, names_to = "parameter", values_to = "value") 
    ggplot(all, aes(as.POSIXct(date), value, colour = parameter)) +
      labs(y = input$comp_y, title = input$comp_mt,
           x = "") + theme2() + geom_line(size = 0.6) + theme(legend.title = element_blank())
   })
  data_da <- eventReactive(input$da, {
    data <- mess(input$avg_hour3, "daily3")
  })
  data_plot <- eventReactive(input$ts, {
    data <- mess(input$avg_hour3, "daily3")
  })
  data_box <- eventReactive(input$box, {
    data <- mess(input$avg_hour3, "daily3")
  })
  data_boxt <- eventReactive(input$boxt, {
    data <- CPCB_f()
    if(input$avg_hour3 == "daily3") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
  })
  data_mbox <- eventReactive(input$mbox, {
    data <- mess(input$avg_hour3, "daily3")
  })
  data_tv <- eventReactive(input$tv, {
    data <- CPCB_f()
  })
  data_cp <- eventReactive(input$cp, {
    data <- CPCB_f()
    
  })
  data_qq <- eventReactive(input$qq, {
    data <- mess(input$avg_hour2, "daily2")
  })
  data_freq <- eventReactive(input$freq, {
    data <- mess(input$avg_hour2, "daily2")
  })
  data_mk <- eventReactive(input$mk, {
    data <- CPCB_f()
    data <- trend(data, input$avg_hour2, "daily2")
  })
  data_ta <- eventReactive(input$ta, {
    data <- CPCB_f()
    data <- trend(data, input$avg_hour2, "daily2")
  })
  data_reg <- eventReactive(input$reg, {
    data <- mess(input$avg_hour1, "daily1")
  })
  data_mreg <- eventReactive(input$mulreg, {
    data <- mess(input$avg_hour1, "daily1")
  })
  data_diurnal <- eventReactive(input$diurnal, {
    data <- CPCB_f()
    return(data)
  })
  
  output$download_diurnal <- downloadHandler(
    filename <- function() {
      name_file <- paste0(name_file(), "_diurnal")
      paste(name_file, "csv", sep = ".")
    },
    content <- function(fname) {
      data <- data_diurnal()
      write.csv(data, fname)
    })
  observe({
    if (is.null(input$file1)) {
      data_joined <- CPCB_f()
      data_joined <- data_joined %>%
        dplyr::select(- date, - day)
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
  
  name_file <- reactive({
    if (is.null(input$file1)) {
      file_n <- "file"
    } else {
    file_n <- gsub(".xlsx", "", input$file1)
    file_n <- gsub(".csv", "", file_n)
    }
  })
  
  output$table1 <- DT::renderDataTable({
    if (is.null(input$file1)) {
      data_joined <- CPCB_f()
      if(input$avg_hour == "daily") {
        data_joined <- openair::timeAverage(data_joined, avg.time = "day")
      } else { data_joined }
    } else {
      data_joined <- data_joined()  
    }
    cols <- names(data_joined)[3:ncol(data_joined)]
    data_joined[ , cols] <- sapply(X = data_joined[ , cols], 
                                   FUN = function(x) as.numeric(as.character(x)))
    setDT(data_joined)
    data_joined[,(cols) := round(.SD, 2), .SDcols = cols]
    if(input$avg_hour == "daily") {
      data_joined <- data_joined %>%
        dplyr::mutate(date = as.Date(date, tz = input$timezone)) %>%
        dplyr::select(date, everything(), - day)
      datatable(data_joined, options = list("pageLength" = 15)) %>% formatDate(1, "toLocaleDateString")
    } else { datatable(data_joined, options = list("pageLength" = 15)) %>% formatDate(1, "toLocaleString") }
  })
  
  output$download <- downloadHandler(
    filename = function(){
      name_file <- paste0(name_file(), "_average")
      paste(name_file, "csv", sep = ".")
    },
    content = function(file) {
      data_joined <- data_joined()
      write.csv(data_joined, file)
    })
  
  theme2 <- reactive({
    theme2 <- list(theme_classic(),
                   theme(legend.text = element_text(size = 18),
                         plot.title = element_text(size = 14, face = "bold"),
                         axis.title = element_text(size = 20, face = "bold"),
                         axis.text = element_text(size = 18, face = "bold"),
                         panel.border = element_rect(colour = "black",
                                                     fill = NA, size = 1.2)))
  })
  f <- reactive({
    f <- function(x) {
      r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }})
  normalilty_t <- reactive({
    data <- data_freq()
    y <- as.numeric(as.character(data[[input$palleInp2]]))
    if(input$normality == "sh") {
      if(length(y) <= 5000) {
        x <- shapiro.test(y)
      } else {
        x <- "Sample size is greater than 500, so Shapiro-Wilk test will not be reliable, please use Anderson-Darling test."
      }
    } else {
      x <- ad.test(y)
    }
    x
  })
  kenda <- reactive({
    data <- data_mk()
    if(input$avg_hour2 == "daily2" & !input$avg_month) {
      x <- zoo(data[[input$palleInp2]], data$date)
      x <- na.interp(x)
    } else if (input$avg_month) {
      data <- data %>%
        mutate(date = format(date, "%Y-%m"),
               date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d", tz = "Asia/Kolkata")) %>%
        group_by(date) %>%
        summarise_all(funs(mean), na.rm = TRUE)
      x <- zoo(data[[input$palleInp2]], data$date)
      x <- na.interp(x)
    } else { 
      NULL
    }
  })
  
  output$normality_test <- renderPrint({
    normalilty_t()
  })
  output$kendal_test <- renderPrint({
    # validate(need(try(all(is.na(y)) == TRUE), "Sorry no data!"))
    if(input$avg_hour2 != "daily2") {
      "Trend analysis are not prominent with hourly values."
    } else {
      y <- kenda()
      c <- trend::mk.test(y)
      c
    }
  })
  output$plot1 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_plot()
      } else {
      data <- data_plot()
    }
    y <- as.numeric(as.character(data[[input$palleInp]]))
    ggplot(data, aes(as.POSIXct(date), y)) +
      labs(y = input$ts_y, title = input$ts_mt,
           x = "") + theme2() + geom_line(size = 0.6, color = "seagreen")
  })
  output$plot2 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_diurnal()
    } else {
      data <- data_diurnal()
    }
    data <- data %>%
      dplyr::mutate(hour = format(date, "%H"),
                    month = format(date, "%b"))
    data <- data %>%
      dplyr::select(hour, month, "y" = input$palleInp) 
    data$hour <- as.numeric(as.character(data$hour))
    if (input$diur == "mesd" && input$diurn == "all") { 
      data <- data %>%
        dplyr::select(hour, y) %>%
        group_by(hour) %>%
        summarise_all(funs(mean, sd), na.rm = TRUE)
      ggplot(data, aes(hour, mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                                                    color = "seagreen") + 
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$diurnal_y, x = "Hour of the day (LT)'", title = input$diurnal_mt) + 
        theme2() + geom_line(size = 0.6, color = "seagreen")
    } else if (input$diur == "mediq" && input$diurn == "all") {
      data <- data %>%
        dplyr::select(hour, y) %>%
        group_by(hour) %>%
        summarise_all(funs(median, p25 = quantile(., .25), p75 = quantile(., .75)), na.rm = TRUE)
      ggplot(data, aes(hour, median)) + geom_errorbar(aes(ymin = p25, ymax = p75), 
                                                      color = "seagreen") + 
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$diurnal_y, x = "Hour of the day (LT)", title = input$diurnal_mt) + 
        theme2() + geom_line(size = 0.6, color = "seagreen")
    } else if (input$diur == "mediq" && input$diurn == "mon") {
      data <- data %>%
        group_by(month, hour) %>%
        summarise_all(funs(median, p25 = quantile(., .25), p75 = quantile(., .75)), na.rm = TRUE)
      ggplot(data, aes(hour, median)) + geom_errorbar(aes(ymin = p25, ymax = p75), 
                                                      color = "seagreen") + 
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$diurnal_y, x = "hour of the day", title = input$diurnal_mt) + 
        theme2() + geom_line(size = 0.6, color = "seagreen")  + facet_wrap(.~month, nrow = 3) + 
        theme(axis.text.y = element_text(size = 12))
    } else if (input$diur == "mesd" && input$diurn == "mon") { 
      data <- data %>%
        group_by(month, hour) %>%
        summarise_all(funs(mean, sd), na.rm = TRUE)
      ggplot(data, aes(hour, mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                                                    color = "seagreen") + 
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$diurnal_y, x = "hour of the day", title = input$diurnal_mt) + 
        theme2() + geom_line(size = 0.6, color = "seagreen") + facet_wrap(.~month, nrow = 3) + 
        theme(axis.text.y = element_text(size = 12))
    }
  })
  output$plot3 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_box()
    } else {
      data <- data_box()
    }
    y <- as.numeric(as.character(data[[input$palleInp]]))
    ggplot(data, aes(x = reorder(format(date,'%b %Y'), date), y)) + 
      stat_summary(fun.data = f(), colour = "seagreen", geom = "boxplot", 
                   width = 0.4, size = 1) + 
      labs(y = input$box_y, x = "", title = input$box_mt) + 
      stat_summary(aes(y = y), fun.y = "mean", colour = "seagreen", 
                   geom = "point", size = 4)  +
      theme2() + theme(axis.text.x = element_text(size = 10, face = "bold", angle = 90))
  })
  output$plot9 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_mbox()
    } else {
      data <- data_mbox()
    }
    y <- as.numeric(as.character(data[[input$palleInp]]))
    ggplot(data, aes(x = reorder(format(date,'%b'), date), y)) +
      stat_summary(fun.data = f(), colour = "seagreen", geom = "boxplot",
                   width = 0.4, size = 1) +
      labs(y = input$box_my, x = "", title = input$box_mmt) +
      stat_summary(aes(y = y), fun.y = "mean", colour = "seagreen",
                   geom = "point", size = 4)  +
      theme2() + theme(axis.text.x = element_text(size = 13, face = "bold"))
    })
  output$plot4 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_tv()
    } else {
      data <- data_tv()
    }
    y <- as.numeric(as.character(data[[input$palleInp1]]))
    if(input$stat_tv == "mean") {
      openair::timeVariation(data, pollutant = input$palleInp1, 
                             par.settings = list(fontsize = list(text = 15)))
    } else if(input$stat_tv == "median") {
      openair::timeVariation(data, pollutant = input$palleInp1, stati = "median", 
                             conf.int = c(0.75, 0.99),
                             par.settings = list(fontsize = list(text = 15)))
    }
    })
  output$plot5 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_cp()
    } else {
      data <- data_cp()
    }
    y <- as.numeric(as.character(data[[input$palleInp1]]))
    openair::calendarPlot(data, pollutant = input$palleInp1, main = input$cp_mt,
                          cols = openColours(c("seagreen", "yellow", "red"), 10),
                          par.settings = list(fontsize = list(text = 15)))
    })
  output$plot6 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_freq()
    } else {
      data <- data_freq()
    }
    y <- as.numeric(as.character(data[[input$palleInp2]]))
    ggplot(data, aes(x = y)) +
      geom_density(color = "deepskyblue", fill = "lightblue") +
      labs(y = "density", x = input$freq_x, title = input$freq_mt) + theme2()
    })
  output$plot7 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_qq()
    } else {
      data <- data_qq()
    }
    y <- as.numeric(as.character(data[[input$palleInp2]]))
    ticks <- qnorm(c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
    labels <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)
    ggplot(data, aes(sample = log10(y))) +
      stat_qq(size = 2, geom = 'point', color = "deepskyblue") +
      stat_qq_line(size = 1, linetype = 2) + 
      scale_x_continuous(breaks = ticks, labels = labels) +
      labs(x = "Emperical percentiles",
           y =  input$qq_y, title = input$qq_mt) + theme2()
    })
  output$plot8 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_reg()
    } else {
      data <- data_reg()
    }
    y <- as.numeric(as.character(data[[input$DepVar]]))
    x <- as.numeric(as.character(data[[input$InDepVar]]))
    reg_eqn <- function(x) {
      R_sq <- round(as.numeric(x$adj.r.squared), digits = 2)
      int <- round(coef(x)[1], digits = 2)
      slope <- round(coef(x)[2], digits = 2)
      eqn <- paste("y = ", slope, "x + (", int, ")")
      return(eqn)
    }
    m <- lm(y ~ x, data)
    s <- summary(m)
    r <- round(s$adj.r.squared, digits = 2)
    ggplot(data = data, aes(x = x, y = y)) +
      geom_point(alpha = 0.5, color = "red") + 
      geom_smooth(method = lm, size = 1.2, se = FALSE, formula = y ~ x, color = "deepskyblue") +
      labs(x = input$reg_x,
           y = input$reg_y, title = input$reg_mt,
           subtitle = paste0("R square: ", r, "; Equation: ", reg_eqn(s))) + 
      theme2()
   })
  output$plot15 <- renderPlot({
    if (is.null(input$file1) | is.null(input$file2)) {
      data <- data_scatter_comp()
      } else {
      data <- data_scatter_comp()
      }
    y <- as.numeric(as.character(data$`Site 1`))
    x <- as.numeric(as.character(data$`Site 2`))
    reg_eqn <- function(x) {
      R_sq <- round(as.numeric(x$adj.r.squared), digits = 2)
      int <- round(coef(x)[1], digits = 2)
      slope <- round(coef(x)[2], digits = 2)
      eqn <- paste("y = ", slope, "x + (", int, ")")
      return(eqn)
    }
    m <- lm(y ~ x, data)
    s <- summary(m)
    r <- round(s$adj.r.squared, digits = 2)
    ggplot(data = data, aes(x = x, y = y)) +
      geom_point(alpha = 0.5, color = "red") + 
      geom_smooth(method = lm, size = 1.2, se = FALSE, formula = y ~ x, color = "deepskyblue") +
      labs(x = input$reg_mx1,
           y = input$reg_my1, title = input$reg_mt1,
           subtitle = paste0("R square: ", r, "; Equation: ", reg_eqn(s))) + 
      theme2()
   })
  output$plot10 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_boxt()
    } else {
      data <- data_boxt()
    }
    data <- data %>%
      dplyr::mutate(month = format(date, "%b")) %>%
      dplyr::select(month, "y" = input$palleInp) %>%
      group_by(month) %>%
      summarise_all(funs(mean, sd), na.rm = TRUE)
    ggplot(data, aes(x = month, mean)) + 
      geom_bar(position = position_dodge(), stat = "identity", colour = 'seagreen', 
               fill  = 'seagreen') + 
      labs(y = input$box_yt, x = "", title = input$box_mtt) + 
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = 
                      position_dodge(.9), color = 'seagreen') +
      theme2() + theme(axis.text.x = element_text(size = 12, face = "bold"))
  })
  output$plot13 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_ta()
    } else {
      data <- data_ta()
    }
    data$date <- as.Date(data$date, "%Y-%m-%d", tz = "Asia/Kolkata")
    if(input$avg_hour2 == "daily2") {
      data <- data.frame(data$date, na.interp(data[[input$palleInp2]])) 
      data_wt <- wt(data, dt = 1) 
    } else { 
      NULL
    }
    par(mfrow = c(1, 1), oma = c(0, 0, 0, 2), mar = c(5, 5, 5, 7) + 0.1) #specify the limits of margins to the plot area
    plot.biwavelet(data_wt, form = "%Y-%m-%d", type = "power.corr.norm",
                   plot.cb = TRUE, lwd.coi = 1, col.coi = "black",
                   cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, 
                   plot.sig = 95, lwd.sig = 2, main = input$title_bi, 
                   ylab = "Period (days)", xlab = "")
  })
  # Data availability plot
  output$plot16 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- data_da()
    } else {
      data <- data_da()
    }
    data_avail <- data %>%
      dplyr::select(day, everything(), - date) %>%
      group_by(day) %>%
      summarise_all(funs(mean), na.rm = TRUE) %>%
      pivot_longer(-day, names_to = "Parameter", values_to = "Value")
    no_na_df <- data_avail 
    no_na_df <- no_na_df[complete.cases(no_na_df), ]
    ggplot(no_na_df, aes(x = day, y = Parameter, color = Parameter)) + 
      geom_errorbarh(aes(xmax = day, xmin = day), size = 0.25) + 
      labs(y = "", title = "Data availability plot", x = "") + 
      scale_x_date(date_breaks = "60 days", date_labels = "%b-%y") +
      theme2() + theme(legend.position = "none")
    })
  # Diurnal for two sites
  output$plot17 <- renderPlot({
    if (is.null(input$file1) | is.null(input$file2)) { 
      data <- data_diurnal_comp()
    } else {
      data <- data_diurnal_comp()
    }
    data <- data %>%
      dplyr::mutate(hour = format(date, "%H"),
                    month = format(date, "%b"))
    data <- data %>%
      dplyr::select(hour, month, site1 = `Site 1`, site2 = `Site 2`) 
    data$hour <- as.numeric(as.character(data$hour))
    if (input$diur1 == "mesd" && input$diurn1 == "all") { 
      data <- data %>%
        dplyr::select(hour, site1, site2) %>%
        group_by(hour) %>%
        summarise_all(funs(mean, sd), na.rm = TRUE)
      ggplot(data, aes(hour, site1_mean)) + 
        geom_errorbar(aes(ymin = site1_mean - site1_sd, ymax = site1_mean + site1_sd), 
                                                    color = "#F8766D") +
        geom_errorbar(aes(ymin = site2_mean - site2_sd, ymax = site2_mean + site2_sd), 
                      color = "#2596BE") + 
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$diurnal_y1, x = "Hour of the day (LT)", title = input$diurnal_mt1) + 
        theme2() + geom_line(aes(y = site1_mean), size = 0.6, color = "#F8766D") + 
        geom_line(aes(y = site2_mean), size = 0.6, color = "#2596BE")
    } else if (input$diur1 == "mediq" && input$diurn1 == "all") {
      data <- data %>%
        dplyr::select(hour, site1, site2) %>%
        group_by(hour) %>%
        summarise_all(funs(median, p25 = quantile(., .25), p75 = quantile(., .75)), na.rm = TRUE)
      ggplot(data, aes(as.numeric(hour), site1_median)) + 
        geom_errorbar(aes(ymin = site1_p25, ymax = site1_p75), color = "#F8766D") +
        geom_errorbar(aes(ymin = site2_p25, ymax = site2_p75), color = "#2596BE") +
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$diurnal_y1, x = "Hour of the day (LT)", title = input$diurnal_mt1) + 
        theme2() + geom_line(aes(y = site1_median), size = 0.6, color = "#F8766D") + 
        geom_line(aes(y = site2_median), size = 0.6, color = "#2596BE")
    } else if (input$diur1 == "mediq" && input$diurn1 == "mon") {
      data <- data %>%
        group_by(month, hour) %>%
        summarise_all(funs(median, p25 = quantile(., .25), p75 = quantile(., .75)), na.rm = TRUE)
      ggplot(data, aes(as.numeric(hour), site1_median)) + 
        geom_errorbar(aes(ymin = site1_p25, ymax = site1_p75), color = "#F8766D") +
        geom_errorbar(aes(ymin = site2_p25, ymax = site2_p75), color = "#2596BE") +
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$diurnal_y1, x = "Hour of the day (LT)", title = input$diurnal_mt1) + 
        theme2() + geom_line(aes(y = site1_median), size = 0.6, color = "#F8766D") + 
        geom_line(aes(y = site2_median), size = 0.6, color = "#2596BE") + facet_wrap(.~month, nrow = 3) + 
        theme(axis.text.y = element_text(size = 12))
    } else if (input$diur1 == "mesd" && input$diurn1 == "mon") { 
      data <- data %>%
        group_by(month, hour) %>%
        summarise_all(funs(mean, sd), na.rm = TRUE)
      ggplot(data, aes(hour, site1_mean)) + 
        geom_errorbar(aes(ymin = site1_mean - site1_sd, ymax = site1_mean + site1_sd), 
                      color = "#F8766D") +
        geom_errorbar(aes(ymin = site2_mean - site2_sd, ymax = site2_mean + site2_sd), 
                      color = "#2596BE") + 
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$diurnal_y1, x = "Hour of the day (LT)", title = input$diurnal_mt1) + 
        theme2() + geom_line(aes(y = site1_mean), size = 0.6, color = "#F8766D") + 
        geom_line(aes(y = site2_mean), size = 0.6, color = "#2596BE") + facet_wrap(.~month, nrow = 3) + 
        theme(axis.text.y = element_text(size = 12))
    }
  })
  # acf(z, lag.max = ((nrow(TimeSerie))/2), na.action = na.pass)
  
  lm_reg <- reactive({
    data <- data_mreg()
    y <- lm(as.formula(paste(input$DepVar1, " ~ ", paste(input$InDepVar1, collapse = "+"))), data)
    y
  })
  output$plot11 <- renderPlot({
    if (is.null(input$file1)) { 
      data <- fortify(lm_reg())
    } else {
      data <- fortify(lm_reg())
    }
    y <- as.numeric(as.character(data[[input$DepVar1]]))
    ggplot(data = data, aes(x = .fitted, y = y)) +
      geom_point(alpha = 0.5, color = "red") + 
      geom_smooth(method = lm, size = 1.2, se = FALSE, formula = y ~ x, color = "deepskyblue") +
      labs(x = input$reg_mx, y = input$reg_my, title = input$reg_mmt) + theme2()
   })
  output$RegOut <- renderPrint({summary(lm_reg())})
  output$DepPrint <- renderPrint({paste("Dependent variable:", input$DepVar1)})
  output$IndPrint <- renderPrint({paste("Independent variables:", input$InDepVar1)})
  
  data_summary <- reactive({
    if (is.null(input$file1)) {
      data <- CPCB_f()
    } else {
      data <- data_joined()
    }
    data <- data %>%
      dplyr::select(day, everything(), - date)
    if(input$avg == "no") {
      data <- data %>%
        dplyr::select(everything(), - day)
      columns <- 1:ncol(data)
      data[, columns] <- lapply(columns, function(x) as.numeric(as.character(data[[x]])))
      tmp1 <- do.call(data.frame,
                      list(Mean = apply(data, 2, function(y)
                      {mean(y, na.rm = TRUE)}),
                      SD = apply(data, 2, function(y)
                      {sd(y, na.rm = TRUE)}),
                      Median = apply(data, 2, median, na.rm = TRUE),
                      IQR = apply(data, 2, IQR, na.rm = TRUE),
                      Min = apply(data, 2, min, na.rm = TRUE),
                      Max = apply(data, 2, max, na.rm = TRUE),
                      p1  = apply(data, 2, quantile, probs = c(0.01),
                                  na.rm = TRUE),
                      p10 = apply(data, 2, quantile, probs = c(0.1),
                                  na.rm = TRUE),
                      p25 = apply(data, 2, quantile, probs = c(0.25),
                                  na.rm = TRUE),
                      p75 = apply(data, 2, quantile, probs = c(0.75),
                                  na.rm = TRUE),
                      p90 = apply(data, 2, quantile, probs = c(0.9),
                                  na.rm = TRUE),
                      p99 = apply(data, 2, quantile, probs = c(0.99),
                                  na.rm = TRUE),
                      non_NA = apply(data, 2,
                                     function(y)
                                     {length(which(!is.na(y)))})))
      tmp <- data.frame(tmp1)
      tmp$Mean   <- round(as.numeric(as.character(tmp$Mean)), digits = 2)
      tmp$IQR    <- round(as.numeric(as.character(tmp$IQR)), digits = 2)
      tmp$Median <- round(as.numeric(as.character(tmp$Median)), digits = 2)
      tmp$Min    <- round(as.numeric(as.character(tmp$Min)), digits = 2)
      tmp$Max    <- round(as.numeric(as.character(tmp$Max)), digits = 2)
      tmp$p10    <- round(as.numeric(as.character(tmp$p10)), digits = 2)
      tmp$SD     <- round(as.numeric(as.character(tmp$SD)), digits = 2)
      tmp$p90    <- round(as.numeric(as.character(tmp$p90)), digits = 2)
      tmp$p75    <- round(as.numeric(as.character(tmp$p75)), digits = 2)
      tmp$p99    <- round(as.numeric(as.character(tmp$p99)), digits = 2)
      tmp$p1     <- round(as.numeric(as.character(tmp$p1)), digits = 2)
      tmp$p25    <- round(as.numeric(as.character(tmp$p25)), digits = 2)
      tmp
      tmp <- t(tmp)
    } else {
      data <- data %>%
        dplyr::mutate(month  = format(day, "%b %Y"),
                      monthly = format(day, "%b")) %>%
        dplyr::select(day, month, monthly, everything())
      data$group <- data[[input$avg]]
      data <- data %>%
        group_by(group) %>%
        summarise_if(is.numeric, funs(Mean = mean, SD = sd, Median = median, IQR = IQR, 
                                      Min = min, Max = max,   
                                      p1 = quantile(., .01), p10 = quantile(., .1), 
                                      p25 = quantile(., .25), p75 = quantile(., .75), 
                                      p90 = quantile(., .9), p99 = quantile(., .99),
                                      non_NA = sum(!is.na(.))), na.rm = TRUE)
      columns <- 2:ncol(data)
      data[, columns] <- lapply(columns, function(x) round(as.numeric(as.character(data[[x]])), digits = 2))
      tmp <- data
    } 
    tmp
  })
  output$table <- DT::renderDataTable({
    tmp <- data_summary()
    datatable(tmp, options = list("pageLength" = 13))
  })
  
  output$download_stats <- downloadHandler(
    filename <- function() {
      name_file <- paste0(name_file(), "_stats")
      paste(name_file, "csv", sep = ".")
      },
    content <- function(fname) {
      data_summary <- data_summary()
      write.csv(data_summary, fname)
    })
  
}
#### Run app
shinyApp(ui, server)


# runApp(display.mode = "showcase")

# fft(data_op$PM10)
# 
# x <- zoo(data_op$PM10, data_op$date)
# x <- as.ts(x)
# x <- na.interp(x)
# y_date <- julian(data_op$date, data_op$date[1])
# data <- cbind(as.numeric(y_date), as.numeric(x))
# ## Continuous wavelet transform
# cwt_data <- spectrum(data, log = "no")
# delta <- 1/365
# specx <- cwt_data$freq / delta
# specy <- 2 * cwt_data$spec[,2]
# plot(specx, specy, xlab = "Period (days)", ylab = "Spectral Density", type = "l")
# write.csv(data_op, "data.csv")

# output$plot14 <- renderPlot({
#   data <- data.frame(
#     pollutant = c("Sulphur dioxide", "Nitrogen dioxide", "PM10", "PM2.5", 
#                   "Ozone", "Ammonia", "Benzene"),
#     levels = c(50, 40, 60, 40, 100, 100, 5)
#   )
#   ggplot(data, aes(pollutant, levels)) +
#     geom_hline(yintercept = 50, colour = "black") +
#     geom_hline(yintercept = 40, colour = "black") +
#     geom_hline(yintercept = 60, colour = "black") +
#     geom_hline(yintercept = 40, colour = "black") +
#     geom_hline(yintercept = 100, colour = "black") +
#     geom_hline(yintercept = 100, colour = "black") +
#     geom_hline(yintercept = 5, colour = "black") +
#     labs(y = "Major Pollutants", title = expression(paste("National Ambient Air Quality Annual Standards in India" , " (", "ug", ~m^{-3}, ")")),
#          x = "") + geom_text(aes(label = paste(pollutant, "=", levels)), 
#                              nudge_x = 0, nudge_y = 3, size = 6) + theme2() +
#     theme(axis.text.x = element_blank(), 
#           plot.title = element_text(size = 22, colour = "black")) 
# })
# zlim = c(0, as.integer(log2(nrow(data)))), 


# data_diurnal <- eventReactive(input$diurnal, {
#   data <- CPCB_f()
#   if(input$avg_hour3 == "daily3") {
#     data <- openair::timeAverage(data, avg.time = "day")
#   } else { 
#     data <- CPCB_f() 
#   }
#   return(data)
# })