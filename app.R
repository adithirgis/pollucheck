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
library(markdown)
library(nortest)
library(janitor)
library(recipes)
library(thematic)
library(bslib)

ui <- fluidPage(
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
                                               selectInput("palleInp", "Select parameter to plot",
                                                           multiple = FALSE, 
                                                           "Select"),
                                               tags$hr(),
                                               tags$hr(),
                                               radioButtons("avg_hour3", "Average to",
                                                            c("Hourly" = "hour3",
                                                              "Daily" = "daily3"), 
                                                            selected = "hour1"),
                                               tags$hr(),
                                               tags$hr(),
                                               textInput("ts_mt", label = "Edit title of plot", 
                                                         value = "Title"),
                                               textInput("ts_y", label = "Edit Y axis title", 
                                                         value = "Pollutant"),
                                               actionButton("ts", "Time Series"),
                                               tags$hr(),
                                               tags$hr(),
                                               textInput("box_mt", label = "Edit title of plot", 
                                                         value = "Title"),
                                               textInput("box_y", label = "Edit Y axis title", 
                                                         value = "Pollutant"),
                                              actionButton("box", "Month-Yearly Box Plot"),
                                              tags$hr(),
                                              tags$hr(),
                                              textInput("box_mmt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("box_my", label = "Edit Y axis title", 
                                                        value = "Pollutant"),
                                              actionButton("mbox", "Monthly Box Plot"),
                                              tags$hr(),
                                              tags$hr(),
                                              textInput("box_mtt", label = "Edit title of plot", 
                                                        value = "Title"),
                                              textInput("box_yt", label = "Edit Y axis title", 
                                                        value = "Pollutant"),
                                              actionButton("boxt", "Vertical Bar Plot"),
                                              tags$hr(),
                                              tags$hr(),
                                              radioButtons("diurn", "Show Diurnal plot for",
                                                           c("All data" = "all",
                                                             "Monthly" = "mon"), 
                                                           selected = "all"),
                                              radioButtons("diur", "Plot using",
                                                           c("Mean and Std Dev" = "mesd",
                                                             "Median and IQR" = "mediq"), 
                                                           selected = "mesd"),
                                              textInput("diurnal_mt", label = "Edit title of plot", 
                                                       value = "Title"),
                                              textInput("diurnal_y", label = "Edit Y axis title", 
                                                       value = "Pollutant"),
                                              actionButton("diurnal", "Diurnal Plot"),
                                              downloadButton('download_diurnal', "Download as csv"),
                                              tags$hr()),
                              conditionalPanel(condition = "input.tabs1 == 5",
                                               tags$hr(),
                                               selectInput("palleInp2", "Select parameter to plot",
                                                           multiple = FALSE, "Select"),
                                               tags$hr(),
                                               tags$hr(),
                                               radioButtons("avg_hour2", "Average to",
                                                            c("Hourly" = "hour2",
                                                              "Daily" = "daily2"), 
                                                            selected = "hour1"),
                                               tags$hr(),
                                               tags$hr(),
                                               radioButtons("normality", "Normality Test",
                                                            c("Anderson-Darling" = "ad",
                                                              "Shapiro-Wilk" = "sh"), 
                                                            selected = "ad"),
                                               tags$hr(),
                                               tags$hr(),
                                               textInput("freq_mt", label = "Edit title of plot", 
                                                         value = "Title"),
                                               textInput("freq_x", label = "Edit X axis title", 
                                                         value = "Pollutant"),
                                               actionButton("freq", "Density Plot"),
                                               tags$hr(),
                                               tags$hr(),
                                               textInput("qq_mt", label = "Edit title of plot", 
                                                         value = "Title"),
                                               textInput("qq_y", label = "Edit Y axis title", 
                                                         value = "Pollutant"),
                                               actionButton("qq", "QQ Plot"),
                                               tags$hr()),
                              conditionalPanel(condition = "input.tabs1 == 2",
                                               tags$hr(),
                                               selectInput("avg",
                                                           label = "Averaging period for summary statistics",
                                                           c("None" = "no",
                                                             "Daily" = "day",
                                                             "Monthly" = "monthly", 
                                                             "Month-Yearly" = "month"),
                                                           multiple = FALSE, 
                                                           selected = "None"),
                                               tags$hr(),
                                               downloadButton('download_stats', "Download as csv"),
                                               tags$hr()),
                              conditionalPanel(condition = "input.tabs1 == 6",
                                               tags$hr(),
                                               radioButtons("avg_hour1", "Average to",
                                                            c("Hourly" = "hour1",
                                                              "Daily" = "daily1"), 
                                                            selected = "hour1"),
                                               tags$hr(),
                                               tags$hr(),
                                               selectInput("DepVar", 
                                                           "Dependent Variable", 
                                                           multiple = FALSE, "Select"),
                                               selectInput("InDepVar", 
                                                           "Independent Variable", 
                                                           multiple = FALSE, "Select"),
                                               textInput("reg_mt", label = "Edit title of plot", 
                                                         value = "Title"),
                                               textInput("reg_x", label = "Edit X axis title", 
                                                         value = "Pollutant"),
                                               textInput("reg_y", label = "Edit Y axis title", 
                                                         value = "Pollutant"),
                                               actionButton("reg", "Linear Regression Plot"),
                                               tags$hr(),
                                               tags$hr(),
                                               selectInput("DepVar1", 
                                                           "Dependent Variable", 
                                                           multiple = FALSE, "Select"),
                                               selectInput("InDepVar1", 
                                                           "Independent Variable(s)", 
                                                           multiple = TRUE, "Select"),
                                               actionButton("mulreg", "Multiple Regression"),
                                               tags$hr()),
                              conditionalPanel(condition = "input.tabs1 == 7"),
                              conditionalPanel(condition = "input.tabs1 == 4",
                                               tags$hr(),
                                               selectInput("palleInp1", "Select parameter to plot",
                                                           multiple = FALSE, "Select"),
                                               tags$hr(),
                                               tags$hr(),
                                               radioButtons("avg_hour4", "Average to",
                                                            c("Hourly" = "hour4",
                                                              "Daily" = "daily4"), 
                                                            selected = "hour1"),
                                               tags$hr(),
                                               tags$hr(),
                                               textInput("cp_mt", label = "Edit title of plot", 
                                                         value = "Title"),
                                               actionButton("cp", "Calendar Plot"),
                                               tags$hr(),
                                               tags$hr(),
                                               radioButtons("stat_tv", "Plot using",
                                                            c("Median and quantiles" = "median",
                                                              "Mean and 95% confidence intervals" = "mean"), 
                                                            selected = "mean"),
                                               actionButton("tv", "Time Variation"),
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
                                                                radioButtons("file", "Time resolution of downloaded data",
                                                                             c("15 minutes" = "15 min",
                                                                               "30 minutes" = "30 min",
                                                                               "60 minutes" = "60 min"), 
                                                                             selected = "60 min")),
                                               tags$hr(),
                                               fileInput("file1",
                                                         "Upload data",
                                                         multiple = TRUE,
                                                         accept = c('.xlsx', '.csv')),
                                               tags$hr(),
                                               checkboxInput('remove_9', 'Remove negative values'),
                                               tags$hr(),
                                               checkboxInput('repeated', 'Remove consecutive measurements'),
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
                                                 sliderInput("per", "Specify % of data completeness required in a day",
                                                              value = 75,  min = 35, max = 100)),
                                               tags$hr(),
                                               numericInput("high_number",
                                                            "Remove PM2.5 and PM10 values above",
                                                            value = 9999),
                                               tags$hr(),
                                               radioButtons("avg_hour", "Average to",
                                                            c("Hourly" = "hour",
                                                              "Daily" = "daily"), 
                                                            selected = "hour"),
                                               tags$hr(),
                                               actionButton("hourly", "Show Data"),
                                               downloadButton('download', "Download as csv"),
                                               tags$hr())),
                mainPanel(
                  theme = bslib::bs_theme(), 
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
                                plotOutput("plot9", width = 800),
                                plotOutput("plot10", width = 800),
                                plotOutput("plot2", width = 800)),
                              tabPanel(
                                value = 5,
                                title = "Statistics Plots",
                                verbatimTextOutput("normality_test"),
                                # verbatimTextOutput("kendal_test"),
                                plotOutput("plot6", width = 800),
                                plotOutput("plot7", width = 800)),
                              tabPanel(
                                value = 6,
                                title = "Linear Regression",
                                plotOutput("plot8", width = 800), 
                                verbatimTextOutput("RegOut"),
                                plotOutput("plot11", width = 800), 
                                verbatimTextOutput(outputId = "IndPrint"),
                                verbatimTextOutput(outputId = "DepPrint")),
                              tabPanel(
                                value = 4,
                                title = "openair package plots",
                                plotOutput("plot5", height = 600),
                                plotOutput("plot4", height = 600)),
                              tabPanel(
                                value = 7,
                                title = "Help",
                                includeMarkdown("include.md")))
                )))


server <- function(input, output, session) {
  # bs_themer()
  options(shiny.maxRequestSize = 30*1024^2, shiny.launch.browser = TRUE)
  LLD <- function(x, y, z, ey) {
    ifelse(x < (y + (ey * z)) && x > (y - (ey * z)), x, NA)
  }
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
  make_df <- function(y, tseries_df) {
    df <- data.frame(y)
    if(!nrow(df))
    {
      df <- tseries_df
    } else {
      names(df) <- as.matrix(df[1, ])
      df <- df[-1, ]
      df[] <- lapply(df, function(x) type.convert(as.character(x)))
      df <- base::Filter(function(x)! all(is.na(x)), df)
      df <- df %>%
        dplyr::select("date" = `To Date`, everything()) %>%
        mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', 
                                 tz = "Asia/Kolkata"))
    }
  }
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
  cpcb <- function(sf, sfd) {
    trial <- read.xlsx2(sf, 1, startRow = 17)
    trial$date <- gsub(":00", ":00:00", trial$To.Date, fixed = TRUE)
    trial$tbl_id <- cumsum(!nzchar(trial$date))
    trial <- trial[nzchar(trial$date), ]
    trial[ , c('From.Date', 'To.Date')] <- list(NULL)
    dt_s <- split(trial[, -ncol(trial)], trial$tbl_id)
    PM <- data.frame(dt_s$`0`) %>%
      mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', tz = "Asia/Kolkata"))
    date <- date_ts(PM, sfd)
    tseries_df <- data.frame(date)
    site1_join <- left_join(tseries_df, PM, by = "date")
    Ben <- make_df(dt_s$`2`, tseries_df)
    Beny <- make_df(dt_s$`4`, tseries_df)
    Bent <- make_df(dt_s$`6`, tseries_df)
    all <- list(site1_join, Ben, Beny, Bent) %>% reduce(left_join, by = "date")
    return(list(all, date))
  }
  an <- function(sdw) {
    trial <- read.csv(sdw, header = TRUE, sep = ",", 
                      row.names = NULL)
    trial <- trial %>%
      dplyr::select("date" = Date..LT., "PM2.5" = Raw.Conc., "Valid" = QC.Name) %>%
      mutate(date  = as.POSIXct(date, format = '%Y-%m-%d %I:%M %p', tz = "Asia/Kolkata")) %>%
      dplyr::filter(Valid == "Valid")
    trial$Valid <- NULL
    date <- date_ts(trial, "60 min")
    tseries_df <- data.frame(date)
    all <- left_join(tseries_df, trial, by = "date")
    return(list(all, date))
  }
  openaq <- function(sgf) {
    trial <- read.csv(sgf, header = TRUE, sep = ",", 
                      row.names = NULL)
    trial <- trial %>%
      dplyr::select("date" = local, "parameter" = parameter, "value" = value) %>%
      mutate(date  = as.POSIXct(date, format = '%Y-%m-%dT%H:%M:%S+05:30', tz = "Asia/Kolkata"))
    date <- date_ts(trial, "15 min")
    tseries_df <- data.frame(date)
    trial <- trial %>%
      pivot_wider(names_from = parameter, values_from = value)
    trial <- trial[order(trial$date), ]
    all <- left_join(tseries_df, trial, by = "date") 
    all$hour <- lubridate::ceiling_date(all$date, "hour")
    all <- all %>%
      group_by(hour) %>%
      summarise_all(funs(mean), na.rm = TRUE) %>%
      dplyr::select(everything(), - date) %>%
      dplyr::select("date" = hour, everything())
    if("pm25" %in% colnames(all))
    {
      all <- all %>%
        dplyr::select(date, everything(), "PM2.5" = pm25)
    }
    if("pm10" %in% colnames(all))
    {
      all <- all %>%
        dplyr::select(date, everything(), "PM10" = pm10)
    }
    if("nox" %in% colnames(all))
    {
      all <- all %>%
        dplyr::select(date, everything(), "NOx" = nox)
    }
    if("no2" %in% colnames(all))
    {
      all <- all %>%
        dplyr::select(date, everything(), "NO2" = no2)
    }
    if("no" %in% colnames(all))
    {
      all <- all %>%
        dplyr::select(date, everything(), "NO" = no)
    }
    return(list(all, date))
  }
  neg <- function(site1_join_f1) {
    col_interest <- 3:ncol(site1_join_f1)
    site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                             FUN = function(x) as.numeric(as.character(x)))
    site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                             FUN = function(x) ifelse(x < 0, NA, x))
    site1_join_f1
  }
  rep <- function(site1_join_f1) {
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
  remov_99 <- function(site1_join_f1, high_number) {
    site1_join_f1$PM2.5 <- ifelse(as.numeric(as.character(site1_join_f1$PM2.5)) > 
                                    as.numeric(as.character(high_number)), 
                                  as.numeric(as.character(NA)), 
                                  as.numeric(as.character(site1_join_f1$PM2.5)))
    site1_join_f1
  }
  ratio <- function(site1_join_f1, high_number) {
    b <- as.numeric(as.character(site1_join_f1$PM10)) > as.numeric(as.character(high_number))
    site1_join_f1$PM10 <- ifelse(b, as.numeric(as.character(NA)), 
                                 as.numeric(as.character(site1_join_f1$PM10)))
    site1_join_f1$ratio <- as.numeric(as.character(site1_join_f1$PM2.5)) / 
      as.numeric(as.character(site1_join_f1$PM10))
    site1_join_f1$PM2.5 <- ifelse(site1_join_f1$ratio >= 1, 
                                  as.numeric(as.character(NA)), 
                                  as.numeric(as.character(site1_join_f1$PM2.5)))
    site1_join_f1$PM10 <- ifelse(site1_join_f1$ratio >= 1, 
                                 as.numeric(as.character(NA)), 
                                 as.numeric(as.character(site1_join_f1$PM10)))
    site1_join_f1
  }
  outlier <- function(site1_join_f1, name, date, eq) {
    site1_join_f1 <- site1_join_f1 %>%
      group_by(day) %>%
      mutate_all(funs(mean, sd), na.rm = TRUE) %>%
      ungroup() %>%
      dplyr::select(date, day, everything(), -date_mean, -date_sd)
    tseries_df <- site1_join_f1 %>%
      select(date)
    for(i in names(name)) {
      data_list <- site1_join_f1 %>% 
        dplyr::select(starts_with(i))
      if(i == "NO") {
        data_list <- data_list %>%
          dplyr::select(- contains(c("NO2", "NOx")))
        mean <- paste0(i, "_mean")
        sd <- paste0(i, "_sd")
      } else if (i == "O") {
        data_list <- data_list %>%
          dplyr::select(- contains(c("Ozone")))
        mean <- paste0(i, "_mean")
        sd <- paste0(i, "_sd")
      } else {
        mean <- paste0(i, "_mean")
        sd <- paste0(i, "_sd")
      }
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
      mutate(day = as.Date(date, format = '%Y-%m-%d', tz = "Asia/Kolkata")) %>%
      dplyr::select(date, day, everything(), -contains(c("_sd", "_mean")))
    tseries_df <- data.frame(date)
    site1_join_f1 <- left_join(tseries_df, site1_join_f1, by = "date")
    site1_join_f1
  }
  CPCB_f <- reactive({
    per1 <- input$per
    if (is.null(input$file1)) {
      return(NULL)
    } else {
      if(input$type == "cpcb") {
        c(all, date) := cpcb(input$file1$datapath, input$file)
        all <- data.frame(all) 
        date <- data.frame(date)
      } else if(input$type == "an") {
        c(all, date) := an(input$file1$datapath)
        all <- data.frame(all) 
        date <- data.frame(date)
      } else if (input$type == "oaq") {
        c(all, date) := openaq(input$file1$datapath)
        all <- data.frame(all) 
        date <- data.frame(date)
      }
      
      site1_join_f1 <- all %>%
        mutate(day = as.Date(date, format = '%Y-%m-%d', tz = "Asia/Kolkata")) %>%
        dplyr::select(date, day, everything())
      
      if(input$remove_9) {
        site1_join_f1 <- neg(site1_join_f1)
      } else { site1_join_f1 }  
      
      if(input$repeated) {
        site1_join_f1 <- rep(site1_join_f1)
      } else { site1_join_f1 }  
      
      name <- site1_join_f1 %>%
        dplyr::select(everything(), -day, -date)
      site1_join_f1 <- site1_join_f1 %>%
        mutate(ratio = NA) %>%
        dplyr::select(date, day, everything())
      if(input$exclude) {
        site1_join_f1 <- outlier(site1_join_f1, name, date, input$ey)
      } else { site1_join_f1 }  
      site1_join_f1 <- site1_join_f1 %>%
        dplyr::select(date, day, everything())
      col_interest <- 3:ncol(site1_join_f1)
      site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                               FUN = function(x) as.numeric(as.character(x)))
      if(input$percent) {
        tseries_df <- data.frame(date)
        for(i in names(name)){
          data_list <- site1_join_f1 %>% 
            dplyr::select(date, day, starts_with(i))
          if(i == "NO") {
            data_list <- data_list %>%
              dplyr::select(-contains(c("NO2", "NOx")))
          } else if(i == "O") {
            data_list <- data_list %>%
              dplyr::select(-contains(c("Ozone")))
          } else {
            NULL
          }
          data_list <- data_list %>% 
            group_by(day) %>%
            mutate_at(vars(contains(i)), list(no_hour = ~ sum(!is.na(.)))) %>%
            dplyr::select(-contains(c("_sd_no_hour", "_mean_no_hour")))
          old_no <- paste0(i, "_no_hour")
          names(data_list)[names(data_list) == old_no] <- 'no_hour'
          if(input$file == "15 min") {
            time_avg = 96
          } else if(input$file == "30 min") {
            time_avg = 48
          } else if(input$file == "60 min") {
            time_avg = 24
          }
          data_list <- subset(data_list, no_hour >= ((per1 / 100) * time_avg))
          data_list[ , c('day', 'no_hour')] <- list(NULL)
          tseries_df <- left_join(tseries_df, data_list, by = "date")
        }
        site1_join_f1 <- tseries_df %>%
          mutate(day = as.Date(date, format = '%Y-%m-%d', tz = "Asia/Kolkata")) 
      } else { site1_join_f1 }
      
      if("PM2.5" %in% colnames(site1_join_f1)) {
        site1_join_f1 <- remov_99(site1_join_f1, input$high_number)
        if("PM10" %in% colnames(site1_join_f1))
        {
          site1_join_f1 <- ratio(site1_join_f1, input$high_number)
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
  data_plot <- eventReactive(input$ts, {
    data <- CPCB_f()
    if(input$avg_hour3 == "daily3") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_box <- eventReactive(input$box, {
    data <- CPCB_f()
    if(input$avg_hour3 == "daily3") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_boxt <- eventReactive(input$boxt, {
    data <- CPCB_f()
    if(input$avg_hour3 == "daily3") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_mbox <- eventReactive(input$mbox, {
    data <- CPCB_f()
    if(input$avg_hour3 == "daily3") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_tv <- eventReactive(input$tv, {
    data <- CPCB_f()
    if(input$avg_hour4 == "daily4") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_cp <- eventReactive(input$cp, {
    data <- CPCB_f()
    if(input$avg_hour4 == "daily4") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_qq <- eventReactive(input$qq, {
    data <- CPCB_f()
    if(input$avg_hour2 == "daily2") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_freq <- eventReactive(input$freq, {
    data <- CPCB_f()
    if(input$avg_hour2 == "daily2") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_reg <- eventReactive(input$reg, {
    data <- CPCB_f()
    if(input$avg_hour1 == "daily1") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_mreg <- eventReactive(input$mulreg, {
    data <- CPCB_f()
    if(input$avg_hour1 == "daily1") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { data }
    return(data)
  })
  data_diurnal <- eventReactive(input$diurnal, {
    data <- CPCB_f()
    if(input$avg_hour3 == "daily3") {
      data <- openair::timeAverage(data, avg.time = "day")
    } else { 
      data <- data %>%
        mutate(hour = format(date, "%H"),
               month = format(date, "%b"))
      data <- data %>%
        dplyr::select(hour, month, "y" = input$palleInp) 
      if(input$diur == "mesd" && input$diurn == "all") {
        data <- data %>%
          select(hour, y) %>%
          group_by(hour) %>%
          summarise_all(funs(mean, sd), na.rm = TRUE)
      } else if(input$diur == "mediq" && input$diurn == "all") {
        data <- data %>%
          select(hour, y) %>%
          group_by(hour) %>%
          summarise_all(funs(median, p25 = quantile(., .25), p75 = quantile(., .75)), na.rm = TRUE)
      } else if (input$diur == "mediq" && input$diurn == "mon") {
        data <- data %>%
          group_by(month, hour) %>%
          summarise_all(funs(median, p25 = quantile(., .25), p75 = quantile(., .75)), na.rm = TRUE)
      } else if (input$diur == "mesd" && input$diurn == "mon") {
        data <- data %>%
          group_by(month, hour) %>%
          summarise_all(funs(mean, sd), na.rm = TRUE)
      } 
      }
    return(data)
  })
  output$download_diurnal <- downloadHandler(
    filename <- function() {"diurnal_data.csv"},
    content <- function(fname) {
      data <- data_diurnal()
      write.csv(data, fname)
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

  theme2 <- reactive({
    theme2 <- list(theme_minimal(),
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
      x <- shapiro.test(y)
    } else {
      x <- ad.test(y)
    }
    x
  })
  # kenda <- reactive({
  #   data <- data_freq()
  #   y <- as.numeric(as.character(data[[input$palleInp2]]))
  #   y <- as.ts(y)
  #   c <- trend::mk.test(y)
  #   c
  # })
  
  output$normality_test <- renderPrint({
    if (is.null(input$file1)) { "No file" }
    else {
      normalilty_t()
    }
  })
  # output$kendal_test <- renderPrint({
  #   if (is.null(input$file1)) { "No file" }
  #   else {
  #     kenda()
  #   }
  # })
  output$plot1 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_plot()
      y <- as.numeric(as.character(data[[input$palleInp]]))
      ggplot(data, aes(as.POSIXct(date), y)) +
        labs(y = input$ts_y, title = input$ts_mt,
             x = "") + theme2() + geom_line(size = 0.6, color = "seagreen")
    }
  })
  output$plot2 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- CPCB_f()
      data <- data %>%
        mutate(hour = format(date, "%H"),
               month = format(date, "%b"))
      data <- data %>%
        dplyr::select(hour, month, "y" = input$palleInp) 
      data$hour <- as.numeric(as.character(data$hour))
      if(input$avg_hour3 == "daily3") {
        NULL
      } else if (input$diur == "mesd" && input$diurn == "all") { 
        data <- data %>%
          select(hour, y) %>%
          group_by(hour) %>%
          summarise_all(funs(mean, sd), na.rm = TRUE)
        ggplot(data, aes(hour, mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                                                      color = "seagreen") + 
          scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
          labs(y = input$diurnal_y, x = "hour of the day", title = input$diurnal_mt) + 
          theme2() + geom_line(size = 0.6, color = "seagreen")
      } else if (input$diur == "mediq" && input$diurn == "all") {
        data <- data %>%
          select(hour, y) %>%
          group_by(hour) %>%
          summarise_all(funs(median, p25 = quantile(., .25), p75 = quantile(., .75)), na.rm = TRUE)
        ggplot(data, aes(hour, median)) + geom_errorbar(aes(ymin = p25, ymax = p75), 
                                                      color = "seagreen") + 
          scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
          labs(y = input$diurnal_y, x = "hour of the day", title = input$diurnal_mt) + 
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
    }
  })
  output$plot3 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_box()
      
      y <- as.numeric(as.character(data[[input$palleInp]]))
      ggplot(data, aes(x = reorder(format(date,'%b %Y'), date), y)) + 
        stat_summary(fun.data = f(), colour = "seagreen", geom = "boxplot", 
                     width = 0.4, size = 1) + 
        labs(y = input$box_y, x = "", title = input$box_mt) + 
        stat_summary(aes(y = y), fun.y = "mean", colour = "seagreen", 
                     geom = "point", size = 4)  +
        theme2() + theme(axis.text.x = element_text(size = 10, face = "bold", angle = 90))
    }
  })
  output$plot9 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_mbox()
      y <- as.numeric(as.character(data[[input$palleInp]]))
      ggplot(data, aes(x = reorder(format(date,'%b'), date), y)) +
        stat_summary(fun.data = f(), colour = "seagreen", geom = "boxplot",
                     width = 0.4, size = 1) +
        labs(y = input$box_my, x = "", title = input$box_mmt) +
        stat_summary(aes(y = y), fun.y = "mean", colour = "seagreen",
                     geom = "point", size = 4)  +
        theme2() + theme(axis.text.x = element_text(size = 13, face = "bold"))
    }
  })
  output$plot4 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_tv()
      y <- as.numeric(as.character(data[[input$palleInp1]]))
      if(input$stat_tv == "mean") {
        openair::timeVariation(data, pollutant = input$palleInp1, 
                               par.settings = list(fontsize = list(text = 15)))
      } else if(input$stat_tv == "median") {
        openair::timeVariation(data, pollutant = input$palleInp1, stati = "median", 
                               conf.int = c(0.75, 0.99),
                               par.settings = list(fontsize = list(text = 15)))
      }
      
    }
  })
  output$plot5 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_cp()
      y <- as.numeric(as.character(data[[input$palleInp1]]))
      openair::calendarPlot(data, pollutant = input$palleInp1, main = input$cp_mt,
                            cols = openColours(c("seagreen", "yellow", "red"), 10),
                            par.settings = list(fontsize = list(text = 15)))
    }
  })
  output$plot6 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_freq()
      y <- as.numeric(as.character(data[[input$palleInp2]]))
      ggplot(data, aes(x = y)) +
        geom_density(color = "deepskyblue", fill = "lightblue") +
        labs(y = "density", x = input$freq_x, title = input$freq_mt) + theme2()
    }
  })
  output$plot7 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_qq()
      y <- as.numeric(as.character(data[[input$palleInp2]]))
      ticks <- qnorm(c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
      labels <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)
      ggplot(data, aes(sample = log10(y))) +
        stat_qq(size = 2, geom = 'point', color = "deepskyblue") +
        stat_qq_line(size = 1, linetype = 2) + 
        scale_x_continuous(breaks = ticks, labels = labels) +
        labs(x = "Emperical percentile",
             y =  input$qq_y, title = input$qq_mt) + theme2()
    }
  })
  output$plot8 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_reg()
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
        geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, linetype = "dashed") + 
        geom_point(alpha = 0.5, color = "red") + 
        geom_smooth(method = lm, size = 1.2, se = FALSE, formula = y ~ x, color = "deepskyblue") +
        labs(x = input$reg_x,
             y = input$reg_y, title = input$reg_mt,
             subtitle = paste0("R square: ", r, "; Equation: ", reg_eqn(s))) + 
        theme2()
    }
  })
  output$plot10 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_boxt()
      data <- data %>%
        mutate(month = format(date, "%b")) %>%
        select(month, "y" = input$palleInp) %>%
        group_by(month) %>%
        summarise_all(funs(mean, sd), na.rm = TRUE)
      ggplot(data, aes(x = month, mean)) + 
        geom_bar(position = position_dodge(), stat = "identity", colour = 'lightblue', 
                 fill  = 'lightblue') + 
        labs(y = input$box_yt, x = "", title = input$box_mtt) + 
        geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2, position = 
                        position_dodge(.9), color = 'deepskyblue') +
        theme2() + theme(axis.text.x = element_text(size = 12, face = "bold"))
    }
  })
  
  # acf(z, lag.max = ((nrow(TimeSerie))/2), na.action = na.pass)
  
  lm_reg <- reactive({
    data <- data_mreg()
    y <- lm(as.formula(paste(input$DepVar1, " ~ ", paste(input$InDepVar1, collapse = "+"))), data)
    y
  })
  output$plot11 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- fortify(lm_reg())
      y <- as.numeric(as.character(data[[input$DepVar1]]))
      ggplot(data = data, aes(x = .fitted, y = y)) +
        geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, linetype = "dashed") + 
        geom_point(alpha = 0.5, color = "red") + 
        geom_smooth(method = lm, size = 1.2, se = FALSE, formula = y ~ x, color = "deepskyblue") +
        labs(x = "Fitted", y = input$DepVar1) + theme2()
    }
  })
  output$RegOut <- renderPrint({summary(lm_reg())})
  output$DepPrint <- renderPrint({paste("Dependent variable:", input$DepVar1)})
  output$IndPrint <- renderPrint({paste("Independent variables:", input$InDepVar1)})
  
  data_summary <- reactive({
    data <- data_joined()
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
        mutate(month  = format(day, "%b %Y"),
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
    filename <- function() {"stats_data.csv"},
    content <- function(fname) {
      data_summary <- data_summary()
      write.csv(data_summary, fname)
    })
  
}
## Run app
shinyApp(ui, server)


# runApp(display.mode = "showcase")



