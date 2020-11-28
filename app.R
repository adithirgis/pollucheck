library(shiny)
library(Cairo)
library(DT)
library(plotly)
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(htmltools)
library(tidyverse)
library(leaflet)
library(XML)
library(shinyjs)
library(lubridate)
library(zoo)
library(caTools)
library(xts)
library(readr)
library(openair)
library(xlsx)
library(openxlsx)
library(rsconnect)

ui <- fluidPage(
  h1("Analyse open source air quality data"),
  tags$head(
    tags$style(HTML(".sidebar {
                    height : 10vh; overflow-y : auto; font-size : 14px;
                    }" ,
                    ".shiny-output-error-validation {
                    color : red; font-size : 14px;
                    }"
    ))),
  sidebarLayout(position = "left",
                sidebarPanel(width = 3,
                             conditionalPanel(condition = "input.tabs1 == 3",
                                              tags$hr(),
                                              selectInput("palleInp", "Plot this parameter",
                                                          "Select"),
                                              tags$hr(),
                                              
                                              actionButton("ts", "Time Series"),
                                              tags$hr(),
                                              actionButton("diurnal", "Diurnal Plot"),
                                              tags$hr(),
                                              actionButton("box", "Monthly Box Plot"),
                                              tags$hr()),
                             conditionalPanel(condition = "input.tabs1 == 2",
                                              tags$hr(),
                                              selectInput("avg",
                                                          label = "Averaging period",
                                                          c("None" = "no",
                                                            "Daily" = "day",
                                                            "Monthly" = "month"),
                                                          selected = "None"),
                                              tags$hr(),
                                              downloadButton('download_stats', "Download as csv")),
                             conditionalPanel(condition = "input.tabs1 == 4",
                                              tags$hr(),
                                              selectInput("palleInp1", "Plot this parameter",
                                                          "Select"),
                                              tags$hr(),
                                              actionButton("cp", "Calendar Plot"),
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
                                              checkboxInput('exclude', 'Cleaning based on Mean and Std Dev'),
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
                                              actionButton("hourly", "Show Data"),
                                              downloadButton('download', "Download as csv"),
                                              tags$hr())),
                mainPanel(
                  tags$head(
                    tags$style(type = 'text/css',
                               ".nav-tabs {
                               font-size: 18px
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
                                plotOutput("plot2", width = 800),
                                plotOutput("plot3", width = 800)),
                              tabPanel(
                                value = 4,
                                title = "openair package plots",
                                plotOutput("plot5", height = 600),
                                plotOutput("plot4", height = 600)))
  )))


server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2, shiny.launch.browser = TRUE)
  
  file_name_CPCB <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }else {
      name_CPCB <- gsub(".xlsx$", "", basename(input$file1$name))
      return(name_CPCB)
    }
  })
  
  CPCB_f <- reactive({
    name_CPCB <- file_name_CPCB()
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
      # the end date was used from each fiel to create a time series dataframe
      PM <- data.frame(dt_s$`0`) %>%
        mutate(date = as.POSIXct(date, format = '%d-%m-%Y %H:%M:%S', tz = "Asia/Kolkata"))
      ye <- format(PM[1, "date"], format = "%Y")
      x1 <- as.POSIXct(paste0(ye, "-01-01 01:00:00"), 
                       format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
      ye <- format(tail(PM$date, n = 3)[1], format = "%Y")
      x2 <- as.POSIXct(paste0(ye, "-12-31 23:00:00"), 
                       format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
      date <- seq(
        from = as.POSIXct(x1, tz = "Asia/Kolkata"),
        to = as.POSIXct(x2, tz = "Asia/Kolkata"),
        by = input$file
      ) 
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
      } else if(input$type == "an") {
        trial <- read.csv(input$file1$datapath, header = TRUE, sep = ",", 
                          row.names = NULL)
        trial <- trial %>%
          dplyr::select("date" = Date..LT., "PM2.5" = Raw.Conc., "Valid" = QC.Name) %>%
          mutate(date  = as.POSIXct(date, format = '%Y-%m-%d %I:%M %p', tz = "Asia/Kolkata")) %>%
          filter(Valid == "Valid")
        trial$Valid <- NULL
        ye <- format(trial[1, "date"], format = "%Y")
        x1 <- as.POSIXct(paste0(ye, "-01-01 01:00:00"), 
                         format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
        ye <- format(tail(trial$date, n = 3)[1], format = "%Y")
        x2 <- as.POSIXct(paste0(ye, "-12-31 23:00:00"), 
                         format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
        date <- seq(
          from = as.POSIXct(x1, tz = "Asia/Kolkata"),
          to = as.POSIXct(x2, tz = "Asia/Kolkata"),
          by = "60 min"
        ) 
        tseries_df <- data.frame(date)
        all <- left_join(tseries_df, trial, by = "date")
      } else if (input$type == "oaq") {
        trial <- read.csv(input$file1$datapath, header = TRUE, sep = ",", 
                          row.names = NULL)
        trial <- trial %>%
          dplyr::select("date" = local, "parameter" = parameter, "value" = value) %>%
          mutate(date  = as.POSIXct(date, format = '%Y-%m-%dT%H:%M:%S+05:30', tz = "Asia/Kolkata"))
        
        ye <- format(trial[1, "date"], format = "%Y")
        x1 <- as.POSIXct(paste0(ye, "-01-01 01:00:00"), 
                         format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
        ye <- format(tail(trial$date, n = 3)[1], format = "%Y")
        x2 <- as.POSIXct(paste0(ye, "-12-31 23:00:00"), 
                         format = '%Y-%m-%d %H:%M:%S', tz = "Asia/Kolkata")
        date <- seq(
          from = as.POSIXct(x1, tz = "Asia/Kolkata"),
          to = as.POSIXct(x2, tz = "Asia/Kolkata"),
          by = "15 min"
        ) 
        tseries_df <- data.frame(date)
        trial <- trial %>%
          pivot_wider(names_from = parameter, values_from = value)
        trial <- trial[order(trial$date), ]
        all <- left_join(tseries_df, trial, by = "date") 
        all$hour <- lubridate::ceiling_date(all$date, "hour")
        all <- all %>%
          group_by(hour) %>%
          summarise_all(funs(mean), na.rm = TRUE) %>%
          select(everything(), - date) %>%
          select("date" = hour, everything())
        if("pm25" %in% colnames(all))
        {
          all <- all %>%
            select(date, everything(), "PM2.5" = pm25)
        }
        if("nox" %in% colnames(all))
        {
          all <- all %>%
            select(date, everything(), "NOx" = nox)
        }
        if("no2" %in% colnames(all))
        {
          all <- all %>%
            select(date, everything(), "NO2" = no2)
        }
        if("no" %in% colnames(all))
        {
          all <- all %>%
            select(date, everything(), "NO" = no)
        }
      }
      site1_join_f1 <- all %>%
        mutate(day = as.Date(date, format = '%Y-%m-%d', tz = "Asia/Kolkata")) %>%
        dplyr::select(date, day, everything())
      if(input$remove_9) {
        col_interest <- 3:ncol(site1_join_f1)
        site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                       FUN = function(x) as.numeric(as.character(x)))
        site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                       FUN = function(x) ifelse(x < 0, NA, x))
      } else { site1_join_f1 }  
      ### Check for consecutive repeated value and remove them using consecutive 
      # difference as 0
      if(input$repeated) {
        col_interest <- 3:ncol(site1_join_f1)
        site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[, col_interest], 
                                                 FUN = function(j)
                                                   ifelse(c(FALSE, diff(as.numeric(j), 1, 1) == 0), 
                                                          NA, j))
        site1_join_f1[ , col_interest] <- sapply(X = site1_join_f1[ , col_interest], 
                                                 FUN = function(x) 
                                                   as.numeric(as.character(x)))
      } else { site1_join_f1 }  
      name <- site1_join_f1 %>%
        dplyr::select(everything(), -day, -date)
      site1_join_f1 <- site1_join_f1 %>%
        mutate(ratio = NA) %>%
        dplyr::select(date, day, everything())
      
      if(input$exclude) {
        ### Now calculate the Mean and SD for all parameters to check for some 
        # conditions
        site1_join_f1 <- site1_join_f1 %>%
          group_by(day) %>%
          mutate_all(funs(mean, sd), na.rm = TRUE) %>%
          ungroup() %>%
          dplyr::select(date, day, everything(), -date_mean, -date_sd)
        tseries_df <- data.frame(date)
        for(i in names(name)){
          data_list <- site1_join_f1 %>% 
            dplyr::select(date, - day, starts_with(i))
          ### Check if you have similar names matching eg - NO
          if(i == "NO") {
            data_list <- data_list %>%
              dplyr::select(-contains(c("NO2", "NOx")))
            mean <- paste0(i, "_mean")
            sd <- paste0(i, "_sd")
          } else {
            mean <- paste0(i, "_mean")
            sd <- paste0(i, "_sd")
          }
          ### Remove empty rows
          data_list <- completeFun(data_list, c(i, mean, sd))
          x <- data_list[[i]]
          y <- grep("_mean", colnames(data_list))
          y <- data_list[[y]]
          z <- grep("_sd", colnames(data_list))
          z <- data_list[[z]]
          ### Apply the condition of removing values which are >< (Mean +- 3 * SD)
          if(!nrow(data_list)){
            NULL 
          } else {
            data_list[[i]] <- mapply(LLD, x, y, z, as.numeric(as.character(input$ey)))
          }
          tseries_df <- left_join(tseries_df, data_list, by = "date")
          
        }
        site1_join_f1 <- tseries_df %>%
          mutate(day = as.Date(date, format = '%Y-%m-%d', tz = "Asia/Kolkata")) %>%
          dplyr::select(date, day, everything(), -contains(c("_sd", "_mean")))
      } else { site1_join_f1 }  
      
      if(input$percent) {
        tseries_df <- data.frame(date)
        for(i in names(name)){
          data_list <- site1_join_f1 %>% 
            dplyr::select(date, day, starts_with(i))
          col_interest <- 3:ncol(data_list)
          data_list[ , col_interest] <- sapply(X = data_list[ , col_interest], 
                                               FUN = function(x) 
                                                 as.numeric(as.character(x)))
          if(i == "NO") {
            data_list <- data_list %>%
              dplyr::select(-contains(c("NO2", "NOx")))
          } else {
            NULL
          }
          ### Remove empty rows
          data_list <- completeFun(data_list, c(i))
          ### Apply the condition of removing values which are >< (Mean +- 3 * SD)
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
          } else {
            time_avg = 24
          }
          data_list <- subset(data_list, no_hour >= ((per1 / 100) * time_avg))
          data_list[ , c('day', 'no_hour')] <- list(NULL)
          tseries_df <- left_join(tseries_df, data_list, by = "date")
        }
        site1_join_f1 <- tseries_df %>%
          mutate(day = as.Date(date, format = '%Y-%m-%d', tz = "Asia/Kolkata")) 
      } else { site1_join_f1 }
      
      ### PM2.5 and PM10 ratio
      if("PM2.5" %in% colnames(site1_join_f1))
      {
        site1_join_f1$PM2.5 <- ifelse(as.numeric(as.character(site1_join_f1$PM2.5)) > input$high_number, 
                                      as.numeric(as.character(NA)), as.numeric(as.character(site1_join_f1$PM2.5)))
        
        ### If PM10 values exist then check the rario of PM2.5 / PM10 and if it is 
        # greater than 1 then remove those values
        if("PM10" %in% colnames(site1_join_f1))
        {
          b <- (site1_join_f1$PM10) > input$high_number
          site1_join_f1$PM10 <- ifelse(b, NA, site1_join_f1$PM10)
          site1_join_f1$ratio <- site1_join_f1$PM2.5 / site1_join_f1$PM10
          site1_join_f1$PM2.5 <- ifelse(site1_join_f1$ratio >= 1, NA, site1_join_f1$PM2.5)
          site1_join_f1$PM10 <- ifelse(site1_join_f1$ratio >= 1, NA, site1_join_f1$PM10)
        } else {
          site1_join_f1$ratio <- NA
        }
      } else { site1_join_f1 }
      site1_join_f1 <- site1_join_f1 %>%
        dplyr::select(date, day, everything())
    }
  })
  
  data_joined <- eventReactive(input$hourly, {
    data <- CPCB_f()
    return(data)
  })
  data_plot <- eventReactive(input$ts, {
    data <- CPCB_f()
    return(data)
  })
  data_diurnal <- eventReactive(input$diurnal, {
    data <- CPCB_f()
    return(data)
  })
  data_box <- eventReactive(input$box, {
    data <- CPCB_f()
    return(data)
  })
  data_tv <- eventReactive(input$tv, {
    data <- CPCB_f()
    return(data)
  })
  data_cp <- eventReactive(input$cp, {
    data <- CPCB_f()
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
  })
  
  output$table1 <- DT::renderDataTable({
    data_joined <- data_joined() 
    cols <- names(data_joined)[3:ncol(data_joined)]
    data_joined[ , cols] <- sapply(X = data_joined[ , cols], 
                                   FUN = function(x) as.numeric(as.character(x)))
    setDT(data_joined)
    data_joined[,(cols) := round(.SD, 2), .SDcols = cols]
    datatable(data_joined, options = list("pageLength" = 25)) %>% formatDate(1, "toLocaleString") 
     
  })
  
  output$download <- downloadHandler(
    filename <- function() {"data.csv"},
    content <- function(fname) {
      data_joined <- data_joined()
      write.csv(data_joined, fname)
    })
  
  
  theme1 <- reactive({
    theme1 <- list(geom_line(size = 0.6, color = "seagreen"),
                   theme_minimal(),
                   theme(legend.text = element_text(size = 18),
                         plot.title = element_text(size = 14, face = "bold"),
                         axis.title = element_text(size = 20, face = "bold"),
                         axis.text = element_text(size = 18, face = "bold"),
                         panel.border = element_rect(colour = "black",
                                                     fill = NA, size = 1.2)))
  })
  
  output$plot1 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_plot()
      y <- as.numeric(as.character(data[[input$palleInp]]))
      ggplot(data, aes(as.POSIXct(date), y)) +
        labs(y = input$palleInp,
             x = "") + theme1()
    }
  })
  
  output$plot2 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_diurnal()
      data <- data %>%
        mutate(hour = format(date, "%H")) 
      data <- data %>%
        dplyr::select(hour, "y" = input$palleInp) 
      data <- data %>%
        group_by(hour) %>%
        summarise_all(funs(mean, sd), na.rm = TRUE)
      data$hour <- as.numeric(as.character(data$hour))
      ggplot(data, aes(hour, mean)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                                                    color = "seagreen") + 
        scale_x_continuous(limits = c(-1, 24), breaks = c(0, 6, 12, 18)) +
        labs(y = input$palleInp, x = "hour of the day") + theme1()
    }
  })
  output$plot3 <- renderPlot({
    if (is.null(input$file1)) { NULL }
    else {
      data <- data_box()
      f <- function(x) {
        r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
        names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
        r
      }
      y <- as.numeric(as.character(data[[input$palleInp]]))
      ggplot(data, aes(x = reorder(format(date,'%b %Y'), date), y)) + 
        stat_summary(fun.data = f, colour = "seagreen", geom = "boxplot", 
                     width = 0.4, size = 1) + 
        labs(y = input$palleInp, x = "") + 
        stat_summary(aes(y = y), fun.y = "mean", colour = "seagreen", 
                     geom = "point", size = 4)  +
        theme_minimal() + theme(legend.text = element_text(size = 18),
                                axis.title = element_text(size = 20, face = "bold"),
                                axis.text.y = element_text(size = 18, face = "bold"),
                                axis.text.x = element_text(size = 10, face = "bold", angle = 90),
                                panel.border = element_rect(colour = "black",
                                                            fill = NA, size = 1.2))
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
      openair::calendarPlot(data, pollutant = input$palleInp1, main = input$palleInp1,
                            cols = openColours(c("seagreen", "yellow", "red"), 10),
                            par.settings = list(fontsize = list(text = 15)))
    }
  })
  
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
        mutate(month  = format(day, "%b %Y")) %>%
        dplyr::select(day, month, everything())
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


# ggplot(Ref2, aes(x = SE_median)) + 
# geom_histogram(aes(y = ..count..), fill = "deepskyblue", 
#                color = "black", bins = 30) +
#   labs(y = "count", x = "CO???- SE of the  medians / Median (using the drive pass means)") +
#   facet_grid(Road_type ~ Area) + theme_minimal() + 
#   theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
#         axis.title = element_text(size = 16, colour = "black", face = "bold"),
#         axis.text = element_text(size = 14, colour = "black", face = "bold"),
#         strip.text = element_text(size = 14, colour = "black", face = "bold")) +
#   scale_y_continuous() + scale_x_continuous(limits = c(0, 1.25), breaks = c (0, 0.5, 1)) +
#   geom_text(aes(label = paste0("n = ", name)), x = 1.0, y = 900, colour = "black", size = 4)


# fin_df_all$Road_type <- factor(fin_df_all$Road_type, levels = c("Highway", "Arterial",
#                                                                 "Residential"))
# cols <- c("Highway" = "maroon", "Arterial" = "orange", "Residential" = "steelblue")
# ticks <- qnorm(c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
# labels <- c(1, 5, 10, 25, 50, 75, 90, 95, 99) 
# p2 <- ggplot(fin_df_all, aes(sample = log10(BC_c), color = Road_type)) + 
#   stat_qq(size = 2, geom = 'point') + 
#   stat_qq_line(size = 1, linetype = 2) + scale_color_manual(values = cols) + 
#   scale_x_continuous(breaks = ticks, labels = labels) + 
#   labs(x = "Emperical percentile",
#        y = expression(paste("BC" ," (", mu, "g",~m^{-3}, ")"))) + theme_ARU +
#   theme(legend.text = element_blank())



# reg_eqn <- function(x) {
#   R_sq <- round(as.numeric(x$adj.r.squared), digits = 2)
#   int <- round(coef(x)[1], digits = 2)
#   slope <- round(coef(x)[2], digits = 2)
#   eqn <- paste("y = ", slope, "x + (", int, ")")
#   return(eqn)
# }
# m <- lm(PM2.5_02 ~ PM2.5_03, Final)
# s <- summary(m)
# r <- round(s$adj.r.squared, digits = 2)
# Final$diffSq <- (Final$PM2.5_02 - Final$PM2.5_03) ^ 2 
# mean_diff_sqr <- mean(Final$diffSq, na.rm = TRUE)
# rmse <- round(sqrt(mean_diff_sqr), digits = 2)
# 
# ggplot(data = Final, aes(x = PM2.5_03, y = PM2.5_02)) +
#   geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, linetype = "dashed") + geom_point(alpha = 0.3, color = "red") + annotate("text", label = reg_eqn(s), x = 30,  y = 80, size = 5) + scale_x_continuous(limits = c(0, 100)) +
#   scale_y_continuous(limits = c(0, 100)) +
#   geom_smooth(method = lm, size = 1.2, se = FALSE, formula = y ~ x) +
#   labs(x = "PM 2.5 - 603",
#        y = "PM 2.5 - 602",
#        subtitle = paste0("1 min measurements; N = 1090 ", "; RMSE:  ", rmse, " (µg/m³); R square: ", r)) + theme_minimal() + theme1 + theme(plot.subtitle = element_text(size = 12, face = "bold"),)
