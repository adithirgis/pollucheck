#' @title pollucheck: Explore Open-Source Air-Quality Data
#' @description Helps to analyze and visualize open source air quality data available
#' online which are - Central Pollution Control Board (India), OpenAQ, AirNow.
#' An example dataset is already loaded for user to walk through all the features of the app.  
#' If any assistance is required with downloading dataset, check the ReadMe file on github. 
#' This app can process all parameters except Wind Direction which when downloaded at
#' 1 hour is processed correctly, and if any other time-resolution is used then
#' the app will not process wind direction correctly.
#' @keywords pollucheck
#' @examples
#' if(interactive()){
#' library(pollucheck)
#' pollucheck_run()
#' }
#'
#' @export
pollucheck_run <- function() {
  Directory <- system.file("shiny", package = "pollucheck")
  if (Directory == "") {
    stop("Try reinstalling the package `pollucheck`.", call. = FALSE)
  }
  shiny::runApp(Directory, display.mode = "normal")
}
