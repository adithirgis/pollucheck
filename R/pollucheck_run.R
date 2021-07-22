#' @title pollucheck: Explore Open-Source Air Quality Data
#' @description Helps to analyze and visualize open source air quality data available
#' online which are - Central Pollution Control Board (India), OpenAQ, AirNow.
#' An example dataset is already loaded for user to walk through all the features of the app.  
#' If any assistance is required with downloading dataset, check the README file on GitHub 
#' This app can process all parameters except wind direction which when downloaded at
#' 1 hour is processed correctly. When wind direction any other time resolution 
#' (e.g. - daily average) is not averaged correctly as it is a vector and 
#' cannot be averaged using simple arithmetic mean.
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
