#' @title pollucheck: Explore Open-Source Air-Quality Data
#' @description This app helps to analyze and visualize open source air
#' quality data available.  An example dataset is already loaded to help
#' you walk through all the features of the app.  If you need help with
#' downloading your own, check the ReadMe file on github.  This app can
#' process all parameters except Wind Direction which when downloaded at
#' 1 hour is processed correctly, any other time-resolution is used then
#' the app will not be process wind direction correctly.
#' @keywords pollucheck
#' @examples
#' \dontrun{
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
