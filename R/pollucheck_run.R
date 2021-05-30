#' @title pollucheck: Explore Open-Source Air-Quality Data
#' @description 
#' @keywords pollucheck
#' @examples
#' \dontrun{
#' library(pollucheck)
#' pollucheck::pollucheck_run()
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
