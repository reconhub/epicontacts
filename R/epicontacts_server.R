#' Run the epicontacts Shiny app
#'
#' This function launches the epicontacts Shiny application on a local host. The app features:
#' \itemize{
#' \item{interactive network visualization}
#' \item{ability to upload new data (as .csv files in line list and contact list formats)}
#' \item{demonstration data from the \code{\link{mers_kor_14}} dataset}
#' \item{demonstration data from the \code{\link{ebola.sim}} dataset (100 records)}
#' }
#'
#' @export
#'
#' @author VP Nagraj (\email{vpnagraj@virginia.edu})
#'
#' @param ... further parameters to be passed to the \code{\link[shiny]{runApp}} function from the Shiny package
#'
#' @examples
#'
#' \dontrun{
#' epicontacts_server()
#'
#' epicontacts_server(display.mode = "showcase")
#' }

epicontacts_server <- function(...) {

    shiny::runApp(system.file("epicontacts_server", package = "epicontacts"), ...)

}