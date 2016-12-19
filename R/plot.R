#' Plot epicontacts objects
#'
#' This function plots \code{\link{epicontacts}} objects using various
#' approaches. The default method uses \code{\link{vis_epicontacts}}.
#'
#' @export
#'
#' @param x an \code{\link{epicontacts}} object
#'
#' @param y a character string indicating the plotting method to be used
#'
#' @param thin A logical indicating if the data should be thinned so that only
#'     cases with contacts should be plotted.
#' 
#' @param ... further arguments passed to the plotting method
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @seealso \code{\link{vis_epicontacts}}, which uses the package \code{visNetwork}.
#'
#' @examples
#' if (require(outbreaks)) {
#' ## example using MERS outbreak in Korea, 2014
#' head(mers_korea_2015[[1]])
#' head(mers_korea_2015[[2]])
#'
#' x <- make_epicontacts(linelist = mers_korea_2015[[1]],
#'                        contacts = mers_korea_2015[[2]], directed=TRUE)
#'
#' \dontrun{
#' plot(x)
#' plot(x, group = "place_infect")
#' plot(x, group = "loc_hosp", legend_max = 20, annot = TRUE)
#' }
#' }
plot.epicontacts <- function(x, y = c("visNetwork"), thin = TRUE, ...){
    ## checks
    if (thin) {
        x <- thin(x)
    }
    y <- match.arg(y)

    ## make plots
    if (y=="visNetwork") {
        return(vis_epicontacts(x, ...))
    }

}
