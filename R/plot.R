#' Plot epi_contacts objects
#'
#' This function plots \code{\link{epi_contacts}} objects using various approaches. The default
#' method uses \code{\link{vis_epi_contacts}}.
#'
#' @export
#'
#' @param x an \code{\link{epi_contacts}} object
#'
#' @param y a character string indicating the plotting method to be used
#'
#' @param ... further arguments passed to the plotting method
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @seealso \code{\link{vis_epi_contacts}}, which uses the package \code{visNetwork}.
#'
plot.epi_contacts <- function(x, y=c("visNetwork"), ...){
    ## checks
    y <- match.arg(y)

    ## make plots
    if (y=="visNetwork") {
        return(vis_epi_contacts(x, ...))
    }

}
