#' Plot epi_contacts objects using visNetwork
#'
#' This function plots \code{\link{epi_contacts}} objects using \code{visNetwork}.
#'
#' @export
#'
#' @param x an \code{\link{epi_contacts}} object
#'
#' @param y a character string indicating the plotting method to be used
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @seealso \code{\linkvis_epi_contacts}}, which uses the package \code{visNetwork}.
#'
plot.epi_contacts <- function(x, y=c("visNetwork"), ...){
    ## checks
    y <- match.arg(y)

    ## make plots
    if (y=="visNetwork") {
        return(vis_epi_contacts(x, ...))
    }

}
