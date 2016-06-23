#' Plot epi_contacts objects using visNetwork
#'
#' This function plots \code{\link{epi_contacts}} objects using \code{visNetwork}.
#'
#' @export
#'
#' @param an \code{\link{epi_contacts}} object
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @importFrom visNetwork visNetwork visGroups visLegend visOptions visNodes
#' @importFrom magrittr "%>%"
#'
#' @return the same output as \code{visNetwork}
#'
#' @seealso \code{\link[visNetwork]{visNetwork}} in the package \code{visNetwork}.
#'
plot.epi_contacts <- function(x, y=c("visNetwork"), ...){
    ## checks
    y <- match.arg(y)

    ## make plots
    if (y=="visNetwork") {
        return(vis_epi_contacts(x, ...))
    }

}
