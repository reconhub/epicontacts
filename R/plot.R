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
#' @examples
#' ## example using MERS outbreak in Korea, 2014
#' head(mers_kor_14[[1]])
#' head(mers_kor_14[[2]])
#'
#' x <- make_epi_contacts(linelist=mers_kor_14[[1]],
#' contacts=mers_kor_14[[2]], directed=TRUE)
#'
#' \dontrun{
#' plot(x)
#' plot(x, group="place_infect")
#' plot(x, group="loc_hosp", legend_max=20, annot=TRUE)
#' }
plot.epi_contacts <- function(x, y=c("visNetwork"), ...){
    ## checks
    y <- match.arg(y)

    ## make plots
    if (y=="visNetwork") {
        return(vis_epi_contacts(x, ...))
    }

}
