#' Plot epicontacts objects
#'
#' This function plots \code{\link{epicontacts}} objects using various
#' approaches. The default method uses \code{\link{vis_epicontacts}}.
#'
#' @export
#'
#' @param x An \code{\link{epicontacts}} object
#'
#' @param group An integer or a character string indicating which attribute column
#' in the linelist should be used to color the nodes.
#'
#' @param thin A logical indicating if the data should be thinned so that only
#'     cases with contacts should be plotted.
#'
#' @param method A character string indicating the plotting method to be used;
#' available values are "visNetwork" and "graph3D"; see details.
#' 
#' @param ... Further arguments passed to the plotting methods.
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @details This function is merely a wrapper for other plotting functions in
#' the package, depending on the value of \code{method}:
#'
#' \itemize{
#'
#' \item \code{visNetwork}: calls the function \code{\link{vis_epicontacts}}
#'
#' \item \code{graph3D}: calls the function \code{\link{graph3D}}
#'
#' }
#'
#' @importFrom graphics plot
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
#' plot(x, "place_infect")
#' plot(x, "loc_hosp", legend_max = 20, annot = TRUE)
#' plot(x, 4)
#' plot(x, 4, method = "graph3D")
#' }
#' }
plot.epicontacts <- function(x, group = "id",
                             method = c("visNetwork", "graph3D"),
                             thin = TRUE, ...){
    ## checks
    
    if (thin) {
        x <- thin(x)
    }
    
    method <- match.arg(method)

    if (is.numeric(group) && length(group) > 0L) {
        group <- names(x$linelist)[group][1L]
    }

    
    ## make plots
    
    if (method == "visNetwork") {
        return(vis_epicontacts(x, group = group, ...))
    }

    if (method == "graph3D") {
        return(graph3D(x, group = group, ...))
    }
    
}
