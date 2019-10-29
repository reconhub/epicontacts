#' Plot epicontacts objects
#'
#' This function plots \code{\link{epicontacts}} objects using various
#' approaches. The default method uses \code{\link{vis_epicontacts}}.
#'
#' @export
#'
#' @param x An \code{\link{epicontacts}} object
#'
#' @param node_color An index or character string indicating which field of the
#'   linelist should be used to color the nodes. If node color = 'R_i', the
#'   individual reproductive number for each case (i.e. number of outgoing
#'   infection/contacts) will be calculated and used to specify the node colour.
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
#' \item \code{ttree}: calls the function \code{link{vis_ttree}}
#'
#' \item \code{ggplot}: calls the function \code{link{vis_ggplot}}
#' 
#' }
#'
#' @importFrom graphics plot
#'
#' @seealso \code{\link{vis_epicontacts}}, which uses the package
#'   \code{visNetwork}, and \code{\link{codeawesome}} for icon codes.
#'
#'
#' @examples
#' if (require(outbreaks)) {
#' ## example using MERS outbreak in Korea, 2014
#' head(mers_korea_2015[[1]])
#' head(mers_korea_2015[[2]])
#'
#' x <- make_epicontacts(linelist = mers_korea_2015[[1]],
#'                       contacts = mers_korea_2015[[2]], directed=TRUE)
#'
#' \dontrun{
#' plot(x)
#' plot(x, "place_infect")
#' plot(x, "loc_hosp", legend_max = 20, annot = TRUE)
#' plot(x, "place_infect", node_shape = "sex",
#'      shapes = c(M = "male", F = "female"))
#' plot(x, 4)
#' plot(x, 4, method = "graph3D")
#' }
#' }
plot.epicontacts <- function(x,
                             node_color = "id",
                             method = c("visNetwork", "graph3D", "ggplot", "ttree"),
                             ...){
  
  method <- match.arg(method)

  lst <- list(visNetwork = vis_epicontacts,
              graph3D = graph3D,
              ggplot = vis_ggplot,
              ttree = vis_ttree)

  out <- lst[[method]](x, node_color = node_color, ...)

  return(out)
  
}
