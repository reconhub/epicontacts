#' Plot epicontacts objects
#'
#' This function plots \code{\link{epicontacts}} objects using various
#' approaches. The default method uses \code{\link{vis_epicontacts}}.
#'
#' @export
#'
#' @param x An \code{\link{epicontacts}} object
#'
#' @param node_color An integer or a character string indicating which attribute column
#' in the linelist should be used to color the nodes.
#'
#' @param thin A logical indicating if the data should be thinned with \code{\link{thin}} so that only cases with contacts should be plotted.
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
plot.epicontacts <- function(x, node_color = "id",
                             method = c("visNetwork", "graph3D", "ggplot"),
                             thin = TRUE, ...){
  ## checks
  if (thin) {
    x <- thin(x)
  }

  method <- match.arg(method)

  if (is.numeric(node_color) && length(node_color) > 0L) {
    node_color <- names(x$linelist)[node_color][1L]
  }


  ## make plots

  if (method == "visNetwork") {
    if(missing(node_color)) {
      return(vis_epicontacts(x, ...))
    } else {
      return(vis_epicontacts(x, node_color = node_color, ...))
    }
  }

  if (method == "graph3D") {
    return(graph3D(x, node_color = node_color, ...))
  }

  if (method == "ggplot") {
    if(missing(node_color)) {
      return(vis_ggplot(x, ...))
    } else {
      return(vis_ggplot(x, node_color = node_color, ...))
    }
  }

  
}
