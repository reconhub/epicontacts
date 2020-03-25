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
#' available values are "visNetwork", "graph3D" and "temporal"; see details.
#'
#' @param x_axis If a temporal axis is to be displayed, a character string
#'   indicating which field of the linelist data should be used to specify the x
#'   axis position (must be numeric or Date).
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
#' \item \code{temporal}: calls the function
#' \code{link{vis_temporal_interactive}} or \code{link{vis_temporal_static}}
#' depending on what argument is provide to \code{method}.
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
                             method = c("visNetwork", "graph3D", "ggplot"),
                             x_axis = NULL,
                             ...){
  
  method <- match.arg(method)

  if(method == "ggplot" && is.null(x_axis)) {
    stop("ggplot method only works if x_axis is specified")
  }

  if(method == "graph3D" && !is.null(x_axis)) {
    stop("x_axis cannot be specified for graph3D")
  }

  if(method == "graph3D") {
    out <- graph3D(x, node_color = node_color, ...)
  } else if(method == 'visNetwork') {
    if(is.null(x_axis)) {
      out <- vis_epicontacts(x, node_color = node_color, ...)
    } else {
      out <- vis_temporal_interactive(x, x_axis = x_axis,
                                      node_color = node_color, ...)
    }
  } else if(method == "ggplot") {
    out <- vis_temporal_static(x, x_axis = x_axis,
                               node_color = node_color, ...)
  }
  
  return(out)
  
}
