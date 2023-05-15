#' Create igraph object from contact data
#'
#' This function creates an igraph object from a given \code{\link{epicontacts}}
#' object containing a 'contacts' dataframe.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param x An \code{\link{epicontacts}} object.
#'
#' @param ... Further arguments passed to \code{as.igraph}
#'
#' @return An \code{igraph} object (from the \code{igraph} package).  Note: any
#'   column called "name" in the original linelist will be stored as a new
#'   vertex attribute in the \code{igraph} object named 'epicontacts_name'.
#'   This is due to the inherent behaviour of igraph creating its own 'name'
#'   vertex attribute.
#'
#' @importFrom igraph as.igraph
#'
#' @examples
#'
#' if (require(outbreaks) && require(igraph)) {
#' ## build data
#'
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                       id = "case_id", to = "case_id", from ="infector",
#'                       directed = TRUE)
#'
#'
#' ## subset data - keep 50 cases from linelist with contacts
#'
#' ids <- get_id(x, "common")[1:50]
#' ids
#' x <- x[ids, ids]
#'
#'
#' ## make igraph object with associated attributes from epicontacts object
#'
#' net <- as.igraph(x)
#' net
#' plot(net, vertex.label = "", vertex.size = 10,
#'      vertex.color = cases_pal(50))
#' }

as.igraph.epicontacts <- function(x, ...){

  ## Create vertex dataframe using combination of linelist and contacts

  all_ids <- data.frame(id = get_id(x, "all"), stringsAsFactors = FALSE)
  verts <- dplyr::full_join(x$linelist, all_ids, by = "id")


  ## Checking if a "name" column exists

  if ("name" %in% colnames(verts)) {
    verts$epicontacts_name <- verts$name
    verts$name <- NULL
  }
  missing_contacts <- anyNA(x$contacts$from) || anyNA(x$contacts$to)
  missing_vertex   <- anyNA(x$linelist$id)
  if (missing_contacts && !missing_vertex) {
    verts[nrow(verts) + 1, 1] <- NA
  }
  ## Creating igraph object

  net <- igraph::graph_from_data_frame(x$contacts, vertices = verts,
                                       directed = x$directed)

  igraph::vertex_attr(net)$id <- igraph::vertex_attr(net)$name

  return(net)
}









