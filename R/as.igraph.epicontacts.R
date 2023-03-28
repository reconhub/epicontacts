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
#' @param na_rm A logical indicating whether linelist and contacts elements with
#'   NA values for case IDs should be removed. If these elements are kept, NAs
#'   are coerced to character and assumed to represent a single case.
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

as.igraph.epicontacts <- function(x, na_rm = FALSE, ...){

  ## Test for NA
  missing_contacts <- is.na(x$contacts$from) | is.na(x$contacts$to)
  missing_vertex   <- is.na(x$linelist$id)

  if(na_rm) {
    x$contacts <- subset(x$contacts, !missing_contacts)
    x$linelist <- subset(x$linelist, !missing_vertex)
  }

  ## Create vertex dataframe using combination of linelist and contacts
  all_ids <- data.frame(id = get_id(x, "all"), stringsAsFactors = FALSE)
  verts <- merge(x$linelist, all_ids, by = 'id', all = TRUE, sort = FALSE)

  ## Checking if a "name" column exists
  if ("name" %in% colnames(verts)) {
    verts$epicontacts_name <- verts$name
    verts$name <- NULL
  }

  ## Add NA vertex if necessary
  if (any(missing_contacts) && !any(missing_vertex) && !na_rm) {
    verts[nrow(verts) + 1, 1] <- NA
  }

  ## Creating igraph object
  ## Replace uninformative with informative warning
  net <- suppressWarnings(igraph::graph_from_data_frame(x$contacts,
                                                        vertices = verts,
                                                        directed = x$directed))

  if((any(missing_contacts) || any(missing_vertex)) && !na_rm) {
    warning("All NA IDs in linelist and contacts have been coerced to character ",
            "and are assumed to represent a single case. If this is unwanted, ",
            "set na_rm = TRUE to remove NA elements from the igraph object.")
  }

  igraph::vertex_attr(net)$id <- igraph::vertex_attr(net)$name

  return(net)

}
