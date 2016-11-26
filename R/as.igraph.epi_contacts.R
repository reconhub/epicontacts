#' Create igraph object from contact data
#'
#' This function creates an igraph object from a given \code{\link{epi_contacts}} object containing
#' a 'contacts' dataframe.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param x An \code{\link{epi_contacts}} object.
#'
#' @return An \code{igraph} object (from the \code{igraph} package).
#'
#' @examples
#'
#' if (require(outbreaks)) {
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' ## subset data - keep 10 cases from linelist with contacts
#' ids <- get_id(x, "common")[1:10]
#' ids
#' x <- x[ids]
#'
#' ## make igraph object with associated attributes from epi_contacts object
#' net <- as.igraph.epi_contacts(x)
#' net
#' plot(net)
#' }

as.igraph.epi_contacts <- function(x){
    ## Create vertex dataframe using combination of linelist and contacts
    all_ids <- data.frame(id = get_id(x, "all"), stringsAsFactors = FALSE)
    verts <- dplyr::full_join(x$linelist, all_ids, by = "id")

    ## Creating igraph object
    net <- igraph::graph_from_data_frame(x$contacts, vertices = verts,
                                 directed = x$directed)

    igraph::vertex_attr(net)$id = igraph::vertex_attr(net)$name

    return(net)
}









