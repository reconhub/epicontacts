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
#'

as.igraph.epi_contacts <- function(x){
#     ## We read in the epi_contacts object, which contains the two dataframes, the 'linelist' and the
#     ## 'contacts' dataframes. This function converts the 'contacts' dataframe into an igraph object.
#     ## It extracts information for the igraph vertices from the corresponding 'linelist' dataframe
#     ## (if they are present), and assigns them as vertx attributes.
#
#     ## extract contact data.frame
#     net_edges <- x$contacts
#
#     ## create igraph object
#     net <- igraph::graph.data.frame(net_edges, directed = x$directed)
#
#     ## adding vertex attributes
#
#     ### verts: vertices of igraph object
#     verts <- data.frame(id = igraph::vertex_attr(net)[[1]], stringsAsFactors = FALSE)
#
#     ### attrs: the epi_contacts linelist, which serves as the source of the attributes
#     attrs <- x$linelist
#
#     ### verts_info: attributes of vertices present in the linelist
#     verts_info <- dplyr::left_join(verts, attrs, by = 'id')
#
#     ### attrs_list: the above attributes in a list format
#     attrs_list <- lapply(verts_info, function(attr_col){attr_col})
#
#     ### assigning the attributes to the igraph
#     igraph::vertex_attr(net) <- attrs_list # {{figure out if I should keep the "id" as an attr or not?}}

    ## Take 2, simpler way to go about it:

    ## Create vertex dataframe using combination of linelist and contacts
    all_ids <- data.frame(id = get_id(x, "all"), stringsAsFactors = FALSE)
    verts <- dplyr::full_join(x$linelist, all_ids, by = "id")

    ## Creating igraph object
    net <- igraph::graph_from_data_frame(x$contacts, vertices = verts,
                                 directed = x$directed)

    igraph::vertex_attr(net)$id = igraph::vertex_attr(net)$name

    return(net)
}









