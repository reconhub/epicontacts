#' Create igraph object from contact data
#'
#' This function creates an igraph object from a given epi_contacts object containing 'contacts' dataframe.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @return An igraph graph object.
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

as.igraph.epi_contacts <- function(epi_contacts){
#     ## We read in the epi_contacts object, which contains the two dataframes, the 'linelist' and the
#     ## 'contacts' dataframes. This function converts the 'contacts' dataframe into an igraph object.
#     ## It extracts information for the igraph vertices from the corresponding 'linelist' dataframe
#     ## (if they are present), and assigns them as vertx attributes.
#
#     ## extract contact data.frame
#     net_edges <- epi_contacts$contacts
#
#     ## create igraph object
#     net <- igraph::graph.data.frame(net_edges, directed = epi_contacts$directed)
#
#     ## adding vertex attributes
#
#     ### verts: vertices of igraph object
#     verts <- data.frame(id = igraph::vertex_attr(net)[[1]], stringsAsFactors = FALSE)
#
#     ### attrs: the epi_contacts linelist, which serves as the source of the attributes
#     attrs <- epi_contacts$linelist
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
    net <- graph_from_data_frame(epi_contacts$contacts, verticies = epi_contacts$linelist,
                                 directed = epi_contacts$directed)

    return(net)
}









