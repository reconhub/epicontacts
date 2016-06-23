#' Create igraph object from contact data
#'
#' This function creates an igraph object from a given epi_contacts object containing 'contacts' dataframe.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts_list an \code{\link{epi_contacts}} object
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
#' ## make igraph object with associates attributes from epi_contacts object
#' net <- as.igraph.epi_contacts(x)
#' net
#' igraph::plot(net)
#'

as.igraph.epi_contacts <- function(epi_contacts_list){
    ## We read in the epi_contacts object, which contains the two dataframes, the 'linelist' and the
    ## 'contacts' dataframes. This function converts the 'contacts' dataframe into an igraph object.
    ## It extracts information for the igraph vertices from the corresponding 'linelist' dataframe
    ## (if they are present), and assigns them as vertx attributes.

    ## extract contact data.frame
    net_edges <- epi_contacts_list$contacts

    ## create igraph object
    net <- igraph::graph.data.frame(net_edges)

    ## adding vertex attributes

    ### verts: vertices of igraph object
    verts <- data.frame(id = igraph::vertex_attr(net)[[1]], stringsAsFactors = FALSE)

    ### attrs: the epi_contacts linelist, which serves as the source of the attributes
    attrs <- epi_contacts_list$linelist

    ### verts_info: attributes of vertices present in the linelist
    verts_info <- dplyr::left_join(verts, attrs, by = 'id')

    ### attrs_list: the above attributes in a list format
    attrs_list <- lapply(verts_info, function(attr_col){attr_col})

    ### assigning the attributes to the igraph
    igraph::vertex_attr(net) <- attrs_list # {{figure out if I should keep the "id" as an attr or not?}}

    return(net)
}









