#' Subset clusters from epi_contacts object by nodes
#'
#' This function subsets an epi_contacts object based on individuals/nodes of interest.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @param nodes node(s) whose clusters need to be subsetted
#'
#' @return An \code{\link{epi_contacts}} object whose contact dataframe corresponds to
#' all clusters containing specified nodes.
#'
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#'
#' nodes <- "cac51e" ## it can be a vector of nodes as well
#'
#'
#' ## subset based on cluster to which "cac51e" belongs
#' x_subset <- subset_clusters_by_nodes(x, nodes)
#'


subset_clusters_by_nodes <- function(epi_contacts, nodes){
    net <- igraph.epi_contacts(epi_contacts)
    cs <- igraph::clusters(net)
    net_nodes <- data.frame(nodes =igraph::V(net)$id, cs_member = cs$membership, stringsAsFactors = FALSE)
    cluster_to_subset <- unique(net_nodes$cs_member[which(net_nodes$nodes %in% nodes)])
    nodes_to_subset <- net_nodes$nodes[ which(net_nodes$cs_member %in% cluster_to_subset)]
    epi_subset <- epi_contacts[nodes_to_subset]
    return(epi_subset)
}



