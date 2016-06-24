#' Subset clusters from epi_contacts object by nodes
#'
#' This function subsets an epi_contacts object based on nodes (individuals) of interest.
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @param nds node(s) whose clusters need to be subsetted
#'
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' nds <- "cac51e" ## it can be a vector of nodes as well
#'
#' ## subset based on cluster to which "cac51e" belongs
#' x_subset <- subset_clusters_by_nodes(x, nds)
#'


subset_clusters_by_nodes <- function(epi_contacts, nds){
    net <- as.igraph.epi_contacts(epi_contacts)
    cs <- igraph::clusters(net)
    nodes <- data.frame(nodes =igraph::V(net)$id, cs_member = cs$membership, stringsAsFactors = FALSE)
    cluster_to_subset <- unique(nodes$cs_member[which(nodes$nodes %in% nds)])
    nodes_to_subset <- nodes$nodes[ which(nodes$cs_member %in% cluster_to_subset)]
    epi_subset <- epi_contacts[nodes_to_subset]
    return(epi_subset)
}



