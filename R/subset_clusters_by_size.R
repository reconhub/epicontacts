
#' Subset clusters from epi_contacts object by cluster size
#'
#' This function subsets an epi_contacts object based on sizes of clusters.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @param cs_min minimum cluster size to be subsetted
#'
#' @param cs_max maximum cluster size to be subsetted
#'
#' @return An \code{\link{epi_contacts}} object whose contact dataframe corresponds to
#' all clusters of specified cluster sizes.
#'
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#'
#' ## subset based on cluster size (using range from min to max)
#' x_subset <- subset_clusters_by_size(x, cs_min = 12, cs_max = 15)
#'
#'
#' ## subset based on cluster size (using single cluster size value)
#' x_subset <- subset_clusters_by_size(x, cs_min = 12)


subset_clusters_by_size <- function(epi_contacts, cs_min, cs_max = cs_min){
    csize <- seq(from = cs_min, to = cs_max, by = 1)
    net <- as.igraph.epi_contacts(epi_contacts)
    cs <- igraph::clusters(net)
    nodes <- data.frame(nodes =igraph::V(net)$id, cs_member = cs$membership, stringsAsFactors = FALSE)
    cluster_to_subset <- which(cs$csize %in% csize)
    nodes_to_subset <- nodes$nodes[ which(nodes$cs_member %in% cluster_to_subset)]
    graph_subset <- igraph::induced_subgraph(net, igraph::V(net)$id %in% nodes_to_subset)

    epi_subset <- epi_contacts[nodes_to_subset]
    return(epi_subset)
}


