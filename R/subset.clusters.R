#' Summary of clusters in igraph object
#'
#' This function provides a summary of the igraph object, and a table of the number of clusters by
#' their sizes.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param net igraph object
#'
#' @examples
#' ## build test graph
#' data = data.frame(from = c(1:10), to = c(2, 3, 4, 9, 6, 11:15))
#' g <- igraph::graph.data.frame(data)
#'
#' ## graph and cluster summary
#' cluster.summary.igraph(g)

cluster.summary.igraph <- function(net){
    cs <- igraph::clusters(net)
    cs_summary <- table(cs$csize)
    cat(sprintf("Total number of nodes: %s \n", igraph::vcount(net)))
    cat(sprintf("Total number of edges: %s \n", igraph::ecount(net)))
    cat(sprintf("Total number of clusters: %s \n", cs$no))
    return(cs_summary)
}





#' Subset clusters from epi_contacts object
#'
#' This function subsets an epi_contacts object based on cluster sizes of connected individuals.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @param cs_min minimum cluster size
#'
#' @param cs_max maximum cluster size
#'
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#'## subset using min and max cluster size
#' x_subset <- subset.clusters.epi_contacts(x, 12, 15)
#'
#' ## subset based on single cluster size
#' x_subset <- subset.clusters.epi_contacts(x, 12)

subset.clusters.epi_contacts <- function(epi_contacts, cs_min, cs_max=cs_min){
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


