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

