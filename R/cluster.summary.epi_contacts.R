#' Summary of clusters in epi_contacts object
#'
#' This function provides a summary of an igraph graph object created from clusters in an \code{\link{epi_contacts}} object, along
#' with the summary of an igraph object created from \code{\link{epi_contacts}}. and a table of the number of clusters
#' of contacts by their cluster size.
#' Define cluster$$$$$$$$
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @return A list with graph specfic summary of \code{\link{epi_contacts}} object
#'
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' ## data's graph and cluster summary
#' cluster.summary.epi_contacts(x)

cluster.summary.epi_contacts <- function(epi_contacts){
    net <- as.igraph.epi_contacts(epi_contacts)
    cs <- igraph::clusters(net)
    cs_table <- table(cs$csize)
    res <- list()
    res$total.nodes <- igraph::vcount(net)
    res$total.edges <- igraph::ecount(net)
    directed <- ifelse(epi_contacts$directed, "directed", "non directed")
    res$directed <- directed
    res$no.of.clusters <- cs$no
    res$table.cluster_size <- cs_table
    return(res)
}

