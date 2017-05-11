#' Subset epicontacts by case-specified clusters
#'
#' This function subsets an \code{\link{epicontacts}} object by identifying
#' clusters of cases connected to specified cases.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param x an \code{\link{epicontacts}} object
#'
#' @param id a character vector of case identifiers; the connected components
#'     attached to these cases will be retained in the output object.
#'
#' @return An \code{\link{epicontacts}} object whose contact dataframe
#'     corresponds to all clusters containing specified case id.
#'
#' @examples
#' if (require(outbreaks)) {
#' ## build data
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                        id="case_id", to="case_id", from="infector",
#'                        directed=TRUE)
#'
#'
#' id <- "cac51e" ## it can be a vector of id as well
#'
#'
#' ## subset based on cluster to which "cac51e" belongs
#' x_subset <- subset_clusters_by_id(x, id)
#'
#' }

subset_clusters_by_id <- function(x, id){
    net <- as.igraph.epicontacts(x)
    cs <- igraph::clusters(net)
    net_nodes <- data.frame(nodes =igraph::V(net)$id,
                            cs_member = cs$membership,
                            stringsAsFactors = FALSE)
    cluster_to_subset <- unique(net_nodes$cs_member[which(net_nodes$nodes %in% id)])
    id_to_subset <- net_nodes$nodes[ which(net_nodes$cs_member %in% cluster_to_subset)]
    epi_subset <- x[id_to_subset]
    return(epi_subset)
}



