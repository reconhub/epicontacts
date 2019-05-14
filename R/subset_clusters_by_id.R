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

    # Convert epicontacts object to igraph and get linelist + contacts dataframes
    net <- as.igraph.epicontacts(x)
    net_linelist <- igraph::as_data_frame(net, "vertices")
    net_contacts <- igraph::as_data_frame(net, "edges")

    # Get cluster information for each node/case
    cs <- igraph::clusters(net)
    net_nodes <- data.frame(nodes =igraph::V(net)$id,
                            cs_member = cs$membership,
                            stringsAsFactors = FALSE)

    # Identify cluster containing nodes/cases of interest
    cluster_to_subset <- unique(net_nodes$cs_member[which(net_nodes$nodes %in% id)])

    # Identify members of cluster belonging to nodes/cases of interest
    id_to_subset <- net_nodes$nodes[ which(net_nodes$cs_member %in% cluster_to_subset)]
    # Create new epicontacts object with cluster members
    new_linelist <- net_linelist[ net_linelist$name %in% id_to_subset, ]
    new_contacts <- net_contacts[ net_contacts$from %in% id_to_subset |
                                  net_contacts$to %in% id_to_subset, ]

    # Return new epicontacts object
    epi_subset <- make_epicontacts(new_linelist, new_contacts, directed = x$directed)
    
    return(epi_subset)
}



