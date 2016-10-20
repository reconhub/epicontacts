#' Assign cluster IDs to epi_contacts data
#'
#' This function assigns cluster ID's to the epi_contacts linelist. These ID's are derived from graph clusters in
#' the epi_contacts 'contacts' dataframe.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @return An \code{\link{epi_contacts}} object whose 'linelist' dataframe contains a new column
#' corresponding to cluster ID.
#'
#' @examples
#' ## build data
#' epi_contact <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' ## add cluster ID to epi_contacts 'linelist'
#' epi_contacts_with_ids <- get_clusters(epi_contact)
#'


get_clusters <- function(epi_contacts){
    net <- igraph.epi_contacts(epi_contacts)
    cs <- igraph::clusters(net)
    # net_nodes <- data.frame(id =igraph::V(net)$id, cluster_member = cs$membership, stringsAsFactors = FALSE)

    # epi_contacts$linelist <- dplyr::left_join(epi_contacts$linelist, net_nodes, by = 'id')

    cs_size <- data.frame(cluster_member = 1:length(cs$csize), cluster_size = cs$csize)
    net_nodes <- data.frame(id =igraph::V(net)$id, cluster_member = cs$membership, stringsAsFactors = FALSE)
    net_nodes <- dplyr::left_join(net_nodes, cs_size, by = "cluster_member")
    epi_contacts$linelist <- dplyr::left_join(epi_contacts$linelist, net_nodes, by = 'id')

    return(epi_contacts)
}

