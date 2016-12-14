#' Assign cluster IDs to epicontacts data
#'
#' This function identifies transitive clusters (i.e. connected components) and adds this information to the linelist data.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epicontacts an \code{\link{epicontacts}} object.
#'
#' @param output either an \code{\link{epicontacts}} object or a \link{factor} containing cluster memberships to which members of \code{\link{epicontacts}} linelist belong to.
#'
#' @return An \code{\link{epicontacts}} object whose 'linelist' dataframe contains new columns corresponding to cluster membership and size, or a \link{data.frame} containing member ids, cluster memberships as factors, and associated cluster sizes.
#'
#' @examples
#' if (require(outbreaks)) {
#' ## build data
#' x <- make_epicontacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' ## add cluster membership and sizes to epicontacts 'linelist'
#' y <- get_clusters(x, output = "epicontacts")
#'
#' ## return a data.frame with linelist member ids and cluster memberships as factors
#' z <- get_clusters(x, output = "factor")
#' }

get_clusters <- function(epicontacts, output = c("epicontacts", "factor")){
    output <- match.arg(output)
    net <- as.igraph.epicontacts(epicontacts)
    cs <- igraph::clusters(net)
    cs_size <- data.frame(cluster_member = seq_along(cs$csize),
                          cluster_size = cs$csize)
    
    net_nodes <- data.frame(id =igraph::V(net)$id,
                            cluster_member = cs$membership,
                            stringsAsFactors = FALSE)
    
    net_nodes <- dplyr::left_join(net_nodes, cs_size, by = "cluster_member")
    if(output == "epicontacts") {
        epicontacts$linelist <- dplyr::left_join(epicontacts$linelist, net_nodes, by = "id")
        return(epicontacts)
    } else {
        f <- net_nodes[ net_nodes$id %in% epicontacts$linelist$id, ]
        f$cluster_member <- as.factor(f$cluster_member)
        return(f)
    }
}


