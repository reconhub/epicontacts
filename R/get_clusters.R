#' Assign cluster IDs to epicontacts data
#'
#' This function identifies transitive clusters (i.e. connected components) and
#' adds this information to the linelist data.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param x An \code{\link{epicontacts}} object.
#'
#' @param output A character string indicating the type of output: either an
#'   \code{\link{epicontacts}} object or a \link{data.frame} containing cluster
#'   memberships to which members of \code{\link{epicontacts}} linelist belong
#'   to.
#'
#' @return An \code{\link{epicontacts}} object whose 'linelist' dataframe
#'   contains new columns corresponding to cluster membership and size, or a
#'   \link{data.frame} containing member ids, cluster memberships as factors,
#'   and associated cluster sizes.
#'
#' @examples
#' if (require(outbreaks)) {
#' ## build data
#'
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                        id = "case.id",
#'                        to = "case.id",
#'                        from = "infector",
#'                        directed = TRUE)
#'
#'
#' ## add cluster membership and sizes to epicontacts 'linelist'
#'
#' y <- get_clusters(x, output = "epicontacts")
#' y
#'
#' ## return a data.frame with linelist member ids and cluster memberships as
#' ## factors
#'
#' z <- get_clusters(x, output = "data.frame")
#' head(z)
#'
#' }

get_clusters <- function(x, output = c("epicontacts", "data.frame")){
  output <- match.arg(output)
  net <- as.igraph.epicontacts(x)
  cs <- igraph::clusters(net)
  cs_size <- data.frame(cluster_member = seq_along(cs$csize),
                        cluster_size = cs$csize)
  net_nodes <- data.frame(id =igraph::V(net)$id,
                          cluster_member = cs$membership,
                          stringsAsFactors = FALSE)

  net_nodes <- dplyr::left_join(net_nodes, cs_size, by = "cluster_member")
  if(output == "epicontacts") {
    x$linelist <- dplyr::full_join(x$linelist, net_nodes, by = "id")
    return(x)
  } else {
    net_nodes$cluster_member <- as.factor(net_nodes$cluster_member)
    return(net_nodes)
  }
}


