#' Assign cluster IDs to epicontacts data
#'
#' This function identifies transitive clusters (i.e. connected components) as well
#' as the number of members in each cluster, and adds this information to the
#' linelist data.
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param x An \code{\link{epicontacts}} object.
#'
#' @param output A character string indicating the type of output: either an
#'   \code{\link{epicontacts}} object (default) or a \link{data.frame} containing
#' cluster memberships to which members of \code{\link{epicontacts}} linelist belong
#'   to.
#'
#' @param member_col Name of column to which cluster membership is assigned to in the
#' linelist. Default name is 'cluster_member'.
#'
#' @param size_col  Name of column to which cluster sizes are assigned to in the
#' linelist. Default name is 'cluster_size'.
#'
#' @param override Logical value indicating whether cluster member and size columns
#' should be overwritten if they already exist in the linelist. Default is 'FALSE'.
#'
#'  @param no_infector Logical value indicating whether to assign clusters to cases with no
#' infector in the contacts dataframe. Default is 'TRUE'.
#'
#' @return An \code{\link{epicontacts}} object whose 'linelist' dataframe
#' contains new columns corresponding to cluster membership and size, or a
#' \link{data.frame} containing member ids, cluster memberships as factors,
#' and associated cluster sizes. All ids that were originally in the 'contacts'
#' dataframe but not in the linelist will also be added to the linelist.
#'
#' @importFrom stats setNames
#'
#'
#' @examples
#' if (require(outbreaks)) {
#' ## build data
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                        id = "case_id",
#'                        to = "case_id",
#'                        from = "infector",
#'                        directed = TRUE)
#'
#'
#' ## add cluster membership and sizes to epicontacts 'linelist'
#' y <- get_clusters(x, output = "epicontacts")
#' y
#'
#' ## return a data.frame with linelist member ids and cluster memberships as
#' ## factors
#' z <- get_clusters(x, output = "data.frame")
#' head(z)
#'
#' }

get_clusters <- function(x, output = c("epicontacts", "data.frame"),
                         member_col = "cluster_member",
                         size_col = "cluster_size",
                         override = FALSE,
                         no_infector = TRUE) {

  ## check if cluster columns pre-exist in linelist
  cluster_cols <- c(member_col, size_col)
  cluster_var <- c("member_col", "size_col")
  cond <- cluster_cols %in% names(x$linelist)
  if ((sum(cond) > 0) & !override) {
    if (sum(cond) == 1) {
      msg <- sprintf("'%s' is already in the linelist. Set 'override = TRUE' to write over it, else assign a different %s name.", cluster_cols[cond], cluster_var[cond])
      stop(msg)
    }
    if (sum(cond) == 2) {
      msg <- sprintf("'%s' and '%s' are already in the linelist. Set 'override = TRUE' to write over them, else assign different cluster column names.", cluster_cols[1], cluster_cols[2])
      stop(msg)
    }
  }


  if(!no_infector){
    x_full <- x$linelist
    x$contacts <- x$contacts[which(!is.na(x$contacts$from)),]
    x$linelist <- x$linelist[which(x$linelist$id %in% c(x$contacts$from, x$contacts$to)),]
  }
  
  
  output <- match.arg(output)
  net <- as.igraph.epicontacts(x)
  cs <- igraph::clusters(net)
  cs_size <- setNames(data.frame(cluster_member = seq_along(cs$csize),
                                 cluster_size = cs$csize),
                      cluster_cols)

  # Drop pre-existing cluster columns
  if (sum(cond) > 0) {
    x$linelist <- x$linelist[ , !(names(x$linelist) %in% cluster_cols)]
  }

  net_nodes <- setNames(data.frame(id = methods::as(igraph::V(net)$id,
                                                    class(x$linelist$id)),
                                   cluster_member = cs$membership,
                                   stringsAsFactors = FALSE),
                        c("id", member_col))

  net_nodes <- dplyr::left_join(net_nodes, cs_size, by = member_col)
  if(output == "epicontacts") {
    if(!no_infector){
      x$linelist <- dplyr::full_join(x_full, net_nodes, by = "id")
    } else {
      x$linelist <- dplyr::full_join(x$linelist, net_nodes, by = "id")
    }
    x$linelist[ member_col ] <- as.factor(x$linelist[[ member_col ]])
    return(x)
  } else {
    net_nodes[ member_col ] <- as.factor(net_nodes[[ member_col ]])
    return(net_nodes)
  }
}


