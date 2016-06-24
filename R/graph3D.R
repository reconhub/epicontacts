#' Create 3D graph from epi_contact object
#'
#' This function creates a 3D graph from an epi_contact object
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' ## subset based on single cluster size
#' x_subset <- subset_clusters_by_size(x, 10, 12)
#'
#' ## 3D graph
#' g <- graph3D(x_subset)


graph3D <- function(epi_contacts) {
    e <- epi_contacts$contacts
    colnames(e)[1:2] <- c("from", "to")
    e$size <- 0.5
    e$color <- "grey"
    g <- threejs::graphjs(e, main="3D nodes", showLabels=TRUE, bg ="black", fg="orange")
    return(g)
}


