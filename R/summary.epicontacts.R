#' Summary method for epicontacts objects
#'
#' This method outputs a summary of the content of \code{epicontacts} objects.
#'
#' @export
#'
#' @aliases summary_epicontacts
#'
#' @author VP Nagraj (\email{vpnagraj@virginia.edu})
#'
#' @param object an \code{\link{epicontacts}} object
#'
#' @param ... further parameters to be passed to other methods (currently not used)
#'
summary.epicontacts <- function(object, ...){

    x <- object

    res <- list()

    res$n_unique_contacts <- length(get_id(x,"contacts"))
    res$n_common <- length(get_id(x, "common"))
    res$n_linelist <- length(get_id(x, "linelist"))
    res$contacts.attributes <- names(x$contacts[,-c(1,2)])
    res$linelist_attributes <- names(x$linelist[,-1])

    class(res) <- "summary_epicontacts"
    return(res)
}
