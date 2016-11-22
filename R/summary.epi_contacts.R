#' Summary method for epi_contacts objects
#'
#' This method outputs a summary of the content of \code{epi_contacts} objects.
#'
#' @export
#'
#' @aliases summary_epi_contacts
#'
#' @author VP Nagraj (\email{vpnagraj@virginia.edu})
#'
#' @param object an \code{\link{epi_contacts}} object
#'
#' @param ... further parameters to be passed to other methods (currently not used)
#'
summary.epi_contacts <- function(object, ...){

    x <- object

    res <- list()

    res$n_unique_contacts <- length(get_id(x,"contacts"))
    res$n_common <- length(get_id(x, "common"))
    res$n_linelist <- length(get_id(x, "linelist"))
    res$contacts.attributes <- names(x$contacts[,-c(1,2)])
    res$linelist_attributes <- names(x$linelist[,-1])

    class(res) <- "summary_epi_contacts"
    return(res)
}
