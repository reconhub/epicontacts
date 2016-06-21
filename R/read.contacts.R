#' Read linelist and contact data
#'
#' This function reads data stored as data.frame containing linelist (case information, where each
#' row corresponds to a unique patient), and contacts between patients. See details for expected
#' data formats.
#'
#' @export
#'
#' @examples
#' if(require(outbreaks)){
#'  x <- ebola.sim$linelist
#' }
read.contacts <- function(linelist, contacts=NULL){
    ## We read data from linelist, which needs to contain at least one case, and contacts, which are
    ## optional. Sanity checks will include standard class and dimensionality checks, as well as
    ## uniqueness of IDs in the line list


    ## Process nodes
    if (is.null(linelist)) {
        stop("linelist is NULL")
    }
    if (is.na(linelist)) {
        stop("linelist is NA")
    }
    if (nrow(linelist) < 1L) {
        stop("linelist should have at least one row")
    }
    if (ncol(linelist) < 1L) {
        stop("linelist should have at least one column")
    }

    ## Process contacts
    if (is.na(contacts)) {
        contacts <- NULL
    }
    if (!is.null(contacts)){
        if(nrow(contacts) < 1L) {
            stop("contacts should have at least one row")
        }
        if (ncol(contacts) < 1L) {
            stop("contacts should have at least one column")
        }
    }

    ## Build final output
    out <- list(linelist=linelist, contacts=contacts)

    class(out) <- c("epi_contacts", "list")
}
