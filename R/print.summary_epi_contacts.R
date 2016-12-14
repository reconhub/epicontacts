#' Print method for summary_epicontacts objects
#'
#' This method outputs a printed summary of the content of
#' \code{summary_epicontacts} objects.
#'
#' @export
#'
#' @author VP Nagraj (\email{vpnagraj@virginia.edu})
#'
#' @param x a \code{\link{summary_epicontacts}} object
#'
#' @param ... further parameters to be passed to other methods (currently not used)
#'
print.summary_epicontacts <- function(x, ...){

    if (!is.null(x$n_linelist)) {
        cat("\n// number of records in linelist:", x$n_linelist)
    }

    if (!is.null(x$n_unique_contacts)) {
        cat("\n// number of unique records in contact list:", x$n_unique_contacts)
    }

    if (!is.null(x$n_common)) {
        cat("\n// number of contacts appearing in linelist:", x$n_common)
    }

    if (!is.null(x$n_common)) {
        cat("\n// percentage of contacts appearing in linelist:",
            paste0(round(((x$n_common) / (x$n_linelist))*100,2),"%"))
    }

    if (!is.null(x$linelist_attributes)) {
        cat("\n// attributes in linelist:", x$linelist_attributes)
    }

    if (!is.null(x$contacts_attributes)) {
        cat("\n// attributes in contacts:",x$contacts_attributes)
    }
    cat("\n")
    
}
