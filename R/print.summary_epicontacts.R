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

    cat("\n/// Overview //")
    if (!is.null(x$n_id_linelist)) {
        cat("\n  // number of unique IDs in linelist:", x$n_id_linelist)
    }

    if (!is.null(x$n_id_contacts)) {
        cat("\n  // number of unique IDs in contacts:", x$n_id_contacts)
    }

    if (!is.null(x$n_id_common)) {
        cat("\n  // number of unique IDs in both:", x$n_id_common)
    }

    if (!is.null(x$n_contacts)) {
        cat("\n  // number of contacts:", x$n_contacts)
    }

    if (!is.null(x$prop_contacts_in_linelist)) {
        cat("\n  // contacts with both cases in linelist:",
            round(100 * x$prop_contacts_in_linelist,3), "%")
    }

    cat("\n\n/// Degrees of the network //")
    if (!is.null(x$deg_in)) {
        cat("\n  // in-degree summary:\n")
        print(x$deg_in)
    }

    if (!is.null(x$deg_out)) {
        cat("\n  // out-degree summary:\n")
        print(x$deg_out)
    }

    if (!is.null(x$deg_both)) {
        cat("\n  // in and out degree summary:\n")
        print(x$deg_both)
    }

    cat("\n/// Attributes //")
    if (!is.null(x$linelist_attributes)) {
        cat("\n  // attributes in linelist:\n", x$linelist_attributes)
    }

    if (!is.null(x$contacts_attributes)) {
        cat("\n\n  // attributes in contacts:\n", x$contacts_attributes)
    }

    cat("\n")
    
}
