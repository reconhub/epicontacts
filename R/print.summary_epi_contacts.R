#' Print method for summary_epi_contacts objects
#'
#' This method outputs a printed summary of the content of \code{summary_epi_contacts} objects.
#'
#' @export
#'
#' @author VP Nagraj (\email{vpnagraj@virginia.edu})
#'
#' @param x a \code{\link{summary_epi_contacts}} object
#'
#' @param ... further parameters to be passed to other methods (currently not used)
#'
print.summary_epi_contacts <- function(x, ...){

    if(!is.null(x$n.linelist)) cat("\n// number of records in linelist:", x$n.linelist)
    if(!is.null(x$n.uniquecontacts)) cat("\n// number of unique records in contact list:", x$n.uniquecontacts)
    if(!is.null(x$n.common)) cat("\n// number of contacts appearing in linelist:", x$n.common)
    if(!is.null(x$n.common)) cat("\n// percentage of contacts appearing in linelist:", paste0(round(((x$n.common) / (x$n.linelist))*100,2),"%"))
    if(!is.null(x$linelist.attributes)) cat("\n// attributes in linelist:", x$linelist.attributes)
    if(!is.null(x$contacts.attributes)) cat("\n// attributes in contacts:",x$contacts.attributes)
    cat("\n")

}
