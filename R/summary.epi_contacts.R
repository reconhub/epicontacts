#' Summary method for epi_contacts objects
#'
#' This method outputs a summary of the content of \code{epi_contacts} objects.
#'
#' @export
#'
#' @author VP Nagraj (\email{vpnagraj@virginia.edu})
#'
#' @param object an \code{\link{epi_contacts}} object
#'
#' @param ... further parameters to be passed to other methods (currently not used)
#'
summary.epi_contacts <- function(object, ...){

    x <- object

    cat("\n/// Summary of Epidemiological Contacts //\n")
    cat("\n// class:", paste(class(x), collapse=", "))

    cat("\n")

    allcontacts <- c(x$contacts[,1],x$contacts[,2])

    contactcoverage <- length(intersect(unique(allcontacts),x$linelist[,1])) / (nrow(x$linelist))

    cat("// number of unique contacts:",
        length(unique(allcontacts)))

    cat("\n")

    cat("// number of records in linelist:",
        nrow(x$linelist))

    cat("\n")

    cat("// number of contacts that appear in linelist:",
        length(intersect(unique(allcontacts),x$linelist[,1])))

    cat("\n")

    cat("// percentage of contacts appearing in linelist:",
        paste0(round(contactcoverage*100,2), "%"))

    cat("\n")

    cat("// attributes in linelist:",
        paste(names(x$linelist[,-1]), collapse = ", "))

    cat("\n")

    cat("// attributes in contacts:",
        paste(names(x$contacts[,-c(1,2)]), collapse = ", "))

    cat("\n")
}
