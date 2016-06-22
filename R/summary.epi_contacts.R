#' Summary method for epi_contacts objects
#'
#' This method outputs a summary of the content of \code{epi_contacts} objects.
#'
#' @export
#'
#' @author VP Nagraj (\email{vpnagraj@virginia.edu})
#'
#' @param x an \code{\link{epi_contacts}} object
#'
#' @param ... further parameters to be passed to other methods (currently not used)
#'
summary.epi_contacts <- function(x, ...){
    cat("\n/// Summary of Epidemiological Contacts //\n")
    cat("\n  // class:", paste(class(x), collapse=", "))

    cat("\n")

    allcontacts <- c(x$contacts[,1],x$contacts[,2])

    contactcoverage <- length(intersect(unique(allcontacts),x$linelist[,1])) / (nrow(x$linelist))

    cat("number of contacts in linelist:",
        length(intersect(unique(allcontacts),x$linelist[,1])),
        "/",
        nrow(x$linelist),
        paste0("(", round(contactcoverage*100,2), "%", ")"))

    cat("\n")

    cat("\n  // summary of linelist\n\n")
    cat(summary(x$linelist))

    cat("\n  // summary of contacts\n\n")
    cat(summary(x$contacts))

    cat("\n")
}
