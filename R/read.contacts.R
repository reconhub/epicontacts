#' Read linelist and contact data
#'
#' This function reads data stored as data.frame containing linelist (case information, where each
#' row corresponds to a unique patient), and contacts between patients. See details for expected
#' data formats.
#'
#' @export
#'
#' @param linelist a data.frame with at least one columns providing unique patient identifiers
#'
#' @param contacts an optional data.frame; if provided, it needs at least two columns indicating patients between which cases take place; these need not be referenced in the linelist
#'
#' @param id an index or name indicating which column in \code{linelist} contains unique identifiers
#'
#' @examples
#' ## make epi_contacts object from simulated Ebola data
#' x <- read_contacts(ebola.sim$linelist, ebola.sim$contacts)
#'
read_contacts <- function(linelist, contacts=NULL, id=1){
    ## We read data from linelist, which needs to contain at least one case, and contacts, which are
    ## optional. Sanity checks will include standard class and dimensionality checks, as well as
    ## uniqueness of IDs in the line list, and enforcing 'character' type for the unique IDs, and
    ## naming the case ID field 'id'.


    ## Process nodes
    if (is.null(linelist)) {
        stop("linelist is NULL")
    }
    if (length(linelist)==1 && is.na(linelist)) {
        stop("linelist is NA")
    }
    if (nrow(linelist) < 1L) {
        stop("linelist should have at least one row")
    }
    if (ncol(linelist) < 1L) {
        stop("linelist should have at least one column")
    }
    linelist[,id] <- as.character(linelist[,id])
    if (sum(temp <- duplicated(linelist[,id]))>0) {
        msg <- paste(linelist[temp,id], collapse=" ")
        stop("Duplicated IDs detected in the linelist; culprits are:", msg)
    }
    colnames(linelist)[id] <- "id"


    ## Process contacts
    if (length(contacts)==1 && is.na(contacts)) {
        contacts <- NULL
    }
    if (!is.null(contacts)){
        if(nrow(contacts) < 1L) {
            stop("contacts should have at least one row")
        }
        if (ncol(contacts) < 2L) {
            stop("contacts should have at least two columns")
        }
    }

    ## Build final output
    out <- list(linelist=linelist, contacts=contacts)

    class(out) <- c("epi_contacts", "list")
    return(out)
}
