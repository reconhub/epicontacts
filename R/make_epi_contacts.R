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
#' @param from an index or name indicating which column in \code{contacts} contains the first case of a contact
#'
#' @param to an index or name indicating which column in \code{contacts} contains the second case of a contact
#'
#' @param directed a logical indicating if contact are directed, or not; not that contacts will be
#' indicated as 'from' and 'to' even in non directed contacts
#'
#' @return A list with two data.frames named 'linelist', and 'contacts'; the first column of
#' 'linelist' is named 'id', and contains a vector of character corresponding to unique identifiers;
#' the first two columns of 'contacts' are named 'from' and 'to', and indicate contacts.
#'
#' @examples
#' ## make epi_contacts object from simulated Ebola data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts)
#'
#' ## test reordering of columns
#' linelist <- ebola.sim$linelist[,rev(seq_len(ncol(ebola.sim$linelist)))]
#' contacts <- ebola.sim$contacts[,rev(seq_len(ncol(ebola.sim$contacts)))]
#' head(linelist)
#' head(contacts)
#'
#' ## make object
#' x <- make_epi_contacts(linelist, contacts, id="case.id", to="case.id", from="infector")
#' head(x$linelist)
#' head(x$contacts)
#'
make_epi_contacts <- function(linelist, contacts=NULL, id=1L, from=1L, to=2L, directed=FALSE){
    ## We read data from linelist, which needs to contain at least one case, and contacts, which are
    ## optional. Sanity checks will include standard class and dimensionality checks, as well as
    ## uniqueness of IDs in the line list, and enforcing 'character' type for the unique IDs, and
    ## naming the case ID field 'id'. We also reorder data so that the first column of 'linelist' is
    ## 'id', and the first two columns of 'contacts' are 'from' and 'to'


    ## process linelist ##
    ## checks
    linelist <- as.data.frame(linelist)

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
        stop("Duplicated IDs detected in the linelist; culprits are: ", msg)
    }

    ## reordering
    if (is.character(id)) {
        id <- match(id, names(linelist))
    }
    names(linelist)[id] <- "id"
    linelist <- linelist[, c(id, setdiff(seq_len(ncol(linelist)), id))]


    ## process contacts ##
    ## checks
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


        ## reordering
        if (is.character(from)) {
            from <- match(from, names(contacts))
        }
        if (is.character(to)) {
            to <- match(to, names(contacts))
        }
        names(contacts)[c(from,to)] <- c("from","to")
        contacts <- contacts[, c(from, to, setdiff(seq_len(ncol(contacts)), c(from,to)))]
    }

    ## Build final output
    out <- list(linelist=linelist, contacts=contacts, directed=directed)

    class(out) <- c("epi_contacts", "list")
    return(out)
}
