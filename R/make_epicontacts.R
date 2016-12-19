#' Read linelist and contact data
#'
#' This function reads data stored as data.frame containing linelist (case
#' information, where each row corresponds to a unique patient), and contacts
#' between patients. Common identifiers should be used in the two data sources
#' for matching to be achieved.
#'
#' @export
#'
#' @aliases make_epicontacts epicontacts
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param linelist a \link{data.frame} with at least one column providing unique
#'     patient identifiers
#'
#' @param contacts a \link{data.frame} that needs at least two columns
#'     indicating patients between which cases take place; these need not be
#'     referenced in the linelist
#'
#' @param id an index or name indicating which column in \code{linelist}
#'     contains unique identifiers; default is first column in \code{linelist}
#'     data frame
#'
#' @param from an index or name indicating which column in \code{contacts}
#'     contains the first case of a contact
#'
#' @param to an index or name indicating which column in \code{contacts}
#'     contains the second case of a contact
#'
#' @param directed a logical indicating if contacts are directed or not; default
#'     is \code{FALSE} but note that contacts will be indicated as 'from' and
#'     'to' even in non-directed contacts
#'
#' @return An \code{epicontacts} object in list format with three elements:
#' 
#' \itemize{
#' \item \code{linelist}: data.frame of cases with first column 'id'
#' containing character vector of unique identifiers
#'
#' \item \code{contacts}: data.frame of contacts with first two columns named
#' 'from' and 'to' indicating unique pairs of contact between individuals
#'
#' \item \code{directed}: indicator as to whether or not the contacts are to be
#' considered directed or not
#' }
#' 
#' @details
#'
#' An \code{epicontacts} object can be created from two components:
#' \itemize{
#' \item a linelist provided as a \code{data.frame} where columns are
#' different variables describing cases, and where each row is a different case.
#' and a contact list.
#' 
#' \item a contact list provided as a \code{data.frame} where each row contains
#' unique pairs of contacts with unique features of contact in columns. The line
#' list and contact list should share an identification scheme for individuals.
#' }
#'
#' @references
#'     \url{http://foodborne.unl.edu/public/role/epidemiologist/lineLists.html}
#'
#' @examples
#' if (require(outbreaks)) {
#' ## make epicontacts object from simulated Ebola data
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts)
#'
#' ## test reordering of columns
#' linelist <- ebola_sim$linelist[,rev(seq_len(ncol(ebola_sim$linelist)))]
#' contacts <- ebola_sim$contacts[,rev(seq_len(ncol(ebola_sim$contacts)))]
#' head(linelist)
#' head(contacts)
#'
#' ## make object
#' x <- make_epicontacts(linelist, contacts, id="case.id",
#'                        to="case.id", from="infector")
#' head(x$linelist)
#' head(x$contacts)
#' }
make_epicontacts <- function(linelist, contacts, id=1L, from=1L, to=2L,
                              directed=FALSE){
    
    ## We read data from linelist, which needs to contain at least one case, and
    ## contacts. Sanity checks will include standard class
    ## and dimensionality checks, as well as uniqueness of IDs in the line list,
    ## and enforcing 'character' type for the unique IDs, and naming the case ID
    ## field 'id'. We also reorder data so that the first column of 'linelist'
    ## is 'id', and the first two columns of 'contacts' are 'from' and 'to'

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
    
    if (is.null(contacts)) {
        stop("contacts is NULL")
    }
    
    if (length(contacts)==1 && is.na(contacts)) {
        stop("contacts is NA")
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

    class(out) <- c("epicontacts")
    return(out)
}
