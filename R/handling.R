#' Subset epi_contacts objects based on case identifiers
#'
#' The "[" operator can be used to subset \code{\link{epi_contacts}} objects, retaining a specified
#' set of case identifiers (\code{i} for the linelist, \code{j} for contacts). Note that unlike most
#' classical R objects, there is no replacement method for \code{\link{epi_contacts}} objects,
#' i.e. no operations such as \code{foo[i] <- bar}.
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param x an \code{\link{epi_contacts}} object
#'
#' @param i a character vector containing case ID to be retained in the linelist
#'
#' @param j a character vector containing case ID to be retained in the contacts; defaults to the
#' same as \code{i}
#'
#' @param contacts a character string indicating on which basis contacts are retained (see details)
#'
#' @param ... not used (there for compatibility with generic)
#'
#' @details
#' Details on the 'contacts' argument; possible values are:
#' \itemize{
#' \item{'both'}{contacts are retained only if both cases are in the subset}
#' \item{'either'}{contacts are retained if at least one of the cases is in the subset}
#' \item{'from'}{contacts are retained only if the source ('from') is in the subset}
#' \item{'to'}{contacts are retained only if the recipient ('to') is in the subset}
#' }
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
"[.epi_contacts" <- function(x, i, j=i, contacts=c("both","either","from","to"), ...){
    ## In all the following, i is used to subset the linelist, j to subset contacts. The variable
    ## 'strict' triggers the subsetting of contacts; if TRUE, then both nodes need to be part of 'j'
    ## for a contact to be retained; if FALSE, only one of them needs to be in 'j'.

    ## check
    if (!is.character(i)) {
        warning("'i' is not a character; enforcing conversion \n(logicals and integers cannot be used to subset epi_contacts objects")
        i <- as.character(i)
    }
    if (!is.character(j)) {
        warning("'j' is not a character; enforcing conversion \n(logicals and integers cannot be used to subset epi_contacts objects")
        j <- as.character(j)
    }
    contacts <- match.arg(contacts)

    ## subset linelist
    to.keep <- x$linelist$id %in% i
    x$linelist <- x$linelist[to.keep, , drop=FALSE]

    ## subset contacts
    if (contacts=="both") {
        to.keep <- (x$contacts$from %in% j) & (x$contacts$to %in% j)
    }
    if (contacts=="either") {
        to.keep <- (x$contacts$from %in% j) | (x$contacts$to %in% j)
    }
    if (contacts=="from") {
        to.keep <- x$contacts$from %in% j
    }
    if (contacts=="to") {
        to.keep <- x$contacts$to %in% j
    }

    x$contacts <- x$contacts[to.keep, , drop=FALSE]

    return(x)
}