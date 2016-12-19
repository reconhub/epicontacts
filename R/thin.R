#' Thin data to retain matching linelist / contacts
#'
#' This function can be used to remove ('thin') data from
#' \code{\link{epicontacts}} objects to ensure stricter matching of linelists
#' and contacts. It has two behaviours, triggered by the argument \code{what}:
#' either dit thins data from \code{$linelist}, keeping only cases that are in
#' \code{$contacts} (\code{thin = "linelist"}, default), or the converse,
#' i.e. removing contacts which are not fully documented in the linelist.
#' 
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param x An \code{\link{epicontacts}} object.
#'
#' @param what A character string or integer determining which type of data is
#'     removed ('thinned'). "linelist" / 1 indicates that only cases appearing
#'     in \code{$contacts} are kept in \code{$linelist}. "contacts / 2"
#'     indicates that only cases appearing in \code{$linelist} are kept in
#'     \code{$contacts}.
#'
#' 
#' @examples
#' if (require(outbreaks)) {
#' ## build data
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                        id = "case.id", to = "case.id", from = "infector",
#'                        directed = TRUE)
#'
#' ## keep contacts from a specific case '916d0a'
#' x <- x[j = "916d0a", contacts = "from"]
#' 
#' }
thin <- function(x, what = "linelist") {
    if (!inherits(x, "epicontacts")) {
        stop("x is not an epicontacts object")
    }

    what <- what[1]
    if (what == "linelist" || what == 1) {
        to.keep <- get_id(x, "contacts")
        out <- x[i = to.keep]
    } else if (what == "contacts" || what == 2) {
        to.keep <- get_id(x, "linelist")
        out <- x[j = to.keep, contacts = "both"]
    } else {
        msg <- paste0("Wrong values for 'what'; accepted values are:\n",
                      "'linelist', 'contact', 1, 2")
        stop(msg)
    }

    return(out)
}
