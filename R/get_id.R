#' Access unique identifiers in epi_contacts objects
#'
#' This accessor is used to extract unique identifiers from \code{\link{epi_contacts}} objects. The
#' argument 'which' can be used to specify if IDs should include: linelist only ('linelist'),
#' contacts only ('contacts'), the union of both ('all'), or the intersection of both ('common');
#' two additional options are 'from' (ID 'giving' contacts) and 'to' (ID 'receiving' contacts).
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param x an \code{\link{epi_contacts}} object
#'
#' @param which the type of ID to return (see description); value can be 'linelist', 'contacts',
#' 'all', or 'common'.
#'
#' @return x a character vector of unique identifiers
#'
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' ## get identifiers
#' id1 <- get_id(x)
#' id2 <- get_id(x, "contacts")
#' id3 <- get_id(x, "all")
#' id4 <- get_id(x, "common")
#'
#' ## check intersections and unions
#'
get_id <- function(x, which=c("linelist","contacts","all","common","from","to")){
    ## Issues with linelist and contacts is that there is no telling how much overlap there are
    ## between the two datasets; whenever looking for a list of unique identifiers, one could in
    ## fact refer to 6 different things which are covered here.

    ## checks
    if (!inherits(x, "epi_contacts")) {
        stop("x is not an 'epi_contacts' object")
    }
    which <- match.arg(which)

    ## get IDs
    if (which=="linelist") {
        out <- x$linelist$id
    }
    if (which=="contacts") {
        out <- unique(unlist(x$contacts[,1:2]))
    }
    if (which=="all") {
        out <- unique(c(x$linelist$id, unlist(x$contacts[,1:2])))
    }
    if (which=="common") {
        out <- intersect(x$linelist$id, unique(unlist(x$contacts[,1:2])))
    }
    if (which=="from") {
        out <- x$contacts$from
    }
    if (which=="to") {
        out <- x$contacts$to
    }

    return(out)
}

