#' Access unique identifiers in epicontacts objects
#'
#' This accessor is used to extract unique identifiers from
#' \code{\link{epicontacts}} objects. The argument 'which' can be used to
#' specify if IDs should include: linelist only ('linelist'), contacts only
#' ('contacts'), the union of both ('all'), or the intersection of both
#' ('common'); two additional options are 'from' (ID 'giving' contacts) and 'to'
#' (ID 'receiving' contacts).
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param x an \code{\link{epicontacts}} object
#'
#' @param which the type of ID to return (see description); value can be
#'     'linelist', 'contacts', 'all', 'common', 'from' or 'to'.
#'
#' @param na.rm a `logical` indicating if `NA` should be removed from the output
#'   (`TRUE`, default) or not.
#'
#' @return x a character vector of unique identifiers
#'
#' @examples
#'
#' if (require(outbreaks)) {
#' ## build data
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                        id="case_id", to="case_id", from="infector",
#'                        directed=TRUE)
#'
#' ## get identifiers
#' id1 <- get_id(x, "linelist")
#' id2 <- get_id(x, "contacts")
#' id3 <- get_id(x, "all")
#' id4 <- get_id(x, "common")
#'
#' ## check intersections and unions
#' all.equal(union(id1, id2), id3)
#' all.equal(intersect(id1, id2), id4)
#'
#' }
#'

get_id <- function(x, which = c("linelist", "contacts", "all", "common", "from", "to"),
                   na.rm = TRUE){
  ## Issues with linelist and contacts is that there is no telling how much
  ## overlap there are between the two datasets; whenever looking for a list
  ## of unique identifiers, one could in fact refer to 6 different things
  ## which are covered here.

  ## checks
  if (!inherits(x, "epicontacts")) {
    stop("x is not an 'epicontacts' object")
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
    if (!x$directed) {
      warning("x is not directed; 'from' has no particular meaning")
    }
    out <- unique(x$contacts$from)
  }
  if (which=="to") {
    if (!x$directed) {
      warning("x is not directed; 'to' has no particular meaning")
    }
    out <- unique(x$contacts$to)
  }
  if (na.rm) {
    to_keep <- !is.na(out)
    out <- out[to_keep]
  }

  return(out)
}
