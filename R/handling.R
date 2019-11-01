#' Subset epicontacts objects based on case identifiers
#'
#' The "[" operator can be used to subset \code{\link{epicontacts}} objects,
#' retaining a specified set of case identifiers (\code{i} for the linelist,
#' \code{j} for contacts). Note that unlike most classical R objects, there is
#' no replacement method for \code{\link{epicontacts}} objects, i.e. no
#' operations such as \code{foo[i] <- bar}.
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param x An \code{\link{epicontacts}} object
#'
#' @param i A character vector containing case ID to be retained in the
#'     linelist; alternatively, an integer or logical vector used to subset the
#'     rows of the \code{$linelist} component.
#'
#' @param j A character vector containing case ID to be retained in the
#'     contacts; alternatively, an integer or logical vector used to subset
#'     the rows of the \code{$contacts} component.
#'
#' @param contacts A character string indicating the rules for retaining
#'     contacts when \code{j} indicates case IDs (see details).
#'
#' @param k An integer, logical, or character vector subsetting the
#'     supplementary columns of \code{x$linelist}, i.e. the columns after 'id';
#'     i.e. \code{k=1} refers to the column immediately after 'id'.
#'
#' @param l An integer, logical, or character vector subsetting the
#'     supplementary columns of \code{x$contacts}, i.e. the columns after 'from'
#'     and 'to'; i.e. \code{l=1} refers to the column immediately after 'to'.

#' @param ... Not used (there for compatibility with generic).
#'
#' @details
#' Details on the 'contacts' argument; possible values are:
#'
#' \itemize{
#'
#' \item 'both': contacts are retained only if both cases are in \code{j}
#'
#' \item 'either': contacts are retained if at least one of the cases is in
#' \code{j}
#'
#' \item 'from': contacts are retained only if the source ('from') is in
#' \code{j}
#'
#' \item 'to': contacts are retained only if the recipient ('to') is in \code{j}
#' }
#'
#' @seealso \code{\link{thin}} to retain matching cases in linelist or contacts.
#'
#' @examples
#' if (require(outbreaks)) {
#' ## build data
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                        id = "case_id", to = "case_id", from = "infector",
#'                        directed = TRUE)
#'
#' ## subset first 10 linelist cases
#' x[1:10]
#'
#' ## same, remove contacts
#' x[1:10, j = FALSE]
#'
#' ## subset first 10 contacts
#' x[j = 1:10]
#'
#' ## remove the metadata
#' x[k = FALSE, j = FALSE]
#'
#' ## keep contacts where both cases are in linelist
#' x[j = get_id(x, "linelist"), contacts = "both"]
#'
#' ## keep contacts from a specific case '916d0a'
#' x[j = "916d0a", contacts = "from"]
#'
#' ## more complex: keep all cases and contacts with > 4 secondary contacts
#' ## i) find cases to keep
#' temp <- table(x$contacts$from)
#' temp[temp > 4]
#' to.keep <- names(temp)[temp > 4]
#' to.keep
#'
#' ## subset the contacts
#' y <- x[j = to.keep, contacts = "either"]
#' y
#'
#' ## keep only relevant entries in the linelist
#' y <- thin(y)
#'
#' ## visualise result
#' plot(y)
#' }
"[.epicontacts" <- function(x, i, j,
                            k = TRUE, l = TRUE,
                            contacts = c("both", "either", "from", "to"),
                            ...) {
  ## In all the following, i is used to subset the linelist, j to subset
  ## contacts. The behaviour is as follows:

  ## i: if i is character, keep rows for which 'id' is in 'i'; if logical /
  ## numeric / integer, subset the rows of $linelist

  ## j: if j is character, keep rows for which 'from/to' are in 'j'; the
  ## argument 'contacts' specify if either, both, and one of the nodes should
  ## be i 'j'; if logical / numeric / integer, subset the rows of $linelist

  ## 'k' and 'l' are used to subset the columns of attributes in x$linelist
  ## and x$contacts, respectively; any usual subsetting is fine for these ones
  ## (index, logical, name), although the 'id', 'from' and 'to' columns are
  ## discarded from the subsetting. That is:

  ## - in x$linelist: k=1 will refer to the 2nd column (i.e. after 'id')

  ## - in x$contacts: l=1 will refer to the 3nd column (i.e. after 'from' and
  ## 'to')

  ## subset $linelist
  if (missing(i)) {
    i <- TRUE
  }
  if (inherits(i, 'Date')) {
    stop("Cannot subset by date")
  }
  if (is.character(i)) {
    i <- x$linelist$id %in% i
  }
  

  x$linelist <- x$linelist[i, , drop=FALSE]

  ## make sure 'id' is the first column, keep columns 'k'
  if (ncol(x$linelist) > 1) {
    x$linelist <- data.frame(c(x$linelist[1],
                               x$linelist[-1][k]),
                             stringsAsFactors = FALSE)
  }


  ## subset $contacts
  if (missing(j)) {
    j <- TRUE
  }

  ## subsetting based on node ids
  if (is.character(j)) {
    contacts <- match.arg(contacts)
    if (contacts == "both") {
      j <- (x$contacts$from %in% j) & (x$contacts$to %in% j)
    }
    if (contacts == "either") {
      j <- (x$contacts$from %in% j) | (x$contacts$to %in% j)
    }
    if (contacts == "from") {
      j <- x$contacts$from %in% j
    }
    if (contacts == "to") {
      j <- x$contacts$to %in% j
    }
  }
  if (inherits(j, 'Date')) {
    stop("Cannot subset by date")
  }


  x$contacts <- x$contacts[j, , drop=FALSE]

  ## make sure from/to are the first 2 columns, keep columns 'l'
  if (ncol(x$contacts) > 2) {
    x$contacts <- data.frame(c(x$contacts[1:2],
                               x$contacts[-c(1:2)][l]),
                             stringsAsFactors = FALSE)

  }

  return(x)
}
