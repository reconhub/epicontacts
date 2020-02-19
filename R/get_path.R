#' Extract the IDs on or the length of the path between cases
#'
#' This function extracts the path between cases and returns the IDs on this
#' path or the length of these paths. The shortest, longest or all paths can be
#' extracted.
#'
#' @export
#'
#' @author
#' Finlay Campbell (\email{finlaycampbell93@@gmail.com})
#'
#' @param x an \code{\link{epicontacts}} object.
#'
#' @param from a vector of IDs indicating the starting point of each path.
#'
#' @param to a vector of IDs of the same length as \code{from} indicating the
#'   ending point of each path.
#'
#' @param which one of "shortest", "longest" or "all", indicating which paths to
#'   be returned. Ties are resolved by picking the first path returned by the
#'   \code{igraph} function \code{all_simple_paths}.
#'
#' @param output "ids" will return a character vector of IDs on each path;
#'   "generations" will return the number of generations in each path.
#'
#' @param simplify logical indicating whether list outputs should be simplified
#'   when only one path is investigated.
#'
#' @return For each path, either a vector of IDs on that path or the length of
#'   that path. These are be returned as a list with one list element for each
#'   path, unless only one path is indicated and simplified = TRUE.
#' 
#' @examples
#' if (require(outbreaks)) {
#'
#' x <- make_epicontacts(linelist=mers_korea_2015[[1]],
#' contacts=mers_korea_2015[[2]], directed=TRUE)
#'
#' ## get the IDs on the longest path between cases
#' get_path(x, c("SK_1", "SK_10"), c("SK_54", "SK_76"), "longest", "ids")
#' 
#' ## get the length of these paths
#' get_path(x, c("SK_1", "SK_10"), c("SK_54", "SK_76"), "longest", "generations")
#' 
#' }
get_path <- function(x, from, to,
                     which = c("shortest", "longest", "all"),
                     output = c("ids", "generations"),
                     simplify = TRUE) {

  ## check matching length
  if(length(from) != length(to)) {
    stop("from and to must be the same length")
  }

  ## convert to character as igraph stores IDs internally as such
  from <- as.character(from)
  to <- as.character(to)

  ## test for missing IDs
  unq <- unique(c(from, to))
  culprit <- unq[!unq %in% get_id(x, "all")]
  if(length(culprit) > 0) {
    stop(sprintf("The following IDs are not found in the epicontacts object: %s",
                 paste0(culprit, collapse = ", ")))
  }

  ## match args
  which <- match.arg(which)
  output <- match.arg(output)

  ## identify all paths
  g <- as.igraph.epicontacts(x)
  paths <- mapply(igraph::all_simple_paths, from, to,
                  MoreArgs = list(graph = g),
                  SIMPLIFY = FALSE)

  ## recode missing paths to NA
  paths[lapply(paths, length) == 0] <- NA

  ## extract IDs from single path using function f to choose on length
  get_single_path <- function(y, f) {
    if(anyNA(y)) {
      return(NA)
    } else {
      return(igraph::as_ids(y[[f(lapply(y, length))]]))
    }
  }

  ## return all path IDs
  get_all_paths <- function(y) {
    if(anyNA(y)) {
      return(NA)
    } else {
      y <- lapply(y, igraph::as_ids)
      names(y) <- paste0("path_", seq_along(y))
      return(y)
    }
  }

  ## extract IDs and generations
  if(which %in% c("shortest", "longest")) {
    out <- lapply(paths, get_single_path,
                  ifelse(which == "shortest", which.min, which.max))
    if(output == "generations") {
      out <- vapply(out, function(y) ifelse(anyNA(y), NA, length(y) - 1), 1)
    }
  } else if(which == "all") {
    out <- lapply(paths, get_all_paths)
    if(output == "generations") {
      out <- lapply(out, function(y)
        return(if(anyNA(y)) NA else vapply(y, length, 1L) - 1))
    }
  }

  ## name outputs
  names(out) <- paste0(from, "_to_", to)

  ## simplify output if necessary and possible
  if(simplify && is.list(out) && length(out) == 1L) {
    out <- out[[1]]
  }
  
  return(out)

}
