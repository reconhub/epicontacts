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
#' @param na_rm_linelist a logical indicating whether linelist elements with
#'   NA IDs should be kept
#'
#' @param na_rm_contacts a logical indicating whether contacts with
#'   NA IDs should be kept
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
#' x <- make_epicontacts(linelist, contacts, id = "case_id",
#'                        to = "case_id", from = "infector")
#' head(x$linelist)
#' head(x$contacts)
#' }
make_epicontacts <- function(linelist, contacts, id = 1L, from = 1L, to = 2L,
                             directed = FALSE,
                             na_rm_linelist = TRUE,
                             na_rm_contacts = TRUE) {
  ## We read data from linelist, which needs to contain at least one case, and
  ## contacts. Sanity checks will include standard class
  ## and dimensionality checks, as well as uniqueness of IDs in the line list,
  ## and enforcing 'character' type for the unique IDs, and naming the case ID
  ## field 'id'. We also reorder data so that the first column of 'linelist'
  ## is 'id', and the first two columns of 'contacts' are 'from' and 'to'

  ## process linelist ##
  ## checks

  if (is.null(linelist)) {
    stop("linelist is NULL")
  }

  if (is.vector(linelist) &&
      length(linelist) == 1L &&
      is.na(linelist)) {
    stop("linelist is NA")
  }

  linelist <- as.data.frame(linelist, stringsAsFactors = FALSE)

  if (nrow(linelist) < 1L) {
    stop("linelist should have at least one row")
  }

  if (is.character(id)) {
    id <- match(id, names(linelist))
  }

  ## reordering of variables
  names(linelist)[id] <- "id"
  linelist <- subset(linelist,
                     select = c(id, setdiff(seq_len(ncol(linelist)), id)))

  ## convert factors and dates to characters
  if (inherits(linelist$id, c("Date", "factor"))) {
    linelist$id <- as.character(linelist$id)
  }

  ## remove linelist elements with NA ids
  linelist_na <- is.na(linelist$id)
  if (any(linelist_na)) {
    if (na_rm_linelist) {
      warning("Removed ", sum(linelist_na), " linelist element(s) with NA IDs; ",
              "to keep these elements, set na_rm_linelist = FALSE")
      linelist <- subset(linelist, !linelist_na)
    } else {
      if (sum(linelist_na) > 1) {
        warning(sum(linelist_na), " NA IDs in the linelist have been renamed ",
                "NA_1 to NA_", sum(linelist_na))
      } else {
        warning("1 NA ID in the linelist has renamed to NA_1")
      }
      linelist$id[linelist_na] <- paste0("NA_", seq_len(sum(linelist_na)))
    }
  }

  ## test for duplicates
  if (sum(temp <- duplicated(linelist$id)) > 0) {
    msg <- paste(linelist$id[temp], collapse = " ")
    stop("Duplicated IDs detected in the linelist; culprits are: ", msg)
  }

  ## process contacts ##
  ## checks

  if (is.null(contacts)) {
    stop("contacts is NULL")
  }

  if (is.vector(contacts) &&
      length(contacts) == 1L &&
      is.na(contacts)) {
    stop("contacts is NA")
  }

  contacts <- as.data.frame(contacts, stringsAsFactors = FALSE)

  if(nrow(contacts) < 1L) {
    stop("contacts should have at least one row")
  }
  if(ncol(contacts) < 2L) {
    stop("contacts should have at least two columns")
  }

  ## reordering
  if (is.character(from)) {
    from <- match(from, names(contacts))
  }
  if (is.character(to)) {
    to <- match(to, names(contacts))
  }
  names(contacts)[c(from, to)] <- c("from", "to")
  contacts <- contacts[, c(from,
                           to,
                           setdiff(seq_len(ncol(contacts)), c(from,to)))]

  ## ensure identical ID classes across linelist and contacts
  ## unless all IDs numeric or integer, convert to character
  ## if all numeric or integer then, unless all integer, convert to numeric
  if (!(inherits(linelist$id, c("numeric", "integer")) &&
        inherits(contacts$from, c("numeric", "integer")) &&
        inherits(contacts$to, c("numeric", "integer")))) {
    linelist$id <- as.character(linelist$id)
    contacts$from <- as.character(contacts$from)
    contacts$to <- as.character(contacts$to)
  } else if (!(inherits(linelist$id, c("integer")) &&
               inherits(contacts$from, c("integer")) &&
               inherits(contacts$to, c("integer")))) {
    linelist$id <- as.numeric(linelist$id)
    contacts$from <- as.numeric(contacts$from)
    contacts$to <- as.numeric(contacts$to)
  }

  ## remove contact elements with NA ids
  contacts_na <- is.na(contacts[c("from", "to")])
  if (any(contacts_na)) {
    if(na_rm_contacts) {
      warning("Removed ", sum(contacts_na), " contact(s) with NA IDs; ",
              "to keep these contacts, set na_rm_contacts = FALSE")
      contacts <- contacts[!apply(contacts_na, 1, any),]
    } else {
      if (sum(contacts_na) > 1) {
        warning(sum(contacts_na), " NA IDs in the contacts have been renamed ",
                "NA_", sum(linelist_na) + 1, " to NA_",
                sum(contacts_na) + sum(linelist_na) + 1)
      } else {
        warning("1 NA ID in the contacts has renamed to NA_", sum(linelist_na) + 1)
      }
      ## index for numbering NAs by row
      ind <- which(t(contacts_na), TRUE)[,c(2, 1)]
      num <- seq(sum(linelist_na) + 1, length = nrow(ind))
      contacts[c("from", "to")][ind] <- paste0("NA_", num)
    }
  }

  ## warn of duplicated contacts
  contacts_dupl <- duplicated(contacts[1:2])
  if (any(contacts_dupl)) {
    warning("The contact(s) listed on row(s) ",
            paste(which(contacts_dupl), collapse = ", "),
            " are duplicates: this may be unwanted")
  }

  ## warn of self-contacts
  contacts_self <- contacts$from == contacts$to
  if(any(contacts_self, na.rm = TRUE)) {
    warning("The contact(s) listed on row(s) ",
            paste(which(contacts_self), collapse = ", "),
            " are between a case and itself: this may be unwanted")
  }
  
  ## Build final output
  out <- list(linelist = linelist, contacts = contacts, directed = directed)
  class(out) <- c("epicontacts")

  ## Check for loops (A -> B and B -> A)
  unq <- unique(contacts[1:2])
  unq <- subset(unq, unq$from != unq$to)
  unq <- rbind(unq, setNames(rev(unq), names(unq)))
  loop_found <- any(duplicated(unq))

  ## Check for cycles (A -> B -> C -> A)
  out_i <- as.igraph.epicontacts(out, na_rm = TRUE)
  cycle_found <- igraph::girth(out_i)$girth > 0

  ## Throw warning if found
  if(loop_found | cycle_found) {
    warning("Cycle(s) detected in the contact network: this may be unwanted")
  }
  
  return(out)
}
