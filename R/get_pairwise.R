#' Characterise contacts by comparing case attributes
#'
#' This function extract attributes of cases involved in contacts using case information provided in
#' the linelist of an \code{\link{epi_contacts}} dataset. If not provided, the function used to
#' process attributes will adjust to the type of attribute selected (see details).
#'
#' @export
#'
#' @author
#' Tom Crellen (\email{tomcrellen@@gmail.com})
#' Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @param x an \code{\link{epi_contacts}} object
#'
#' @param attribute the attribute to be examined between contact pairs
#'
#' @param f a function processing the attributes of 'from' and 'to'
#'
#' @param hard_NA a logical indicating if the output should be NA whenever one of the paired values
#' is NA (TRUE, default); otherwise, 'NA' may be treated as another character (e.g. when pasting paired
#' values)
#'
#' @examples
#' ## example using MERS outbreak in Korea, 2014
#' head(mers_kor_14[[1]])
#' head(mers_kor_14[[2]])
#'
#' x <- make_epi_contacts(linelist=mers_kor_14[[1]],
#' contacts=mers_kor_14[[2]], directed=TRUE)
#'
#' ## estimate serial interval (onset->onset)
#' SI <- get_pairwise(x, "dt_onset")
#' SI
#' summary(SI)
#' hist(SI, col="grey", border="white", xlab="Days after symptoms",
#'      main="MERS Korea 2014 - Serial Interval")
#'
#' ## check gender mixing:
#' get_pairwise(x, "sex") # not good, we want 2-way table
#'
#' get_pairwise(x, "sex", f=table) # use custom function
#' fisher.test(get_pairwise(x, "sex", f=table)) # test association
#'
get_pairwise <- function(x, attribute, f=NULL, hard_NA=TRUE){
    ## This function pulls values of a variable defined in the linelist for the 'from' and 'to' of
    ## the contacts. 'f' is the function processing these paired values, with some pre-defined
    ## behaviours for some types (dates, numeric); 'hard_NA' defines the behaviour for NAs, and if
    ## TRUE will enforce a NA wherever the pair contained at least one NA.

    ## checks
    if (!inherits(x, "epi_contacts")) {
        stop("x is not an 'epi_contacts' object")
    }
    if (!is.character(attribute)) {
        attribute <- names(x$linelist)[attribute]
    }
    if (!attribute %in% names(x$linelist)){
        stop("attribute does not exist; available attributes are: ",
             paste(names(x$linelist)[-1], collapse =", "))
    }

    ## find values for from and to
    values <- x$linelist[, attribute, drop=TRUE]
    names(values) <- x$linelist$id
    values.from <- values[x$contacts$from]
    values.to <- values[x$contacts$to]
    ori.NA <- is.na(values.from) | is.na(values.to)

    ## define default function if not provided:
    ## - for 'Date': absolute number of difference in days
    ## - for 'numeric'/'integer': absolute difference
    ## - for other stuff: paste values
    if (is.null(f)){
        if (inherits(values, "Date")) {
            f <- function(a, b) {
                as.integer(abs(a-b))
            }
        } else if (is.numeric(values)) {
            f <- function(a, b) {
                abs(a-b)
            }
        } else {
            f <- function(a, b){
                sep <- ifelse(x$directed, " -> "," - ")
                paste(a, b, sep=sep)
            }
        }
    }
    out <- f(values.from, values.to)
    if (length(out)==length(ori.NA) && hard_NA) {
        out[ori.NA] <- NA
    }

    return(out)
}
