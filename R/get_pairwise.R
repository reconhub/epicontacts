#' Find pairwise node characteristics for epi_contacts objects
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
#' @param type of operation to be performed on node value. Can take forms "factor", "numeric", or "date"

get_pairwise <- function(x, attribute, f=NULL){
    ## checks
    if (!inherits(x, "epi_contacts")) {
        stop("x is not an 'epi_contacts' object")
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

    ## define default function if not provided:
    ## - for 'Date': absolute number of difference in days
    ## - for 'numeric'/'integer': absolute difference
    ## - for other stuff: paste values
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

    out <- f(values.from, values.to)
    return(out)
}
