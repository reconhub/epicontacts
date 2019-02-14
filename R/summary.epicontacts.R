#' Summary method for epicontacts objects
#'
#' This method outputs a summary of the content of \code{epicontacts} objects.
#'
#' @export
#'
#' @aliases summary_epicontacts
#'
#' @author VP Nagraj (\email{vpnagraj@virginia.edu})
#'
#' @param object an \code{\link{epicontacts}} object
#'
#' @param ... further parameters to be passed to other methods (currently not used)
#'
summary.epicontacts <- function(object, ...){

    x <- object

    res <- list()

    res$n_id_linelist <- length(get_id(x, "linelist"))
    res$n_id_contacts <- length(get_id(x,"contacts"))
    res$n_id_common   <- length(get_id(x, "common"))

    na_from     <- sum(is.na(x$contacts$from))
    res$na_from <- if (na_from == 0) NULL else na_from
    na_to       <- sum(is.na(x$contacts$to))
    res$na_to   <- if (na_to == 0) NULL else na_to

    res$n_contacts <- nrow(x$contacts)

    from_in_linelist <- x$contacts$from %in% get_id(x, "linelist")
    to_in_linelist <- x$contacts$to %in% get_id(x, "linelist")
    res$prop_contacts_in_linelist <- mean(from_in_linelist & to_in_linelist)

    res$deg_in <- summary(get_degree(x, "in"))
    res$deg_out <- summary(get_degree(x, "out"))
    res$deg_both <- summary(get_degree(x, "both"))

    res$linelist_attributes <- names(x$linelist[,-1])
    res$contacts_attributes <- names(x$contacts[,-c(1,2)])

    class(res) <- "summary_epicontacts"
    return(res)
}
