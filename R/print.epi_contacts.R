#' Print method for epi_contacts objects
#'
#' This method prints the content of \code{epi_contacts} objects, giving a brief summary of the
#' reported cases and contacts.
#'
#' @export
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
print.epi_contacts <- function(x, ...){
    cat("\n/// Epidemiological Contacts //\n")
    cat("\n  // class:", paste(class(x), collapse=", "))
    cat("\n  //", format(nrow(x$linelist),big.mark=","),
        "cases in linelist;",
        format(nrow(x$contacts), big.mark=","),
        "contacts; ", ifelse(x$directed, "directed", "non directed"),
        "\n")

    cat("\n  // linelist\n\n")
    print(dplyr::tbl_df(x$linelist))

    cat("\n  // contacts\n\n")
    print(dplyr::tbl_df(x$contacts))

    cat("\n")
}
