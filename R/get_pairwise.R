#' Find pairwise node characteristics for epi_contacts objects
#'
#' This function finds all instances of pairwise node attributes for contact pairs in a \code{\link{epi_contacts}}
#' dataset, and can process them as factors, numeric or date.
#' Factor returns all pairwise factors. Numeric returns absolute differences between contact pairs.
#' Date returns absolute difference between dates.
#'
#' @export
#'
#' @author Tom Crellen (\email{tomcrellen@@gmail.com})
#'
#' @param x an \code{\link{epi_contacts}} object
#'
#' @param node_value the node attribute to be examined between contact pairs
#'
#' @param type of operation to be performed on node value. Can take forms "factor", "numeric", or "date"

get_pairwise <- function(x, node_value, type=c("factor", "numeric", "date")){
    ## checks
    if (!inherits(x, "epi_contacts")) {
        stop("x is not an 'epi_contacts' object")
    }
    if (!node_value %in% names(x$linelist)){
        stop("node value does not exist, possibe node values: ",
             paste(names(x$linelist),collapse =", "))
    }

    #Messy process to combine dataframes - from and to - with "node value"
    df.from <- data.frame(id = x$contacts$from)
    df.from$id <- as.character(df.from$id)
    df.from.value <- dplyr::left_join(df.from, x$linelist, by="id")

    df.to <- data.frame(id = x$contacts$to)
    df.to$id <- as.character(df.to$id)
    df.to.value <- dplyr::left_join(df.to, x$linelist, by="id")

    #nodes is final df with 4 columns
    nodes <- data.frame(from = df.from.value$id, from_value =df.from.value[,node_value],
                        to=df.to.value$id, to_value=df.to.value[,node_value])

    #For factors
    if (type=="factor") {
        if (!is.factor(x$linelist[,node_value])) {
            stop("node value is not a factor")}
        pairwise<- paste(as.character(nodes$from_value),as.character(nodes$to_value), sep=",")
        return(pairwise)}

    if (type=="numeric") {
        if (!is.numeric(x$linelist[,node_value])) {
            stop("node value is not numeric")}
        pairwise<- abs(nodes$to_value - nodes$from_value)
        return(pairwise)}

    if (type=="date"){
        if (!lubridate::is.Date(x$linelist[,node_value])) {
            stop("node value is not a date")}
        pairwise <- abs(nodes$to_value - nodes$from_value)
        pairwise <- as.numeric(pairwise)
        return(pairwise)}
        }