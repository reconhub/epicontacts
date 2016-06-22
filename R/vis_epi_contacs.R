#' Plot epi_contacts objects
#'
#' This function plots \code{\link{epi_contacts}} objects using various approaches.
#'
#' @export
#'
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @return the same output as \code{visNetwork}
#'
#' @seealso \code{\link[visNetwork]{visNetwork}} in the package \code{visNetwork}.
#'
#' @param x an \code{\link{epi_contacts}} object
#'
#' @param legend a logical indicating whether a legend should be added to the plot
#'
#' @param selector a logical indicating whether a group selector tool should be added to the plot
#'
#' @param editor a logical indicating whether an editor tool should be added to the plot
#'
#' @param width the width of the output, in html compatible format (e.g. '90%' or '800px'
#'
#' @param height the height of the output, in html compatible format (e.g. '800px'
#'
#' @param ... further arguments to be passed to \code{visNetwork}
#'
#' @author Thibaut Jombart (\email{thibautjombart@@gmail.com})
#'
#' @importFrom visNetwork visNetwork visGroups visLegend visOptions visNodes
#'
#' @importFrom magrittr "%>%"
#'
#' @return the same output as \code{visNetwork}
#'
#' @seealso \code{\link[visNetwork]{visNetwork}} in the package \code{visNetwork}.
#'
#'
vis_epi_contacts <- function(x, legend=TRUE,
                             selector=TRUE, editor=TRUE,
                             col.pal=case_pal,
                             width="90%", height="700px", ...){

    ## make visNetwork inputs: nodes
    nodes <- x$linelist
    nodes$label <- nodes$id


    ## make visNetwork inputs: edges
    edges <- x$contacts
    if (x$directed) {
        edges$arrows <- "to"
    }

    ## OUTPUT

    ## visNetwork output
    out <- visNetwork::visNetwork(nodes, edges,
                                  width=width, height=height, ...)

    ## ## add group info/color
    ## out <- out %>% visNetwork::visGroups(groupname = "internal", color = col.internal)
    ## grp.col <- col.pal(K)
    ## for(i in seq.int(K)){
    ##     out <- out %>% visNetwork::visGroups(groupname = levels(x$group)[i], color = grp.col[i])
    ## }

    ## add legend
    if (legend) {
        out <- out %>% visNetwork::visLegend()
    }

    ## add selector / editor
    if (selector) {
        selectedBy <- "group"
    } else {
        selectedBy <- NULL
    }
    out <- out %>% visNetwork::visOptions(selectedBy=selectedBy, manipulation=editor)

    ## set nodes borders
    out <- out %>% visNetwork::visNodes(borderWidth=2)
    return(out)
}
