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
#' @param cex a size factor for the nodes of the network
#'
#' @param lab.cex a size factor for the tip annotations (genetic clusters)
#'
#' @param col.pal a color palette to be used for the genetic clusters (tips)
#'
#' @param col.internal a color for internal nodes
#'
#' @param plot a logical indicating whether a plot should be displayed
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
vis_epi_contacts <- function(x,  cex=1, lab.cex=1, plot=TRUE, legend=TRUE,
                              selector=TRUE, editor=TRUE,
                              col.pal=dibbler.pal2, col.internal="#b2accb",
                              width="90%", height="700px", ...){
    ## convert input graph to visNetwork inputs
    out <- igraph2visNetwork(x$graph)
    nodes <- out$nodes$id
    edges <- out$edges

    ## basic variables
    id.terminal <- which(!nodes %in% out$edges$from)
    id.basal <- which(!nodes %in% out$edges$to)
    id.internal <- which(!nodes %in% nodes[id.terminal])
    N <- length(nodes)
    N.internal <- length(id.internal)
    N.tips <- length(id.terminal)
    K <- length(levels(x$group))


    ## NODES ##
    ## LABELS
    out$nodes$label <- nodes

    ## GROUP
    ## (groups will define color)
    v.group <- rep("internal", N)
    names(v.group) <- nodes
    v.group[names(x$group)] <- as.character(x$group)

    ## set value
    out$nodes$group <- v.group

    ## TITLE
    ## (used when hovering over nodes)


    ## SHAPE
    ## vertex shapes
    v.shape <- rep("dot", N)
    v.shape[id.terminal] <- "triangle"
    v.shape[id.basal] <- "diamond"

    ## set value
    out$nodes$shape <- v.shape

    ## SIZES
    out$nodes$value <- sapply(nodes, function(e) sum(e == out$edges$from))


    ## EDGES ##
    ## SHAPES
    ## set value
    out$edges$arrows <- "to"

    ## COLORS
    out$edges$color <- col.internal

    ## OUTPUT
    ## escape if no plotting
    if(!plot) return(invisible(out))

    ## visNetwork output
    out <- visNetwork::visNetwork(nodes=out$nodes, edges=out$edges,
                                  width=width, height=height, ...)

    ## add group info/color
    out <- out %>% visNetwork::visGroups(groupname = "internal", color = col.internal)
    grp.col <- col.pal(K)
    for(i in seq.int(K)){
        out <- out %>% visNetwork::visGroups(groupname = levels(x$group)[i], color = grp.col[i])
    }

    ## add legend
    if(legend){
        out <- out %>% visNetwork::visLegend()
    }

    ## add selector / editor
    if(selector) {
        selectedBy <- "group"
    } else {
        selectedBy <- NULL
    }
    out <- out %>% visNetwork::visOptions(selectedBy=selectedBy, manipulation=editor)

    ## set nodes borders
    out <- out %>% visNetwork::visNodes(borderWidth=2)
    return(out)
}
