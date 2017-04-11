#' Interactive 3D Force-directed graph from epi_contact object
#'
#' This function creates a 3D graph from an epi_contact object
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param x An \code{\link{epicontacts}} object
#'
#' @param group An index or character string indicating which field of the
#'     linelist should be used to color the nodes. Default is \code{id}
#'
#' @param annot A logical indicating whether nodes should be annoted upon
#'     mouseover. Default is \code{TRUE}.
#'
#' @param col_pal A color palette for the groups.
#'
#' @param NA_col The color used for unknown group.
#'
#' @param g_title The title of the graph.
#'
#' @param bg_col The background color of graph.
#'
#' @param label_col The color of the graph title and labels of groups.
#'
#' @param node_size The sizes of graph nodes.
#'
#' @param edge_size The width of graph edges.
#'
#' @note All colors must be specified as color names like "red", "blue", etc. or
#' as hexadecimal color values without opacity channel, for example "#FF0000", "#0a3e55"
#' (upper or lower case hex digits are allowed).
#'
#' Double-click or tap on the plot to reset the view.
#'
#' @return
#' An htmlwidget object that is displayed using the object's show or print method.
#' (If you don't see your widget plot, try printing it with the \code{print} function.)
#'
#' @references
#'
#' Original three.js code by David Piegza:
#'     \url{https://github.com/davidpiegza/Graph-Visualization}.
#'
#' Original rthreejs code by B. W. Lewis:
#' \url{https://github.com/bwlewis/rthreejs}.
#'
#' @examples
#' if (require(outbreaks)) {
#' x <- make_epicontacts(linelist = mers_korea_2015$linelist,
#'                       contacts = mers_korea_2015$contacts,
#'                       directed = FALSE)
#'
#' \dontrun{
#' graph3D(x)
#' graph3D(x, group = "sex", g_title = "MERS Korea 2014")
#' }
#' }

graph3D <- function(x,
                    group = "id",
                    annot = TRUE,
                    col_pal = cases_pal,
                    NA_col = "lightgrey",
                    g_title = "",
                    bg_col = "white",
                    label_col = "darkgrey",
                    node_size = 1,
                    edge_size = .5) {

    ## Create igraph object to pass on as data for 3D graph (because original
    ## epicontacts object may contain NA's, which will hinder creation of 3D
    ## graph with threejs::graphjs()

    x <- subset_clusters_by_size(x, cs_min = 2)
    x <- as.igraph.epicontacts(x)

    ## Get vertex attributes and prepare as input for graph
    nodes <- igraph::get.vertex.attribute(x)
    nodes <- as.data.frame(nodes, stringsAsFactors = FALSE)

    # attribute for grouping
    nodes$group <- as.character(nodes[,group])
    nodes$group[is.na(nodes$group)] <- "NA"
    nodes$group <- factor(nodes$group)

    # changing original "id" column to one required by threejs::graphjs()
    #   & backing up old id
    nodes$orig_id <- nodes$id
    nodes$id <- 1:nrow(nodes) # has to be integer



    # Set node attributes
    # node color
    K <- length(unique(nodes$group))
    grp.col <- col_pal(K)
    grp.col[levels(nodes$group)=="NA"] <- NA_col

    nodes$color <- grp.col[factor(nodes$group)]

    if(annot) {
        if(group == "id") {
            nodes$label <- nodes$label <- sprintf( "id: %s", nodes$orig_id)
        } else {
            nodes$label <- sprintf( "id: %s, %s: %s",
                                   nodes$orig_id,group, nodes$group)
        }
    } else {
        nodes$label = ""
    }


    ## Get edge list and format prepare as input for graph
    edges <- igraph::get.edgelist(x, names=FALSE)
    edges <- as.data.frame(edges, stringsAsFactors = FALSE)
    colnames(edges) = c("from", "to")

    ## Set edge attributes
    edges$size = edge_size
    edges$color = "lightgrey"

    ## Set vertex attributes
    nodes$size = node_size

    ## Subset vertex dataframe for graphjs
    nodes <- nodes[ , c("group", "id", "orig_id", "size", "color", "label")]

    ## Create 3D graph
    g <- threejs::graphjs(edges = edges, nodes = nodes, main = g_title,
                          showLabels=FALSE, fg = label_col, bg = bg_col)
    return(g)
}


