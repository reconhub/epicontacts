#' Interactive 3D Force-directed graph from epi_contact object
#'
#' This function creates a 3D graph from an epi_contact object
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epicontacts an \code{\link{epicontacts}} object
#'
#' @param label_column the column name in \code{linelist} to be used for node
#'     labels. Default is \code{id}
#'
#' @param v_col_by the column name in \code{linelist} to be used for node colors
#'
#' @param v_col the color of all nodes in graph, unless \code{v_col_by} is
#'     defined. Default is 'darkturquoise'
#'
#' @param g_title the title of the graph, if needed
#'
#' @param g_bg background color of graph. Default is 'black'
#'
#' @param g_fg foreground color of graph. Default is 'darkturquoise'
#'
#' @param v_size size of nodes in graph. Default is 1
#'
#' @param e_size width of edges in graph. Default is 0.5
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
#' ## build data
#' x <- make_epicontacts(ebola_sim$linelist, ebola_sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' ## subset based on single cluster size
#' x_subset <- subset_clusters_by_size(x, 10, 12)
#'
#' ## 3D graph
#' g <- graph3D(x_subset)
#' }

graph3D <- function(epicontacts,
                    label_column = "id",
                    v_col_by = "NA",
                    v_col = "darkturquoise",
                    g_title = "",
                    g_bg = "white",
                    g_fg = "darkturquoise",
                    v_size = 1,
                    e_size = .5) {

    ## Create igraph object to pass on as data for 3D graph (because original
    ## epicontacts object may contain NA's, which will hinder creation of 3D
    ## graph with threejs::graphjs()

    x <- as.igraph.epicontacts(epicontacts)

    ## Get vertex attributes and prepare as input for graph
    v <- igraph::get.vertex.attribute(x)
    v <- as.data.frame(v, stringsAsFactors = FALSE)

    v$label <- v$id ## assigning the unique identifier as default label
    v$id = 1:nrow(v) ## required by threejs::graphjs(). It has to be an integer.

    # Set node attributes
    if(v_col_by == "NA" & v_col == "darkturquoise") {
        v$color = "darkturquoise"
    } else {
        if(v_col_by == "NA" & v_col != "darkturquoise") {
            v$color = v_col
        } else {
            if(v_col_by != "NA") {
                gp <- as.character(v[[v_col_by]])
                pal <- colorspace::rainbow_hcl(length(unique(gp[ !is.na(gp) ])))
                cols <- pal[factor(gp)]
                cols[ is.na(cols) ] <- "lightgrey"
                v$color <- cols
                v$label <- sprintf( "id: %s, %s: %s", v$label, v_col_by, gp )
            }
        }
    }

    ## Get edge list and format prepare as input for graph
    e <- igraph::get.edgelist(x, names=FALSE)
    e <- as.data.frame(e, stringsAsFactors = FALSE)
    colnames(e) = c("from", "to")

    ## Set edge attributes
    e$size = e_size
    e$color = "lightgrey"

    ## Set vertex attributes
    v$size = v_size

    ## Subset vertex dataframe for graphjs
    v <- v[ , c("label", "id", "size", "color")]

    ## Create 3D graph
    g <- threejs::graphjs(edges = e, nodes = v, main = g_title,
                          showLabels=FALSE, fg = g_fg, bg = g_bg)
    return(g)
}

