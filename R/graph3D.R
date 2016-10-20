#' Interactive 3D Force-directed graph from epi_contact object
#'
#' This function creates a 3D graph from an epi_contact object
#'
#' @export
#'
#' @author Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @param epi_contacts an \code{\link{epi_contacts}} object
#'
#' @param label_column the column name in \code{linelist} to be used for node labels. Default is \code{id}
#'
#' @param v_col_by the column name in \code{linelist} to be used for node colors
#'
#' @param v.col the color of all nodes in graph, unless \code{v_col_by} is defined. Default is 'darkturquoise'
#'
#' @param g.title the title of the graph, if needed
#'
#' @param g.bg background color of graph. Default is 'black'
#'
#' @param g.fg foreground color of graph. Default is 'darkturquoise'
#'
#' @param v.size size of nodes in graph. Default is 1
#'
#' @param e.size width of edges in graph. Default is 0.5
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
#' Original three.js code by David Piegza: \url{https://github.com/davidpiegza/Graph-Visualization}.
#' Original rthreejs code by B. W. Lewis: \url{https://github.com/bwlewis/rthreejs}.
#'
#' @examples
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#'                        id="case.id", to="case.id", from="infector",
#'                        directed=TRUE)
#'
#' ## subset based on single cluster size
#' x_subset <- subset_clusters_by_size(x, 10, 12)
#'
#' ## 3D graph
#' g <- graph3D(x_subset)
#'



graph3D <- function(epi_contacts,
                    label_column = "id",
                    v_col_by = "NA",
                    v.col = "darkturquoise",
                    g.title = "",
                    g.bg = "white",
                    g.fg = "darkturquoise",
                    v.size = 1,
                    e.size = .5) {

    ## Create igraph object to pass on as data for 3D graph (because original epi_contacts object
    ## may contain NA's, which will hinder creation of 3D graph with threejs::graphjs()
    x <- as.igraph.epi_contacts(epi_contacts)

    ## Get vertex attributes and prepare as input for graph
    v <- igraph::get.vertex.attribute(x)
    v <- as.data.frame(v, stringsAsFactors = FALSE)

    v$label <- v$id ## assigning the unique identifier as default label
    v$id = 1:nrow(v) ## required by threejs::graphjs(). It has to be an integer.

    # Set node attributes
    if(v_col_by == "NA" & v.col == "darkturquoise") {
        v$color = "darkturquoise"
    } else {
        if(v_col_by == "NA" & v.col != "darkturquoise") {
            v$color = v.col
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
    e$size = e.size
    e$color = "lightgrey"

    ## Set vertex attributes
    v$size = v.size

    ## Subset vertex dataframe for graphjs
    v <- v[ , c("label", "id", "size", "color")]

    ## Create 3D graph
    g <- threejs::graphjs(edges = e, nodes = v, main = g.title, showLabels=FALSE, fg = g.fg, bg = g.bg)
    return(g)
}

