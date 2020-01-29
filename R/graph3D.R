#' Interactive 3D Force-directed graph from epicontacts object
#'
#' This function creates a 3D graph from an epicontacts object
#'
#' @export
#'
#' @author
#' Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#' Thibaut Jombart (\email{thibautjombart@@gmail.com})
#' VP Nagraj (\email{vpnagraj@@virginia.edu})
#'
#' @param x An \code{\link{epicontacts}} object
#'
#' @param node_color An index or character string indicating which field of the
#'     linelist should be used to color the nodes. Default is \code{id}
#'
#' @param annot An index, logical, or character string indicating which fields
#'   of the linelist should be used for annotating the nodes upon mouseover. The
#'   default \code{TRUE} shows the 'id' and 'node_color' (if the grouping column
#'   is different from 'id').
#'
#' @param col_pal A color palette for the node_colors.
#'
#' @param NA_col The color used for unknown node_color.
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
#' @param thin A logical indicating if the data should be thinned with
#'   \code{\link{thin}} so that only cases with contacts should be plotted.
#'
#' @note All colors must be specified as color names like "red", "blue", etc. or
#'   as hexadecimal color values without opacity channel, for example "#FF0000",
#'   "#0a3e55" (upper or lower case hex digits are allowed).
#'
#' Double-click or tap on the plot to reset the view.
#'
#' @return An htmlwidget object that is displayed using the object's show or
#'   print method.  (If you don't see your widget plot, try printing it with the
#'   \code{print} function.)
#'
#' @references
#'
#' Original rthreejs code by B. W. Lewis:
#' \url{https://github.com/bwlewis/rthreejs}.
#'
#' @examples
#' if (require(outbreaks)) {
#'
#' ## example using MERS outbreak in Korea, 2014
#' head(mers_korea_2015[[1]])
#' head(mers_korea_2015[[2]])
#'
#' x <- make_epicontacts(linelist = mers_korea_2015$linelist,
#'                       contacts = mers_korea_2015$contacts,
#'                       directed = FALSE)
#'
#' \dontrun{
#' graph3D(x)
#' graph3D(x, annot = FALSE)
#' graph3D(x, node_color = "sex", g_title = "MERS Korea 2014")
#' graph3D(x, node_color = "sex", annot = c("sex", "age"),
#'         g_title = "MERS Korea 2014")
#' }
#' }

graph3D <- function(x,
                    node_color = "id",
                    annot = TRUE,
                    col_pal = cases_pal,
                    NA_col = "lightgrey",
                    g_title = "",
                    bg_col = "white",
                    label_col = "darkgrey",
                    node_size = 1,
                    edge_size = .5,
                    thin = TRUE) {

  ## check node_color (node attribute used for color)
  node_color <- assert_node_color(x, node_color)
  
  ## thin
  if (thin) {
    x <- thin(x)
  }

  ## check annot (txt displayed when clicking on node)
  if (is.logical(annot) && sum(annot) == 0L) {
    annot <- NULL
  }
  if (!is.null(annot)) {
    if (is.numeric(annot)) {
      annot <- names(x$linelist)[annot]
    } else {
      if (is.logical(annot)) {
        annot = unique(c("id", node_color))
      }
    }

    if (!all(annot %in% names(x$linelist))) {
      culprits <- annot[!annot %in% names(x$linelist)]
      culprits <- paste(culprits, collapse = ", ")
      msg <- sprintf("Annot '%s' is not in the linelist", culprits)
      stop(msg)
    }
  }

  ## Subset those ids which have at least one edge with another id
  ##    (to mimic visNetwork plot, else loner nodes are also printed)
  x <- subset_clusters_by_size(x, cs_min = 2)
  g <- as.igraph.epicontacts(x, TRUE)

  ## Get vertex attributes and prepare as input for graph
  nodes <- data.frame(id = unique(c(x$linelist$id,
                                    x$contacts$from,
                                    x$contacts$to)),
                      stringsAsFactors = FALSE)

  ## join back to linelist to retrieve attributes for grouping
  nodes <- suppressMessages(
    suppressWarnings(merge(nodes, x$linelist, all.x = TRUE, sort = FALSE))
  )

  ## generate annotations ('label' in threejs terms)
  if (!is.null(annot)) {
    temp <- nodes[, annot, drop = FALSE]
    temp <- sapply(names(temp), function(e) paste(e, temp[, e], sep = ": "))
    nodes$label <- paste(g_title, "<p>",
                         apply(temp, 1, paste0, collapse = "<br>"), "</p>")
  } else nodes$label <- ""

  ## attribute for grouping
  if (!is.null(node_color)) {
    nodes$group <- as.character(nodes[, node_color])
    nodes$group[is.na(nodes$group)] <- "NA"
    nodes$group <- factor(nodes$group)
  }

  ## Set node attributes
  ## node color
  if (!is.null(node_color)) {
    K <- length(unique(nodes$group))
    grp.col <- col_pal(K)
    grp.col[levels(nodes$group)=="NA"] <- NA_col
    nodes$color <- grp.col[factor(nodes$group)]
  } else {
    ## setting to match default visNetwork color
    nodes$color <- "#97C2FC"
  }

  igraph::V(g)$color <- nodes$color
  igraph::V(g)$label <- nodes$label

  ## Set edge attributes
  igraph::E(g)$size <- edge_size
  igraph::E(g)$color <- "lightgray"

  ## Create 3D graph (note fg not supported, but may be in the future)
  threejs::graphjs(g, main = g_title, fg = label_col, bg = bg_col,
                   vertex.color = nodes$color, vertex.size = node_size)
  
}
