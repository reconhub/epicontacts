#' Plot epicontacts objects using visNetwork
#'
#' This function plots \code{\link{epicontacts}} objects using the
#' \code{visNetwork} package. The produced object is an \code{htmlwidget} which
#' will need rendering within a web browser.
#'
#' @export
#'
#'
#' @author
#' Thibaut Jombart (\email{thibautjombart@@gmail.com})
#' VP Nagraj (\email{vpnagraj@@virginia.edu})
#'
#' @param x An \code{\link{epicontacts}} object.
#' 
#' @param thin A logical indicating if the data should be thinned with \code{\link{thin}} so that only cases with contacts should be plotted.
#'
#' @param node_color An index or character string indicating which field of the
#'     linelist should be used to color the nodes.
#'
#' @param annot An index, logical, or character string indicating which fields
#'   of the linelist should be used for annotating the nodes. Logical will be
#'   recycled if necessary, so that the default \code{TRUE} effectively uses all
#'   columns of the linelist.
#'
#' @param node_shape An index or character string indicating which field of the
#'   linelist should be used to determine the shapes of the nodes.
#'
#' @param shapes A named vector of characters indicating which icon code should
#'   be used for each value \code{node_shape}, e.g. \code{c(m = "male", f =
#'   "female")} if 'm' amd 'f' are values from \code{node_shape}. See
#'   \code{\link{codeawesome}} for all available codes.
#'
#' @param label An index, logical, or character string indicating which fields
#'   of the linelist should be used for labelling the nodes. Logical will be
#'   recycled if necessary, so that the default \code{TRUE} effectively uses all
#'   columns of the linelist.
#'
#' @param edge_label An index or character string indicating which field of the
#'   contacts data should be used to label the edges of the graph.
#'
#' @param edge_color An index or character string indicating which field of the
#'   contacts data should be used to color the edges of the graph.
#'
#' @param edge_width An integer indicating the width of the edges. Defaults to
#'   3.
#'
#' @param legend A logical indicating whether a legend should be added to the
#'   plot.
#' 
#' @param x_axis A character string indicating which field of the linelist data
#'   should be used to specify the x axis position (must be numeric or Date)
#'
#' @param legend_max The maximum number of groups for a legend to be displayed.
#'
#' @param col_pal A color palette for the nodes.
#'
#' @param edge_col_pal A color palette for the edges.
#'
#' @param NA_col The color used for unknown group.
#'
#' @param width The width of the output, in html compatible format (e.g. '90\%'
#'   or '800px').
#'
#' @param height The height of the output, in html compatible format
#'   (e.g. '800px').
#'
#' @param selector A logical indicating if the selector tool should be used;
#'   defaults to TRUE.
#'
#' @param editor A logical indicating if the editor tool should be used;
#'   defaults to FALSE.
#'
#' @param ... Further arguments to be passed to \code{visNetwork}.
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#'
#' @return The same output as \code{visNetwork}.
#'
#' @seealso \code{\link[visNetwork]{visNetwork}} in the package \code{visNetwork}.
#'
#' @examples
#' if (require(outbreaks)) {
#'
#' ## example using MERS outbreak in Korea, 2014
#' head(mers_korea_2015[[1]])
#' head(mers_korea_2015[[2]])
#'
#' x <- make_epicontacts(linelist=mers_korea_2015[[1]],
#'                        contacts = mers_korea_2015[[2]],
#'                        directed=TRUE)
#'
#' \dontrun{
#' plot(x)
#' plot(x, node_color = "place_infect")
#' plot(x, node_color = "loc_hosp", legend_max=20, annot=TRUE)
#' plot(x, "place_infect", node_shape = "sex",
#'      shapes = c(M = "male", F = "female"))
#'
#' plot(x, "sex", node_shape = "sex", shapes = c(F = "female", M = "male"),
#'      edge_label = "exposure", edge_color = "exposure")
#' }
#' }
vis_epicontacts <- function(x, thin = TRUE, node_color = "id", label = "id",
                            annot  =  TRUE, node_shape = NULL, shapes = NULL,
                            edge_label = NULL, edge_color = NULL, legend = TRUE,
                            legend_max = 10, x_axis = NULL, col_pal = cases_pal,
                            NA_col = "lightgrey", edge_col_pal = edges_pal,
                            width = "90%", height = "700px", selector = TRUE,
                            editor = FALSE, edge_width = 3, ...){

  ## In the following, we pull the list of all plotted nodes (those from the
  ## linelist, and from the contacts data.frame, and then derive node attributes
  ## for the whole lot. These attributes are in turn used for plotting: as color
  ## ('group' in visNetwork terminology) or as annotations (converted to html
  ## code).

  ## handling
  
  if (thin) {
    x <- thin(x)
  }
  
  ## check node_color (node attribute used for color)
  node_color <- assert_node_color(x, node_color)

  ## check node_shape (node attribute used for color)
  node_shape <- assert_node_shape(x, node_shape)

  ## check annot (txt displayed when clicking on node)
  annot <- assert_annot(x, annot)

  ## check node_color (node attribute used for color)
  edge_label <- assert_edge_label(x, edge_label)

  ## check node_color (node attribute used for color)
  edge_color <- assert_edge_color(x, edge_color)


  ## make a list of all nodes, and generate a data.frame of node attributes
  nodes <- data.frame(id = unique(c(x$linelist$id,
                                    x$contacts$from,
                                    x$contacts$to)),
                      stringsAsFactors = FALSE)

  nodes <- merge(nodes, x$linelist, by = "id", all = TRUE)


  ## generate annotations ('title' in visNetwork terms)

  if (!is.null(label)) {
    labels <- apply(nodes[, label, drop = FALSE], 1,
                    paste, collapse = "\n")
    nodes$label <- labels
  }


  ## generate annotations ('title' in visNetwork terms)

  if (!is.null(annot)) {
    temp <- nodes[, annot, drop = FALSE]
    temp <- vapply(names(temp),
                   function(e) paste(e, temp[, e], sep = ": "),
                   character(nrow(nodes)))
    nodes$title <- paste("<p>",
                         apply(temp, 1, paste0, collapse = "<br>"), "</p>")
  }


  ## add node color ('group')

  if (!is.null(node_color)) {
    node_col_info <- fac2col(factor(nodes[, node_color]),
                             col_pal,
                             NA_col,
                             legend = TRUE)
    K <- length(node_col_info$leg_lab)
    nodes$group.color <- nodes$icon.color <- node_col_info$color
  }


  ## add shape info

  if (!is.null(node_shape)) {
    if (is.null(shapes)) {
      msg <- paste("'shapes' needed if 'node_shape' provided;",
                   "to see codes, node_shape: codeawesome")
      stop(msg)
    }
    vec_node_shapes <- as.character(unlist(nodes[node_shape]))
    shapes["NA"] <- "question-circle"
    unknown_codes <- !shapes %in% names(codeawesome)
    if (any(unknown_codes)) {
      culprits <- paste(shapes[unknown_codes],
                        collapse = ", ")
      msg <- sprintf("unknown icon codes: %s \nto see 'codeawesome'",
                     culprits)
      stop(msg)
    }

    vec_node_shapes <- paste(vec_node_shapes)
    node_code <- codeawesome[shapes[vec_node_shapes]]
    nodes$shape <- "icon"
    nodes$icon.code <- node_code
  } else {
    nodes$borderWidth <- 2
  }
  
  ## add edge info
  edges <- x$contacts
  edges$width <- edge_width
  if (x$directed) {
    edges$arrows <- "to"
  }

  if (!is.null(edge_label)) {
    edges$label <- edges[, edge_label]
  }


  if (!is.null(edge_color)) {
    edge_col_info <- fac2col(factor(edges[, edge_color]),
                             edge_col_pal,
                             NA_col,
                             legend = TRUE)
    L <- length(edge_col_info$leg_lab)
    edges$color <- edge_col_info$color
  }

  if(!is.null(x_axis)) {
    if(!inherits(x$linelist[[x_axis]], c("numeric", "Date", "integer"))) {
      stop("Data used to specify x axis must be a date or number")
    }
    nodes$level <- nodes[[x_axis]] - min(nodes[[x_axis]], na.rm = TRUE) + 1L
  } 

  ## build visNetwork output
  out <- visNetwork::visNetwork(nodes, edges,
                                width = width,
                                height = height, ...)
  ## specify group colors, add legend

  if (legend) {
    if (!is.null(node_color) &&  (K < legend_max)) {
      leg_nodes <- data.frame(label = node_col_info$leg_lab,
                              color = node_col_info$leg_col,
                              shape = "box",
                              shadow = TRUE,
                              font.size = 20)
    } else {
      leg_nodes <- NULL
    }

    if (!is.null(edge_color) &&  (L < legend_max)) {
      leg_edges <- data.frame(label = edge_col_info$leg_lab,
                              color = edge_col_info$leg_col,
                              font.size = 15)
    } else {
      leg_edges <- NULL
    }

    out <- out %>% visNetwork::visLegend(addNodes = leg_nodes,
                                         addEdges = leg_edges,
                                         useGroups = FALSE)

  }


  if (!is.null(x_axis)){
    out <- visNetwork::visHierarchicalLayout(out, 
					     direction = 'LR',
					     sortMethod = 'directed')
  }
  ## set nodes borders, edge width, and plotting options

  enabled <- list(enabled = TRUE)
  arg_selec <- if (selector) node_color else NULL

  out <- out %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visOptions(selectedBy = arg_selec,
                           manipulation = editor,
                           highlightNearest = enabled) %>%
    visNetwork::visPhysics(stabilization = FALSE)
  
  # add fontAwesome
  out <-
    out %>%
    visNetwork::addFontAwesome()
  
  return(out)
}
