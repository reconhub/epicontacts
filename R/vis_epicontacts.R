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
#' Zhian N. Kamvar (\email{zkamvar@@gmail.com})
#' Finlay Campbell (\email{finlaycampbell93@@gmail.com})
#'
#' @param x An \code{\link{epicontacts}} object.
#'
#' @param node_color An index or character string indicating which field of the
#'   linelist should be used to color the nodes. If node color = 'R_i', the
#'   individual reproductive number for each case (i.e. number of outgoing
#'   infection/contacts) will be calculated and used to specify the node colour.
#'
#' @param node_shape An index or character string indicating which field of the
#'   linelist should be used to determine the shapes of the nodes. If node color
#'   = 'R_i', the individual reproductive number for each case (i.e. number of
#'   outgoing infection/contacts) will be calculated and used to specify the
#'   node shape.
#'
#' @param node_size An integer indicating the size of the nodes, or a
#'   character string indicating which field of the linelist should be used
#'   to determine the size of the node. Defaults to 3. If node color = 'R_i', the
#'   individual reproductive number for each case (i.e. number of outgoing
#'   infection/contacts) will be calculated and used to specify the node size.
#'
#' @param edge_color An index or character string indicating which field of the
#'   contacts data should be used to color the edges of the graph.
#'
#' @param edge_width An integer indicating the width of the edges, or a
#'   character string indicating which field of the contacts data should be used
#'   to determine the width of the edge.
#'
#' @param edge_linetype An integer or character string indicating which field of
#'   the contacts data should be used to indicate the edge linetype. If the
#'   output format is visNetwork, this field of the contacts data must contain a
#'   binary variable, as visNetwork only supports dashed/non-dashed edges.
#'
#' @param edge_label An index or character string indicating which field of the
#'   contacts data should be used to label the edges of the graph.
#'
#' @param col_pal A color palette for the nodes. Can be a function accepting a
#'   single number n and returning a vector of n colors, or a named character
#'   vector/list matching all factor levels in node_color to a color.
#'
#' @param edge_col_pal A color palette for the edges. Can be a function accepting a
#'   single number n and returning a vector of n colors, or a named character
#'   vector/list matching all factor levels in edge_color to a color.
#'
#' @param NA_col The color used for unknown group.
#'
#' @param shapes A named vector of characters indicating which icon code should
#'   be used for each value \code{node_shape}, e.g. \code{c(m = "male", f =
#'   "female")} if 'm' amd 'f' are values from \code{node_shape}. See
#'   \code{\link{codeawesome}} for all available codes.
#'
#' @param size_range A numeric vector of length 2, indicating the minimum and
#'   maximum node size.
#'
#' @param width_range A numeric vector of length 2, indicating the minimum and
#'   maximum edge width.
#'
#' @param label An index, logical, or character string indicating which fields
#'   of the linelist should be used for labelling the nodes. Logical will be
#'   recycled if necessary, so that the default \code{TRUE} effectively uses all
#'   columns of the linelist.
#'
#' @param annot An index, logical, or character string indicating which fields
#'   of the linelist should be used for annotating the nodes. Logical will be
#'   recycled if necessary, so that the default \code{TRUE} effectively uses all
#'   columns of the linelist.
#'
#' @param width The width of the output, in html compatible format (e.g. '90\%'
#'   or '800px').
#'
#' @param height The height of the output, in html compatible format
#'   (e.g. '800px').
#'
#' @param title An optional character string indicating the plot title.
#'
#' @param legend A logical indicating whether a legend should be added to the
#'   plot.
#'
#' @param legend_max The maximum number of groups for a legend to be displayed.
#'
#' @param legend_width The width of the legend as a proportion of the total
#'   figure width.
#'
#' @param legend_ncol The number of columns in the legend.
#'
#' @param legend_text_color The colour of the legend text.
#'
#' @param date_labels A string giving the formatting specification for the
#' x-axis date labels. Codes are defined in ‘strftime()’.
#'
#' @param thin A logical indicating if the data should be thinned with
#'   \code{\link{thin}} so that only cases with contacts should be plotted.
#'
#' @param selector A string indicating which column should be used for the
#'   selector tool. Set to FALSE to disable.
#'
#' @param editor A logical indicating if the editor tool should be used;
#'   defaults to FALSE.
#'
#' @param highlight_downstream A logical indicating if all cases 'downstream' of
#'   the the selected node should be highlighted.
#'
#' @param collapse A logical indicating if the network should be collapsed at a
#'   given node upon double-clicking.
#'
#' @param font_size The font size of the node and edge labels.
#'
#' @param arrow_size The size of the arrow.
#'
#' @param ... Further arguments to be passed to \code{visNetwork}.
#'
#' @return The same output as \code{visNetwork}.
#'
#' @seealso \code{\link[visNetwork]{visNetwork}} in the package \code{visNetwork}.
#'   \code{\link{edges_pal}} and \code{\link{cases_pal}} for color palettes used
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
vis_epicontacts <- function(x,
                            node_color = "id",
                            node_shape = NULL,
                            node_size = 20,
                            edge_color = NULL,
                            edge_width = 3,
                            edge_linetype = NULL,
                            edge_label = NULL,
                            col_pal = cases_pal,
                            edge_col_pal = edges_pal,
                            NA_col = "lightgrey",
                            shapes = NULL,
                            size_range = c(5, 20),
                            width_range = c(1, 5),
                            length_range = c(1, 5),
                            label = "id",
                            annot  =  TRUE,
                            width = "90%",
                            height = "700px",
                            title = NULL,
                            legend = TRUE,
                            legend_max = 10,
                            legend_width = 0.1,
                            legend_ncol = 1,
                            legend_text_color = "black",
                            date_labels = "%Y-%m-%d",
                            thin = TRUE,
                            selector = node_color,
                            editor = FALSE,
                            highlight_downstream = FALSE,
                            collapse = TRUE,
                            font_size = NULL,
                            arrow_size = 2,
                            ...){

  ## In the following, we pull the list of all plotted nodes (those from the
  ## linelist, and from the contacts data.frame, and then derive node attributes
  ## for the whole lot. These attributes are in turn used for plotting: as color
  ## ('group' in visNetwork terminology) or as annotations (converted to html
  ## code).

  ## Remove NAs in contacts
  x <- x[j = !is.na(x$contacts$from) & !is.na(x$contacts$to)]

  ## thin
  if (thin) {
    x <- thin(x)
  }

  ## check node_color (node attribute used for color)
  node_color <- assert_node_color(x$linelist, node_color, "node_color")

  ## check node_shape (node attribute used for color)
  node_shape <- assert_node_shape(x$linelist, node_shape, "node_shape", shapes)

  ## check node_size (node attribute used for color)
  node_size <- assert_node_size(x$linelist, node_size, "node_shape")

  ## check annot (txt displayed when clicking on node)
  annot <- assert_annot(x, annot)

  ## check edge_label (edge attribute used for label)
  edge_label <- assert_edge_label(x$contacts, edge_label, "edge_label")

  ## check edge_color (edge attribute used for color)
  edge_color <- assert_edge_color(x$contacts, edge_color, "edge_color")

  ## check edge_width (edge attribute used for width)
  edge_width <- assert_edge_width(x$contacts, edge_width, "edge_width")

  ## check edge_linetype (edge attribute used for linetype)
  edge_linetype <- assert_edge_linetype(x$contacts, edge_linetype, "edge_linetype")

  ## make a list of all nodes, and generate a data.frame of node attributes
  all_nodes <- get_id(x, which = "all")

  ## find out which nodes are unconnected to any other nodes
  ## This is only relevant when `thin = TRUE`
  nodes <- data.frame(id = all_nodes,
                      stringsAsFactors = FALSE)

  ## Calculate R_i if needed
  if('R_i' %in% c(node_shape, node_color, node_size)) {
    x$linelist$R_i <- sapply(x$linelist$id,
                             function(i) sum(x$contacts$from == i, na.rm = TRUE))
  }

  ## make a list of all nodes, and generate a data.frame of node attributes
  cont_nodes <- c(x$contacts$from, x$contacts$to)
  list_nodes <- x$linelist$id

  nodes <- data.frame(id = get_id(x, "all"), stringsAsFactors = FALSE)

  nodes <- merge(nodes, x$linelist, by = "id", all = TRUE, sort = FALSE)

  edges <- x$contacts

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

    if(!is.null(node_shape)) {
      nodes$icon.color <- node_col_info$color
    } else {
      nodes$color.background <- nodes$color.highlight.background <- node_col_info$color
      nodes$color.border <- nodes$color.highlight.border <- 'black'
    }
  } else {
    ## if node_color set to NULL (not default) color nodes black
    nodes$color.background <- nodes$color.highlight.background <- 'black'
    nodes$color.border <- nodes$color.highlight.border <- 'black'
  }

  ## add node size
  if(!is.null(node_size)) {
    ## if numeric, use node_size for all nodes
    if(is.numeric(node_size)) {
      nodes$size <- node_size
    } else {
      node_size_values <- nodes[[node_size]]
      if(is.character(node_size_values)) {
        stop("node_size cannot be mapped to character variable")
      }
      nodes$size <- rescale(as.numeric(node_size_values),
                            size_range[1],
                            size_range[2])
    }
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

    ## define icon size if node_size is specified
    if(!is.null(node_size)) nodes$icon.size <- nodes$size
    node_shape_info <- data.frame(icon = unique(node_code),
                                  leg_lab = unique(vec_node_shapes))
    node_shape_info <- node_shape_info[node_shape_info$leg_lab != "NA",]
  }
  nodes$borderWidth <- 2

  ## add edge width
  if(!is.null(edge_width)) {
    if(is.numeric(edge_width)) {
      edges$width <- edge_width
    } else {
      edge_width_values <- edges[[edge_width]]
      if(is.character(edge_width_values)) {
        stop("edge_width cannot be mapped to character variable")
      }
      edges$width <- rescale(as.numeric(edge_width_values),
                             width_range[1],
                             width_range[2])
    }
  }

  ## add edge labels
  if (!is.null(edge_label)) {
    edges$label <- as.character(edges[, edge_label])
  }

  ## add edge color
  if (!is.null(edge_color)) {
    edge_col_info <- fac2col(factor(edges[, edge_color]),
                             edge_col_pal,
                             NA_col,
                             legend = TRUE)
    L <- length(edge_col_info$leg_lab)
    edges$color <- edge_col_info$color
  } else {
    edges$color <- 'black'
  }

  ## add edge linetype
  if(!is.null(edge_linetype)) {
    unq_linetype <- unique(edges[[edge_linetype]])
    if(length(stats::na.omit(unq_linetype)) > 2) {
      msg <- paste0("visNetwork only supports two linetypes; ",
                    "use binary variable or set method = 'ggplot'.")
      stop(msg)
    }
    ## use alphabetical order / factor order
    edges$dashes <- edges[[edge_linetype]] != sort(unq_linetype)[1]
  }

  ## Change font size
  if(!is.null(font_size)) {
    edges$font.size <- font_size
    nodes$font.size <- font_size
  }

  ## convert height "xx%" to "xxvh" due to visNetwork bug
  if(is.character(height)) height <- gsub("%", "vh", height)

  ## build visNetwork output
  out <- visNetwork::visNetwork(nodes, edges,
                                width = width,
                                height = height, ...)

  ## specify tool tip
  tt_style <- paste0("position: fixed;visibility:hidden;",
                     "padding: 5px;white-space: nowrap;",
                     "font-family: verdana;font-size:14px;",
                     "font-color:#000000;background-color: #EEEEEE;",
                     "-moz-border-radius: 3px;-webkit-border-radius: 3px;",
                     "border-radius: 3px;border: 1px solid #000000;")
  out <- visNetwork::visInteraction(out, tooltipStyle = tt_style)

  ## add arrows if directed
  if (x$directed) {
    out <- visNetwork::visEdges(out,
                                arrows = list(to = list(scaleFactor = arrow_size)))
  }

  ## add legend
  if (legend) {

    ## node color legend
    if (!is.null(node_color) &&  (K < legend_max)) {
      leg_nodes <- data.frame(label = node_col_info$leg_lab,
                              color.background = node_col_info$leg_col,
                              color.border = "black",
                              shape = "dot",
                              borderWidth = 2,
                              shadow = FALSE,
                              font.size = ifelse(is.null(font_size),
                                                 14, font_size))
    } else {
      leg_nodes <- NULL
    }

    ## node shape legend
    if (!is.null(node_shape) && nrow(node_shape_info) < legend_max) {
      ## don't add extra legend keys if variable is the same as node_color
      if(node_shape == node_color){
        leg_nodes$shape <- "icon"
        leg_nodes$icon.code <- node_shape_info$icon
        leg_nodes[c("color.border",
                    "color.highlight.border",
                    "borderWidth")]<- NULL
        names(leg_nodes)[names(leg_nodes) == "color.background"] <- "icon.color"
      } else {
        tmp <- data.frame(label = node_shape_info$leg_lab,
                          icon.color = 'black',
                          shape = "icon",
                          icon.code = node_shape_info$icon,
                          shadow = FALSE,
                          font.size = ifelse(is.null(font_size),
                                             14, font_size))
        leg_nodes$shape <- "icon"
        leg_nodes$icon.code <- codeawesome["circle"]
        leg_nodes[c("color.border",
                    "color.highlight.border",
                    "borderWidth")]<- NULL
        names(leg_nodes)[names(leg_nodes) == "color.background"] <- "icon.color"
        leg_nodes <- rbind(leg_nodes, tmp)
      }
    }

    ## edge color legend
    if (!is.null(edge_color) &&  (L < legend_max)) {
      leg_edges <- data.frame(label = edge_col_info$leg_lab,
                              color = edge_col_info$leg_col,
                              dashes = FALSE,
                              font.size = ifelse(is.null(font_size),
                                                 14, font_size),
                              font.align = 'bottom')
    } else {
      leg_edges <- NULL
    }

    ## edge linetype legend
    if (!is.null(edge_linetype)) {
      ## Don't add extra legend keys if variable is the same
      if(!is.null(edge_color) && edge_linetype == edge_color) {
        leg_edges$dashes <- c(FALSE, TRUE)
      } else {
        tmp <- data.frame(label = unq_linetype,
                          color = 'black',
                          dashes = c(FALSE, TRUE),
                          font.size = ifelse(is.null(font_size),
                                             14, font_size),
                          font.align = 'bottom')
        leg_edges <- rbind(leg_edges, tmp)
      }
    }

    leg_nodes$font.color <- legend_text_color
    leg_edges$font.color <- legend_text_color

    out <- visNetwork::visLegend(out,
                                 zoom = FALSE,
                                 addNodes = leg_nodes,
                                 addEdges = leg_edges,
                                 width = legend_width,
                                 ncol = legend_ncol,
                                 useGroups = FALSE)

  }

  ## set nodes borders, edge width, and plotting options
  enabled <- list(enabled = TRUE)

  ## set selector
  if(is.logical(selector)) {
    arg_selec <- if(selector) "id" else NULL
  } else {
    arg_selec <- selector
  }

  ## should nodes collapse upon double clicking
  if(collapse) {
    collapse <- list(enabled = TRUE, keepCoord = TRUE)
  }

  ## options specific to highlight_downstream
  if(highlight_downstream) {
    out <- visNetwork::visOptions(
                         out,
                         highlightNearest = list(enabled = TRUE,
                                                 algorithm = "hierarchical",
                                                 degree = list(from = 0, to = 50),
                                                 hideColor = 'rgba(200,200,200,1)'),
                         nodesIdSelection = FALSE,
                         selectedBy = arg_selec,
                         manipulation = editor,
                         collapse = collapse
                       )
  } else {
    out <- visNetwork::visOptions(
                         out,
                         highlightNearest = list(enabled = TRUE,
                                                 hideColor = 'rgba(200,200,200,1)'),
                         selectedBy = arg_selec,
                         manipulation = editor,
                         collapse = collapse
                       )
  }

  ## disable stabilization
  out <- visNetwork::visPhysics(out, stabilization = FALSE)

  ## add fontAwesome
  out <- visNetwork::addFontAwesome(out)

  ## add plot title
  if(!is.null(title)) {
    out$x$main$text <- title
    out$x$main$style <- "font-weight:bold;font-size:20px;text-align:center"
  }

  return(out)

}
