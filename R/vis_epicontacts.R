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
#' Finlay Campbell (\email{f.campbell15@imperial.ac.uk})
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
#'   to determine the width of the edge. Defaults to 3.
#'
#' @param edge_linetype An integer or character string indicating which field of
#'   the contacts data should be used to indicate the edge linetype. If the
#'   output format is visNetwork, this field of the contacts data must contain a
#'   binary variable, as visNetwork only supports dashed/non-dashed edges.
#' 
#' @param edge_label An index or character string indicating which field of the
#'   contacts data should be used to label the edges of the graph.
#'
#' @param col_pal A color palette for the nodes. 
#' 
#' @param edge_col_pal A color palette for the edges.
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
#' @param x_axis A character string indicating which field of the linelist data
#'   should be used to specify the x axis position (must be numeric or Date).
#'
#' @param date_labels A string giving the formatting specification for the
#' x-axis date labels. Codes are defined in ‘strftime()’.
#' 
#' @param thin A logical indicating if the data should be thinned with
#'   \code{\link{thin}} so that only cases with contacts should be plotted.
#'
#' @param selector A logical indicating if the selector tool should be used;
#'   defaults to TRUE.
#'
#' @param editor A logical indicating if the editor tool should be used;
#'   defaults to FALSE.
#'
#' @param highlight_downstream A logical indicating if all cases 'downstream' of
#'   the the selected node should be highlighted. 
#'
#' @param hide_labels A logical indicating if case labels should be hidden.
#'
#' @param collapse A logical indicating if the network should be collapsed at a
#'   given node upon double-clicking.
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
#' # show transmission tree with time as the horizontal axis, showing all nodes
#' vis_epicontacts(x, x_axis = "dt_onset", thin = FALSE) 
#' plot(x, node_color = "loc_hosp", legend_max=20, annot=TRUE)
#' plot(x, node_color = "loc_hosp", legend_max=20, annot=TRUE, x_axis = "dt_onset")
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
                            label = "id",
                            annot  =  TRUE,
                            width = "90%",
                            height = "700px",
                            title = NULL,
                            legend = TRUE,
                            legend_max = 10,
                            x_axis = NULL,
                            date_labels = "%d/%m/%Y",
                            thin = TRUE,
                            selector = TRUE,
                            editor = FALSE,
                            highlight_downstream = FALSE,
                            hide_labels = FALSE,
                            collapse = TRUE,
                            ...){
  
  ## In the following, we pull the list of all plotted nodes (those from the
  ## linelist, and from the contacts data.frame, and then derive node attributes
  ## for the whole lot. These attributes are in turn used for plotting: as color
  ## ('group' in visNetwork terminology) or as annotations (converted to html
  ## code).

  ## Remove NAs in contacts
  x$contacts <- subset(x$contacts, !is.na(x$contacts$from) & !is.na(x$contacts$to))
  
  ## check node_color (node attribute used for color)
  node_color <- assert_node_color(x, node_color)

  ## check node_shape (node attribute used for color)
  node_shape <- assert_node_shape(x, node_shape)

  ## check node_size (node attribute used for color)
  node_size <- assert_node_size(x, node_size)

  ## check annot (txt displayed when clicking on node)
  annot <- assert_annot(x, annot)

  ## check edge_label (edge attribute used for label)
  edge_label <- assert_edge_label(x, edge_label)

  ## check edge_color (edge attribute used for color)
  edge_color <- assert_edge_color(x, edge_color)

  ## check edge_width (edge attribute used for width)
  edge_width <- assert_edge_width(x, edge_width)

  ## check edge_linetype (edge attribute used for linetype)
  edge_linetype <- assert_edge_linetype(x, edge_linetype)

  ## make a list of all nodes, and generate a data.frame of node attributes
  all_nodes <- get_id(x, which = "all", na.rm = TRUE)

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

  nodes <- data.frame(id = unique(c(list_nodes, cont_nodes)),
                      stringsAsFactors = FALSE)

  nodes <- merge(nodes, x$linelist, by = "id", all = TRUE)

  edges <- x$contacts
  
  ## generate annotations ('title' in visNetwork terms)
  if (!is.null(label)) {
    if(hide_labels) {
      nodes$label <- ""
    } else {
      labels <- apply(nodes[, label, drop = FALSE], 1,
                      paste, collapse = "\n")
      nodes$label <- labels
    }
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
                             legend = TRUE,
                             adj_width = TRUE)
    K <- length(node_col_info$leg_lab)
    if(!is.null(node_shape)) {
      nodes$icon.color <- node_col_info$color
    } else if(!is.null(x_axis)) {
      nodes$group.color <- nodes$icon.color <- node_col_info$color
    } else {
      nodes$color.background <- nodes$color.highlight.background <- node_col_info$color
      nodes$color.border <- nodes$color.highlight.border <- 'black'
    }
  } else {
    ## if node_color set to NULL (not default) color nodes black
    nodes$group.color <- nodes$icon.color <- nodes$color.border <- 'black'
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
      nodes$size <- rescale(as.numeric(node_size_values), size_range[1], size_range[2])
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
  } else {
    nodes$borderWidth <- 2
  }

  ## add edge width
  if(!is.null(edge_width)) {
    if(is.numeric(edge_width)) {
      edges$width <- edge_width
    } else {
      edge_width_values <- edges[[edge_width]]
      if(is.character(edge_width_values)) {
        stop("edge_width cannot be mapped to character variable")
      }
      edges$width <- rescale(as.numeric(edge_width_values), width_range[1], width_range[2])
    }
  }
  
  if (x$directed) {
    edges$arrows <- "to"
  }


  ## add edge labels
  if (!is.null(edge_label)) {
    edges$label <- edges[, edge_label]
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

  ## ## add edge linetype
  ## if(!is.null(edge_linetype)) {
  ##   unq <- unique(edges[[edge_linetype]])
  ##   if(length(stats::na.omit(unq)) > 2) {
  ##     stop("visNetwork only supports two linetypes; use binary variable or set method = 'ggplot'.")
  ##   }
  ##   ## Uses alphabetical order / factor order
  ##   edges$dashes <- edges[[edge_linetype]] != sort(unq)[1]
  ## }

  ## add edge linetype
  if(!is.null(edge_linetype)) {
    unq_linetype <- unique(edges[[edge_linetype]])
    if(length(stats::na.omit(unq_linetype)) > 2) {
      stop("visNetwork only supports two linetypes; use binary variable or set method = 'ggplot'.")
    }
    ## Uses alphabetical order / factor order
    edges$dashes <- edges[[edge_linetype]] != sort(unq_linetype)[1]
  }



  ## set up nodes and edges if x_axis is specified
  if (!is.null(x_axis)) {
    if (!inherits(nodes[[x_axis]], c("numeric", "Date", "integer"))) {
      stop("Data used to specify x axis must be a date or number")
    }
    drange      <- range(nodes[[x_axis]], na.rm = TRUE)
    nodes$level <- nodes[[x_axis]] - drange[1] + 1L
    drange      <- seq(drange[1], drange[2], by = 1L)

    if(inherits(drange, "Date")) {
      drange_id <- format(drange, date_labels)
    } else {
      drange_id <- drange
    }
    
    dnodes      <- data.frame(
      id = as.character(drange_id),
      level = drange - drange[1] + 1L,
      stringsAsFactors = FALSE
    )
    dedges      <- data.frame(
      from = dnodes$id[-nrow(dnodes)],
      to   = dnodes$id[-1]
    )
    nmerge      <- c("id", "level")
    emerge      <- c("from", "to")
    if (!is.null(label)) {
      dnodes$label <- dnodes$id
      nmerge <- c(nmerge, "label")
    }
    if (!is.null(node_shape)) {
      dnodes$shape     <- "icon"
      dnodes$icon.code <- codeawesome["clock-o"]
      nmerge <- c(nmerge, "shape", "icon.code")
    }
    if (!is.null(node_color)) {
      dnodes$group.color <- dnodes$icon.color <- "#666666"
      nmerge <- c(nmerge, "group.color", "icon.color")
    }
    if (!is.null(annot)) {
      dnodes$title <- sprintf("<h3>%s</h3>", dnodes$id)
      nmerge <- c(nmerge, "title")
    }

    nodes <- merge(nodes,
                   dnodes,
                   by = nmerge,
                   all = TRUE,
                   sort = FALSE
                   )
    nodes <- nodes[!is.na(nodes$level), , drop = FALSE]
    edges <- merge(edges,
                   dedges,
                   by = emerge,
                   all = TRUE,
                   sort = FALSE
                   )
  }
  
  ## build visNetwork output

  out <- visNetwork::visNetwork(nodes, edges,
                                width = width,
                                height = height, ...)
  
  tt_style <- paste0("position: fixed;visibility:hidden;",
                     "padding: 5px;white-space: nowrap;",
                     "font-family: verdana;font-size:14px;",
                     "font-color:#000000;background-color: #EEEEEE;",
                     "-moz-border-radius: 3px;-webkit-border-radius: 3px;",
                     "border-radius: 3px;border: 1px solid #000000;")
  out <- visNetwork::visInteraction(out, tooltipStyle = tt_style)
  
  ## specify group colors, add legend
  if (legend) {
    if (!is.null(node_color) &&  (K < legend_max)) {
      leg_nodes <- data.frame(label = node_col_info$leg_lab,
                              color = node_col_info$leg_col,
                              shape = "circle",
                              shadow = FALSE,
                              font.size = 20)
    } else {
      leg_nodes <- NULL
    }

    if (!is.null(edge_color) &&  (L < legend_max)) {
      leg_edges <- data.frame(label = edge_col_info$leg_lab,
                              color = edge_col_info$leg_col,
                              dashes = FALSE,
                              font.size = 15,
                              font.align = 'top')
    } else {
      leg_edges <- NULL
    }

    if (!is.null(edge_linetype)) {
      ## Don't add extra legend keys if variable is the same
      if(!is.null(edge_color) && edge_linetype == edge_color) {
        leg_edges$dashes <- c(FALSE, TRUE)
      } else {
        tmp <- data.frame(label = unq_linetype,
                          color = 'black',
                          dashes = c(FALSE, TRUE),
                          font.size = 15,
                          font.align = 'top')
        leg_edges <- rbind(leg_edges, tmp)
      }
    }

    out <- visNetwork::visLegend(out,
                                 addNodes = leg_nodes,
                                 addEdges = leg_edges,
                                 useGroups = FALSE)

  }

  ## set nodes borders, edge width, and plotting options
  enabled <- list(enabled = TRUE)
  arg_selec <- if (selector) node_color else NULL
  
  ## options specific for x_axis
  if (!is.null(x_axis)) {
    
    out <- visNetwork::visHierarchicalLayout(out,
                                             direction = 'LR')
    
                                        # only display ids of "real" (i.e. case or linelist) nodes in select list
    selectvals <- setdiff(nodes$id, dnodes$id)
    out <- visNetwork::visOptions(out, 
                                  nodesIdSelection = list(values = selectvals),
                                  selectedBy = arg_selec,
                                  manipulation = editor)
  }
  
  ## should nodes collapse upon double clicking
  if(collapse) {
    collapse <- list(enabled = TRUE, keepCoord = TRUE)
  }

  ## options specific to highlight_downstream
  if(highlight_downstream) {
    

    out <- visNetwork::visOptions(out,
                                  highlightNearest = list(enabled = TRUE,
                                                          algorithm = "hierarchical",
                                                          degree = list(from = 0, to = 50),
                                                          hideColor = 'rgba(200,200,200,1)'),
                                  nodesIdSelection = FALSE,
                                  selectedBy = arg_selec,
                                  manipulation = editor,
                                  collapse = collapse)

  } else {
    out <- visNetwork::visOptions(out,
                                  highlightNearest = list(enabled = TRUE,
                                                          hideColor = 'rgba(200,200,200,1)'),
                                  selectedBy = arg_selec,
                                  manipulation = editor,
                                  collapse = collapse)
  }

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
