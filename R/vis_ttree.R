#' Plot epicontacts objects using visNetwork
#'
#' This function plots \code{\link{epicontacts}} objects with a dated x-axis
#' using the \code{visNetwork} package. The produced object is an
#' \code{htmlwidget} which will need rendering within a web browser.
#'
#' @export
#'
#'
#' @author
#' Finlay Campbell (\email{f.campbell15@imperial.ac.uk})
#'
#' @param x An \code{\link{epicontacts}} object.
#'
#' @param x_axis A character string indicating which field of the linelist data
#'   should be used to specify the x axis position (must be numeric or Date).
#'
#' @param ttree_shape 'branching' will create a branching transmission
#'   tree. 'rectangle' will create a rectangular shaped plot similar to a
#'   phylogeny that avoids overlapping edges.
#'
#' @param root_order A character string indicating which field of the linelist
#'   data is used to vertically order index cases of individual transmission
#'   chains (i.e. the 'roots' of the transmission trees). If root_order =
#'   'size', index cases will be ordered by the size of the downstream
#'   transmission chains they generate.
#'
#' @param node_order A character string indicating which field of the linelist
#'   data is used to vertically order nodes in the transmission tree (i.e. the
#'   'roots' of the transmission trees). If node_order = 'size', nodes will be
#'   ordered by the size of the downstream transmission chains they generate.
#'
#' @param reverse_root_order A logical indicating if the ordering of the roots
#'   should be reversed.
#'
#' @param reverse_node_order A logical indicating if the ordering of the nodes
#'   should be reversed.
#'
#' @param rank_contact If more than one incoming contact is provided for a given
#'   case, which attribute in the contact list should be used to rank the
#'   contacts and choose the top value. This contact forms the 'backbone' of the
#'   transmission tree and determines the y-position of the case. If a node
#'   attribute is provided, the pairwise difference in node attributes will be
#'   taken to rank the contacts.
#'
#' @param reverse_rank_contact Logical indicating if the contact ranking should
#'   be reversed in order.
#'
#' @param unlinked_pos A character string indicating where unlinked cases
#'   should be placed. Valid options are 'top', 'bottom' and 'middle', where
#'   'middle' will place unlinked cases according to root_order. 
#'
#' @param edge_flex A logical indicating if edges should bend when moved in
#'   visNetwork. If FALSE, edges will remain straight. 
#'
#' @param position_dodge A logical indicating if two cases can occupy the same y
#'   coordinate or 'dodge' each other.
#'
#' @param parent_pos Specify the position of the parent node relative to its
#'   children. Can be one of 'middle', 'top' or 'bottom'.
#' 
#' @param n_breaks The number of breaks on the x-axis timeline.
#'
#' @param axis_type Number of axes to be plotted (one of 'single', 'double',
#'   'none')
#'
#' @param igraph_type Alternate tree layouts provided by igraph. Must be one of
#'   'rt' for Reingold-Tilford layout, 'sugiyama' for Sugiyama layout or 'fr'
#'   for Fruchterman-Reingold layout.
#'
#' @param ... Additional arguments specified in \code{vis_epicontacts}.
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
#' plot(x,
#' method = 'ttree',
#' x_axis = 'dt_onset',
#' ttree_shape = 'rectangle',
#' node_size = 'R_i',
#' edge_label = 'exposure',
#' node_colour = 'sex',
#' edge_color = 'diff_dt_onset',
#' node_color = 'sex',
#' size_range = c(5, 30),
#' height = 2500,
#' selector = FALSE,
#' hide_labels = TRUE,
#' axis_type = 'double',
#' date_labels = "%b %d",
#' width = 1500)
#' }
#' }
vis_ttree <- function(x,
                      x_axis = NULL,
                      ttree_shape = 'branching',
                      root_order = x_axis,
                      node_order = x_axis,
                      reverse_root_order = FALSE,
                      reverse_node_order = FALSE,
                      rank_contact = x_axis,
                      reverse_rank_contact = FALSE,
                      unlinked_pos = 'bottom',
                      edge_flex = FALSE,
                      position_dodge = FALSE,
                      parent_pos = c('middle', 'top', 'bottom'),
                      n_breaks = 5,
                      axis_type = c("single", "double", "none"),
                      igraph_type = NULL,
                      ...) {

  ## this will assign the value specified in ... if present, otherwise use the
  ## specified default. A list based method using the assign function looks
  ## neater but causes global binding warnings in check.
  args <- list(...)
  node_color <- get_val('node_color', x_axis, args)
  node_shape <- get_val('node_shape', NULL, args)
  node_size <- get_val('node_size', 15, args)
  edge_color <- get_val('edge_color', NULL, args)
  edge_width <- get_val('edge_width', 3, args)
  edge_linetype <- get_val('edge_linetype', NULL, args)
  edge_label <- get_val('edge_label', NULL, args)
  col_pal <- get_val('col_pal', viridis::viridis_pal(), args)
  edge_col_pal <- get_val('edge_col_pal', grDevices::colorRampPalette(c("black", "red")), args)
  NA_col <- get_val('NA_col', "lightgrey", args)
  shapes <- get_val('shapes', NULL, args)
  size_range <- get_val('size_range', c(5, 20), args)
  width_range <- get_val('width_range', c(1, 5), args)
  label <- get_val('label', "id", args)
  annot  <- get_val('annot', TRUE, args)
  width <- get_val('width', "700px", args)
  height <- get_val('height', "700px", args)
  title <- get_val('title', NULL, args)
  legend <- get_val('legend', TRUE, args)
  legend_max <- get_val('legend_max', 10, args)
  selector <- get_val('selector', FALSE, args)
  editor <- get_val('editor', FALSE, args)
  hide_labels <- get_val('hide_labels', FALSE, args)
  highlight_downstream <- get_val('highlight_downstream', TRUE, args)
  date_labels <- get_val('date_labels', "%d/%m/%Y", args)
  collapse <- get_val('collapse', TRUE, args)
  thin <- get_val('thin', FALSE, args)
  font_size <- get_val('font_size', 15, args)
  custom_parent_pos <- get_val('custom_parent_pos', NULL, args)

  parent_pos <- match.arg(parent_pos)
  axis_type <- match.arg(axis_type)

  ## In the following, we pull the list of all plotted nodes (those from the
  ## linelist, and from the contacts data.frame, and then derive node attributes
  ## for the whole lot. These attributes are in turn used for plotting: as color
  ## ('group' in visNetwork terminology) or as annotations (converted to html
  ## code).

  ## Remove NAs in contacts and remove contacts that don't have both nodes in linelist
  x$contacts <- subset(x$contacts, !is.na(x$contacts$from) & !is.na(x$contacts$to))
  x$contacts <- subset(x$contacts, x$contacts$from %in% x$linelist$id &
                                   x$contacts$to %in% x$linelist$id)

  ## Remove linelist elements that aren't in contacts
  if(thin) {
    x$linelist <- x$linelist[x$linelist$id %in% x$contacts$from |
                             x$linelist$id %in% x$contacts$to,]
  }

  
  ## check that x_axis is specified
  if (is.null(x_axis)) {
    stop("x_axis must be specified")
  } else if(any(is.na(x$linelist[[x_axis]]))) {
    ## Prune NA x_axis values
    is_na <- is.na(x$linelist[[x_axis]])
    x$contacts <- x$contacts[!(x$contacts$from %in% x$linelist$id[is_na] |
                               x$contacts$to %in% x$linelist$id[is_na]),]
    x$linelist <- x$linelist[!is_na,]
    
  }

  ## Calculate R_i if needed
  if('R_i' %in% c(node_shape, node_color, node_size, node_order, root_order)) {
    x$linelist$R_i <- vapply(x$linelist$id,
                             function(i) sum(x$contacts$from == i, na.rm = TRUE),
                             numeric(1))
  }
  
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

  ## check node_order (node attribute used for vertical node ordering)
  node_order <- assert_node_order(x, node_order)

  ## check root_order (node attribute used for vertical root ordering)
  root_order <- assert_root_order(x, root_order)

  ## check rank_contact
  rank_contact <- assert_rank_contact(x, rank_contact)

  ## check root_order (node attribute used for vertical root ordering)
  custom_parent_pos <- assert_custom_parent_pos(custom_parent_pos)

  ## make a list of all nodes, and generate a data.frame of node attributes
  all_nodes <- get_id(x, which = "all", na.rm = TRUE)

  ## find out which nodes are unconnected to any other nodes
  ## This is only relevant when `thin = TRUE`
  nodes <- data.frame(id = all_nodes,
                      stringsAsFactors = FALSE)

  nodes <- x$linelist
  edges <- x$contacts

  ## Get x and y coordinates
  coor <- get_coor(x,
                   x_axis = x_axis,
                   position_dodge = position_dodge,
                   root_order = root_order,
                   reverse_root_order = reverse_root_order,
                   node_order = node_order,
                   reverse_node_order = reverse_node_order,
                   unlinked_pos = unlinked_pos,
                   rank_contact = rank_contact,
                   reverse_rank_contact = reverse_rank_contact,
                   axis_type = axis_type,
                   parent_pos = parent_pos,
                   custom_parent_pos = custom_parent_pos,
                   igraph_type = igraph_type)

  nodes$x <- x$linelist[[x_axis]]
  nodes$subtree_size <- coor$subtree_size
  x_axis_lab <- pretty(nodes[[x_axis]], n = n_breaks)
  
  ## Date -> numeric as visnetwork doesn't support dates
  if(inherits(nodes$x, c('Date', 'POSIXct'))) {
    nodes$x <- as.numeric(nodes$x)
  }
  
  ## Rescale coordinates to 0-1, flip y coordinates
  ## Make sure x rescaling accounts for x-axis nodes
  resc_x <- rescale(c(nodes$x, x_axis_lab), 0, 1)

  coor$y <- 1 - coor$y
  
  ## If percent specification, scale to 150x1840px (ie 100% dimension)
  if(regexpr('%', width) != -1) {
    resc_x <- extr_num(width)*resc_x*18.4
  } else {
    resc_x <- extr_num(width)*resc_x
  }
  nodes$x <- resc_x[1:nrow(nodes)]

  if(regexpr('%', height) != -1) {
    coor$y <- extr_num(height)*coor$y*1.5
  } else {
    coor$y <- extr_num(height)*coor$y
  }

  ## extract y coordinates ignoring axes
  if(axis_type %in% c("single", "double")) {
    nodes$y <- coor$y[-(1:ifelse(axis_type == 'double', 2, 1))]
  } else {
    nodes$y <- coor$y
  }
  
  ## Get nodes and edges for rectangle shape
  if(ttree_shape == 'rectangle') {

    rect <- get_v_rect(nodes, edges)
    nodes <- rect$nodes
    edges <- rect$edges
    hidden <- nodes$hidden
    
    ## No hidden nodes if shape is branching
  } else if(ttree_shape == 'branching') {
    hidden <- NULL
    if(nrow(edges) > 0) edges$to_node <- TRUE
  }

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
      edges$width <- rescale(as.numeric(edge_width_values), width_range[1], width_range[2])
    }
  }

  if (x$directed) {
    edges$arrows <- ifelse(edges$to_node, 'to', NA)
  }

  ## add edge labels
  if (!is.null(edge_label)) {
    edges$label <- as.character(edges[, edge_label])
    edges$label[!edges$to_node] <- ""
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
      stop("visNetwork only supports two linetypes; use binary variable or set method = 'ggplot'.")
    }
    ## Uses alphabetical order / factor order
    edges$dashes <- edges[[edge_linetype]] != sort(unq_linetype)[1]
  }

  ## set up nodes and edges if x_axis is specified
  
  if (!inherits(nodes[[x_axis]], c("numeric", "Date", "integer", "POSIXct"))) {
    stop("Data used to specify x axis must be a date or number")
  }

  ## add axes
  if(axis_type %in% c("single", "double")) {
    
    drange <- range(nodes[[x_axis]], na.rm = TRUE)
    nodes$level <- nodes[[x_axis]] - drange[1] + 1L
    drange <- seq(drange[1], drange[2], by = 1L)
    drange <- pretty(drange, n = n_breaks)
    ## create unique node ids for axes
    drange_id <- paste0("date_", seq_along(drange))
    
    dnodes <- data.frame(
      id = as.character(drange_id),
      level = drange - drange[1] + 1L,
      stringsAsFactors = FALSE
    )
    dedges <- data.frame(
      from = dnodes$id[-nrow(dnodes)],
      to   = dnodes$id[-1]
    )
    nmerge <- c("id", "level")
    emerge <- c("from", "to")
    if (!is.null(label)) {
      if(inherits(drange, c("Date", "POSIXct"))) {
        dnodes$label <- as.character(format(drange, date_labels))
      } else {
        dnodes$label <- as.character(drange)
      }
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

    ddnodes <- nodes[rep(1, nrow(dnodes)),]
    ddnodes[] <- NA
    var <- c('id', 'label', 'title')
    ddnodes[var] <- dnodes[var]
    col_var <- c('color.background', 'color.highlight.background',
                 'color.border', 'color.highlight.border')
    if(is.null(node_shape)) {
      ddnodes[col_var] <- 'black'
    }
    ddnodes$size <- 0.1
    ddnodes$borderWidth <- 0.1
    ddnodes$x <- utils::tail(resc_x, length(pretty(x$linelist[[x_axis]], n = n_breaks)))
    ddnodes$y <- coor$y[1]
    ddnodes$level <- 1
    if(ttree_shape == 'rectangle') ddnodes$hidden <- FALSE

    if(axis_type == 'double') {

      ddnodes_2 <- ddnodes
      ddnodes_2$y <- coor$y[2]
      ddnodes_2$id <- paste0(ddnodes_2$id, "_2")
      ddnodes <- rbind(ddnodes, ddnodes_2)

      dedges_2 <- dedges
      dedges_2$from <- paste0(dedges_2$from, "_2")
      dedges_2$to <- paste0(dedges_2$to, "_2")
      dedges <- rbind(dedges, dedges_2)
      
    }

    dnodes <- ddnodes
    nmerge <- names(dnodes)

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

  ## Change font size
  if(!is.null(font_size)) {
    edges$font.size <- font_size
    nodes$font.size <- font_size
  }
  
  ## build visNetwork output
  edges$arrows.ScaleFactor <- 5
  out <- visNetwork::visNetwork(nodes, edges,
                                width = width,
                                height = height)
  out <- visNetwork::visNodes(out,
                              fixed = list(x = TRUE, y = FALSE),
                              hidden = hidden,
                              borderWidth = 2,
                              borderWidthSelected = 2)
  out <- visNetwork::visEdges(out,
                              smooth = edge_flex,
                              arrowStrikethrough = FALSE,
                              font = list(align = 'top'),
                              selectionWidth = 1)
  out <- visNetwork::visPhysics(out,
                                enabled = FALSE)
  
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
                              font.size = 15)
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
                                 zoom = FALSE,
                                 addNodes = leg_nodes,
                                 addEdges = leg_edges,
                                 width = ifelse(is.null(edge_linetype),
                                                0.07,
                                                0.10),
                                 useGroups = FALSE)

  }

  ## set nodes borders, edge width, and plotting options
  enabled <- list(enabled = TRUE)
  arg_selec <- if (selector) 'id' else NULL

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

  ## add fontAwesome
  out <- visNetwork::addFontAwesome(out)

  ## add plot title
  if(!is.null(title)) {
    out$x$main$text <- title
    out$x$main$style <- "font-weight:bold;font-size:20px;text-align:center"
  }
  
  return(out)
  
}
