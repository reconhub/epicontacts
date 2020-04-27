#' Plot temporal networks using visNetwork
#'
#' This function plots \code{\link{epicontacts}} objects with a temporal x-axis
#' using the \code{visNetwork} package. The produced object is an
#' \code{htmlwidget} which will need rendering within a web browser.
#'
#' @export
#'
#'
#' @author Finlay Campbell (\email{finlaycampbell93@gmail.com})
#'
#' @param x An \code{\link{epicontacts}} object.
#'
#' @param x_axis A character string indicating which field of the linelist data
#'   should be used to specify the x axis position (must be numeric or Date).
#'
#' @param network_shape 'branching' will create a branching transmission
#'   tree. 'rectangle' will create a rectangular shaped plot similar to a
#'   phylogeny that avoids overlapping edges.
#'
#' @param root_order A character string indicating which field of the linelist
#'   data is used to vertically order index cases of individual transmission
#'   chains (i.e. the 'roots' of the transmission trees). If root_order =
#'   'subtree_size', index cases will be ordered by the size of the downstream
#'   transmission chains they generate.
#'
#' @param node_order A character string indicating which field of the linelist
#'   data is used to vertically order nodes in the transmission tree (i.e. the
#'   'roots' of the transmission trees). If node_order = 'subtree_size', nodes will be
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
#'   contacts and choose the top value. These contacts forms the 'scaffold' of the
#'   transmission tree and determine the y-position of each node. If a node
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
#' @param custom_parent_pos A function specifying the position of children nodes
#'   relative to their parent. This function must accept a single integer `x` as
#'   its only argument, specifying the number of children nodes. It must return
#'   a vector of length x, specifying the position of each child relative to the
#'   parent, where a x > 0 indicates above the parent, x < 0 indicates below the
#'   parent, and x = 0 indicates the same height as the parent.
#'
#' @param n_breaks The number of breaks on the x-axis timeline.
#'
#' @param axis_type Number of axes to be plotted (one of 'single', 'double',
#'   'none')
#'
#' @param igraph_type Alternate network shapes provided by igraph. Must be one of
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
#' x <- make_epicontacts(linelist = mers_korea_2015[[1]],
#'                        contacts = mers_korea_2015[[2]],
#'                        directed = TRUE)
#'
#' \dontrun{
#' plot(x,
#' x_axis = 'dt_onset',
#' network_shape = 'rectangle',
#' node_size = 'R_i',
#' edge_label = 'exposure',
#' node_colour = 'sex',
#' edge_color = 'diff_dt_onset',
#' node_color = 'sex',
#' size_range = c(5, 30),
#' height = 2500,
#' selector = FALSE,
#' label = FALSE,
#' axis_type = 'double',
#' date_labels = "%b %d",
#' width = 1500)
#' }
#' }
vis_temporal_interactive <- function(x,
                                     x_axis = NULL,
                                     network_shape = c('branching', 'rectangle'),
                                     root_order = x_axis,
                                     node_order = x_axis,
                                     reverse_root_order = FALSE,
                                     reverse_node_order = FALSE,
                                     rank_contact = x_axis,
                                     reverse_rank_contact = FALSE,
                                     unlinked_pos = c('bottom', 'top', 'middle'),
                                     edge_flex = FALSE,
                                     position_dodge = FALSE,
                                     parent_pos = c('middle', 'top', 'bottom'),
                                     custom_parent_pos = NULL,
                                     n_breaks = 5,
                                     axis_type = c("single", "double", "none"),
                                     igraph_type = NULL,
                                     ...) {

  ## this will assign the value specified in ... if present, otherwises use the
  ## defaults given for vis_epicontacts. A list based method using the assign
  ## function looks neater but causes global binding warnings in check.
  def <- as.list(args(vis_epicontacts))
  args <- list(...)
  node_color <- get_val('node_color', def, args)
  node_shape <- get_val('node_shape', def, args)
  node_size <- get_val('node_size', def, args)
  edge_color <- get_val('edge_color', def, args)
  edge_width <- get_val('edge_width', def, args)
  edge_linetype <- get_val('edge_linetype', def, args)
  edge_label <- get_val('edge_label', def, args)
  col_pal <- get_val('col_pal', def, args)
  edge_col_pal <- get_val('edge_col_pal', def, args)
  NA_col <- get_val('NA_col', def, args)
  shapes <- get_val('shapes', def, args)
  size_range <- get_val('size_range', def, args)
  width_range <- get_val('width_range', def, args)
  label <- get_val('label', def, args)
  annot  <- get_val('annot', def, args)
  width <- get_val('width', def, args)
  height <- get_val('height', def, args)
  title <- get_val('title', def, args)
  legend <- get_val('legend', def, args)
  legend_max <- get_val('legend_max', def, args)
  selector <- get_val('selector', def, args)
  editor <- get_val('editor', def, args)
  highlight_downstream <- get_val('highlight_downstream', def, args)
  date_labels <- get_val('date_labels', def, args)
  collapse <- get_val('collapse', def, args)
  thin <- get_val('thin', def, args)
  font_size <- get_val('font_size', def, args)
  arrow_size <- get_val("arrow_size", def, args)

  ## match arguments
  network_shape <- match.arg(network_shape)
  parent_pos <- match.arg(parent_pos)
  unlinked_pos <- match.arg(unlinked_pos)
  axis_type <- match.arg(axis_type)

  ## Calculate R_i if needed
  if('R_i' %in% c(node_shape, node_color, node_size, node_order, root_order)) {
    x$linelist$R_i <- vapply(x$linelist$id,
                             function(i) sum(x$contacts$from == i, na.rm = TRUE),
                             numeric(1))
  }


  ## in the following, we pull the list of all plotted nodes (those from the
  ## linelist, and from the contacts data.frame, and then derive node attributes
  ## for the whole lot. These attributes are in turn used for plotting: as color
  ## ('group' in visNetwork terminology) or as annotations (converted to html
  ## code).

  ## remove NAs in contacts and linelist
  x <- x[i = !is.na(x$linelist$id),
         j = !is.na(x$contacts$from) & !is.na(x$contacts$to)]

  ## test x_axis
  x_axis <- assert_x_axis(x, x_axis)

  ## count number of nodes not in linelist
  not_in_ll <- sum(!get_id(x, 'contacts') %in% get_id(x, 'linelist'))

  ## count number of contacts without x_axis data
  contacts_rm <- sum(is.na(get_pairwise(x, x_axis)))

  ## identify linelist elements with NAs in x_axis
  na_x_axis <- is.na(x$linelist[[x_axis]])

  ## warning to list number of nodes and edges not displayed
  msg <- "%s nodes and %s edges removed as x_axis data is unavailable"
  sm <- not_in_ll + sum(na_x_axis) + contacts_rm
  if(sm > 0) {
    warning(sprintf(msg, not_in_ll + sum(na_x_axis), contacts_rm))
  }

  ## remove NA x_axis elements from linelist
  x <- x[!na_x_axis]

  ## remove contacts that don't have both nodes in linelist
  x <- thin(x, what = 'contacts')

  ## remove linelist elements that aren't in contacts if thin = TRUE
  if(thin) {
    x <- thin(x)
  }

  ## check that contacts with x_axis data are available
  if(nrow(x$contacts) == 0L) {
    stop("No contacts found between cases with available x_axis data")
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

  ## assign nodes and edges
  nodes <- x$linelist
  edges <- x$contacts

  ## Get y coordinates and tree properties
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

  ## Get x coordinates
  nodes$x <- x$linelist[[x_axis]]

  ## Get subtree size for potential later use
  nodes$subtree_size <- coor$subtree_size

  ## Date -> numeric as visnetwork doesn't support dates
  x_axis_lab <- pretty(nodes[[x_axis]], n = n_breaks)
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

  ## extract y coordinates, ignore
  if(axis_type %in% c("single", "double")) {
    nodes$y <- coor$y[-(1:ifelse(axis_type == 'double', 2, 1))]
  } else {
    nodes$y <- coor$y
  }

  ## Get nodes and edges for rectangle shape
  if(network_shape == 'rectangle') {

    rect <- get_v_rect(nodes, edges)
    nodes <- rect$nodes
    edges <- rect$edges
    hidden <- nodes$hidden

    ## No hidden nodes if shape is branching
  } else if(network_shape == 'branching') {
    hidden <- NULL
    if(nrow(edges) > 0) edges$to_node <- TRUE
  }

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
    if(!is.null(node_size)) nodes$icon.size <- nodes$size
    node_shape_info <- data.frame(icon = unique(node_code),
                                  leg_lab = unique(vec_node_shapes))
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

  ## no arrows for intermediate edges
  if (x$directed) {
    edges$arrows.to <- edges$to_node
  } else {
    edges$arrows.to <- FALSE
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
      msg <- paste0("visNetwork only supports two linetypes; ",
                    "use binary variable or set method = 'ggplot'.")
      stop(msg)
    }
    ## use alphabetical order / factor order
    edges$dashes <- edges[[edge_linetype]] != sort(unq_linetype)[1]
  }

  ## check x_axis
  if (!inherits(nodes[[x_axis]], c("numeric", "Date", "integer", "POSIXct"))) {
    stop("Data used to specify x axis must be a date or number")
  }

  ## add axes
  if(axis_type %in% c("single", "double")) {

    ## get the range of dates
    axis_range <- range(nodes[[x_axis]], na.rm = TRUE)
    nodes$level <- nodes[[x_axis]] - axis_range[1] + 1L
    axis_range <- seq(axis_range[1], axis_range[2], by = 1L)
    axis_range <- pretty(axis_range, n = n_breaks)

    ## create axis nodes and edges
    axis_nodes <- data.frame(id = paste0("date_", seq_along(axis_range)),
                             level = axis_range - axis_range[1] + 1L,
                             stringsAsFactors = FALSE)
    axis_edges <- data.frame(from = axis_nodes$id[-nrow(axis_nodes)],
                             to = axis_nodes$id[-1],
                             stringsAsFactors = FALSE)

    ## add labels
    if(inherits(axis_range, c("Date", "POSIXct"))) {
      axis_nodes$label <- as.character(format(axis_range, date_labels))
    } else {
      axis_nodes$label <- as.character(axis_range)
    }

    ## create node dataframe and fill with axis_nodes data
    var <- c('id', 'label')
    val <- axis_nodes[var]

    axis_nodes <- nodes[rep(1, nrow(axis_nodes)),]
    axis_nodes[] <- NA
    axis_nodes[var] <- val

    ## format axis nodes
    col_var <- c('color.background', 'color.highlight.background',
                 'color.border', 'color.highlight.border')
    if(is.null(node_shape)) axis_nodes[col_var] <- 'black'
    axis_nodes$size <- 0.1
    axis_nodes$borderWidth <- 0.1

    ## assign x and y axis positions
    axis_nodes$x <- utils::tail(resc_x, length(pretty(x$linelist[[x_axis]],
                                                   n = n_breaks)))
    axis_nodes$y <- coor$y[1]
    axis_nodes$level <- 1

    if(network_shape == 'rectangle') axis_nodes$hidden <- FALSE

    if(axis_type == 'double') {

      axis_nodes_2 <- axis_nodes
      axis_nodes_2$y <- coor$y[2]
      axis_nodes_2$id <- paste0(axis_nodes_2$id, "_2")
      axis_nodes <- rbind(axis_nodes, axis_nodes_2)

      axis_edges_2 <- axis_edges
      axis_edges_2$from <- paste0(axis_edges_2$from, "_2")
      axis_edges_2$to <- paste0(axis_edges_2$to, "_2")
      axis_edges <- rbind(axis_edges, axis_edges_2)

    }

    nodes <- merge(nodes, axis_nodes, by = names(axis_nodes),
                   all = TRUE, sort = FALSE)
    nodes <- nodes[!is.na(nodes$level), , drop = FALSE]

    edges <- merge(edges, axis_edges, by = c("from", "to"),
                   all = TRUE, sort = FALSE)
    edges$arrows.to[is.na(edges$arrows.to)] <- FALSE
    edges$color[is.na(edges$color)] <- 'black'

  }

  ## Change font size
  if(!is.null(font_size)) {
    edges$font.size <- font_size
    nodes$font.size <- font_size
  }

  ## build visNetwork output
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
  if(x$directed) {
    out <- visNetwork::visEdges(out, arrows = list(to = list(scaleFactor = arrow_size)))
  }
  out <- visNetwork::visPhysics(out, enabled = FALSE)

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
                              font.size = ifelse(is.null(font_size),
                                                 14, font_size))
    } else {
      leg_nodes <- NULL
    }

    ## specify edge colors, add legend
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

    ## specify edge linetype, add legend
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

    ## specify node shape, add legend
    if (!is.null(node_shape) && nrow(node_shape_info) < legend_max) {
      tmp <- data.frame(label = node_shape_info$leg_lab,
                        icon.color = 'black',
                        shape = "icon",
                        icon.code = node_shape_info$icon,
                        shadow = FALSE,
                        font.size = ifelse(is.null(font_size),
                                           14, font_size))
      leg_nodes$shape <- "icon"
      leg_nodes$icon.code <- codeawesome["circle"]
      names(leg_nodes)[names(leg_nodes) == "color"] <- "icon.color"
      leg_nodes <- rbind(leg_nodes, tmp)
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
  arg_selec <- selector

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
