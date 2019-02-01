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
#' @param type A character string indicating the type of plot. 'network' will
#'   use the visNetwork physics engine to determine node positions and can be
#'   used for all network types. 'ttree' calculates node positions manually and
#'   is designed specifically for visualising transmission trees. 'ttree' only
#'   works for acyclical networks with one ingoing edge per node (i.e. one
#'   infector per individual).
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
#' @param legend A logical indicating whether a legend should be added to the
#'   plot.
#' 
#' @param legend_max The maximum number of groups for a legend to be displayed.
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
#' @param x_axis A character string indicating which field of the linelist data
#'   should be used to specify the x axis position (must be numeric or Date).
#'
#' @param ttree_shape 'branching' will create a branching transmission
#'   tree. 'rectangle' will create a rectangular shaped plot similar to a
#'   phylogeny that avoids overlapping edges. This argument is only called when
#'   type = 'ttree'.
#'
#' @param root_order A character string indicating which field of the linelist
#'   data is used to vertically order index cases of individual transmission
#'   chains (i.e. the 'roots' of the transmission trees). If root_order =
#'   'size', index cases will be ordered by the size of the downstream
#'   transmission chains they generate. This argument is only called when type =
#'   'ttree'.
#'
#' @param node_order A character string indicating which field of the linelist
#'   data is used to vertically order nodes in the transmission tree (i.e. the
#'   'roots' of the transmission trees). If node_order = 'size', nodes will be
#'   ordered by the size of the downstream transmission chains they generate.
#'   This argument is only called when type = 'ttree'.
#'
#' @param reverse_root_order A logical indicating if the ordering of the roots
#'   hsould be reversed. This argument is only called when type = 'ttree'.
#'
#' @param reverse_node_order A logical indicating if the ordering of the nodes
#'   should be reversed. This argument is only called when type = 'ttree'.
#'
#' @param rank_contact If more than one incoming contact is provided for a given
#'   case, which attribute in the linelist should be used to rank the contacts
#'   and choose the top value. This contact forms the 'backbone' of the
#'   transmission tree and determines the y-position of the case.
#'
#' @param reverse_rank_contact Logical indicating if the contact ranking should
#'   be reversed in order.
#'
#' @param position_unlinked A character string indicating where unlinked cases
#'   should be placed. Valid options are 'top', 'bottom' and 'middle', where
#'   'middle' will place unlinked cases according to root_order. This argument
#'   is only called when type = 'ttree'.
#'
#' @param edge_flex A logical indicating if edges should bend when moved in
#'   visNetwork. If FALSE, edges will remain straight. This argument
#'   is only called when type = 'ttree' and ttree_output = 'visNetwork'.
#'
#' @param highlight_downstream A logical indicating if all cases 'downstream' of
#'   the the selected node should be highlighted. Defaults to FALSE if type =
#'   'network' and defaults to TRUE if type = 'ttree'.
#'
#' @param position_dodge A logical indicating if two cases can occupy the same y
#'   coordinate or 'dodge' each other. This argument is only called when type =
#'   'ttree'.
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
                            type = 'network',
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
                            legend = TRUE,
                            legend_max = 10,
                            thin = TRUE,
                            selector = TRUE,
                            editor = FALSE,
                            x_axis = NULL,
                            ttree_shape = 'branching',
                            root_order = 'size',
                            node_order = 'onset',
                            reverse_root_order = FALSE,
                            reverse_node_order = FALSE,
                            rank_contact = x_axis,
                            reverse_rank_contact = FALSE,
                            position_unlinked = 'bottom',
                            edge_flex = FALSE,
                            highlight_downstream = FALSE,
                            position_dodge = FALSE,
                            split_type = 2,
                            ...){

  ## In the following, we pull the list of all plotted nodes (those from the
  ## linelist, and from the contacts data.frame, and then derive node attributes
  ## for the whole lot. These attributes are in turn used for plotting: as color
  ## ('group' in visNetwork terminology) or as annotations (converted to html
  ## code).

  ## check that x_axis is specified
  if(type == 'ttree') {
    if (is.null(x_axis)) {
      stop("x_axis must be specified if type = 'ttree'")
    } else if(any(is.na(x$linelist[[x_axis]]))) {
      ## Prune NA x_axis values
      is_na <- is.na(x$linelist[[x_axis]])
      x$contacts <- x$contacts[!(x$contacts$from %in% x$linelist$id[is_na] |
                                 x$contacts$to %in% x$linelist$id[is_na]),]
      x$linelist <- x$linelist[!is_na,]
      if(thin) x <- thin(x)
    }
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

  ## Calculate R_i if needed
  if('R_i' %in% c(node_shape, node_color, node_size, node_order, root_order)) {
    x$linelist$R_i <- sapply(x$linelist$id, function(i) sum(x$contacts$from == i, na.rm = TRUE))
  }
  
  if (type == 'network') {

    ## make a list of all nodes, and generate a data.frame of node attributes
    cont_nodes <- c(x$contacts$from, x$contacts$to)
    list_nodes <- x$linelist$id

    nodes <- data.frame(id = unique(c(list_nodes, cont_nodes)),
                        stringsAsFactors = FALSE)

    nodes <- merge(nodes, x$linelist, by = "id", all = TRUE)

    
    if(!missing(ttree_shape)) {
      warning("Argument 'ttree_shape' is unused when type = 'network'")
    }

    edges <- x$contacts
    
    ## check ttree options
  } else if(type == 'ttree') {

    ## Change default height and width to pixel measures - visNetwork seems to
    ## struggle with percentage height specification
    if(missing(width)) {
      width <- "1000px"
    }
    if(missing(height)) {
      height <- "900px"
    }
    if(missing(node_color)) {
      node_color <- x_axis
    }
    if(missing(col_pal)) {
      col_pal <- viridis::viridis_pal()
    }
    if(missing(edge_col_pal)) {
      edge_col_pal <- colorRampPalette(c("red", "black"))
    }
    
    ## Check for multiple incoming edges per node
#    tab <- table(x$contacts$to)
#    culprits <- names(tab)[tab > 1]
#    if (length(culprits) != 0) {
#      culprits <- paste(culprits, collapse = ", ")
#      msg <- sprintf("multiple infectors found for %s . use type = 'network'",
#                     culprits)
#      stop(msg)
#    }

    ## set default highlight_nearest to TRUE
    if(missing(highlight_downstream)) {
      highlight_downstream <- TRUE
    }

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
                     position_unlinked = position_unlinked,
                     rank_contact = rank_contact,
                     reverse_rank_contact = reverse_rank_contact,
                     split_type = split_type)

    nodes$x <- coor$x
    nodes$y <- coor$y

    ## Date -> numeric as visnetwork doesn't support dates
    if(inherits(nodes$x, 'Date')) {
      nodes$x <- as.numeric(nodes$x)
    }
    
    ## Rescale coordinates to 0-1, flip y coordinates
    nodes$x <- rescale(nodes$x, 0, 1)
    nodes$y <- 1 - nodes$y

    ## If percent specification, scale to 150x1840px (ie 100% dimension)
    if(regexpr('%', width) != -1) {
      nodes$x <- extr_num(width)*nodes$x*18.4
    } else {
      nodes$x <- extr_num(width)*nodes$x
    }

    if(regexpr('%', height) != -1) {
      nodes$y <- extr_num(height)*nodes$y*1.5
    } else {
      nodes$y <- extr_num(height)*nodes$y
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
    }
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
                             legend = TRUE)
    K <- length(node_col_info$leg_lab)
    if(!is.null(node_shape)) {
      nodes$icon.color <- node_col_info$color
    } else if(!is.null(x_axis) & type == 'network') {
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

  ## set up nodes and edges if x_axis is specified
  if (type == "network" & !is.null(x_axis)) {
    if (!inherits(nodes[[x_axis]], c("numeric", "Date", "integer"))) {
      stop("Data used to specify x axis must be a date or number")
    }
    drange      <- range(nodes[[x_axis]], na.rm = TRUE)
    nodes$level <- nodes[[x_axis]] - drange[1] + 1L
    drange      <- seq(drange[1], drange[2], by = 1L)
    dnodes      <- data.frame(
      id = as.character(drange),
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
    ## default ot black
    edges$color <- 'black'
  }


  ## add edge linetype
  if(!is.null(edge_linetype)) {
    unq <- unique(edges[[edge_linetype]])
    if(length(unq) > 2) {
      stop("visNetwork only supports two linetypes; use binary variable or set method = 'ggplot'.")
    }
    ## Uses alphabetical order / factor order
    edges$dashes <- edges[[edge_linetype]] != sort(unq)[1]
  }

  
  ## build visNetwork output

  if (type == 'network') {
    
    out <- visNetwork::visNetwork(nodes, edges,
                                  width = width,
                                  height = height, ...)
  } else if (type == 'ttree') {

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
                    arrows = NULL,
                    selectionWidth = 1)
    out <- visNetwork::visPhysics(out,
                      enabled = FALSE)

  }
  
  ## specify group colors, add legend
  if (legend) {
    if (!is.null(node_color) &&  (K < legend_max)) {
      leg_nodes <- data.frame(label = node_col_info$leg_lab,
                              color = node_col_info$leg_col,
                              shape = "circle",
                              shadow = FALSE,
                              position = 'top',
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

    out <- visNetwork::visLegend(out,
                                 addNodes = leg_nodes,
                                 addEdges = leg_edges,
                                 useGroups = FALSE)

  }

  ## set nodes borders, edge width, and plotting options
  enabled <- list(enabled = TRUE)
  arg_selec <- if (selector) node_color else NULL
  
  ## options specific for x_axis and type = 'network'
  if (!is.null(x_axis) & type == 'network'){
    
    out <- visNetwork::visHierarchicalLayout(out, 
                                             direction = 'LR')
    
                                        # only display ids of "real" (i.e. case or linelist) nodes in select list
    selectvals <- setdiff(nodes$id, dnodes$id)
    out <- visNetwork::visOptions(out, 
                                  nodesIdSelection = list(values = selectvals),
                                  selectedBy = arg_selec,
                                  manipulation = editor)
  }


  ## options specific to highlight_downstream
  if(highlight_downstream) {
    out <- visNetwork::visOptions(out,
                                  highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", 
                                                          degree = list(from = 0, to = 50),
                                                          hideColor = 'rgba(200,200,200,1)'),
                                  nodesIdSelection = FALSE,
                                  selectedBy = arg_selec,
                                  manipulation = editor)
  } else {
    out <- visNetwork::visOptions(out,
                                  highlightNearest = list(enabled = TRUE,
                                                          hideColor = 'rgba(200,200,200,1)'),
                                  selectedBy = arg_selec,
                                  manipulation = editor)
  }

  if(type != 'ttree') {
    out <- visNetwork::visPhysics(out, stabilization = FALSE)
  }

  ## add fontAwesome
  out <- visNetwork::addFontAwesome(out)

  return(out)
  
}
