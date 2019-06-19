#' Plot epicontacts objects using ggplot
#'
#' This function plots \code{\link{epicontacts}} objects using the
#' \code{ggplot2} package. This output is only available for directed, acyclical
#' networks with one incoming edge per node (e.g. a transmission network with
#' only one infector per case)
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
#' @param edge_alpha An integer/numeric indicating the global transparency of
#'   the edges, or a character string indicating which field of the contacts data
#'   should be used to indicate the edge transparency.
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
#' @param null_node_color A character indicating the global node colour if
#'   node_color has not been specifed or has been specified as NULL.
#'
#' @param null_edge_color A character indicating the global edge colour if
#'   edge_color has not been specifed or has been specified as NULL.
#'
#' @param lineend Character indicating the lineend to be used for geom_segment.
#' 
#' @param unlinked_pos A character string indicating where unlinked cases
#'   should be placed. Valid options are 'top', 'bottom' and 'middle', where
#'   'middle' will place unlinked cases according to root_order. This argument
#'   is only called when type = 'ttree'.
#'
#' @param position_dodge A logical indicating if two cases can occupy the same y
#'   coordinate or 'dodge' each other. This argument is only called when type =
#'   'ttree'.
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
#' @param y_label A logical indicating if case IDs should be displayed on the
#'   y-axis labels. Only works when position_dodge = TRUE, otherwise
#'   y-coordinates are not unique.
#'
#' @param y_coor Manual specification of y coordinates. Must be a vector with one
#'   y coordinate for each case between 0 and 1.
#'
#' @param igraph_type Alternate tree layouts provided by igraph. Must be one of
#'   'rt' for Reingold-Tilford layout, 'sugiyama' for Sugiyama layout or 'fr'
#'   for Fruchterman-Reingold layout.
#'
#'
#' @param ... Further arguments to be passed to \code{ggplot}.
#'
#' @return The same output as \code{ggplot2}.
#'
#' @seealso \code{\link[visNetwork]{visNetwork}} in the package \code{visNetwork}.
#'   \code{\link{edges_pal}} and \code{\link{cases_pal}} for color palettes used
#'
#' @importFrom ggplot2 aes_string element_blank geom_point geom_segment ggplot
#'   labs scale_color_gradient scale_color_viridis_d scale_fill_viridis_c
#'   scale_fill_viridis_d scale_size scale_y_continuous theme theme_minimal unit
#'
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
vis_ggplot <- function(x,
                       x_axis,
                       edge_alpha = NULL,
                       ttree_shape = 'branching',
                       root_order = 'size',
                       node_order = 'size',
                       reverse_root_order = FALSE,
                       reverse_node_order = FALSE,
                       null_node_color = 'black',
                       null_edge_color = 'black',
                       lineend = 'butt',
                       unlinked_pos = 'bottom',
                       position_dodge = FALSE,
                       parent_pos = 'middle',
                       custom_parent_pos = NULL,
                       y_label = FALSE,
                       y_coor = NULL,
                       igraph_type = NULL) {


  ## this will assign the value specified in ... if present, otherwise use the
  ## specified default. A list based method using the assign function looks
  ## neater but causes global binding warnings in check.
  args <- list(...)
  node_color <- get_val('node_color', "id", args)
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
  highlight_downstream <- get_val('highlight_downstream', TRUE, args)
  date_labels <- get_val('date_labels', "%d/%m/%Y", args)
  collapse <- get_val('collapse', TRUE, args)
  thin <- get_val('thin', TRUE, args)
  font_size <- get_val('font_size', 15, args)
  custom_parent_pos <- get_val('custom_parent_pos', NULL, args)

  parent_pos <- match.arg(parent_pos)

  
  ## In the following, we pull the list of all plotted nodes (those from the
  ## linelist, and from the contacts data.frame, and then derive node attributes
  ## for the whole lot. These attributes are in turn used for plotting: as color
  ## ('group' in visNetwork terminology) or as annotations (converted to html
  ## code).

  ## check node_color (node attribute used for color)
  node_color <- assert_node_color(x, node_color)

  ## check node_size (node attribute used for color)
  node_size <- assert_node_size(x, node_size)

  ## check edge_color (edge attribute used for color)
  edge_color <- assert_edge_color(x, edge_color)

  ## check edge_linetype (edge attribute used for linetype)
  edge_linetype <- assert_edge_linetype(x, edge_linetype)

  ## check edge_alpha (edge attribute used for alpha)
  edge_alpha <- assert_edge_alpha(x, edge_alpha)

  ## If label = TRUE, position_dodge must also be
  if(y_label & !position_dodge) {
    stop("position_dodge must be TRUE if y_label is TRUE")
  }

  ## Remove NAs in contacts
  x$contacts <- subset(x$contacts, !is.na(x$contacts$from) & !is.na(x$contacts$to))
  
  ## Calculate R_i if needed
  if('R_i' %in% c(node_color, node_size, node_order, root_order)) {
    x$linelist$R_i <- vapply(x$linelist$id,
                             function(i) sum(x$contacts$from == i, na.rm = TRUE),
                             1)
  }
  
  ## check that x_axis is specified
  if (is.null(x_axis)) {
    stop("x_axis must be specified if type = 'ttree'")
  }

  nodes <- x$linelist
  edges <- x$contacts

  ## Get x and y coordinates
  if(is.null(y_coor)) {
    coor <- get_coor(x,
                     x_axis = x_axis,
                     position_dodge = position_dodge,
                     root_order = root_order,
                     reverse_root_order = reverse_root_order,
                     node_order = node_order,
                     reverse_node_order = reverse_node_order,
                     unlinked_pos = unlinked_pos,
                     axis_type = 'none',
                     parent_pos = parent_pos,
                     custom_parent_pos = custom_parent_pos,
                     method = 'ggplot',
                     igraph_type = igraph_type)
    nodes$y <- coor$y
  } else {
    nodes$y <- y_coor
  }
  
  nodes$x <- x$linelist[[x_axis]]
  nodes$subtree_size <- coor$subtree_size

  if(ttree_shape == 'rectangle') {

    ## Get vertical and horizontal edges with correct edge attributes
    df <- get_g_rect(nodes, edges)

    i_ind <- match(edges$to, nodes$id)
    inf_ind <- match(edges$from, nodes$id)

    ## Get horizontal edges
    df1 <- data.frame(y = nodes$y[i_ind],
                      yend = nodes$y[i_ind],
                      x = nodes$x[inf_ind],
                      xend = nodes$x[i_ind])
    df1 <- cbind(df1, edges[!names(edges) %in% c("from", "to")])
    df <- df[apply(df[,1:4], 1, function(xx) !any(is.na(xx))),]
    
    df <- rbind(df, df1)

  } else if(ttree_shape == 'branching') {

    i_ind <- match(edges$to, nodes$id)
    inf_ind <- match(edges$from, nodes$id)
    
    df <- data.frame(y = nodes$y[inf_ind],
                     yend = nodes$y[i_ind],
                     x = nodes$x[inf_ind],
                     xend = nodes$x[i_ind])
    df <- cbind(df, edges[!names(edges) %in% c("from", "to")])
    df <- df[apply(df[,1:4], 1, function(xx) !any(is.na(xx))),]

  }

  ## Specifying node color palette for different use cases
  if(!is.null(node_color) & !missing(node_color)) {

    if(missing(col_pal) & inherits(nodes[[node_color]], c('factor', 'character'))) {
      
      col_pal <- scale_fill_viridis_d()
      
      ## Annoying workaround to use viridis_color with dates
    } else if(inherits(nodes[[node_color]], 'Date')) {
      
      dates <- pretty(nodes[[node_color]])
      numeric_node_color <- as.numeric(nodes[[node_color]])
      node_color <- paste0(node_color, '_')
      nodes[[node_color]] <- numeric_node_color

      if(missing(col_pal)) {
        col_pal <- scale_fill_viridis_c(breaks = as.numeric(dates), labels = dates)
      } else {
        col_pal <- col_pal(breaks = as.numeric(dates), labels = dates)
      }

      ## Continous colour scales
    } else {
      if(missing(col_pal)) {
        col_pal <- scale_fill_viridis_c()
      } else {
        col_pal <- col_pal()
      }
    }
  } else {
    col_pal <- NULL
  }

  ## Specifying edge color palette for different use cases
  if(!is.null(edge_color)) {
    
    if(missing(edge_col_pal) & inherits(edges[[edge_color]], c('factor', 'character'))) {
      
      edge_col_pal <- scale_color_viridis_d()
      
      ## Annoying workaround to use colour scale with dates
    } else if(inherits(edges[[edge_color]], 'Date')) {
      
      dates <- pretty(edges[[edge_color]])
      numeric_edge_color <- as.numeric(df[[edge_color]])
      edge_color <- paste0(edge_color, '_')
      df[[edge_color]] <- numeric_edge_color

      if(missing(edge_col_pal)) {
        edge_col_pal <- scale_color_gradient(low = 'black', high = 'red',
                                             breaks = as.numeric(dates),
                                             labels = dates)
      } else {
        edge_col_pal <- edge_col_pal(breaks = as.numeric(dates), labels = dates)
      }
      
    } else {
      if(missing(edge_col_pal)) {
        edge_col_pal <- scale_color_gradient(low = 'black', high = 'red')
      } else {
        edge_col_pal <- edge_col_pal()
      }
    }
  } else {
    edge_col_pal <- NULL
  }

  ## ggplot default node_size is 3
  if(missing(node_size) | is.null(node_size)) {
    node_size <- 3
  }

  ## Check node size attribute
  if(inherits(node_size, c("numeric", "integer"))) {

    if(is.null(node_color) | missing(node_color)) {
      point <- geom_point(data = nodes,
                          aes_string(x = "x",
                                     y = "y"),
                          fill = null_node_color,
                          size = node_size,
                          shape = 21)
    } else {
      point <- geom_point(data = nodes,
                          aes_string(x = "x",
                                     y = "y",
                                     fill = node_color),
                          size = node_size,
                          shape = 21)
    }
    
    size_pal <- NULL

  } else {

    if(inherits(nodes[[node_size]], 'character')) {
      
      stop("Character class cannot be mapped to size; convert to factor if necessary.")
      
    } else if(inherits(nodes[[node_size]], 'Date')) {
      
      dates <- pretty(nodes[[node_size]])
      numeric_node_size <- as.numeric(nodes[[node_size]])
      node_size <- paste0(node_size, '_')
      nodes[[node_size]] <- numeric_node_size
      size_pal <- scale_size(range = c(size_range[1], size_range[2]),
                             breaks = scales::pretty_breaks(as.numeric(dates)),
                             labels = dates)
      
    } else if(inherits(nodes[[node_size]], 'factor')) {
      
      warning("Mapping factor to size; converting factors to integers.")
      lev <- levels(nodes[[node_size]])
      ind <- as.integer(nodes[[node_size]])
      nodes[[node_size]] <- ind
      size_pal <- scale_size(range = c(size_range[1], size_range[2]),
                             breaks = scales::pretty_breaks(sort(unique(ind))),
                             labels = lev)
    } else {
      size_pal <- scale_size(range = c(size_range[1], size_range[2]),
                             breaks = scales::pretty_breaks())
    }

    if(is.null(node_color) | missing(node_color)) {
      point <- geom_point(data = nodes,
                          aes_string(x = "x",
                                     y = "y",
                                     size = node_size),
                          fill = null_node_color,
                          shape = 21)
    } else {
      point <- geom_point(data = nodes,
                          aes_string(x = "x",
                                     y = "y",
                                     size = node_size,
                                     fill = node_color),
                          shape = 21)
    }
    
  }

  if(x$directed) {
    arrow <- arrow(length = unit(0.015, "npc"), type = 'closed', ends = 'last')
  } else {
    arrow <- NULL
  }
  
  ## If edge_alpha is numeric, use as alpha specification for all edges
  if(inherits(edge_alpha, c("numeric", "integer"))) {
    if(is.null(edge_color) | missing(edge_color)) {
      seg <- geom_segment(aes_string(x = "x",
                                     xend = "xend",
                                     y = "y",
                                     yend = "yend",
                                     linetype = edge_linetype),
                          color = null_edge_color,
                          arrow = arrow,
                          alpha = edge_alpha,
                          lineend = lineend,
                          size = edge_width)
    } else {
      seg <- geom_segment(aes_string(x = "x",
                                     xend = "xend",
                                     y = "y",
                                     yend = "yend",
                                     color = edge_color,
                                     linetype = edge_linetype),
                          arrow = arrow,
                          alpha = edge_alpha,
                          lineend = lineend,
                          size = edge_width)

    }
  } else {

    if(is.null(edge_color) | missing(edge_color)) {

      seg <- geom_segment(aes_string(x = "x",
                                     xend = "xend",
                                     y = "y",
                                     yend = "yend",
                                     linetype = edge_linetype,
                                     alpha = edge_alpha),
                          color = null_edge_color,
                          lineend = lineend,
                          arrow = arrow,
                          size = edge_width)
    } else {
      seg <- geom_segment(aes_string(x = "x",
                                     xend = "xend",
                                     y = "y",
                                     yend = "yend",
                                     color = edge_color,
                                     linetype = edge_linetype,
                                     alpha = edge_alpha),
                          lineend = lineend,
                          arrow = arrow,
                          size = edge_width)
    }
  }

  if(y_label) {
    y_scale <- scale_y_continuous(name = NULL,
                                  breaks = sort(coor$y),
                                  minor_breaks = NULL,
                                  labels = x$linelist[[label]][order(coor$y)],
                                  expand = c(0.01, 0.01))
    ttheme <- theme(axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.minor.y = element_blank())
  } else {
    y_scale <- NULL
    ttheme <-theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title.y = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank())
  }

  out <- ggplot(df) +
    seg +
    point +
    col_pal +
    edge_col_pal +
    size_pal +
    y_scale +
    theme_minimal() +
    ttheme +
    labs(x = x_axis)
    
  return(out)
  
}
