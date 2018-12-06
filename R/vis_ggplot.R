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
#' @param node_color An index or character string indicating which field of the
#'   linelist should be used to color the nodes. If node color = 'R_i', the
#'   individual reproductive number for each case (i.e. number of outgoing
#'   infection/contacts) will be calculated and used to specify the node colour.
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
#' @param edge_linetype An integer or character string indicating which field of
#'   the contacts data should be used to indicate the edge linetype. If the
#'   output format is visNetwork, this field of the contacts data must contain a
#'   binary variable, as visNetwork only supports dashed/non-dashed edges.
#'  
#' @param col_pal A scale_fill ggplot function specifying the color palette for
#'   the nodes. The function must be provided (e.g. scale_fill_discrete), not
#'   the palette itself (e.g. scale_fill_discrete()). 
#'
#' @param edge_col_pal A scale_color ggplot function specifying the color palette for
#'   the edges. The function must be provided (e.g. scale_color_discrete), not
#'   the palette itself (e.g. scale_color_discrete()).
#'
#' @param size_range A numeric vector of length 2, indicating the minimum and
#'   maximum node size.
#'
#' @param legend A logical indicating whether a legend should be added to the
#'   plot.
#' 
#' @param thin A logical indicating if the data should be thinned with
#'   \code{\link{thin}} so that only cases with contacts should be plotted.
#'
#' @param ttree_shape 'branching' will create a branching transmission
#'   tree. 'rectangle' will create a rectangular shaped plot similar to a
#'   phylogeny that avoids overlapping edges. This argument is only called when
#'   type = 'ttree'.
#'
#' @param ttree_output 'visnetwork' will output to visNetwork, 'ggplot' will
#'   output to ggplot2. visnetwork is recommended for interactive uses, ggplot
#'   for static figure generation and when axis labels and legends are
#'   required. ggplot figures are only available for type = 'ttree'.
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
#' @param position_unlinked A character string indicating where unlinked cases
#'   should be placed. Valid options are 'top', 'bottom' and 'middle', where
#'   'middle' will place unlinked cases according to root_order. This argument
#'   is only called when type = 'ttree'.
#'
#' @param position_dodge A logical indicating if two cases can occupy the same y
#'   coordinate or 'dodge' each other. This argument is only called when type =
#'   'ttree'.
#'
#' @param ... Further arguments to be passed to \code{ggplot}.
#'
#' @return The same output as \code{ggplot2}.
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
vis_ggplot <- function(x,
                       x_axis,
                       node_color = x_axis,
                       node_size = 5,
                       edge_color = NULL,
                       edge_width = 1,
                       edge_linetype = NULL,
                       col_pal = NULL,
                       edge_col_pal = NULL,
                       size_range = c(3, 10),
                       legend = TRUE,
                       thin = TRUE,
                       ttree_shape = 'branching',
                       root_order = 'size',
                       node_order = 'size',
                       reverse_root_order = FALSE,
                       reverse_node_order = FALSE,
                       position_unlinked = 'bottom',
                       position_dodge = FALSE){

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

  ## check node_size (node attribute used for color)
  node_size <- assert_node_size(x, node_size)

  ## check edge_color (edge attribute used for color)
  edge_color <- assert_edge_color(x, edge_color)

  ## check edge_linetype (edge attribute used for linetype)
  edge_linetype <- assert_edge_linetype(x, edge_linetype)

  ## Calculate R_i if needed
  if('R_i' %in% c(node_color, node_size)) {
    x$linelist$R_i <- sapply(x$linelist$id, function(i) sum(x$contacts$from == i, na.rm = TRUE))
  }
  
  ## Check for multiple incoming edges per node
  tab <- table(x$contacts$to)
  culprits <- names(tab)[tab > 1]
  if (length(culprits) != 0) {
    culprits <- paste(culprits, collapse = ", ")
    msg <- sprintf("multiple infectors found for %s . use type = 'network'",
                   culprits)
    stop(msg)
  }

  ## check that x_axis is specified
  if (is.null(x_axis)) {
    stop("x_axis must be specified if type = 'ttree'")
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
                   position_unlinked = position_unlinked)
    
  nodes$x <- coor$x
  nodes$y <- coor$y
    
  ## Move isolated cases to the bottom
  if(position_unlinked == 'top') {
    nodes$y[nodes$y == 0] <- 1
  }

  if(ttree_shape == 'rectangle') {

    ## Get vertical and horizontal edges with correct edge attributes
    df <- get_g_rect(nodes, edges)
    
    inf_ind <- match(edges$from[match(edges$to, nodes$id)], nodes$id)

    df1 <- data.frame(y = nodes$y,
                      yend = nodes$y,
                      x = nodes$x[inf_ind],
                      xend = nodes$x)
    df1 <- cbind(df1, edges[!names(edges) %in% c("from", "to")])
    df1 <- na.omit(df1)
    
    df <- rbind(df, df1)

  } else if(ttree_shape == 'branching') {

    inf_ind <- match(edges$from[match(edges$to, nodes$id)], nodes$id)
    i_ind <- match(edges$to, nodes$id)
    
    df <- data.frame(y = nodes$y[inf_ind],
                     yend = nodes$y[i_ind],
                     x = nodes$x[inf_ind],
                     xend = nodes$x[i_ind])
    df <- cbind(df, edges[!names(edges) %in% c("from", "to")])
    df <- na.omit(df)

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
                          fill = 'black',
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
                             breaks = as.numeric(dates),
                             labels = dates)
      
    } else if(inherits(nodes[[node_size]], 'factor')) {
      
      warning("Mapping factor to size; converting factors to integers.")
      lev <- levels(nodes[[node_size]])
      ind <- as.integer(nodes[[node_size]])
      nodes[[node_size]] <- ind
      size_pal <- scale_size(range = c(size_range[1], size_range[2]),
                             breaks = sort(unique(ind)),
                             labels = lev)
    } else {
      size_pal <- scale_size(range = c(size_range[1], size_range[2]))
    }

    if(is.null(node_color) | missing(node_color)) {
      point <- geom_point(data = nodes,
                          aes_string(x = "x",
                                     y = "y",
                                     size = node_size),
                          fill = 'black',
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

  out <- ggplot(df) +
    geom_segment(aes_string(x = "x",
                            xend = "xend",
                            y = "y",
                            yend = "yend",
                            color = edge_color,
                            linetype = edge_linetype),
                 lineend = 'square',
                 size = edge_width) +
    point +
    col_pal +
    edge_col_pal +
    size_pal +
    theme_minimal() +
    labs(x = x_axis) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

  return(out)
  
}
