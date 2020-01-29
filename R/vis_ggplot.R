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
#' @param ttree_shape "branching" will create a branching transmission
#'   tree. "rectangle" will create a rectangular shaped plot similar to a
#'   phylogeny that avoids overlapping edges.
#'
#' @param root_order A character string indicating which field of the linelist
#'   data is used to vertically order index cases of individual transmission
#'   chains (i.e. the "roots" of the transmission trees). If root_order =
#'   "size", index cases will be ordered by the size of the downstream
#'   transmission chains they generate.
#'
#' @param node_order A character string indicating which field of the linelist
#'   data is used to vertically order nodes in the transmission tree. If
#'   node_order = "size", nodes will be ordered by the size of the downstream
#'   transmission chains they generate.
#'
#' @param reverse_root_order A logical indicating if the ordering of the roots
#'   hsould be reversed. This argument is only called when type = "ttree".
#'
#' @param reverse_node_order A logical indicating if the ordering of the nodes
#'   should be reversed. This argument is only called when type = "ttree".
#'
#' @param lineend Character indicating the lineend to be used for
#'   geom_segment. One of "round", "butt" or "square".
#' 
#' @param unlinked_pos A character string indicating where unlinked cases
#'   should be placed. Valid options are "top", "bottom" and "middle", where
#'   "middle" will place unlinked cases according to root_order. This argument
#'   is only called when type = "ttree".
#'
#' @param position_dodge A logical indicating if two cases can occupy the same y
#'   coordinate or "dodge" each other. This argument is only called when type =
#'   "ttree".
#'
#' @param parent_pos Specify the position of the parent node relative to its
#'   children. Can be one of "middle", "top" or "bottom".
#'
#' @param custom_parent_pos A function specifying the position of children nodes
#'   relative to their parent. This function must accept a single integer `x` as
#'   its only argument, specifying the number of children nodes. It must return
#'   a vector of length x, specifying the position of each child relative to the
#'   parent, where a x > 0 indicates above the parent, x < 0 indicates below the
#'   parent, and x = 0 indicates the same height as the parent.
#'
#' @param y_label A character string which element of the linelist should be
#'   displayed on the y-axis labels. If NULL, no y-axis labels are
#'   provided. Only works when position_dodge = TRUE, otherwise y-coordinates
#'   are not unique.
#'
#' @param node_label A character string which element of the linelist should be
#'   display node labels. If NULL, node labels are provided.
#'
#' @param y_coor Manual specification of y coordinates. Must be a vector with one
#'   y coordinate for each case between 0 and 1.
#'
#' @param igraph_type Alternate tree layouts provided by igraph. Must be one of
#'   "rt" for Reingold-Tilford layout, "sugiyama" for Sugiyama layout or "fr"
#'   for Fruchterman-Reingold layout.
#'
#' @param col_pal A color palette for the nodes. Must be a function accepting a
#'   single number n and returning a vector of n colors.
#'
#' @param edge_col_pal A color palette for the edges. Must be a function accepting a
#'   single number n and returning a vector of n colors.
#'
#' @param ... Additional arguments specified in \code{vis_epicontacts}.
#'
#' @return The same output as \code{ggplot2}.
#'
#' @seealso \code{\link[ggplot2]{ggplot}} in the package \code{ggplot2}.
#'   \code{\link{edges_pal}} and \code{\link{cases_pal}} for color palettes used
#'
#' @importFrom ggplot2 aes_string element_blank geom_point geom_segment ggplot
#'   labs scale_color_gradient scale_color_viridis_d scale_fill_viridis_c
#'   scale_fill_viridis_d scale_size scale_y_continuous scale_color_gradientn
#'   scale_fill_gradientn theme theme_minimal unit scale_fill_manual
#'   scale_fill_continuous scale_color_manual scale_color_continuous geom_text
#'   arrow
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
#' plot(x, method = "ggplot", x_axis = "dt_onset", node_color = "sex")
#' }
#' }
vis_ggplot <- function(x,
                       x_axis,
                       edge_alpha = NULL,
                       ttree_shape = "branching",
                       root_order = "subtree_size",
                       node_order = "subtree_size",
                       reverse_root_order = FALSE,
                       reverse_node_order = FALSE,
                       lineend = c("butt", "round", "square"),
                       unlinked_pos = c("bottom", "top", "middle"),
                       position_dodge = FALSE,
                       parent_pos = c("middle", "top", "bottom"),
                       custom_parent_pos = NULL,
                       y_label = NULL,
                       node_label = NULL,
                       y_coor = NULL,
                       igraph_type = NULL,
                       col_pal = cases_pal,
                       edge_col_pal = edges_pal,
                       ...) {

  ## this will assign the value specified in ... if present, otherwise use the
  ## specified default. A list based method using the assign function looks
  ## neater but causes global binding warnings in check.
  def <- as.list(args(vis_epicontacts))

  ## specify alternative defaults for ggplot vs visnetwork
  def$node_size <- 5
  def$edge_width <- 1
  def$size_range <- c(3, 10)

  ## modify defaults
  args <- list(...)
  node_color <- get_val("node_color", def, args)
  node_size <- get_val("node_size", def, args)
  edge_color <- get_val("edge_color", def, args)
  edge_width <- get_val("edge_width", def, args)
  edge_linetype <- get_val("edge_linetype", def, args)
  NA_col <- get_val("NA_col", def, args)
  size_range <- get_val("size_range", def, args)
  width_range <- get_val("width_range", def, args)
  thin <- get_val("thin", def, args)
  custom_parent_pos <- get_val("custom_parent_pos", def, args)
  legend_max <- get_val("legend_max", def, args)

  parent_pos <- match.arg(parent_pos)
  unlinked_pos <- match.arg(unlinked_pos)
  lineend <- match.arg(lineend)

  ## Remove NAs in contacts and linelist
  x <- x[i = !is.na(x$linelist$id),
         j = !is.na(x$contacts$from) & !is.na(x$contacts$to)]
  
  ## check that x_axis is specified
  if (is.null(x_axis)) {
    stop("x_axis must be specified")
  } else {
    ## test x_axis
    x_axis <- assert_x_axis(x, x_axis)
    ## Remove NAs in x_axis
    x <- x[!is.na(x$linelist[[x_axis]])]
    ## Remove contacts that don't have both nodes in linelist
    x <- thin(x, what = "contacts")
  }

  ## Remove linelist elements that aren't in contacts if thin = TRUE
  if(thin) {
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

  ## check edge_alpha (edge attribute used for alpha)
  edge_alpha <- assert_edge_alpha(x, edge_alpha)

  ## edge width can only be numeric in vis_ggplot
  if(length(edge_width) > 1 | !inherits(edge_width, c("numeric", "integer"))) {
    msg <- paste("edge width must be a single number in vis_ggplot (cannot be",
                "mapped to a variable because scale_size is reserved for node_size)")
    stop(msg)
  }

  ## check y_label
  if(!is.null(y_label)) {
    if(!y_label %in% names(x$linelist)) {
      stop("y_label does not exist in linelist")
    }
    if(!position_dodge) {
      stop("position_dodge must be TRUE if y-axis labels are specifed")
    }
    if(!is.null(igraph_type)) {
      stop("igraph_type cannot be specified with y-axis labels")
    }
  }
  
  ## Calculate R_i if needed
  if("R_i" %in% c(node_color, node_size, node_order, root_order)) {
    x$linelist$R_i <- vapply(x$linelist$id,
                             function(i) sum(x$contacts$from == i, na.rm = TRUE),
                             numeric(1))
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
                     axis_type = "none",
                     parent_pos = parent_pos,
                     custom_parent_pos = custom_parent_pos,
                     method = "ggplot",
                     igraph_type = igraph_type)
    nodes$y <- coor$y
  } else {
    nodes$y <- y_coor
  }
  
  nodes$x <- x$linelist[[x_axis]]
  nodes$subtree_size <- coor$subtree_size

  if(ttree_shape == "rectangle") {

    ## Get vertical and horizontal edges with correct edge attributes
    df <- get_g_rect(nodes, edges)

    ## Get case and parent indices
    i_ind <- match(edges$to, nodes$id)
    inf_ind <- match(edges$from, nodes$id)

    ## Get horizontal edges
    df1 <- data.frame(y = nodes$y[i_ind],
                      yend = nodes$y[i_ind],
                      x = nodes$x[inf_ind],
                      xend = nodes$x[i_ind])
    df1 <- cbind(df1, edges[!names(edges) %in% c("from", "to")])
    if(!is.null(df)) {
      df <- df[apply(df[,1:4], 1, function(xx) !any(is.na(xx))),]
    }
    
    df <- rbind(df, df1)

  } else if(ttree_shape == "branching") {

    i_ind <- match(edges$to, nodes$id)
    inf_ind <- match(edges$from, nodes$id)
    
    df <- data.frame(y = nodes$y[inf_ind],
                     yend = nodes$y[i_ind],
                     x = nodes$x[inf_ind],
                     xend = nodes$x[i_ind])
    df <- cbind(df, edges[!names(edges) %in% c("from", "to")])
    df <- df[apply(df[,1:4], 1, function(xx) !any(is.na(xx))),]

    to_node <- rep(TRUE, nrow(df))

  }

  ## Specifying node color palette for different use cases
  if(!is.null(node_color)) {

    if(inherits(nodes[[node_color]], c("factor", "character"))) {
      
      cols <- fac2col(factor(nodes[, node_color]), col_pal, NA_col, TRUE)

      vals <- cols$leg_col
      names(vals) <- cols$leg_lab
      
      col_pal <- scale_fill_manual(values = vals, na.value = NA_col)

      ## annoying workaround to specify colors for dates
    } else if(inherits(nodes[[node_color]], "Date")) {

      dates <- pretty(nodes[[node_color]])
      numeric_node_color <- as.numeric(nodes[[node_color]])
      node_color <- paste0(node_color, "_")
      nodes[[node_color]] <- numeric_node_color

      if(missing(col_pal)) {
        col_pal <- scale_fill_continuous(breaks = as.numeric(dates),
                                         labels = dates)
      } else {
        cols <- col_pal(10)
        col_pal <- scale_fill_gradientn(colors = cols,
                                        breaks = as.numeric(dates),
                                        labels = dates)
      }
      
    } else if(inherits(nodes[[node_color]], c("numeric", "integer"))) {
      
      if(missing(col_pal)) {
        col_pal <- scale_fill_continuous()
      } else {
        cols <- col_pal(10)
        col_pal <- scale_fill_gradientn(colors = cols)
      }

    }

  } else {

    col_pal <- NULL
    
  }

  ## Specifying edge color palette for different use cases
  if(!is.null(edge_color)) {

    if(inherits(edges[[edge_color]], c("factor", "character"))) {
      
      cols <- fac2col(factor(edges[, edge_color]), edge_col_pal, NA_col, TRUE)

      vals <- cols$leg_col
      names(vals) <- cols$leg_lab
      
      edge_col_pal <- scale_color_manual(values = vals, na.value = NA_col)
      
      ## annoying workaround to specify colors for dates
      ## creates additional column called paste0(edge_color, "_")
    } else if(inherits(edges[[edge_color]], "Date")) {

      dates <- pretty(edges[[edge_color]])
      numeric_edge_color <- as.numeric(edges[[edge_color]])
      edge_color <- paste0(edge_color, "_")
      edges[[edge_color]] <- numeric_edge_color

      if(missing(edge_col_pal)) {
        edge_col_pal <- scale_color_continuous(breaks = as.numeric(dates),
                                               labels = dates,
                                               na.value = NA_col)
      } else {
        cols <- edge_col_pal(10)
        edge_col_pal <- scale_color_gradientn(colors = cols,
                                              breaks = as.numeric(dates),
                                              labels = dates,
                                              na.value = NA_col)
      }
      
    } else if(inherits(edges[[edge_color]], c("numeric", "integer"))) {
      
      if(missing(edge_col_pal)) {
        edge_col_pal <- scale_color_continuous(na.value = NA_col)
      } else {
        cols <- edge_col_pal(10)
        edge_col_pal <- scale_color_gradientn(colors = cols,
                                             na.value = NA_col)
      }

    }

  } else {

    edge_col_pal <- NULL
    
  }

  ## Check node size attribute
  if(inherits(node_size, c("numeric", "integer"))) {

    point <- geom_point(data = nodes,
                        aes_string(x = "x",
                                   y = "y",
                                   fill = node_color),
                        size = node_size,
                        shape = 21)
    
    size_pal <- NULL

  } else {

    if(inherits(nodes[[node_size]], "character")) {
      
      stop("node_size cannot be mapped to character variable")
      
    } else if(inherits(nodes[[node_size]], "Date")) {
      
      dates <- pretty(nodes[[node_size]])
      numeric_node_size <- as.numeric(nodes[[node_size]])
      node_size <- paste0(node_size, "_")
      nodes[[node_size]] <- numeric_node_size
      size_pal <- scale_size(range = c(size_range[1], size_range[2]),
                             breaks = scales::pretty_breaks(as.numeric(dates)),
                             labels = dates)
      
    } else if(inherits(nodes[[node_size]], "factor")) {
      
      warning("Mapping factor to size; converting factors to integers.")
      lev <- levels(nodes[[node_size]])
      ind <- as.integer(nodes[[node_size]])
      node_size <- paste0(node_size, "_")
      nodes[[node_size]] <- ind
      size_pal <- scale_size(range = c(size_range[1], size_range[2]),
                             breaks = scales::pretty_breaks(sort(unique(ind))),
                             labels = lev)
    } else {
      size_pal <- scale_size(range = c(size_range[1], size_range[2]),
                             breaks = scales::pretty_breaks())
    }

    point <- geom_point(data = nodes,
                        aes_string(x = "x",
                                   y = "y",
                                   size = node_size,
                                   fill = node_color),
                        shape = 21)
    
  }

  if(x$directed) {
    arrow <- arrow(length = unit(0.015, "npc"),
                   type = "closed",
                   ends = "last")
  } else {
    arrow <- NULL
  }

  ## If edge_alpha is numeric, use as alpha specification for all edges
  if(inherits(edge_alpha, c("numeric", "integer"))) {
    segment <- geom_segment(aes_string(x = "x",
                                       xend = "xend",
                                       y = "y",
                                       yend = "yend",
                                       color = edge_color,
                                       linetype = edge_linetype),
                            arrow = arrow,
                            alpha = edge_alpha,
                            lineend = lineend,
                            size = edge_width)
  } else {
    segment1 <- geom_segment(aes_string(x = "xmid",
                                        xend = "xend",
                                        y = "ymid",
                                        yend = "yend",
                                        color = edge_color,
                                        linetype = edge_linetype,
                                        alpha = edge_alpha),
                             lineend = lineend,
                             size = edge_width)
    segment2 <- geom_segment(aes_string(x = "x",
                                        xend = "xmid",
                                        y = "y",
                                        yend = "ymid",
                                        color = edge_color,
                                        linetype = edge_linetype,
                                        alpha = edge_alpha),
                             lineend = lineend,
                             arrow = arrow,
                             size = edge_width)
  }

  if(!is.null(node_label)) {
    nodes$y <- nodes$y
    lab <- geom_text(data = nodes,
                     aes_string("x", "y", label = node_label),
                     color = "black",
                     size = 2)
  } else {
    lab <- NULL
  }

  if(!is.null(y_label)) {
    y_scale <- scale_y_continuous(name = NULL,
                                  breaks = sort(coor$y),
                                  minor_breaks = NULL,
                                  labels = x$linelist[[y_label]][order(coor$y)],
                                  expand = c(0.01, 0.01))
    gg_theme <- theme(axis.ticks.y = element_blank(),
                      axis.title.y = element_blank(),
                      panel.grid.minor.y = element_blank())
  } else {
    y_scale <- NULL
    gg_theme <-theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank())
  }

  if(inherits(df$x, "Date")) {
    df$xmid <- as.Date((as.numeric(df$x) + as.numeric(df$xend))/2,
                       origin = "1970-01-01")
  } else {
    df$xmid <- (df$x + df$xend)/2
  }
  df$ymid <- (df$y + df$yend)/2
  
  out <- ggplot(df) +
    segment1 +
    segment2 +
    point +
    lab +
    col_pal +
    edge_col_pal +
    size_pal +
    y_scale +
    theme_minimal() +
    gg_theme +
    labs(x = x_axis)
    
  return(out)
  
}
