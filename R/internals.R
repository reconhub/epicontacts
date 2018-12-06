
## assert_xxx functions make basic tests on 'xxx' against the data ('x', an
## epicontacts object), and return a possibly processed 'xxx'.

## author: Thibaut Jombart

assert_node_color <- function(x, node_color) {
  if (length(node_color) > 1L) {
    stop("'node_color' must indicate a single node attribute")
  }
  if (is.logical(node_color) && !node_color) {
    node_color <- NULL
  }
  if (!is.null(node_color)) {
    if (is.numeric(node_color)) {
      node_color <- names(x$linelist)[node_color]
    }

    if (!node_color %in% c(names(x$linelist), 'R_i')) {
      msg <- sprintf("node_color '%s' is not in the linelist", node_color)
      stop(msg)
    }
  }

  return(node_color)
}






assert_node_shape <- function(x, node_shape) {

  if (length(node_shape) > 1L) {
    stop("'node_shape' must indicate a single node attribute")
  }
  if (is.logical(node_shape) && !node_shape) {
    node_shape <- NULL
  }
  if (!is.null(node_shape)) {
    if (is.numeric(node_shape)) {
      node_shape <- names(x$linelist)[node_shape]
    }

    if (!node_shape %in% c(names(x$linelist), 'R_i')) {
      msg <- sprintf("node_shape '%s' is not in the linelist", node_shape)
      stop(msg)
    }
  }

  return(node_shape)
}






assert_node_size <- function(x, node_size) {

  if (length(node_size) > 1L) {
    stop("'node_size' must indicate a single node attribute")
  }
  if (is.logical(node_size) && !node_size) {
    node_size <- NULL
  }
  if (!is.null(node_size)) {
    if (!is.numeric(node_size) & !node_size %in% c(names(x$linelist), "R_i")) {
      msg <- sprintf("node_size '%s' is not in the linelist", node_size)
      stop(msg)
    }
  }

  return(node_size)
}





assert_annot <- function(x, annot) {
  if (is.logical(annot) && sum(annot) == 0L) {
    annot <- NULL
  }
  if (!is.null(annot)) {
    if (is.numeric(annot) || is.logical(annot)) {
      annot <- names(x$linelist)[annot]
    }

    if (!all(annot %in% names(x$linelist))) {
      culprits <- annot[!annot %in% names(x$linelist)]
      culprits <- paste(culprits, collapse = ", ")
      msg <- sprintf("Annot '%s' is not in the linelist", culprits)
      stop(msg)
    }
  }

  return(annot)
}






assert_edge_label <- function(x, edge_label) {
  if (length(edge_label) > 1L) {
    stop("'edge_label' must indicate a single edge attribute")
  }
  if (is.logical(edge_label) && !edge_label) {
    edge_label <- NULL
  }
  if (!is.null(edge_label)) {
    if (is.numeric(edge_label)) {
      edge_label <- names(x$contacts)[edge_label]
    }

    if (!edge_label %in% names(x$contacts)) {
      msg <- sprintf("edge_label '%s' is not in the contacts", edge_label)
      stop(msg)
    }
  }

  return(edge_label)
}






assert_edge_color <- function(x, edge_color) {
  if (length(edge_color) > 1L) {
    stop("'edge_color' must indicate a single edge attribute")
  }
  if (is.logical(edge_color) && !edge_color) {
    edge_color <- NULL
  }
  if (!is.null(edge_color)) {
    if (is.numeric(edge_color)) {
      edge_color <- names(x$contacts)[edge_color]
    }

    if (!edge_color %in% names(x$contacts)) {
      msg <- sprintf("edge_color '%s' is not in the contacts", edge_color)
      stop(msg)
    }
  }

  return(edge_color)
}







assert_edge_linetype <- function(x, edge_linetype) {
  if (length(edge_linetype) > 1L) {
    stop("'edge_linetype' must indicate a single edge attribute")
  }
  if (is.logical(edge_linetype) && !edge_linetype) {
    edge_linetype <- NULL
  }
  if (!is.null(edge_linetype)) {
    if (is.numeric(edge_linetype)) {
      edge_linetype <- names(x$contacts)[edge_linetype]
    }
    if(is.character(edge_linetype) & !edge_linetype %in% names(x$contacts)) {
      msg <- sprintf("edge_linetype '%s' is not in the contacts", edge_linetype)
      stop(msg)
    }
  }

  return(edge_linetype)
}






assert_edge_width <- function(x, edge_width) {
  if (length(edge_width) > 1L) {
    stop("'edge_width' must indicate a single edge attribute")
  }
  if (is.logical(edge_width) && !edge_width) {
    edge_width <- NULL
  }
  if (!is.null(edge_width)) {
    if(is.character(edge_width) & !edge_width %in% names(x$contacts)) {
      msg <- sprintf("edge_width '%s' is not in the contacts", edge_width)
      stop(msg)
    }
  }

  return(edge_width)
  
}






## Recursive function to identify how many layers deep a case is and who its
## root is. Leaf indicates the node you start with.
get_treestat <- function(i, depth, clust_size, contacts, leaf) {
  infector <- contacts$from[which(contacts$to == i)]
  if(length(infector) == 0) {
    infector <- NA
  }
  ## Break loop
  if(!is.na(infector) & infector == leaf) {
    stop("type = 'ttree' does not work with cyclical networks. use type = 'network'")
  }
  if(is.na(infector) | infector == 0) {
    return(list(depth = depth, clust_size = clust_size))
  } else {
    depth <- depth + 1
    inf_ind <- which(contacts$to == infector)
    clust_size[inf_ind] <- clust_size[inf_ind] + 1
    get_treestat(infector, depth, clust_size, contacts, leaf)
  }
}






## This function will order the cases in such a manner that the infector is
## closest to its infectees, starting at the leaves of the tree and moving to
## the root. It returns the x and y coordinates of each case.
get_coor <- function(x,
                     x_axis = x_axis,
                     position_dodge = FALSE,
                     root_order = 'size',
                     node_order = NULL,
                     reverse_root_order = FALSE,
                     reverse_node_order = TRUE,
                     position_unlinked = 'bottom') {

  ## Position_dodge specifies if two cases can occupy the same y coordinate
  get_split <- function(len) {
    if(len %% 2 != 0 & position_dodge) {
      len <- len + 1
      odd <- TRUE
    } else {
      odd <- FALSE
    }
    sq <- seq(1, 1 + 2*(len - 1), 2)
    out <- sq - median(sq)
    if(odd & position_dodge) out <- out[-1]
    return(out)
  }

  linelist <- x$linelist
  contacts <- x$contacts

  N <- nrow(linelist)
  depth <- rep(0, N)
  clust_size <- rep(1, N)
  for(i in linelist$id) {
    treestat <- get_treestat(i, 1, clust_size, contacts, i)
    depth[which(linelist$id == i)] <- treestat$depth
    clust_size <- treestat$clust_size
  }

  contacts$from[is.na(contacts$from)] <- 0

  ## mat is a matrix with the added y location for each depth, for each case
  mat <- matrix(0, nrow(linelist), max(depth))

  ## prop is the proportion of total y space (1) at each depth
  prop <- numeric(max(depth))

  for(i in seq_len(max(depth))) {

    ## index of the cases found at a given depth i
    ind <- which(depth == i)

    to_add <- numeric(length(ind))
    
    ## prop at depth i is prop at depth i - 1 divided by the number of cases at
    ## depth i (plus one)
    prop[i] <- ifelse(length(prop[i-1]) == 0, 1, prop[i-1])/(length(ind) + 1)
    
    ## Analyse non-root cases
    if(i > 1) {

      ## Group cases by infector
      grouped <- split(ind, contacts$from[ind])
      
      for(y in grouped) {

        ## Get the splitting at each infector
        splt <- get_split(length(y))

        ## Re-order nodes by order_nodes
        if(!is.null(node_order)) {

          if(node_order == 'size') {

            y <- y[order(clust_size[y], decreasing = reverse_node_order)]
            
          } else {
            
            y <- y[order(linelist[[node_order]][y], decreasing = reverse_node_order)]

          }
          
        }

        ## Add splitting to ordered nodes
        to_add[match(y, ind)] <- splt
        
      }
      
      ## Make cases adopt values from infectors
      mat[ind, 1:(i-1)] <- mat[match(contacts$from[ind], linelist$id), 1:(i-1)]
      
    } else {

      splt <- get_split(length(ind))
      
      if(!is.null(root_order)) {
        
        if(root_order == 'size') {
          
          to_add[order(clust_size[ind], decreasing = reverse_root_order)] <- splt
          
        } else {

          to_add[order(linelist[[root_order]][ind], decreasing = reverse_root_order)] <- splt
          
        }
        
      } else {

        to_add <- splt
        
      }

    }
    
    mat[ind, i] <- to_add*prop[i]#/2
    
  }

  ## Get the numeric y value
  val <- apply(mat, 1, sum)
  
  ## Get isolated cases and put them on the bottom
  if(position_unlinked == 'top') {
    val[depth == 1 & clust_size == 1] <- 1000
  } else if(position_unlinked == 'bottom') {
    val[depth == 1 & clust_size == 1] <- -1000
  }
  
  ## This orders the cases
  y_pos <- match(val, sort(unique(val)))
  y_pos <- y_pos/max(y_pos)
  
  return(data.frame(x = x$linelist[[x_axis]], y = y_pos))
  
}






## Get rectangular nodes and edges for visNetwork output. This function returns
## the nodes and edges (with corresponding node and edge attributes) for a
## 'rectangular' shaped transmission tree.
get_v_rect <- function(linelist, contacts) {

  ## Get linelist index of infectors
  inf_ind <- match(contacts$from[match(linelist$id, contacts$to)], linelist$id)

  ## Get coordinates of infector-infectee pairs
  df <- data.frame(y_inf = linelist$y[inf_ind],
                   y_i = linelist$y,
                   x_inf = linelist$x[inf_ind],
                   x_i = linelist$x)

  ## Cases which are not on the same height as their infector will require a new node
  to_keep <- which(apply(df, 1, function(i) i[1] != i[2]))

  ## These nodes are all hidden - to give matching df size, recycle linelist elements
  new_node <- linelist[rep(1, length(to_keep)),]
  new_node$id <- (nrow(linelist)+1):(nrow(linelist)+length(to_keep))
  if(is.character(linelist$id)) {
    new_node$id <- as.character(new_node$id)
  }

  ## These unobserved nodes will sit on the x position of the infector and the
  ## y position of the infectee
  new_node$x <- df$x_inf[to_keep]
  new_node$y <- df$y_i[to_keep]

  ## Edge df providing horizontal edges from leaves to interior node
  new_edge <- data.frame(from = new_node$id,
                         to = linelist$id[to_keep],
                         stringsAsFactors = FALSE)

  tpair <- cbind(contacts$from[match(new_edge$to, contacts$to)],
                 new_edge$to)
  
  match_ind <- apply(tpair, 1, function(x) which(x[[1]] == contacts$from &
                                                 x[[2]] == contacts$to))

  new_edge <- cbind(new_edge, contacts[match_ind, !names(contacts) %in% c("from", "to")])
  
  
  ## Group into cases with shared ancestor
  splt <- split(new_edge$from, contacts$from[to_keep])
  nm <- names(splt)
  for(i in seq_along(splt)) {
    
    ## y position of new_node
    anchor <- nm[i]

    ## Match classes of unobserved and observed nodes by re-converting to
    ## numeric if need be
    if(is.numeric(linelist$id)) {
      anchor <- as.numeric(anchor)
    }
    
    ids <- splt[[i]]
    y <- linelist$y[match(nm[i], linelist$id)]
    yy <- new_node$y[match(ids, new_node$id)]

    ## Identify the next nodes up and down
    node_up <- ids[which(yy == min(yy[yy > y]))]
    node_down <- ids[which(yy == max(yy[yy < y]))]
    vert_nodes <- data.frame(from = c(anchor, anchor),
                             to = c(node_up, node_down))

    while(nrow(vert_nodes) < length(ids)) {
      next_node_up <- ids[which(yy == min(yy[yy > yy[which(ids == node_up)]]))]
      next_node_down <- ids[which(yy == max(yy[yy < yy[which(ids == node_down)]]))]
      vert_nodes <- rbind(vert_nodes,
                          data.frame(from = c(node_up, node_down),
                                     to = c(next_node_up, next_node_down)))
      node_up <- next_node_up
      node_down <- next_node_down
    }

    ## cbind additional edge data
    vert_nodes <- cbind(vert_nodes,
                        new_edge[match(vert_nodes$to, new_edge$from),
                                 !names(new_edge) %in% c("from", "to")])
    
    new_edge <- rbind(new_edge, vert_nodes)
    
  }
  
  nodes <- rbind(linelist, new_node)
  edges <- rbind(contacts[-to_keep,], new_edge)

  nodes$hidden <- TRUE
  nodes$hidden[match(linelist$id, nodes$id)] <- FALSE

  return(list(nodes = nodes, edges = edges))
  
}






## Get coordinates for for ggplot geom_segments when ttree_shape =
## 'rectangle'. Corresponding node and edge attributes are passed on.
get_g_rect <- function(linelist, contacts) {

  ## Get linelist index of infectors
  inf_ind <- match(contacts$from[match(linelist$id, contacts$to)], linelist$id)

  ## Get coordinates of infector-infectee pairs
  df <- data.frame(y_inf = linelist$y[inf_ind],
                   y_i = linelist$y,
                   x_inf = linelist$x[inf_ind],
                   x_i = linelist$x)

  ## Cases which are not on the same height as their infector will require a new node
  new_node_ind <- which(apply(df, 1, function(i) i[1] != i[2]))

  ## These nodes are all hidden - to give matching df size, recycle linelist elements
  new_node <- linelist[rep(1, length(new_node_ind)),]
  new_node$id <- (nrow(linelist)+1):(nrow(linelist)+length(new_node_ind))
  if(is.character(linelist$id)) {
    new_node$id <- as.character(new_node$id)
  }
  
  ## These unobserved nodes will sit on the x position of the infector and the
  ## y position of the infectee
  new_node$x <- df$x_inf[new_node_ind]
  new_node$y <- df$y_i[new_node_ind]

  ## Group into cases with shared ancestor
  splt <- split(linelist$id[new_node_ind], contacts$from[new_node_ind])
  nm <- names(splt)
  out <- NULL
  
  for(i in seq_along(splt)) {
    
    ## anchor is the common ancestor of all cases in splt[[i]]
    anchor <- nm[i]

    ## Match classes of unobserved and observed nodes by re-converting to
    ## numeric if need be
    if(is.numeric(linelist$id)) {
      anchor <- as.numeric(anchor)
    }

    ## ids of cases sharing anchor as ancestor
    ids <- splt[[i]]

    ## y coordinates of anchor
    anchor_y <- linelist$y[match(anchor, linelist$id)]

    ## y coordinates of the infectees of anchor
    d_y <- linelist$y[match(ids, linelist$id)]

    ## Identify the next nodes up and down
    node_up <- ids[which(d_y == min(d_y[d_y > anchor_y]))]
    node_down <- ids[which(d_y == max(d_y[d_y < anchor_y]))]

    ## Get coordinates of the nodes above and below anchor
    vert_node <- data.frame(from = c(anchor, anchor),
                            to = c(node_up, node_down))

    ## Iterate until a node has been added for each case in ids
    while(nrow(vert_node) < length(ids)) {
      next_node_up <- ids[which(d_y == min(d_y[d_y > d_y[which(ids == node_up)]]))]
      next_node_down <- ids[which(d_y == max(d_y[d_y < d_y[which(ids == node_down)]]))]
      vert_node <- rbind(vert_node,
                         data.frame(from = c(node_up, node_down),
                                    to = c(next_node_up, next_node_down)))
      node_up <- next_node_up
      node_down <- next_node_down
    }

    ## Construct coordinates from these nodes
    tmp <- data.frame(y = linelist$y[match(vert_node$from, linelist$id)],
                      yend = linelist$y[match(vert_node$to, linelist$id)])
    tmp$xend <- tmp$x <- linelist$x[match(anchor, linelist$id)]

    ## Find edge attributes going from anchor -> each leaf
    vert_node$from <- anchor
    ind <- apply(vert_node, 1, function(x) which(x[[1]] == contacts$from &
                                                 x[[2]] == contacts$to))

    ## cbind edge attributes
    tmp <- cbind(tmp, contacts[ind, !names(contacts) %in% c("from", "to")])
    
    out <- rbind(out, tmp)

  }

  return(out)
  
}






## Extract numeric from string to avoid stringr dependency
extr_num <- function(x) {
  grg <- gregexpr('[0-9]', x)[[1]]
  return(as.numeric(substr(x, grg[[1]][1], length(grg))))
}






## Rescale a vector of numerics to min and max
rescale <- function(x, min_val = 0, max_val = 1) {
  out <- (x - min(x))/(diff(range(x)))*(max_val - min_val)
  out <- out + min_val
}
