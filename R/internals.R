#' Simulated outbreak for vignette
#'
#' @name sim
#' @docType data
#'
#'
"sim"

## assert_xxx functions make basic tests on 'xxx' against the data ('x', an
## epicontacts object), and return a possibly processed 'xxx'.

## author: Thibaut Jombart

assert_node_color <- function(df, node_color, var = "node_color") {
  if(!is.null(df)) {
    if (length(node_color) > 1L) {
      stop(sprintf("'%s' must indicate a single node attribute", var))
    }
    if (is.logical(node_color) && !node_color) {
      node_color <- NULL
    }
    if (!is.null(node_color)) {
      if (is.numeric(node_color)) {
        node_color <- names(df)[node_color]
      }
      if (!node_color %in% c(names(df), 'R_i', 'subtree_size')) {
        msg <- sprintf("%s '%s' is not in the linelist", var, node_color)
        stop(msg)
      }
    }
  }
  return(node_color)
}


assert_node_shape <- function(df, node_shape, var = "node_shape", shapes) {
  if(!is.null(df)) {
    if (length(node_shape) > 1L) {
      stop(sprintf("'%s' must indicate a single node attribute", var))
    }
    if (is.logical(node_shape) && !node_shape) {
      node_shape <- NULL
    }
    if (!is.null(node_shape)) {
      if (is.numeric(node_shape)) {
        node_shape <- names(df)[node_shape]
      }
      if (!node_shape %in% c(names(df), 'R_i', 'subtree_size')) {
        msg <- sprintf("%s '%s' is not in the linelist", var, node_shape)
        stop(msg)
      }
      if (is.null(shapes)) {
        msg <- paste0("'shapes' needed if '", var, "' provided;",
                     "to see codes, node_shape: codeawesome")
        stop(msg)
      }
      culprit <- unique(df[!df[[node_shape]] %in% names(shapes), node_shape])
      culprit <- culprit[!is.na(culprit)]
      if (length(culprit) > 0) {
        stop(paste0("No shape specified for ", paste0("'", culprit, "'", collapse = ", ")))
      }
    }
  }
  return(node_shape)
}


assert_node_size <- function(df, node_size, var = "node_size") {
  if(!is.null(df)) {
    if (length(node_size) > 1L) {
      stop("'%s' must indicate a single node attribute", var)
    }
    if (is.logical(node_size) && !node_size) {
      node_size <- NULL
    }
    if (!is.null(node_size) & !is.numeric(node_size)) {
      if (!node_size %in% c(names(df), 'R_i', 'subtree_size')) {
        msg <- sprintf("%s '%s' is not in the linelist", var, node_size)
        stop(msg)
      }
      if(var == "node_size" & inherits(df[[node_size]], "character")) {
        stop("node_size cannot be mapped to character variable")
      }
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


assert_edge_label <- function(df, edge_label, var = "edge_label") {
  if(!is.null(df)) {
    if (length(edge_label) > 1L) {
      stop(sprintf("'%s' must indicate a single edge attribute", var))
    }
    if (is.logical(edge_label) && !edge_label) {
      edge_label <- NULL
    }
    if (!is.null(edge_label)) {
      if (is.numeric(edge_label)) {
        edge_label <- names(df)[edge_label]
      }
      if (!edge_label %in% names(df)) {
        msg <- sprintf("%s '%s' is not in the contacts", var, edge_label)
        stop(msg)
      }
    }
  }
  return(edge_label)
}


assert_edge_color <- function(df, edge_color, var = "edge_color") {
  if(!is.null(df)) {
    if (length(edge_color) > 1L) {
      stop(sprintf("'%s' must indicate a single edge attribute", var))
    }
    if (is.logical(edge_color) && !edge_color) {
      edge_color <- NULL
    }
    if (!is.null(edge_color)) {
      if (is.numeric(edge_color)) {
        edge_color <- names(df)[edge_color]
      }
      if (!edge_color %in% names(df)) {
        msg <- sprintf("%s '%s' is not in the contacts", var, edge_color)
        stop(msg)
      }
    }
  }
  return(edge_color)
}


assert_edge_linetype <- function(df, edge_linetype, var = "edge_linetype") {
  if(!is.null(df)) {
    if (length(edge_linetype) > 1L) {
      stop(sprintf("'%s' must indicate a single edge attribute", var))
    }
    if (is.logical(edge_linetype) && !edge_linetype) {
      edge_linetype <- NULL
    }
    if (!is.null(edge_linetype)) {
      if (is.numeric(edge_linetype)) {
        edge_linetype <- names(df)[edge_linetype]
      }
      if(!edge_linetype %in% names(df)) {
        msg <- sprintf("%s '%s' is not in the contacts", var, edge_linetype)
        stop(msg)
      }
    }
  }
  return(edge_linetype)
}


assert_edge_width <- function(df, edge_width, var) {
  if(!is.null(df)) {
    if (length(edge_width) > 1L) {
      stop(sprintf("'%s' must indicate a single edge attribute", var))
    }
    if (is.logical(edge_width) && !edge_width) {
      edge_width <- NULL
    }
    if (!is.null(edge_width) & !is.numeric(edge_width)) {
      if(!edge_width %in% names(df)) {
        msg <- sprintf("%s '%s' is not in the contacts", var, edge_width)
        stop(msg)
      }
      if(var == "edge_width" & inherits(df[[edge_width]], "character")) {
        stop("edge_width cannot be mapped to character variable")
      }
    }
  }
  return(edge_width)
}


assert_edge_alpha <- function(x, edge_alpha) {
  if (length(edge_alpha) > 1L) {
    stop("'edge_alpha' must indicate a single edge attribute")
  }
  if (is.logical(edge_alpha) && !edge_alpha) {
    edge_alpha <- NULL
  }
  if (!is.null(edge_alpha)) {
    if (is.numeric(edge_alpha)) {
      edge_alpha <- names(x$contacts)[edge_alpha]
    }

    if (!edge_alpha %in% names(x$contacts)) {
      msg <- sprintf("edge_alpha '%s' is not in the contacts", edge_alpha)
      stop(msg)
    }
  }

  return(edge_alpha)
}


assert_node_order <- function(x, node_order) {

  if (length(node_order) > 1L) {
    stop("'node_order' must indicate a single node attribute")
  }
  if (is.logical(node_order)){
    stop("'node_order' cannot be a logical")
  }
  if (!is.null(node_order)) {
    if (is.numeric(node_order)) {
      node_order <- names(x$linelist)[node_order]
    }

    if (!node_order %in% c(names(x$linelist), 'R_i', 'subtree_size')) {
      msg <- sprintf("node_order '%s' is not in the linelist", node_order)
      stop(msg)
    }
  }

  return(node_order)
}


assert_root_order <- function(x, root_order) {

  if (length(root_order) > 1L) {
    stop("'root_order' must indicate a single node attribute")
  }
  if (is.logical(root_order)){
    stop("'root_order' cannot be a logical")
  }
  if (!is.null(root_order)) {
    if (is.numeric(root_order)) {
      root_order <- names(x$linelist)[root_order]
    }

    if (!root_order %in% c(names(x$linelist), 'R_i', 'subtree_size')) {
      msg <- sprintf("root_order '%s' is not in the linelist", root_order)
      stop(msg)
    }
  }

  return(root_order)
}


assert_rank_contact <- function(x, rank_contact) {

  if (length(rank_contact) > 1L) {
    stop("'rank_contact' must indicate a single edge or node attribute")
  }
  if (is.logical(rank_contact)){
    stop("'rank_contact' cannot be a logical")
  }
  if (!is.null(rank_contact)) {
    if (is.numeric(rank_contact)) {
      rank_contact <- names(x$contacts)[rank_contact]
    }

    if (!rank_contact %in% c(names(x$linelist), names(x$contacts), 'R_i', 'subtree_size')) {
      msg <- sprintf("rank_contact '%s' is not in linelist or contacts", rank_contact)
      stop(msg)
    }
  }

  return(rank_contact)
}


assert_custom_parent_pos <- function(custom_parent_pos) {

  if(!is.null(custom_parent_pos)) {
    if (!is.function(custom_parent_pos)) {
      stop("'custom_parent_pos' must be a function")
    }
    if (length(methods::formalArgs(custom_parent_pos)) != 1L) {
      stop("'custom_parent_pos' must have only one argument")
    }
    if (length(custom_parent_pos(10)) != 10) {
      stop(paste0("custom_parent_pos must be a function of n",
                  " returning a numeric vector of length n"))
    }
  }

  return(custom_parent_pos)

}


assert_x_axis <- function(x, x_axis) {

  if (length(x_axis) > 1L) {
    stop("'x_axis' must indicate a single node attribute")
  }
  if (!is.null(x_axis)) {

    if (is.numeric(x_axis)) {
      x_axis <- names(x$linelist)[x_axis]
    }

    if (!x_axis %in% c(names(x$linelist), 'R_i', 'subtree_size')) {
      msg <- sprintf("'%s' is not in the linelist", x_axis)
      stop(msg)
    }

    if(!inherits(x$linelist[[x_axis]],
                 c("Date", "numeric", "integer", "POSIXct", "POSIXt"))) {
      stop("x_axis must indicate a Date, numeric or integer value")
    }

  }

  return(x_axis)

}


assert_timeline <- function(timeline, x, x_axis) {

  if(!is.null(timeline)) {
    if(!inherits(timeline$start, class(x$linelist[[x_axis]])) |
       !inherits(timeline$end, class(x$linelist[[x_axis]]))) {
      stop("timeline dates must be of the same class as the x_axis")
    }
    ind <- timeline$id %in% get_id(x, "linelist") &
      !is.na(timeline$start) &
      !is.na(timeline$end)
    if(any(!ind)) {
      warning(sprintf("%i timeline row(s) removed as ID not found in linelist or start/end date is NA",
                      sum(!ind)))
    }
    timeline <- as.data.frame(subset(timeline, ind))
  }

  return(timeline)

}



## this is a recursive function calculating various properties of the leaf node
##  -depth is the number of edges from a root node
##  -identify of the root node
## in the case of multiple parent nodes, the edge with the maximum value in
## 'rank_contact' is taken, which is either an edge attribute, or an edge
## attribute calculated from a node attribute by taking the difference in node
## attributes (e.g. difference in times of infection if rank_contact = 't_inf')
get_treestat <- function(i, depth, subtree_size, contacts, linelist, leaf,
                         rank_contact, reverse_rank_contact, leaf_parent) {

  ## identify parent by choosing highest ranked edge
  parents_ind <- which(contacts$to == i)
  parents <- contacts$from[parents_ind]
  if(length(parents) == 0) {
    parent <- NA
  } else if(length(parents) > 1) {
    edge_rank <- contacts[[rank_contact]][parents_ind]
    if(!reverse_rank_contact) {
      parent <- parents[which.max(edge_rank)]
    } else {
      parent <- parents[which.min(edge_rank)]
    }
  } else {
    parent <- parents
  }

  ## store parent of the leaf
  if(is.null(leaf_parent)) leaf_parent <- parent

  ## error if network is cyclical
  if(!is.na(parent) & parent == leaf) {
    stop("pruned network still contains cycles")
  }

  ## return results if root is hit or go one layer higher
  if(is.na(parent) || parent == 0) {
    return(list(depth = depth,
                subtree_size = subtree_size,
                parent = leaf_parent,
                root = i))
  } else {
    parent_ind <- linelist$id == parent
    get_treestat(i = parent,
                 depth = depth + 1,
                 replace(subtree_size, parent_ind, subtree_size[parent_ind] + 1),
                 contacts,
                 contacts,
                 leaf,
                 rank_contact,
                 reverse_rank_contact,
                 leaf_parent)
  }
}



## identify cycles in the network and remove the edge with the lowest rank in
## 'rank_contact'
prune_cycles <- function(i, leaf, contacts, cycle_edges,
                         rank_contact, reverse_rank_contact) {

  ## choose parent with highest ranked edge
  parents <- contacts[which(contacts$to == i),]
  if(nrow(parents) == 0) {
    return(contacts)
  } else if(nrow(parents) > 1) {
    if(!reverse_rank_contact) {
      parent <- parents[which.max(parents[[rank_contact]]),]
    } else {
      parent <- parents[which.min(parents[[rank_contact]]),]
    }
  } else {
    parent <- parents
  }

  ## return contacts if root found
  if(is.na(parent$from) || parent$from == 0) {
    return(contacts)
  }

  ## if this parent exists in the cycle, a cycle has been identified, the lowest
  ## ranked edge is removed and the pruning function is restarted to make sure no other
  ## cycles exist
  if(parent$from %in% cycle_edges$to) {
    cycle_edges <- rbind(cycle_edges, parent)
    edge_remove <- cycle_edges[which.min(cycle_edges[[rank_contact]]),]
    contacts <- contacts[-which(contacts$from == edge_remove$from &
                                contacts$to == edge_remove$to),]
    ## restart loop from leaf with pruned contacts
    contacts <- prune_cycles(leaf, leaf, contacts, NULL,
                             rank_contact, reverse_rank_contact)

  } else {

    ## if no loop, move onwards
    cycle_edges <- rbind(cycle_edges, parent)
    contacts <- prune_cycles(parent$from, leaf, contacts, cycle_edges,
                             rank_contact, reverse_rank_contact)

  }

  return(contacts)

}



## get_child_pos outputs children node positions relative to the parent
## position_dodge specifies if a parent and child can share the same y position
## parent_pos specifies where the parent sits relative to the children
get_child_pos <- function(n_children,
                          parent_pos = 'middle',
                          custom_parent_pos = NULL,
                          position_dodge = FALSE) {

  if(is.null(custom_parent_pos)) {

    if(parent_pos == 'middle') {

      if(n_children %% 2 != 0 & position_dodge) {
        n_children <- n_children + 1
        odd <- TRUE
      } else {
        odd <- FALSE
      }
      sq <- seq(1, 1 + 2*(n_children - 1), 2)
      out <- sq - stats::median(sq)
      if(odd & position_dodge) out <- out[-1]

    } else if(parent_pos == 'bottom') {

      if(position_dodge) {
        out <- 1:n_children
      } else {
        out <- 0:(n_children - 1)
      }

    } else if(parent_pos == 'top') {

      if(position_dodge) {
        out <- (1:n_children)*-1
      } else {
        out <- (0:(n_children - 1)*-1)
      }

    }
  } else {

    out <- custom_parent_pos(n_children)

  }

  return(out)

}



## this function will order the cases in such a manner that parent nodes are
## closest to their children, starting at the leaves of the tree and moving to
## the root. It returns the x and y coordinates of each node.
get_coor <- function(x,
                     x_axis,
                     position_dodge = FALSE,
                     root_order = 'subtree_size',
                     node_order = 'subtree_size',
                     reverse_root_order = FALSE,
                     reverse_node_order = FALSE,
                     rank_contact = x_axis,
                     reverse_rank_contact = FALSE,
                     unlinked_pos = 'bottom',
                     axis_type = c("single", "double", "none"),
                     parent_pos = 'middle',
                     custom_parent_pos = NULL,
                     method = 'ttree',
                     igraph_type = NULL) {

  axis_type <- match.arg(axis_type)

  ## add cluster membership, for use in root clustering
  x <- get_clusters(x)
  linelist <- x$linelist
  contacts <- x$contacts

  ## if rank_contact is a node attribute, calculate an edge attribute for each
  ## contact by taking the difference in node attributes
  if(!rank_contact %in% names(contacts)) {

    if(!rank_contact %in% names(linelist)) {
      stop("rank_contact is not found in linelist or contacts")
    }

    if(!inherits(linelist[[rank_contact]],
                 c("Date", "numeric", "integer", "POSIXct", "POSIXt"))) {
      stop("rank_contact must indicate a Date, numeric or integer value")
    }

    from_ind <- match(contacts$from, linelist$id)
    to_ind <- match(contacts$to, linelist$id)

    weight <- linelist[[rank_contact]][to_ind] - linelist[[rank_contact]][from_ind]
    contacts[[rank_contact]] <- as.numeric(weight)

  }

  N <- nrow(linelist)

  ## depth is the number of generations between a leaf and its root (+ 1)
  depth <- rep(0, N)

  ## parent is the parent node in the 'scaffold' tree (if multiple parents
  ## exist, one of these is selected to build the scaffold tree)
  parent <- root <- rep(NA, N)

  ## subtree size is the number of nodes downstream of a given node
  subtree_size <- rep(1, N)

  ## cycles are removed when building the scaffold tree by recursively removing
  ## the lowest ranked edge in a cycle (as defined by rank_contact)
  contacts_clean <- contacts
  for(i in linelist$id) {
    contacts_clean <- prune_cycles(i,
                                   leaf = i,
                                   contacts = contacts_clean,
                                   cycle_edges = NULL,
                                   rank_contact = rank_contact,
                                   reverse_rank_contact = reverse_rank_contact)
  }

  ## calculate various tree statistics using scaffold tree
  for(i in linelist$id) {
    treestat <- get_treestat(i,
                             depth = 1,
                             subtree_size = subtree_size,
                             contacts = contacts_clean,
                             linelist = linelist,
                             leaf = i,
                             rank_contact = rank_contact,
                             reverse_rank_contact = reverse_rank_contact,
                             leaf_parent = NULL)
    ind <- which(linelist$id == i)
    depth[ind] <- treestat$depth
    parent[ind] <- treestat$parent
    root[ind] <- treestat$root
    subtree_size <- treestat$subtree_size
  }

  ## add subtree size to linelist so that it can be called in node_order
  linelist$subtree_size <- subtree_size

  ## NAs treated as 0 to be safe, though these should be removed beforehand
  contacts$from[is.na(contacts$from)] <- 0

  ## pos is a matrix with the ranking of a node relative to its parent at each
  ## depth. every node inherits the ranking of its parent, ensuring nodes only
  ## split from their parents at the correct depth
  pos <- matrix(0, nrow(linelist), max(depth))

  ## iterate across every depth and split cases at that depth
  for(i in seq_len(max(depth))) {

    ## index of the cases found at a given depth i
    ind <- which(depth == i)

    ## child_pos_add will be the ranking of a child relative to its parent
    child_pos_add <- numeric(length(ind))

    ## non-root cases
    if(i > 1) {

      ## group nodes into siblings with shared parent
      siblings_grouped <- split(ind, parent[ind])

      for(siblings in siblings_grouped) {

        ## get the child position relative to the parent
        child_pos <- get_child_pos(length(siblings), parent_pos, custom_parent_pos, position_dodge)

        ## re-order siblings by order_nodes
        if(!is.null(node_order)) {
          siblings <- siblings[order(linelist[[node_order]][siblings],
                                     decreasing = reverse_node_order)]
        }

        ## add children position according to ordering
        child_pos_add[match(siblings, ind)] <- child_pos

      }

      ## children adopt ranking values from parent
      pos[ind, 1:(i-1)] <- pos[match(parent[ind], linelist$id), 1:(i-1)]

    } else {

      ## this section links roots that are connected by secondary connections
      n_parents <- table(contacts$to)
      multiple_parents <- names(n_parents)[n_parents > 1]
      if(length(multiple_parents) > 0) {

        ## identify cluster, group roots by cluster
        linelist$cluster_member <- as.numeric(linelist$cluster_member)
        root_ind <- match(unique(root), linelist$id)
        linked <- split(linelist$id[root_ind],
                        linelist$cluster_member[root_ind])

        ## get all combinations of linked roots
        get_combn <- function(j) if(length(unique(j)) > 1) t(utils::combn(unique(j), 2))
        linked_roots <- lapply(linked, get_combn)
        linked_roots <- linked_roots[!vapply(linked_roots, is.null, TRUE)]
        if(length(linked_roots) > 0) {
          linked_roots <- do.call(rbind, linked_roots)
        }

      } else {
        linked_roots <- numeric()
      }

      ## get position of roots
      child_pos <- get_child_pos(length(ind), parent_pos, custom_parent_pos, position_dodge)

      if(!is.null(root_order)) {

        ## get initial ordering of roots by root_order
        ord <- order(linelist[[root_order]][ind], decreasing = reverse_root_order)

        ## if linked roots exist, these are placed next to each other. a 'root
        ## of roots' which is highest ranked in root_order is chosen
        if(length(linked_roots) > 0) {

          ## bring the lower ranked root (from root_order) right below the
          ## higher ranked root by subtracting 0.0001
          for(j in seq_len(nrow(linked_roots))) {
            sub_ord <- ord[match(linked_roots[j,], linelist$id[ind])]
            min_ind <- match(linked_roots[j, which.min(sub_ord)], linelist$id[ind])
            max_ind <- match(linked_roots[j, which.max(sub_ord)], linelist$id[ind])
            ord[min_ind] <- ord[max_ind] - 0.0001*j
          }

          ## roots are now re-ordered the roots, considering only the 'roots of
          ## roots', ie the base root of a group of linked roots
          rr <- ord %in% ceiling(ord)

          ## this points to the root of each root
          rr_ind <- match(ceiling(ord), ord[rr])

          ## this gets the rank of the root of roots
          rr_ord <- rank(linelist[[root_order]][ind][rr],
                         ties.method = "first")

          ## this places the roots under the roots of roots
          diff <- ord - ceiling(ord)
          ord <- rr_ord[rr_ind] + diff

          ## get indices and get child positions
          ord <- match(ord, sort(ord))
          child_pos_add <- child_pos[ord]

        } else {

          ## add normal child positions if we don't have linked roots
          child_pos_add[ord] <- child_pos

        }

      } else {

        ## if node order is null, use native ordering in linelist
        child_pos_add <- child_pos

      }

    }

    ## add the child positions at the given depth
    pos[ind, i] <- child_pos_add

  }

  ## this is true if i is ranked higher than j (essentially looks at the
  ## difference in ranking at each depth and choose the first difference)
  is_higher <- function(i, j) {
    comp <- pos[i,] - pos[j,]
    out <- comp[comp != 0][1] > 0
  }

  ## rank all cases by pairwise comparison - this is quite slow for large
  ## networks, there is definitely a quicker way
  comb <- expand.grid(seq_len(nrow(pos)), seq_len(nrow(pos)))
  rel_pos <- matrix(FALSE, nrow = nrow(pos), ncol = nrow(pos))

  rel_pos[as.matrix(comb)] <- apply(comb, 1, function(x) is_higher(x[1], x[2]))
  rel_pos[is.na(rel_pos)] <- FALSE

  ## get global ranking by taking the sum of all pairwise rankings
  abs_pos <- apply(rel_pos, 1, sum)

  ## Get isolated cases and place them as specified by unlinked_pos
  ## If position_dodge, order these by root_order
  contacts <- contacts[contacts$from != 0,]
  unlinked <- which(!linelist$id %in% c(contacts$from, contacts$to))
  if(length(unlinked) > 0) {
    if(position_dodge) {
      unlinked_order <- order(linelist[[root_order]][unlinked],
                              decreasing = reverse_root_order)
      unlinked_to_add <- seq_along(unlinked)
    } else {
      unlinked_order <- seq_along(unlinked)
      unlinked_to_add <- 0
    }

    ## Place nodes at top or bottom by adding 10000 to abs_pos
    if(unlinked_pos == 'top') {
      abs_pos[unlinked[unlinked_order]] <- 10000 + unlinked_to_add
    } else if(unlinked_pos == 'bottom') {
      abs_pos[unlinked[unlinked_order]] <- -10000 + unlinked_to_add
    }
  }

  ## use igraph coordinate calculations instead
  if(!is.null(igraph_type)) {

    net <- igraph::graph_from_data_frame(stats::na.omit(x$contacts),
                                         vertices = x$linelist)

    if(igraph_type == 'rt') {
      abs_pos <- igraph::layout.reingold.tilford(net, root = which(depth == 1))[,1]
    } else if(igraph_type == 'sugiyama') {
      abs_pos <- igraph::layout.sugiyama(net, layers = x$linelist[[x_axis]])$layout[,1]
    } else if(igraph_type == 'fr') {
      abs_pos <- igraph::layout.fruchterman.reingold(net,
                                                 minx = x$linelist[[x_axis]],
                                                 maxx = x$linelist[[x_axis]])[,2]
    } else {
      stop("igraph_type must be one of 'rt', 'sugiyama' or 'fr'")
    }

  }

  ## this orders the cases and adds y_adj positions at the top and bottom to
  ## provide space for the axes
  y_adj <- 2
  if(method == 'ttree') {
    if(axis_type == 'double') {
      y_pos <- match(abs_pos, sort(unique(abs_pos)))
      y_pos = rescale(c(min(y_pos) - y_adj, max(y_pos) + y_adj, y_pos), 0, 1)
    } else if(axis_type == 'single') {
      y_pos <- match(abs_pos, sort(unique(abs_pos)))
      y_pos = rescale(c(min(y_pos) - y_adj, y_pos), 0, 1)
    } else if(axis_type == 'none') {
      y_pos <- match(abs_pos, sort(unique(abs_pos)))
      y_pos = rescale(y_pos, 0, 1)
    }
  } else {
    y_pos <- match(abs_pos, sort(unique(abs_pos)))
    y_pos <- rescale(y_pos, 0, 1)
  }

  ## Also return parent because we need scaffold tree for later
  return(list(y = y_pos, subtree_size = subtree_size))

}



## Get rectangular nodes and edges for visNetwork output. This function returns
## the nodes and edges (with corresponding node and edge attributes) for a
## 'rectangular' shaped transmission tree.
get_v_rect <- function(linelist, contacts) {

  from_ind <- match(contacts$from, linelist$id)
  to_ind <- match(contacts$to, linelist$id)

  ## Get coordinates of infector-infectee pairs
  df <- data.frame(y_inf = linelist$y[from_ind],
                   y_i = linelist$y[to_ind],
                   x_inf = linelist$x[from_ind],
                   x_i = linelist$x[to_ind])

  ## Cases which are not on the same height as their infector will require a new node
  to_keep <- which(apply(df, 1, function(i) i[1] != i[2]))

  if(length(to_keep) > 0) {

    ## These nodes are all hidden - to give matching df size, recycle linelist elements
    new_node <- linelist[rep(1, length(to_keep)),]
    ## Random node ids
    new_node$id <- stats::runif(length(to_keep))
    if(is.character(linelist$id)) {
      new_node$id <- as.character(new_node$id)
    }

    ## These unobserved nodes will sit on the x position of the infector and the
    ## y position of the infectee
    new_node$x <- df$x_inf[to_keep]
    new_node$y <- df$y_i[to_keep]

    ## Edge df providing horizontal edges from leaves to interior node
    new_edge <- data.frame(from = new_node$id,
                           to = contacts$to[to_keep],
                           stringsAsFactors = FALSE)

    tpair <- contacts[to_keep, c("from", "to")]

    new_edge <- cbind(new_edge, contacts[to_keep, !names(contacts) %in% c("from", "to")])
    names(new_edge) <- names(contacts)

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
      anchor_y <- linelist$y[match(nm[i], linelist$id)]
      d_y <- new_node$y[match(ids, new_node$id)]

      ## Identify the next nodes up and down
      vert_nodes <- NULL

      y_above <- d_y[d_y > anchor_y]
      if(length(y_above) > 0) {
        id_above <- ids[d_y > anchor_y]
        id_above_order <- id_above[order(y_above)]

        tmp <- data.frame(from = c(anchor, id_above_order[-length(id_above_order)]),
                          to = id_above_order)
        vert_nodes <- rbind(vert_nodes, tmp)
      }

      y_below <- d_y[d_y < anchor_y]
      if(length(y_below) > 0) {
        id_below <- ids[d_y < anchor_y]
        id_below_order <- id_below[order(y_below, decreasing = TRUE)]
        tmp <- data.frame(from = c(anchor, id_below_order[-length(id_below_order)]),
                          to = id_below_order)
        vert_nodes <- rbind(vert_nodes, tmp)
      }

      ## cbind additional edge data
      vert_nodes <- cbind(vert_nodes,
                          new_edge[match(vert_nodes$to, new_edge$from),
                                   !names(new_edge) %in% c("from", "to")])
      names(vert_nodes) <- names(contacts)

      new_edge <- rbind(new_edge, vert_nodes)

    }

    nodes <- rbind(linelist, new_node)

    old_edge <- contacts[-to_keep,]

    if(nrow(old_edge) > 0) {
      old_edge$to_node <- TRUE
      new_edge$to_node <- new_edge$to %in% linelist$id
      edges <- rbind(old_edge, new_edge)
    } else {
      new_edge$to_node <- new_edge$to %in% linelist$id
      edges <- new_edge
    }

  } else {
    nodes <- linelist
    edges <- contacts
    edges$to_node <- TRUE
  }

  nodes$hidden <- TRUE
  nodes$hidden[match(linelist$id, nodes$id)] <- FALSE

  return(list(nodes = nodes, edges = edges))

}



## Get coordinates for for ggplot geom_segments when network_shape =
## 'rectangle'. Corresponding node and edge attributes are passed on.
get_g_rect <- function(linelist, contacts) {

  from_ind <- match(contacts$from, linelist$id)
  to_ind <- match(contacts$to, linelist$id)

  ## Get coordinates of infector-infectee pairs
  df <- data.frame(y_inf = linelist$y[from_ind],
                   y_i = linelist$y[to_ind],
                   x_inf = linelist$x[from_ind],
                   x_i = linelist$x[to_ind])

  ## Cases which are not on the same height as their infector will require a new node
  new_node_ind <- which(df[,1] != df[,2])

  ## These nodes are all hidden - to give matching df size, recycle linelist elements
  new_node <- linelist[rep(1, length(new_node_ind)),]
  new_node$id <- stats::runif(length(new_node_ind))
  if(is.character(linelist$id)) {
    new_node$id <- as.character(new_node$id)
  }

  ## These unobserved nodes will sit on the x position of the infector and the
  ## y position of the infectee
  new_node$x <- df$x_inf[new_node_ind]
  new_node$y <- df$y_i[new_node_ind]

  ## Group into cases with shared ancestor
  splt <- split(contacts$to[new_node_ind],
                contacts$from[new_node_ind])
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
    vert_nodes <- NULL
    y_above <- d_y[d_y > anchor_y]
    y_below <- d_y[d_y < anchor_y]
    if(length(y_above) != 0) {
      node_up <- ids[which(d_y == min(y_above))]
      vert_nodes <- rbind(vert_nodes,
                          data.frame(from = anchor, to = node_up))
    }
    if(length(y_below) != 0) {
      node_down <- ids[which(d_y == max(y_below))]
      vert_nodes <- rbind(vert_nodes,
                          data.frame(from = anchor, to = node_down))
    }

    while(nrow(vert_nodes) < length(ids)) {
      if(length(y_above != 0)) {
        y_above <- d_y[d_y > d_y[which(ids == node_up)]]
      }
      if(length(y_below != 0)) {
        y_below <- d_y[d_y < d_y[which(ids == node_down)]]
      }
      if(length(y_above) != 0) {
        next_node_up <- ids[which(d_y == min(y_above))]
        vert_nodes <- rbind(vert_nodes,
                            data.frame(from = node_up, to = next_node_up))
        node_up <- next_node_up
      }
      if(length(y_below) != 0) {
        next_node_down <- ids[which(d_y == max(y_below))]
        vert_nodes <- rbind(vert_nodes,
                            data.frame(from = node_down, to = next_node_down))
        node_down <- next_node_down
      }
    }

    ## Construct coordinates from these nodes
    tmp <- data.frame(y = linelist$y[match(vert_nodes$from, linelist$id)],
                      yend = linelist$y[match(vert_nodes$to, linelist$id)])
    tmp$xend <- tmp$x <- linelist$x[match(anchor, linelist$id)]

    ## Find edge attributes going from anchor -> each leaf
    vert_nodes$from <- anchor
    ind <- apply(vert_nodes, 1,
                 function(x) which(x[[1]] == contacts$from &
                                   x[[2]] == contacts$to))

    ## cbind edge attributes - choose the first one if there are multiple
    tmp <- cbind(tmp, contacts[vapply(ind, "[", 1, 1),
                               !names(contacts) %in% c("from", "to"),
                               drop = FALSE])

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
  if(length(unique(x)) == 1L) return(x)
  out <- (x - min(x, na.rm = TRUE))/(diff(range(x, na.rm = TRUE)))*(max_val - min_val)
  out <- out + min_val
  return(out)
}



## This function will return the value of var in args if present, otherwise
## returns the default value in def.
get_val <- function(var, def, args) {
  if(var %in% names(args)) {
    return(eval(args[[var]]))
  } else {
    return(eval(def[[var]]))
  }
}



## Determine if character string is a color
is_color <- function(x) {
  vapply(x, function(e)
    !is.null(tryCatch(grDevices::col2rgb(e), error = function(e) NULL)),
    FALSE)
}



## Join node information from nodes and timelines
join_node_vals <- function(nodes, timeline, node, start, end) {

  node_val <- if(is.null(nodes)) NULL
              else if(is.null(node)) factor(rep("unmapped", nrow(nodes)))
              else factor(nodes[, node])
  start_val <- if(is.null(timeline)) NULL
               else if(is.null(start)) factor(rep("unmapped", nrow(timeline)))
               else factor(timeline[, start])
  end_val <- if(is.null(timeline)) NULL
             else if(is.null(end)) factor(rep("unmapped", nrow(timeline)))
             else factor(timeline[, end])

  lev <- unique(c(levels(node_val), levels(start_val), levels(end_val)))
  lev <- c(lev[lev != "unmapped"], "unmapped")

  ## this maintains factor levels
  out <- factor(c(as.character(node_val),
                  as.character(start_val),
                  as.character(end_val)),
                lev)

  return(out)

}




## Join edge information from edges and timelines
join_edge_vals <- function(edges, timeline, edge, tl_edge) {

  edge_val <- if(is.null(edges)) NULL
              else if(is.null(edge)) factor(rep("unmapped", nrow(edges)))
              else factor(edges[, edge])
  tl_val <- if(is.null(timeline)) NULL
            else if(is.null(tl_edge)) factor(rep("unmapped", nrow(timeline)))
            else factor(timeline[, tl_edge])

  lev <- unique(c(levels(edge_val), levels(tl_val)))
  lev <- c(lev[lev != "unmapped"], "unmapped")

  ## this maintains factor levels
  out <- factor(c(as.character(edge_val), as.character(tl_val)), lev)

  ## coerce to character to stop factor coercion to integer
  return(out)

}




## Join edge information from edges and timelines
join_edge_linetype <- function(edges, timeline, edge, tl_edge) {

  edge_val <- if(is.null(edges)) NULL
              else if(is.null(edge)) rep("unmapped", nrow(edges))
              else edges[, edge]
  tl_val <- if(is.null(timeline)) NULL
            else if(is.null(tl_edge)) rep("unmapped", nrow(timeline))
            else timeline[, tl_edge]

  ## coerce to character to stop factor coercion to integer
  return(c(as.character(edge_val), as.character(tl_val)))

}


## Join edge information from edges and timelines
join_edge_width <- function(edges, timeline, edge, tl_edge) {

  edge_val <- if(is.null(edges)) NULL
              else if(is.null(edge)) rep("unmapped", nrow(edges))
              else if(is.numeric(edge)) rep("numeric", nrow(edges))
              else if(is.character(edges[, edge])) stop("edge_width cannot be mapped to character variable")
              else as.numeric(edges[, edge])

  tl_val <- if(is.null(timeline)) NULL
            else if(is.null(tl_edge)) rep("unmapped_tl", nrow(timeline))
            else if(is.numeric(tl_edge)) rep("numeric_tl", nrow(timeline))
            else if(is.character(timeline[, tl_edge])) stop("tl_edge_width cannot be mapped to character variable")
            else as.numeric(timeline[, tl_edge])

  ## coerce to character to stop factor coercion to integer
  return(c(as.character(edge_val), as.character(tl_val)))

}





## Join node information from nodes and timelines
join_node_sizes <- function(nodes, timeline, node, start, end) {

  node_val <- if(is.null(nodes)) NULL
              else if(is.null(node)) rep("unmapped", nrow(nodes))
              else if(is.numeric(node)) rep("numeric", nrow(nodes))
              else if(is.character(nodes[, node])) stop("node_size cannot be mapped to character variable")
              else as.numeric(nodes[, node])

  start_val <- if(is.null(timeline)) NULL
               else if(is.null(start)) rep("unmapped", nrow(timeline))
               else if(is.numeric(start)) rep("numeric_start", nrow(timeline))
               else if(is.character(timeline[, start])) stop("tl_start_node_size cannot be mapped to character variable")
               else as.numeric(timeline[, start])

  end_val <- if(is.null(timeline)) NULL
               else if(is.null(end)) rep("unmapped", nrow(timeline))
               else if(is.numeric(end)) rep("numeric_end", nrow(timeline))
               else if(is.character(timeline[, end])) stop("tl_end_node_size cannot be mapped to character variable")
               else as.numeric(timeline[, end])

  return(c(as.character(node_val), as.character(start_val), as.character(end_val)))

}


## check whether a and b are the same (both null or the same)
null_or_same <- function(a, b) (!is.null(a) && !is.null(b) && a == b) | is.null(c(a, b))
