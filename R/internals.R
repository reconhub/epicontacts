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

    if (!node_color %in% c(names(x$linelist), 'R_i', 'subtree_size')) {
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

    if (!node_shape %in% c(names(x$linelist), 'R_i', 'subtree_size')) {
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
    if (!is.numeric(node_size) & !node_size %in% c(names(x$linelist), 'R_i', 'subtree_size')) {
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






## Recursive function to identify how many layers deep a case is and who its
## root is. Leaf indicates the node you start with. With multiple infectors,
## choose the link with the maximum value in 'rank_contact', which is either an
## edge attribute, or an edge attribute calculated from a node attribute by
## taking the difference in node attributes (e.g. difference in times of
## infection if rank_contact = 't_inf').
get_treestat <- function(i, depth, subtree_size, contacts, linelist, leaf,
                         rank_contact, reverse_rank_contact, infector_keep, root) {

  ind <- which(contacts$to == i)
  infector <- contacts$from[ind]

  ## To remove cycles, we store all edges in a given function call - if we hit
  ## upon a loop, we remove the edge with the lowest score in rank_contact by
  ## directly editing the contacts dataframe. 

  if(length(infector) == 0) {
    infector <- NA
  } else if(length(infector) > 1) {
#    ll_rank <- linelist[[rank_contact]][match(infector, linelist$id)]
    ll_rank <- contacts[[rank_contact]][ind]
    if(!reverse_rank_contact) {
      infector <- infector[which.max(ll_rank)]
    } else {
      infector <- infector[which.min(ll_rank)]
    }
  }
  ## This is the infector of the *leaf* - have to start the algorithm with
  ## infector_keep = NULL so that we know when it's been assigned
  if(is.null(infector_keep)) infector_keep <- infector
  ## Break loop if cyclical
  if(!is.na(infector) & infector == leaf) {
    stop("type = 'ttree' does not work with cyclical networks. use type = 'network'")
  }
  if(is.na(infector) || infector == 0) {
    return(list(depth = depth, subtree_size = subtree_size, infector = infector_keep, root = i)) 
  } else {
    depth <- depth + 1
    inf_ind <- which(linelist$id == infector)
    subtree_size[inf_ind] <- subtree_size[inf_ind] + 1
    get_treestat(infector, depth, subtree_size, contacts, linelist, leaf,
                 rank_contact, reverse_rank_contact, infector_keep, NULL)
  }
}



## Identify cycles in the network and remove the edge with the lowest rank in
## 'rank_contact'
clean_cycles <- function(i, leaf, contacts, cycle_elements,
                         rank_contact, reverse_rank_contact) {

  incoming_edge <- contacts[which(contacts$to == i),]

  if(nrow(incoming_edge) == 0) {
    return(contacts)
  } else if(nrow(incoming_edge) > 1) {
    if(!reverse_rank_contact) {
      to_keep <- incoming_edge[which.max(incoming_edge[[rank_contact]]),]
    } else {
      to_keep <- incoming_edge[which.min(incoming_edge[[rank_contact]]),]
    }
  } else {
    to_keep <- incoming_edge
  }

  if(is.na(to_keep$from) || to_keep$from == 0) {
    return(contacts)
  }

  ## Does this new infector exist in our cycle?  If yes, we have found a cycle
  ## and we need to remove the weakest link. We then need to restart the loop to
  ## make sure no other cycles exist, using the modified contacts
  if(to_keep$from %in% cycle_elements$to) {
    cycle_elements <- rbind(cycle_elements, to_keep)
    edge_remove <- cycle_elements[which.min(cycle_elements[[rank_contact]]),]
    ind_remove <- which(contacts$from == edge_remove$from &
                        contacts$to == edge_remove$to)
    contacts <- contacts[-ind_remove,]

    ## Restart loop from leaf with updated contacts
    contacts <- clean_cycles(leaf, leaf, contacts, NULL,
                             rank_contact, reverse_rank_contact)
    
    ## If no loop, move onwards
  } else {
    
    cycle_elements <- rbind(cycle_elements, to_keep)
    contacts <- clean_cycles(to_keep$from, leaf, contacts, cycle_elements,
                             rank_contact, reverse_rank_contact)
    
  }

  return(contacts)

}





## Get all roots of a given case (i.e. if a case has multiple infectors, travel
## down all branches and identify every root)
get_all_roots <- function(i, root, contacts, linelist, leaf) {

  infector <- contacts$from[which(contacts$to == i)]
  if(length(infector) == 0) {
    infector <- NA
  }

  
  if(length(infector) == 1 & any(is.na(infector)) |
     length(infector) == 1 & any(infector == 0)) {
      return(i)
  }

  ## If it is an import (infector = 0) and has anothe infector, ignore import
  for(j in infector[infector != 0]) {
    
    ## Break loop if cyclical
    if(j == leaf) {
      stop("type = 'ttree' does not work with cyclical networks. use type = 'network'")
    }

    sub_root <- get_all_roots(j, root = root, contacts, linelist, leaf)
    root <- c(root, sub_root)
    
  }

  return(root)
  
}






## This function will order the cases in such a manner that the infector is
## closest to its infectees, starting at the leaves of the tree and moving to
## the root. It returns the x and y coordinates of each case.
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

  ## Get split defines how children nodes are 'split' relative to their parent
  ## Position_dodge specifies if a parent and child can share the same y position
  ## split_type specifies where the parent sits relative to the children
  get_split <- function(len, parent_pos = 'middle', custom_parent_pos = NULL) {

    if(is.null(custom_parent_pos)) {
      if(parent_pos == 'middle') {
        if(len %% 2 != 0 & position_dodge) {
          len <- len + 1
          odd <- TRUE
        } else {
          odd <- FALSE
        }
        sq <- seq(1, 1 + 2*(len - 1), 2)
        out <- sq - stats::median(sq)
        if(odd & position_dodge) out <- out[-1]
      } else if(parent_pos == 'bottom') {
        if(position_dodge) {
          out <- 1:len
        } else {
          out <- 0:(len - 1)
        }
      } else if(parent_pos == 'top') {
        if(position_dodge) {
          out <- (1:len)*-1
        } else {
          out <- (0:(len - 1)*-1)
        }
      }
    } else {
      out <- custom_parent_pos(len)
    }

    return(out)
  }

  axis_type <- match.arg(axis_type)

  ## Add cluster membership, for use in root clustering
  x$linelist$id <- as.character(x$linelist$id)
  x$contacts$from <- as.character(x$contacts$from)
  x$contacts$to <- as.character(x$contacts$to)
  x <- get_clusters(x)
  linelist <- x$linelist
  contacts <- x$contacts
  
  ## If rank_contact is a node attribute, calculate an edge attribute for each
  ## contact by taking the difference in node attributes
  if(!rank_contact %in% names(contacts)) {
    
    if(!rank_contact %in% names(linelist)) {
      stop("rank_contact is not found in linelist or contacts")
    }

    if(!inherits(linelist[[rank_contact]],
                 c("Date", "numeric", "integer", "POSIXct", "POSIXt"))) {
      stop("rank_contact must indicate a Date, numeric or integer value")
    }

    mtch_from <- match(contacts$from, linelist$id)
    mtch_to <- match(contacts$to, linelist$id)
    
    weight <- linelist[[rank_contact]][mtch_to] - linelist[[rank_contact]][mtch_from]
    weight <- as.numeric(weight)
    contacts[[rank_contact]] <- weight

  }

  N <- nrow(linelist)

  ## Depth is the number of generations between a leaf and its root (+ 1)
  depth <- rep(0, N)

  ## Infector is the parent node in the 'scaffold' tree (i.e. if we have
  ## multiple incoming edges, we will choose one of these infectors and build
  ## the scaffold tree using that infector)
  infector <- root <- rep(NA, N)

  ## Subtree size is the number of nodes downstream of a given node
  subtree_size <- rep(1, N)

  ## We remove cycles when building the scaffold tree. We do so by recursively
  ## removing the weakest edge in a cycle (as defined by rank_contact)
  contacts_clean <- contacts
  
  for(i in linelist$id) {
    contacts_clean <- clean_cycles(i,
                                   leaf = i,
                                   contacts = contacts_clean,
                                   cycle_elements = NULL,
                                   rank_contact = rank_contact,
                                   reverse_rank_contact = reverse_rank_contact)
  }

  ## Calculate various tree statistics using scaffold tree
  for(i in linelist$id) {
    treestat <- get_treestat(i,
                             depth = 1,
                             subtree_size = subtree_size,
                             contacts = contacts_clean,
                             linelist = linelist,
                             leaf = i,
                             rank_contact = rank_contact,
                             reverse_rank_contact = reverse_rank_contact,
                             infector_keep = NULL,
                             root = NULL)

    ind <- which(linelist$id == i) 
    depth[ind] <- treestat$depth
    infector[ind] <- treestat$infector
    root[ind] <- treestat$root
    subtree_size <- treestat$subtree_size
  }

  ## Add cluster size to linelist so that it can be called in node_order /
  ## root_order This will overwrite a node attribute called size, if it exists
  ## (but only within this function)
  linelist$subtree_size <- subtree_size

  ## NAs treated as 0 to be safe, though these should be removed beforehand
  contacts$from[is.na(contacts$from)] <- 0

  ## mat is a matrix with the ranking of a node relative to its parent, at each
  ## depth. Every node inherits the ranking of its parent, ensuring nodes only
  ## split from their parents at the correct depth.
  mat <- matrix(0, nrow(linelist), max(depth))

  ## We iterate through every depth value and split cases at that depth
  for(i in seq_len(max(depth))) {

    ## index of the cases found at a given depth i
    ind <- which(depth == i)

    ## to_add will be the ranking of a child relative to its parent
    to_add <- numeric(length(ind))

    ## Analyse non-root cases
    if(i > 1) {

      ## Group cases by single infector identified in scaffold tree
      grouped <- split(ind, infector[ind])
      
      for(y in grouped) {

        ## Get the splitting at each infector
        splt <- get_split(length(y), parent_pos, custom_parent_pos)

        ## Re-order nodes by order_nodes
        if(!is.null(node_order)) {
          
          y <- y[order(linelist[[node_order]][y], decreasing = reverse_node_order)]
          
        }

        ## Add splitting to ordered nodes
        to_add[match(y, ind)] <- splt
        
      }

      ## Children adopt ranking values from parent in scaffold tree
      mat[ind, 1:(i-1)] <- mat[match(infector[ind], linelist$id), 1:(i-1)]
      
    } else {

      ## We need to link roots that are connected by secondary connections
      tab <- table(contacts$to)
      multiple_inf <- names(tab)[tab>1]
      if(length(multiple_inf) > 0) {

        ## Identify cluster, group roots by cluster
        linelist$cluster_member <- as.numeric(linelist$cluster_member)
        root_ind <- match(unique(root), linelist$id)
        linked <- split(linelist$id[root_ind],
                        linelist$cluster_member[root_ind])

        ## Get all combinations of linked roots
        get_combn <- function(j) if(length(unique(j)) > 1) t(utils::combn(unique(j), 2))
        linked_roots <- lapply(linked, get_combn)
        linked_roots <- linked_roots[!vapply(linked_roots, is.null, TRUE)]
        if(length(linked_roots) > 0) {
          linked_roots <- do.call(rbind, linked_roots)
        }

      } else {
        linked_roots <- numeric()
      }

      ## Get splitting of roots
      splt <- get_split(length(ind), parent_pos, custom_parent_pos)
      
      if(!is.null(root_order)) {

        ## Get initiail ordering of roots by root_order
        ord <- order(linelist[[root_order]][ind], decreasing = reverse_root_order)

        ## If we have linked roots, we need to place them next to each other. We
        ## choose a 'root of roots' that is higest ranked in root_order
        if(length(linked_roots) > 0) {
          
          ## Bring the lower ranked root (from root_order) right below the
          ## higher ranked root by subtracting 0.0001
          for(j in seq_len(nrow(linked_roots))) {
            sub_ord <- ord[match(linked_roots[j,], linelist$id[ind])]
            min_ind <- match(linked_roots[j, which.min(sub_ord)], linelist$id[ind])
            max_ind <- match(linked_roots[j, which.max(sub_ord)], linelist$id[ind])
            ord[min_ind] <- ord[max_ind] - 0.0001*j
          }

          ## We now need to re-order the roots, considering only the 'roots of
          ## roots', ie the base root of a group of linked roots
          root_root <- ord %in% ceiling(ord)

          ## This points to the root of each root
          mtch <- match(ceiling(ord), ord[root_root])

          ## This gets the rank of the root of roots
          rr_ord <- rank(linelist[[root_order]][ind][root_root],
                         ties.method = "first")

          ## This places the roots under the roots of roots
          diff <- ord - ceiling(ord)
          ord <- rr_ord[mtch] + diff

          ## Get indices and get splitting
          ord <- match(ord, sort(ord))
          to_add <- splt[ord]
          
        } else {

          ## Add normal splitting if we don't have linked roots
          to_add[ord] <- splt
          
        }

        
      } else {

        ## If node_order is null, use native ordering in linelist
        to_add <- splt
        
      }

    }

    ## Add the splitting at the given depth
    mat[ind, i] <- to_add
    
  }

  ## This is true if i is ranked higher than j (essentially looks at the
  ## difference in ranking at each depth and choose the first difference)
  is_higher <- function(i, j) {
    comp <- mat[i,] - mat[j,]
    out <- comp[comp != 0][1] > 0
  }

  ## Rank all cases by pairwise comparison - this is quite slow for large
  ## networks, there is definitely a quicker way
  comb <- expand.grid(seq_len(nrow(mat)), seq_len(nrow(mat)))
  mat2 <- matrix(FALSE, nrow = nrow(mat), ncol = nrow(mat))

  mat2[as.matrix(comb)] <- apply(comb, 1, function(x) is_higher(x[1], x[2]))
  mat2[is.na(mat2)] <- FALSE

  ## We get global ranking by taking the sum of all pairwise rankings
  val <- apply(mat2, 1, sum)
  
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

    ## Place nodes at top or bottom by adding 10000 to val
    if(unlinked_pos == 'top') {
      val[unlinked[unlinked_order]] <- 10000 + unlinked_to_add
    } else if(unlinked_pos == 'bottom') {
      val[unlinked[unlinked_order]] <- -10000 + unlinked_to_add
    }
  }

  ## potential igraph algorithm
  if(!is.null(igraph_type)) {

    net <- igraph::graph_from_data_frame(stats::na.omit(x$contacts),
                                         vertices = x$linelist)

    if(igraph_type == 'rt') {
      val <- igraph::layout.reingold.tilford(net, root = which(depth == 1))[,1]
    } else if(igraph_type == 'sugiyama') {
      val <- igraph::layout.sugiyama(net, layers = x$linelist[[x_axis]])$layout[,1]
    } else if(igraph_type == 'fr') {
      val <- igraph::layout.fruchterman.reingold(net,
                                                 minx = x$linelist[[x_axis]],
                                                 maxx = x$linelist[[x_axis]])[,2]
    } else {
      stop("igraph_type must be one of 'rt', 'sugiyama' or 'fr'")
    }

  }

  ## This orders the cases and adds two positions at the top and bottom to
  ## provide space for the axes - only do this for method == 'ttree', otherwise
  ## we will have additional nodes in the ggplot
  y_adj <- 2
  if(method == 'ttree') {
    if(axis_type == 'double') {
      y_pos <- match(val, sort(unique(val)))
      y_pos = rescale(c(min(y_pos) - y_adj, max(y_pos) + y_adj, y_pos), 0, 1)
    } else if(axis_type == 'single') {
      y_pos <- match(val, sort(unique(val)))
      y_pos = rescale(c(min(y_pos) - y_adj, y_pos), 0, 1)
    } else if(axis_type == 'none') {
      y_pos <- match(val, sort(unique(val)))
      y_pos = rescale(y_pos, 0, 1)
    }
  } else {
    y_pos <- match(val, sort(unique(val)))
    y_pos <- rescale(y_pos, 0, 1)
  }

  ## Also return infector because we need scaffold tree for later
  return(list(y = y_pos, infector = infector, subtree_size = subtree_size))
  
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
      new_edge$to_node <- ifelse(new_edge$to %in% linelist$id, TRUE, FALSE)
      edges <- rbind(old_edge, new_edge)
    } else {
      new_edge$to_node <- ifelse(new_edge$to %in% linelist$id, TRUE, FALSE)
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






## Get coordinates for for ggplot geom_segments when ttree_shape =
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
                               !names(contacts) %in% c("from", "to")])
    
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


## Make all string elements the same length n by inserting whitespaces
get_adj_width <- function(x, n) {
  diff <- n - nchar(x)
  edge <- vapply(diff/2, strrep, " ", x = "  ")
  return(paste0(edge, x, edge))
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
