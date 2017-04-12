#' Interactive 3D Force-directed graph from epi_contact object
#'
#' This function creates a 3D graph from an epi_contact object
#'
#' @export
#'
#' @author
#' Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#' Thibaut Jombart (\email{thibautjombart@@gmail.com})
#' VP Nagraj (\email{vpnagraj@@virginia.edu})
#'
#' @param x An \code{\link{epicontacts}} object
#'
#' @param group An index or character string indicating which field of the
#'     linelist should be used to color the nodes. Default is \code{id}
#'
#' @param annot An index, logical, or character string indicating which fields
#' of the linelist should be used for annotating the nodes upon mouseover. The default
#' \code{TRUE} shows the 'id' and 'group' (if the grouping column is different from 'id').
#' 

#' @param col_pal A color palette for the groups.
#'
#' @param NA_col The color used for unknown group.
#'
#' @param g_title The title of the graph.
#'
#' @param bg_col The background color of graph.
#'
#' @param label_col The color of the graph title and labels of groups.
#'
#' @param node_size The sizes of graph nodes.
#'
#' @param edge_size The width of graph edges.
#'
#' @note All colors must be specified as color names like "red", "blue", etc. or
#' as hexadecimal color values without opacity channel, for example "#FF0000", "#0a3e55"
#' (upper or lower case hex digits are allowed).
#'
#' Double-click or tap on the plot to reset the view.
#'
#' @return
#' An htmlwidget object that is displayed using the object's show or print method.
#' (If you don't see your widget plot, try printing it with the \code{print} function.)
#'
#' @references
#'
#' Original three.js code by David Piegza:
#'     \url{https://github.com/davidpiegza/Graph-Visualization}.
#'
#' Original rthreejs code by B. W. Lewis:
#' \url{https://github.com/bwlewis/rthreejs}.
#'
#' @examples
#' if (require(outbreaks)) {
#'
#' ## example using MERS outbreak in Korea, 2014
#' head(mers_korea_2015[[1]])
#' head(mers_korea_2015[[2]])
#' 
#' x <- make_epicontacts(linelist = mers_korea_2015$linelist,
#'                       contacts = mers_korea_2015$contacts,
#'                       directed = FALSE)
#'
#' \dontrun{
#' graph3D(x)
#' graph3D(x, group = "sex", g_title = "MERS Korea 2014")
#' graph3D(x, group = "sex", annot = c("sex", "age"),
#'         g_title = "MERS Korea 2014")
#' }
#' }

graph3D <- function(x,
                    group = "id",
                    annot = TRUE,
                    col_pal = cases_pal,
                    NA_col = "lightgrey",
                    g_title = "",
                    bg_col = "white",
                    label_col = "darkgrey",
                    node_size = 1,
                    edge_size = .5) {

    ## check group
    if (!is.null(group)) {
        if (!group %in% names(x$linelist)) {
            msg <- sprintf("Group '%s' is not in the linelist", group)
            stop(msg)
        }
    }


    ## check annot
    if (!is.null(annot) && is.character(annot)) {
        if (!all(annot %in% names(x$linelist))) {
            culprits <- annot[!annot %in% names(x$linelist)]
            culprits <- paste(culprits, collapse = ", ")
            msg <- sprintf("Annot '%s' is not in the linelist", culprits)
            stop(msg)
        }
    }


    ## Subset those ids which have at least one edge with another id
    ##    (to mimic visNetwork plot, else loner nodes are also printed)
    x <- subset_clusters_by_size(x, cs_min = 2)
    # x <- as.igraph.epicontacts(x)

    ## Get vertex attributes and prepare as input for graph
    nodes <- data.frame(id = unique(c(x$linelist$id,
                                      x$contacts$from,
                                      x$contacts$to)))


    ## join back to linelist to retrieve attributes for grouping

    nodes <- suppressMessages(
        suppressWarnings(dplyr::left_join(nodes, x$linelist)))


    ## getting annotations
    if(annot[1] == TRUE) {
        if (group == "id") {
            annot = group
        } else {
            annot = c("id", group)
        }
    }
    
            
    temp <- nodes[, annot, drop = FALSE]
    temp <- sapply(names(temp), function(e) paste(e, temp[, e], sep = ": "))
    nodes$label <- paste("<p>",
                         apply(temp, 1, paste0, collapse = "<br>"), "</p>")



    # attribute for grouping
    nodes$group <- as.character(nodes[,group])
    nodes$group[is.na(nodes$group)] <- "NA"
    nodes$group <- factor(nodes$group)

    # Set node attributes
    
    ## get annotations
    ## Put the id column back as the first column
    temp <- nodes[ , c(ncol(nodes), 1:(ncol(nodes) - 1))]
    
    ## Drop the "names" and columns created when epicontacts object is converted
    ##    to an igraph object
    
    drop_name <- which(names(temp) %in% "name")
    temp <- temp[ , -drop_name]
    temp <- temp[, annot, drop = FALSE]
    temp <- sapply(names(temp), function(e) paste(e, temp[, e], sep = ": "))
    nodes$label <- paste("<p>",
                         apply(temp, 1, paste0, collapse = "<br>"), "</p>")
    
    ## node color
    K <- length(unique(nodes$group))
    grp.col <- col_pal(K)
    grp.col[levels(nodes$group)=="NA"] <- NA_col
    nodes$color <- grp.col[factor(nodes$group)]

        
    # changing original "id" column to one required by threejs::graphjs()
    #   & backing up old id
    nodes$orig_id <- nodes$id 
    nodes$id <- 1:nrow(nodes) # has to be integer


    
    ## make visNetwork inputs: edges
    edges <- x$contacts
    edges_from <- dplyr::left_join(edges, nodes[ , c("orig_id", "id")],
                                  by = c("from" = "orig_id"))["id"]
    
    edges_to <- dplyr::left_join(edges, nodes[ , c("orig_id", "id")],
                                  by = c("to" = "orig_id"))["id"]
    edges$from <- edges_from$id
    edges$to <- edges_to$id

    
    ## Set edge attributes
    edges$size = edge_size
    edges$color = "lightgrey"

    ## Set vertex attributes
    nodes$size = node_size

    # Subset vertex dataframe for graphjs
    nodes <- nodes[ , c("group", "id", "size", "color", "label")]

    # Create 3D graph
    g <- threejs::graphjs(edges = edges, nodes = nodes, main = g_title,
                          showLabels=FALSE, fg = label_col, bg = bg_col)

    return(g)
}


