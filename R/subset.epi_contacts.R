#' Subset an epi_contact object by factors
#'
#' @author Finlay Campbell
#'
#' @export
#'
#' @param x an epi_contact object to be subsetted
#' @param node.attribute a named list defining the node attribute name and node attribute value (as a single value or vector of values).
#' Dates must be provided as a vector of date objects, defining the range of dates included in the subset
#' @param edge.attribute a named list defining the edge attribute name and edge attribute value (as a single value or vector of values).
#' Dates must be provided as a vector of date objects, defining the range of dates included in the subset
#' @param ... further arguments passed on to other methods
#' 
#' @examples
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#' id="case.id", to="case.id", from="infector", directed=FALSE)
#' y <- subset.epi_contacts(x, node.attribute=list("gender"="f"), edge.attribute=list("source"="funeral"))

subset.epi_contacts <- function(x,node.attribute=NULL,edge.attribute=NULL,...){

    ## Check if epi_contacts object and node/edge attributes are provided correctly, and exist in the dataset  
  
    if(!inherits(x, "epi_contacts")) stop("x is not an 'epi_contacts' object")
    if(!is.null(node.attribute) & !inherits(node.attribute,"list")) stop("node.attribute is not a list")   
    if(!is.null(edge.attribute) & !inherits(edge.attribute,"list")) stop("edge.attribute is not a list") 
  
    #Check if node/edge attribute names are found in linelist/contacts
    if(!all(names(node.attribute) %in% names(x$linelist)) & !is.null(node.attribute)) stop("Node attribute name is not found in dataset")
    if(!all(names(edge.attribute) %in% names(x$contacts)) & !is.null(edge.attribute)) stop("Edge attribute name is not found in dataset")

    #Check if node/edge attribute values are found in linelist/contacts (doesn't work for dates, as these describe a range)
    if(!all(unlist(sapply(names(node.attribute),function(i){
        if(inherits(x$linelist[[i]],"Date") & !inherits(node.attribute[[i]],"Date")) stop("Date node attributes must be provided as date object")
        else if(inherits(x$linelist[[i]],"Date") & inherits(node.attribute[[i]],"Date")) return(TRUE)
        else node.attribute[[i]] %in% as.character(x$linelist[[i]])
        }))))
    stop("Node attribute value is not found in dataset")
  
    if(!all(unlist(sapply(names(edge.attribute),function(i){
      if(inherits(x$contacts[[i]],"Date") & !inherits(edge.attribute[[i]],"Date")) stop("Date edge attributes must be provided as date object")
      if(inherits(x$contacts[[i]],"Date") & inherits(edge.attribute[[i]],"Date")) return(TRUE)
      else edge.attribute[[i]] %in% as.character(x$contacts[[i]])
    }))))
    stop("Edge attribute value is not found in dataset")
  
    if(is.null(edge.attribute) & is.null(node.attribute)){
        warning("No node or edge attributes provided, returning unmodified epi.contact object")
        return(x)
    }

    node.id <- x$linelist$id
    edge.from <- x$contacts$from
    edge.to <- x$contacts$to

    if(!(is.null(node.attribute))){
        for(i in names(node.attribute)){
            if(class(node.attribute[[i]]) %in% c("character","factor","numeric")){
                x$linelist <- dplyr::filter(x$linelist,x$linelist[[i]] %in% node.attribute[[i]])
            }

            if(class(node.attribute[[i]]) %in% c("Date")){
                if(length(node.attribute[[i]])!=2) stop("Node attribute of class date must contain a start and end date")
                x$linelist <- dplyr::filter(x$linelist,x$linelist[[i]] >= node.attribute[[i]][1] & x$linelist[[i]] <= node.attribute[[i]][2])
            }
        }
        node.id <- x$linelist$id
    }

    if(!(is.null(edge.attribute))){
        for(i in names(edge.attribute)){
            if(class(edge.attribute[[i]]) %in% c("character","factor","numeric")){
                x$contacts <- dplyr::filter(x$contacts,x$contacts[[i]] %in% edge.attribute[[i]])
            }

            if(class(edge.attribute[[i]]) %in% c("Date")){
                if(length(node.attribute[[i]])!=2) stop("Node attribute of class date must contain a start and end date")
                x$contacts <- dplyr::filter(x$contacts,x$contacts[[i]] >= edge.attribute[[i]][1] & x$contacts[[i]] <= edge.attribute[[i]][2])
            }
        }
        edge.from <- x$contacts$from
        edge.to <- x$contacts$to
    }
    
    #Is degenerate I think - can be removed?
    out <- x[i=node.id,j=edge.from,contacts="from"]
    out <- out[i=node.id,edge.to,contacts="to"]

    return(out)
}