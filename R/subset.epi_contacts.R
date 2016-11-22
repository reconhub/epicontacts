#' Subset an epi_contact object by factors
#'
#' This function subsets an \code{\link{epi_contacts}} object based on node, edge and/or cluster attributes.
#' Specifying node attributes will return an \code{\link{epi_contacts}} object containing only individuals 
#' with these given attributes in the linelist. Specifying edge attributes will return contacts with the 
#' attributes provided. Specifying cluster attributes will return clusters of connected cases, and can be 
#' defined by ids (returning clusters of cases connected to specified cases) or cluster sizes (returning 
#' cluster of a specific, minimum or maximum size).
#'
#' @author Finlay Campbell (\email{f.campbell15@@imperial.ac.uk}), Nistara Randhawa (\email{nrandhawa@@ucdavis.edu})
#'
#' @export
#'
#' @param x an epi_contact object to be subsetted
#' 
#' @param node.attribute a named list defining the node attribute name and node attribute value (as a single value or vector of values).
#' Dates must be provided as a vector of date objects, defining the range of dates included in the subset
#' 
#' @param edge.attribute a named list defining the edge attribute name and edge attribute value (as a single value or vector of values).
#' Dates must be provided as a vector of date objects, defining the range of dates included in the subset
#' 
#' @param cluster_id a character vector of case identifiers; the connected components attached to these
#' cases will be retained in the output object.
#' 
#' @param cs cluster size to be used for subsetting
#'
#' @param cs_min minimum cluster size for subsetting
#'
#' @param cs_max maximum cluster size for subsetting
#' 
#' @param ... further arguments passed on to other methods
#' 
#' @examples
#' if (require(outbreaks)) {
#' ## build data
#' x <- make_epi_contacts(ebola.sim$linelist, ebola.sim$contacts,
#' id="case.id", to="case.id", from="infector", directed=FALSE)
#' 
#' ## subset based on node and edge attributes
#' x_subset <- subset(x, node.attribute=list("gender"="f"), edge.attribute=list("source"="funeral"))
#'
#' ## subset a cluster connected to a given id (can be a vector of ids as well)
#' id <- "cac51e"
#' x_subset <- subset(x, cluster_id=id)
#'
#' ## subset based on cluster size range
#' x_subset <- subset(x, cs_min = 12, cs_max = 15)
#'
#' ## subset based on single cluster size
#' x_subset <- subset(x, cs = 12)
#'
#' ## subset based on minimum cluster size
#' x_subset <- subset(x, cs_min = 10)
#'
#' ## subset based on maximum cluster size
#' x_subset <- subset(x, cs_max = 9)
#' }

subset.epi_contacts <- function(x,node.attribute=NULL,edge.attribute=NULL,cluster_id=NULL,
                                cs=NULL,cs_min=NULL,cs_max=NULL,...){

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
  
    if(is.null(c(edge.attribute,node.attribute,cluster_id,cs,cs_min,cs_max))){
        warning("No subsetting attributes provided, returning unmodified epi.contact object")
        return(x)
    }
  
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
    }
  
    if(!is.null(cluster_id)){
        x <- subset_clusters_by_id(x,cluster_id)
    }
        
    if(!is.null(c(cs,cs_min,cs_max))){
        x <- subset_clusters_by_size(x,cs,cs_min,cs_max)
    }

    return(x)
}
