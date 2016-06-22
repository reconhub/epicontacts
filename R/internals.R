######################################
## These are non-exported functions ##
######################################

## from dibbler::dibbler.pal2
cases_pal <- function(n){
    if(!is.numeric(n)) stop("n is not a number")
    colors <- c("#ccddff", "#79d2a6", "#ffb3b3",
                "#a4a4c1","#ffcc00", "#ff9f80",
                "#ccff99", "#df9fbf","#ffcc99",
                "#cdcdcd")
    if(n<length(colors)) {
        return(colors[seq_len(n)])
    } else {
        return(graphics::colorRampPalette(colors)(n))
    }
}


## from adegenet::fac2col
char2col <- function (x, pal = cases_pal, na.col = "grey"){
    x <- factor(x)
    lev <- levels(x)
    nlev <- length(lev)
    col <- pal(nlev)
    res <- rep(na.col, length(x))
    res[!is.na(x)] <- col[as.integer(x[!is.na(x)])]
    return(res)
}
