library(outbreaks)


is_color <- function(x) {
  vapply(x, function(e)
         !is.null(tryCatch(col2rgb(x), error = function(e) NULL)),
         FALSE)
}


