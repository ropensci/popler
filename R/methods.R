# summary method for popler objects
summary.popler <- function(x) {
 out <- length(unique(x$title))
 names(out) <- "number of projects"
 return(out)
}
