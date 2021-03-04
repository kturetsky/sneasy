#' Grand mean center a vector
#'
#' Centers each value of a vector on the mean of that vector.
#'
#' For networks, useful for control variables (e.g., network size) in a model so that intercept represents average individual.
#'
#' @param x Vector to be grand mean centered
#' @return Vector of grand mean centered values
#' @export
gmc <- function(x){x-mean(x, na.rm=T)}
