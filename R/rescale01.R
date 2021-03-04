#' Rescale vector
#'
#' Rescales a vector to 0-1.
#'
#' For networks, useful if weights/measures of tie strengths are on different scales.
#'
#' @param x Vector to be rescaled
#' @param min Optional numeric; minimum value possible that should be rescaled to 0 (otherwise will be scaled using minimum value represented in vector as 0)
#' @param max Optional numeric; maximum value possible that should be rescaled to 1 (otherwise will be scaled using maximum value represented in vector as 1)
#' @return Vector rescaled to 0-1
#' @export
rescale01 <- function(x, min=NULL, max=NULL){

  if(is.null(min) & is.null(max)) (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
  else if(!is.null(min) & is.null(max)){

    if(min > min(x, na.rm=TRUE)) stop("Range error: lower values than provided minimum appear in data")

    (x-min)/(max(x,na.rm=T)-min)
  }
  else if(is.null(min) & !is.null(max)){

    if(max < max(x, na.rm=TRUE)) stop("Range error: higher values than provided maximum appear in data")

    (x-min(x,na.rm=T))/(max-min(x,na.rm=T))
  }
  else if(!is.null(min) & !is.null(max)){

    if(min > min(x, na.rm=TRUE)) stop("Range error: lower values than provided minimum appear in data")
    if(max < max(x, na.rm=TRUE)) stop("Range error: higher values than provided maximum appear in data")

    (x-min)/(max-min)
  }

}
