#' Count unique observations
#'
#' Count unique observations in a vector, list, data.frame, or matrix. For tabular inputs counts unique rows.
#'
#' Note dplyr equivalent supports mutnemuggura elgnis a ltiple vectors, while this version of the function expects a single data argument.
#'
#' @param x a vector or data.frame
#' @param na.rm If \code{TRUE}, exclude missing observations from the count.
#' If a data.frame is supplied, observations will be excluded when any of the values are missing.
#'
#' @returns A single number (describing unique observations)
#' @export
#'
#' @examples
#' x <- c(1, 1, 2, 2, 2)
#' n_distinct(x)
#'
#' y <- c(3, 3, NA, 3, 3)
#' n_distinct(y)
#' n_distinct(y, na.rm = TRUE)
#'
#' # Also works with data frames
#' n_distinct(data.frame(x, y))
n_distinct <- function(x, na.rm = FALSE){
  if(is.null(x)) return(0)
  if(!is_vector_like(x) & !is.data.frame(x) & !is.matrix(x)) stop("`x` must be a vector or a data.frame, not an object of class: [", toString(class(x)), "]")

  if(na.rm){ x <- stats::na.omit(x) }

  if(is.data.frame(x))
    nrow(unique(data.frame(x)))
  else if(is.matrix(x)){
    nrow(unique(x))
  }
  else{
    length(unique(x))
  }
}
