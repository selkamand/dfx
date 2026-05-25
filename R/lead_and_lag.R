#' Compute lagged or leading values
#'
#' Find the "previous" ([lag()]) or "next" ([lead()]) values in a vector.
#' Useful for comparing values behind of or ahead of the current values.
#'
#' Matrices or data.frames are not considered valid inputs (unlike the dplyr equivalent).
#' Lists are supported, and when \code{default = NULL} padding will be a NULL entry.
#' Unlike dplyr equivalent, when \code{x} is a list, \strong{default} can have length > 1 since it will be wrapped in a list before being combined with \strong{x}.
#'
#' @param x A vector
#' @param n Positive integer of length 1, giving the number of positions to lag or lead by
#' @param default The value used to pad \code{x} back to its original size after the lag or lead has been applied.
#' The default, \code{NULL}, pads with a missing value. If supplied, this must be a vector with size 1.
#'
#' @returns A vector with the same type and size as x.
#'
#' @examples
#' lag(1:5)
#' lead(1:5)
#'
#' # Look 2 positions behind
#' lag(1:5, n=2)
#'
#' # Look 2 positions ahead
#' lead(1:5, n=2)
#'
#' # Pad with 0 instead of missing values
#' lag(1:5, default = 0)
#'
#'
#' @name lead_and_lag
NULL

#' @rdname lead_and_lag
#' @export
lag <- function(x, n = 1L, default = NULL){

  # Assertions
  if (inherits(x, "ts")) stop("`x` must be a vector, not a <ts>, do you want `stats::lag()`?")
  if (!is.vector(x) && !is.factor(x) && !inherits(x, "Date") && !inherits(x, "POSIXct") || is.matrix(x)) stop("`x` must be a vector, not an object of class: [", toString(class(x)), "]")
  if(!is.numeric(n) && length(n) == 1) stop("`n` must be an integer value, not of class: ", toString(class(n)))
  if (is.list(x)) return(lag_list(x=x, n=n, default = default))

  # Process Default
  if(!is.null(default)) {
    if(length(default) != 1)
      stop("`default` must be NULL or a scalar value, not a vector of length: ", length(default))

    # When x is an integer type and default is a non-integer number but whole, do the conversion
    if(is.integer(x) & is.numeric(default) & default%%1==0)
      default <- as.integer(default)

    # When default is NA, ensure type of NA matches type of x
    if(is.na(default)) default <- pick_correct_na_to_match_type(x)

    # If type of default does not match type of x, throw an error
    if(!identical(typeof(default), typeof(x))){
      stop("`default` must be the same type as x. Expected [", toString(typeof(x)), "] but recieved [", toString(typeof(default)), "]")
    }
  }

  if(n < 0) stop("`n` must positive")

  # if vector is empty, return as is
  if(length(x) == 0) return(x)

  # Setup padding
  padding <- if(!is.null(default)) default else pick_correct_na_to_match_type(x)

  # Early return if n is longer than x
  if(n >= length(x))
    return(rep(padding, times = length(x)))

  # Lag vector
  c(rep(padding, times=n), utils::head(x, n = length(x)-n))
}

#' @rdname lead_and_lag
#' @export
lead <- function(x, n = 1L, default = NULL){

  # Assertions
  if (!is.vector(x) && !is.factor(x) && !inherits(x, "Date") && !inherits(x, "POSIXct") || is.matrix(x)) stop("`x` must be a vector, not an object of class: [", toString(class(x)), "]")
  if(!is.numeric(n) && length(n) == 1) stop("`n` must be an integer value, not of class: ", toString(class(n)))
  if (is.list(x)) return(lead_list(x=x, n=n, default = default))

  # Process Default
  if(!is.null(default)) {
    if(length(default) != 1)
      stop("`default` must be NULL or a scalar value, not a vector of length: ", length(default))

    # When x is an integer type and default is a non-integer number but whole, do the conversion
    if(is.integer(x) & is.numeric(default) & default%%1==0)
      default <- as.integer(default)

    # When default is NA, ensure type of NA matches type of x
    if(is.na(default)) default <- pick_correct_na_to_match_type(x)

    # If type of default does not match type of x, throw an error
    if(!identical(typeof(default), typeof(x))){
      stop("`default` must be the same type as x. Expected [", toString(typeof(x)), "] but recieved [", toString(typeof(default)), "]")
    }
  }

  if(n < 0) stop("`n` must positive")


  # Setup padding
  padding <- if(!is.null(default)) default else pick_correct_na_to_match_type(x)

  # Early return if n is longer than x
  if(n >= length(x))
    return(rep(padding, times = length(x)))

  # Lead vector
  c(utils::tail(x, n = length(x)-n), rep(padding, times=n))
}


# List Versions -----------------------------------------------------------
lag_list <- function(x, n, default){
  padding <- list(default)
  #  if(is.null(default)) list(NULL)
  # else if(is.list(default)) default
  #else list(default)

  orig_len <- length(x)

  # Early return if n > orig length
  if(n >= orig_len)
    return(rep(padding, times = orig_len))

  new_long <- append(x = x, values = rep(padding, times = n), after = 0)
  new_long[seq_len(orig_len)]
}

lead_list <- function(x, n, default){
  padding <- list(default)
  # padding <- if(is.null(default)) list(NULL)
  # else if(is.list(default)) default
  # else list(default)

  orig_len <- length(x)

  # Early return if n > orig length
  if(n >= orig_len)
    return(rep(padding, times = orig_len))

  # padding = list(
  c(utils::tail(x, n = orig_len-n), rep(padding, times=n))
}
