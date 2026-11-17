#' Are values within a range
#'
#' Compare values with inclusive lower and upper bounds.
#'
#' @param x A numeric, Date, or POSIXt vector.
#' @param left,right Inclusive vector bounds, each of length 1 or `length(x)`.
#'
#' @return A logical vector describing whether each element of `x` is >= left and <= right.
#'
#' @details
#'
#' Differences from `dplyr::between()` include:
#'  - Character vectors are not accepted by dfx.
#'  - When Dates / datetimes are supplied, `Inf` and `-Inf` Dates are considered
#'    open-ended bounds. Dplyr casts them to missing.
#'  - Matrices and arrays are not supported for `x` or either bound.
#'
#' @examples
#' # Scalar
#' between(x=5, left = 2, right = 7)
#'
#' # Vector `x`
#' between(x=1:5, left = 2, right = 7)
#'
#' # Vector `x`, `left`, `right`
#' between(x=1:3, left = c(0, 0, 0), right = c(0, 5, 5))
#'
#' @export
between <- function(x, left, right) {
  if (!is.null(dim(x))) {
    stop("`x` must be a vector, not a matrix or array.")
  }
  if (!is.null(dim(left))) {
    stop("`left` must be a vector, not a matrix or array.")
  }
  if (!is.null(dim(right))) {
    stop("`right` must be a vector, not a matrix or array.")
  }

  # If x or bounds are dates / date-times route to the `between_dates` function to deal with this special case.
  temporal_x <- inherits(x, c("Date", "POSIXt"))
  temporal_bound <- inherits(left, c("Date", "POSIXt")) ||
    inherits(right, c("Date", "POSIXt"))

  # But first double check that both x and bounds are 'temporal' (dates/datetime)
  if (temporal_bound && !temporal_x) {
    stop(
      "`x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [",
      toString(class(x)),
      "]."
    )
  }

  # Send temporal values and bounds to a dedicated `between_dates` function
  # which decides how to handle all the possible combos of Dates/ Datetimes
  # (e.g. how do we compare Date values if bounds are Datetimes and vice-versa?)
  if (temporal_x) {
    return(between_dates(x, left, right))
  }

  # Otherwise deal with the standard case where `x` is a numeric vector

  # Assert x is a numeric vector
  if (!is.numeric(x)) {
    stop(
      "`x` must be a numeric vector, not an object of class [",
      toString(class(x)),
      "]"
    )
  }
  if (is.object(x) && !isS4(x)) {
    stop("Numeric S3 classes are not supported for `x`.")
  }

  # Assert bounds are numeric and have compatible lengths
  if (!is.numeric(left)) {
    stop(
      "`left` must be a numeric vector, not an object of class [",
      toString(class(left)),
      "]"
    )
  }
  if (is.object(left) && !isS4(left)) {
    stop("Numeric S3 classes are not supported for `left`.")
  }
  if (length(left) != 1L && length(left) != length(x)) {
    stop(
      "`left` must have length 1 or the same length as `x`. Length of `left`: [",
      length(left),
      "]; length of `x`: [",
      length(x),
      "]"
    )
  }

  if (!is.numeric(right)) {
    stop(
      "`right` must be a numeric vector, not an object of class [",
      toString(class(right)),
      "]"
    )
  }
  if (is.object(right) && !isS4(right)) {
    stop("Numeric S3 classes are not supported for `right`.")
  }
  if (length(right) != 1L && length(right) != length(x)) {
    stop(
      "`right` must have length 1 or the same length as `x`. Length of `right`: [",
      length(right),
      "]; length of `x`: [",
      length(x),
      "]"
    )
  }

  unname(x >= left & x <= right)
}

between_dates <- function(x, left, right) {
  if (
    inherits(x, "POSIXt") ||
      inherits(left, "POSIXt") ||
      inherits(right, "POSIXt")
  ) {
    # Promote Dates to midnight in the first explicitly specified timezone.
    tz <- ""
    for (value in list(x, left, right)) {
      if (!inherits(value, "POSIXt")) {
        next
      }

      zone <- attr(value, "tzone", exact = TRUE)
      if (length(zone) > 0L && !is.na(zone[[1L]]) && nzchar(zone[[1L]])) {
        tz <- zone[[1L]]
        break
      }
    }

    if (inherits(x, "Date")) {
      x <- as.POSIXct(as.POSIXlt(x, tz = tz))
    }
    if (inherits(left, "Date")) {
      left <- as.POSIXct(as.POSIXlt(left, tz = tz))
    }
    if (inherits(right, "Date")) {
      right <- as.POSIXct(as.POSIXlt(right, tz = tz))
    }
  }

  if (!inherits(x, c("Date", "POSIXt"))) {
    stop("`x` must be a Date or datetime vector.")
  }
  kind <- if (
    inherits(x, "POSIXt") ||
      inherits(left, "POSIXt") ||
      inherits(right, "POSIXt")
  ) {
    "POSIXt"
  } else {
    "Date"
  }

  check_bound <- function(bound, name) {
    compatible <- inherits(bound, kind) ||
      (is.logical(bound) && all(is.na(bound)))

    if (!compatible) {
      stop("`", name, "` must be ", kind, " or missing when `x` is ", kind, ".")
    }
    if (length(bound) != 1L && length(bound) != length(x)) {
      stop("`", name, "` must have length 1 or the same length as `x`.")
    }
  }

  check_bound(left, "left")
  check_bound(right, "right")

  # Dates become day counts; datetimes become seconds since the epoch.
  unname(
    as.numeric(x) >= as.numeric(left) &
      as.numeric(x) <= as.numeric(right)
  )
}
