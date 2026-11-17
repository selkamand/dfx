#' Are values within a range
#'
#' Compare values with inclusive lower and upper bounds.
#'
#' @param x A numeric, Date, or POSIXt vector.
#' @param left,right Inclusive vector bounds. Must be length 1 or `length(x)`.
#'
#' @return A logical vector the same length as `x`. Each element is `TRUE` when
#'   `left <= x` and `x <= right`, `FALSE` otherwise.
#'
#' @details
#'
#' Differences from `dplyr::between()` include:
#'  - Logical `x` vectors are rejected, while `dplyr::between()` casts them to
#'    numeric.
#'  - Character vectors and matrices/arrays of any type are not supported.
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
#' # Dates
#' visits <- as.Date(c("2024-01-15", "2024-02-20", "2024-03-25"))
#' between(x = visits, left = as.Date("2024-01-01"), right = as.Date("2024-02-29"))
#'
#' # Datetimes
#' appointments <- as.POSIXct(
#'   c("2024-01-15 09:00:00", "2024-01-15 13:30:00"),
#'   tz = "UTC"
#' )
#' between(
#'   x = appointments,
#'   left = as.POSIXct("2024-01-15 12:00:00", tz = "UTC"),
#'   right = as.POSIXct("2024-01-15 17:00:00", tz = "UTC")
#' )
#'
#' @export
between <- function(x, left, right) {
  # Assert neither `x` nor bounds are matrices/arrays.
  #
  # Why?
  # We don't support matrices/arrays because we want to guarantee the returned
  # value is a logical vector.
  # While there are lots of ways we could turn multidimensional
  # data into a single logical vector based on 'between-ness', that is
  # exactly the problem. None are quite obvious enough that future me will
  # remember which route we took. So we just won't support it at all.

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
  if (is.object(x)) {
    stop("Numeric S3 and S4 classes are not supported for `x`.")
  }

  # Match dplyr's coercion of logical bounds to numeric.
  if (is.logical(left) && !is.object(left)) {
    left <- as.numeric(left)
  }
  if (is.logical(right) && !is.object(right)) {
    right <- as.numeric(right)
  }

  # Assert bounds are numeric and have compatible lengths
  if (!is.numeric(left)) {
    stop(
      "`left` must be a numeric vector, not an object of class [",
      toString(class(left)),
      "]"
    )
  }
  if (is.object(left)) {
    stop("Numeric S3 and S4 classes are not supported for `left`.")
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
  if (is.object(right)) {
    stop("Numeric S3 and S4 classes are not supported for `right`.")
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

    # Convert Dates to midnight in `tz`, letting the OS resolve whether that
    # instant falls in daylight saving time. `as.POSIXlt()` can otherwise
    # leave `isdst` as 0, which shifts the instant by an hour during DST.
    date_to_posixct <- function(value, zone) {
      converted <- as.POSIXlt(value, tz = zone)
      converted$isdst <- -1L
      as.POSIXct(converted)
    }

    if (inherits(x, "Date")) {
      x <- date_to_posixct(x, tz)
    }
    if (inherits(left, "Date")) {
      left <- date_to_posixct(left, tz)
    }
    if (inherits(right, "Date")) {
      right <- date_to_posixct(right, tz)
    }
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
    # Only unclassed logical NAs count as missing; classed values are rejected.
    missing <- is.logical(bound) && !is.object(bound) && all(is.na(bound))
    compatible <- inherits(bound, kind) || missing

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
