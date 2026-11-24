# anyNA() was introduced in R 3.1.0.
# nolint start: backport_linter.
if (getRversion() < "3.1.0") {
  anyNA <- function(x) {
    any(is.na(x))
  }
}


# lengths() was introduced in R 3.2.0.
if (getRversion() < "3.2.0") {
  lengths <- function(x, use.names = TRUE) {
    ans <- vapply(x, length, integer(1L), USE.NAMES = FALSE)
    names(ans) <- if (use.names) names(x) else NULL
    ans
  }
}
# nolint end
