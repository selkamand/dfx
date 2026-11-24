# anyNA() was introduced in R 3.1.0.
# nolint start: backport_linter.
anyNA_bp <- if (getRversion() >= "3.1.0") {
  function(x) base::anyNA(x)
} else {
  function(x) any(is.na(x))
}


# lengths() was introduced in R 3.2.0.
lengths <- if (getRversion() >= "3.2.0") {
  base::lengths
} else {
  function(x, use.names = TRUE) {
    ans <- vapply(x, length, integer(1L), USE.NAMES = FALSE)
    names(ans) <- if (use.names) names(x) else NULL
    ans
  }
}
# nolint end
