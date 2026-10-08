# A tapply except it always returns a logical vector with 1 element per value
# Checks nothing about params. Please confirm upstream that data is a data.frame, fun is a function that takes one arg, and by is a character vector of column names `data`
tapply_for_dataframe_filter <- function(data, fun, by) {
  ls_split <- split.data.frame(
    x = data,
    f = data[, by, drop = FALSE],
    drop = TRUE
  )
  bool_per_group <- vapply(
    X = ls_split,
    FUN = fun,
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )

  # Count rows in data.frame
  n_rows_per_group <- vapply(ls_split, FUN = nrow, FUN.VALUE = numeric(1))

  # Expand result based on list lengths
  bool_per_row <- rep(bool_per_group, times = n_rows_per_group)

  return(bool_per_row)
}
