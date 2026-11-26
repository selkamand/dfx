# Subsetting a data frame with [] syntax in base R can turn a multidimensional
# array column into a vector, even with drop=FALSE. See example:
#
x <- data.frame(id = 1:2)
x$a <- array(1:4, dim = c(2, 1, 2))
x[2, , drop = FALSE]$a # 2L, with no dimensions
x$a[2, , , drop = FALSE] # 2L and 4L in a 1 x 1 x 2 array
#
# Take the usual data-frame subset first, then rebuild any flattened
# array columns from the originals by picking rows along the first
# dimension and keeping all other dimensions and their names. Do the
# same for arrays inside nested data-frame columns.
#
# - `data` is a data frame.
# - `rows` is an integer vector of positive row positions (possibly
#   empty) or a logical vector with one TRUE or FALSE per input row.
# - `columns` is an integer vector of column positions or a character
#   vector of column names. It defaults to all columns.
slice_rows <- function(data, rows, columns = seq_along(data)) {
  if (missing(columns)) {
    return(restore_sliced_arrays(data[rows, , drop = FALSE], data, rows))
  }
  restore_sliced_arrays(data[rows, columns, drop = FALSE], data[columns], rows)
}

restore_sliced_arrays <- function(output, source, rows) {
  # Use the output row count to check the shape of sliced arrays.
  # A logical mask can select fewer rows than its length.
  n_rows <- nrow(output)
  for (i in seq_along(source)) {
    column <- source[[i]]
    sliced <- output[[i]]

    if (is.data.frame(column)) {
      sliced <- restore_sliced_arrays(sliced, column, rows)
      if (!identical(sliced, output[[i]])) {
        output[[i]] <- sliced
      }
    } else if (is.array(column)) {
      shape <- dim(column)
      if (!identical(dim(sliced), c(n_rows, shape[-1L]))) {
        # Blank trailing subscripts keep all values along those dimensions.
        output[[i]] <- do.call(
          `[`,
          c(
            list(column, rows),
            rep(unname(alist(index = )), length(shape) - 1L),
            list(drop = FALSE)
          )
        )
      }
    }
  }

  output
}
