# The set of assertions used in many different dfx functions
# Used to ensure error messages are consistent across functions.

# Assert data is a data.frame
assert_dataframe <- function(data, call) {
  if (!is.data.frame(data)) {
    stop(simpleError(
      message = paste0(
        "`data` must be a data.frame, not an object of class [",
        toString(class(data)),
        "]"
      ),
      call = call
    ))
  }

  invisible(NULL)
}


# Assert columns is a
# - character vector
# - unique
# - present as a column name of `data`
# - not NA
# - non-zero characters
assert_columns_sensible <- function(columns, data, call) {
  # Assert its a character type
  if (!is.character(columns)) {
    stop(
      simpleError(
        paste0(
          "`columns` must be a character vector, not an object of class [",
          toString(class(columns)),
          "]"
        ),
        call = call
      )
    )
  }

  # Assert vector
  if (!is.vector(columns)) {
    stop(simpleError(
      "`columns` must be a character vector with no attributes other than names",
      call = call
    ))
  }

  # Assert no missing
  if (anyNA(columns)) {
    stop(simpleError(
      "`columns` must not contain missing values",
      call = call
    ))
  }

  # Assert values are all non-empty (i.e. error if "")
  if (any(!nzchar(columns))) {
    stop(simpleError(
      "`columns` must not contain empty strings",
      call = call
    ))
  }

  # Check there are no duplicates
  if (anyDuplicated(columns) > 0) {
    stop(simpleError(
      "`columns` must not contain duplicates",
      call = call
    ))
  }

  # Check the columns are in the data.frame
  cols_not_found <- setdiff(columns, colnames(data))
  if (length(cols_not_found) != 0) {
    stop(
      simpleError(
        paste0(
          "Could not find ",
          pluralise("column", n = length(cols_not_found)),
          " in `data`: [",
          paste0(encodeString(cols_not_found, quote = '"'), collapse = ", "),
          "]"
        ),
        call = call
      )
    )
  }

  invisible(NULL)
}
