#' Count
#'
#' Tally the frequency of each level in a column
#' (or combination of levels in several column).
#'
#' @param .data a data.frame.
#' @param columns names of columns whole levels to tally (character vector).
#' @param sort sort by tallies in descending order.
#' If \code{FALSE} table is sorted by columns (flag)
#' @param name name of column describing frequencies.
#' Where \code{name} clashes with \code{columns} it will be
#' dot-prefixed until clash is removed. See details (string).
#' @param drop exclude zero-counts from factor levels not present in data.
#' @return a data.frame describing counts per level.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   x = 1:5,
#'   y = letters[1:5],
#'   z = rnorm(5)
#' )
#'
#' # Count columns 'x' and 'z'
#' count(df, c("x", "z"))
#'
#' # Set name of tally column
#' count(df, c("x", "z"), name = "tally")
#'
#' @details
#' It is not guaranteed that the tally column name defined by `name`
#' will be the true name in the resulting data.frame.
#' If the `name` argument clashes with a column in `columns` then name will
#' be prefixed with '.' until unique.
#'
#' The fact we cannot guarantee tally column name means there are only two safe ways extract this tally column.
#' 1. Extract the final column of the count data.frame
#' 2. Lookup the 'n' attribute of the count data.frame which describes the true name of the tally column.
#'
#' @export
count <- function(.data, columns, sort = FALSE, name = "n", drop = TRUE) {
  # Assertions
  if (!is.data.frame(.data)) {
    stop("'.data' must be a data.frame")
  }

  if (!is.vector(columns)) {
    stop("'columns' argument must be a character vector, not a [", paste0(class(columns), collapse = "/"), "]")
  }

  if (!is.character(columns)) {
    stop("'columns' argument must be a character vector")
  }

  if (anyDuplicated(columns) > 0) {
    stop("'columns' argument must not contain duplicates")
  }

  cols_not_found <- setdiff(columns, colnames(.data))
  if (length(cols_not_found) != 0) {
    stop("Could not find column/s: [", paste0(cols_not_found, collapse = ", "), "]")
  }

  if (!is.logical(sort) || length(sort) != 1) {
    stop("`sort` argument must be TRUE/FALSE, not [", toString(sort), "]")
  }

  if (!is.character(name)) {
    stop("`name` argument must be a string, not an object of class [", toString(class(name)), "]")
  }

  if (length(name) != 1) {
    stop("`name` argument  must have length of 1, not [", length(name), "]")
  }

  # Ensure tally column name does NOT
  # clash with columns we're tallying by
  # adding '.' prefix until all clashes resolve
  if (name %in% columns) {
    name <- prefix_dots_until_not_overlapping(name, taken = columns)
  }

  # Tally unique combinations
  results <- as.data.frame(
    table(.data[, columns, drop = FALSE], useNA = "ifany"),
    stringsAsFactors = FALSE
  )

  # Rename Last Column to name
  colnames(results)[ncol(results)] <- name

  if (sort) {
    # Sort by tally (descending) if sort = TRUE
    results <- results[order(-results[[name]]), , drop = FALSE]
  } else {
    # Sort by keys (ascending) if sort = FALSE
    results <- results[do.call(order, results[columns]), , drop = FALSE]
  }

  # Fix Key Column Types (convert back to original class)
  # (since table + as.data.frame either converts keys to factors or strings)
  for (col in columns) {
    results[[col]] <- convert_vector_to_match_target(results[[col]], .data[[col]], failure = "keep_original")
  }

  # If drop=TRUE remove zero counts
  # (produced from factors not present in data)
  if (drop) {
    results <- results[results[[name]] > 0, , drop = FALSE]
  }

  # Ensure rownames are empty
  rownames(results) <- NULL

  # Add attribute describing true tally count name (see @details)
  attr(results, "n") <- name

  # Return Result
  return(results)
}


prefix_dots_until_not_overlapping <- function(string, taken) {
  while (string %in% taken) {
    string <- paste0(".", string)
  }

  return(string)
}
