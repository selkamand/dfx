#' Convert a column into row names
#'
#' Move the values of a single column into the row names of a data frame,
#' removing that column from the returned data frame.
#'
#' @param data A data frame.
#' @param column A single column name, supplied as a character string.
#'
#' @return `data` with `column` removed and its values used as row names.
#'
#' @examples
#' df <- data.frame(
#'   id = c("sample_1", "sample_2", "sample_3"),
#'   value = c(10, 20, 30)
#' )
#' column_to_rownames(df, "id")
#'
#' @seealso [rownames_to_column()] [base::row.names()]
#'
#' @export
column_to_rownames <- function(data, column) {
  # Assert data is a dataframe
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data.frame, not an object of class [",
      toString(class(data)),
      "]"
    )
  }

  # Assert column is a string
  if (!is.character(column)) {
    stop(
      "`column` must be a character string, not an object of class [",
      toString(class(column)),
      "]"
    )
  }

  if (length(column) != 1L || is.na(column)) {
    stop("`column` must be a single, non-missing column name")
  }

  if (!nzchar(column)) {
    stop("`column` must be a non-empty column name")
  }

  # Assert column is a valid column name of data
  if (!column %in% colnames(data)) {
    stop(
      "Column [",
      column,
      "] is not a column in `data`. Valid column names include: [",
      paste0(colnames(data), collapse = ", "),
      "]"
    )
  }

  # Assert data does not already have row names
  if (.row_names_info(data) > 0L) {
    stop(
      "`data` already has row names. Remove them before calling `column_to_rownames()`"
    )
  }

  # Convert values to character, then check they can be used as row names
  row_names <- as.character(data[[column]])

  if (anyNA(row_names)) {
    stop(
      "Column [",
      column,
      "] contains ",
      sum(is.na(row_names)),
      " missing values. Row names cannot be missing"
    )
  }

  duplicated_row_names <- unique(row_names[duplicated(row_names)])
  if (length(duplicated_row_names) > 0L) {
    stop(
      "Column [",
      column,
      "] must contain unique values to use as row names. Duplicated values: [",
      paste0(duplicated_row_names, collapse = ", "),
      "]"
    )
  }

  row.names(data) <- row_names
  data[[column]] <- NULL
  data
}
