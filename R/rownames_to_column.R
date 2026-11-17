#' Convert row names into a column
#'
#' Add the row names of a data frame as a new first column, then remove them.
#'
#' @param data A data frame.
#' @param column Name for the new column, supplied as a character string.
#'
#' @return `data` with its row names added as a new first column and automatic
#' row names.
#'
#' @details
#'  - When `data` has no explicit row names, its automatic row names (`"1"`,
#'    `"2"`, ...) are used as values.
#'
#' @examples
#' df <- data.frame(value = c(10, 20, 30))
#' row.names(df) <- c("sample_1", "sample_2", "sample_3")
#' rownames_to_column(df, "id")
#'
#' @seealso [column_to_rownames()] [base::row.names()]
#'
#' @export
rownames_to_column <- function(data, column) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data.frame, not an object of class [",
      toString(class(data)),
      "]"
    )
  }

  # data.table's `[` treats a bare numeric vector as row indices, so the
  # column reordering at the end would subset rows and leave the new column
  # in the wrong position.
  if (inherits(data, "data.table")) {
    stop("`data.table` objects are not supported by `rownames_to_column()`")
  }

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

  if (column %in% colnames(data)) {
    stop(
      "Column [",
      column,
      "] is already a column in `data`. Choose a different name for the row names column"
    )
  }

  data[[column]] <- row.names(data)

  # Reorder by position to put the new column first.
  data <- data[c(ncol(data), seq_len(ncol(data) - 1L))]

  # Reset row names so the values live only in the new column.
  row.names(data) <- NULL
  data
}
