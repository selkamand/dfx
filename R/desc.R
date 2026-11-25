#' Flag a column for descending order
#'
#' Mark a column name for descending sorting in [arrange()].
#'
#' @param column A single, non-missing, non-empty column name, supplied as a
#'   character string with no attributes other than names.
#'
#' @return `column` with its name set to `"..DeSc.."`, replacing any existing name.
#'
#' @details
#' Unlike `dplyr::desc()`, this function marks a column name rather than
#' transforming a vector of values. `desc(desc("x"))` and `desc("x")` both sort
#' column `x` in descending order.
#' Preserve the name when combining it with other columns: naming the result or
#' removing its name changes the sorting instruction.
#'
#' @examples
#' arrange(iris, c("Species", desc("Sepal.Length")))
#'
#' @seealso [arrange()]
#' @export
desc <- function(column) {
  if (!is.character(column)) {
    stop(
      "`column` must be a character string, not an object of class [",
      toString(class(column)),
      "]"
    )
  }

  if (!is.vector(column)) {
    stop(
      "`column` must be a character string with no attributes other than names"
    )
  }

  if (length(column) != 1L || is.na(column)) {
    stop("`column` must be a single, non-missing column name")
  }

  if (!nzchar(column)) {
    stop("`column` must be a non-empty column name")
  }

  # Names survive c(), allowing flagged and unflagged column names to be mixed.
  names(column) <- "..DeSc.."
  column
}
