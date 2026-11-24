#' Arrange rows by column values
#'
#' Order rows of a data frame by values of the selected columns.
#'
#' @param data A data frame to sort.
#' @param columns A character vector of column names, in order of priority.
#'   Wrap a column name in [desc()] to sort that column in descending order.
#' @param locale Either `"C"` (the default) for C ordering of character columns,
#'   or `NULL` to use the session's current collation. Other locale names are not
#'   supported.
#'
#' @return A data frame with rows sorted by `columns`.
#'
#' @details
#' Missing values sort last in both directions. Rows tied on all selected
#' columns keep their original order. An empty character vector leaves `data`
#' unchanged.
#'
#' Sorting columns must be vectors, including factors, dates, and date-times.
#' List columns, matrices, and nested data frames are not supported.
#'
#' Character columns use C ordering by default, which can differ from the
#' session's collation, particularly for mixed-case text. Use `locale = NULL`
#' to retain the session-dependent behavior of earlier versions. Unlike
#' `dplyr::arrange()`, `NULL` selects the current collation rather than C ordering.
#' Session collation settings are never changed. Factors retain their level order.
#'
#' Names in `columns` must be empty or the reserved `"..DeSc.."` marker set by
#' [desc()]. Naming or removing names from a flagged element changes the marker,
#' so combine column names with `c("group", desc("value"))`.
#'
#' @examples
#' arrange(iris, c("Species", "Sepal.Length"))
#' arrange(iris, c("Species", desc("Sepal.Length")))
#'
#' df <- data.frame(name = c("b", "A", "a", "B"))
#' arrange(df, "name")
#' arrange(df, "name", locale = NULL)
#'
#' @seealso [desc()]
#' @export
arrange <- function(data, columns, locale = "C") {
  call <- sys.call()
  assert_dataframe(data, call = call)
  assert_columns_sensible(columns, data, call = call)

  if (anyNA(names(columns)) || any(!names(columns) %in% c("", "..DeSc.."))) {
    stop("`columns` must be unnamed or use `desc()` to flag descending order")
  }

  if (!is.null(locale) && !is.character(locale)) {
    stop(
      "`locale` must be NULL or a character string, not an object of class [",
      toString(class(locale)),
      "]"
    )
  }

  if (!is.null(locale) && !identical(unname(locale), "C")) {
    stop('`locale` must be NULL or "C"')
  }

  if (length(columns) == 0L) {
    return(data)
  }

  # Remove column names so none can match order()'s control arguments.
  keys <- unname(data[columns])
  for (i in seq_along(keys)) {
    if (is.list(keys[[i]]) || !is.null(dim(keys[[i]]))) {
      stop(
        "Column [",
        encodeString(columns[[i]], quote = '"'),
        "] in `data` must be a vector, not an object of class [",
        toString(class(keys[[i]])),
        "]"
      )
    }

    if (!is.null(locale) && is.character(keys[[i]])) {
      # Radix sorting gives C collation without changing session settings.
      values <- enc2utf8(keys[[i]])
      keys[[i]] <- match(
        values,
        sort(unique(values), method = "radix", na.last = NA)
      )
    }
  }

  for (i in which(names(columns) %in% "..DeSc..")) {
    keys[[i]] <- -xtfrm(keys[[i]])
  }

  orders <- do.call(order, keys)
  data[orders, , drop = FALSE]
}
