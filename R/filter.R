#' Keep rows selected by a logical vector
#'
#' Subset a data frame using an already computed logical vector.
#'
#' @param data A data frame to filter.
#' @param keep A logical vector with one value per row of `data`.
#'
#' @return A data frame containing rows where `keep` is `TRUE`.
#'
#' @details
#' Differences from `dplyr::filter()` include:
#'  - `keep` is a computed logical vector, rather than a data-masked expression.
#'  - Missing values in `keep` cause an error rather than dropping those rows.
#'  - The mask applies to the whole data frame; it is not evaluated within groups.
#'
#' @examples
#' keep_rows(mtcars, keep = mtcars$mpg > 20 & mtcars$cyl == 6)
#'
#' @export
#' @seealso [filter()]
keep_rows <- function(data, keep) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data.frame, not an object of class [",
      toString(class(data)),
      "]"
    )
  }
  if (!is.logical(keep) || !is.vector(keep)) {
    stop(
      "`keep` must be a logical vector, not an object of class [",
      toString(class(keep)),
      "]"
    )
  }
  if (length(keep) != nrow(data)) {
    stop(
      "`keep` must have one value for each row in `data`. Length of `keep`: [",
      length(keep),
      "] != Rows in `data` [",
      nrow(data),
      "]"
    )
  }
  if (anyNA(keep)) {
    stop(
      "`keep` contains ",
      sum(is.na(keep)),
      " missing values. All values of `keep` must be TRUE/FALSE"
    )
  }

  data[keep, , drop = FALSE]
}

#' Filter rows using a data frame predicate
#'
#' Pass the full data frame to a function that decides which rows to keep.
#'
#' @param data A data frame to filter.
#' @param fun A one-argument function that receives `data` and returns a logical
#'   vector with one value per row.
#'
#' @return A data frame containing rows where `fun` returns `TRUE`.
#'
#' @details
#' Differences from `dplyr::filter()` include:
#'  - `fun` receives the full data frame instead of evaluating a data-masked
#'    expression. Refer to columns with `$` inside the function.
#'  - Missing values in the result of `fun` cause an error rather than dropping
#'    those rows.
#'  - `fun` runs once on the full data frame, not separately within groups.
#'
#' @examples
#' filter(mtcars, function(d) d$mpg > 20 & d$cyl == 6)
#'
#' @export
#' @seealso [keep_rows()]
filter <- function(data, fun) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data.frame, not an object of class [",
      toString(class(data)),
      "]"
    )
  }

  # Assert `fun` is a function
  if (!is.function(fun)) {
    stop(
      "`fun` must be a function. Not an object of class [",
      toString(class(fun)),
      "]"
    )
  }

  # Assert `func` does not support variable arguments (`...`)
  if (func_supports_variable_arguments(fun)) {
    stop(
      "`fun` supports variable arguments (has a `...` parameter). This is not permitted by the `filter` function. All parameters must be explicit and named."
    )
  }
  # Assert `fun` takes only one argument
  if (func_arg_count(fun, dots = "count_as_0") != 1) {
    stop(
      "`fun` must have exactly one parameter (expecting a data.frame). Found ",
      func_arg_count(fun, dots = "count_as_0"),
    )
  }

  # Apply function to data.frame
  bool_vector <- fun(data)

  if (!is.logical(bool_vector) || !is.vector(bool_vector)) {
    stop(
      "`fun` must return a logical vector, not an object of class [",
      toString(class(bool_vector)),
      "]"
    )
  }
  if (length(bool_vector) != nrow(data)) {
    stop(
      "`fun` must return a vector with one value for each row in `data`. Length of returned vector: [",
      length(bool_vector),
      "] != Rows in `data` [",
      nrow(data),
      "]"
    )
  }
  if (anyNA(bool_vector)) {
    stop(
      "`fun` returned ",
      sum(is.na(bool_vector)),
      " missing values. All values returned by `fun` must be TRUE/FALSE"
    )
  }

  data[bool_vector, , drop = FALSE]
}

#' Filter rows using selected column vectors
#'
#' Pass selected columns to a function that decides which rows to keep.
#' This function is not exported because I'm not sure how the API should work.
#'
#' @param data A data frame to filter.
#' @param columns A character vector of column names passed to `fun`.
#' @param fun A function with one argument per selected column that returns a
#'   logical vector with one value per row of `data`.
#'
#' @return A data frame containing rows where `fun` returns `TRUE`.
#'
#' @details
#' Columns are passed to `fun` as separate vectors in the order given by
#' `columns`; arguments are matched by position, not by name.
#'
#' Differences from `dplyr::filter()` include:
#'  - Column names are supplied as strings, and `fun` receives their vectors
#'    instead of evaluating a data-masked expression.
#'  - Missing values in the result of `fun` cause an error rather than dropping
#'    those rows.
#'  - `fun` runs once on the full column vectors, not separately within groups.
#'
#' @examples
#' filter_on(
#'   data = mtcars,
#'   columns = c("mpg", "cyl"),
#'   function(mpg, cyl) mpg > 20 & cyl == 6
#' )
#'
#' @seealso [filter()], [keep_rows()]
filter_on <- function(data, columns, fun) {
  # Assertions on data
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data.frame, not an object of class [",
      toString(class(data)),
      "]"
    )
  }

  # Assert columns are character vectors
  if (!is.character(columns) || !is.vector(columns)) {
    stop(
      "`columns` must be a character vector, not an object of class [",
      toString(class(data)),
      "]"
    )
  }
  # Assert columns have nonzero length
  if (length(columns) == 0) {
    stop("`columns` must have a length > 0")
  }
  # Assert all columns are present in `data`
  if (any(!columns %in% colnames(data))) {
    missing_cols <- setdiff(columns, colnames(data))
    n_missing_cols <- length(missing_cols)
    stop(
      n_missing_cols,
      " columns not found in data, [",
      toString(missing_cols),
      "]"
    )
  }
  # Assert no duplicates
  if (anyDuplicated(columns)) {
    dup_columns <- columns[duplicated(columns)]
    ndups <- length(dup_columns)
    stop(
      "The `columns` vector included ",
      ndups,
      " duplicate values including [",
      toString(dup_columns),
      "]. Deduplicate and try again."
    )
  }

  # Assert `fun` is a function
  if (!is.function(fun)) {
    stop(
      "`fun` must be a function. Not an object of class [",
      toString(class(fun)),
      "]"
    )
  }

  # Assert `func` does not support variable arguments (`...`)
  if (func_supports_variable_arguments(fun)) {
    stop(
      "`fun` supports variable arguments (has a `...` parameter). This is not permitted by the `filter_on` function. All parameters must be explicit and named."
    )
  }
  # Assert `fun` has one parameter for every column
  if (func_arg_count(fun, dots = "count_as_0") != length(columns)) {
    stop(
      "`fun` must have exactly one parameter per column described by `columns` vector. Expected ",
      length(columns),
      " parameters but found ",
      func_arg_count(fun, dots = "count_as_0")
    )
  }

  # TODO: assertions on `by` should go here

  # Apply function to data - I think we should change this to force function parameter names to match column names exactly.
  # Yes this means columns with spaces don't work very well but it will save so much pain later on right? i image
  # all kinds of accidental misalignments would happen. -- but then we lose the generalisability of the funcitons - you can't
  # write a general method and apply it to any two columns?
  keep <- do.call(fun, unname(as.list(data[columns]))) # note we unname so it doesn't automatically match column names to parameter names
  keep_rows(data, keep)
}
