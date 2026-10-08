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
#'   vector with one value per row, or if `by` is supplied, one value per group.
#' @param by name of columns to group operations by.
#'
#' @return A data frame containing rows where `fun` returns `TRUE`.
#'
#' @details
#' Differences from `dplyr::filter()` include:
#'  - `fun` receives the full data frame instead of evaluating a data-masked
#'    expression. Refer to columns with `$` inside the function.
#'  - Missing values in the result of `fun` cause an error rather than dropping
#'    those rows.
#'  - When `by` is supplied function must return EXACTLY 1 logical value per group whereas
#'    dplyr::filter allows can't set a grouping variable and then filter the data the same way.
#'    E.g. `iris[iris$Petal.Width > 0.4,] == dplyr::filter(iris, Petal.Width > 0.4, .by = Species)` whereas
#'    `dfx::filter(iris, \(x){x$Petal.Width > 0.4}, by = "Species")` will error
#'
#' @examples
#' filter(mtcars, function(d) d$mpg > 20 & d$cyl == 6)
#'
#' # Filter for any plants whose species includes
#' # at least one plant with Petl.Width > 0.4
#' filter(head(iris), \(df){any(df$Petal.Width>0.4)}, by = "Species")
#'
#' @export
#' @seealso [keep_rows()]
filter <- function(data, fun, by = NULL) {
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
      func_arg_count(fun, dots = "count_as_0")
    )
  }

  #  Create logical filter mask based on `by` if supplied
  bool_vector <- if (is.null(by)) {
    fun(data)
  } else {
    # Assert `by` argument is a character vector
    if (!is.character(by)) {
      stop(
        "`by` must be a character vector. Not an object of class [",
        toString(class(by)),
        "]"
      )
    }

    # Ensure `by` is unique by removing dups
    by <- unique(by)

    # Assert `by` describes names of columns in `data`
    if (!all(by %in% colnames(data))) {
      stop(
        "`by` must be valid column names. Could not find column/s: ",
        toString(setdiff(by), colnames(data)),
        " in `data`"
      )
    }

    # Apply function to groups specified by 'by'
    # TODO: we should try to improve the error message when fun produces more than 1 value per group
    # TODO: Add tests when `by` is used
    tapply_for_dataframe_filter(data, fun, by)
  }

  # Check result is an appropriately sized boolean vector
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
