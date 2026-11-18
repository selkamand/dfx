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
#' @seealso [filter()] [base::subset()]
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
#'  - `fun` receives the full data frame instead of a data-masked
#'    expression.
#'  - Missing values in the result of `fun` cause an error rather than dropping
#'    those rows.
#'  - When `by` is supplied function must return EXACTLY 1 logical value per group whereas
#'    dplyr::filter allows it.
#'    E.g. `iris[iris$Petal.Width > 0.3,] == dplyr::filter(iris, Petal.Width > 0.4, .by = Species)` whereas
#'    `dfx::filter(iris, \(x){x$Petal.Width > 0.4}, by = "Species")` will error.
#'  - Matrix-valued columns cannot be used in `by`; `dplyr::filter()` supports
#'    grouping by them.
#'
#' @examples
#' filter(mtcars, function(d) d$mpg > 20 & d$cyl == 6)
#'
#' # Filter for any plants whose species includes
#' # at least one plant with Petl.Width > 0.3
#' filter(head(iris), function(df){ any(df$Petal.Width>0.3) }, by = "Species")
#'
#' @export
#' @seealso [keep_rows()] [base::subset()]
filter <- function(data, fun, by = NULL) {
  # Assert `data` is data.frame
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

  # Save caller info for later
  #(so error message can say `Error in filter(...)`
  call <- sys.call()

  # Create logical filter mask based on `by` if supplied
  bool_vector <- if (is.null(by)) {
    # Apply function to whole dataset, catching and adding error message
    # when fun fails using a tryCatch
    tryCatch(
      fun(data),
      error = function(err) {
        stop(simpleError(
          message = paste0(
            "`fun` threw an error when applied to the data: ",
            conditionMessage(err)
          ),
          call = call
        ))
      }
    )
  } else {
    # Assert `by` key columns are sensible
    by <- ensure_by_is_sensible(data = data, by = by, call = call)

    # Apply function to groups specified by 'by'
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
