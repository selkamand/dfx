#' Summarise rows in a data.frame
#'
#' Pass the full data frame to a function that computes summary metrics.
#'
#' @param data A data frame
#' @param fun A one-argument function that receives `data` and computes
#' summary metrics (a named list of computed scalar values).
#'
#' @param by Name of columns to group operations by.
#'
#' @return A data frame containing rows where `fun` returns `TRUE`.
#'
#' @details
#' Differences to dplyr summarise include:
#' - dfx throws an error if you try and compute two metrics with the same name.
#' - dfx throws an error if a metric returned by `fun` is not a scalar atomic
#'   vector.
#' - dfx throws an error if for grouped summaries `fun` returns a different type
#'   rather than casting (e.g. integers will not be cast to doubles).
#'
#' @examples
#' # summarise
#' summarise(mtcars, function(d) {
#'   list(mean_mpg = mean(d$mpg), max_cyl = max(d$cyl))
#' })
#'
#' # summarise by group
#' summarise(
#'     iris,
#'     function(df) {
#'       list(
#'         widest_petal = max(df$Petal.Width),
#'         longest_petal = max(df$Petal.Length)
#'       )
#'     },
#'     by = "Species"
#'   )
#'
#' @export
#' @seealso [summarise_vector_by()]
summarise <- function(data, fun, by = NULL) {
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

  # Assert `fun` does not support variable arguments (`...`)
  if (func_supports_variable_arguments(fun)) {
    stop(
      "`fun` supports variable arguments (has a `...` parameter). This is not permitted by the `summarise` function. All parameters must be explicit and named."
    )
  }

  # Assert `fun` takes only one argument
  if (func_arg_count(fun, dots = "count_as_0") != 1) {
    stop(
      "`fun` must have exactly one parameter (expecting a data.frame). Found ",
      func_arg_count(fun, dots = "count_as_0")
    )
  }

  # Save calling environment so errors thrown in internal helpers show
  # `error in summarise()` not `error in internal_helper()`
  call <- sys.call()

  # Simple case: `by` is null so we just apply the function to whole data.frame
  if (is.null(by)) {
    # Apply function to whole dataset, catching and adding error message
    # when fun fails using a tryCatch
    ls_result <- tryCatch(
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

    # assert resulting list is sensible (a named list)
    assert_summarise_function_return_is_sensible(
      ls_result,
      expected_rows = 1,
      call = call
    )

    # Combine rows into data.frame
    df_metrics <- as.data.frame(ls_result, check.names = FALSE)

    # Check all columns are unique
    # (& `fun` doesn't return two metrics with the same name)
    assert_no_duplicates_in_metric_names(colnames(df_metrics), call = call)

    return(df_metrics)
  }

  # Grouping columns were supplied (`by`) so we now apply
  # Group application of fun `by`
  # But first we have to ensure `by` key columns are sensible
  by <- ensure_by_is_sensible(data = data, by = by, call = call)

  # Then we generate a key (unique combination of grouping column levels)
  # assigned to each row
  key <- generate_unique_keys(data = data, by = by)

  # Generate a list with one vector per unique key, describing all row indices
  # belonging to that group. Groups are kept in first-appearance order.
  groups <- split(seq_len(nrow(data)), factor(key, levels = unique(key)))

  # Create a data.frame where cols = grouping cols specified by `by`
  # and rows represent the unique combination of grouping column values
  # that correspond to that key. This will be used at the end to add
  # grouping column values to the resulting code but also
  # to make errors more understandable

  # TODO: investigate performance. Run profiling.
  first_row_per_group <- vapply(
    groups,
    function(rows) rows[[1L]],
    integer(1),
    USE.NAMES = FALSE
  )
  df_group_keys <- data[first_row_per_group, by, drop = FALSE]
  rownames(df_group_keys) <- NULL

  group_label <- function(i) {
    values <- paste(by, df_group_keys[i, , drop = TRUE], sep = "=")
    paste0("group ", i, " [", paste(values, collapse = " & "), "]")
  }

  # Loop through each group to get a vector of row_ids (rows).
  ls_metrics <- lapply(X = seq_along((groups)), function(i) {
    rows <- unname(groups[[i]])

    # Grab just the rows in current group
    group_data <- data[rows, , drop = FALSE]

    # Apply function to the group, error-ing if function throws an error
    ls_result_group <- tryCatch(
      fun(group_data),
      error = function(err) {
        stop(simpleError(
          message = paste0(
            "`fun` threw an error when applied to the data in ",
            group_label(i),
            ": ",
            conditionMessage(err)
          ),
          call = call
        ))
      }
    )

    # When `fun` succeeds, is its output sensible:
    # We expect a named list of scalar atomic vectors
    tryCatch(
      assert_summarise_function_return_is_sensible(
        ls_result_group,
        expected_rows = 1,
        call = call
      ),
      error = function(err) {
        stop(simpleError(
          message = paste0("In ", group_label(i), ": ", conditionMessage(err)),
          call = call
        ))
      }
    )

    # Turn named list of scalar atomics into data.frame
    # with 1 column per metric calculated
    as.data.frame(ls_result_group, check.names = FALSE)
  })

  assert_list_of_metric_dataframes_are_sensible(
    ls_metrics,
    group_label,
    call = call
  )

  # After summary for all groups have been computed, combine the results
  df_collated_summary <- do.call(rbind, ls_metrics)

  # Check for duplicate column names
  assert_no_duplicates_in_metric_names(
    colnames(df_collated_summary),
    call = call
  )

  # Check metric column names do not overlap with grouping column names
  shared <- intersect(colnames(df_collated_summary), colnames(df_group_keys))
  if (length(shared) != 0) {
    stop(
      "the names of metrics computed by `fun` must not match the name of grouping columns. Found [",
      length(shared),
      "] shared names: [",
      toString(shared),
      "]"
    )
  }

  # Add grouping column information before metric columns
  return(cbind(df_group_keys, df_collated_summary))
}

# Take a set of column names (from metric data.frame) and assert there are no
# duplicates
assert_no_duplicates_in_metric_names <- function(cols, call) {
  # Check for duplicate column names
  if (anyDuplicated(cols)) {
    duplicated_cols <- cols[duplicated(cols)]
    stop(
      simpleError(
        message = paste0(
          "`fun` returned ",
          sum(duplicated(cols)),
          " metrics with the same name: [",
          toString(duplicated_cols),
          "]"
        ),
        call = call
      )
    )
  }
}

# Goal of this function is to take a list of 1-row data.frames
# and return an error if:
#  1) the data.frames have inconsistent widths (ncol)
#  2) their metric names or types returned by `fun` changes compared to the
#  result from the first group.
assert_list_of_metric_dataframes_are_sensible <- function(
  ls_metrics,
  group_label,
  call
) {
  # All of the following checks only make sense when there are multiple groups.
  # So if we only have metrics calculated for one group we just do an
  # early return
  if (length(ls_metrics) < 2L) {
    return(invisible(NULL))
  }

  # Did the summarise `fun` generate the same number of 'rows' (different metrics)
  # for all groups? If not we throw an error.
  # Here we use `lengths()` which for a list of data.frames returns a vector of
  # column numbers for each data.frame
  n_cols_per_group <- lengths(ls_metrics)
  if (length(unique(n_cols_per_group)) != 1) {
    stop(simpleError(
      message = "`fun` must compute the exact same number of metrics per group.",
      call = call
    ))
  }

  # Grab the names and types of the first row of metrics calculated.
  # These will be our reference to compare subsequent results to.
  expected_types <- vapply(ls_metrics[[1L]], typeof, character(1))
  metric_names <- colnames(ls_metrics[[1L]])

  for (i in seq_along(ls_metrics)[-1L]) {
    actual_types <- vapply(ls_metrics[[i]], typeof, character(1))

    # The names of metrics must be the same as the first row,
    # Otherwise throw an error.
    actual_metric_names <- colnames(ls_metrics[[i]])
    if (!identical(metric_names, actual_metric_names)) {
      # This != is only legal here because we guarantee our
      mismatched_names_lgl <- metric_names != actual_metric_names
      mismatch_description <- paste(
        metric_names[mismatched_names_lgl],
        actual_metric_names[mismatched_names_lgl],
        sep = "!=",
        collapse = " & "
      )

      stop(simpleError(
        paste0(
          "`fun` returns summary metrics in a different order between ",
          group_label(1L),
          " and ",
          group_label(i),
          ". ",
          mismatch_description,
          "."
        ),
        call = call
      ))
    }

    # Are there any mismatches in type?
    mismatch <- which(actual_types != expected_types)

    # If so throw an informative error
    if (length(mismatch)) {
      j <- mismatch[[1L]]
      current_row_value <- ls_metrics[[i]][[j]]
      first_row_value <- ls_metrics[[1L]][[j]]
      stop(simpleError(
        paste0(
          "Metric `",
          metric_names[[j]],
          "` has type ",
          expected_types[[j]],
          " (value = ",
          first_row_value,
          ") in ",
          group_label(1L),
          ", but had type ",
          actual_types[[j]],
          " (value = ",
          current_row_value,
          ") in ",
          group_label(i),
          ". "
        ),
        call = call
      ))
    }
  }

  invisible(NULL)
}

assert_summarise_function_return_is_sensible <- function(
  x,
  expected_rows = 1,
  call = NULL
) {
  xnames <- names(x)
  if (is.null(x)) {
    stop(simpleError("`fun` must return a list, not NULL", call = call))
  } else if (!is.list(x)) {
    msg <- paste0(
      "`fun` must return a list, not an object of class [",
      toString(x),
      "]."
    )
    stop(simpleError(msg, call = call))
  } else if (is.null(xnames)) {
    stop(simpleError(
      "`fun` must return a named list. No names found.",
      call = call
    ))
  } else if (any(!nzchar(xnames)) || anyNA(xnames)) {
    stop(simpleError(
      paste0(
        "`fun` must return a named list. ",
        sum(!nzchar(xnames) | is.na(xnames)),
        "/",
        length(xnames),
        " elements are unnamed."
      ),
      call = call
    ))
  } else if (!all(lengths(x) == expected_rows)) {
    dodgy <- lengths(x) != expected_rows
    dodgy_entries <- x[dodgy]

    msg <- paste0(
      "`fun` returned an unexpected number of values when calculating [",
      names(dodgy_entries)[1],
      "]. Expected ",
      expected_rows,
      " but found ",
      lengths(dodgy_entries[1]),
      "."
    )
    stop(simpleError(msg, call = call))
  } else if (
    # Assert `fun` returns atomic vectors
    any(vapply(
      X = x,
      FUN = function(res) {
        is.list(res) || !is.atomic(res)
      },
      FUN.VALUE = logical(1)
    ))
  ) {
    is_atomic_vector <- vapply(
      X = x,
      FUN = function(res) {
        !is.list(res) & is.atomic(res)
      },
      FUN.VALUE = logical(1)
    )

    dodgy_entries <- x[!is_atomic_vector]
    msg <- paste0(
      "`fun` returned an object of class: [",
      toString(class(x)),
      "] when calculating [",
      names(dodgy_entries)[1],
      "]. Expected an atomic vector."
    )

    stop(simpleError(msg, call = call))
  }

  return(invisible(NULL))
}

#' Split a vector by some other variable and summarise
#'
#' Apply a function to the values of `x` in each group of `by` and
#' return one summary value per group, in factor-level order.
#'
#' @param x A vector of values to summarise.
#' @param by A factor, character, numeric, or logical vector labelling which
#'   group each element in `x` belongs to. Must have the same length as `x`.
#' @param fun A function applied to each nonempty group. Must return a value
#'   compatible with the length and type of `template`.
#' @param template A length-one atomic vector indicating the type of value
#'   `fun` must return, such as `numeric(1)` or character(1).
#'    Passed to the `FUN.VALUE` argument of `vapply()`.
#' @param drop If `TRUE`, unused factor levels in `by` are omitted.
#'   If `drop=FALSE` they are included.
#' @param default
#'   The value to return for unused levels. Only relevant when `drop = FALSE`.
#'   By default, a missing value of a type matching `template` will be supplied.
#'   If template is a raw vector, a `0x00` byte is used since they do not support
#'   explicit missing values
#'   Supplied value must be a scalar value whose type matches `template`.
#'
#' @details
#' Non-factor `by` values are converted to a factor, so their groups appear in
#' sorted order. Ordinary missing values in `by` are omitted. An explicit `NA`
#' factor level is treated as a group when it has observations. Unused levels
#' receive `default` without calling `fun` on an empty vector.
#'
#' @return A named atomic vector with one element per observed group, or per factor
#'   level when by is a factor and `drop = FALSE`.
#'
#' @examples
#' vehicles <- data.frame(
#'   type = factor(
#'     c("car", "car", "bus", "car", "bus"),
#'     levels = c("car", "bus", "truck")
#'   ),
#'   speed = c(90, 96, 50, 93, 52)
#' )
#'
#' # Calculate average speed by vehicle type. Return numeric vector.
#' summarise_vector_by(
#'  x = vehicles$speed,
#'  by = vehicles$type,
#'  fun = mean,
#'  template = numeric(1)
#' )
#'
#' # To avoid dropping the unused 'truck' level, set drop = FALSE.
#' # By default it will now appear with a missing value.
#' summarise_vector_by(
#'    x = vehicles$speed,
#'    by = vehicles$type,
#'    fun = mean,
#'    template = numeric(1),
#'    drop = FALSE
#' )
#'
#' # Manually set what value should be returned for unused values
#' # using the `default` argument
#' summarise_vector_by(
#'   vehicles$speed,
#'   vehicles$type,
#'   mean,
#'   template = numeric(1),
#'   drop = FALSE,
#'   default = 0
#' )
#'
#' @export
summarise_vector_by <- function(
  x,
  by,
  fun,
  template,
  drop = TRUE,
  default = NULL
) {
  if (missing(fun)) {
    stop("`fun` must be supplied.")
  }
  fun <- match.fun(fun)

  if (
    !is.factor(by) && !is.character(by) && !is.numeric(by) && !is.logical(by)
  ) {
    stop(
      "`by` must be a factor, character, numeric, or logical vector. Not an object of class: ",
      toString(class(by))
    )
  }
  if (length(x) != length(by)) {
    stop(
      "`x` and `by` must have the same length. ",
      length(x),
      "!= ",
      length(by)
    )
  }
  if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
    stop("`drop` must be TRUE or FALSE.")
  }
  if (is.list(template)) {
    stop(
      "List templates are not supported; `template` must be a length-one atomic vector."
    )
  }
  if (
    length(template) != 1L ||
      !is.null(dim(template)) ||
      !is.atomic(template)
  ) {
    stop("`template` must be a length-one atomic vector.")
  }
  if (is.null(default) || identical(default, NA)) {
    default <- pick_correct_na_to_match_type(template)
  }
  vapply(list(default), identity, FUN.VALUE = template)

  # Convert non-factor grouping vectors to factors.
  if (!is.factor(by)) {
    by <- factor(by)
  }

  # Split x by group
  groups <- split(x = x, f = by, drop = FALSE)
  unused <- lengths(groups) == 0L

  if (drop) {
    groups <- groups[!unused]
  }

  result <- vapply(
    X = groups,
    FUN = function(group) {
      # If `by` is a factor with levels not present in `x`
      # return whatever the user specified as `default` (defaults to missing value)
      if (length(group) == 0L) {
        default
      } else {
        # Otherwise call the actual function
        fun(group)
      }
    },
    FUN.VALUE = template,
    USE.NAMES = TRUE
  )

  # If template is a date, turn the result of vapply back into a date
  # (vapply will strip it out)
  if (identical(class(template), "Date")) {
    class(result) <- class(template)
  } else if (identical(class(template), c("POSIXct", "POSIXt"))) {
    class(result) <- class(template)
    attr(result, "tzone") <- attr(template, "tzone")
  }

  result
}
