#' Summarise a data frame into a single row
#'
#' Aggregate many rows of a data frame down into a single row of summary metrics
#' by applying a custom function (`fun`) that computes them.
#' Can create a row of summary metrics per group using `by`.
#'
#'
#' @param data A data frame
#' @param fun A one-argument function that receives `data` and computes
#' summary metrics (a named list of computed scalar values).
#' @param by Name of columns to group operations by.
#'
#' @return A new data frame with one column per computed summary metric and one
#' row per unique combination of groups. Also includes the grouping columns.
#' Since summarise creates a new data frame, it will only have default attributes
#' and row names.
#' Note summarising an empty data frame will produce a single-row data.frame
#' if `by=NULL` but a zero-row data frame if grouping columns are supplied.
#'
#' @details
#' Differences to dplyr summarise include:
#' - dfx throws an error if you try and compute two metrics with the same name.
#' - dfx throws an error if a metric returned by `fun` is not a scalar atomic
#'   vector.
#' - dfx throws an error if for grouped summaries `fun` returns different types
#'   between groups, rather than casting. For example:
#'   - integers are not be cast to doubles
#'   - ordered factors are not compatible with unordered factors
#'   - Date objects are not compatible with unclassed doubles
#' - `by` does not support grouping by list columns, nested data frames,
#'     matrices, or multidimensional arrays.
#'
#' ##  Summarisation functions
#' `fun` must return a nonempty named list of length-one atomic vectors.
#' `fun` can alternatively return a single-row data frame (essentially a list in
#' R anyway)
#' Each name should describe the metric being calculated.
#' Metrics must be either unclassed or use one of the base R classes:
#' - `factor`
#' - `ordered`
#' - `Date`
#' - `POSIXct`
#' - `difftime`
#'
#' We throw an error when custom classes and attributes are present.
#' Computed metrics may have names and the standard attributes of their class
#'   (factor levels, a `POSIXct` time zone, or `difftime` units etc.).
#'
#' ## Zero-row inputs
#'
#' Performing an **ungrouped** summarisation always returns a one-row
#' data.frame, even when the input `data` has zero rows. There are three
#' primary justifications:
#'
#' 1. Some summary functions make sense for an empty data frame for example
#'    counting the number of rows or the number of levels of factor variables.
#' 2. It makes it easier to chain a data frame filter into a summarise without
#'    having to worry about whether at least one row is retained.
#' 3. Feature parity with dplyr and data.table equivalents.
#'
#' In contrast, performing a **grouped** summarisation on a zero-row dataframe
#' will produce a zero-row data.frame. Why? Well a grouped summary returns one
#' row per **observed** combination of grouping values. With zero input rows
#' there are no observed unique combinations, so the result has zero rows.
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
#' # `fun` can also return metrics as a one-row data frame
#' summarise(mtcars, function(d) {
#'   data.frame(mean_mpg = mean(d$mpg), rows = nrow(d))
#' })
#'
#' @md
#' @export
#' @seealso [summarise_vector_by()]
summarise <- function(data, fun, by = NULL) {
  # Save calling environment so errors thrown in internal helpers show
  # `error in summarise()` not `error in internal_helper()`
  call <- sys.call()

  assert_dataframe(
    data,
    call = call,
    warn_grouped = TRUE,
    warn_rowwise = TRUE
  )

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

  if (is.null(by)) {
    return(summarise_ungrouped(data, fun, call = call))
  }

  # Deal with the more complex case:
  # Grouping columns were supplied (`by`) so we now apply `fun` over each group.
  # But first we must ensure `by` key columns are sensible
  by <- ensure_by_is_sensible(data = data, by = by, call = call)

  # If there are no rows in `data` we still want to return a 0-row data.frame
  # with both grouping columns and metric columns (with sensible types).
  #
  # This behaviour might seem weird since this same input will return a 1-row
  # data.frame if no grouping variables are supplied. Why change this?
  # Well if you say 'summarise data frame by group' and there are no groups
  # but you still want to return a 1-row data.frame - what do you put in the
  # grouping columns of the output? There can be no observed unique combination
  # of grouping variables if there are no rows. So there must be no entries.
  #
  # Note the only way to create those sensibly typed metrics columns is to run
  # the function on the empty dataframe.
  # This introduces a constraint on summary functions. They must work even
  # when input is length-0.
  if (nrow(data) == 0L) {
    df_metrics <- summarise_ungrouped(data, fun, call = call)
    assert_metric_names_do_not_overlap_groups(
      names(df_metrics),
      by,
      call = call
    )

    return(cbind(
      data[, by, drop = FALSE],
      df_metrics[integer(), , drop = FALSE]
    ))
  }

  # Then we generate a key (unique combination of grouping column levels)
  # assigned to each row
  key <- generate_unique_keys(data = data, by = by)

  # Generate a list with one vector per unique key, describing all row indices
  # belonging to that group. Groups are kept in first-appearance order.
  groups <- split(seq_len(nrow(data)), factor(key, levels = unique(key)))

  # Create a data.frame with one column for each grouping col specified by `by`
  # and 1 row per unique combination of grouping column values
  # (nuique key). This data.frame is saved as df_group_keys.
  #
  # The reason we create it is to help us later:
  # 1. Add  grouping column values to the returned summary table
  # 2. to make errors mention group information (see group_label() helper below)
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
    metrics_to_data_frame(ls_result_group)
  })

  # Perform strict assertions on the list of single row data.frames
  # (described by comments in helper function).
  #
  # NOTE: it may seem weird to do more assertions when we already run
  # `assert_summarise_function_return_is_sensible` above. However the assertion
  # below checks properties that are hard to validate from within the body of a
  # per-group loop.
  #
  # For example, say you want to compare whether the number, types, and orders
  # of metrics computed per group match the very first row. This is not possible
  # in above loop without running the `fun` on first group outside of the
  # lapply.
  #
  # Personally I think just doing these checks after the computation is
  # cleaner.
  #
  # The real question is whether some (or all) of the checks we do in
  # assert_summarise_function_return_is_sensible or even the as.data.frame call
  # should be moved out of the per-group lapply and placed here after the
  # initial loop. Would let us keep more assertion logic together. Not doing
  # this now because the code works as is and would have to change below code.
  # to expect a list of lists instead of a list of single-row data.frames.
  assert_list_of_metric_dataframes_are_sensible(
    ls_metrics,
    group_label, # function to help make error messages more beautiful
    call = call
  )

  # After 1-row summary data.frames for all groups have been computed,
  # combine the results with rbind.
  df_collated_summary <- do.call(rbind, ls_metrics)

  # rbind() can replace a missing factor value with the explicit NA level.
  # We can restore these values based on the original metrics list.
  df_collated_summary <- restore_missing_factor_codes(
    df_collated_summary,
    ls_metrics
  )

  # Check for duplicate column names
  assert_no_duplicates_in_metric_names(
    colnames(df_collated_summary),
    call = call
  )

  # Check metric column names do not overlap with grouping column names
  assert_metric_names_do_not_overlap_groups(
    names(df_collated_summary),
    names(df_group_keys),
    call = call
  )

  # Add grouping column information before metric columns
  return(cbind(df_group_keys, df_collated_summary))
}

summarise_ungrouped <- function(data, fun, call) {
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

  # assert resulting list is sensible (a named list with one entry per metric)
  assert_summarise_function_return_is_sensible(
    ls_result,
    expected_rows = 1,
    call = call
  )

  # Why do we expect 1 row even if input data.frame has zero rows? (see above
  # assertion is set for expected_rows = 1)
  #
  # There are 3 main reasons:
  #
  # 1) Some summary functions make sense for an empty data.frame. For example
  #    a count of row numbers (dplyr::n()) or factor levels is useful even
  #    when the entire data.frame is empty.
  # 2) It matches how dplyr::summarise works.
  # 3) Upstream filters will often create a zero-row data.frame that gets fed
  #    into a summarise function. Forcing the summarise contract to always
  #    produces the same set of columns makes downstream code easier to reason
  #    about and simplify attempts to chain filter -> summarise.

  # Combine rows into data.frame
  df_metrics <- metrics_to_data_frame(ls_result)

  # Check all columns are unique
  # (& `fun` doesn't return two metrics with the same name)
  assert_no_duplicates_in_metric_names(colnames(df_metrics), call = call)

  return(df_metrics)
}

restore_missing_factor_codes <- function(summary, group_metrics) {
  for (j in seq_along(summary)) {
    metric <- summary[[j]]
    if (is.factor(metric) && anyNA(levels(metric))) {
      # rbind() can turn missing codes into the explicit NA level. It binds
      # one row per group in order, so use the original codes to restore
      # missing values in this column.
      missing_code <- vapply(
        group_metrics,
        function(row) {
          is.na(unclass(row[[j]]))
        },
        logical(1)
      )

      is.na(metric)[missing_code] <- TRUE
      summary[[j]] <- metric
    }
  }

  summary
}

# Take a set of column names (from metric data.frame) and assert there are no
# duplicates
assert_no_duplicates_in_metric_names <- function(cols, call) {
  # Check for duplicate column names
  if (anyDuplicated(cols)) {
    ndups <- sum(duplicated(cols))
    duplicated_cols <- cols[duplicated(cols)]

    msg <- paste(
      "Each summary metric must have a unique name. "
    )
    if (ndups != 1) {
      msg <- paste(
        msg,
        "The names ",
        quote_and_collapse(duplicated_cols),
        " were returned more than once.",
        sep = ""
      )
    } else {
      msg <- paste(
        msg,
        "The name ",
        quote_and_collapse(duplicated_cols),
        " was returned more than once.",
        sep = ""
      )
    }

    stop(
      simpleError(
        message = msg,
        call = call
      )
    )
  }
}

assert_metric_names_do_not_overlap_groups <- function(
  metric_names,
  group_names,
  call
) {
  shared <- intersect(metric_names, group_names)
  if (length(shared) != 0L) {
    stop(simpleError(
      message = paste0(
        "the names of metrics computed by `fun` must not match the name of grouping columns. Found [",
        length(shared),
        "] shared names: [",
        toString(shared),
        "]"
      ),
      call = call
    ))
  }
}

# Goal of this function is to take a list of 1-row data.frames
# and return an error if:
#  1) the data.frames have inconsistent widths (ncol)
#  2) their metric names or types returned by `fun` changes compared to the
#     result from the first group.
#  3) if first group metrics include an ordered factor, ensure all subsequent
#     ordered factors have identical levels (order preserved)
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
  # column numbers for each data.frame.
  n_cols_per_group <- lengths(ls_metrics)
  if (length(unique(n_cols_per_group)) != 1) {
    # We now know we have to error. The rest of this closure is just
    # crafting the nicest error message possible.

    # Set our expected number of metrics based on first group
    expected_count <- n_cols_per_group[[1L]]

    # How which indices have aare mismatched
    count_mismatches <- which(n_cols_per_group != expected_count)
    i <- count_mismatches[[1L]]
    actual_count <- n_cols_per_group[[i]]

    stop(simpleError(
      message = paste0(
        "`fun` must return the same number of metrics for every group. ",
        "It returned ",
        expected_count,
        " ",
        ngettext(expected_count, "metric", "metrics"),
        " for ",
        group_label(1L),
        " and ",
        actual_count,
        " ",
        ngettext(actual_count, "metric", "metrics"),
        " for ",
        group_label(i),
        "."
      ),
      call = call
    ))
  }

  # Grab the names and types of the first row of metrics calculated.
  # These will be our reference to compare subsequent results to.
  expected_types <- vapply(ls_metrics[[1L]], metric_type, character(1))
  ordered_factor_cols <- which(expected_types == "ordered_factor")
  metric_names <- colnames(ls_metrics[[1L]])

  # Starting from the second group, go through the summarised data.frame and
  # compare types & metric names to original
  for (i in seq_along(ls_metrics)[-1L]) {
    actual_types <- vapply(ls_metrics[[i]], metric_type, character(1))

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
          "` has incompatible types across groups.\n  ",
          group_label(1L),
          ": ",
          expected_types[[j]],
          " [value = ",
          first_row_value,
          "]\n  ",
          group_label(i),
          ": ",
          actual_types[[j]],
          " [value = ",
          current_row_value,
          "]"
        ),
        call = call
      ))
    }

    # If there are any ordered factor columns loop through and check levels are
    # identical
    for (j in ordered_factor_cols) {
      expected_levels <- levels(ls_metrics[[1L]][[j]])
      actual_levels <- levels(ls_metrics[[i]][[j]])

      if (!identical(expected_levels, actual_levels)) {
        stop(simpleError(
          paste0(
            "Ordered factor metric `",
            metric_names[[j]],
            "` must have identical levels (in the same order) across groups. ",
            "Found [",
            toString(expected_levels),
            "] in ",
            group_label(1L),
            " and [",
            toString(actual_levels),
            "] in ",
            group_label(i),
            "."
          ),
          call = call
        ))
      }
    }
  }

  invisible(NULL)
}

metric_type <- function(x) {
  if (is.ordered(x)) {
    return("ordered_factor")
  }

  if (is.factor(x)) {
    return("factor")
  }

  # When a class attribute is supplied we
  # add that to storage_type (typeof)
  # to make sure that calling functions can distinguish cases where
  # custom class attributes differ but they share the same underlying
  # storage_type (or the reverse)
  explicit_class <- attr(x, "class", exact = TRUE)
  storage_type <- typeof(x)

  if (is.null(explicit_class)) {
    return(storage_type)
  }

  paste0(paste(explicit_class, collapse = "/"), ":", storage_type, "")
}

unsupported_metric_metadata <- function(metric) {
  metadata <- attributes(metric)
  if (is.null(metadata)) {
    return("")
  }

  metric_class <- metadata[["class"]]
  allowed <- if (is.null(metric_class)) {
    character()
  } else if (
    identical(metric_class, "factor") ||
      identical(metric_class, c("ordered", "factor"))
  ) {
    c("class", "levels")
  } else if (identical(metric_class, "Date")) {
    "class"
  } else if (identical(metric_class, c("POSIXct", "POSIXt"))) {
    c("class", "tzone")
  } else if (identical(metric_class, "difftime")) {
    c("class", "units")
  } else {
    return(paste0("has unsupported class [", toString(metric_class), "]."))
  }

  extra <- setdiff(names(metadata), c("names", allowed))
  if (length(extra)) {
    return(paste0("has unsupported attributes [", toString(extra), "]."))
  }

  ""
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
  } else if (length(x) == 0L) {
    stop(simpleError("`fun` must return at least one metric.", call = call))
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

  # A one-cell matrix can sneak pass the length check.
  # Reject dimensions here because implicit data-frame conversion can expand
  # the metric or replace its name.
  dimensioned <- which(vapply(
    x,
    function(metric) {
      !is.null(dim(metric))
    },
    logical(1)
  ))

  if (length(dimensioned)) {
    metric_name <- xnames[[dimensioned[[1L]]]]
    stop(simpleError(
      paste0(
        "Metric `",
        metric_name,
        "` must be a length-one atomic vector without dimensions."
      ),
      call = call
    ))
  }

  # Data-frame conversion can consume a metric named row.names as row
  # metadata. So this checks the callback's original names before that can happen.
  assert_no_duplicates_in_metric_names(xnames, call = call)

  # Check that the object returned has metadata (attributes) that are
  # supported by dfx::summarise
  # Return values must be unclassed (no class attribute, e.g. plain vectors)
  # or have a supported class attribute:
  # - `factor`
  # - `ordered`
  # - `Date`
  # - `POSIXct`
  # - `difftime`
  unsupported <- vapply(x, unsupported_metric_metadata, character(1))
  invalid <- which(nzchar(unsupported))
  if (length(invalid)) {
    j <- invalid[[1L]]
    stop(simpleError(
      paste0("Metric `", xnames[[j]], "` ", unsupported[[j]]),
      call = call
    ))
  }

  return(invisible(NULL))
}

# Turn a named list of scalar values into a data.frame
# in a way that doesn't let internal R weirdness silently change column names
# See test "summarise preserves names that overlap conversion internals" for
# details
metrics_to_data_frame <- function(metrics) {
  metric_names <- names(metrics)

  # Drop the list's attributes before conversion so its names cannot match
  # data.frame() arguments. Keep the attributes on each metric value.
  attributes(metrics) <- NULL
  result <- as.data.frame(metrics, check.names = FALSE)
  colnames(result) <- metric_names

  # Ensure data.frame has no rownames.
  rownames(result) <- NULL

  result
}
