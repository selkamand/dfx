# To support filtering dataframes `by` some set of grouping columns
# we created a stricter version of tapply that returns a logical vector with 1 element per value.
# To keep this function fast this function checks nothing about params.
# Please confirm upstream that data is a data.frame, fun is a function that takes one arg, and `by` is a character vector of column names `data`

tapply_for_dataframe_filter <- function(data, fun, by) {
  # Implementation Approach:
  # 1. Generate a new key vector that has a unique value for every unique combination of values across
  # columns in `by`.
  # We do this by asssigning an integer code to each unique value of the grouping columns and concatenate IDs into a `:` sepearated key.
  # The conversion into an integer ensures character values don't mess up concatination.
  codes <- lapply(data[by], function(x) match(x, unique(x)))
  key <- do.call(paste, c(unname(codes), list(sep = ":")))

  # Generate a list with one vector per unique key, describing all row indices belonging to that group.
  # Groups are kept in first-appearance order.
  groups <- split(seq_len(nrow(data)), factor(key, levels = unique(key)))

  # Initialise the logical vector we'll return (1 element per data.frame row)
  keep <- rep(FALSE, nrow(data))

  # Save parent frame to use in error messages later (so errors can say 'error in filter(...)'
  filter_call <- sys.call(sys.parent(1L))

  # For each group of rows, apply the function to decide whether they should be kept
  for (rows in groups) {
    decision <- tryCatch(
      fun(data[rows, , drop = FALSE]),
      error = function(err) {
        group_label <- format_filter_group_label(data, by, rows[[1L]])
        stop(simpleError(
          message = paste0(
            "`fun` threw an error when applied to the data in Group (",
            group_label,
            "): ",
            conditionMessage(err)
          ),
          call = filter_call
        ))
      }
    )
    if (!is.logical(decision) || length(decision) != 1L) {
      stop_because_of_invalid_group_decision(data, by, rows[[1L]], decision)
    }
    keep[rows] <- decision
  }

  keep
}

stop_because_of_invalid_group_decision <- function(data, by, row, decision) {
  group_label <- format_filter_group_label(data, by, row)
  n_values <- length(decision)

  error_msg <- paste0(
    "`fun` must return exactly one logical value per group but instead returned ",
    n_values,
    " ",
    typeof(decision),
    " value",
    if (n_values == 1L) "" else "s",
    " for Group (",
    group_label,
    ")"
  )

  # Make error message display the function call two calls up (the original filter function)
  # I considered passing the call frame down as an argument but since this is an internal function exclusive
  # to dfx::filter and always exactly two calls down from filter I think this is fine
  error_call <- sys.call(sys.parent(2L))

  stop(simpleError(message = error_msg, call = error_call))
}

format_filter_group_label <- function(data, by, row) {
  group_values <- vapply(
    data[row, by, drop = FALSE],
    function(value) {
      if (is.atomic(value) && length(value) == 1L && is.na(value)) {
        return("NA")
      }
      if (is.character(value) || is.factor(value)) {
        return(encodeString(as.character(value), quote = '"'))
      }
      paste(format(value, trim = TRUE), collapse = ", ")
    },
    character(1)
  )
  paste(
    paste0("`", by, "` = ", group_values),
    collapse = ", "
  )
}

# Return a sensible `by` vector for grouped operations OR an error if it is not
# appropriate.
# data must be the grouping data.frame
# This helper function was created since both `filter` and `summarise` use the
# same `by` argument with the same constraints
ensure_by_is_sensible <- function(data, by, call) {
  # Ensure `by` is unique by removing dups
  # If unique `by` vector passess all the assertions, it will be returned
  by <- unique(by)

  # Assert `by` argument is a character vector
  error_message <- if (!is.character(by) || !is.vector(by)) {
    paste0(
      "`by` must be a character vector. Not an object of class [",
      toString(class(by)),
      "]"
    )
  } else if (length(by) == 0L) {
    # Ensure `by` is non-empty
    "`by` must be NULL or a non-empty vector. Found a zero-length vector."
  } else if (!all(by %in% colnames(data))) {
    # Assert `by` describes names of columns in `data`
    paste0(
      "`by` must be valid column names. Could not find column/s: ",
      toString(setdiff(by, colnames(data))),
      " in `data`"
    )
  } else if (any(vapply(data[by], is.matrix, logical(1)))) {
    # Ensure columns specified in `by` are not matrix-columns
    matrix_by <- by[vapply(data[by], is.matrix, logical(1))]
    paste0(
      "`by` cannot include matrix-valued columns: ",
      paste0("`", matrix_by, "`", collapse = ", ")
    )
  } else {
    # Passed all checks, so return by
    return(by)
  }

  # Throw error
  stop(simpleError(error_message, call = call))
}

generate_unique_keys <- function(data, by) {
  # Implementation Approach:
  # 1. Generate a new key vector that has a unique value for every unique combination of values across
  # columns in `by`.
  # We do this by asssigning an integer code to each unique value of the grouping columns and concatenate IDs into a `:` sepearated key.
  # The conversion into an integer ensures character values don't mess up concatination.
  codes <- lapply(data[by], function(x) match(x, unique(x)))
  key <- do.call(paste, c(unname(codes), list(sep = ":")))

  return(key)
}
