#' Summarise rows in a data.frame
#'
#' Pass the full data frame to a function that computes summary metrics.
#'
#' @param data A data frame to filter.
#' @param fun A one-argument function that receives `data` and returns a logical
#'   vector with one value per row, or if `by` is supplied, one value per group.
#' @param by name of columns to group operations by.
#'
#' @return A data frame containing rows where `fun` returns `TRUE`.
#'
#' @details
#' Differences from `dplyr::summarise()` include:
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
    return(as.data.frame(ls_result))
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

  # Loop through each group to get a vector of row_ids.
  # For loop is used over apply to get better control of error messages when
  # function fails.
  # Result ls_result gets returned
  container <- list()
  i <- 0
  for (rows in groups) {
    i <- i + 1

    # Grab just the rows in current group
    group_data <- data[rows, , drop = FALSE]

    # Apply function to the group, error-ing if function throws an error
    ls_result_group <- tryCatch(
      fun(group_data),
      error = function(err) {
        stop(simpleError(
          message = paste0(
            "`fun` threw an error when applied to the data: ", #TODO: make this error message mention the group where the function failed
            conditionMessage(err)
          ),
          call = call
        ))
      }
    )

    # assert resulting list is sensible (a named list)
    # TODO: make this function inform user which group failed
    assert_summarise_function_return_is_sensible(
      ls_result_group,
      expected_rows = 1,
      call = call
    )

    # Combine rows into data.frame and save to container under 'key'.
    container[[i]] <- as.data.frame(ls_result_group)
  }

  # After summary for all groups have been computed, combine the results
  df_collated_summary <- do.call(rbind, container)

  # Add each unique combination of levels as separate columns
  df_collated_summary[["key"]] <- unique(key)

  return(df_collated_summary)
}

assert_summarise_function_return_is_sensible <- function(
  x,
  expected_rows = 1,
  call = NULL
) {
  xnames <- names(x)
  error_message <- if (is.null(x)) {
    paste0("`fun` must return a list, not NULL")
  } else if (!is.list(x)) {
    paste0(
      "`fun` must return a list, not an object of class [",
      toString(x),
      "]."
    )
  } else if (is.null(xnames)) {
    paste0("summarise `fun` must return a named list. No names found.")
  } else if (anyNA(xnames)) {
    paste0(
      "`fun` must return a named list. ",
      sum(is.na(xnames)),
      "/",
      length(xnames),
      " elements are unnamed."
    )
  } else if (!all(lengths(x) == expected_rows)) {
    dodgy <- lengths(x) != expected_rows
    dodgy_entries <- x[dodgy]

    paste0(
      "`fun` returned an unexpected number of values for [",
      names(dodgy_entries)[1],
      "]. Expected ",
      expected_rows,
      " but found ",
      lengths(dodgy_entries[1]),
      "."
    )
  } else {
    # TODO: add checks that each element of the list is a vector.
    return(invisible(NULL))
  }

  stop(simpleError(error_message, call = call))
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
