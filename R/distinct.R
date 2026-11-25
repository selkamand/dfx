#' Keep distinct rows
#'
#' Keep only unique/distinct rows from a data frame.
#' This is similar to dplyr::distinct but considerably slower ;)
#'
#' @param data A data frame.
#' @param columns Names of columns to base uniqueness on. By default all columns
#' will be used. Zero length vectors are not accepted.
#' @param keep_all Keep all columns
#'
#' @return
#' A subset of the original data frame containing the first of each duplicated
#' row. Explicit row names are preserved and automatic row names are reset.
#'
#' @examples
#' distinct(iris, "Species")
#'
#' @export
distinct <- function(data, columns = NULL, keep_all = FALSE) {
  call <- sys.call()

  # Assertions
  assert_dataframe(data, call = call, warn_grouped = TRUE)

  # Assert keep_all is a flag
  if (!is.logical(keep_all)) {
    stop(
      "`keep_all` must be either TRUE or FALSE, not an object of class: [",
      toString(class(keep_all)),
      "]"
    )
  }
  if (length(keep_all) != 1) {
    stop(
      "`keep_all` must be either TRUE or FALSE, not an object of length: [",
      length(keep_all),
      "]"
    )
  }
  if (is.na(keep_all)) {
    stop("`keep_all` must be either TRUE or FALSE, not a missing value (NA)")
  }

  # If columns is non-null strictly assert their contents
  if (!is.null(columns)) {
    assert_columns_sensible(columns, data, call = call)

    # Add a special check that `columns` contains at least one column name
    # Since when a user supplies columns=character(0) it not 100% clear what
    # they want. The error message asks them to explicitly set columns = NULL
    # if trying to use all columns to identify dups.
    if (length(columns) == 0L) {
      stop(simpleError(
        "`columns` must contain at least one column name. Use `columns = NULL` to use all columns.",
        call = call
      ))
    }
  } else {
    # Otherwise we'll calculate unique-ness based on all columns
    # by setting columns to a numeric vector [1 -> ncol(data)]
    columns <- seq_len(ncol(data))
  }

  # Identify duplicate rows
  duped <- duplicated(data[columns])

  # If data frame has no cols (only rows) then treat every row after the first
  # one as a duplicate. This overrides the above `duped` variable.
  # If we do NOT override duped in this special-case then  dfx::distinct of a
  # 0-col data frame returns a 0-row data frame (even if it had multiple)
  if (ncol(data) == 0) {
    duped <- seq_len(nrow(data)) > 1L
  }

  # Return unique rows and:
  # - all columns if `keep_all = TRUE`
  # - or just the columns uniqueness is based on if `keep_all = FALSE`
  df_deduped <- if (keep_all) {
    data[!duped, , drop = FALSE]
  } else {
    data[!duped, columns, drop = FALSE]
  }

  # If original data frame had default rownames we reset them after the distinct
  # Otherwise, the subset above will preserve the rownames
  if (.row_names_info(data) < 0L) {
    rownames(df_deduped) <- NULL
  }

  return(df_deduped)
}
