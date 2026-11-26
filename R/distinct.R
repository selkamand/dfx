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
  # We need to answer two separate questions: which rows are duplicated,
  # and which columns should appear in the result (based on keep_all).

  # Save calling environment so errors thrown in internal helpers show
  # `error in distinct()` not `error in internal_helper()`
  call <- sys.call()

  # Assert data is a dataframe
  assert_dataframe(
    data,
    call = call,
    warn_grouped = TRUE,
    warn_rowwise = TRUE
  )

  # keep_all must be either TRUE / FALSE. No exceptions.
  if (!is.logical(keep_all)) {
    stop(
      "`keep_all` must be either TRUE or FALSE, not an object of class: [",
      toString(class(keep_all)),
      "]"
    )
  }
  if (length(keep_all) != 1L) {
    stop(
      "`keep_all` must be either TRUE or FALSE, not an object of length: [",
      length(keep_all),
      "]"
    )
  }
  if (is.na(keep_all)) {
    stop("`keep_all` must be either TRUE or FALSE, not a missing value (NA)")
  }

  # Setting `columns` to NULL tells distinct to base uniqueness on all columns
  if (is.null(columns)) {
    # Grab 'all' columns (by making a vector that for a 3 column data.frame would be 1:3)
    columns <- seq_len(ncol(data))
  } else {
    assert_columns_sensible(columns, data, call = call)

    # An explicitly empty (e.g. `character(0)`) selection of columns
    # can come from calling code that found no matching column names.
    # Report it here so it cannot silently make every row look like a duplicate.
    #
    # I considered other behavour for when `columns` has length of zero, but
    # we already have NULL defining 'all columns' and the intention of
    # setting `columns=character(0)` is ambiguous so we throw error to force
    # an explicit decision
    if (length(columns) == 0L) {
      stop(simpleError(
        "`columns` must contain at least one column name. Use `columns = NULL` to use all columns.",
        call = call
      ))
    }
  }

  # `duplicated` powers the distinct function - but it has limitations
  # we need to resolve. E.g. it can behave strangely on nested data-frame cols
  # and multidimensional arrays.
  #
  # The distinct_key_columns function basically just widens out these structures
  # out (e.g. 4 column nested data.frame expanded into 4 top-level columns)
  #
  # Note it returns an unnamed list of columns ready for duplicated()
  key_columns <- distinct_key_columns(data[columns])

  rows <- if (length(key_columns) == 0L) {
    # Sometimes an input data.frame will have no columns but several rows.
    # In this case, we just grab the first input row directly when one exists.
    seq_len(min(nrow(data), 1L))
  } else {
    # Give the prepared list data-frame attributes so duplicated()
    # compares rows. Calling data.frame() here would expand a list column
    # into several columns which we do NOT want.
    # structure() keeps each prepared column intact.
    comparison <- structure(
      key_columns,
      names = as.character(seq_along(key_columns)),
      row.names = seq_len(nrow(data)),
      class = "data.frame"
    )

    # duplicated() flags later occurrences of each key. which() turns the
    # unflagged entries into row positions in input order.
    which(!duplicated(comparison))
  }

  # keep_all changes what columns we return but not which rows survive.
  output_columns <- if (keep_all) seq_len(ncol(data)) else columns

  # Slice the original data because the key_columns `comparison` table no longer
  # has the original nested columns or array shapes. The shared helper also
  # protects multidimensional arrays that plain data[rows, columns]
  # can flatten, even with drop = FALSE.
  result <- slice_rows(data, rows, output_columns)

  # Base R reports a negative row-name count for automatically numbered
  # rows. Reset those labels so retaining rows 1 and 3 gives output labels
  # 1 and 2. Explicit row names may carry meaning, so preserve them.
  if (.row_names_info(data) < 0L) {
    rownames(result) <- NULL
  }
  result
}

# Input: selected columns as a data frame.
# Output: an unnamed list of these exact same 'comparison columns' except:
#   - A column containing a nested N-column data-frame will be expanded into
#     two different top-level comparison columns
#   - An array column with four values per row contributes four comparison
#     columns, one for each position in the array slice.
#   - A list column still jurst contributes one comparison column.
#     If a row contains c(1, 2), that whole vector is compared as a single entry.
#
# This helps 'duplicated()' work more sensibly
distinct_key_columns <- function(columns) {
  # Each selected column can contribute several comparison columns.
  # Keep those contributions in separate lists until we can join them
  # without unpacking the values stored inside list columns.
  parts <- lapply(columns, function(column) {
    if (is.data.frame(column)) {
      # A packed data frame contributes its columns, such as city and
      # postcode. Going through those columns drops the packed frame's
      # row labels and also handles another packed frame inside it.
      return(distinct_key_columns(column))
    }

    if (!is.array(column)) {
      # An ordinary column already has one value per row. In a list
      # column, that value may itself be a vector or another object.
      # Wrap the entire column so its individual entries stay intact.
      return(list(column))
    }

    # The first array dimension identifies input rows. Combine the
    # remaining dimensions into matrix columns. For example, an array
    # of shape n x 2 x 2 contributes four values per row, so it becomes
    # an n-row matrix with four columns. Giving both sizes explicitly
    # also handles arrays with zero rows or an empty trailing dimension.
    shape <- dim(column)
    components <- matrix(
      column,
      nrow = shape[1L],
      ncol = prod(shape[-1L])
    )
    lapply(seq_len(ncol(components)), function(i) components[, i])
  })

  # Join the lists of comparison columns one level deep. Recursive
  # unlisting would also unpack values inside list columns, which would
  # lose the boundary between one row's value and the next.
  unlist(parts, recursive = FALSE, use.names = FALSE)
}
