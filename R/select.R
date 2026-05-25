#' Rename columns in a data frame
#'
#' This function renames columns in a data frame
#'
#' @param .data A data frame whose columns will be renamed.
#' @param namemap A named vector or list, where the names represent the new column
#' names and the values represent the current column names in the data frame.
#'
#' @return The data frame with renamed columns.
#'
#' @details The function checks if all the values (current column names) in the
#' `namemap` are present in the data frame. If any expected column names are missing,
#' the function stops with an error. If all expected names are present, the corresponding
#' columns are renamed according to the names provided in `namemap`.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   old_name1 = 1:5,
#'   old_name2 = letters[1:5]
#' )
#'
#' # Rename columns
#' df_renamed <- rename(df, c(new_name1 = "old_name1", new_name2 = "old_name2"))
#' print(df_renamed)
#'
#' @export
rename <- function(.data, namemap) {
  if (!is.vector(names(namemap))) {
    stop("rename: 'namemap' must be a named vector")
  }

  if (any(nchar(names(namemap)) == 0)) {
    stop("rename: all elements in 'namemap' must be named")
  }

  missing_names <- setdiff(namemap, colnames(.data))
  if (length(missing_names) > 0) {
    stop("rename: could not find column/s named [", paste0(missing_names, collapse = ", "), "]. Valid column names include: [", paste0(colnames(.data), collapse = ", "), "]")
  }

  colnames(.data)[match(namemap, colnames(.data))] <- names(namemap)
  return(.data)
}


#' Select (and optionally rename) columns from a data frame
#'
#' This function selects columns from a data frame based on a character vector of column names.
#'
#' If the `columns` vector is named, `select` will both select and rename the columns
#' in the resulting data frame. The new column names are taken from the names of the `columns` vector,
#' and the old column names are taken from the values of the `columns` vector.
#'
#' @param .data A data frame from which to select columns.
#' @param columns A character vector of column names to select from \code{.data}.
#' If \code{columns} is a named vector, the selected columns will be renamed in the returned data frame
#' using the names of \code{columns} as the new column names.
#'
#' @return A data frame containing only the specified columns from \code{.data}.
#' If \code{columns} is a named vector, the columns in the returned data frame will be renamed accordingly.
#'
#' @details
#' The function checks if the specified columns exist in the data frame and ensures there are no duplicates
#' in the \code{columns} vector. If \code{columns} is named, the function uses \code{rename} internally
#' to rename the columns after subsetting.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   x = 1:5,
#'   y = letters[1:5],
#'   z = rnorm(5)
#' )
#'
#' # Select columns 'x' and 'z'
#' df_selected <- select(df, c("x", "z"))
#' print(df_selected)
#'
#' # Select and rename columns
#' df_selected_renamed <- select(df, c(new_x = "x", new_z = "z"))
#' print(df_selected_renamed)
#'
#' @export
select <- function(.data, columns) {
  # Assertions
  if (!is.data.frame(.data)) {
    stop("select: '.data' must be a data.frame")
  }

  if (!is.vector(columns)) {
    stop("select: 'columns' argument must be a character vector, not a [", paste0(class(columns), collapse = "/"), "]")
  }

  if (!is.character(columns)) {
    stop("select: 'columns' argument must be a character vector")
  }

  if (anyDuplicated(columns) > 0) {
    stop("select: 'columns' argument must not contain duplicates")
  }

  cols_not_found <- setdiff(columns, colnames(.data))
  if (length(cols_not_found) != 0) {
    stop("select: Could not find column/s: [", paste0(cols_not_found, collapse = ", "), "]")
  }

  # Both subset and rename dataframe if 'columns' vector is named
  if (!is.null(names(columns))) {
    return(rename(.data[columns], named_only(columns)))
  }

  # Otherwise just subset the data.frame
  return(.data[columns])
}

#' Deprecated
#'
#' Please use the [select()] function instead
#'
#' @inherit select
#'
#' @export
bselect <- function(.data, columns){
  warning("`bselect` has now been renamed to `select` and will be removed from this package in an upcoming release")
  select(.data, columns)
}

#' Deprecated
#'
#' Please use the [rename()] function instead
#'
#' @inherit rename
#'
#' @export
brename <- function(.data, namemap){
  warning("`brename` has now been renamed to `rename` and will be removed from this package in an upcoming release")
  brename(.data, namemap)
}

# Function returns only the named elements of a vector
named_only <- function(vec) {
  vec[nchar(names(vec)) > 0]
}
