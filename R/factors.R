#' Reorder factor levels by hand
#'
#' Move any number of levels to any location.
#'
#' @param x a factor (or character vector) whose levels you want to reorder
#' @param ref level names in the order you want them (character vector)
#' @param after the position to move the reference levels to (number)
#'
#' @return a factor with levels reordered based on their order in `ref`.
#'
#' @examples
#' f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
#' fct_relevel(f, "a")
#' fct_relevel(f, c("b", "a"))
#' fct_relevel(f, c("b", "a", after = Inf))
#'
#' @export
#' @md
fct_relevel <- function(x, ref, after = 0L) {
  # Assertions about x:
  #  - Must be a character vector or factor
  #  - If a character vector -> silently convert to an unordered factor
  if (is.character(x)) {
    x <- factor(x)
  } else if (!is.factor(x)) {
    stop(
      "`x` must be a factor or character vector, not an object of class [",
      toString(class(x)),
      "]"
    )
  }
  if (!is.character(ref)) {
    stop(
      "`ref` must be a character vector, not an object of class [",
      toString(class(ref)),
      "]"
    )
  }

  if (!is.numeric(after) || length(after) != 1L || is.na(after)) {
    stop("`after` must be a single non-missing whole number.")
  }
  if (after < 0) {
    stop("`after` must be greater than or equal to 0.")
  }

  if (!is.infinite(after) && after != floor(after)) {
    stop("`after` must be a whole number.", call. = FALSE)
  }

  old_levels <- levels(x)

  ref <- unique(ref)

  # Intersect will fetch levels in ref
  # But importantly - in the order of ref
  levels_in_ref <- intersect(ref, old_levels)

  # Grab levels not in ref, preserving their original order
  levels_not_in_ref <- setdiff(old_levels, ref)

  # If after is longer than total level count, set to length()
  after <- min(after, length(levels_not_in_ref))

  # New Levels
  new_levels <- append(levels_not_in_ref, levels_in_ref, after = after)

  # Create new factor
  xnew <- change_factor_levels(x, new_levels)

  return(xnew)
}


#' Reverse factor level order
#'
#' Reverse levels of a factor
#' @param x a factor (or character vector) whose levels you want to reverse
#' @return A factor with its levels in reverse order.
#'
#' @details
#' Explicit `NA` levels in factors are preserved. If a factor contains both an
#' explicit `NA` level and genuinely missing values, the missing values become
#' part of the `NA` level, as in `forcats::fct_rev()`. Character vectors are
#' converted to factors without creating an explicit `NA` level.
#'
#' @examples
#' f <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))
#' reversed <- fct_rev(f)
#'
#' levels(reversed) # d, c, b, a
#'
#' @export
#' @md
fct_rev <- function(x) {
  # Assertions about x:
  #  - Must be a character vector or factor
  #  - If a character vector -> silently convert to an unordered factor
  if (is.character(x)) {
    x <- factor(x)
  } else if (!is.factor(x)) {
    stop(
      "`x` must be a factor or character vector, not an object of class [",
      toString(class(x)),
      "]"
    )
  }

  # Get reversed order of levels
  new_levels <- rev(levels(x))

  # Create a new factor, updating the levels but preserving all other attributes, names, etc
  xnew <- change_factor_levels(x, new_levels)

  return(xnew)
}

# Factor helpers ----

# Create a new factor identical to an existing one but with different levels
# Preserves names, ordered-status, explicit NA levels, and attributes.
# x should be a factor, and levels a character vector with the new levels
change_factor_levels <- function(x, levels) {
  xnew <- factor(
    as.character(x),
    levels = levels,
    ordered = is.ordered(x),
    exclude = NULL
  )

  names(xnew) <- names(x)
  attributes(xnew) <- utils::modifyList(attributes(x), attributes(xnew))

  return(xnew)
}

# Reorder a factors levels based on a reference factor
# x will be re-leveled so to match reference, with any levels
# not in reference added at end.
# Unlike in fct_relevel, where levels exclusive to reference are ignored,
# fct_align adds those levels to x (in the order they're present in reference)
#
# Used in dplyr::count
# If in the future we want to export, we need to add some assertion logic
fct_align <- function(x, reference) {
  ref_levels <- levels(reference)
  new_levels <- append(ref_levels, setdiff(levels(x), ref_levels))

  xnew <- factor(
    as.character(x),
    levels = new_levels,
    ordered = is.ordered(x)
  )

  # Copy over names
  names(xnew) <- names(x)

  return(xnew)
}
