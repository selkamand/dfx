#' Reorder factor levels by hand
#'
#' Move any number of levels to any location.
#'
#' @param x a factor whose levels you want to reorder
#' @param ref level names in the order you want them (character vector)
#' @param after the position to move the reference levels to (number)
#' @return a factor whose levels with reordered levels
#'
#' @examples
#' f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
#' fct_relevel(f, "a")
#' fct_relevel(f, c("b", "a"))
#' fct_relevel(f, c("b", "a", after = Inf))
#'
#' @export
fct_relevel <- function(x, ref, after = 0L) {
  # Assertions
  if (!is.factor(x)) {
    stop(
      "`x` must be a factor, not an object of class [",
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
  xnew <- factor(
    as.character(x),
    levels = new_levels,
    ordered = is.ordered(x)
  )

  # Copy over names
  names(xnew) <- names(x)

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
