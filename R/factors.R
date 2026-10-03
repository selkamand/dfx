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
#' @details
#' Any explicit `contrasts` attributes remain unchanged. If levels change,
#' a warning reminds you to consider rebuilding contrasts after all level changes.
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
    stop("`after` must be a whole number.")
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
#' An explicit `contrasts` attribute is retained unchanged. If levels change,
#' a warning reminds you to consider rebuilding contrasts after all level changes.
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

#' Reorder factor levels by frequency
#'
#' Reorder factor levels based on how many times they are observed.
#'
#' @param x a factor or character vector
#' @param ordered should the factor returned be ordered. By default, returned factor will have the same `ordered` status as the input factor.
#'
#' @return A factor with levels reordered based on observation frequency
#'
#' @details
#' An explicit `contrasts` attribute is retained unchanged. If levels change,
#' a warning reminds you to consider rebuilding contrasts after all level changes.
#'
#' @examples
#' f <- factor(c("b", "b", "a", "c", "c", "c"))
#' levels(fct_infreq(f)) # "c" "b" "a"
#'
#' @export
fct_infreq <- function(x, ordered = NA) {
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

  # Assertions about `ordered`
  #   - must be a logical, scalar value (TRUE, FALSE, or NA)
  if (!is.logical(ordered)) {
    stop(
      "`ordered` must be either TRUE, FALSE or NA. Not an object of class [",
      toString(class(ordered)),
      "]"
    )
  }
  if (length(ordered) != 1) {
    stop(
      "`ordered` must be a scalar value (expected length = 1, observed length = ",
      length(ordered),
      ")"
    )
  }

  # Names will be levels, values will be counts
  lvl_counts <- summarise_vector_by(
    x = x,
    by = x,
    fun = length,
    template = numeric(1),
    drop = FALSE,
    default = 0
  )

  # Relevel factor based on frequency conuts
  xnew <- fct_relevel(x, names(sort(lvl_counts, decreasing = TRUE)))

  # Ensure factor ordered status matches the expectation of `ordered` argument
  if (!is.na(ordered) && ordered != is.ordered(xnew)) {
    class(xnew) <- if (ordered) c("ordered", "factor") else "factor"
  }

  return(xnew)
}

#' Add levels to a factor
#'
#' Add levels to a factor. Any levels already present will be ignored
#'
#' @return a factor with `levels` added
#'
#' @param x a factor
#' @param add character vector of levels to add
#'
#' @details
#' New levels will be appended to the end.
#' Any levels already in `x` will be left in their original position (NOT moved to end).
#' An explicit `contrasts` attribute is retained unchanged. If levels change,
#' a warning reminds you to consider rebuilding contrasts after all level changes.
#'
#' Differences from `forcats::fct_expand` include:
#'  - dfx::fct_expand does NOT support `...`, a vector of levels to add must be supplied to `add` argument
#'  - dfx::fct_expand does NOT allow position of levels to be controlled by an `after` argument.
#'    The user intent of an `after` argument is unclear, since `add` can contain levels already present in factor
#'    and that should not be moved.
#'    Users can simply call fct_relevel after fct_expand to move levels precisely where they need.
#'
#'
#' @examples
#' f <- factor(c("A", "A", "B", "C"))
#'
#' # Expand Levels to include D, E and F
#' fct_expand(f, add = c("D, E, F"))
#'
#'
#' # If you attempt to add levels that already exist
#' # (e.g. "A") they will be ignored and kept at their existing position
#' fct_expand(f, add = c("A", "D, E, F"))
#'
#' @md
#' @export
fct_expand <- function(x, add) {
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

  # Assertions about levels:
  #  - Must be a character vector
  if (!is.character(add) || !is.atomic(add)) {
    stop(
      "`levels` to add in fct_expand must be a character vector, not an object of class [",
      toString(class(add)),
      "]"
    )
  }

  old_levels <- levels(x)

  # Ignore levels already in factor
  levels_to_append <- setdiff(add, old_levels)

  # Append new to levels
  revised_levels <- c(old_levels, levels_to_append)

  # Change factor levels
  change_factor_levels(x, revised_levels)
}

# Factor helpers ----

# Create a new factor identical to an existing one but with different levels
# Preserves names, ordered-status, explicit NA levels, and attributes.
# Explicit contrast attributes are left unchanged, but we warn when
# levels change so callers can revisit their contrasts if needed.
# x should be a factor, and levels a character vector with the new levels
# neither of these type constraints are asserted in this function. Must guarantee in upstream code
change_factor_levels <- function(x, levels) {
  xnew <- factor(
    as.character(x),
    levels = levels,
    ordered = is.ordered(x),
    exclude = NULL
  )

  names(xnew) <- names(x)
  attributes(xnew) <- utils::modifyList(attributes(x), attributes(xnew))

  # Warn user if there is an explicit contrasts matrix (describes an experimental design)
  # And the levels have changed, meaning theres a chance contrast matrix has gone out of sync
  # with factor. See issue #10
  old_contrasts <- attr(x, "contrasts", exact = TRUE)
  if (!is.null(old_contrasts)) {
    if (!identical(levels(x), levels(xnew))) {
      warning(
        "Factor levels changed while an explicit `contrasts` attribute is set. ",
        "Consider rebuilding contrasts after all level changes are complete."
      )
    }
  }

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
