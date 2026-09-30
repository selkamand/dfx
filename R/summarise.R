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
#'   `fun` must return, such as `numeric(1)`. List templates are not supported.
#'   Passed to the `FUN.VALUE` argument of `vapply()`.
#' @param drop If `TRUE`, unused factor levels in `by` are omitted. If `drop=FALSE` they are included.
#' @param default The value to return for unused levels. Only relevant when `drop = FALSE`. `NULL`
#'   by default, a missing value of type matching `template`will be supplied.
#'   If template is a raw vector, a `0x00` byte is used since raw vectors have no missing value
#'   A supplied value must be compatible with the length and type of `template`.
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
  template = numeric(1),
  drop = TRUE,
  default = NULL
) {
  if (missing(fun)) {
    stop("`fun` must be supplied.", call. = FALSE)
  }
  fun <- match.fun(fun)

  if (
    !is.factor(by) && !is.character(by) && !is.numeric(by) && !is.logical(by)
  ) {
    stop(
      "`by` must be a factor, character, numeric, or logical vector. Not an object of class: ",
      toString(class(by)),
      call. = FALSE
    )
  }
  if (length(x) != length(by)) {
    stop(
      "`x` and `by` must have the same length. ",
      length(x),
      "!= ",
      length(by),
      call. = FALSE
    )
  }
  if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
    stop("`drop` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.list(template)) {
    stop(
      "List templates are not supported; `template` must be a length-one atomic vector.",
      call. = FALSE
    )
  }
  if (
    length(template) != 1L ||
      !is.null(dim(template)) ||
      !is.atomic(template)
  ) {
    stop("`template` must be a length-one atomic vector.", call. = FALSE)
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

  vapply(
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
}
