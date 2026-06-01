pick_correct_na_to_match_type <- function(x) {
  if (inherits(x, "Date")) {
    as.Date(NA)
  } else if (inherits(x, "POSIXct")) {
    as.POSIXct(NA)
  } else if (is.factor(x)) {
    as.factor(NA)
  } else if (is.complex(x)) {
    NA_complex_
  } else if (is.logical(x)) {
    NA
  } else if (is.character(x)) {
    NA_character_
  } else if (is.integer(x)) {
    NA_integer_
  } else if (is.numeric(x)) {
    NA_real_
  } else {
    NA
  }
}


is_vector_like <- function(x) {
  (is.atomic(x) || is.list(x)) && is.null(dim(x))
}


#' Convert type to match target
#'
#' Convert vector x to target (must be a vector)
#' On failure if failure = `error` throw an error. If `keep_original` just return the original x
#'
#' @param x a vector whose type you want to convert.
#' @param target a vector whose type you want to convert x to.
#' @param failure When conversion errors should we throw an \code{error} or \code{keep_original} type.
#' @param error_prefix a string to prefix on the error. This can be used to
#' indicate calling scope since we set `call.=FALSE` when calling stop since
#' the trycatch scope is pretty uninformative
#'
#'
#' @return vector \code{x} with a class matching target
#'
convert_vector_to_match_target <- function(x, target, failure = c("error", "keep_original"), error_prefix = "conversion failure: ") {
  # Assertions & Arg prep
  failure <- match.arg(failure)

  target_class <- class(target)[1]

  conversion_function <- if (target_class == "numeric") {
    as.numeric
  } else if (target_class == "function") {
    as.function
  } else if (target_class == "logical") {
    as.logical
  } else if (target_class == "integer") {
    as.integer
  } else if (target_class == "numeric") {
    as.numeric
  } else if (target_class == "factor") {
    as.factor
  } else if (target_class == "character") {
    as.character
  } else if (target_class == "complex") {
    as.complex
  } else if (target_class == "Date") {
    as.Date
  } else if (target_class == "POSIXct") {
    as.POSIXct
  } else if (target_class == "POSIXlt") {
    as.POSIXlt
  } else if (target_class == "name") {
    as.name
  } else if (target_class == "pairlist") {
    as.pairlist
  } else if (target_class == "array") {
    as.array
  } else if (failure == "keep_original") {
    function(.x) {
      .x
    }
  } else {
    function(.x) {
      stop()
    }
  }

  newx <- tryCatch(
    conversion_function(x),
    warning = function(warn) {
      handle_conversion_failure(x, target, error_prefix, failure)
    },
    error = function(err) {
      handle_conversion_failure(x, target, error_prefix, failure)
    }
  )

  return(newx)
}

handle_conversion_failure <- function(x, target, error_prefix, failure) {
  if (failure == "error") {
    stop(
      error_prefix,
      "can not convert [",
      toString(class(x)),
      "] to [",
      toString(class(target)),
      "]",
      call. = FALSE
    )
  }

  if (failure == "keep_original") {
    return(x)
  }

  stop("no implementation for failure = `", failure, "`", call. = FALSE)
}
