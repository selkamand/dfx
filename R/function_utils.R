# A series of helpers for extracting information about functions

func_arg_names <- function(func) {
  names(formals(args(func)))
}

func_supports_variable_arguments <- function(func) {
  arg_names <- func_arg_names(func)
  any(grepl(x = arg_names, pattern = "...", fixed = TRUE))
}

func_args_as_pairlist <- function(func) {
  formals(args(func))
}

func_required_arg_names <- function(func) {
  args <- formals(args(func))
  if (length(args) == 0) {
    return(character(0))
  }
  required_args <- args[vapply(args, is.symbol, FUN.VALUE = TRUE)]
  required_args <- names(required_args)
  setdiff(required_args, "...")
}
#
# func_args_as_alist <- function(func){
#   a= unlist(func_args_as_pairlist(func))
# }

# func_arg_remove_defaults <- function(func, n){
#   #foo <- as.pairlist(alist(foo=)) ; names(foo) <- names(formals(f))[1]; formals(f)[1] <- foo; f
#   formals(func)[[1]] <- substitute()
#   return(func)
# }

func_arg_count <- function(
  func,
  dots = c("count_as_0", "count_as_1", "count_as_inf", "throw_error")
) {
  dots <- match.arg(dots)

  param_names <- func_arg_names(func)
  param_count <- length(param_names)

  supports_varargs <- func_supports_variable_arguments(func)

  if (supports_varargs) {
    if (dots == "throw_error") {
      stop(
        "Cannot count number of arguments if there are dots (...) present. Can explicitly set how we should deal with this problem via the dots argument"
      )
    } else if (dots == "count_as_0") {
      param_count <- param_count - 1
    } else if (dots == "count_as_1") {
      param_count <- param_count
    } else if (dots == "count_as_inf") {
      param_count <- Inf
    }
  }

  return(param_count)
}
