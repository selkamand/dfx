# The set of assertions used in many different dfx functions
# Used to ensure error messages are consistent across functions.

assert_dataframe <- function(data, call) {
  if (!is.data.frame(data)) {
    stop(simpleError(
      message = paste0(
        "`data` must be a data.frame, not an object of class [",
        toString(class(data)),
        "]"
      ),
      call = call
    ))
  }

  invisible(NULL)
}
