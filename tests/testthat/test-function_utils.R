test_that("func_arg_count counts explicit parameters, including defaults", {
  expect_equal(func_arg_count(function() NULL), 0L)
  expect_equal(func_arg_count(function(d) NULL), 1L)
  expect_equal(func_arg_count(function(d, other) NULL), 2L)
  expect_equal(func_arg_count(function(d, other = NULL) NULL), 2L)
})

test_that("func_supports_variable_arguments recognizes only a true dots formal", {
  expect_false(func_supports_variable_arguments(function(d) NULL))
  expect_false(func_supports_variable_arguments(function(...value) NULL))
  expect_true(func_supports_variable_arguments(function(...) NULL))
  expect_true(func_supports_variable_arguments(function(d, ...) NULL))
})

test_that("a dotted parameter name remains usable by filter", {
  data <- data.frame(id = 1:4)

  result <- dfx::filter(data, function(...value) ...value$id > 2L)

  expect_identical(result$id, c(3L, 4L))
})

test_that("func_arg_count follows the requested dots policy", {
  fun <- function(d, ...) NULL

  expect_equal(func_arg_count(fun, dots = "count_as_0"), 1L)
  expect_equal(func_arg_count(fun, dots = "count_as_1"), 2L)
  expect_identical(func_arg_count(fun, dots = "count_as_inf"), Inf)
  expect_error(
    func_arg_count(fun, dots = "throw_error"),
    "Cannot count number of arguments if there are dots",
    fixed = TRUE
  )
})
