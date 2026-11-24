test_that("assert_dataframe accepts data frames and their subclasses", {
  inputs <- list(
    data.frame(),
    data.frame(value = 1:3),
    data.frame(row.names = c("a", "b")),
    structure(data.frame(value = 1:3), class = c("custom", "data.frame"))
  )

  for (data in inputs) {
    original <- data
    expect_invisible(assert_dataframe(data, call = NULL))
    expect_identical(data, original)
  }
})

test_that("assert_dataframe reports the original class and supplied call", {
  call <- quote(arrange(data, "value"))
  err <- tryCatch(assert_dataframe(NULL, call = call), error = identity)

  expect_identical(conditionCall(err), call)

  expect_snapshot(error = TRUE, cran = TRUE, {
    assert_dataframe(NULL, call = call)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    assert_dataframe(matrix(1:4, ncol = 2L), call = call)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    assert_dataframe(
      structure(list(), class = c("custom", "list")),
      call = call
    )
  })
})

test_that("assert_dataframe suppresses the error call when it is NULL", {
  err <- tryCatch(assert_dataframe(NULL, call = NULL), error = identity)

  expect_null(conditionCall(err))
  expect_snapshot(error = TRUE, cran = TRUE, {
    assert_dataframe(NULL, call = NULL)
  })
})
