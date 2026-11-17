test_that("between accepts scalar and elementwise numeric bounds", {
  expect_identical(between(numeric(), 0, 1), logical())
  expect_identical(between(numeric(), numeric(), numeric()), logical())
  expect_identical(between(1:3, 1, 2), c(TRUE, TRUE, FALSE))
  expect_identical(between(1:3, c(1, 2, 4), 3), c(TRUE, TRUE, FALSE))
  expect_identical(between(1:3, 1, c(2, 2, 2)), c(TRUE, TRUE, FALSE))
})

test_that("between handles each numeric missing-bound type like dplyr", {
  x <- c(-1, 1, 3)
  missing <- list(
    logical = NA,
    integer = NA_integer_,
    double = NA_real_,
    nan = NaN
  )

  expect_identical(between(x, 0, 2), c(FALSE, TRUE, FALSE))

  for (type in names(missing)) {
    bound <- missing[[type]]
    expect_identical(between(x, bound, 2), c(NA, NA, FALSE), info = type)
    expect_identical(between(x, 0, bound), c(FALSE, NA, NA), info = type)
  }

  for (left_type in names(missing)) {
    for (right_type in names(missing)) {
      expect_identical(
        between(x, missing[[left_type]], missing[[right_type]]),
        c(NA, NA, NA),
        info = paste(left_type, right_type)
      )
    }
  }
})

test_that("between handles elementwise and empty numeric missing bounds", {
  x <- c(-1, 1, 3)

  expect_identical(between(x, c(NA, NA, NA), 2), c(NA, NA, FALSE))
  expect_identical(between(x, 0, c(NA, NA, NA)), c(FALSE, NA, NA))
  expect_identical(between(x, c(NA, NA, NA), c(NA, NA, NA)), c(NA, NA, NA))
  expect_identical(between(x, c(0, NA, 0), 2), c(FALSE, NA, FALSE))
  expect_identical(between(x, 0, c(2, NA, 2)), c(FALSE, NA, FALSE))
  expect_identical(between(numeric(), NA, 2), logical())
  expect_identical(between(numeric(), 0, NA), logical())
})

test_that("between rejects incompatible typed missing numeric bounds like dplyr", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, NA_complex_, 2)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, 0, NA_complex_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, NA_character_, 2)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, 0, NA_character_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, as.Date(NA), 2)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, 0, as.Date(NA))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, as.POSIXct(NA, tz = "UTC"), 2)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, 0, as.POSIXct(NA, tz = "UTC"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, as.POSIXlt(NA, tz = "UTC"), 2)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1, 0, as.POSIXlt(NA, tz = "UTC"))
  })
})

test_that("between requires vector inputs", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(matrix(1:4, nrow = 2), 2, 3)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(array(1:4, dim = c(2, 2, 1)), 2, 3)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1:4, matrix(2), 4)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1:4, 2, matrix(4))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(
      as.Date("2024-01-02"),
      structure(as.Date("2024-01-01"), dim = c(1L, 1L)),
      as.Date("2024-01-03")
    )
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(NA, as.Date("2024-01-01"), matrix(2))
  })
})

test_that("between drops names like dplyr::between", {
  result <- between(c(a = 1, b = 2), 1, 2)

  expect_identical(result, c(TRUE, TRUE))
  expect_null(names(result))
})

test_that("between rejects numeric S3 classes in every argument", {
  series <- ts(1:3)

  expect_error(
    between(series, 1, 3),
    "Numeric S3 classes are not supported for `x`.",
    fixed = TRUE
  )
  expect_error(
    between(1:3, series, 3),
    "Numeric S3 classes are not supported for `left`.",
    fixed = TRUE
  )
  expect_error(
    between(1:3, 1, series),
    "Numeric S3 classes are not supported for `right`.",
    fixed = TRUE
  )
})

test_that("between rejects nonnumeric inputs and incompatible bound lengths", {
  expect_error(
    between("1", 1, 2),
    "`x` must be a numeric vector, not an object of class [character]",
    fixed = TRUE
  )
  expect_error(
    between(1:3, "1", 3),
    "`left` must be a numeric vector, not an object of class [character]",
    fixed = TRUE
  )
  expect_error(
    between(1:3, 1, "3"),
    "`right` must be a numeric vector, not an object of class [character]",
    fixed = TRUE
  )

  expect_error(
    between(1:3, 1:2, 3),
    "`left` must have length 1 or the same length as `x`. Length of `left`: [2]; length of `x`: [3]",
    fixed = TRUE
  )
  expect_error(
    between(1:3, 1, 1:2),
    "`right` must have length 1 or the same length as `x`. Length of `right`: [2]; length of `x`: [3]",
    fixed = TRUE
  )
  expect_error(
    between(1:3, numeric(), 3),
    "Length of `left`: [0]",
    fixed = TRUE
  )
  expect_error(
    between(1:3, 1, numeric()),
    "Length of `right`: [0]",
    fixed = TRUE
  )
})
