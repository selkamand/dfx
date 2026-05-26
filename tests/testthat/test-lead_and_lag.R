test_that("`lead()` / `lag()` get the direction right", {
  expect_identical(lead(1:5), c(2:5, NA))
  expect_identical(lag(1:5), c(NA, 1:4))
})

test_that("If n = 0, lead and lag return x", {
  x <- c(10L, 8L, 1L, 3L, 6L, 9L, 4L, 2L, 5L, 7L)
  expect_equal(lead(x, 0), x)
  expect_equal(lag(x, 0), x)
})

test_that("If n = length(x), returns all missing", {
  x <- c(10L, 8L, 1L, 3L, 6L, 9L, 4L, 2L, 5L, 7L)

  expect_equal(lead(x, length(x)), rep(NA_integer_, length(x)))
  expect_equal(lag(x, length(x)), rep(NA_integer_, length(x)))
})

test_that("`lag()` gives informative error for <ts> objects", {
  expect_snapshot(error = TRUE, {
    lag(ts(1:10))
  })
})

test_that("lead and lag preserve factors", {
  x <- factor(c("a", "b", "c"))

  expect_equal(levels(lead(x)), c("a", "b", "c"))
  expect_equal(levels(lag(x)), c("a", "b", "c"))
})

test_that("lead and lag preserves dates and times", {
  x <- as.Date("2013-01-01") + 1:3
  y <- as.POSIXct(x)

  expect_s3_class(lead(x), "Date")
  expect_s3_class(lag(x), "Date")

  expect_s3_class(lead(y), "POSIXct")
  expect_s3_class(lag(y), "POSIXct")
})



test_that("`lead()` / `lag()` validate `n`", {
  expect_snapshot(error = TRUE, {
    lead(1:5, n = 1:2)
    lead(1:5, -1)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, n = 1:2)
    lag(1:5, -1)
  })
})

test_that("`lead()` / `lag()` reject non-numeric `n`", {
  expect_snapshot(error = TRUE, {
    lead(1:5, n = "1")
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, n = "1")
  })
})

test_that("`lead()` / `lag()` check for empty dots", {
  expect_snapshot(error = TRUE, {
    lead(1:5, deault = 1)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, deault = 1)
  })
})

test_that("`lead()` / `lag()` require that `x` is a vector", {
  expect_snapshot(error = TRUE, {
    lead(environment())
  })
  expect_snapshot(error = TRUE, {
    lag(environment())
  })
})

test_that("works with all 4 combinations of with/without `default` and lag/lead", {
  x <- 1:5

  expect_identical(lag(x, n = 2L), c(NA, NA, 1L, 2L, 3L))
  expect_identical(lag(x, n = 2L, default = 0L), c(0L, 0L, 1L, 2L, 3L))

  expect_identical(lead(x, n = 2L), c(3L, 4L, 5L, NA, NA))
  expect_identical(lead(x, n = 2L, default = 0L), c(3L, 4L, 5L, 0L, 0L))
})

test_that("works with size 0 input", {
  x <- integer()

  expect_identical(lag(x, n = 2L), x)
  expect_identical(lag(x, n = 2L, default = 3L), x)
  expect_identical(lead(x, n = 2L), x)
  expect_identical(lead(x, n = 2L, default = 3L), x)
})

test_that("works with `n = 0` with and without `default`", {
  x <- 1:5

  expect_identical(lag(x, n = 0L), x)
  expect_identical(lag(x, n = 0L), x)
  expect_identical(lead(x, n = 0L, default = -1L), x)
  expect_identical(lead(x, n = 0L, default = -1L), x)

  x <- integer()

  expect_identical(lag(x, n = 0L), x)
  expect_identical(lag(x, n = 0L), x)
  expect_identical(lead(x, n = 0L, default = -1L), x)
  expect_identical(lead(x, n = 0L, default = -1L), x)
})

test_that("throws error when data.frames are supplied", {
  df <- data.frame(a = 1:3, b = letters[1:3])

  expect_snapshot(error = TRUE, { lead(df) })
  expect_snapshot(error = TRUE, { lag(df) })

})

test_that("throws error when matrices are supplied", {
  mx <- matrix(1:6, nrow=3)

  expect_snapshot(error = TRUE, { lead(mx) })
  expect_snapshot(error = TRUE, { lag(mx) })
})

test_that("`default` is cast to the type of `x`", {
  expect_identical(lag(1L, default = 2), 2L)

  expect_snapshot(error = TRUE, {
    lag(1L, default = 1.5)
  })

  expect_snapshot(error = TRUE, {
    lead(1L, default = 1.5)
  })
})

test_that("`lead()` / `lag()` require scalar `default`", {
  expect_snapshot(error = TRUE, {
    lead(1:5, default = 1:2)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, default = 1:2)
  })
})


test_that("`default = NA` is typed to match `x`", {
  expect_identical(typeof(lag(1L, default = NA)), "integer")
  expect_identical(typeof(lead(1L, default = NA)), "integer")

  x_date <- as.Date("2020-01-01") + 0:2
  expect_s3_class(lag(x_date, default = NA), "Date")
  expect_s3_class(lead(x_date, default = NA), "Date")
})

test_that("`default` must be size 1 (#5641)", {
  expect_snapshot(error = TRUE, {
    shift(1:5, default = 1:2)
  })
  expect_snapshot(error = TRUE, {
    shift(1:5, default = integer())
  })
})

test_that("`n` is validated", {
  expect_snapshot(error = TRUE, {
    shift(1, n = 1:2)
  })
})



# List Inputs -------------------------------------------------------------

test_that("lead and lag work on lists", {
  x <- list(a = 1, b = 1000, c("AA", "BB", "CC"))
  expect_equal(lag(x), list(NULL, a=1, b = 1000))
  expect_equal(lead(x), list(b = 1000, c("AA", "BB", "CC"), NULL))

  # What about when n > length of x
  expect_equal(lag(x, n=5), list(NULL, NULL, NULL))
  expect_equal(lead(x, n=5), list(NULL, NULL, NULL))
})

test_that("lag() works on list input with default NULL padding", {
  x <- list("a", "b", "c")

  expect_identical(
    lag(x),
    list(NULL, "a", "b")
  )
})

test_that("lead() works on list input with default NULL padding", {
  x <- list("a", "b", "c")

  expect_identical(
    lead(x),
    list("b", "c", NULL)
  )
})

test_that("lag() preserves list elements as-is", {
  x <- list(
    1:3,
    data.frame(a = 1:2),
    list(inner = "value")
  )

  expect_identical(
    lag(x),
    list(NULL, 1:3, data.frame(a = 1:2))
  )
})

test_that("lead() preserves list elements as-is", {
  x <- list(
    1:3,
    data.frame(a = 1:2),
    list(inner = "value")
  )

  expect_identical(
    lead(x),
    list(data.frame(a = 1:2), list(inner = "value"), NULL)
  )
})

test_that("lag() supports n greater than 1 for list input", {
  x <- list("a", "b", "c", "d")

  expect_identical(
    lag(x, n = 2),
    list(NULL, NULL, "a", "b")
  )
})

test_that("lead() supports n greater than 1 for list input", {
  x <- list("a", "b", "c", "d")

  expect_identical(
    lead(x, n = 2),
    list("c", "d", NULL, NULL)
  )
})

test_that("lag() supports n = 0 for list input", {
  x <- list("a", "b", "c")

  expect_identical(
    lag(x, n = 0),
    x
  )
})

test_that("lead() supports n = 0 for list input", {
  x <- list("a", "b", "c")

  expect_identical(
    lead(x, n = 0),
    x
  )
})

test_that("lag() returns all NULL padding when n equals list length", {
  x <- list("a", "b", "c")

  expect_identical(
    lag(x, n = 3),
    list(NULL, NULL, NULL)
  )
})

test_that("lead() returns all NULL padding when n equals list length", {
  x <- list("a", "b", "c")

  expect_identical(
    lead(x, n = 3),
    list(NULL, NULL, NULL)
  )
})

test_that("lag() returns all NULL padding when n exceeds list length", {
  x <- list("a", "b", "c")

  expect_identical(
    lag(x, n = 10),
    list(NULL, NULL, NULL)
  )
})

test_that("lead() returns all NULL padding when n exceeds list length", {
  x <- list("a", "b", "c")

  expect_identical(
    lead(x, n = 10),
    list(NULL, NULL, NULL)
  )
})

test_that("lag() supports explicit default padding for list input", {
  x <- list("a", "b", "c")

  expect_identical(
    lag(x, default = "missing"),
    list("missing", "a", "b")
  )
})

test_that("lead() supports explicit default padding for list input", {
  x <- list("a", "b", "c")

  expect_identical(
    lead(x, default = "missing"),
    list("b", "c", "missing")
  )
})

test_that("lag() supports explicit list default padding", {
  x <- list("a", "b", "c")
  default <- list(value = "missing")

  expect_identical(
    lag(x, default = default),
    list(default, "a", "b")
  )
})

test_that("lead() supports explicit list default padding", {
  x <- list("a", "b", "c")
  default <- list(value = "missing")

  expect_identical(
    lead(x, default = default),
    list("b", "c", default)
  )
})

test_that("lag() works on empty lists", {
  x <- list()

  expect_identical(
    lag(x),
    list()
  )
})

test_that("lead() works on empty lists", {
  x <- list()

  expect_identical(
    lead(x),
    list()
  )
})

test_that("lag() works on length-one lists", {
  x <- list("a")

  expect_identical(
    lag(x),
    list(NULL)
  )
})

test_that("lead() works on length-one lists", {
  x <- list("a")

  expect_identical(
    lead(x),
    list(NULL)
  )
})

test_that("lag() preserves NULL values already present in the input", {
  x <- list("a", NULL, "c")

  expect_identical(
    lag(x),
    list(NULL, "a", NULL)
  )
})

test_that("lead() preserves NULL values already present in the input", {
  x <- list("a", NULL, "c")

  expect_identical(
    lead(x),
    list(NULL, "c", NULL)
  )
})
