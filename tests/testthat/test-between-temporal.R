# Most expected values were checked against dplyr::between() 1.2.1. Tests for
# unsupported input types document intentional differences. No test loads dplyr.

test_that("between includes Date endpoints and preserves missing values", {
  x <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", NA))

  expect_identical(
    between(x, as.Date("2024-01-01"), as.Date("2024-01-02")),
    c(TRUE, TRUE, FALSE, NA)
  )
})

test_that("between uses elementwise Date bounds and three-valued logic", {
  x <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  left <- as.Date(c("2024-01-01", NA, "2024-01-04"))

  expect_identical(
    between(x, left, as.Date("2024-01-03")),
    c(TRUE, NA, FALSE)
  )
})

test_that("between handles empty and named Date vectors", {
  expect_identical(
    between(as.Date(character()), as.Date("2024-01-01"), as.Date("2024-01-02")),
    logical()
  )

  x <- setNames(as.Date(c("2024-01-01", "2024-01-02")), c("a", "b"))
  expect_identical(
    between(x, as.Date("2024-01-01"), as.Date("2024-01-02")),
    c(TRUE, TRUE)
  )
})

test_that("between includes datetime endpoints and preserves missing values", {
  x <- as.POSIXct(
    c("2024-01-01 09:00:00", "2024-01-01 12:00:00", "2024-01-01 17:00:00", NA),
    tz = "UTC"
  )
  left <- as.POSIXct("2024-01-01 09:00:00", tz = "UTC")
  right <- as.POSIXct("2024-01-01 17:00:00", tz = "UTC")

  expect_identical(between(x, left, right), c(TRUE, TRUE, TRUE, NA))
})

test_that("between compares datetime instants across time zones", {
  x <- as.POSIXct(
    c(
      "2023-12-31 23:59:59",
      "2024-01-01 00:00:00",
      "2024-01-02 00:00:00",
      "2024-01-02 00:00:01"
    ),
    tz = "UTC"
  )
  left <- as.POSIXct("2024-01-01 13:00:00", tz = "Pacific/Auckland")
  right <- as.POSIXct("2024-01-01 19:00:00", tz = "America/New_York")

  expect_identical(between(x, left, right), c(FALSE, TRUE, TRUE, FALSE))
})

test_that("between compares POSIXlt and POSIXct values", {
  x <- as.POSIXct(
    c("2024-01-01 12:00:00", "2024-01-02 12:00:00", NA),
    tz = "UTC"
  )
  left <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  right <- as.POSIXct("2024-01-02 00:00:00", tz = "UTC")

  expect_identical(between(as.POSIXlt(x), left, right), c(TRUE, FALSE, NA))
  expect_identical(
    between(x, as.POSIXlt(left), as.POSIXlt(right)),
    c(TRUE, FALSE, NA)
  )
})

test_that("between accepts bare missing bounds for temporal x", {
  dates <- as.Date(c("2024-01-01", "2024-01-02"))
  times <- as.POSIXct(
    c("2024-01-01 12:00:00", "2024-01-02 12:00:00"),
    tz = "UTC"
  )

  expect_identical(
    between(dates, NA, as.Date("2024-01-01")),
    c(NA, FALSE)
  )
  expect_identical(
    between(times, NA, as.POSIXct("2024-01-02 00:00:00", tz = "UTC")),
    c(NA, FALSE)
  )
})

test_that("between rejects classed logical missing temporal bounds", {
  dates <- as.Date(c("2024-01-02", "2024-01-03"))
  times <- as.POSIXct(
    c("2024-01-02 12:00:00", "2024-01-03 12:00:00"),
    tz = "UTC"
  )
  classed <- structure(NA, class = "foo")

  expect_error(
    between(dates, classed, as.Date("2024-01-03")),
    "`left` must be Date or missing when `x` is Date.",
    fixed = TRUE
  )
  expect_error(
    between(times, classed, as.POSIXct("2024-01-03 00:00:00", tz = "UTC")),
    "`left` must be POSIXt or missing when `x` is POSIXt.",
    fixed = TRUE
  )
  expect_error(
    between(dates, as.Date("2024-01-02"), classed),
    "`right` must be Date or missing when `x` is Date.",
    fixed = TRUE
  )
})

test_that("between rejects S4 logical missing temporal bounds", {
  methods::setClass("dfx_test_logical_s4", contains = "logical")
  on.exit(methods::removeClass("dfx_test_logical_s4"), add = TRUE)
  bound <- methods::new("dfx_test_logical_s4", NA)

  expect_error(
    between(as.Date("2024-01-02"), bound, as.Date("2024-01-03")),
    "`left` must be Date or missing when `x` is Date.",
    fixed = TRUE
  )
  expect_error(
    between(
      as.POSIXct("2024-01-02 12:00:00", tz = "UTC"),
      as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
      bound
    ),
    "`right` must be POSIXt or missing when `x` is POSIXt.",
    fixed = TRUE
  )
})

test_that("between handles each temporal missing-bound type like dplyr", {
  date <- as.Date(c("2023-12-31", "2024-01-02", "2024-01-04"))
  time <- as.POSIXct(
    c("2023-12-31 12:00:00", "2024-01-02 12:00:00", "2024-01-04 12:00:00"),
    tz = "UTC"
  )
  cases <- list(
    Date = list(
      x = date,
      left = as.Date("2024-01-01"),
      right = as.Date("2024-01-03")
    ),
    POSIXct = list(
      x = time,
      left = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
      right = as.POSIXct("2024-01-03 00:00:00", tz = "UTC")
    ),
    POSIXlt = list(
      x = as.POSIXlt(time),
      left = as.POSIXlt(as.POSIXct("2024-01-01 00:00:00", tz = "UTC")),
      right = as.POSIXlt(as.POSIXct("2024-01-03 00:00:00", tz = "UTC"))
    )
  )
  missing <- list(
    logical = NA,
    Date = as.Date(NA),
    POSIXct = as.POSIXct(NA, tz = "UTC"),
    POSIXlt = as.POSIXlt(NA, tz = "UTC")
  )

  for (kind in names(cases)) {
    case <- cases[[kind]]
    expect_identical(
      between(case$x, case$left, case$right),
      c(FALSE, TRUE, FALSE),
      info = kind
    )

    for (type in names(missing)) {
      bound <- missing[[type]]
      expect_identical(
        between(case$x, bound, case$right),
        c(NA, NA, FALSE),
        info = paste(kind, type, "left")
      )
      expect_identical(
        between(case$x, case$left, bound),
        c(FALSE, NA, NA),
        info = paste(kind, type, "right")
      )
    }

    for (left_type in names(missing)) {
      for (right_type in names(missing)) {
        expect_identical(
          between(case$x, missing[[left_type]], missing[[right_type]]),
          c(NA, NA, NA),
          info = paste(kind, left_type, right_type)
        )
      }
    }
  }
})

test_that("between handles elementwise temporal missing bounds like dplyr", {
  x <- as.Date(c("2023-12-31", "2024-01-02", "2024-01-04"))
  left <- as.Date("2024-01-01")
  right <- as.Date("2024-01-03")

  expect_identical(between(x, c(NA, NA, NA), right), c(NA, NA, FALSE))
  expect_identical(between(x, left, c(NA, NA, NA)), c(FALSE, NA, NA))
  expect_identical(
    between(x, as.Date(c("2024-01-01", NA, "2024-01-01")), right),
    c(FALSE, NA, FALSE)
  )
  expect_identical(
    between(x, left, as.Date(c("2024-01-03", NA, "2024-01-03"))),
    c(FALSE, NA, FALSE)
  )
})

test_that("between rejects incompatible typed missing temporal bounds like dplyr", {
  date <- as.Date("2024-01-02")
  time <- as.POSIXct("2024-01-02 12:00:00", tz = "UTC")
  date_left <- as.Date("2024-01-01")
  date_right <- as.Date("2024-01-03")
  time_left <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  time_right <- as.POSIXct("2024-01-03 00:00:00", tz = "UTC")

  expect_snapshot(error = TRUE, cran = TRUE, {
    between(date, NA_integer_, date_right)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(date, date_left, NA_integer_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(date, NA_real_, date_right)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(date, date_left, NA_real_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(date, NA_complex_, date_right)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(date, date_left, NA_complex_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(date, NA_character_, date_right)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(date, date_left, NA_character_)
  })

  expect_snapshot(error = TRUE, cran = TRUE, {
    between(time, NA_integer_, time_right)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(time, time_left, NA_integer_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(time, NA_real_, time_right)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(time, time_left, NA_real_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(time, NA_complex_, time_right)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(time, time_left, NA_complex_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(time, NA_character_, time_right)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(time, time_left, NA_character_)
  })
})

test_that("between rejects incompatible temporal bounds and lengths", {
  dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))

  expect_error(
    between(dates, "2024-01-01", as.Date("2024-01-03")),
    "`left` must be Date",
    fixed = TRUE
  )
  expect_error(
    between(
      dates,
      as.Date(c("2024-01-01", "2024-01-02")),
      as.Date("2024-01-03")
    ),
    "`left` must have length 1 or the same length as `x`.",
    fixed = TRUE
  )
})

test_that("between rejects non-temporal x with Date or datetime bounds", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(1:2, as.Date("2024-01-01"), as.Date("2024-01-02"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(
      NA_real_,
      as.POSIXct("2024-01-01", tz = "UTC"),
      as.POSIXct("2024-01-02", tz = "UTC")
    )
  })
})

test_that("between handles Date x with datetime bounds", {
  x <- as.Date(c("2024-01-01", "2024-01-02", NA))
  left <- as.POSIXct("2024-01-01 00:00:00", tz = "Pacific/Auckland")
  right <- as.POSIXct("2024-01-02 00:00:00", tz = "Pacific/Auckland")

  expect_identical(between(x, left, right), c(TRUE, TRUE, NA))
})

test_that("between converts Date x to local midnight outside UTC", {
  # Before R 4.3.0, as.POSIXlt.Date() ignored its tz argument, shifting the
  # Date by 13 hours in Auckland and placing it after the right bound.
  x <- as.Date("2024-01-01")
  left <- as.POSIXct("2024-01-01 00:00:00", tz = "Pacific/Auckland")
  right <- as.POSIXct("2024-01-01 12:00:00", tz = "Pacific/Auckland")

  expect_true(between(x, left, right))

  reversed <- as.POSIXct("2024-01-01 00:30:00", tz = "Pacific/Auckland")
  expect_true(between(reversed, as.Date("2024-01-01"), right))
})

test_that("between uses the first datetime bound timezone for Date x", {
  x <- as.Date(c("2024-01-01", "2024-01-02"))
  left <- as.POSIXct("2024-01-01 00:00:00", tz = "America/New_York")
  right <- as.POSIXct("2024-01-02 00:00:00", tz = "Pacific/Auckland")

  expect_identical(between(x, left, right), c(TRUE, FALSE))
})

test_that("between converts a Date bound with Date x and a datetime bound", {
  x <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  left <- as.Date("2024-01-01")
  right <- as.POSIXct("2024-01-02 00:00:00", tz = "Pacific/Auckland")

  expect_identical(between(x, left, right), c(TRUE, TRUE, FALSE))
})

test_that("between converts Date x at local midnight across daylight saving", {
  x <- as.Date(c("2024-03-09", "2024-03-10", "2024-03-11"))
  left <- as.POSIXct("2024-03-10 00:00:00", tz = "America/New_York")
  right <- as.POSIXct("2024-03-11 00:00:00", tz = "America/New_York")

  expect_identical(between(x, left, right), c(FALSE, TRUE, TRUE))
})

test_that("between handles datetime x with Date bounds", {
  x <- as.POSIXct(
    c("2024-01-01 12:00:00", "2024-01-02 12:00:00", NA),
    tz = "Pacific/Auckland"
  )

  expect_identical(
    between(x, as.Date("2024-01-01"), as.Date("2024-01-02")),
    c(TRUE, FALSE, NA)
  )
})

test_that("between handles POSIXlt x with Date bounds", {
  x <- as.POSIXlt(as.POSIXct(
    c("2024-01-01 12:00:00", "2024-01-02 12:00:00", NA),
    tz = "Pacific/Auckland"
  ))

  expect_identical(
    between(x, as.Date("2024-01-01"), as.Date("2024-01-02")),
    c(TRUE, FALSE, NA)
  )
})

test_that("between handles every Date and datetime bound combination", {
  dates <- as.Date(c("2024-01-01", "2024-01-02", NA))
  times <- as.POSIXct(
    c("2024-01-01 12:00:00", "2024-01-02 12:00:00", NA),
    tz = "Pacific/Auckland"
  )
  date_left <- as.Date("2024-01-01")
  time_left <- as.POSIXct("2024-01-01 00:00:00", tz = "Pacific/Auckland")
  date_right <- as.Date("2024-01-02")
  time_right <- as.POSIXct("2024-01-02 00:00:00", tz = "Pacific/Auckland")

  expect_identical(between(dates, time_left, date_right), c(TRUE, TRUE, NA))
  expect_identical(between(times, date_left, time_right), c(TRUE, FALSE, NA))
  expect_identical(between(times, time_left, date_right), c(TRUE, FALSE, NA))
})

test_that("between uses the x timezone when converting Date bounds", {
  x <- as.POSIXct(
    c("2023-12-31 20:00:00", "2024-01-01 00:00:00"),
    tz = "America/New_York"
  )
  left <- as.Date("2024-01-01")
  right <- as.POSIXct("2024-01-02 00:00:00", tz = "Pacific/Auckland")

  expect_identical(between(x, left, right), c(FALSE, TRUE))
})

test_that("between rejects all-missing non-temporal x with temporal bounds", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(NA, as.Date("2024-01-01"), as.Date("2024-01-02"))
  })

  left <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  right <- as.POSIXct("2024-01-02 00:00:00", tz = "UTC")
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(c(NA, NA), left, right)
  })

  x <- c(first = NA, second = NA)
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(x, as.Date("2024-01-01"), as.Date("2024-01-02"))
  })

  date_left <- as.Date("2024-01-01")
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(c(NA, NA), date_left, "bad")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    between(c(NA, NA), date_left, rep(as.Date("2024-01-02"), 3))
  })
})

test_that("between keeps infinite Date values ordered against datetime bounds", {
  x <- structure(c(-Inf, Inf, NaN, NA_real_), class = "Date")
  left <- as.POSIXct("2024-01-01", tz = "UTC")
  right <- as.POSIXct("2024-12-31", tz = "UTC")

  expect_identical(between(x, left, right), c(FALSE, FALSE, NA, NA))
})

test_that("between uses infinite Date bounds as open ends", {
  x <- as.POSIXct(c("2023-12-31", "2024-06-01", "2025-01-01", NA), tz = "UTC")
  left <- as.POSIXct("2024-01-01", tz = "UTC")
  right <- as.POSIXct("2024-12-31", tz = "UTC")
  negative_infinity <- structure(-Inf, class = "Date")
  positive_infinity <- structure(Inf, class = "Date")

  expect_identical(
    between(x, negative_infinity, right),
    c(TRUE, TRUE, FALSE, NA)
  )
  expect_identical(
    between(x, left, positive_infinity),
    c(FALSE, TRUE, TRUE, NA)
  )
})

test_that("between compares distant Dates when base R can convert them", {
  far_future <- as.Date("9999-12-31") + 1
  converted <- suppressWarnings(
    tryCatch(
      as.POSIXct(as.POSIXlt(far_future, tz = "UTC")),
      error = function(e) as.POSIXct(NA, tz = "UTC")
    )
  )
  skip_if(is.na(converted), "Base R cannot convert year 10000 to POSIXct")

  left <- as.POSIXct("2024-01-01", tz = "UTC")
  right <- as.POSIXct("2024-12-31", tz = "UTC")
  x <- as.POSIXct("2024-06-01", tz = "UTC")

  expect_identical(between(far_future, left, right), FALSE)
  expect_identical(between(x, far_future, right), FALSE)
  expect_identical(between(x, left, far_future), TRUE)
})
