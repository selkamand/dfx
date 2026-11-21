test_that("summarise happy path works (ungrouped)", {
  dfx_summary <- summarise(mtcars, function(d) {
    list(mean_mpg = round(mean(d$mpg), digits = 0), max_cyl = max(d$cyl))
  })
  expect_identical(dfx_summary, data.frame(mean_mpg = 20, max_cyl = 8))
})

test_that("summarise happy path works (grouped)", {
  dfx_summary <- summarise(
    iris,
    function(df) {
      list(
        widest_petal = max(df$Petal.Width),
        longest_petal = max(df$Petal.Length)
      )
    },
    by = "Species"
  )

  expect_identical(
    dfx_summary,
    data.frame(
      Species = factor(c("setosa", "versicolor", "virginica")),
      widest_petal = c(0.6, 1.8, 2.5),
      longest_petal = c(1.9, 5.1, 6.9)
    )
  )
})

test_that("summarise calls fun once with the whole data frame", {
  data <- data.frame(
    id = 1:3,
    value = c(2, NA_real_, 8),
    row.names = c("row-a", "row-b", "row-c")
  )
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L

  result <- summarise(
    data,
    function(d = NULL) {
      observed$calls <- observed$calls + 1L
      observed$data <- d
      list(rows = nrow(d), average = mean(d$value, na.rm = TRUE))
    },
    by = NULL
  )

  expect_identical(observed$calls, 1L)
  expect_identical(observed$data, data)
  expect_identical(result, data.frame(rows = 3L, average = 5))
})

test_that("summarise passes empty rows or columns to an ungrouped callback", {
  expect_identical(
    summarise(data.frame(value = integer()), function(d) {
      list(rows = nrow(d), total = sum(d$value))
    }),
    data.frame(rows = 0L, total = 0L)
  )
  expect_identical(
    summarise(data.frame(row.names = c("a", "b")), function(d) {
      list(rows = nrow(d), columns = ncol(d))
    }),
    data.frame(rows = 2L, columns = 0L)
  )
})

test_that("summarise preserves atomic metric types and attributes", {
  dates <- as.Date("2024-01-01") + 0:2
  data <- data.frame(
    key = c("b", "a", "b"),
    count = 1:3,
    score = c(1.5, NA_real_, 3.5),
    flag = c(FALSE, NA, TRUE),
    label = c("first", NA_character_, "last"),
    rating = ordered(c("low", "high", "low"), levels = c("low", "high")),
    date = dates,
    time = as.POSIXct(dates, tz = "Pacific/Auckland"),
    phase = c(1 + 2i, NA_complex_, 3 + 4i),
    byte = as.raw(c(1, 2, 3))
  )
  last_values <- function(d) {
    lapply(d[names(d) != "key"], function(column) column[nrow(d)])
  }

  expected_ungrouped <- data[3L, names(data) != "key", drop = FALSE]
  rownames(expected_ungrouped) <- NULL
  expect_identical(summarise(data, last_values), expected_ungrouped)

  expected_grouped <- data[c(3L, 2L), , drop = FALSE]
  rownames(expected_grouped) <- NULL
  expect_identical(summarise(data, last_values, by = "key"), expected_grouped)
})

test_that("summarise calls fun once per observed group in first-appearance order", {
  data <- data.frame(
    key = factor(
      c("b", "a", "b", "c", "a"),
      levels = c("a", "c", "b", "unused")
    ),
    value = c(4L, 10L, 9L, 22L, 5L),
    row.names = paste0("row-", 1:5)
  )
  observed <- new.env(parent = emptyenv())
  observed$groups <- list()

  result <- summarise(
    data,
    function(d) {
      observed$groups <- c(observed$groups, list(d))
      list(rows = nrow(d), total = sum(d$value))
    },
    by = "key"
  )

  expect_identical(
    observed$groups,
    list(
      data[c(1L, 3L), , drop = FALSE],
      data[c(2L, 5L), , drop = FALSE],
      data[4L, , drop = FALSE]
    )
  )
  expect_identical(
    result,
    data.frame(
      key = data$key[c(1L, 2L, 4L)],
      rows = c(2L, 2L, 1L),
      total = c(13L, 15L, 22L)
    )
  )
})

test_that("summarise groups by distinct key combinations in by column order", {
  data <- data.frame(
    region = c("c", "b:c", "c", "c", "b:c"),
    site = c("a:b", "a", "a:b", "a", "a:b"),
    value = c(2L, 5L, 11L, 23L, 47L)
  )

  result <- summarise(
    data,
    function(d) list(total = sum(d$value)),
    by = c("site", "region", "site")
  )

  expect_identical(
    result,
    data.frame(
      site = c("a:b", "a", "a", "a:b"),
      region = c("c", "b:c", "c", "b:c"),
      total = c(13L, 5L, 23L, 47L)
    )
  )
})

test_that("summarise groups missing, literal NA, and empty keys separately", {
  data <- data.frame(
    key = c(NA_character_, "NA", "", NA_character_, "NA"),
    value = c(2L, 5L, 11L, 23L, 47L)
  )

  expect_identical(
    summarise(data, function(d) list(total = sum(d$value)), by = "key"),
    data.frame(key = c(NA_character_, "NA", ""), total = c(25L, 52L, 11L))
  )
})

test_that("summarise passes one-column groups as data frames", {
  dates <- as.Date("2024-01-01") + c(1, 0, 1)
  data <- data.frame(date = dates)

  expect_identical(
    summarise(data, function(d) list(rows = nrow(d)), by = "date"),
    data.frame(date = dates[1:2], rows = c(2L, 1L))
  )
})

test_that("summarise returns empty grouping columns when there are no groups", {
  data <- data.frame(
    key = factor(character(), levels = c("a", "b")),
    date = as.Date(character()),
    value = integer()
  )

  expect_identical(
    summarise(
      data,
      function(d) stop("callback was called"),
      by = c("date", "key")
    ),
    data[c("date", "key")]
  )
})

test_that("summarise errors when metric order changes across groups", {
  data <- data.frame(key = c("a", "b"))
  # We flip the order of metrics `total` and `label` across different groups
  # to test whether dfx catches this error.
  metrics <- function(d) {
    if (d$key == "a") {
      list(total = 1L, label = "a")
    } else {
      list(label = "b", total = 2)
    }
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
})

test_that("summarise errors when metric type changes between groups", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    if (d$key == "a") {
      list(total = 1L, label = "a")
    } else {
      list(total = "b", label = 2)
    }
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
})

test_that("summarise rejects integer and double metrics across groups", {
  data <- data.frame(key = c("b", "a"), value = 1:2)
  metrics <- function(d) {
    if (d$key == "b") {
      list(total = sum(d$value))
    } else {
      list(total = mean(d$value))
    }
  }

  expect_snapshot(
    summarise(data, metrics, by = "key"),
    error = TRUE,
    cran = TRUE
  )
})

test_that("summarise rejects different metric counts across groups", {
  data <- data.frame(key = c("b", "a"), value = 1:2)
  metrics <- function(d) {
    if (d$key == "b") {
      list(total = sum(d$value))
    } else {
      list(total = sum(d$value), rows = nrow(d))
    }
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data[2:1, ], metrics, by = "key")
  })
})

test_that("summarise rejects a non-factor metric after a factor metric", {
  data <- data.frame(key = c("b", "a", "c"), value = 1:3)
  metrics <- function(d) {
    if (d$key == "c") {
      list(risk = d$value)
    } else {
      list(risk = factor("low", levels = c("low", "high")))
    }
  }

  expect_snapshot(
    error = TRUE,
    cran = TRUE,
    summarise(data, metrics, by = "key"),
  )
})

test_that("summarise rejects factor and ordered factor metrics across groups", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    list(
      risk = factor(
        "low",
        levels = c("low", "high"),
        ordered = d$key == "b"
      )
    )
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data[2:1, , drop = FALSE], metrics, by = "key")
  })
})

test_that("summarise rejects ordered factors with different level orders", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    levels <- c("low", "high")
    if (d$key == "b") {
      levels <- rev(levels)
    }
    list(risk = ordered("low", levels = levels))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
})

test_that("summarise requires data and fun to be supplied", {
  data <- data.frame(value = 1L)
  fun <- function(d) list(rows = nrow(d))

  expect_snapshot(error = TRUE, {
    summarise(fun = fun)
  })
  expect_snapshot(error = TRUE, {
    summarise(data)
  })
})

test_that("summarise rejects invalid data before calling fun", {
  fun <- function(d) stop("callback was called")

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(NULL, fun)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(matrix(1:4, ncol = 2L), fun)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(list(value = 1:4), fun)
  })
})

test_that("summarise requires a function with one explicit parameter", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, NULL)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, "mean")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function() list(total = 3L))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d, other = NULL) list(total = sum(d$value)))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d, ...) list(total = sum(d$value)))
  })
})

test_that("summarise validates grouping columns before calling fun", {
  data <- data.frame(key = c("b", "a"))
  data$matrix_key <- matrix(1:4, nrow = 2L)
  fun <- function(d) stop("callback was called")

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = 1L)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = character())
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = c("key", "unknown", NA_character_))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = c("key", "matrix_key"))
  })
})

test_that("summarise rejects array-valued grouping columns", {
  data <- data.frame(key = c("b", "a"))
  data$array_key <- array(1:4, dim = c(2L, 1L, 2L))
  fun <- function(d) stop("callback was called")

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = c("key", "array_key"))
  })
})

test_that("summarise rejects blank and missing grouping column names", {
  data <- setNames(data.frame(value = 1:2, other = 3:4), c("", NA_character_))
  fun <- function(d) stop("callback was called")

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = "")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = NA_character_)
  })
})

test_that("summarise preserves the message of an ungrouped callback error", {
  data <- data.frame(value = 1L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) stop("cannot compute total"))
  })
})

test_that("summarise stops processing groups after a callback error", {
  data <- data.frame(key = c("b", "a", "c"))
  observed <- new.env(parent = emptyenv())
  observed$keys <- character()
  fun <- function(d) {
    key <- d$key[[1L]]
    observed$keys <- c(observed$keys, key)
    if (key == "a") {
      stop("cannot compute group total")
    }
    list(rows = nrow(d))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = "key")
  })
  expect_identical(observed$keys, c("b", "a"))
})

test_that("summarise reports all three grouping values when fun errors", {
  data <- data.frame(
    site = c("west", "east", "east"),
    year = c(2024L, 2025L, 2025L),
    active = c(FALSE, TRUE, TRUE),
    value = c(1L, 2L, NA_integer_)
  )
  fun <- function(d) {
    if (anyNA(d$value)) {
      stop("cannot compute group total")
    }
    list(total = sum(d$value))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = c("site", "year", "active"))
  })
})

test_that("summarise requires fun to return a list", {
  data <- data.frame(value = 1L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) NULL)
  })
  expect_snapshot(
    summarise(data, function(d) c(total = sum(d$value))),
    error = TRUE,
    cran = TRUE,
    transform = function(lines) {
      sub("(`fun` must return a list),.*", "\\1", lines)
    }
  )
})

test_that("summarise rejects unnamed lists and NA metric names", {
  data <- data.frame(value = 1L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) list(nrow(d)))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) {
      setNames(list(1L, 2L), c("total", NA_character_))
    })
  })
})

test_that("summarise rejects metrics that have zero or multiple values", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) list(total = integer()))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) list(rows = nrow(d), bounds = range(d$value)))
  })
})

test_that("summarise rejects a NULL return from a group", {
  data <- data.frame(key = "a")
  fun <- function(d) NULL

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = "key")
  })
})

test_that("summarise rejects a non-list return from a group", {
  data <- data.frame(key = "a")
  fun <- function(d) 42L

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = "key")
  })
})

test_that("summarise rejects a list without names from a group", {
  data <- data.frame(key = "a")
  fun <- function(d) list(1L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = "key")
  })
})

test_that("summarise rejects an empty metric name in a group", {
  data <- data.frame(key = "a")
  fun <- function(d) list(a = 1L, 2L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = "key")
  })
})

test_that("summarise rejects a missing metric name in a group", {
  data <- data.frame(key = "a")
  fun <- function(d) setNames(list(1L), NA_character_)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = "key")
  })
})

test_that("summarise rejects a zero-length metric in a group", {
  data <- data.frame(key = "a")
  fun <- function(d) list(total = integer())

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, fun, by = "key")
  })
})

test_that("summarise validates results from later groups", {
  data <- data.frame(key = c("b", "a", "a"), value = 1:3)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) list(total = d$value), by = "key")
  })
})

test_that("summarise reports all three grouping values for an invalid return", {
  data <- data.frame(
    site = c("west", "east", "east"),
    year = c(2024L, 2025L, 2025L),
    active = c(FALSE, TRUE, TRUE),
    value = 1:3
  )

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(
      data,
      function(d) list(total = d$value),
      by = c("site", "year", "active")
    )
  })
})

test_that("summarise rejects inconsistent metric sets across groups", {
  data <- data.frame(key = c("b", "a"), value = c(2L, 7L))
  metrics <- function(d) setNames(list(sum(d$value)), d$key[[1L]])

  expect_snapshot(error = TRUE, {
    summarise(data, metrics, by = "key")
  })
})

test_that("summarise rejects empty metric names", {
  data <- data.frame(value = 1L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) list(total = sum(d$value), nrow(d)))
  })
})

test_that("summarise rejects non-atomic metrics in both modes", {
  # skip("The atomic metric check currently allows list and data frame values")
  data <- data.frame(key = c("b", "a"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) list(total = list(sum(d$value))))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(
      data,
      function(d) list(total = data.frame(value = 1:2)),
      by = "key"
    )
  })
})

test_that("summarise rejects a character matrix as by", {
  data <- data.frame(key = c("b", "a"))

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) list(rows = nrow(d)), by = matrix("key"))
  })
})

test_that("summarise does not do name repair on metric names", {
  data <- data.frame(group = c("a", "b"))
  metrics <- function(d) {
    list(`mean mpg` = 10)
  }

  # Test without grouping
  expect_identical(
    summarise(data, metrics),
    data.frame(
      `mean mpg` = 10,
      check.names = FALSE
    )
  )

  # Test when grouping
  expect_identical(
    summarise(data, metrics, by = "group"),
    data.frame(
      group = c("a", "b"),
      `mean mpg` = c(10, 10),
      check.names = FALSE
    )
  )
})


test_that("summarise rejects metric names that match grouping columns", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  metrics <- function(d) {
    list(group = sum(d$value))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "group")
  })
})

test_that("summarise rejects duplicate metric names (ungrouped)", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))

  metrics <- function(d) {
    setNames(list(sum(d$value), mean(d$value)), c("total", "total"))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics)
  })
})

test_that("summarise rejects duplicate metric names (grouped)", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))

  metrics <- function(d) {
    setNames(list(sum(d$value), mean(d$value)), c("total", "total"))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "group")
  })
})
