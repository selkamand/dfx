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

test_that("summarise returns one ungrouped integer metric", {
  data <- data.frame(value = c(2L, NA_integer_, 5L))

  result <- summarise(data, function(d) {
    list(total = sum(d$value, na.rm = TRUE))
  })

  expect_identical(result, data.frame(total = 7L))
})

test_that("summarise preserves mixed metrics for one observed factor group", {
  data <- data.frame(
    key = factor(rep("observed", 3), levels = c("observed", "unused")),
    value = c(2L, 4L, 6L),
    date = as.Date("2024-01-01") + 0:2
  )

  result <- summarise(
    data,
    function(d) {
      list(
        rows = nrow(d),
        average = mean(d$value),
        has_large_value = any(d$value > 5L),
        label = paste(d$value, collapse = ","),
        rating = factor("low", levels = c("low", "high")),
        latest = max(d$date),
        span = as.difftime(diff(range(d$value)), units = "mins"),
        byte = as.raw(max(d$value))
      )
    },
    by = "key"
  )

  expect_identical(
    result,
    data.frame(
      key = factor("observed", levels = c("observed", "unused")),
      rows = 3L,
      average = 4,
      has_large_value = TRUE,
      label = "2,4,6",
      rating = factor("low", levels = c("low", "high")),
      latest = as.Date("2024-01-03"),
      span = as.difftime(4, units = "mins"),
      byte = as.raw(6)
    )
  )
})

test_that("summarise returns mixed metrics for observed Date and factor groups", {
  data <- data.frame(
    date = as.Date("2024-01-01") + c(1, 0, 1, 1),
    key = factor(c("b", "b", "b", "a"), levels = c("a", "b", "unused")),
    value = c(2L, 6L, 4L, 8L)
  )

  result <- summarise(
    data,
    function(d) {
      list(
        rows = nrow(d),
        average = mean(d$value),
        has_large_value = any(d$value > 5L),
        label = paste(d$value, collapse = ","),
        total = complex(real = sum(d$value), imaginary = nrow(d))
      )
    },
    by = c("key", "date")
  )

  expect_identical(
    result,
    data.frame(
      key = factor(c("b", "b", "a"), levels = c("a", "b", "unused")),
      date = as.Date("2024-01-01") + c(1, 0, 1),
      rows = c(2L, 1L, 1L),
      average = c(3, 6, 8),
      has_large_value = c(FALSE, TRUE, TRUE),
      label = c("2,4", "6", "8"),
      total = c(6 + 2i, 6 + 1i, 8 + 1i)
    )
  )
})

test_that("summarise preserves ordered levels and typed missing metrics", {
  data <- data.frame(
    key = c("a", "b"),
    rating = ordered(c("high", NA_character_), levels = c("low", "high"))
  )

  result <- summarise(
    data,
    function(d) list(peak = max(d$rating)),
    by = "key"
  )

  expect_identical(
    result,
    data.frame(
      key = c("a", "b"),
      peak = ordered(c("high", NA_character_), levels = c("low", "high"))
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

test_that("summarise counts rows in a data frame with no rows or columns", {
  expect_identical(
    summarise(data.frame(), function(d) list(rows = nrow(d))),
    data.frame(rows = 0L)
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

test_that("summarise preserves Date, factor, and metric types with no groups", {
  data <- data.frame(
    key = factor(character(), levels = c("a", "b")),
    date = as.Date(character()),
    value = integer()
  )

  expect_identical(
    summarise(
      data,
      function(d) list(total = sum(d$value)),
      by = c("date", "key")
    ),
    data.frame(date = data$date, key = data$key, total = integer())
  )
})

test_that("summarise preserves metric columns for zero rows grouped by character", {
  data <- data.frame(g = character(), value = integer())

  result <- summarise(
    data,
    function(d) list(n = nrow(d), total = sum(d$value)),
    by = "g"
  )

  expect_identical(
    result,
    data.frame(g = character(), n = integer(), total = integer())
  )
})

test_that("summarise propagates callback errors for zero rows grouped by character", {
  data <- data.frame(g = character())

  expect_error(
    summarise(
      data,
      function(d) stop("summary callback was evaluated"),
      by = "g"
    ),
    "summary callback was evaluated",
    fixed = TRUE
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

test_that("summarise rejects mixed metric classes backed by doubles", {
  values <- list(
    double = 5,
    Date = as.Date("1970-01-02"),
    POSIXct = as.POSIXct("1970-01-02", tz = "UTC"),
    difftime = as.difftime(2, units = "hours")
  )
  metrics <- function(d) list(value = values[[d$key]])
  expect_incompatible <- function(first, second) {
    expect_error(
      summarise(data.frame(key = c(first, second)), metrics, by = "key"),
      "Metric `value`",
      info = paste(first, "followed by", second)
    )
  }

  expect_incompatible("double", "Date")
  expect_incompatible("Date", "double")
  expect_incompatible("double", "POSIXct")
  expect_incompatible("POSIXct", "double")
  expect_incompatible("double", "difftime")
  expect_incompatible("difftime", "double")
  expect_incompatible("Date", "POSIXct")
  expect_incompatible("POSIXct", "Date")
  expect_incompatible("difftime", "Date")
  expect_incompatible("difftime", "POSIXct")
})

test_that("summarise rejects mixed storage within the same metric class", {
  data <- data.frame(key = c("a", "b"))
  scores <- list(
    a = structure(1L, class = "Date"),
    b = structure(2, class = "Date")
  )
  metrics <- function(d) list(score = scores[[d$key]])

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
    summarise(data[2:1, , drop = FALSE], metrics, by = "key")
  })
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

test_that("summarise combines unordered factor metrics by label and unions levels", {
  data <- data.frame(key = c("a", "b", "c"))
  risk_by_key <- list(
    a = factor("low", levels = c("low", "moderate")),
    b = factor("high", levels = c("high", "low")),
    c = factor("low", levels = c("high", "low", "critical"))
  )
  metrics <- function(d) list(risk = risk_by_key[[d$key]])

  result <- summarise(data, metrics, by = "key")

  expect_identical(class(result$risk), "factor")
  expect_identical(as.character(result$risk), c("low", "high", "low"))
  # Keep the first group's levels, then append unseen levels in encounter order.
  expect_identical(
    levels(result$risk),
    c("low", "moderate", "high", "critical")
  )
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

test_that("summarise rejects a zero-length metric from zero-row input", {
  data <- data.frame(value = integer())

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) list(raw = d$value))
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

test_that("summarise rejects list-valued grouping columns before calling fun", {
  data <- data.frame(value = 1:3)
  data$key <- list(1L, "1", 1)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) stop("callback was called"), by = "key")
  })
})

test_that("summarise separates an NA factor level from missing factor values", {
  key <- factor(c("observed", NA, NA, NA), exclude = NULL)
  # The third row has a missing factor code. The second and fourth rows have
  # a valid code for the explicit NA level. Both forms print as missing.
  is.na(key)[3L] <- TRUE
  data <- data.frame(key = key)

  expect_identical(
    summarise(data, function(d) list(rows = nrow(d)), by = "key"),
    data.frame(key = key[1:3], rows = c(1L, 2L, 1L))
  )
})

test_that("summarise rejects nested data frame grouping columns before calling fun", {
  data <- data.frame(value = 1:3)
  data$key <- data.frame(region = c("north", "north", "south"), site = 1:3)

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) stop("callback was called"), by = "key")
  })
})

test_that("summarise groups by POSIXlt datetime columns", {
  data <- data.frame(value = c(2L, 3L, 5L))
  data$key <- as.POSIXlt(
    as.POSIXct("2024-01-01", tz = "UTC") + c(3600, 0, 3600)
  )
  expected <- data[1:2, "key", drop = FALSE]
  expected$total <- c(7L, 3L)

  expect_identical(
    summarise(data, function(d) list(total = sum(d$value)), by = "key"),
    expected
  )
})

test_that("summarise rejects custom metric classes", {
  distances <- list(
    a = structure(1, class = "distance", unit = "m"),
    b = structure(2, class = "distance", unit = "km")
  )
  metrics <- function(d) list(distance = distances[[d$key]])

  expect_snapshot(
    cran = TRUE,
    error = TRUE,
    summarise(data.frame(key = c("a", "b")), metrics, by = "key")
  )
  expect_snapshot(
    cran = TRUE,
    error = TRUE,
    summarise(data.frame(key = c("b", "a")), metrics, by = "key"),
  )
})

test_that("summarise rejects mixing a factor subclass and a plain factor", {
  risk <- factor("low", levels = c("low", "high"))
  risks <- list(
    a = structure(risk, class = c("risk_rating", "factor")),
    b = risk
  )
  metrics <- function(d) list(risk = risks[[d$key]])

  # The factor levels agree across groups. Row binding can still strip the
  # risk_rating subclass, changing which methods dispatch on the metric column.
  expect_error(
    summarise(data.frame(key = c("a", "b")), metrics, by = "key"),
    "Metric `risk`"
  )
  expect_error(
    summarise(data.frame(key = c("b", "a")), metrics, by = "key"),
    "Metric `risk`"
  )
})

test_that("summarise rejects a one-cell matrix as a scalar metric", {
  data <- data.frame(key = c("a", "b"))
  empty <- data[FALSE, , drop = FALSE]

  # A one-cell matrix passes a length check, but conversion can replace
  # the metric name with the matrix column name.
  metrics <- function(d) {
    list(total = matrix(1L, dimnames = list(NULL, "renamed")))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(empty, metrics, by = "key")
  })
})

test_that("summarise detects duplicate row.names metrics before conversion", {
  data <- data.frame(key = c("a", "b"))
  empty <- data[integer(), , drop = FALSE]
  # Data-frame conversion can use one of these metrics as the row.names
  # argument. A duplicate-name check after conversion misses the lost value.
  metrics <- function(d) setNames(list(1L, 2L), rep("row.names", 2L))

  expect_snapshot(error = TRUE, cran = TRUE, summarise(data, metrics))
  expect_snapshot(
    error = TRUE,
    cran = TRUE,
    summarise(data, metrics, by = "key")
  )
  expect_snapshot(
    error = TRUE,
    cran = TRUE,
    summarise(empty, metrics, by = "key")
  )
})

test_that("ungrouped summarise throws error if fun returns named empty list", {
  data <- data.frame(value = 1:2)
  # An explicitly named empty list describes a zero-column summary. The
  # callback runs once for the whole input, so the summary still has one row.
  metrics <- function(d) setNames(list(), character())

  expect_snapshot(
    error = TRUE,
    cran = TRUE,
    summarise(data, metrics)
  )
})

test_that("summarise distinguishes an NA metric level from a missing code", {
  # Row binding can turn a missing code into the explicit NA level. Compare
  # the factors directly because the missing code and the level print alike.
  metric <- factor(c(NA, NA), exclude = NULL)
  is.na(metric)[2L] <- TRUE
  data <- data.frame(key = c("a", "b"), metric = metric)
  metrics <- function(d) list(metric = d$metric[1L])

  expect_identical(summarise(data, metrics, by = "key"), data)

  ordered_data <- data
  ordered_data$metric <- structure(metric, class = c("ordered", "factor"))
  expect_identical(summarise(ordered_data, metrics, by = "key"), ordered_data)
})

test_that("summarise rejects factor subclasses even when groups agree", {
  metric <- structure(factor(c("a", "b")), class = c("rating", "factor"))
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) list(metric = metric[match(d$key, data$key)])

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data[1L, , drop = FALSE], metrics)
    summarise(data, metrics, by = "key")
  })
})

test_that("summarise rejects custom attributes on supported metric types", {
  data <- data.frame(key = c("a", "b"))
  empty <- data[integer(), , drop = FALSE]
  tagged_integer <- function(d) list(value = structure(1L, unit = "m"))
  tagged_date <- function(d) {
    list(day = structure(as.Date("2024-01-01"), source = "manual"))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, tagged_integer)
    summarise(data, tagged_date, by = "key")
    summarise(empty, tagged_integer, by = "key")
  })
})

test_that("summarise preserves names that overlap conversion internals", {
  data <- data.frame(key = c("a", "b"))
  empty <- data[integer(), , drop = FALSE]
  # R temporarily renames that metric to ..adfl.check.names.
  # After conversion, R strips the ..adfl. prefix from every column that starts with it.
  # That restores check.names and accidentally renames ..adfl.other to other.
  # This test confirms our summarise function doesn't let the internal R
  # data.frame conversion silently change the column names.
  metric_names <- c("check.names", "..adfl.other")
  metrics <- function(d) setNames(list(1L, 2L), metric_names)

  expect_identical(names(summarise(data, metrics)), metric_names)
  expect_identical(
    names(summarise(data, metrics, by = "key")),
    c("key", metric_names)
  )
  expect_identical(
    names(summarise(empty, metrics, by = "key")),
    c("key", metric_names)
  )
})


test_that("grouped summarise throws error if fun returns empty list", {
  data <- data.frame(key = c("b", "a", "b"))
  # Each observed group contributes a row even with zero metrics. If fun
  # returns an empty list then summarise should error.
  metrics <- function(d) setNames(list(), character())

  expect_snapshot(
    error = TRUE,
    cran = TRUE,
    summarise(data, metrics, by = "key")
  )
})


test_that("summarise converts difftime metrics to the first group's units", {
  data <- data.frame(key = c("a", "b"))
  durations <- list(
    as.difftime(1, units = "hours"),
    as.difftime(30, units = "mins")
  )
  metrics <- function(d) {
    list(duration = durations[[match(d$key, data$key)]])
  }

  expect_identical(
    summarise(data, metrics, by = "key"),
    data.frame(
      key = c("a", "b"),
      duration = as.difftime(c(1, 0.5), units = "hours")
    )
  )
  expect_identical(
    summarise(data[2:1, , drop = FALSE], metrics, by = "key"),
    data.frame(
      key = c("b", "a"),
      duration = as.difftime(c(30, 60), units = "mins")
    )
  )
})

test_that("summarise preserves instants across different metric time zones", {
  data <- data.frame(key = c("a", "b"))
  times <- list(
    as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-01-01 00:00:00", tz = "Australia/Sydney")
  )
  metrics <- function(d) {
    list(time = times[[match(d$key, data$key)]])
  }

  # Equal local clock readings in these zones represent different instants.
  instants <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") +
    c(0, -11 * 3600)

  expect_identical(
    summarise(data, metrics, by = "key"),
    data.frame(key = c("a", "b"), time = instants)
  )

  reversed_instants <- instants[2:1]
  attr(reversed_instants, "tzone") <- "Australia/Sydney"

  expect_identical(
    summarise(data[2:1, , drop = FALSE], metrics, by = "key"),
    data.frame(key = c("b", "a"), time = reversed_instants)
  )
})

test_that("summarise keeps nearby numeric grouping keys distinct", {
  keys <- c(1, 1 + 1e-15)
  data <- data.frame(key = keys[c(1L, 2L, 2L)])

  # These distinct doubles can print identically at ordinary precision.
  expect_identical(
    summarise(data, function(d) list(rows = nrow(d)), by = "key"),
    data.frame(key = keys, rows = c(1L, 2L))
  )
})

test_that("summarise groups NA, NaN, and infinities separately", {
  data <- data.frame(
    key = c(NaN, NA_real_, Inf, -Inf, NaN, NA_real_, Inf)
  )

  expect_identical(
    summarise(data, function(d) list(rows = nrow(d)), by = "key"),
    data.frame(
      key = c(NaN, NA_real_, Inf, -Inf),
      rows = c(2L, 2L, 2L, 1L)
    )
  )
})

test_that("ungrouped summarise always returns automatic row names", {
  inputs <- list(
    nonempty = data.frame(
      value = 1:3,
      row.names = c("input-a", "input-b", "input-c")
    ),
    one_row = data.frame(value = 1L, row.names = "input-a"),
    zero_rows = data.frame(value = integer()),
    zero_columns = data.frame(row.names = c("input-a", "input-b")),
    zero_rows_and_columns = data.frame()
  )

  callbacks <- list(
    unnamed_scalars = function(d) {
      list(rows = nrow(d), flag = TRUE)
    },
    named_scalars = function(d) {
      # Inner names describe callback values, not rows of the summary.
      list(
        rows = setNames(nrow(d), "metric-row"),
        flag = setNames(TRUE, "another-metric-row")
      )
    }
  )

  for (input_name in names(inputs)) {
    data <- inputs[[input_name]]

    for (callback_name in names(callbacks)) {
      result <- summarise(data, callbacks[[callback_name]])
      context <- paste(input_name, callback_name)

      expect_identical(
        result,
        data.frame(rows = nrow(data), flag = TRUE),
        info = context
      )

      # Negative values identify automatic row names; explicit "1" does not.
      expect_identical(
        .row_names_info(result, type = 1L),
        -1L,
        info = context
      )
    }
  }
})

test_that("grouped summarise always returns automatic row names", {
  fixtures <- list(
    one_group = list(
      data = data.frame(
        key = c("a", "a"),
        site = c("x", "x"),
        row.names = c("input-a", "input-b")
      ),
      expected = data.frame(
        key = "a",
        site = "x",
        rows = 2L,
        flag = TRUE
      )
    ),
    multiple_groups = list(
      data = data.frame(
        key = c("b", "a", "b"),
        site = c("y", "x", "y"),
        row.names = c("input-a", "input-b", "input-c")
      ),
      expected = data.frame(
        key = c("b", "a"),
        site = c("y", "x"),
        rows = c(2L, 1L),
        flag = c(TRUE, TRUE)
      )
    ),
    zero_groups = list(
      data = data.frame(key = character(), site = character()),
      expected = data.frame(
        key = character(),
        site = character(),
        rows = integer(),
        flag = logical()
      )
    )
  )

  callbacks <- list(
    unnamed_scalars = function(d) {
      list(rows = nrow(d), flag = TRUE)
    },
    repeated_scalar_names = function(d) {
      list(
        rows = setNames(nrow(d), "metric-row"),
        flag = setNames(TRUE, "metric-row")
      )
    },
    different_scalar_names = function(d) {
      label <- if (nrow(d)) d$key[[1L]] else "empty"
      list(
        rows = setNames(nrow(d), paste0("rows-", label)),
        flag = setNames(TRUE, paste0("flag-", label))
      )
    }
  )

  for (fixture_name in names(fixtures)) {
    fixture <- fixtures[[fixture_name]]

    for (by in list("key", c("key", "site"))) {
      expected <- fixture$expected[c(by, "rows", "flag")]

      for (callback_name in names(callbacks)) {
        result <- summarise(fixture$data, callbacks[[callback_name]], by = by)
        context <- paste(fixture_name, paste(by, collapse = ","), callback_name)

        expect_identical(result, expected, info = context)

        # Zero-row frames use 0; nonempty frames use a negative row count.
        expect_identical(
          .row_names_info(result, type = 1L),
          -nrow(expected),
          info = context
        )
      }
    }
  }
})

test_that("summarise accepts one-row data frames and preserves metric types", {
  data <- data.frame(key = c("b", "a", "b"), value = c(2L, 5L, 3L))
  metrics <- function(d) {
    data.frame(
      rows = nrow(d),
      `total value` = sum(d$value),
      day = as.Date("2024-01-01"),
      rating = ordered("high", levels = c("low", "high")),
      row.names = "callback-row",
      check.names = FALSE
    )
  }

  expect_identical(
    summarise(data, metrics),
    data.frame(
      rows = 3L,
      `total value` = 10L,
      day = as.Date("2024-01-01"),
      rating = ordered("high", levels = c("low", "high")),
      check.names = FALSE
    )
  )

  expected <- data.frame(
    key = c("b", "a"),
    rows = c(2L, 1L),
    `total value` = c(5L, 5L),
    day = rep(as.Date("2024-01-01"), 2L),
    rating = ordered(c("high", "high"), levels = c("low", "high")),
    check.names = FALSE
  )

  expect_identical(summarise(data, metrics, by = "key"), expected)
  expect_identical(
    summarise(data[data$key == "b", , drop = FALSE], metrics, by = "key"),
    expected[1L, , drop = FALSE]
  )
})

test_that("summarise accepts one-row data-frame returns for empty inputs", {
  data <- data.frame(key = character(), value = integer())
  metrics <- function(d) {
    data.frame(
      rows = nrow(d),
      total = sum(d$value),
      row.names = "callback-row"
    )
  }

  expect_identical(
    summarise(data, metrics),
    data.frame(rows = 0L, total = 0L)
  )
  expect_identical(
    summarise(data, metrics, by = "key"),
    data.frame(key = character(), rows = integer(), total = integer())
  )
})

test_that("summarise allows list and data-frame returns across groups", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    if (d$key == "a") {
      list(total = 1L)
    } else {
      data.frame(total = 2L)
    }
  }

  expect_identical(
    summarise(data, metrics, by = "key"),
    data.frame(key = c("a", "b"), total = c(1L, 2L))
  )
  expect_identical(
    summarise(data[2:1, , drop = FALSE], metrics, by = "key"),
    data.frame(key = c("b", "a"), total = c(2L, 1L))
  )
})

test_that("summarise validates metrics in data-frame returns in every mode", {
  data <- data.frame(key = c("a", "b"))

  duplicate_names <- data.frame(first = 1L, second = 2L)
  names(duplicate_names) <- c("total", "total")

  list_column <- data.frame(total = 1L)
  list_column$total <- list(1L)

  matrix_column <- data.frame(total = 1L)
  matrix_column$total <- matrix(1L, nrow = 1L)

  custom_class <- data.frame(total = 1L)
  custom_class$total <- structure(1L, class = "custom_metric")

  invalid <- list(
    zero_rows = list(
      value = data.frame(total = integer()),
      message = "unexpected number of values"
    ),
    multiple_rows = list(
      value = data.frame(total = 1:2),
      message = "unexpected number of values"
    ),
    zero_columns = list(
      value = data.frame(row.names = "callback-row"),
      message = "at least one metric"
    ),
    duplicate_names = list(
      value = duplicate_names,
      message = "unique name"
    ),
    list_column = list(
      value = list_column,
      message = "Expected an atomic vector"
    ),
    matrix_column = list(
      value = matrix_column,
      message = "without dimensions"
    ),
    custom_class = list(
      value = custom_class,
      message = "unsupported class"
    )
  )

  for (case_name in names(invalid)) {
    case <- invalid[[case_name]]
    metrics <- function(d) case$value

    expect_error(
      summarise(data, metrics),
      case$message,
      fixed = TRUE
    )
    expect_error(
      summarise(data, metrics, by = "key"),
      case$message,
      fixed = TRUE
    )
    expect_error(
      summarise(data[FALSE, , drop = FALSE], metrics, by = "key"),
      case$message,
      fixed = TRUE
    )
  }
})

test_that("data-frame returns must have compatible metrics across groups", {
  data <- data.frame(key = c("a", "b"))

  different_types <- function(d) {
    data.frame(total = if (d$key == "a") 1L else 2)
  }
  different_names <- function(d) {
    result <- data.frame(total = 1L)
    if (d$key == "b") {
      names(result) <- "average"
    }
    result
  }

  expect_error(
    summarise(data, different_types, by = "key"),
    "incompatible types",
    fixed = TRUE
  )
  expect_error(
    summarise(data, different_names, by = "key"),
    "different order",
    fixed = TRUE
  )

  expect_error(
    summarise(data, function(d) data.frame(key = 1L), by = "key"),
    "must not match",
    fixed = TRUE
  )
})

test_that("empty grouped summarise calls fun once with the complete input", {
  data <- data.frame(
    key = factor(character(), levels = c("a", "b")),
    value = integer()
  )
  observed <- list()
  metrics <- function(d) {
    observed[[length(observed) + 1L]] <<- d
    data.frame(rows = nrow(d), total = sum(d$value))
  }

  result <- summarise(data, metrics, by = "key")

  # With no observed groups, one callback supplies the metric column types.
  expect_identical(observed, list(data))
  expect_identical(
    result,
    data.frame(key = data$key, rows = integer(), total = integer())
  )
})

test_that("summarise rejects blank and missing data-frame metric names", {
  data <- data.frame(key = c("a", "b"))

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) {
      setNames(data.frame(total = 1L), "")
    })
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) {
      setNames(data.frame(total = 1L), NA_character_)
    })
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(
      data,
      function(d) {
        setNames(data.frame(total = 1L), "")
      },
      by = "key"
    )
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(
      data,
      function(d) {
        setNames(data.frame(total = 1L), NA_character_)
      },
      by = "key"
    )
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(
      data[FALSE, , drop = FALSE],
      function(d) {
        setNames(data.frame(total = 1L), "")
      },
      by = "key"
    )
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(
      data[FALSE, , drop = FALSE],
      function(d) {
        setNames(data.frame(total = 1L), NA_character_)
      },
      by = "key"
    )
  })
})

test_that("data-frame returns must have the same metric count across groups", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    if (d$key == "a") {
      data.frame(total = 1L)
    } else {
      data.frame(total = 2L, rows = 1L)
    }
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data[2:1, , drop = FALSE], metrics, by = "key")
  })
})

test_that("data-frame returns must preserve metric order across groups", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    result <- data.frame(total = 1L, rows = 1L)
    if (d$key == "b") {
      result <- result[c("rows", "total")]
    }
    result
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
})

test_that("empty grouped summarise rejects data-frame metrics matching groups", {
  data <- data.frame(key = character(), value = integer())

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, function(d) data.frame(key = 1L), by = "key")
  })
})

test_that("a valid metric cannot mask a list-valued metric", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    # Put the valid metric first to check that the error identifies bad.
    list(good = 1L, bad = list(2L))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data[FALSE, , drop = FALSE], metrics, by = "key")
  })
})

test_that("summarise rejects non-list non-atomic metrics", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    # A symbol has length one but is neither a list nor an atomic vector.
    list(good = 1L, bad = quote(x))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data[FALSE, , drop = FALSE], metrics, by = "key")
  })
})

test_that("a supported grouping column cannot mask a list-valued column", {
  data <- data.frame(key = c("a", "b"))
  data$bad <- list(1L, 2L)
  metrics <- function(d) list(rows = nrow(d))

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = c("key", "bad"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data[FALSE, , drop = FALSE], metrics, by = c("key", "bad"))
  })
})

test_that("a supported grouping column cannot mask a nested data frame", {
  data <- data.frame(key = c("a", "b"))
  data$bad <- data.frame(value = 1:2)
  metrics <- function(d) list(rows = nrow(d))

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = c("key", "bad"))
  })
})

test_that("summarise reports multiple duplicated metric names", {
  data <- data.frame(key = c("a", "b"))
  metrics <- function(d) {
    setNames(list(1L, 2L, 3L, 4L), c("first", "first", "second", "second"))
  }

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = "key")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data[FALSE, , drop = FALSE], metrics, by = "key")
  })
})

test_that("summarise rejects a blank grouping name beside a valid name", {
  # The blank column exists, so this must reach the blank-name validation.
  data <- setNames(data.frame(first = 1:2, second = 3:4), c("key", ""))
  metrics <- function(d) list(rows = nrow(d))

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = c("key", ""))
  })
})

test_that("summarise reports all matrix-valued grouping columns", {
  data <- data.frame(key = c("a", "b"))
  data$first <- matrix(1:2, ncol = 1L)
  data$second <- matrix(3:4, ncol = 1L)
  metrics <- function(d) list(rows = nrow(d))

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = c("key", "first", "second"))
  })
})

test_that("summarise reports all array-valued grouping columns", {
  data <- data.frame(key = c("a", "b"))
  data$first <- array(1:2, dim = c(2L, 1L, 1L))
  data$second <- array(3:4, dim = c(2L, 1L, 1L))
  metrics <- function(d) list(rows = nrow(d))

  expect_snapshot(error = TRUE, cran = TRUE, {
    summarise(data, metrics, by = c("key", "first", "second"))
  })
})

