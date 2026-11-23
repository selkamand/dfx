make_filter_data <- function() {
  data.frame(
    id = 1:4,
    value = c(10, NA_real_, 30, 40),
    row.names = c("row-a", "row-b", "row-c", "row-d")
  )
}

test_that("filter calls its predicate once with the whole data frame", {
  data <- make_filter_data()
  original <- data
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L

  result <- filter(data, function(d) {
    observed$calls <- observed$calls + 1L
    observed$data <- d
    d$id %in% c(1L, 3L)
  })

  expect_identical(observed$calls, 1L)
  expect_identical(observed$data, data)
  expect_identical(result$id, c(1L, 3L))
  expect_identical(row.names(result), c("row-a", "row-c"))
  expect_identical(data, original)
})

test_that("filter accepts all, no, and named predicate selections", {
  data <- make_filter_data()

  expect_identical(filter(data, function(d) rep(TRUE, nrow(d))), data)

  none <- filter(data, function(d) rep(FALSE, nrow(d)))
  expect_s3_class(none, "data.frame")
  expect_identical(dim(none), c(0L, 2L))
  expect_identical(names(none), names(data))

  named_result <- filter(data, function(d) {
    setNames(d$id %% 2L == 0L, row.names(d))
  })
  expect_identical(named_result$id, c(2L, 4L))
})

test_that("filter retains data frame shape at small dimensions", {
  data <- make_filter_data()

  one_column <- data["id"]
  one_column_result <- filter(one_column, function(d) d$id > 2L)
  expect_s3_class(one_column_result, "data.frame")
  expect_identical(dim(one_column_result), c(2L, 1L))
  expect_identical(one_column_result$id, c(3L, 4L))

  zero_rows <- data[FALSE, , drop = FALSE]
  expect_identical(
    filter(zero_rows, function(d) logical(nrow(d))),
    zero_rows
  )

  zero_columns <- data[, FALSE, drop = FALSE]
  zero_column_result <- filter(
    zero_columns,
    function(d) c(TRUE, FALSE, TRUE, FALSE)
  )
  expect_s3_class(zero_column_result, "data.frame")
  expect_identical(dim(zero_column_result), c(2L, 0L))
  expect_identical(row.names(zero_column_result), c("row-a", "row-c"))

  one_row <- data[1L, , drop = FALSE]
  expect_identical(filter(one_row, function(d) TRUE), one_row)
  expect_identical(
    dim(filter(one_row, function(d) FALSE)),
    c(0L, 2L)
  )
})

test_that("filter preserves column types and missing data values", {
  data <- make_filter_data()
  data$category <- factor(
    c("low", "high", "low", "high"),
    levels = c("low", "high", "unused")
  )
  data$day <- as.Date("2024-01-01") + 0:3
  data$payload <- list(1L, letters[1:2], NULL, list(key = "value"))

  result <- filter(data, function(d) d$id %in% c(2L, 3L))

  expect_identical(result$id, c(2L, 3L))
  expect_identical(result$value, c(NA_real_, 30))
  expect_identical(result$category, data$category[2:3])
  expect_identical(levels(result$category), c("low", "high", "unused"))
  expect_identical(result$day, data$day[2:3])
  expect_identical(result$payload, data$payload[2:3])
})

test_that("filter rejects NULL data before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(NULL, function(d) stop("predicate was called"))
  })
})

test_that("filter rejects vector data before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(1:4, function(d) stop("predicate was called"))
  })
})

test_that("filter rejects matrix data before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(matrix(1:4, ncol = 1), function(d) stop("predicate was called"))
  })
})

test_that("filter rejects list data before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(list(id = 1:4), function(d) stop("predicate was called"))
  })
})

test_that("filter rejects NULL as a predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), NULL)
  })
})

test_that("filter rejects an integer as a predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), 1L)
  })
})

test_that("filter rejects a string as a predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), "predicate")
  })
})

test_that("filter rejects a list as a predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), list(TRUE))
  })
})

test_that("filter rejects a predicate with no parameters", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function() TRUE)
  })
})

test_that("filter rejects a predicate with two required parameters", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d, other) TRUE)
  })
})

test_that("filter rejects a predicate with an optional second parameter", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d, other = NULL) TRUE)
  })
})

test_that("filter rejects a predicate with dots", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d, ...) TRUE)
  })
})

test_that("filter rejects a NULL predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) NULL)
  })
})

test_that("filter rejects an integer predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) 1L)
  })
})

test_that("filter rejects a character predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) c("TRUE", "FALSE", "TRUE", "FALSE"))
  })
})

test_that("filter rejects a list predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) list(TRUE, FALSE, TRUE, FALSE))
  })
})

test_that("filter rejects a matrix predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) {
      matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 1L)
    })
  })
})

test_that("filter rejects a zero-length predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) logical())
  })
})

test_that("filter rejects a length-one predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) TRUE)
  })
})

test_that("filter rejects a short predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) rep(TRUE, 3))
  })
})

test_that("filter rejects a long predicate result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) rep(TRUE, 5))
  })
})

test_that("filter reports one missing predicate value", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) c(TRUE, NA, FALSE, FALSE))
  })
})

test_that("filter reports two missing predicate values", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) c(NA, TRUE, FALSE, NA))
  })
})

test_that("filter reports errors raised by the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_filter_data(), function(d) stop("predicate failed"))
  })
})

test_that("filter and keep_rows select the expected IDs for every short mask", {
  data <- data.frame(id = 1:3)
  masks <- expand.grid(rep(list(c(FALSE, TRUE)), nrow(data)))

  for (i in seq_len(nrow(masks))) {
    keep <- unname(as.logical(masks[i, ]))
    expected_ids <- data$id[which(keep)]

    expect_identical(keep_rows(data, keep)$id, expected_ids)
    expect_identical(filter(data, function(d) keep)$id, expected_ids)
  }
})

make_grouped_filter_data <- function() {
  data.frame(
    id = 1:6,
    group = c("b", "a", "b", "a", "c", "c"),
    value = c(10, 20, 30, 40, 50, 60),
    row.names = paste0("row-", 1:6)
  )
}

test_that("filter calls its predicate once with each complete group", {
  data <- make_grouped_filter_data()
  observed <- new.env(parent = emptyenv())
  observed$calls <- character()
  observed$groups <- list()

  result <- filter(
    data,
    function(d) {
      key <- d$group[[1L]]
      observed$calls <- c(observed$calls, key)
      observed$groups[[key]] <- d
      TRUE
    },
    by = "group"
  )

  expect_identical(result, data)
  expect_length(observed$calls, 3L)
  expect_setequal(observed$calls, c("a", "b", "c"))
  expect_identical(observed$groups[["a"]], data[c(2L, 4L), , drop = FALSE])
  expect_identical(observed$groups[["b"]], data[c(1L, 3L), , drop = FALSE])
  expect_identical(observed$groups[["c"]], data[c(5L, 6L), , drop = FALSE])
})

test_that("filter keeps selected interleaved groups in original row order", {
  data <- make_grouped_filter_data()

  result <- filter(data, function(d) d$group[[1L]] == "a", by = "group")

  expect_identical(result, data[c(2L, 4L), , drop = FALSE])
})

test_that("filter groups by combinations of columns", {
  data <- make_grouped_filter_data()
  data$group <- c("b", "a", "b", "a", "b", "a")
  data$segment <- c("x", "x", "y", "x", "x", "y")

  result <- filter(
    data,
    function(d) {
      d$group[[1L]] == "b" && d$segment[[1L]] == "x"
    },
    by = c("group", "segment")
  )

  expect_identical(result, data[c(1L, 5L), , drop = FALSE])
})

test_that("filter rejects matrix-valued grouping columns before calling the predicate", {
  data <- data.frame(id = 1:4)
  data$key <- matrix(c(1L, 1L, 2L, 2L, 10L, 20L, 10L, 20L), ncol = 2L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(data, function(d) stop("predicate was called"), by = "key")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(data, function(d) stop("predicate was called"), by = c("id", "key"))
  })

  expect_identical(filter(data, function(d) TRUE, by = "id"), data)
})

test_that("filter ignores duplicate names in by", {
  data <- data.frame(id = 1:4, group = c("a", "a", "b", "b"))
  predicate <- function(d) d$group[[1L]] == "a"

  expect_identical(
    filter(data, predicate, by = c("group", "group")),
    filter(data, predicate, by = "group")
  )
  expect_identical(
    filter(data, predicate, by = "group"),
    data[1:2, , drop = FALSE]
  )
})

test_that("filter rejects list and nested data frame grouping columns before calling fun", {
  data <- data.frame(group = c("a", "b", "a"))
  data$list_key <- list(1L, "1", 1)
  data$nested_key <- data.frame(
    region = c("north", "north", "south"),
    site = 1:3
  )
  predicate <- function(d) stop("predicate was called")

  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(data, predicate, by = c("group", "list_key"))
    filter(data, predicate, by = "nested_key")
  })

  expect_identical(
    filter(data, function(d) nrow(d) > 1L, by = "group"),
    data[c(1L, 3L), , drop = FALSE]
  )
})

test_that("filter groups by POSIXlt datetime columns", {
  data <- data.frame(value = c(2L, 3L, 5L))
  data$key <- as.POSIXlt(
    as.POSIXct("2024-01-01", tz = "UTC") + c(3600, 0, 3600)
  )

  expect_identical(
    filter(data, function(d) nrow(d) > 1L, by = "key"),
    data[c(1L, 3L), , drop = FALSE]
  )
})

test_that("filter treats missing grouping keys as one group", {
  data <- data.frame(
    id = 1:5,
    group = c("a", NA_character_, "a", NA_character_, "b")
  )
  observed <- new.env(parent = emptyenv())
  observed$missing_group <- NULL

  result <- filter(
    data,
    function(d) {
      if (all(is.na(d$group))) {
        observed$missing_group <- d
        TRUE
      } else {
        FALSE
      }
    },
    by = "group"
  )

  expect_identical(observed$missing_group, data[c(2L, 4L), , drop = FALSE])
  expect_identical(result, data[c(2L, 4L), , drop = FALSE])
})

test_that("filter does not evaluate unused factor groups", {
  data <- data.frame(
    id = 1:4,
    group = factor(c("b", "a", "b", "a"), levels = c("a", "b", "unused"))
  )
  observed <- new.env(parent = emptyenv())
  observed$calls <- character()

  result <- filter(
    data,
    function(d) {
      observed$calls <- c(observed$calls, as.character(d$group[[1L]]))
      TRUE
    },
    by = "group"
  )

  expect_identical(result, data)
  expect_length(observed$calls, 2L)
  expect_setequal(observed$calls, c("a", "b"))
  expect_identical(levels(result$group), levels(data$group))
})

test_that("filter returns zero-row grouped data without calling the predicate", {
  data <- make_grouped_filter_data()[FALSE, , drop = FALSE]
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L

  result <- filter(
    data,
    function(d) {
      observed$calls <- observed$calls + 1L
      TRUE
    },
    by = "group"
  )

  expect_identical(result, data)
  expect_identical(observed$calls, 0L)
})

test_that("filter rejects empty grouping columns before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) stop("predicate was called"),
      by = character()
    )
  })
})

test_that("filter rejects integer grouping columns before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) stop("predicate was called"),
      by = 1L
    )
  })
})

test_that("filter rejects logical grouping columns before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) stop("predicate was called"),
      by = TRUE
    )
  })
})

test_that("filter rejects factor grouping columns before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) stop("predicate was called"),
      by = factor("group")
    )
  })
})

test_that("filter rejects list grouping columns before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) stop("predicate was called"),
      by = list("group")
    )
  })
})

test_that("filter rejects unknown grouping columns before calling the predicate", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) stop("predicate was called"),
      by = "missing"
    )
  })
})

test_that("grouped filter reports an error from the first group", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) stop("first group failed"),
      by = "group"
    )
  })
})

test_that("grouped filter reports an error from a later group", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) {
        if (d$group[[1L]] == "a") {
          stop("later group failed")
        }
        TRUE
      },
      by = "group"
    )
  })
})

test_that("grouped filter reports an error with multiple grouping columns", {
  data <- data.frame(
    group = c("a", "a", "b"),
    segment = c("x", "y", "x")
  )

  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      data,
      function(d) {
        if (d$group[[1L]] == "a" && d$segment[[1L]] == "y") {
          stop("group combination failed")
        }
        TRUE
      },
      by = c("group", "segment")
    )
  })
})

test_that("grouped filter rejects an integer result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_grouped_filter_data(), function(d) 1L, by = "group")
  })
})

test_that("grouped filter rejects a zero-length result", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(make_grouped_filter_data(), function(d) logical(), by = "group")
  })
})

test_that("grouped filter identifies the group with two results", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) if (d$group[[1L]] == "a") c(TRUE, FALSE) else TRUE,
      by = "group"
    )
  })
})

test_that("grouped filter rejects missing predicate values", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      make_grouped_filter_data(),
      function(d) if (d$group[[1L]] == "a") NA else TRUE,
      by = "group"
    )
  })
})

test_that("grouped filter errors identify multiple grouping values", {
  data <- data.frame(
    group = c("a", "a", "a", "b"),
    segment = c("x", "y", "y", "x")
  )

  expect_snapshot(error = TRUE, cran = TRUE, {
    filter(
      data,
      function(d) if (d$segment[[1L]] == "y") c(TRUE, FALSE) else TRUE,
      by = c("group", "segment")
    )
  })
})
