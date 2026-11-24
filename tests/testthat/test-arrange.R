test_that("arrange sorts by column priority with independent directions", {
  data <- data.frame(
    group = c("b", "a", "a", "b", "a"),
    score = c(1, 2, 3, 2, 3),
    id = 1:5,
    row.names = letters[1:5]
  )
  original <- data

  expect_identical(arrange(data, "score"), data[c(1, 2, 4, 3, 5), ])
  expect_identical(
    arrange(data, c("group", desc("score"))),
    data[c(3, 5, 2, 4, 1), ]
  )
  expect_identical(
    arrange(data, c(desc("group"), "score")),
    data[c(1, 4, 2, 3, 5), ]
  )
  expect_identical(
    arrange(data, c(desc("group"), desc("score"))),
    data[c(4, 1, 3, 5, 2), ]
  )
  expect_identical(data, original)
})

test_that("arrange sorts character, factor, and temporal values descending", {
  inputs <- list(
    c("b", "a", "c", "b"),
    factor(c("a", "z", "m", "a"), levels = c("z", "a", "m")),
    ordered(c("a", "z", "m", "a"), levels = c("z", "a", "m")),
    as.Date("2020-01-01") + c(1, 0, 2, 1),
    as.POSIXct("2020-01-01", tz = "UTC") + c(1, 0, 2, 1)
  )

  for (value in inputs) {
    data <- data.frame(value = value, id = seq_along(value))
    expect_identical(arrange(data, desc("value")), data[c(3, 1, 4, 2), ])
  }
})

test_that("arrange puts missing values last in both directions", {
  data <- data.frame(value = c(NA, 2, 1, NA, 2), id = 1:5)

  expect_identical(arrange(data, "value"), data[c(3, 2, 5, 1, 4), ])
  expect_identical(arrange(data, desc("value")), data[c(2, 5, 3, 1, 4), ])

  data <- data.frame(
    group = c("a", NA, "a", NA, "b"),
    value = c(NA, 3, 1, 1, 2)
  )
  expect_identical(
    arrange(data, c("group", desc("value"))),
    data[c(3, 1, 5, 2, 4), ]
  )
})

test_that("arrange preserves dimensions and handles empty inputs", {
  data <- data.frame(value = c(2L, 1L))

  expect_identical(
    arrange(data, desc("value")),
    data[1:2, , drop = FALSE]
  )
  expect_identical(arrange(data, character()), data)
  expect_identical(
    arrange(data[FALSE, , drop = FALSE], desc("value")),
    data[FALSE, , drop = FALSE]
  )
  expect_identical(arrange(data.frame(), character()), data.frame())
})

test_that("arrange does not pass column names as order control arguments", {
  data <- data.frame(decreasing = c(2, 1, 3), method = c("b", "a", "c"))

  expect_identical(arrange(data, "decreasing"), data[c(2, 1, 3), ])
  expect_identical(arrange(data, desc("method")), data[c(3, 1, 2), ])
})

test_that("arrange shares data and column validation conventions", {
  data <- data.frame(value = 1:3)

  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(NULL, "value")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, NULL)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, matrix("value"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, NA_character_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, c("value", desc("value")))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, desc("missing"))
  })
})

test_that("arrange rejects unexpected names and modified descending markers", {
  data <- data.frame(value = 1:3)

  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, c(label = "value"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, c(label = desc("value")))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, setNames("value", NA_character_))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, c("..DeSc" = "value"))
  })
})

test_that("arrange rejects list, matrix, array, and nested data-frame keys", {
  data <- data.frame(id = 1:3)
  data$list <- list(3, 1, 2)
  data$matrix <- matrix(1:6, nrow = 3)
  data$array <- array(1:6, dim = c(3, 1, 2))
  data$nested <- data.frame(value = c(3, 1, 2))

  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "list")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, desc("matrix"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "array")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, desc("nested"))
  })
})
