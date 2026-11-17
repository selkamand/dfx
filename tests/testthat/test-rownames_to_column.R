# Tests for rownames_to_column ----------------------------------------------

make_rownames_to_column_data <- function() {
  data <- data.frame(
    value = c(1L, 2L, 3L),
    label = c("alpha", "beta", "gamma"),
    category = factor(
      c("low", "high", "low"),
      levels = c("low", "high", "unused")
    )
  )
  row.names(data) <- c("row-a", "row-b", "row-c")
  data
}

test_that("rownames_to_column adds row names as the first column", {
  data <- make_rownames_to_column_data()
  original <- data

  result <- rownames_to_column(data, "id")

  expect_identical(names(result), c("id", "value", "label", "category"))
  expect_identical(result$id, c("row-a", "row-b", "row-c"))
  expect_identical(result$value, data$value)
  expect_identical(result$label, data$label)
  expect_identical(result$category, data$category)
  expect_identical(data, original)
})

test_that("rownames_to_column uses automatic row names as values", {
  data <- data.frame(value = 1:3)

  result <- rownames_to_column(data, "id")

  expect_identical(result$id, c("1", "2", "3"))
})

test_that("rownames_to_column returns a data frame with automatic row names", {
  data <- make_rownames_to_column_data()

  result <- rownames_to_column(data, "id")

  expect_true(.row_names_info(result) < 0L)
  expect_identical(row.names(result), c("1", "2", "3"))
})

test_that("rownames_to_column preserves attributes of existing columns", {
  data <- make_rownames_to_column_data()
  data$day <- as.Date("2024-01-01") + 0:2
  data$payload <- list(1L, letters[1:2], NULL)

  result <- rownames_to_column(data, "id")

  expect_identical(result$day, data$day)
  expect_identical(result$category, data$category)
  expect_identical(levels(result$category), c("low", "high", "unused"))
  expect_identical(result$payload, data$payload)
})

test_that("rownames_to_column retains data frame shape at small dimensions", {
  single_row <- data.frame(value = 1L, row.names = "only")
  single_row_result <- rownames_to_column(single_row, "id")
  expect_identical(dim(single_row_result), c(1L, 2L))
  expect_identical(names(single_row_result), c("id", "value"))
  expect_identical(single_row_result$id, "only")

  single_column <- data.frame(value = 1:3)
  single_column_result <- rownames_to_column(single_column, "id")
  expect_identical(dim(single_column_result), c(3L, 2L))
  expect_identical(single_column_result$id, c("1", "2", "3"))

  zero_rows <- data.frame(value = integer(0))
  zero_rows_result <- rownames_to_column(zero_rows, "id")
  expect_identical(dim(zero_rows_result), c(0L, 2L))
  expect_identical(names(zero_rows_result), c("id", "value"))
  expect_identical(zero_rows_result$id, character(0))

  zero_columns <- data.frame(row.names = c("row-a", "row-b"))
  zero_columns_result <- rownames_to_column(zero_columns, "id")
  expect_identical(dim(zero_columns_result), c(2L, 1L))
  expect_identical(names(zero_columns_result), "id")
  expect_identical(zero_columns_result$id, c("row-a", "row-b"))

  empty <- data.frame()
  empty_result <- rownames_to_column(empty, "id")
  expect_identical(dim(empty_result), c(0L, 1L))
  expect_identical(names(empty_result), "id")
  expect_identical(empty_result$id, character(0))
})

test_that("rownames_to_column round-trips with column_to_rownames", {
  data <- data.frame(id = c("row-b", "row-a"), value = 1:2, label = c("x", "y"))

  result <- rownames_to_column(column_to_rownames(data, "id"), "id")

  expect_identical(names(result), c("id", "value", "label"))
  expect_identical(result$id, data$id)
  expect_identical(result$value, data$value)
  expect_identical(result$label, data$label)
  expect_identical(row.names(result), c("1", "2"))
})

test_that("rownames_to_column rejects NULL data", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(NULL, "id")
  })
})

test_that("rownames_to_column rejects vector data", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(1:3, "id")
  })
})

test_that("rownames_to_column rejects matrix data", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(matrix(1:4, ncol = 2), "id")
  })
})

test_that("rownames_to_column rejects list data", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(list(value = 1:2), "id")
  })
})

test_that("rownames_to_column rejects an integer column name", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, 1L)
  })
})

test_that("rownames_to_column rejects a logical column name", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, TRUE)
  })
})

test_that("rownames_to_column rejects a factor column name", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, factor("id"))
  })
})

test_that("rownames_to_column rejects a list column name", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, list("id"))
  })
})

test_that("rownames_to_column rejects a NULL column name", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, NULL)
  })
})

test_that("rownames_to_column requires a non-empty column name", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, character(0))
  })
})

test_that("rownames_to_column rejects multiple column names", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, c("id", "value"))
  })
})

test_that("rownames_to_column rejects a missing column name", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, NA_character_)
  })
})

test_that("rownames_to_column rejects an empty string column name", {
  data <- data.frame(value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, "")
  })
})

test_that("rownames_to_column rejects an existing column name", {
  data <- data.frame(rowname = 1:2, value = 3:4)
  original <- data

  expect_snapshot(error = TRUE, cran = TRUE, {
    rownames_to_column(data, "rowname")
  })
  expect_identical(data, original)
})
