# Tests for column_to_rownames ----------------------------------------------

make_column_to_rownames_data <- function() {
  data.frame(
    id = c("row-a", "row-b", "row-c"),
    value = c(1L, 2L, 3L),
    label = c("alpha", "beta", "gamma"),
    category = factor(
      c("low", "high", "low"),
      levels = c("low", "high", "unused")
    ),
    row.names = NULL
  )
}

test_that("column_to_rownames moves values into row names and removes column", {
  data <- make_column_to_rownames_data()
  original <- data

  result <- column_to_rownames(data, "id")

  expect_identical(row.names(result), c("row-a", "row-b", "row-c"))
  expect_identical(names(result), c("value", "label", "category"))
  expect_identical(result$value, data$value)
  expect_identical(result$label, data$label)
  expect_identical(result$category, data$category)
  expect_identical(data, original)
})

test_that("column_to_rownames uses values in their original row order", {
  data <- data.frame(id = c("z", "a", "m"), value = 1:3)

  result <- column_to_rownames(data, "id")

  expect_identical(row.names(result), c("z", "a", "m"))
})

test_that("column_to_rownames handles the column in any position", {
  first <- data.frame(id = c("a", "b"), x = 1:2, y = 3:4)
  middle <- data.frame(x = 1:2, id = c("a", "b"), y = 3:4)
  last <- data.frame(x = 1:2, y = 3:4, id = c("a", "b"))

  first_result <- column_to_rownames(first, "id")
  middle_result <- column_to_rownames(middle, "id")
  last_result <- column_to_rownames(last, "id")

  expect_identical(names(first_result), c("x", "y"))
  expect_identical(names(middle_result), c("x", "y"))
  expect_identical(names(last_result), c("x", "y"))
  expect_identical(first_result, middle_result)
  expect_identical(middle_result, last_result)
})

test_that("column_to_rownames retains data frame shape at small dimensions", {
  single_row <- data.frame(id = "only", value = 1L)
  single_row_result <- column_to_rownames(single_row, "id")
  expect_identical(dim(single_row_result), c(1L, 1L))
  expect_identical(row.names(single_row_result), "only")
  expect_identical(single_row_result$value, 1L)

  one_remaining <- data.frame(id = c("a", "b"), value = 1:2)
  one_remaining_result <- column_to_rownames(one_remaining, "id")
  expect_identical(dim(one_remaining_result), c(2L, 1L))

  only_column <- data.frame(id = c("a", "b", "c"))
  only_column_result <- column_to_rownames(only_column, "id")
  expect_s3_class(only_column_result, "data.frame")
  expect_identical(dim(only_column_result), c(3L, 0L))
  expect_identical(row.names(only_column_result), c("a", "b", "c"))
})

test_that("column_to_rownames handles zero-row data frames", {
  character_id <- data.frame(id = character(0), value = integer(0))
  character_result <- column_to_rownames(character_id, "id")
  expect_identical(dim(character_result), c(0L, 1L))
  expect_identical(names(character_result), "value")
  expect_identical(row.names(character_result), character(0))

  empty_numeric_id <- data.frame(id = numeric(0), value = integer(0))
  numeric_result <- column_to_rownames(empty_numeric_id, "id")
  expect_identical(dim(numeric_result), c(0L, 1L))
  expect_identical(row.names(numeric_result), character(0))
})

test_that("column_to_rownames converts values to character like base R", {
  cases <- list(
    numeric = list(values = c(1, 2.5), expected = c("1", "2.5")),
    logical = list(values = c(TRUE, FALSE), expected = c("TRUE", "FALSE")),
    factor = list(
      values = factor(c("low", "high")),
      expected = c("low", "high")
    ),
    Date = list(
      values = as.Date("2024-01-01") + 0:1,
      expected = c("2024-01-01", "2024-01-02")
    )
  )

  for (case in cases) {
    data <- data.frame(id = case$values)
    result <- column_to_rownames(data, "id")
    expect_identical(row.names(result), case$expected)
  }
})

test_that("column_to_rownames allows empty string row names", {
  data <- data.frame(id = c("", "row-b"), value = 1:2)

  result <- column_to_rownames(data, "id")

  expect_identical(row.names(result), c("", "row-b"))
})

test_that("column_to_rownames rejects duplicated row names", {
  data <- data.frame(id = c("a", "b", "a", "b", "c"), value = 1:5)
  original <- data

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, "id")
  })
  expect_identical(data, original)
})

test_that("column_to_rownames rejects missing row names and reports their count", {
  data <- data.frame(id = c("a", NA_character_, NA_character_), value = 1:3)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, "id")
  })
})

test_that("column_to_rownames requires an existing column", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, "missing")
  })
})

test_that("column_to_rownames requires a non-empty column name", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, character(0))
  })
})

test_that("column_to_rownames rejects an empty string column name", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, "")
  })
})

test_that("column_to_rownames rejects multiple column names", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, c("id", "value"))
  })
})

test_that("column_to_rownames rejects a missing column name", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, NA_character_)
  })
})

test_that("column_to_rownames rejects an integer column name", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, 1L)
  })
})

test_that("column_to_rownames rejects a logical column name", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, TRUE)
  })
})

test_that("column_to_rownames rejects a factor column name", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, factor("id"))
  })
})

test_that("column_to_rownames rejects a list column name", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, list("id"))
  })
})

test_that("column_to_rownames rejects a NULL column name", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, NULL)
  })
})

test_that("column_to_rownames rejects NULL data", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(NULL, "id")
  })
})

test_that("column_to_rownames rejects vector data", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(1:3, "id")
  })
})

test_that("column_to_rownames rejects matrix data", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(matrix(1:4, ncol = 2), "id")
  })
})

test_that("column_to_rownames rejects list data", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(list(id = c("a", "b")), "id")
  })
})

test_that("column_to_rownames requires data without existing row names", {
  data <- data.frame(id = c("new-a", "new-b"), value = 1:2)
  row.names(data) <- c("old-a", "old-b")

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, "id")
  })
})

test_that("column_to_rownames accepts automatic row names", {
  data <- data.frame(id = c("a", "b"), value = 1:2)

  expect_false(.row_names_info(data) > 0L)
  expect_identical(
    row.names(column_to_rownames(data, "id")),
    c("a", "b")
  )
})

test_that("column_to_rownames preserves attributes of remaining columns", {
  data <- data.frame(id = c("a", "b"), value = 1:2)
  data$day <- as.Date("2024-01-01") + 0:1
  data$category <- factor(c("low", "high"), levels = c("low", "high", "unused"))
  data$payload <- list(1L, letters[1:2])

  result <- column_to_rownames(data, "id")

  expect_identical(result$day, data$day)
  expect_identical(result$category, data$category)
  expect_identical(levels(result$category), c("low", "high", "unused"))
  expect_identical(result$payload, data$payload)
})

test_that("column_to_rownames errors on data frames with no columns", {
  data <- data.frame(row.names = c("a", "b"))

  expect_snapshot(error = TRUE, cran = TRUE, {
    column_to_rownames(data, "id")
  })
})
