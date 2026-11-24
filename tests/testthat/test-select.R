# Tests for rename ----------------------------------------------------------

test_that("rename renames a single column correctly", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
  expect_equal(result$new_a, df$a)
})

test_that("rename renames multiple columns correctly", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  namemap <- c(new_a = "a", new_c = "c")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b", "new_c"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$new_c, df$c)
})

test_that("rename leaves unspecified columns unchanged", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
  expect_equal(result$b, df$b)
})

test_that("rename maintains the original column order", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  namemap <- c(new_b = "b")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("a", "new_b", "c"))
})

test_that("rename handles namemap provided as a named list", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- list(new_a = "a")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
})

test_that("rename throws an error if a specified column does not exist", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a", new_c = "c")
  expect_error(
    rename(df, namemap),
    regexp = "rename: could not find column/s named"
  )
})

test_that("rename throws an error when namemap is empty", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c()
  expect_error(
    rename(df, namemap),
    regexp = "rename: 'namemap' must be a named vector"
  )
})

test_that("rename throws an error when namemap is NULL", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- NULL
  expect_error(
    rename(df, namemap),
    regexp = "rename: 'namemap' must be a named vector"
  )
})

test_that("rename throws an error when namemap elements are unnamed", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a")
  expect_error(
    rename(df, namemap),
    regexp = "rename: 'namemap' must be a named vector",
  )
})

test_that("rename throws an error when namemap is not a named vector", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a", "b")
  expect_error(
    rename(df, namemap),
    regexp = "rename: 'namemap' must be a named vector"
  )
})

test_that("rename throws an error when namemap has empty names", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a", new_b = "b")
  expect_error(
    rename(df, namemap),
    regexp = "rename: all elements in 'namemap' must be named"
  )
})

test_that("rename handles duplicate new column names in namemap", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(dup_name = "a", dup_name = "b")
  result <- rename(df, namemap)
  expect_equal(colnames(result), c("dup_name", "dup_name"))
  expect_equal(result[[1]], df$a)
  expect_equal(result[[2]], df$b)
})

# Tests for select ----------------------------------------------------------

test_that("select returns correct columns", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  result <- select(df, c("a", "c"))
  expect_equal(colnames(result), c("a", "c"))
  expect_equal(ncol(result), 2)
  expect_equal(result$a, df$a)
  expect_equal(result$c, df$c)
})

test_that("select maintains the original data types", {
  df <- data.frame(
    num = 1:5,
    char = letters[1:5],
    factor = factor(letters[1:5]),
    stringsAsFactors = FALSE
  )
  result <- select(df, c("num", "factor"))
  expect_type(result$num, "integer")
  expect_s3_class(result$factor, "factor")
})

test_that("select with non-existent columns throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, c("a", "d"))
  })
})

test_that("select reports multiple non-existent columns", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, c("d", "e"))
  })
})

test_that("select quotes non-existent column names unambiguously", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, c("NA", "a, b", "a\nb"))
  })
})

test_that("select with duplicate columns throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, c("a", "a"))
  })
})

test_that("select with non-character columns argument throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, c(1, 2))
  })
})

test_that("select with list columns argument throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, list("a", "b"))
  })
})

test_that("select with matrix columns argument throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, matrix(c("a", "b"), nrow = 1L))
  })
})

test_that("select explains why attributed character columns are invalid", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, structure("a", note = "extra attribute"))
  })
})

test_that("select with factor columns argument throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, factor(c("a", "b")))
  })
})

test_that("select rejects missing column names", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, c("a", NA_character_))
  })
})

test_that("select rejects missing column names even when present in data", {
  df <- data.frame(a = 1:5, b = 6:10)
  names(df)[1L] <- NA_character_
  call <- quote(select(df, NA_character_))
  err <- tryCatch(select(df, NA_character_), error = identity)

  expect_identical(conditionCall(err), call)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, NA_character_)
  })
})

test_that("select rejects empty column names even when present in data", {
  df <- data.frame(a = 1:5, b = 6:10)
  names(df)[1L] <- ""
  call <- quote(select(df, ""))
  err <- tryCatch(select(df, ""), error = identity)

  expect_identical(conditionCall(err), call)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, "")
  })
})

test_that("select with data not a data.frame throws an error", {
  df <- matrix(1:10, nrow = 5)
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, c("a", "b"))
  })
})

test_that("select with empty columns vector returns empty data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  result <- select(df, character(0))
  expect_equal(ncol(result), 0)
  expect_true(is.data.frame(result))
})

test_that("select with all columns returns original data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  result <- select(df, c("a", "b"))
  expect_equal(result, df)
})

test_that("select does not modify the original data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  df_copy <- df
  select(df, c("a"))
  expect_equal(df, df_copy)
})


test_that("select selects and renames columns when 'columns' is a named vector", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  columns <- c(new_a = "a", new_c = "c")
  result <- select(df, columns)
  expect_equal(colnames(result), c("new_a", "new_c"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$new_c, df$c)
})

test_that("select selects and renames columns with a mix of named and unnamed elements", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  columns <- c(new_a = "a", "b", new_c = "c")
  result <- select(df, columns)
  # Expected column names: new_a, b, new_c
  expect_equal(colnames(result), c("new_a", "b", "new_c"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$b, df$b)
  expect_equal(result$new_c, df$c)
})

test_that("select only renames columns corresponding to named elements in 'columns'", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(new_a = "a", "b")
  result <- select(df, columns)
  expect_equal(colnames(result), c("new_a", "b"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$b, df$b)
})

test_that("select handles 'columns' vector with duplicate new names", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(dup_name = "a", dup_name = "b")
  result <- select(df, columns)
  expect_equal(colnames(result), c("dup_name", "dup_name"))
  expect_equal(result[[1]], df$a)
  expect_equal(result[[2]], df$b)
})

test_that("select throws an error when 'columns' has duplicate old names", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(new_a = "a", new_b = "a")
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, columns)
  })
})

test_that("select with NULL columns throws an error", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c()
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, columns)
  })
})

test_that("select handles 'columns' with names but empty values", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(new_a = "")
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, columns)
  })
})

test_that("select maintains data types after renaming", {
  df <- data.frame(
    num = 1:5,
    char = letters[1:5],
    factor = factor(letters[1:5]),
    stringsAsFactors = FALSE
  )
  columns <- c(new_num = "num", new_factor = "factor")
  result <- select(df, columns)
  expect_type(result$new_num, "integer")
  expect_s3_class(result$new_factor, "factor")
})

test_that("select does not modify the original data frame when renaming", {
  df <- data.frame(a = 1:5, b = 6:10)
  df_copy <- df
  select(df, c(new_a = "a"))
  expect_equal(df, df_copy)
})

test_that("select throws an error if specified columns do not exist (with names)", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(new_a = "a", new_c = "c")
  expect_snapshot(error = TRUE, cran = TRUE, {
    select(df, columns)
  })
})

test_that("select works correctly with only unnamed elements in 'columns'", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c("a", "b")
  result <- select(df, columns)
  expect_equal(colnames(result), c("a", "b"))
  expect_equal(result$a, df$a)
  expect_equal(result$b, df$b)
})

test_that("select works correctly with a mix of named and unnamed elements in different orders", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  columns <- c("b", new_a = "a", "c")
  result <- select(df, columns)
  expect_equal(colnames(result), c("b", "new_a", "c"))
  expect_equal(result$b, df$b)
  expect_equal(result$new_a, df$a)
  expect_equal(result$c, df$c)
})

test_that("select returns columns in the order specified in 'columns'", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  columns <- c("c", new_b = "b", "a")
  result <- select(df, columns)
  expect_equal(colnames(result), c("c", "new_b", "a"))
  expect_equal(result$c, df$c)
  expect_equal(result$new_b, df$b)
  expect_equal(result$a, df$a)
})

test_that("select handles 'columns' with all elements named", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  columns <- c(new_x = "x", new_y = "y", new_z = "z")
  result <- select(df, columns)
  expect_equal(colnames(result), c("new_x", "new_y", "new_z"))
  expect_equal(result$new_x, df$x)
  expect_equal(result$new_y, df$y)
  expect_equal(result$new_z, df$z)
})
