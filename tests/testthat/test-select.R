# Tests for brename ----------------------------------------------------------

test_that("brename renames a single column correctly", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a")
  result <- brename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
  expect_equal(result$new_a, df$a)
})

test_that("brename renames multiple columns correctly", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  namemap <- c(new_a = "a", new_c = "c")
  result <- brename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b", "new_c"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$new_c, df$c)
})

test_that("brename leaves unspecified columns unchanged", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a")
  result <- brename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
  expect_equal(result$b, df$b)
})

test_that("brename maintains the original column order", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  namemap <- c(new_b = "b")
  result <- brename(df, namemap)
  expect_equal(colnames(result), c("a", "new_b", "c"))
})

test_that("brename handles namemap provided as a named list", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- list(new_a = "a")
  result <- brename(df, namemap)
  expect_equal(colnames(result), c("new_a", "b"))
})

test_that("brename throws an error if a specified column does not exist", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(new_a = "a", new_c = "c")
  expect_error(
    brename(df, namemap),
    regexp = "brename: could not find column/s named"
  )
})

test_that("brename throws an error when namemap is empty", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c()
  expect_error(
    brename(df, namemap),
    regexp = "brename: 'namemap' must be a named vector"
  )
})

test_that("brename throws an error when namemap is NULL", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- NULL
  expect_error(
    brename(df, namemap),
    regexp = "brename: 'namemap' must be a named vector"
  )
})

test_that("brename throws an error when namemap elements are unnamed", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a")
  expect_error(
    brename(df, namemap),
    regexp = "brename: 'namemap' must be a named vector",
  )
})

test_that("brename throws an error when namemap is not a named vector", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a", "b")
  expect_error(
    brename(df, namemap),
    regexp = "brename: 'namemap' must be a named vector"
  )
})

test_that("brename throws an error when namemap has empty names", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c("a", new_b = "b")
  expect_error(
    brename(df, namemap),
    regexp = "brename: all elements in 'namemap' must be named"
  )
})

test_that("brename handles duplicate new column names in namemap", {
  df <- data.frame(a = 1:3, b = 4:6)
  namemap <- c(dup_name = "a", dup_name = "b")
  result <- brename(df, namemap)
  expect_equal(colnames(result), c("dup_name", "dup_name"))
  expect_equal(result[[1]], df$a)
  expect_equal(result[[2]], df$b)
})

# Tests for bselect ----------------------------------------------------------

test_that("bselect returns correct columns", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  result <- bselect(df, c("a", "c"))
  expect_equal(colnames(result), c("a", "c"))
  expect_equal(ncol(result), 2)
  expect_equal(result$a, df$a)
  expect_equal(result$c, df$c)
})

test_that("bselect maintains the original data types", {
  df <- data.frame(
    num = 1:5,
    char = letters[1:5],
    factor = factor(letters[1:5]),
    stringsAsFactors = FALSE
  )
  result <- bselect(df, c("num", "factor"))
  expect_type(result$num, "integer")
  expect_s3_class(result$factor, "factor")
})

test_that("bselect with non-existent columns throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    bselect(df, c("a", "d")),
    regexp = "bselect: Could not find column/s: \\[d\\]"
  )
})

test_that("bselect with duplicate columns throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    bselect(df, c("a", "a")),
    regexp = "bselect: 'columns' argument must not contain duplicates"
  )
})

test_that("bselect with non-character columns argument throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    bselect(df, c(1, 2)),
    regexp = "bselect: 'columns' argument must be a character vector"
  )
})

test_that("bselect with columns not a vector throws an error", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    bselect(df, list("a", "b")),
    regexp = "bselect: 'columns' argument must be a character vector"
  )
})

test_that("bselect with .data not a data.frame throws an error", {
  df <- matrix(1:10, nrow = 5)
  expect_error(
    bselect(df, c("a", "b")),
    regexp = "bselect: '.data' must be a data.frame"
  )
})

test_that("bselect with empty columns vector returns empty data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  result <- bselect(df, character(0))
  expect_equal(ncol(result), 0)
  expect_true(is.data.frame(result))
})

test_that("bselect with all columns returns original data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  result <- bselect(df, c("a", "b"))
  expect_equal(result, df)
})

test_that("bselect does not modify the original data frame", {
  df <- data.frame(a = 1:5, b = 6:10)
  df_copy <- df
  bselect(df, c("a"))
  expect_equal(df, df_copy)
})


test_that("bselect selects and renames columns when 'columns' is a named vector", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  columns <- c(new_a = "a", new_c = "c")
  result <- bselect(df, columns)
  expect_equal(colnames(result), c("new_a", "new_c"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$new_c, df$c)
})

test_that("bselect selects and renames columns with a mix of named and unnamed elements", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  columns <- c(new_a = "a", "b", new_c = "c")
  result <- bselect(df, columns)
  # Expected column names: new_a, b, new_c
  expect_equal(colnames(result), c("new_a", "b", "new_c"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$b, df$b)
  expect_equal(result$new_c, df$c)
})

test_that("bselect only renames columns corresponding to named elements in 'columns'", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(new_a = "a", "b")
  result <- bselect(df, columns)
  expect_equal(colnames(result), c("new_a", "b"))
  expect_equal(result$new_a, df$a)
  expect_equal(result$b, df$b)
})

test_that("bselect handles 'columns' vector with duplicate new names", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(dup_name = "a", dup_name = "b")
  result <- bselect(df, columns)
  expect_equal(colnames(result), c("dup_name", "dup_name"))
  expect_equal(result[[1]], df$a)
  expect_equal(result[[2]], df$b)
})

test_that("bselect throws an error when 'columns' has duplicate old names", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(new_a = "a", new_b = "a")
  expect_error(
    bselect(df, columns),
    regexp =  "bselect: 'columns' argument must not contain duplicates"
  )
})

test_that("bselect throws an error correctly when 'columns' is an empty vector", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c()
  expect_error(bselect(df, columns), "'columns' argument must be a character vector, not a [NULL]", fixed=TRUE)
})

test_that("bselect handles 'columns' with names but empty values", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(new_a = "")
  expect_error(
    bselect(df, columns),
    regexp = "bselect: Could not find column/s: \\[\\]"
  )
})

test_that("bselect maintains data types after renaming", {
  df <- data.frame(
    num = 1:5,
    char = letters[1:5],
    factor = factor(letters[1:5]),
    stringsAsFactors = FALSE
  )
  columns <- c(new_num = "num", new_factor = "factor")
  result <- bselect(df, columns)
  expect_type(result$new_num, "integer")
  expect_s3_class(result$new_factor, "factor")
})

test_that("bselect does not modify the original data frame when renaming", {
  df <- data.frame(a = 1:5, b = 6:10)
  df_copy <- df
  bselect(df, c(new_a = "a"))
  expect_equal(df, df_copy)
})

test_that("bselect throws an error if specified columns do not exist (with names)", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c(new_a = "a", new_c = "c")
  expect_error(
    bselect(df, columns),
    regexp = "bselect: Could not find column/s: \\[c\\]"
  )
})

test_that("bselect works correctly with only unnamed elements in 'columns'", {
  df <- data.frame(a = 1:3, b = 4:6)
  columns <- c("a", "b")
  result <- bselect(df, columns)
  expect_equal(colnames(result), c("a", "b"))
  expect_equal(result$a, df$a)
  expect_equal(result$b, df$b)
})

test_that("bselect works correctly with a mix of named and unnamed elements in different orders", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  columns <- c("b", new_a = "a", "c")
  result <- bselect(df, columns)
  expect_equal(colnames(result), c("b", "new_a", "c"))
  expect_equal(result$b, df$b)
  expect_equal(result$new_a, df$a)
  expect_equal(result$c, df$c)
})

test_that("bselect returns columns in the order specified in 'columns'", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  columns <- c("c", new_b = "b", "a")
  result <- bselect(df, columns)
  expect_equal(colnames(result), c("c", "new_b", "a"))
  expect_equal(result$c, df$c)
  expect_equal(result$new_b, df$b)
  expect_equal(result$a, df$a)
})

test_that("bselect handles 'columns' with all elements named", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  columns <- c(new_x = "x", new_y = "y", new_z = "z")
  result <- bselect(df, columns)
  expect_equal(colnames(result), c("new_x", "new_y", "new_z"))
  expect_equal(result$new_x, df$x)
  expect_equal(result$new_y, df$y)
  expect_equal(result$new_z, df$z)
})

