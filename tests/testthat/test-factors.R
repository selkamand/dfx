# fct_relevel ----------

test_that("fct_relevel throws error when no ref is supplied", {
  x <- factor(c("a", "b", "c"), levels = c("b", "c", "a"))

  expect_error(fct_relevel(x))
})


test_that("fct_relevel moves a single level to the front by default", {
  x <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))

  result <- fct_relevel(x, "a")

  expect_identical(levels(result), c("a", "b", "c", "d"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel moves multiple levels to the front in ref order", {
  x <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))

  result <- fct_relevel(x, c("d", "b"))

  expect_identical(levels(result), c("d", "b", "c", "a"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel keeps x levels not present in ref at the end", {
  x <- factor(
    c("low", "medium", "high", "unknown"),
    levels = c("unknown", "medium", "low", "high")
  )

  result <- fct_relevel(x, c("low", "medium"))

  expect_identical(levels(result), c("low", "medium", "unknown", "high"))
})


test_that("fct_relevel ignores ref levels that are not present in x", {
  x <- factor(c("a", "b", "c"), levels = c("c", "b", "a"))

  result <- fct_relevel(x, c("z", "a", "y", "c"))

  expect_identical(levels(result), c("a", "c", "b"))
  expect_false("z" %in% levels(result))
  expect_false("y" %in% levels(result))
})


test_that("fct_relevel only uses duplicated ref levels once", {
  x <- factor(c("a", "b", "c"), levels = c("c", "b", "a"))

  result <- fct_relevel(x, c("a", "a", "c", "a"))

  expect_identical(levels(result), c("a", "c", "b"))
})


test_that("fct_relevel can insert ref levels after a position", {
  x <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))

  result <- fct_relevel(x, c("d", "b"), after = 1L)

  expect_identical(levels(result), c("a", "d", "b", "c"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel can move ref levels to the end with after = Inf", {
  x <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))

  result <- fct_relevel(x, c("d", "b"), after = Inf)

  expect_identical(levels(result), c("a", "c", "d", "b"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel clamps after values beyond the number of remaining levels", {
  x <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))

  result <- fct_relevel(x, c("d", "b"), after = 100L)

  expect_identical(levels(result), c("a", "c", "d", "b"))
})


test_that("fct_relevel preserves ordered factors", {
  x <- ordered(
    c("low", "medium", "high"),
    levels = c("medium", "low", "high")
  )

  result <- fct_relevel(x, c("low", "medium", "high"))

  expect_s3_class(result, "ordered")
  expect_s3_class(result, "factor")
  expect_true(is.ordered(result))
  expect_identical(levels(result), c("low", "medium", "high"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel preserves missing values", {
  x <- factor(
    c("a", NA, "b", "c"),
    levels = c("c", "b", "a")
  )

  result <- fct_relevel(x, c("a", "b"))

  expect_identical(levels(result), c("a", "b", "c"))
  expect_identical(is.na(result), is.na(x))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel preserves an explicit NA level", {
  x <- factor(c("a", NA, "b"), levels = c("a", "b", NA), exclude = NULL)
  expected <- factor(c("a", NA, "b"), levels = c("b", "a", NA), exclude = NULL)

  expect_identical(fct_relevel(x, "b"), expected)
})


test_that("fct_relevel can move an explicit NA level", {
  x <- factor(c("a", NA, "b"), levels = c("a", "b", NA), exclude = NULL)

  result <- fct_relevel(x, NA_character_)

  expect_identical(levels(result), c(NA_character_, "a", "b"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel preserves extra factor attributes", {
  x <- factor(c("a", "b"))
  attr(x, "label") <- "example"
  attr(x, "tag") <- list(source = "test")

  result <- fct_relevel(x, "b")

  expect_identical(
    attributes(result)[c("label", "tag")],
    attributes(x)[c("label", "tag")]
  )
})


test_that("fct_relevel preserves names", {
  x <- factor(
    c(first = "a", second = "b", third = "c"),
    levels = c("c", "b", "a")
  )

  result <- fct_relevel(x, c("a", "b"))

  expect_identical(names(result), names(x))
  expect_identical(levels(result), c("a", "b", "c"))
})


test_that("fct_relevel converts character vectors to unordered factors", {
  x <- c(first = "z", second = NA_character_, third = "a", fourth = "b")

  expect_silent(result <- fct_relevel(x, "b"))

  expect_identical(levels(result), c("b", "a", "z"))
  expect_identical(as.character(result), unname(x))
  expect_identical(names(result), names(x))
  expect_identical(is.na(result), is.na(x))
  expect_identical(class(result), "factor")
})


test_that("fct_relevel rejects unsupported input types", {
  expect_error(
    fct_relevel(1:3, "a"),
    "`x` must be a factor or character vector",
    fixed = TRUE
  )
})


test_that("fct_relevel errors when ref is not character or factor", {
  x <- factor(c("a", "b", "c"))

  expect_error(
    fct_relevel(x, 1:2),
    "`ref` must be a character vector",
    fixed = TRUE
  )
})


test_that("fct_relevel validates after", {
  x <- factor(c("a", "b", "c"))

  expect_error(
    fct_relevel(x, "a", after = NA),
    "`after` must be a single non-missing whole number.",
    fixed = TRUE
  )

  expect_error(
    fct_relevel(x, "a", after = c(0, 1)),
    "`after` must be a single non-missing whole number.",
    fixed = TRUE
  )

  expect_error(
    fct_relevel(x, "a", after = -1),
    "`after` must be greater than or equal to 0.",
    fixed = TRUE
  )

  expect_error(
    fct_relevel(x, "a", after = 1.5),
    "`after` must be a whole number.",
    fixed = TRUE
  )
})

# fct_rev ----------

test_that("fct_rev reverses all levels without moving values", {
  x <- factor(
    c("a", "c", "a", "b"),
    levels = c("b", "a", "unused", "c")
  )

  result <- fct_rev(x)

  expect_identical(levels(result), c("c", "unused", "a", "b"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(is.na(result), is.na(x))
  expect_identical(class(result), "factor")
})


test_that("fct_rev preserves ordered factors", {
  x <- ordered(c("low", "high", "medium"), levels = c("low", "medium", "high"))

  result <- fct_rev(x)

  expect_identical(levels(result), c("high", "medium", "low"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(class(result), c("ordered", "factor"))
})


test_that("fct_rev preserves names and missing values", {
  x <- factor(
    c(first = "a", missing = NA_character_, last = "b"),
    levels = c("b", "a")
  )

  result <- fct_rev(x)

  expect_identical(names(result), names(x))
  expect_identical(levels(result), c("a", "b"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(is.na(result), is.na(x))
})


test_that("fct_rev converts character input to an unordered factor", {
  x <- c(first = "b", missing = NA_character_, last = "a")

  result <- fct_rev(x)

  expect_identical(class(result), "factor")
  expect_identical(levels(result), c("b", "a"))
  expect_identical(as.character(result), unname(x))
  expect_identical(names(result), names(x))
  expect_identical(is.na(result), is.na(x))
})


test_that("fct_rev handles empty inputs", {
  empty_factor <- factor(character(), levels = c("a", "unused", "b"))

  result <- fct_rev(empty_factor)
  empty_character_result <- fct_rev(character())

  expect_identical(levels(result), c("b", "unused", "a"))
  expect_identical(length(result), 0L)
  expect_identical(class(result), "factor")
  expect_identical(empty_character_result, factor(character()))
})


test_that("reversing twice restores an ordinary factor", {
  x <- factor(c("b", NA, "a"), levels = c("a", "unused", "b"))

  expect_identical(fct_rev(fct_rev(x)), x)
})


test_that("fct_rev rejects unsupported input types", {
  expect_error(fct_rev(1:3))
  expect_error(fct_rev(c(TRUE, FALSE)))
  expect_error(fct_rev(list("a", "b")))
  expect_error(fct_rev(NULL))
})


test_that("fct_rev preserves an explicit NA level", {
  x <- factor(c("a", NA, "b"), levels = c("a", "b", NA), exclude = NULL)
  expected <- factor(c("a", NA, "b"), levels = c(NA, "b", "a"), exclude = NULL)

  expect_identical(fct_rev(x), expected)
})

test_that("fct_rev does NOT create NA levels when character vector is supplied", {
  x <- c("a", NA_character_, "b")
  expected <- factor(c("a", NA_character_, "b"), levels = c("b", "a"))

  expect_identical(fct_rev(x), expected)
})

test_that("fct_rev preserves extra factor attributes", {
  x <- factor(c("a", "b"))
  attr(x, "label") <- "example"

  expect_identical(attr(fct_rev(x), "label"), "example")
})
