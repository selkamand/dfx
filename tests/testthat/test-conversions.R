test_that("convert_vector_to_match_target converts to simple atomic target types", {
  expect_identical(
    convert_vector_to_match_target(c("1", "2", "3"), numeric()),
    c(1, 2, 3)
  )

  expect_identical(
    convert_vector_to_match_target(c("1", "2", "3"), integer()),
    c(1L, 2L, 3L)
  )

  expect_identical(
    convert_vector_to_match_target(c(1, 2, 3), character()),
    c("1", "2", "3")
  )

  expect_identical(
    convert_vector_to_match_target(c("1+2i", "3+4i"), complex()),
    c(1 + 2i, 3 + 4i)
  )

  expect_identical(
    convert_vector_to_match_target(c(TRUE, FALSE), character()),
    c("TRUE", "FALSE")
  )
})


test_that("convert_vector_to_match_target converts to factor targets", {
  result <- convert_vector_to_match_target(c("a", "b", "a"), factor())

  expect_s3_class(result, "factor")
  expect_identical(as.character(result), c("a", "b", "a"))
  expect_identical(levels(result), c("a", "b"))
})


test_that("convert_vector_to_match_target converts to Date targets", {
  result <- convert_vector_to_match_target(
    c("2024-01-01", "2024-02-03"),
    as.Date(character())
  )

  expect_s3_class(result, "Date")
  expect_identical(
    result,
    as.Date(c("2024-01-01", "2024-02-03"))
  )
})


test_that("convert_vector_to_match_target converts to POSIXct targets", {
  result <- convert_vector_to_match_target(
    c("2024-01-01 10:00:00", "2024-01-02 11:30:00"),
    as.POSIXct(character())
  )

  expect_s3_class(result, "POSIXct")
  expect_true(inherits(result, "POSIXt"))
  expect_identical(
    result,
    as.POSIXct(c("2024-01-01 10:00:00", "2024-01-02 11:30:00"))
  )
})

# Dropped POSIXlt since its list based (not a vector)
# test_that("convert_vector_to_match_target converts to POSIXlt targets", {
#   result <- convert_vector_to_match_target(
#     c("2024-01-01 10:00:00", "2024-01-02 11:30:00"),
#     as.POSIXlt(character())
#   )
#
#   expect_s3_class(result, "POSIXlt")
#   expect_true(inherits(result, "POSIXt"))
#   expect_identical(
#     as.POSIXct(result),
#     as.POSIXct(c("2024-01-01 10:00:00", "2024-01-02 11:30:00"))
#   )
# })
#

test_that("convert_vector_to_match_target converts to pairlist targets", {
  result <- convert_vector_to_match_target(
    list(a = 1, b = 2),
    pairlist(s = 1) # Pairlist has to have some value or it returns NULL
  )

  expect_type(result, "pairlist")
  expect_identical(as.list(result), list(a = 1, b = 2))
})


test_that("convert_vector_to_match_target converts to array targets", {
  result <- convert_vector_to_match_target(
    c(1, 2, 3),
    array()
  )

  expect_true(is.array(result))
  expect_identical(as.vector(result), c(1, 2, 3))
})

test_that("convert_vector_to_match_target converts factor labels to numeric values, not factor codes", {
  x <- factor(c("10", "20", "30"))

  # Factor to numeric
  expect_identical(
    convert_vector_to_match_target(x, numeric()),
    c(10, 20, 30)
  )

  # Factor to integer
  expect_identical(
    convert_vector_to_match_target(x, integer()),
    c(10L, 20L, 30L)
  )
})

test_that("convert_vector_to_match_target treats non-numeric factor labels as failed numeric conversion", {
  x <- factor(c("10", "not-a-number", "30"))

  expect_snapshot(
    error = TRUE,
    convert_vector_to_match_target(x, numeric(), failure = "error")
  )

  expect_identical(
    convert_vector_to_match_target(x, numeric(), failure = "keep_original"),
    x
  )
})

test_that("convert_vector_to_match_target treats conversion warnings as failures by default", {
  expect_error(
    convert_vector_to_match_target("not-a-number", numeric()),
    "conversion failure"
  )

  expect_error(
    convert_vector_to_match_target("not-a-number", integer()),
    "conversion failure"
  )
})


test_that("convert_vector_to_match_target keeps original vector when warning occurs and failure is keep_original", {
  x <- "not-a-number"

  result <- convert_vector_to_match_target(
    x,
    numeric(),
    failure = "keep_original"
  )

  expect_identical(result, x)
})


test_that("convert_vector_to_match_target errors when conversion itself errors", {
  x <- list(a = 1, b = 2)

  expect_error(
    convert_vector_to_match_target(x, as.Date(character())),
    "conversion failure"
  )
})


test_that("convert_vector_to_match_target keeps original vector when conversion errors and failure is keep_original", {
  x <- list(a = 1, b = 2)

  result <- convert_vector_to_match_target(
    x,
    as.Date(character()),
    failure = "keep_original"
  )

  expect_identical(result, x)
})


test_that("convert_vector_to_match_target errors for unsupported target classes by default", {
  target <- structure(list(), class = "unsupported_target_class")

  expect_error(
    convert_vector_to_match_target(c(1, 2, 3), target),
    "conversion failure"
  )
})


test_that("convert_vector_to_match_target keeps original vector for unsupported target classes when requested", {
  x <- c(1, 2, 3)
  target <- structure(list(), class = "unsupported_target_class")

  result <- convert_vector_to_match_target(
    x,
    target,
    failure = "keep_original"
  )

  expect_identical(result, x)
})


test_that("convert_vector_to_match_target validates failure argument", {
  expect_error(
    convert_vector_to_match_target(c(1, 2, 3), character(), failure = "other"),
    "'arg' should be one of"
  )
})


test_that("convert_vector_to_match_target returns objects matching the intent of the target class", {
  expect_type(
    convert_vector_to_match_target(c("1", "2"), numeric()),
    "double"
  )

  expect_type(
    convert_vector_to_match_target(c("1", "2"), integer()),
    "integer"
  )

  expect_type(
    convert_vector_to_match_target(c(1, 2), character()),
    "character"
  )

  expect_type(
    convert_vector_to_match_target(c("1+0i", "2+0i"), complex()),
    "complex"
  )

  expect_s3_class(
    convert_vector_to_match_target(c("a", "b"), factor()),
    "factor"
  )

  expect_s3_class(
    convert_vector_to_match_target("2024-01-01", as.Date(character())),
    "Date"
  )

  expect_s3_class(
    convert_vector_to_match_target("2024-01-01 00:00:00", as.POSIXct(character())),
    "POSIXct"
  )
})

test_that("convert_vector_to_match_target handles ordered factor targets", {
  target <- ordered(c("low", "medium", "high"), levels = c("low", "medium", "high"))

  result <- convert_vector_to_match_target(
    c("low", "high"),
    target
  )

  expect_s3_class(result, "ordered")
  expect_s3_class(result, "factor")
  expect_identical(as.character(result), c("low", "high"))
})

test_that("convert_vector_to_match_target ignores factor levels from target", {
  target <- factor(c("low", "medium", "high"), levels = c("low", "medium", "high"))

  result <- convert_vector_to_match_target(
    c("high", "low"),
    target
  )

  expect_s3_class(result, "factor")
  expect_identical(levels(result), c("high", "low"))
})

test_that("convert_vector_to_match_target preserves POSIXct timezone from target", {
  target <- as.POSIXct(character(), tz = "UTC")

  result <- convert_vector_to_match_target(
    "2024-01-01 12:00:00",
    target
  )

  expect_s3_class(result, "POSIXct")
  expect_identical(attr(result, "tzone"), "UTC")
})

test_that("convert_vector_to_match_target error message reports the target class", {
  expect_error(
    convert_vector_to_match_target("not-a-number", numeric()),
    regexp = "can not convert \\[character\\] to \\[numeric\\]",
    fixed = FALSE
  )
})

test_that("convert_vector_to_match_target handles difftime targets", {
  target <- as.difftime(numeric(), units = "days")

  result <- convert_vector_to_match_target(
    c(1, 2),
    target
  )

  expect_s3_class(result, "difftime")
  expect_identical(attr(result, "units"), "days")
  expect_equal(as.numeric(result), c(1, 2))
})

test_that("convert_vector_to_match_target handles raw targets", {
  result <- convert_vector_to_match_target(
    c(1, 2, 255),
    raw()
  )

  expect_type(result, "raw")
  expect_identical(result, as.raw(c(1, 2, 255)))
})

test_that("convert_vector_to_match_target converts to logical targets", {
  expect_identical(
    convert_vector_to_match_target(c("TRUE", "FALSE", "TRUE"), logical()),
    c(TRUE, FALSE, TRUE)
  )

  expect_identical(
    convert_vector_to_match_target(c(1, 0, 1), logical()),
    c(TRUE, FALSE, TRUE)
  )
  expect_type(
    convert_vector_to_match_target(c(1, 0), logical()),
    "logical"
  )
})

test_that("convert_vector_to_match_target errors clearly for NULL targets", {
  expect_snapshot(
    error = TRUE,
    convert_vector_to_match_target(1:3, NULL)
  )
})

test_that("convert_vector_to_match_target handles NULL x intentionally", {
  expect_identical(
    convert_vector_to_match_target(NULL, character()),
    character()
  )
})

test_that("convert_vector_to_match_target converts NA to typed NA", {
  expect_identical(
    convert_vector_to_match_target(NA, character()),
    NA_character_
  )

  expect_identical(
    convert_vector_to_match_target(NA, integer()),
    NA_integer_
  )

  expect_identical(
    convert_vector_to_match_target(NA, numeric()),
    NA_real_
  )

  expect_equal(
    convert_vector_to_match_target(NA_character_, complex()),
    NA_complex_
  )

  expect_identical(
    convert_vector_to_match_target(NA, logical()),
    NA
  )

  expect_identical(
    convert_vector_to_match_target(NA, as.Date(character())),
    as.Date(NA)
  )
})

test_that("convert_vector_to_match_target: snapshot errors thrown when conversion function throws a warning", {
  expect_snapshot(
    error = TRUE,
    convert_vector_to_match_target("not-a-number", numeric(), failure = "error")
  )

  expect_snapshot(
    error = TRUE,
    convert_vector_to_match_target("not-an-integer", integer(), failure = "error")
  )
})


test_that("convert_vector_to_match_target: snapshot errors thrown when target class in unsupported", {
  target <- structure(list(), class = "unsupported_target_class")

  expect_snapshot(
    error = TRUE,
    convert_vector_to_match_target(c(1, 2, 3), target, failure = "error")
  )
})


test_that("convert_vector_to_match_target: snapshots invalid failure argument error message", {
  expect_snapshot(
    error = TRUE,
    convert_vector_to_match_target(c(1, 2, 3), character(), failure = "other")
  )
})

test_that("convert_vector_to_match_target snapshots custom error_prefix", {
  expect_snapshot(
    error = TRUE,
    convert_vector_to_match_target(
      "not-a-number",
      numeric(),
      failure = "error",
      error_prefix = "mutate column `score`: "
    )
  )
})
