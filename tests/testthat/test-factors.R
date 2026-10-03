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


test_that("fct_relevel retains custom contrasts when reordering levels", {
  x <- factor(
    c("a", "b", "b", "c", "c", "c"),
    levels = c("a", "b", "unused", "c")
  )
  stats::contrasts(x) <- stats::contr.sum(levels(x))
  expect_warning(
    result <- fct_relevel(x, c("c", "a"), after = 1L),
    "explicit `contrasts` attribute",
    fixed = TRUE
  )

  expect_identical(levels(result), c("b", "c", "a", "unused"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(
    attr(result, "contrasts", exact = TRUE),
    attr(x, "contrasts", exact = TRUE)
  )
})


test_that("fct_relevel retains a character contrast setting", {
  x <- factor(c("a", "b", "c"), levels = c("a", "b", "c"))
  stats::contrasts(x) <- "contr.sum"

  expect_warning(result <- fct_relevel(x, "c"), "explicit `contrasts` attribute", fixed = TRUE)

  expect_identical(attr(result, "contrasts"), "contr.sum")
  expect_identical(rownames(stats::contrasts(result)), levels(result))
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


test_that("fct_rev retains custom contrasts when reversing levels", {
  x <- ordered(
    c("a", "b", "b", "c", "c", "c"),
    levels = c("a", "b", "unused", "c")
  )
  stats::contrasts(x) <- stats::contr.sum(levels(x))
  expect_warning(result <- fct_rev(x), "explicit `contrasts` attribute", fixed = TRUE)

  expect_identical(levels(result), c("c", "unused", "b", "a"))
  expect_identical(class(result), c("ordered", "factor"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(
    attr(result, "contrasts", exact = TRUE),
    attr(x, "contrasts", exact = TRUE)
  )
})


test_that("fct_rev retains a character contrast setting", {
  x <- ordered(c("a", "b", "c"), levels = c("a", "b", "c"))
  stats::contrasts(x) <- "contr.sum"

  expect_warning(result <- fct_rev(x), "explicit `contrasts` attribute", fixed = TRUE)

  expect_identical(attr(result, "contrasts"), "contr.sum")
  expect_identical(rownames(stats::contrasts(result)), levels(result))
})

# fct_infreq ----------

test_that("fct_infreq orders levels by descending observed frequency", {
  x <- factor(
    c("a", "b", "b", "c", "c", "c"),
    levels = c("b", "a", "c", "unused")
  )

  result <- fct_infreq(x)

  expect_identical(levels(result), c("c", "b", "a", "unused"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(class(result), "factor")
})


test_that("fct_infreq breaks ties in original level order", {
  x <- factor(
    c("a", "b", "a", "b", "c"),
    levels = c("c", "b", "a", "unused2", "unused1")
  )

  expect_identical(
    levels(fct_infreq(x)),
    c("b", "a", "c", "unused2", "unused1")
  )
})


test_that("fct_infreq converts character input without adding an NA level", {
  x <- c(
    first = "z",
    second = "a",
    third = "z",
    missing = NA_character_,
    fifth = "b",
    sixth = "b",
    seventh = "b"
  )

  expect_silent(result <- fct_infreq(x))

  expect_identical(levels(result), c("b", "z", "a"))
  expect_identical(as.character(result), unname(x))
  expect_identical(names(result), names(x))
  expect_identical(is.na(result), is.na(x))
  expect_identical(class(result), "factor")
})


test_that("fct_infreq does not count ordinary missing values", {
  x <- factor(
    c("a", NA, NA, NA, "b", "b"),
    levels = c("a", "b", "unused")
  )

  result <- fct_infreq(x)

  expect_identical(levels(result), c("b", "a", "unused"))
  expect_identical(is.na(result), is.na(x))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_infreq counts an observed explicit NA level", {
  x <- factor(
    c("a", NA, NA, "b"),
    levels = c("a", "b", NA, "unused"),
    exclude = NULL
  )

  result <- fct_infreq(x)

  expect_identical(levels(result), c(NA_character_, "a", "b", "unused"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(is.na(result), is.na(x))
})


test_that("fct_infreq keeps an unused explicit NA level among zero counts", {
  x <- factor(
    c("b", "b", "a"),
    levels = c(NA, "unused", "a", "b"),
    exclude = NULL
  )

  result <- fct_infreq(x)

  expect_identical(levels(result), c("b", "a", NA_character_, "unused"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_infreq distinguishes a literal NA label from an NA level", {
  x <- factor(
    c("NA", NA, "NA"),
    levels = c(NA, "NA", "unused"),
    exclude = NULL
  )

  expect_identical(
    levels(fct_infreq(x)),
    c("NA", NA_character_, "unused")
  )
})


test_that("fct_infreq handles empty and all-missing inputs", {
  empty_factor <- factor(character(), levels = c("b", "a"))
  all_missing_factor <- factor(c(NA, NA), levels = c("b", "a"))

  expect_identical(fct_infreq(empty_factor), empty_factor)
  expect_identical(fct_infreq(character()), factor(character()))
  expect_identical(fct_infreq(all_missing_factor), all_missing_factor)
  expect_identical(
    fct_infreq(c(NA_character_, NA_character_)),
    factor(c(NA, NA))
  )
})


test_that("fct_infreq preserves unordered factors across ordered settings", {
  x <- factor(
    c(first = "a", second = "b", third = "b"),
    levels = c("unused", "a", "b")
  )
  attr(x, "label") <- "example"
  attr(x, "tag") <- list(source = "test")
  settings <- list(
    preserve = list(value = NA, class = "factor"),
    order = list(value = TRUE, class = c("ordered", "factor")),
    unorder = list(value = FALSE, class = "factor")
  )

  for (setting in settings) {
    result <- fct_infreq(x, ordered = setting$value)

    expect_identical(levels(result), c("b", "a", "unused"))
    expect_identical(class(result), setting$class)
    expect_identical(as.character(result), as.character(x))
    expect_identical(names(result), names(x))
    expect_identical(attr(result, "label"), attr(x, "label"))
    expect_identical(attr(result, "tag"), attr(x, "tag"))
  }
})


test_that("fct_infreq preserves ordered factors across ordered settings", {
  x <- ordered(
    c(first = "a", second = "b", third = "b"),
    levels = c("unused", "a", "b")
  )
  attr(x, "label") <- "example"
  settings <- list(
    preserve = list(value = NA, class = c("ordered", "factor")),
    order = list(value = TRUE, class = c("ordered", "factor")),
    unorder = list(value = FALSE, class = "factor")
  )

  for (setting in settings) {
    result <- fct_infreq(x, ordered = setting$value)

    expect_identical(levels(result), c("b", "a", "unused"))
    expect_identical(class(result), setting$class)
    expect_identical(as.character(result), as.character(x))
    expect_identical(names(result), names(x))
    expect_identical(attr(result, "label"), attr(x, "label"))
  }
})


test_that("fct_infreq supports all ordered settings for character input", {
  x <- c(first = "a", second = "b", third = "b")
  settings <- list(
    preserve = list(value = NA, class = "factor"),
    order = list(value = TRUE, class = c("ordered", "factor")),
    unorder = list(value = FALSE, class = "factor")
  )

  for (setting in settings) {
    result <- fct_infreq(x, ordered = setting$value)

    expect_identical(levels(result), c("b", "a"))
    expect_identical(class(result), setting$class)
    expect_identical(as.character(result), unname(x))
    expect_identical(names(result), names(x))
  }
})


test_that("fct_infreq rejects unsupported input types", {
  expect_error(
    fct_infreq(1:3),
    "`x` must be a factor or character vector",
    fixed = TRUE
  )
  expect_error(
    fct_infreq(c(TRUE, FALSE)),
    "`x` must be a factor or character vector",
    fixed = TRUE
  )
  expect_error(
    fct_infreq(list("a", "b")),
    "`x` must be a factor or character vector",
    fixed = TRUE
  )
  expect_error(
    fct_infreq(NULL),
    "`x` must be a factor or character vector",
    fixed = TRUE
  )
  expect_error(
    fct_infreq(data.frame(x = "a")),
    "`x` must be a factor or character vector",
    fixed = TRUE
  )
})


test_that("fct_infreq validates ordered", {
  x <- factor(c("a", "b"))

  expect_error(
    fct_infreq(x, ordered = 1),
    "`ordered` must be either TRUE, FALSE or NA",
    fixed = TRUE
  )
  expect_error(
    fct_infreq(x, ordered = "TRUE"),
    "`ordered` must be either TRUE, FALSE or NA",
    fixed = TRUE
  )
  expect_error(
    fct_infreq(x, ordered = NULL),
    "`ordered` must be either TRUE, FALSE or NA",
    fixed = TRUE
  )
  expect_error(
    fct_infreq(x, ordered = logical()),
    "`ordered` must be a scalar value",
    fixed = TRUE
  )
  expect_error(
    fct_infreq(x, ordered = c(TRUE, FALSE)),
    "`ordered` must be a scalar value",
    fixed = TRUE
  )
})


test_that("fct_infreq retains custom contrasts when reordering levels", {
  x <- factor(
    c("a", "b", "b", "c", "c", "c"),
    levels = c("a", "b", "unused", "c")
  )
  stats::contrasts(x) <- stats::contr.sum(levels(x))

  expect_warning(result <- fct_infreq(x), "explicit `contrasts` attribute", fixed = TRUE)

  expect_identical(levels(result), c("c", "b", "a", "unused"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(
    attr(result, "contrasts", exact = TRUE),
    attr(x, "contrasts", exact = TRUE)
  )
})


test_that("fct_infreq retains a character contrast setting", {
  x <- factor(c("a", "b", "b"), levels = c("a", "b"))
  stats::contrasts(x) <- "contr.sum"

  expect_warning(result <- fct_infreq(x), "explicit `contrasts` attribute", fixed = TRUE)

  expect_identical(attr(result, "contrasts"), "contr.sum")
  expect_identical(rownames(stats::contrasts(result)), levels(result))
})


test_that("fct_infreq warns once for explicit contrasts across ordered settings", {
  x <- factor(
    c("a", "b", "b", "c", "c", "c"),
    levels = c("a", "b", "unused", "c")
  )
  stats::contrasts(x) <- stats::contr.sum(levels(x))
  for (ordered_setting in list(NA, TRUE, FALSE)) {
    warnings <- character()
    result <- withCallingHandlers(
      fct_infreq(x, ordered = ordered_setting),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    expect_length(warnings, 1L)
    expect_match(warnings[[1L]], "explicit `contrasts` attribute", fixed = TRUE)
    expect_identical(
      attr(result, "contrasts", exact = TRUE),
      attr(x, "contrasts", exact = TRUE),
      info = paste("ordered =", ordered_setting)
    )
  }
})

# fct_expand ----------

test_that("fct_expand appends new levels after existing and unused levels", {
  x <- factor(c("b", "a", "b"), levels = c("b", "a", "unused"))

  result <- fct_expand(x, c("d", "c"))

  expect_identical(levels(result), c("b", "a", "unused", "d", "c"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(as.integer(result), as.integer(x))
  expect_identical(class(result), "factor")
})


test_that("fct_expand ignores existing and repeated levels without moving them", {
  x <- factor(c("a", "b"), levels = c("b", "a", "unused"))

  result <- fct_expand(x, c("a", "new2", "new1", "new2", "b", "new3"))

  expect_identical(
    levels(result),
    c("b", "a", "unused", "new2", "new1", "new3")
  )
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_expand handles empty additions and empty inputs", {
  x <- factor(c("a", NA), levels = c("a", "unused"))
  empty_factor <- factor(character(), levels = c("b", "a"))

  expect_identical(fct_expand(x, character()), x)
  expect_identical(
    fct_expand(empty_factor, "c"),
    factor(character(), levels = c("b", "a", "c"))
  )
  expect_identical(
    fct_expand(character(), "c"),
    factor(character(), levels = "c")
  )
})


test_that("fct_expand preserves ordered factors", {
  x <- ordered(c("low", "high"), levels = c("high", "low"))
  expected <- ordered(c("low", "high"), levels = c("high", "low", "medium"))

  expect_identical(fct_expand(x, "medium"), expected)
})


test_that("fct_expand preserves names, missing values, and extra attributes", {
  x <- factor(
    c(first = "b", missing = NA_character_, last = "a"),
    levels = c("a", "b", "unused")
  )
  attr(x, "label") <- "example"
  attr(x, "tag") <- list(source = "test")

  result <- fct_expand(x, "c")

  expect_identical(levels(result), c("a", "b", "unused", "c"))
  expect_identical(as.character(result), as.character(x))
  expect_identical(is.na(result), is.na(x))
  expect_identical(names(result), names(x))
  expect_identical(attr(result, "label"), attr(x, "label"))
  expect_identical(attr(result, "tag"), attr(x, "tag"))
})


test_that("fct_expand converts character input to an unordered factor", {
  x <- c(first = "z", missing = NA_character_, last = "a")

  result <- fct_expand(x, "new")

  expect_identical(levels(result), c("a", "z", "new"))
  expect_identical(as.character(result), unname(x))
  expect_identical(is.na(result), is.na(x))
  expect_identical(names(result), names(x))
  expect_identical(class(result), "factor")
})


test_that("fct_expand preserves an explicit NA level", {
  x <- factor(c("a", NA, "b"), levels = c("a", NA, "b"), exclude = NULL)
  expected <- factor(
    c("a", NA, "b"),
    levels = c("a", NA, "b", "c"),
    exclude = NULL
  )

  expect_identical(fct_expand(x, "c"), expected)
})


test_that("fct_expand can add an explicit NA level", {
  x <- factor(c("a", NA), levels = "a")
  expected <- factor(c("a", NA), levels = c("a", NA), exclude = NULL)

  expect_identical(fct_expand(x, NA_character_), expected)
})


test_that("fct_expand rejects unsupported x inputs", {
  for (x in list(1:2, c(TRUE, FALSE), list("a"), NULL)) {
    expect_error(
      fct_expand(x, "new"),
      "`x` must be a factor or character vector",
      fixed = TRUE
    )
  }
})


test_that("fct_expand requires a character add argument", {
  x <- factor("a")

  expect_error(fct_expand(x))
  for (add in list(list("new"), factor("new"), NULL)) {
    expect_error(
      fct_expand(x, add),
      "must be a character vector",
      fixed = TRUE
    )
  }
})


test_that("fct_expand reports the class of an invalid add argument", {
  x <- factor("a")

  expect_error(fct_expand(x, 1L), "class [integer]", fixed = TRUE)
})


test_that("fct_expand preserves custom matrix contrasts when adding levels", {
  x <- factor(c("a", "b", "a"), levels = c("a", "b", "unused"))
  stats::contrasts(x) <- stats::contr.sum(levels(x))

  expect_warning(result <- fct_expand(x, "c"), "explicit `contrasts` attribute", fixed = TRUE)

  expect_identical(levels(result), c("a", "b", "unused", "c"))
  expect_identical(
    attr(result, "contrasts", exact = TRUE),
    attr(x, "contrasts", exact = TRUE)
  )
})


test_that("fct_expand preserves a character contrast setting", {
  x <- factor(c("a", "b"))
  stats::contrasts(x) <- "contr.sum"

  expect_warning(result <- fct_expand(x, "c"), "explicit `contrasts` attribute", fixed = TRUE)

  expect_identical(levels(result), c("a", "b", "c"))
  expect_identical(attr(result, "contrasts"), "contr.sum")
})


test_that("unchanged levels do not warn about explicit contrasts", {
  x <- factor(c("a", "a", "b"), levels = c("a", "b"))
  stats::contrasts(x) <- stats::contr.sum(levels(x))

  expect_silent(fct_relevel(x, "a"))
  expect_silent(fct_infreq(x))
  expect_silent(fct_infreq(x, ordered = TRUE))
  expect_silent(fct_expand(x, c("a", "b")))
})
