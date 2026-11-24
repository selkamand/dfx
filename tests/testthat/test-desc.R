test_that("desc flags a column name without changing its value", {
  for (column in c("value", "some column", "..DeSc..", " ")) {
    original <- column
    result <- desc(column)

    expect_identical(unname(result), column)
    expect_identical(names(result), "..DeSc..")
    expect_identical(column, original)
  }
})

test_that("desc replaces existing names and repeated calls keep the flag", {
  column <- c(old_name = "value")

  expect_identical(desc(column), c("..DeSc.." = "value"))
  expect_identical(desc(desc(column)), desc(column))
  expect_identical(column, c(old_name = "value"))
})

test_that("desc flags survive combination with ordinary column names", {
  expect_identical(
    c("group", desc("value"), desc("other")),
    c("group", "..DeSc.." = "value", "..DeSc.." = "other")
  )
})

test_that("desc rejects inputs that are not character strings", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(NULL)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(1)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(list("value"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(factor("value"))
  })
})

test_that("desc rejects attributes other than names", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(matrix("value"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(structure("value", note = "extra attribute"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(structure("value", class = "custom"))
  })
})

test_that("desc requires one non-missing, non-empty column name", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(character())
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(c("value", "other"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc(NA_character_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    desc("")
  })
})
