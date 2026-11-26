test_that("arrange moves whole array slices with their rows", {
  data <- data.frame(id = c(2L, 3L, 1L))
  data$payload <- array(
    1:6, c(3L, 1L, 2L), dimnames = list(letters[1:3], "x", c("a", "b"))
  )

  result <- arrange(data, "id")

  expect_identical(result$id, 1:3)
  expect_identical(result$payload, data$payload[c(3L, 1L, 2L), , , drop = FALSE])
})

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

test_that("arrange preserves POSIXlt columns when ordering timestamps", {
  instants <- as.POSIXct(
    c("2024-11-03 06:30:00", "2024-11-03 05:30:00"),
    tz = "UTC"
  )

  for (zone in c("UTC", "America/New_York")) {
    data <- data.frame(id = 1:6, tie = c(1, 2, 1, 1, 1, 1))
    # Assign after construction so data.frame() cannot coerce POSIXlt to POSIXct.
    # In New York, these instants span both occurrences of 01:30 at DST fallback.
    data$time <- as.POSIXlt(instants[c(1, 2, NA, 2, 1, 2)], tz = zone)
    expect_s3_class(data$time, "POSIXlt")

    expect_identical(arrange(data, "time"), data[c(2, 4, 6, 1, 5, 3), ])
    expect_identical(arrange(data, desc("time")), data[c(1, 5, 2, 4, 6, 3), ])
    expect_identical(
      arrange(data, c("time", "tie")),
      data[c(4, 6, 2, 1, 5, 3), ]
    )
    expect_identical(
      arrange(data, c(desc("time"), "tie")),
      data[c(1, 5, 4, 6, 2, 3), ]
    )
    expect_identical(arrange(data[FALSE, ], "time"), data[FALSE, ])
    expect_identical(arrange(data[FALSE, ], desc("time")), data[FALSE, ])
    missing <- data[c(3, 3), ]
    expect_identical(arrange(missing, "time"), missing)
    expect_identical(arrange(missing, desc("time")), missing)
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

test_that("arrange keeps NA and NaN tied and last in ascending sorts", {
  data <- data.frame(value = c(NaN, NA_real_, 2, 1, NaN, NA_real_), id = 1:6)

  expect_identical(
    arrange(data, "value"),
    data[c(4, 3, 1, 2, 5, 6), , drop = FALSE]
  )
})

test_that("arrange keeps NA and NaN tied and last in descending sorts", {
  data <- data.frame(value = c(NaN, NA_real_, 2, 1, NaN, NA_real_), id = 1:6)

  expect_identical(
    arrange(data, desc("value")),
    data[c(3, 4, 1, 2, 5, 6), , drop = FALSE]
  )
})

test_that("arrange uses secondary columns to break ties between NA and NaN", {
  data <- data.frame(
    value = c(NaN, NA_real_, 2, 1, NaN, NA_real_),
    secondary = c(2, 1, 0, 0, 1, 2),
    id = 1:6
  )

  expect_identical(
    arrange(data, c("value", "secondary")),
    data[c(4, 3, 2, 5, 1, 6), , drop = FALSE]
  )
  expect_identical(
    arrange(data, c(desc("value"), "secondary")),
    data[c(3, 4, 2, 5, 1, 6), , drop = FALSE]
  )
  expect_identical(
    arrange(data, c("value", desc("secondary"))),
    data[c(4, 3, 1, 6, 2, 5), , drop = FALSE]
  )
  expect_identical(
    arrange(data, c(desc("value"), desc("secondary"))),
    data[c(3, 4, 1, 6, 2, 5), , drop = FALSE]
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

test_that("arrange defaults to C ordering for character columns", {
  data <- data.frame(value = c("a", "Z", "\u00e4", "A", "", "a", NA_character_))
  ascending <- data[c(5, 4, 2, 1, 6, 3, 7), , drop = FALSE]
  descending <- data[c(3, 1, 6, 2, 4, 5, 7), , drop = FALSE]

  expect_identical(arrange(data, "value"), ascending)
  expect_identical(arrange(data, "value", locale = "C"), ascending)
  expect_identical(arrange(data, desc("value")), descending)
  expect_identical(arrange(data, desc("value"), locale = "C"), descending)
})

# Restore the locale before reporting expectations so LC_CTYPE = "C" cannot
# turn the progress reporter's Unicode spinner into escaped character codes.
for (system_locale in c("C", "en_US.UTF-8", "sv_SE.UTF-8")) {
  test_that(paste("arrange respects locale choices under", system_locale), {
    skip_if_not_installed("withr")
    requested <- c(LC_COLLATE = system_locale, LC_CTYPE = system_locale)
    available <- suppressWarnings(withr::with_locale(
      requested,
      all(vapply(names(requested), Sys.getlocale, character(1)) == requested)
    ))
    skip_if(!available, paste("System locale unavailable:", system_locale))

    locale_before <- vapply(names(requested), Sys.getlocale, character(1))
    results <- withr::with_locale(requested, {
      latin1 <- iconv("\u00e4", from = "UTF-8", to = "latin1")
      data <- data.frame(
        value = c("a", "Z", "\u00e4", "A", "", latin1, NA_character_),
        id = 1:7
      )

      list(
        ascending = arrange(data, "value"),
        ascending_expected = data[c(5, 4, 2, 1, 3, 6, 7), , drop = FALSE],
        descending = arrange(data, desc("value")),
        descending_expected = data[c(3, 6, 1, 2, 4, 5, 7), , drop = FALSE],
        session_ascending = arrange(data, "value", locale = NULL),
        session_ascending_expected = data[order(data$value), , drop = FALSE],
        session_descending = arrange(data, desc("value"), locale = NULL),
        session_descending_expected = data[
          order(-xtfrm(data$value)),
          ,
          drop = FALSE
        ],
        locale_after = vapply(names(requested), Sys.getlocale, character(1))
      )
    })
    expect_identical(results$ascending, results$ascending_expected)
    expect_identical(results$descending, results$descending_expected)
    expect_identical(
      results$session_ascending,
      results$session_ascending_expected
    )
    expect_identical(
      results$session_descending,
      results$session_descending_expected
    )
    expect_identical(results$locale_after, requested)
    expect_identical(
      vapply(names(requested), Sys.getlocale, character(1)),
      locale_before
    )
  })
}

test_that("arrange uses consistent C ordering across character encodings", {
  latin1 <- iconv("\u00e4", from = "UTF-8", to = "latin1")
  data <- data.frame(
    value = c(latin1, "\u00e4", "\u00ff", NA_character_),
    secondary = c(2L, 1L, 0L, 0L),
    id = 1:4
  )
  original <- data

  expect_identical(arrange(data, "value"), data)
  expect_identical(
    arrange(data, desc("value")),
    data[c(3, 1, 2, 4), , drop = FALSE]
  )
  expect_identical(
    arrange(data, c("value", "secondary")),
    data[c(2, 1, 3, 4), , drop = FALSE]
  )
  expect_identical(
    arrange(data, c(desc("value"), "secondary")),
    data[c(3, 2, 1, 4), , drop = FALSE]
  )
  expect_identical(data, original)
})

for (system_locale in c("default", "C", "en_US.UTF-8")) {
  test_that(
    paste(
      "arrange orders native text after UTF-8 conversion under",
      system_locale
    ),
    {
      requested <- c(LC_CTYPE = system_locale)
      if (system_locale == "default") {
        requested <- requested[FALSE]
      } else {
        available <- suppressWarnings(withr::with_locale(
          requested,
          identical(Sys.getlocale("LC_CTYPE"), system_locale)
        ))
        skip_if(!available, paste("System locale unavailable:", system_locale))
      }

      results <- withr::with_locale(requested, {
        data <- data.frame(
          value = c(rawToChar(as.raw(255)), "<ff>", "z", NA_character_),
          secondary = c(2L, 1L, 0L, 0L),
          id = 1:4
        )

        # The unmarked byte can convert differently across platforms, so use
        # the current session's UTF-8 keys to compute the expected row order.
        key <- enc2utf8(data$value)
        expected <- list(
          ascending = order(key, method = "radix", na.last = TRUE),
          descending = order(
            key,
            method = "radix",
            na.last = TRUE,
            decreasing = TRUE
          ),
          ascending_secondary = order(
            key,
            data$secondary,
            method = "radix",
            na.last = TRUE
          ),
          descending_secondary = order(
            key,
            data$secondary,
            method = "radix",
            na.last = TRUE,
            decreasing = c(TRUE, FALSE)
          )
        )
        columns <- list(
          ascending = "value",
          descending = desc("value"),
          ascending_secondary = c("value", "secondary"),
          descending_secondary = c(desc("value"), "secondary")
        )
        key_bytes <- vapply(
          key[!is.na(key)],
          function(value) paste(charToRaw(value), collapse = " "),
          character(1)
        )

        list(
          expected = expected,
          bytes = lapply(data$value, charToRaw),
          encoding = Encoding(data$value),
          missing = is.na(data$value),
          actual = lapply(columns, function(keys) {
            arrange(data, keys, locale = "C")
          }),
          info = paste0(
            R.version.string,
            " (",
            R.version$platform,
            ")",
            ", LC_CTYPE = ",
            Sys.getlocale("LC_CTYPE"),
            ", converted key bytes = ",
            paste(key_bytes, collapse = " / ")
          )
        )
      })

      for (variant in names(results$expected)) {
        rows <- results$expected[[variant]]
        actual <- results$actual[[variant]]
        info <- paste(variant, results$info)
        expect_identical(actual$id, rows, info = info)
        expect_identical(
          is.na(actual$value),
          results$missing[rows],
          info = info
        )
        expect_identical(
          lapply(actual$value, charToRaw),
          results$bytes[rows],
          info = info
        )
        expect_identical(
          Encoding(actual$value),
          results$encoding[rows],
          info = info
        )
      }
    }
  )
}

for (system_locale in c("C", "en_US.UTF-8")) {
  test_that(
    paste("arrange rejects bytes-marked sort keys under", system_locale),
    {
      requested <- c(LC_CTYPE = system_locale)
      available <- suppressWarnings(withr::with_locale(
        requested,
        identical(Sys.getlocale("LC_CTYPE"), system_locale)
      ))
      skip_if(!available, paste("System locale unavailable:", system_locale))

      byte <- rawToChar(as.raw(255))
      Encoding(byte) <- "bytes"
      data <- data.frame(
        value = c(byte, "<ff>", "z", byte, NA_character_),
        secondary = c(2L, 1L, 0L, 1L, 0L),
        id = 1:5
      )
      original <- data

      expect_snapshot(error = TRUE, cran = TRUE, {
        withr::with_locale(requested, {
          arrange(data, "value")
        })
      })
      expect_snapshot(error = TRUE, cran = TRUE, {
        withr::with_locale(requested, {
          arrange(data, desc("value"), locale = "C")
        })
      })
      expect_snapshot(error = TRUE, cran = TRUE, {
        withr::with_locale(requested, {
          arrange(data, "value", locale = NULL)
        })
      })
      expect_snapshot(error = TRUE, cran = TRUE, {
        withr::with_locale(requested, {
          arrange(data, desc("value"), locale = NULL)
        })
      })
      expect_snapshot(error = TRUE, cran = TRUE, {
        withr::with_locale(requested, {
          arrange(data, c("secondary", "value"), locale = "C")
        })
      })
      expect_snapshot(error = TRUE, cran = TRUE, {
        withr::with_locale(requested, {
          arrange(data, c("secondary", desc("value")), locale = NULL)
        })
      })
      expect_identical(withr::with_locale(requested, arrange(data, "id")), data)
      expect_identical(
        withr::with_locale(requested, arrange(data, character())),
        data
      )
      expect_identical(data, original)
    }
  )
}

test_that("arrange applies C ordering to character keys in either position", {
  data <- data.frame(
    group = c(1L, 1L, 1L, 2L, 2L, 2L),
    text = c("a", "A", "a", "a", "A", "a"),
    score = c(1L, 2L, 3L, 3L, 2L, 1L)
  )

  expect_identical(
    arrange(data, c("group", desc("text"), "score")),
    data[c(1, 3, 2, 6, 4, 5), , drop = FALSE]
  )
  expect_identical(
    arrange(data, c("text", desc("group"), "score")),
    data[c(5, 2, 6, 4, 1, 3), , drop = FALSE]
  )
})

test_that("arrange with NULL locale retains session collation", {
  data <- data.frame(
    text = c("a", "Z", "\u00e4", "A", "a", NA_character_),
    score = c(1L, 2L, 3L, 4L, 5L, 6L)
  )

  expect_identical(
    arrange(data, c("text", desc("score")), locale = NULL),
    data[order(data$text, -xtfrm(data$score)), , drop = FALSE]
  )
  expect_identical(
    arrange(data, c(desc("text"), "score"), locale = NULL),
    data[order(-xtfrm(data$text), data$score), , drop = FALSE]
  )
})

test_that("locale selection preserves factor, numeric, and temporal ordering", {
  inputs <- list(
    factor(c("A", "a", "Z"), levels = c("Z", "a", "A")),
    ordered(c("A", "a", "Z"), levels = c("Z", "a", "A")),
    c(3L, 2L, 1L),
    as.Date("2020-01-01") + c(2, 1, 0),
    as.POSIXct("2020-01-01", tz = "UTC") + c(2, 1, 0)
  )

  for (value in inputs) {
    data <- data.frame(value = value, id = 1:3)
    for (locale in list("C", NULL)) {
      expect_identical(
        arrange(data, "value", locale = locale),
        data[3:1, , drop = FALSE]
      )
      expect_identical(arrange(data, desc("value"), locale = locale), data)
    }
  }
})

test_that("locale selection preserves empty and all-missing character inputs", {
  missing <- data.frame(value = rep(NA_character_, 3), id = 1:3)
  empty <- data.frame(value = character())

  for (locale in list("C", NULL)) {
    expect_identical(arrange(missing, "value", locale = locale), missing)
    expect_identical(arrange(missing, desc("value"), locale = locale), missing)
    expect_identical(arrange(empty, "value", locale = locale), empty)
    expect_identical(arrange(empty, desc("value"), locale = locale), empty)
    expect_identical(arrange(missing, character(), locale = locale), missing)
    expect_identical(
      arrange(data.frame(), character(), locale = locale),
      data.frame()
    )
  }
})

test_that("arrange rejects invalid locale types", {
  data <- data.frame(value = "a")

  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = 1)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = FALSE)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = list("C"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = factor("C"))
  })
})

test_that("arrange rejects invalid locale lengths and values", {
  data <- data.frame(value = "a")

  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = character())
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = c("C", "C"))
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = NA_character_)
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = "")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = "en")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = "en_US.UTF-8")
  })
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "value", locale = matrix("C"))
  })
})

test_that("arrange accepts a named locale for ascending sorts", {
  # Arrange does NOT use the names for anything.
  # This just confirms they don't cause any errors
  data <- data.frame(value = c("a", "Z", "A", "a", NA_character_), id = 1:5)

  expect_identical(
    arrange(data, "value", locale = c(chosen = "C")),
    data[c(3, 2, 1, 4, 5), , drop = FALSE]
  )
})

test_that("arrange accepts a named locale for descending sorts", {
  # Arrange does NOT use the names for anything.
  # This just confirms they don't cause any errors
  data <- data.frame(value = c("a", "Z", "A", "a", NA_character_), id = 1:5)

  expect_identical(
    arrange(data, desc("value"), locale = c(chosen = "C")),
    data[c(1, 4, 2, 3, 5), , drop = FALSE]
  )
})

test_that("arrange validates locale even without sorting columns", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data.frame(), character(), locale = "en")
  })
})

test_that("arrange leaves session collation unchanged on success and failure", {
  data <- data.frame(value = c("a", "A", "\u00e4", "Z"))
  locale_before <- Sys.getlocale("LC_COLLATE")
  order_before <- order(data$value)
  has_icu <- capabilities("ICU")
  icu_before <- if (has_icu) icuGetCollate() else NULL

  for (locale in list("C", NULL)) {
    arrange(data, "value", locale = locale)
    arrange(data, desc("value"), locale = locale)
    expect_identical(Sys.getlocale("LC_COLLATE"), locale_before)
    expect_identical(order(data$value), order_before)
    if (has_icu) {
      expect_identical(icuGetCollate(), icu_before)
    }
  }

  err <- tryCatch(arrange(data, "value", locale = "en"), error = identity)
  expect_s3_class(err, "error")
  expect_identical(Sys.getlocale("LC_COLLATE"), locale_before)
  expect_identical(order(data$value), order_before)
  if (has_icu) {
    expect_identical(icuGetCollate(), icu_before)
  }
})

test_that("arrange rejects duplicate data names even when sorting another column", {
  data <- data.frame(id = 1:2, x = 1:2, x = 2:1, check.names = FALSE)
  expect_snapshot(error = TRUE, cran = TRUE, {
    arrange(data, "id")
  })
})

test_that("arrange sorts raw bytes by their unsigned value", {
  data <- data.frame(id = 1:5, byte = as.raw(c(255, 0, 128, 127, 128)))
  expect_identical(arrange(data, "byte")$id, c(2L, 4L, 3L, 5L, 1L))
})

test_that("arrange sorts raw bytes in descending order", {
  data <- data.frame(id = 1:5, byte = as.raw(c(255, 0, 128, 127, 128)))
  expect_identical(arrange(data, desc("byte"))$id, c(1L, 3L, 5L, 4L, 2L))
})
