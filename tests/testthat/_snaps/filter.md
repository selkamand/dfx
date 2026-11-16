# filter rejects NULL data before calling the predicate

    Code
      filter(NULL, function(d) stop("predicate was called"))
    Condition
      Error in `filter()`:
      ! `data` must be a data.frame, not an object of class [NULL]

# filter rejects vector data before calling the predicate

    Code
      filter(1:4, function(d) stop("predicate was called"))
    Condition
      Error in `filter()`:
      ! `data` must be a data.frame, not an object of class [integer]

# filter rejects matrix data before calling the predicate

    Code
      filter(matrix(1:4, ncol = 1), function(d) stop("predicate was called"))
    Condition
      Error in `filter()`:
      ! `data` must be a data.frame, not an object of class [matrix, array]

# filter rejects list data before calling the predicate

    Code
      filter(list(id = 1:4), function(d) stop("predicate was called"))
    Condition
      Error in `filter()`:
      ! `data` must be a data.frame, not an object of class [list]

# filter rejects NULL as a predicate

    Code
      filter(make_filter_data(), NULL)
    Condition
      Error in `filter()`:
      ! `fun` must be a function. Not an object of class [NULL]

# filter rejects an integer as a predicate

    Code
      filter(make_filter_data(), 1L)
    Condition
      Error in `filter()`:
      ! `fun` must be a function. Not an object of class [integer]

# filter rejects a string as a predicate

    Code
      filter(make_filter_data(), "predicate")
    Condition
      Error in `filter()`:
      ! `fun` must be a function. Not an object of class [character]

# filter rejects a list as a predicate

    Code
      filter(make_filter_data(), list(TRUE))
    Condition
      Error in `filter()`:
      ! `fun` must be a function. Not an object of class [list]

# filter rejects a predicate with no parameters

    Code
      filter(make_filter_data(), function() TRUE)
    Condition
      Error in `filter()`:
      ! `fun` must have exactly one parameter (expecting a data.frame). Found 0

# filter rejects a predicate with two required parameters

    Code
      filter(make_filter_data(), function(d, other) TRUE)
    Condition
      Error in `filter()`:
      ! `fun` must have exactly one parameter (expecting a data.frame). Found 2

# filter rejects a predicate with an optional second parameter

    Code
      filter(make_filter_data(), function(d, other = NULL) TRUE)
    Condition
      Error in `filter()`:
      ! `fun` must have exactly one parameter (expecting a data.frame). Found 2

# filter rejects a predicate with dots

    Code
      filter(make_filter_data(), function(d, ...) TRUE)
    Condition
      Error in `filter()`:
      ! `fun` supports variable arguments (has a `...` parameter). This is not permitted by the `filter` function. All parameters must be explicit and named.

# filter rejects a NULL predicate result

    Code
      filter(make_filter_data(), function(d) NULL)
    Condition
      Error in `filter()`:
      ! `fun` must return a logical vector, not an object of class [NULL]

# filter rejects an integer predicate result

    Code
      filter(make_filter_data(), function(d) 1L)
    Condition
      Error in `filter()`:
      ! `fun` must return a logical vector, not an object of class [integer]

# filter rejects a character predicate result

    Code
      filter(make_filter_data(), function(d) c("TRUE", "FALSE", "TRUE", "FALSE"))
    Condition
      Error in `filter()`:
      ! `fun` must return a logical vector, not an object of class [character]

# filter rejects a list predicate result

    Code
      filter(make_filter_data(), function(d) list(TRUE, FALSE, TRUE, FALSE))
    Condition
      Error in `filter()`:
      ! `fun` must return a logical vector, not an object of class [list]

# filter rejects a matrix predicate result

    Code
      filter(make_filter_data(), function(d) {
        matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 1L)
      })
    Condition
      Error in `filter()`:
      ! `fun` must return a logical vector, not an object of class [matrix, array]

# filter rejects a zero-length predicate result

    Code
      filter(make_filter_data(), function(d) logical())
    Condition
      Error in `filter()`:
      ! `fun` must return a vector with one value for each row in `data`. Length of returned vector: [0] != Rows in `data` [4]

# filter rejects a length-one predicate result

    Code
      filter(make_filter_data(), function(d) TRUE)
    Condition
      Error in `filter()`:
      ! `fun` must return a vector with one value for each row in `data`. Length of returned vector: [1] != Rows in `data` [4]

# filter rejects a short predicate result

    Code
      filter(make_filter_data(), function(d) rep(TRUE, 3))
    Condition
      Error in `filter()`:
      ! `fun` must return a vector with one value for each row in `data`. Length of returned vector: [3] != Rows in `data` [4]

# filter rejects a long predicate result

    Code
      filter(make_filter_data(), function(d) rep(TRUE, 5))
    Condition
      Error in `filter()`:
      ! `fun` must return a vector with one value for each row in `data`. Length of returned vector: [5] != Rows in `data` [4]

# filter reports one missing predicate value

    Code
      filter(make_filter_data(), function(d) c(TRUE, NA, FALSE, FALSE))
    Condition
      Error in `filter()`:
      ! `fun` returned 1 missing values. All values returned by `fun` must be TRUE/FALSE

# filter reports two missing predicate values

    Code
      filter(make_filter_data(), function(d) c(NA, TRUE, FALSE, NA))
    Condition
      Error in `filter()`:
      ! `fun` returned 2 missing values. All values returned by `fun` must be TRUE/FALSE

# filter propagates errors raised by the predicate

    Code
      filter(make_filter_data(), function(d) stop("predicate failed"))
    Condition
      Error in `fun()`:
      ! predicate failed

# filter rejects empty grouping columns before calling the predicate

    Code
      filter(make_grouped_filter_data(), function(d) stop("predicate was called"),
      by = character())
    Condition
      Error in `filter()`:
      ! `by` must be NULL or a non-empty vector. Found a zero-length vector.

# filter rejects integer grouping columns before calling the predicate

    Code
      filter(make_grouped_filter_data(), function(d) stop("predicate was called"),
      by = 1L)
    Condition
      Error in `filter()`:
      ! `by` must be a character vector. Not an object of class [integer]

# filter rejects logical grouping columns before calling the predicate

    Code
      filter(make_grouped_filter_data(), function(d) stop("predicate was called"),
      by = TRUE)
    Condition
      Error in `filter()`:
      ! `by` must be a character vector. Not an object of class [logical]

# filter rejects factor grouping columns before calling the predicate

    Code
      filter(make_grouped_filter_data(), function(d) stop("predicate was called"),
      by = factor("group"))
    Condition
      Error in `filter()`:
      ! `by` must be a character vector. Not an object of class [factor]

# filter rejects list grouping columns before calling the predicate

    Code
      filter(make_grouped_filter_data(), function(d) stop("predicate was called"),
      by = list("group"))
    Condition
      Error in `filter()`:
      ! `by` must be a character vector. Not an object of class [list]

# filter rejects unknown grouping columns before calling the predicate

    Code
      filter(make_grouped_filter_data(), function(d) stop("predicate was called"),
      by = "missing")
    Condition
      Error in `filter()`:
      ! `by` must be valid column names. Could not find column/s: missing in `data`

# grouped filter reports an error from the first group

    Code
      filter(make_grouped_filter_data(), function(d) stop("first group failed"), by = "group")
    Condition
      Error in `filter()`:
      ! `fun` threw an error when applied to the data in Group (`group` = "b"): first group failed

# grouped filter reports an error from a later group

    Code
      filter(make_grouped_filter_data(), function(d) {
        if (d$group[[1L]] == "a") {
          stop("later group failed")
        }
        TRUE
      }, by = "group")
    Condition
      Error in `filter()`:
      ! `fun` threw an error when applied to the data in Group (`group` = "a"): later group failed

# grouped filter reports an error with multiple grouping columns

    Code
      filter(data, function(d) {
        if (d$group[[1L]] == "a" && d$segment[[1L]] == "y") {
          stop("group combination failed")
        }
        TRUE
      }, by = c("group", "segment"))
    Condition
      Error in `filter()`:
      ! `fun` threw an error when applied to the data in Group (`group` = "a", `segment` = "y"): group combination failed

# grouped filter rejects an integer result

    Code
      filter(make_grouped_filter_data(), function(d) 1L, by = "group")
    Condition
      Error in `filter()`:
      ! `fun` must return exactly one logical value per group but instead returned 1 integer value for Group (`group` = "b")

# grouped filter rejects a zero-length result

    Code
      filter(make_grouped_filter_data(), function(d) logical(), by = "group")
    Condition
      Error in `filter()`:
      ! `fun` must return exactly one logical value per group but instead returned 0 logical values for Group (`group` = "b")

# grouped filter identifies the group with two results

    Code
      filter(make_grouped_filter_data(), function(d) if (d$group[[1L]] == "a") c(TRUE,
        FALSE) else TRUE, by = "group")
    Condition
      Error in `filter()`:
      ! `fun` must return exactly one logical value per group but instead returned 2 logical values for Group (`group` = "a")

# grouped filter rejects missing predicate values

    Code
      filter(make_grouped_filter_data(), function(d) if (d$group[[1L]] ==
      "a") NA else TRUE, by = "group")
    Condition
      Error in `filter()`:
      ! `fun` returned 2 missing values. All values returned by `fun` must be TRUE/FALSE

# grouped filter errors identify multiple grouping values

    Code
      filter(data, function(d) if (d$segment[[1L]] == "y") c(TRUE, FALSE) else TRUE,
      by = c("group", "segment"))
    Condition
      Error in `filter()`:
      ! `fun` must return exactly one logical value per group but instead returned 2 logical values for Group (`group` = "a", `segment` = "y")

