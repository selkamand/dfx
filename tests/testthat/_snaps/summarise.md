# summarise rejects invalid data before calling fun

    Code
      summarise(NULL, fun)
    Condition
      Error in `summarise()`:
      ! `data` must be a data.frame, not an object of class [NULL]

---

    Code
      summarise(matrix(1:4, ncol = 2L), fun)
    Condition
      Error in `summarise()`:
      ! `data` must be a data.frame, not an object of class [matrix, array]

---

    Code
      summarise(list(value = 1:4), fun)
    Condition
      Error in `summarise()`:
      ! `data` must be a data.frame, not an object of class [list]

# summarise requires a function with one explicit parameter

    Code
      summarise(data, NULL)
    Condition
      Error in `summarise()`:
      ! `fun` must be a function. Not an object of class [NULL]

---

    Code
      summarise(data, "mean")
    Condition
      Error in `summarise()`:
      ! `fun` must be a function. Not an object of class [character]

---

    Code
      summarise(data, function() list(total = 3L))
    Condition
      Error in `summarise()`:
      ! `fun` must have exactly one parameter (expecting a data.frame). Found 0

---

    Code
      summarise(data, function(d, other = NULL) list(total = sum(d$value)))
    Condition
      Error in `summarise()`:
      ! `fun` must have exactly one parameter (expecting a data.frame). Found 2

---

    Code
      summarise(data, function(d, ...) list(total = sum(d$value)))
    Condition
      Error in `summarise()`:
      ! `fun` supports variable arguments (has a `...` parameter). This is not permitted by the `summarise` function. All parameters must be explicit and named.

# summarise validates grouping columns before calling fun

    Code
      summarise(data, fun, by = 1L)
    Condition
      Error in `summarise()`:
      ! `by` must be a character vector. Not an object of class [integer]

---

    Code
      summarise(data, fun, by = character())
    Condition
      Error in `summarise()`:
      ! `by` must be NULL or a non-empty vector. Found a zero-length vector.

---

    Code
      summarise(data, fun, by = c("key", "unknown", NA_character_))
    Condition
      Error in `summarise()`:
      ! `by` must be valid column names. Could not find column/s: unknown, NA in `data`

---

    Code
      summarise(data, fun, by = c("key", "matrix_key"))
    Condition
      Error in `summarise()`:
      ! `by` cannot include matrix-valued columns: `matrix_key`

# summarise preserves the message of an ungrouped callback error

    Code
      summarise(data, function(d) stop("cannot compute total"))
    Condition
      Error in `summarise()`:
      ! `fun` threw an error when applied to the data: cannot compute total

# summarise stops processing groups after a callback error

    Code
      summarise(data, fun, by = "key")
    Condition
      Error in `summarise()`:
      ! `fun` threw an error when applied to the data in group 2 [key=a]: cannot compute group total

# summarise reports all three grouping values when fun errors

    Code
      summarise(data, fun, by = c("site", "year", "active"))
    Condition
      Error in `summarise()`:
      ! `fun` threw an error when applied to the data in group 2 [site=east & year=2025 & active=TRUE]: cannot compute group total

# summarise requires fun to return a list

    Code
      summarise(data, function(d) NULL)
    Condition
      Error in `summarise()`:
      ! `fun` must return a list, not NULL

---

    Code
      summarise(data, function(d) c(total = sum(d$value)))
    Condition
      Error in `summarise()`:
      ! `fun` must return a list

# summarise rejects unnamed lists and NA metric names

    Code
      summarise(data, function(d) list(nrow(d)))
    Condition
      Error in `summarise()`:
      ! `fun` must return a named list. No names found.

---

    Code
      summarise(data, function(d) {
        setNames(list(1L, 2L), c("total", NA_character_))
      })
    Condition
      Error in `summarise()`:
      ! `fun` must return a named list. 1/2 elements are unnamed.

# summarise rejects metrics that have zero or multiple values

    Code
      summarise(data, function(d) list(total = integer()))
    Condition
      Error in `summarise()`:
      ! `fun` returned an unexpected number of values when calculating [total]. Expected 1 but found 0.

---

    Code
      summarise(data, function(d) list(rows = nrow(d), bounds = range(d$value)))
    Condition
      Error in `summarise()`:
      ! `fun` returned an unexpected number of values when calculating [bounds]. Expected 1 but found 2.

# summarise validates results from later groups

    Code
      summarise(data, function(d) list(total = d$value), by = "key")
    Condition
      Error in `summarise()`:
      ! `fun` returned an unexpected number of values when calculating [total]. Expected 1 but found 2.

# summarise rejects inconsistent metric sets across groups

    Code
      summarise(data, metrics, by = "key")
    Condition
      Error in `match.names()`:
      ! names do not match previous names

# summarise rejects empty metric names

    Code
      summarise(data, function(d) list(total = sum(d$value), nrow(d)))
    Condition
      Error in `summarise()`:
      ! `fun` must return a named list. 1/2 elements are unnamed.

# summarise rejects non-atomic metrics in both modes

    Code
      summarise(data, function(d) list(total = list(sum(d$value))))
    Condition
      Error in `summarise()`:
      ! `fun` returned an object of class: [list] when calculating [total]. Expected an atomic vector.

---

    Code
      summarise(data, function(d) list(total = data.frame(value = 1:2)), by = "key")
    Condition
      Error in `summarise()`:
      ! `fun` returned an object of class: [list] when calculating [total]. Expected an atomic vector.

# summarise rejects a character matrix as by

    Code
      summarise(data, function(d) list(rows = nrow(d)), by = matrix("key"))
    Condition
      Error in `summarise()`:
      ! `by` must be a character vector. Not an object of class [matrix, array]

