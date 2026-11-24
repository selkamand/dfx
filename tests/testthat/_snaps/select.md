# select with non-existent columns throws an error

    Code
      select(df, c("a", "d"))
    Condition
      Error in `select()`:
      ! Could not find column in `data`: ["d"]

# select reports multiple non-existent columns

    Code
      select(df, c("d", "e"))
    Condition
      Error in `select()`:
      ! Could not find columns in `data`: ["d", "e"]

# select quotes non-existent column names unambiguously

    Code
      select(df, c("NA", "a, b", "a\nb"))
    Condition
      Error in `select()`:
      ! Could not find columns in `data`: ["NA", "a, b", "a\nb"]

# select with duplicate columns throws an error

    Code
      select(df, c("a", "a"))
    Condition
      Error in `select()`:
      ! `columns` must not contain duplicates

# select with non-character columns argument throws an error

    Code
      select(df, c(1, 2))
    Condition
      Error in `select()`:
      ! `columns` must be a character vector, not an object of class [numeric]

# select with list columns argument throws an error

    Code
      select(df, list("a", "b"))
    Condition
      Error in `select()`:
      ! `columns` must be a character vector, not an object of class [list]

# select with matrix columns argument throws an error

    Code
      select(df, matrix(c("a", "b"), nrow = 1L))
    Condition
      Error in `select()`:
      ! `columns` must be a character vector with no attributes other than names

# select explains why attributed character columns are invalid

    Code
      select(df, structure("a", note = "extra attribute"))
    Condition
      Error in `select()`:
      ! `columns` must be a character vector with no attributes other than names

# select with factor columns argument throws an error

    Code
      select(df, factor(c("a", "b")))
    Condition
      Error in `select()`:
      ! `columns` must be a character vector, not an object of class [factor]

# select rejects missing column names

    Code
      select(df, c("a", NA_character_))
    Condition
      Error in `select()`:
      ! `columns` must not contain missing values

# select rejects missing column names even when present in data

    Code
      select(df, NA_character_)
    Condition
      Error in `select()`:
      ! `columns` must not contain missing values

# select rejects empty column names even when present in data

    Code
      select(df, "")
    Condition
      Error in `select()`:
      ! `columns` must not contain empty strings

# select with data not a data.frame throws an error

    Code
      select(df, c("a", "b"))
    Condition
      Error in `select()`:
      ! `data` must be a data.frame, not an object of class [matrix, array]

# select throws an error when 'columns' has duplicate old names

    Code
      select(df, columns)
    Condition
      Error in `select()`:
      ! `columns` must not contain duplicates

# select with NULL columns throws an error

    Code
      select(df, columns)
    Condition
      Error in `select()`:
      ! `columns` must be a character vector, not an object of class [NULL]

# select handles 'columns' with names but empty values

    Code
      select(df, columns)
    Condition
      Error in `select()`:
      ! `columns` must not contain empty strings

# select throws an error if specified columns do not exist (with names)

    Code
      select(df, columns)
    Condition
      Error in `select()`:
      ! Could not find column in `data`: ["c"]

