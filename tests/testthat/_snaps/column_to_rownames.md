# column_to_rownames rejects duplicated row names

    Code
      column_to_rownames(data, "id")
    Condition
      Error in `column_to_rownames()`:
      ! Column [id] must contain unique values to use as row names. Duplicated values: [a, b]

# column_to_rownames rejects missing row names and reports their count

    Code
      column_to_rownames(data, "id")
    Condition
      Error in `column_to_rownames()`:
      ! Column [id] contains 2 missing values. Row names cannot be missing

# column_to_rownames requires an existing column

    Code
      column_to_rownames(data, "missing")
    Condition
      Error in `column_to_rownames()`:
      ! Column [missing] is not a column in `data`. Valid column names include: [id, value]

# column_to_rownames requires a non-empty column name

    Code
      column_to_rownames(data, character(0))
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a single, non-missing column name

# column_to_rownames rejects an empty string column name

    Code
      column_to_rownames(data, "")
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a non-empty column name

# column_to_rownames rejects multiple column names

    Code
      column_to_rownames(data, c("id", "value"))
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a single, non-missing column name

# column_to_rownames rejects a missing column name

    Code
      column_to_rownames(data, NA_character_)
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a single, non-missing column name

# column_to_rownames rejects an integer column name

    Code
      column_to_rownames(data, 1L)
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a character string, not an object of class [integer]

# column_to_rownames rejects a logical column name

    Code
      column_to_rownames(data, TRUE)
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a character string, not an object of class [logical]

# column_to_rownames rejects a factor column name

    Code
      column_to_rownames(data, factor("id"))
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a character string, not an object of class [factor]

# column_to_rownames rejects a list column name

    Code
      column_to_rownames(data, list("id"))
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a character string, not an object of class [list]

# column_to_rownames rejects a NULL column name

    Code
      column_to_rownames(data, NULL)
    Condition
      Error in `column_to_rownames()`:
      ! `column` must be a character string, not an object of class [NULL]

# column_to_rownames rejects NULL data

    Code
      column_to_rownames(NULL, "id")
    Condition
      Error in `column_to_rownames()`:
      ! `data` must be a data.frame, not an object of class [NULL]

# column_to_rownames rejects vector data

    Code
      column_to_rownames(1:3, "id")
    Condition
      Error in `column_to_rownames()`:
      ! `data` must be a data.frame, not an object of class [integer]

# column_to_rownames rejects matrix data

    Code
      column_to_rownames(matrix(1:4, ncol = 2), "id")
    Condition
      Error in `column_to_rownames()`:
      ! `data` must be a data.frame, not an object of class [matrix, array]

# column_to_rownames rejects list data

    Code
      column_to_rownames(list(id = c("a", "b")), "id")
    Condition
      Error in `column_to_rownames()`:
      ! `data` must be a data.frame, not an object of class [list]

# column_to_rownames requires data without existing row names

    Code
      column_to_rownames(data, "id")
    Condition
      Error in `column_to_rownames()`:
      ! `data` already has row names. Remove them before calling `column_to_rownames()`

# column_to_rownames errors on data frames with no columns

    Code
      column_to_rownames(data, "id")
    Condition
      Error in `column_to_rownames()`:
      ! Column [id] is not a column in `data`. Valid column names include: []

