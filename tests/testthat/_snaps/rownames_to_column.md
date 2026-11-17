# rownames_to_column rejects NULL data

    Code
      rownames_to_column(NULL, "id")
    Condition
      Error in `rownames_to_column()`:
      ! `data` must be a data.frame, not an object of class [NULL]

# rownames_to_column rejects vector data

    Code
      rownames_to_column(1:3, "id")
    Condition
      Error in `rownames_to_column()`:
      ! `data` must be a data.frame, not an object of class [integer]

# rownames_to_column rejects matrix data

    Code
      rownames_to_column(matrix(1:4, ncol = 2), "id")
    Condition
      Error in `rownames_to_column()`:
      ! `data` must be a data.frame, not an object of class [matrix, array]

# rownames_to_column rejects list data

    Code
      rownames_to_column(list(value = 1:2), "id")
    Condition
      Error in `rownames_to_column()`:
      ! `data` must be a data.frame, not an object of class [list]

# rownames_to_column rejects an integer column name

    Code
      rownames_to_column(data, 1L)
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a character string, not an object of class [integer]

# rownames_to_column rejects a logical column name

    Code
      rownames_to_column(data, TRUE)
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a character string, not an object of class [logical]

# rownames_to_column rejects a factor column name

    Code
      rownames_to_column(data, factor("id"))
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a character string, not an object of class [factor]

# rownames_to_column rejects a list column name

    Code
      rownames_to_column(data, list("id"))
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a character string, not an object of class [list]

# rownames_to_column rejects a NULL column name

    Code
      rownames_to_column(data, NULL)
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a character string, not an object of class [NULL]

# rownames_to_column requires a non-empty column name

    Code
      rownames_to_column(data, character(0))
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a single, non-missing column name

# rownames_to_column rejects multiple column names

    Code
      rownames_to_column(data, c("id", "value"))
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a single, non-missing column name

# rownames_to_column rejects a missing column name

    Code
      rownames_to_column(data, NA_character_)
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a single, non-missing column name

# rownames_to_column rejects an empty string column name

    Code
      rownames_to_column(data, "")
    Condition
      Error in `rownames_to_column()`:
      ! `column` must be a non-empty column name

# rownames_to_column rejects an existing column name

    Code
      rownames_to_column(data, "rowname")
    Condition
      Error in `rownames_to_column()`:
      ! Column [rowname] is already a column in `data`. Choose a different name for the row names column

