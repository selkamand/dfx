# distinct rejects inputs that are not data frames

    Code
      distinct(1:3)
    Condition
      Error in `distinct()`:
      ! `data` must be a data.frame, not an object of class [integer]

# distinct rejects an unknown column

    Code
      distinct(data, columns = "unknown")
    Condition
      Error in `distinct()`:
      ! Could not find column in `data`: ["unknown"]

# distinct rejects an empty column selection

    Code
      distinct(data, columns = character(0))
    Condition
      Error in `distinct()`:
      ! `columns` must contain at least one column name. Use `columns = NULL` to use all columns.

# distinct rejects duplicate input column names

    Code
      distinct(data)
    Condition
      Error in `distinct()`:
      ! `data` must not have duplicate column names

# distinct rejects duplicate entries in columns

    Code
      distinct(data, columns = c("id", "id"))
    Condition
      Error in `distinct()`:
      ! `columns` must not contain duplicates

