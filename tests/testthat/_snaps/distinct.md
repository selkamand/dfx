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

