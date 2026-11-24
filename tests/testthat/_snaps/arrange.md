# arrange shares data and column validation conventions

    Code
      arrange(NULL, "value")
    Condition
      Error in `arrange()`:
      ! `data` must be a data.frame, not an object of class [NULL]

---

    Code
      arrange(data, NULL)
    Condition
      Error in `arrange()`:
      ! `columns` must be a character vector, not an object of class [NULL]

---

    Code
      arrange(data, matrix("value"))
    Condition
      Error in `arrange()`:
      ! `columns` must be a character vector with no attributes other than names

---

    Code
      arrange(data, NA_character_)
    Condition
      Error in `arrange()`:
      ! `columns` must not contain missing values

---

    Code
      arrange(data, "")
    Condition
      Error in `arrange()`:
      ! `columns` must not contain empty strings

---

    Code
      arrange(data, c("value", desc("value")))
    Condition
      Error in `arrange()`:
      ! `columns` must not contain duplicates

---

    Code
      arrange(data, desc("missing"))
    Condition
      Error in `arrange()`:
      ! Could not find column in `data`: ["missing"]

# arrange rejects unexpected names and modified descending markers

    Code
      arrange(data, c(label = "value"))
    Condition
      Error in `arrange()`:
      ! `columns` must be unnamed or use `desc()` to flag descending order

---

    Code
      arrange(data, c(label = desc("value")))
    Condition
      Error in `arrange()`:
      ! `columns` must be unnamed or use `desc()` to flag descending order

---

    Code
      arrange(data, setNames("value", NA_character_))
    Condition
      Error in `arrange()`:
      ! `columns` must be unnamed or use `desc()` to flag descending order

---

    Code
      arrange(data, c(..DeSc = "value"))
    Condition
      Error in `arrange()`:
      ! `columns` must be unnamed or use `desc()` to flag descending order

# arrange rejects list, matrix, array, and nested data-frame keys

    Code
      arrange(data, "list")
    Condition
      Error in `arrange()`:
      ! Column ["list"] in `data` must be a vector, not an object of class [list]

---

    Code
      arrange(data, desc("matrix"))
    Condition
      Error in `arrange()`:
      ! Column ["matrix"] in `data` must be a vector, not an object of class [matrix, array]

---

    Code
      arrange(data, "array")
    Condition
      Error in `arrange()`:
      ! Column ["array"] in `data` must be a vector, not an object of class [array]

---

    Code
      arrange(data, desc("nested"))
    Condition
      Error in `arrange()`:
      ! Column ["nested"] in `data` must be a vector, not an object of class [data.frame]

