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

# arrange rejects invalid locale types

    Code
      arrange(data, "value", locale = 1)
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or a character string, not an object of class [numeric]

---

    Code
      arrange(data, "value", locale = FALSE)
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or a character string, not an object of class [logical]

---

    Code
      arrange(data, "value", locale = list("C"))
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or a character string, not an object of class [list]

---

    Code
      arrange(data, "value", locale = factor("C"))
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or a character string, not an object of class [factor]

# arrange rejects invalid locale lengths and values

    Code
      arrange(data, "value", locale = character())
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or "C"

---

    Code
      arrange(data, "value", locale = c("C", "C"))
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or "C"

---

    Code
      arrange(data, "value", locale = NA_character_)
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or "C"

---

    Code
      arrange(data, "value", locale = "")
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or "C"

---

    Code
      arrange(data, "value", locale = "en")
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or "C"

---

    Code
      arrange(data, "value", locale = "en_US.UTF-8")
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or "C"

---

    Code
      arrange(data, "value", locale = matrix("C"))
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or "C"

# arrange validates locale even without sorting columns

    Code
      arrange(data.frame(), character(), locale = "en")
    Condition
      Error in `arrange()`:
      ! `locale` must be NULL or "C"

# arrange rejects duplicate data names even when sorting another column

    Code
      arrange(data, "id")
    Condition
      Error in `arrange()`:
      ! `data` must not have duplicate column names

