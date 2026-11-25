# desc rejects inputs that are not character strings

    Code
      desc(NULL)
    Condition
      Error in `desc()`:
      ! `column` must be a character string, not an object of class [NULL]

---

    Code
      desc(1)
    Condition
      Error in `desc()`:
      ! `column` must be a character string, not an object of class [numeric]

---

    Code
      desc(list("value"))
    Condition
      Error in `desc()`:
      ! `column` must be a character string, not an object of class [list]

---

    Code
      desc(factor("value"))
    Condition
      Error in `desc()`:
      ! `column` must be a character string, not an object of class [factor]

# desc rejects attributes other than names

    Code
      desc(matrix("value"))
    Condition
      Error in `desc()`:
      ! `column` must be a character string with no attributes other than names

---

    Code
      desc(structure("value", note = "extra attribute"))
    Condition
      Error in `desc()`:
      ! `column` must be a character string with no attributes other than names

---

    Code
      desc(structure("value", class = "custom"))
    Condition
      Error in `desc()`:
      ! `column` must be a character string with no attributes other than names

# desc requires one non-missing, non-empty column name

    Code
      desc(character())
    Condition
      Error in `desc()`:
      ! `column` must be a single, non-missing column name

---

    Code
      desc(c("value", "other"))
    Condition
      Error in `desc()`:
      ! `column` must be a single, non-missing column name

---

    Code
      desc(NA_character_)
    Condition
      Error in `desc()`:
      ! `column` must be a single, non-missing column name

---

    Code
      desc("")
    Condition
      Error in `desc()`:
      ! `column` must be a non-empty column name

