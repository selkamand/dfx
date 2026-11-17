# between rejects incompatible typed missing numeric bounds like dplyr

    Code
      between(1, NA_complex_, 2)
    Condition
      Error in `between()`:
      ! `left` must be a numeric vector, not an object of class [complex]

---

    Code
      between(1, 0, NA_complex_)
    Condition
      Error in `between()`:
      ! `right` must be a numeric vector, not an object of class [complex]

---

    Code
      between(1, NA_character_, 2)
    Condition
      Error in `between()`:
      ! `left` must be a numeric vector, not an object of class [character]

---

    Code
      between(1, 0, NA_character_)
    Condition
      Error in `between()`:
      ! `right` must be a numeric vector, not an object of class [character]

---

    Code
      between(1, as.Date(NA), 2)
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [numeric].

---

    Code
      between(1, 0, as.Date(NA))
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [numeric].

---

    Code
      between(1, as.POSIXct(NA, tz = "UTC"), 2)
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [numeric].

---

    Code
      between(1, 0, as.POSIXct(NA, tz = "UTC"))
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [numeric].

---

    Code
      between(1, as.POSIXlt(NA, tz = "UTC"), 2)
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [numeric].

---

    Code
      between(1, 0, as.POSIXlt(NA, tz = "UTC"))
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [numeric].

# between requires vector inputs

    Code
      between(matrix(1:4, nrow = 2), 2, 3)
    Condition
      Error in `between()`:
      ! `x` must be a vector, not a matrix or array.

---

    Code
      between(array(1:4, dim = c(2, 2, 1)), 2, 3)
    Condition
      Error in `between()`:
      ! `x` must be a vector, not a matrix or array.

---

    Code
      between(1:4, matrix(2), 4)
    Condition
      Error in `between()`:
      ! `left` must be a vector, not a matrix or array.

---

    Code
      between(1:4, 2, matrix(4))
    Condition
      Error in `between()`:
      ! `right` must be a vector, not a matrix or array.

---

    Code
      between(as.Date("2024-01-02"), structure(as.Date("2024-01-01"), dim = c(1L, 1L)),
      as.Date("2024-01-03"))
    Condition
      Error in `between()`:
      ! `left` must be a vector, not a matrix or array.

---

    Code
      between(NA, as.Date("2024-01-01"), matrix(2))
    Condition
      Error in `between()`:
      ! `right` must be a vector, not a matrix or array.

