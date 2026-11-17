# between rejects incompatible typed missing temporal bounds like dplyr

    Code
      between(date, NA_integer_, date_right)
    Condition
      Error in `check_bound()`:
      ! `left` must be Date or missing when `x` is Date.

---

    Code
      between(date, date_left, NA_integer_)
    Condition
      Error in `check_bound()`:
      ! `right` must be Date or missing when `x` is Date.

---

    Code
      between(date, NA_real_, date_right)
    Condition
      Error in `check_bound()`:
      ! `left` must be Date or missing when `x` is Date.

---

    Code
      between(date, date_left, NA_real_)
    Condition
      Error in `check_bound()`:
      ! `right` must be Date or missing when `x` is Date.

---

    Code
      between(date, NA_complex_, date_right)
    Condition
      Error in `check_bound()`:
      ! `left` must be Date or missing when `x` is Date.

---

    Code
      between(date, date_left, NA_complex_)
    Condition
      Error in `check_bound()`:
      ! `right` must be Date or missing when `x` is Date.

---

    Code
      between(date, NA_character_, date_right)
    Condition
      Error in `check_bound()`:
      ! `left` must be Date or missing when `x` is Date.

---

    Code
      between(date, date_left, NA_character_)
    Condition
      Error in `check_bound()`:
      ! `right` must be Date or missing when `x` is Date.

---

    Code
      between(time, NA_integer_, time_right)
    Condition
      Error in `check_bound()`:
      ! `left` must be POSIXt or missing when `x` is POSIXt.

---

    Code
      between(time, time_left, NA_integer_)
    Condition
      Error in `check_bound()`:
      ! `right` must be POSIXt or missing when `x` is POSIXt.

---

    Code
      between(time, NA_real_, time_right)
    Condition
      Error in `check_bound()`:
      ! `left` must be POSIXt or missing when `x` is POSIXt.

---

    Code
      between(time, time_left, NA_real_)
    Condition
      Error in `check_bound()`:
      ! `right` must be POSIXt or missing when `x` is POSIXt.

---

    Code
      between(time, NA_complex_, time_right)
    Condition
      Error in `check_bound()`:
      ! `left` must be POSIXt or missing when `x` is POSIXt.

---

    Code
      between(time, time_left, NA_complex_)
    Condition
      Error in `check_bound()`:
      ! `right` must be POSIXt or missing when `x` is POSIXt.

---

    Code
      between(time, NA_character_, time_right)
    Condition
      Error in `check_bound()`:
      ! `left` must be POSIXt or missing when `x` is POSIXt.

---

    Code
      between(time, time_left, NA_character_)
    Condition
      Error in `check_bound()`:
      ! `right` must be POSIXt or missing when `x` is POSIXt.

# between rejects non-temporal x with Date or datetime bounds

    Code
      between(1:2, as.Date("2024-01-01"), as.Date("2024-01-02"))
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [integer].

---

    Code
      between(NA_real_, as.POSIXct("2024-01-01", tz = "UTC"), as.POSIXct("2024-01-02",
        tz = "UTC"))
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [numeric].

# between rejects all-missing non-temporal x with temporal bounds

    Code
      between(NA, as.Date("2024-01-01"), as.Date("2024-01-02"))
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [logical].

---

    Code
      between(c(NA, NA), left, right)
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [logical].

---

    Code
      between(x, as.Date("2024-01-01"), as.Date("2024-01-02"))
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [logical].

---

    Code
      between(c(NA, NA), date_left, "bad")
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [logical].

---

    Code
      between(c(NA, NA), date_left, rep(as.Date("2024-01-02"), 3))
    Condition
      Error in `between()`:
      ! `x` must be a Date or datetime vector when a bound is a Date or datetime, not an object of class [logical].

