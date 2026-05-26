# `lag()` gives informative error for <ts> objects

    Code
      lag(ts(1:10))
    Condition
      Error in `lag()`:
      ! `x` must be a vector, not a <ts>, do you want `stats::lag()`?

# `lead()` / `lag()` validate `n`

    Code
      lead(1:5, n = 1:2)
    Condition
      Error in `lead()`:
      ! `n` must be a single number (supplied value had length of 2)
    Code
      lead(1:5, -1)
    Condition
      Error in `lead()`:
      ! `n` must positive

---

    Code
      lag(1:5, n = 1:2)
    Condition
      Error in `lag()`:
      ! `n` must be a single number (supplied value had length of 2)
    Code
      lag(1:5, -1)
    Condition
      Error in `lag()`:
      ! `n` must positive

# `lead()` / `lag()` reject non-numeric `n`

    Code
      lead(1:5, n = "1")
    Condition
      Error in `lead()`:
      ! `n` must be an integer, not an object of class [character]

---

    Code
      lag(1:5, n = "1")
    Condition
      Error in `lag()`:
      ! `n` must be an integer, not an object of class [character]

# `lead()` / `lag()` check for empty dots

    Code
      lead(1:5, deault = 1)
    Condition
      Error in `lead()`:
      ! unused argument (deault = 1)

---

    Code
      lag(1:5, deault = 1)
    Condition
      Error in `lag()`:
      ! unused argument (deault = 1)

# `lead()` / `lag()` require that `x` is a vector

    Code
      lead(environment())
    Condition
      Error in `lead()`:
      ! `x` must be a vector, not an object of class: [environment]

---

    Code
      lag(environment())
    Condition
      Error in `lag()`:
      ! `x` must be a vector, not an object of class: [environment]

# throws error when data.frames are supplied

    Code
      lead(df)
    Condition
      Error in `lead()`:
      ! `x` must be a vector, not an object of class: [data.frame]

---

    Code
      lag(df)
    Condition
      Error in `lag()`:
      ! `x` must be a vector, not an object of class: [data.frame]

# throws error when matrices are supplied

    Code
      lead(mx)
    Condition
      Error in `lead()`:
      ! `x` must be a vector, not an object of class: [matrix, array]

---

    Code
      lag(mx)
    Condition
      Error in `lag()`:
      ! `x` must be a vector, not an object of class: [matrix, array]

# `default` is cast to the type of `x`

    Code
      lag(1L, default = 1.5)
    Condition
      Error in `lag()`:
      ! `default` must be the same type as x. Expected [integer] but recieved [double]

---

    Code
      lead(1L, default = 1.5)
    Condition
      Error in `lead()`:
      ! `default` must be the same type as x. Expected [integer] but recieved [double]

# `default` must be size 1 (#5641)

    Code
      shift(1:5, default = 1:2)
    Condition
      Error in `shift()`:
      ! could not find function "shift"

---

    Code
      shift(1:5, default = integer())
    Condition
      Error in `shift()`:
      ! could not find function "shift"

# `n` is validated

    Code
      shift(1, n = 1:2)
    Condition
      Error in `shift()`:
      ! could not find function "shift"

