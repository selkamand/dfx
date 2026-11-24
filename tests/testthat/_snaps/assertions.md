# assert_dataframe reports the original class and supplied call

    Code
      assert_dataframe(NULL, call = call)
    Condition
      Error in `arrange()`:
      ! `data` must be a data.frame, not an object of class [NULL]

---

    Code
      assert_dataframe(matrix(1:4, ncol = 2L), call = call)
    Condition
      Error in `arrange()`:
      ! `data` must be a data.frame, not an object of class [matrix, array]

---

    Code
      assert_dataframe(structure(list(), class = c("custom", "list")), call = call)
    Condition
      Error in `arrange()`:
      ! `data` must be a data.frame, not an object of class [custom, list]

# assert_dataframe suppresses the error call when it is NULL

    Code
      assert_dataframe(NULL, call = NULL)
    Condition
      Error:
      ! `data` must be a data.frame, not an object of class [NULL]

