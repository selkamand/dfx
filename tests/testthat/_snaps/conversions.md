# convert_vector_to_match_target: snapshot errors thrown when conversion function throws a warning

    Code
      convert_vector_to_match_target("not-a-number", numeric(), failure = "error")
    Condition
      Error:
      ! conversion failure: can not convert [character] to [numeric]

---

    Code
      convert_vector_to_match_target("not-an-integer", integer(), failure = "error")
    Condition
      Error:
      ! conversion failure: can not convert [character] to [integer]

# convert_vector_to_match_target: snapshot errors thrown when target class in unsupported

    Code
      convert_vector_to_match_target(c(1, 2, 3), target, failure = "error")
    Condition
      Error:
      ! conversion failure: can not convert [numeric] to [unsupported_target_class]

# convert_vector_to_match_target: snapshots invalid failure argument error message

    Code
      convert_vector_to_match_target(c(1, 2, 3), character(), failure = "other")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "error", "keep_original"

# convert_vector_to_match_target snapshots custom error_prefix

    Code
      convert_vector_to_match_target("not-a-number", numeric(), failure = "error",
      error_prefix = "mutate column `score`: ")
    Condition
      Error:
      ! mutate column `score`: can not convert [character] to [numeric]

