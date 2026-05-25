pick_correct_na_to_match_type <- function(x){
  if(inherits(x, "Date")) as.Date(NA)
  else if(inherits(x, "POSIXct")) as.POSIXct(NA)
  else if(is.factor(x)) as.factor(NA)
  else if(is.complex(x)) NA_complex_
  else if(is.logical(x)) NA
  else if(is.character(x)) NA_character_
  else if(is.integer(x)) NA_integer_
  else if(is.numeric(x)) NA_real_
  else NA
}
