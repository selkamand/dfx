## Turn a singular form of a word into a pluralised if n != 1
pluralise <- function(singular, n) {
  if (n != 1) {
    return(paste(singular, "s", sep = ""))
  }

  return(singular)
}

quote_and_collapse <- function(vec, q = '"', collapse = ",") {
  paste0(paste0(q, vec, q), collapse = collapse)
}
