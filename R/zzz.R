.onAttach <- function(...) {
  if(requireNamespace("gagglr", quietly = TRUE))
    gagglr::oha_check("gophr", suppress_success = TRUE)
}
