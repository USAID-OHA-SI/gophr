#' Check if all variables in list exist
#'
#' @param df data frame
#' @param vars quoted variable(s)
#' @keywords internal
var_exists <- function(df, vars){
  all(vars %in% names(df))
}

#' Check if all variables in list are missing
#'
#' @param df data frame
#' @param vars quoted variable(s)
#' @keywords internal
var_missing <- function(df, vars){
  !any(vars %in% names(df))
}

#' Get location
#' Identify where a user is working - locally or on workbench
#'
#' @keywords internal
get_location <- function(){
  ifelse(grepl("rstudio-server.*datim.org",
               as.list(Sys.info())$nodename),
         "pdap", "local")
}
