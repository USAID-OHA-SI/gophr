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
get_pdap_loc <- function(){
  ifelse(grepl("rstudio-server.*datim.org",
               as.list(Sys.info())$nodename),
         "pdap", "local")
}


#' Local PDAP Bucket
#'
#' Identify which bucket to source for a given file when on PEPFAR Posit
#' Workbench - S3_READ or S3_WRITE
#'
#' @inheritParams read_psd
#'
#' @keywords internal
#'
locate_bucket <- function(file){

  #location - local or pdap
  file_location <- get_pdap_loc()

  if (file_location == "local")
    return(NULL)

  if(file_location == "pdap" && !requireNamespace("aws.s3", quietly = TRUE))
    usethis::ui_stop("Package {usethis::ui_field('aws.s3')} is required for importing on PDAP. Restart session and install - {usethis::ui_code('install.packages(\\'aws.s3\\')')}")

  if (suppressMessages(aws.s3::object_exists(file,
                                             bucket = Sys.getenv("S3_READ")))) {
    usethis::ui_info("Located file in {usethis::ui_field('PDAP S3_READ')} bucket")
    return(Sys.getenv("S3_READ"))

  } else if (suppressMessages(aws.s3::object_exists(file,
                                                    bucket = Sys.getenv("S3_WRITE")))) {
    usethis::ui_info("Located file in {usethis::ui_field('PDAP S3_WRITE')} bucket")
    return(Sys.getenv("S3_WRITE"))

  } else {

    usethis::ui_stop("Unable to find the specified file in either {usethis::ui_field('PDAP S3_READ')} or {usethis::ui_field('PDAP S3_WRITE')} buckets")
  }

}
