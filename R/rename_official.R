#' Apply the latest mechanism and partner names from DATIM
#'
#' Some mechanisms and partners are recorded in FACTSInfo with multiple
#' names over different time period. This function replaces all partner and
#' mechanism names the most recent name for each mechanism ID pulling from a
#' DATIM SQL View. With an DHIS2 update to DATIM in 2021, the DATIM mechanism
#' tables requires a password to access. We would recommend using
#' `glamr::set_datim()` to store your DATIM credentials securely on your local
#' machine. If you don't have them stored, you will be prompted each time to
#' enter your password to acces DATIM.
#'
#' @param df identify the MER Structured Data Set to clean
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_psnu_im <- rename_official(df_psnu_im) }

rename_official <- function(df) {

  #check that mechanism exists in MSD before starting (OUxIM or PSNUxIM, not PSNU)
  if(("mech_code" %in% names(df) == FALSE)) {
    usethis::ui_stop('This dataset does not have mechanisms. Make sure it is OUxIM or PSNUxIM')
  }

  #check internet connection
  if(curl::has_internet() == FALSE) {
    usethis::ui_warn("No internet connection. Cannot access offical names & rename.")
  } else {

    usethis::ui_info("Connecting to DATIM and accessing mechanism table. This will take over a minute to run. Please be patient.")

  #store column names (to work for both lower case and camel case) & then covert to lowercase
    headers_orig <- names(df)
    df <- dplyr::rename_all(df, tolower)

  #identify what OUs to map over
    if("operatingunit" %in% names(df)){
      ous <- unique(df$operatingunit)
    } else {
      ous <- lst_ous
    }

  #ask for credentials if not stored
    if(glamr::is_stored("datim")){
      datim_user <- glamr::datim_user()
      datim_pwd <- glamr::datim_pwd()
    } else {
      datim_user <- getPass::getPass("DATIM username")
      datim_pwd <- getPass::getPass("DATIM password", forcemask = TRUE)
    }

  #access current mechanism list
    mech_official <- purrr::map_dfr(ous,
                                    purrr::possibly(~extract_datim_names(.x, datim_user, datim_pwd),
                                                    NULL))

  #rename variables to match MSD and remove mechid from mech name
    mech_official <- mech_official %>%
      dplyr::select(mech_code = code,
                    primepartner_zXz = partner,
                    mech_name_zXz = mechanism) %>%
      dplyr::mutate(mech_name_zXz = stringr::str_remove(mech_name_zXz, "0000[0|1] |[:digit:]+ - "))

  #remove award information from mech_name
    mech_official <- mech_official %>%
      dplyr::mutate(mech_name_zXz = stringr::str_remove(mech_name_zXz,
            "^(720|AID|GH(AG|0)|U[:digit:]|NUGGH|UGH|U91|CK0|HT0|N[:digit:]|SGY||NU2|[:digit:]NU2|1U2).* - "))

  #merge official names into df
    df <- dplyr::left_join(df, mech_official, by="mech_code")

  #replace prime partner and mech names with official names and then remove
    if(!"mech_name" %in% names(df)){
      df <- dplyr::mutate(df, mech_name = as.character(NA))
      headers_orig <- c(headers_orig, "mech_name")
    }
    if(!"primepartner" %in% names(df)){
      df <- dplyr::mutate(df, primepartner = as.character(NA))
      headers_orig <- c(headers_orig, "primepartner")
    }

    df <- df %>%
      dplyr::mutate(mech_name = mech_name_zXz,
                    primepartner = primepartner_zXz) %>%
      dplyr::select(-dplyr::ends_with("_zXz"))

  #reapply original variable casing type
    names(df) <- headers_orig

  #reorder new variables
    df <- df %>%
      dplyr::relocate(c(mech_name, primepartner), .after = mech_code)

  }

  return(df)
}



#' Extract Data from DATIM
#'
#' @param url url for query
#'
#' @return converts json to data frame
#' @keywords internal
#'
extract_datim_names <- function(ou, username, password){

  sql_view_url <- paste0("https://www.datim.org/",
                         "api/sqlViews/fgUtV6e9YIX/data.json?",
                         "criteria=ou:", ou, "&",
                         "paging=false")

  r <- sql_view_url %>%
    utils::URLencode() %>%
    httr::GET(httr::authenticate(username, password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = TRUE)

  df <- tibble::as_tibble(x = r$listGrid$rows,
                           .name_repair = ~ r$listGrid$headers$column)

  return(df)
}

