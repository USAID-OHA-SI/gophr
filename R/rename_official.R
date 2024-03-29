#' Apply the latest mechanism and partner names from DATIM
#'
#' Some mechanisms and partners are recorded in FACTSInfo with multiple
#' names over different time period. This function replaces all partner and
#' mechanism names the most recent name for each mechanism ID pulling from a
#' DATIM SQL View.  The `mech_code` variable is required in your dataset, and
#' having `operatingunit` and `fiscal_year` or `period` are highly recommended
#' as they will limit the size of the DATIM queries.With an DHIS2 update to
#' DATIM in 2021, the DATIM mechanism tables requires a password to access. We
#' would recommend using `glamr::set_datim()` to store your DATIM credentials
#' securely on your local machine. If you don't have them stored, you will be
#' prompted each time to enter your password to acces DATIM.
#'
#' @param df identify the PEPFAR Structured Data Set to clean
#' @param datim_user DATIM username; if missing will look for stored credentials
#' first and then prompt for them if not found
#' @param datim_pwd DATIM password; if missing will look for stored credentials
#' first and then prompt for them if not found
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_psnu_im <- read_psd(file) %>% filter(country == "Saturn")
#' df_psnu_im <- rename_official(df_psnu_im) }

rename_official <- function(df, datim_user, datim_pwd) {

  #check that mechanism exists in MSD before starting (OUxIM or PSNUxIM, not PSNU)
  if(("mech_code" %in% names(df) == FALSE)) {
    usethis::ui_stop('This dataset does not have mechanisms. Make sure it is OUxIM or PSNUxIM')
  }

  #check internet connection
  if(curl::has_internet() == FALSE) {
    usethis::ui_warn("No internet connection. Cannot access offical names & rename.")
  } else {

    usethis::ui_info("Connecting to DATIM and accessing mechanism table. This may take over a minute to run. Please be patient.")

  #store column names (to work for both lower case and camel case) & then covert to lowercase
    headers_orig <- names(df)
    df <- dplyr::rename_all(df, tolower)

  #ask for credentials if not stored - username
    if(glamr::is_stored("datim") && missing(datim_user)){
      datim_user <- glamr::datim_user()
    } else if(missing(datim_user)) {
      datim_user <- getPass::getPass("DATIM username")
    }

  #ask for credentials if not stored - password
    if(glamr::is_stored("datim") && missing(datim_pwd)){
      datim_pwd <- glamr::datim_pwd()
    } else if(missing(datim_pwd)) {
      datim_pwd <- getPass::getPass("DATIM password", forcemask = TRUE)
    }



  #identify what OUs to map over
    if("operatingunit" %in% names(df)){
      ous <- unique(df$operatingunit)
    } else {
      ous <- lst_ous
    }

  #if year is present, us that to filter down request
    if("fiscal_year" %in% names(df)){
      date_floor <- (min(df$fiscal_year, na.rm = TRUE) - 1) %>% paste0("09-30-", .)
    } else if("period" %in% names(df)){
      fy_floor <- min(df$period, na.rm = TRUE) %>% stringr::str_sub(3, 4) %>% as.numeric()
      date_floor <- paste0("09-30-20", fy_floor-1)
    } else {
      date_floor <- NULL
    }

  #access current mechanism list
    mech_official <-
      purrr::map_dfr(.x = ous,
                     .f = ~ extract_datim_names(ou = .x,
                                                end_date = date_floor,
                                                username = datim_user,
                                                password = datim_pwd)
      )

  #rename variables to match MSD and remove mechid from mech name
    mech_official <- mech_official %>%
      dplyr::select(mech_code = code,
                    prime_partner_name_zXz = partner,
                    mech_name_zXz = mechanism,
                    funding_agency_zXz = agency) %>%
      dplyr::mutate(mech_name_zXz = stringr::str_remove(mech_name_zXz, "0000[0|1] |[:digit:]+ - "))

  #remove award information from mech_name
    mech_official <- mech_official %>%
      dplyr::mutate(mech_name_zXz = stringr::str_remove(mech_name_zXz,
            "^(720|AID|GH(AG|0)|U[:digit:]|NUGGH|UGH|U91|CK0|HT0|N[:digit:]|SGY||NU2|[:digit:]NU2|1U2).* - "))

  #merge official names into df
    df <- dplyr::left_join(df, mech_official, by="mech_code")

  #clean up Dedup and AGWY
    df <- df %>%
      dplyr::mutate(dplyr::across(c(prime_partner_name_zXz, mech_name_zXz, funding_agency_zXz), ~ ifelse(mech_code %in% c("00000", "00001"), "Dedup", .)),
                    prime_partner_name_zXz = ifelse(mech_code == "HllvX50cXC0", "Default", prime_partner_name_zXz),
                    funding_agency_zXz = ifelse(mech_code == "HllvX50cXC0", "Default", funding_agency_zXz),
                    mech_name_zXz = ifelse(mech_code == "HllvX50cXC0", "Missing", mech_name_zXz))

  #replace prime partner and mech names with official names and then remove
    if(!"mech_name" %in% names(df)){
      df <- dplyr::mutate(df, mech_name = as.character(NA))
      headers_orig <- c(headers_orig, "mech_name")
    }
    if(!"prime_partner_name" %in% names(df)){
      df <- dplyr::mutate(df, prime_partner_name = as.character(NA))
      headers_orig <- c(headers_orig, "prime_partner_name")
    }
    if(!"funding_agency" %in% names(df)){
      df <- dplyr::mutate(df, funding_agency = as.character(NA))
      headers_orig <- c(headers_orig, "funding_agency")
    }

    df <- df %>%
      dplyr::mutate(mech_name = mech_name_zXz,
                    prime_partner_name = prime_partner_name_zXz,
                    funding_agency = funding_agency_zXz) %>%
      dplyr::select(-dplyr::ends_with("_zXz"))

  #reapply original variable casing type
    names(df) <- headers_orig

  #reorder new variables
    df <- df %>%
      dplyr::relocate(c(mech_name, prime_partner_name, funding_agency), .after = mech_code)

  }

  return(df)
}



#' Extract Data from DATIM
#'
#' @param ou       Operatingunit name
#' @param end_date Exclude mechs ending by this date
#' @param base_url url for query
#' @param username Datim username
#' @param password Datim password
#' @param verbose  Show notification during process
#'
#' @return converts json to data frame
#' @keywords internal
#'
extract_datim_names <- function(ou,
                                end_date = NULL,
                                base_url = "https://www.datim.org/",
                                username,
                                password,
                                verbose = FALSE){

  #Exclude apostrophe from OU name
  if (stringr::str_detect(ou, "\\'"))
    ou <- stringr::str_extract(ou, "^.*(?=\\')")

  # Core + OU filter
  sql_view_url <- paste0(base_url,
                         "api/sqlViews/fgUtV6e9YIX/data.json?",
                         "filter=ou:ilike:", ou)
  # end date filter
  if (!is.null(end_date))
    sql_view_url <- paste0(sql_view_url, "&filter=enddate:gt:", end_date)

  # Remove Pagination
  sql_view_url <- paste0(sql_view_url, "&paging=false")

  # Notification
  if (verbose) {
    usethis::ui_info(base::paste0("Extracting mechanisms details for: ", base::toupper(ou)))
    usethis::ui_info(sql_view_url)
  }

  # Run the query
  r <- sql_view_url %>%
    utils::URLencode() %>%
    httr::GET(httr::authenticate(username, password)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = TRUE)

  # Clean response
  # Keep structure for empty response
  df <- purrr::map(r$listGrid$headers$column, ~character()) %>%
    purrr::set_names(r$listGrid$headers$column) %>%
    tibble::as_tibble()

  # Convert to tibble with name repair
  if (length(r$listGrid$rows) > 0) {
    df <- tibble::as_tibble(x = r$listGrid$rows,
                           .name_repair = ~ r$listGrid$headers$column)
  }

  return(df)
}
