#' @title Clean column data
#'
#' @param .data   MSD Datasets
#' @param colname Name of the column(s)
#' @return  Cleaned MSD DataFrame
#' @family column munging
#' @export
#' @examples
#' \dontrun{
#'  df_msd %>% clean_column(colname = "psnu") }

clean_column <- function(.data, colname = "psnu") {

    # Check params
    name <- {{colname}}

    # Check for valid column name
    if (length(dplyr::setdiff(name,  names(.data))) > 0) {
      cat("\nERROR - Column name is unknown: ",
          crayon::red({{colname}}), "\n")

      return(NULL)
    }

    # Funding Agencies
    if(name == "funding_agency") {
      .data <- .data %>%
        clean_agency() %>%
        dplyr::mutate(funding_agency = dplyr::case_when(
          funding_agency == "PC" ~ "Peace Corps",
          TRUE ~ funding_agency
        ))

      return(.data)
    }

    # Operatingunit / country
    if(base::all(name %in% c("operatingunit", "country"))) {
      .data <- .data %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(name),
                      ~ dplyr::case_when(
                        . == "Democratic Republic of the Congo" ~ "DRC",
                        . == "Papua New Guinea" ~ "PNG",
                        . == "Dominican Republic" ~ "DR",
                        . == "Trinidad and Tobago" ~ "T&T",
                        . == "Antigua and Barbuda" ~ "Antigua & Barbuda",
                        . == "Saint Kitts and Nevis" ~ "St Kitts & Nevis",
                        . == "Saint Vincent and the Grenadines" ~ "St Vincent & Grenadines",
                        . == "Asia Regional Program" ~ "ARP",
                        . == "Central America Region" ~ "CAR",
                        . == "West Africa Region" ~ "WAR",
                        . == "Western Hemisphere Region" ~ "WHR",
                        TRUE ~ .
                      )))

      return(.data)
    }

    # Remove characters
    rmv_tail <- c(
        "District",
        "County",
        "District Municipality",
        "Metropolitan Municipality",
        "Municipality"
      ) %>%
      paste0(" ", ., "$", collapse = "|")

    # Remove characters at the end
    .data <- .data %>%
      dplyr::mutate_at(.vars = dplyr::all_of(name),
                       stringr::str_remove,
                       pattern = rmv_tail)

    # Remove first 2 leading chars
    rmv_lead <- "^[A-Za-z]{2}[[:space:]]"

    # Remove characters
    .data <- .data %>%
      dplyr::mutate_at(.vars = dplyr::all_of(name),
                       stringr::str_remove,
                       pattern = rmv_lead)
  }


#' @title Clean PSNU column data
#'
#' @param .data   MSD Datasets
#' @return  Cleaned MSD DataFrame
#' @family column munging
#' @export
#' @examples
#' \dontrun{
#'  df_msd %>% clean_psnu() }

clean_psnu <- function(.data) {

    # Check for valid column name
    if (!"psnu" %in% names(.data)) {
      cat("\nERROR - psnu column is not available as a column.\n")
      return(NULL)
    }

    # Remove extract characters
    .data <- .data %>%
      clean_column(colname = "psnu")
  }


#' @title Clean data from funding agency column
#'
#' This function converts all funding agency names to upper case removes
#' the HHS prefix for those agencies
#' and moves State and USAID subsidiaries under their parent agencies
#'
#' @param .data MSD Datasets
#' @return  Cleaned MSD DataFrame
#' @family column munging
#' @export
#' @examples
#' \dontrun{
#'  df_msd %>% clean_agency() }
#'
clean_agency <- function(.data) {

    # Check for valid column name
    if (!"funding_agency" %in% names(.data)) {
      cat("\nERROR - funding_agency column is not available as a column.\n")
      return(NULL)
    }

    # clean column data
    .data <- .data %>%
      dplyr::mutate(funding_agency = stringr::str_to_upper(funding_agency) %>%
                      stringr::str_remove("(^HHS\\/|\\/.*$)"))
  }


#' Clean indicators (apply _D suffix)
#'
#' `clean_indicator` applies a '_D' suffix to any indicators that are a
#' denominator. This is particularly useful when aggregating data or
#' reshaping.
#'
#' @param df MSD data frame
#'
#' @return indicators with denominator have _D suffix
#' @family column munging
#' @export
#'
#' @examples
#' \dontrun{
#' df <- df %>%
#' filter(indicator == "TX_PVLS",
#'        standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
#' clean_indicator() }
clean_indicator <- function(df){
  # Check for valid column name
  if (!all(c("indicator", "numeratordenom") %in% names(df))) {
    usethis::ui_warn("ERROR - {usethis::ui_field('indicator')} and/or {usethis::ui_field('numeratordenom')} are not columns in the dataframe. No adjustments to indicator made.")
  } else {
    #add _D to indicators that are denominators
    df <- df %>%
      dplyr::mutate(indicator =
                      dplyr::case_when(indicator %in% c("TX_TB_D_NEG", "TX_TB_D_POS") ~ glue::glue("{indicator}"),
                                       numeratordenom == "D" ~ glue::glue("{indicator}_D"),
                                       TRUE ~ glue::glue("{indicator}")) %>% paste(.))

  }
  return(df)

}
