#' Apply Funding Type
#'
#' When working with financial or HRH data, its often useful to determine
#' whether funding or staffing are counted as "service delivery" or "non-service
#' delivery"
#'
#' @param df Financial Structured Dataset data frame
#'
#' @return a new column, funding_type
#' @export
#'

apply_funding_type <- function(df){

  #check that mechanism exists in dataset
  if(any(c("interaction_type", "program") %in% names(df)) == FALSE) {
    stop('This dataset does not contain either interaction_type or program. Make
         sure this is a FSD or HRH SD and these variables have not been dropped')
  }

  df %>%
    dplyr::mutate(funding_type =
                    dplyr::case_when(interaction_type %in% c("Direct Service Delivery", "Service Delivery")  ~ "Service Delivery (SD)",
                                     interaction_type == "Not Specified" ~ interaction_type,
                                     program == "ASP" ~ "Above Site Non-SD",
                                     program %in% c("PM", "Applied Pipeline") ~ program,
                                     interaction_type %in% c("Non Service Delivery", "Non-Service Delivery") ~ "Site Level Non-SD"),
                  .after = interaction_type)
}

