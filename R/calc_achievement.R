#' Adorn Achievement - Percent and Color
#'
#' `adorn_achievement` calculate target achievement (cumulative and/or quarterly) for a
#' standard MSD or one reshaped using `reshape_msd`()` as well as to apply achievement
#' group labels and colors. It will run `calc_achievement` if an achievement column
#' does not exist in the dataset.
#'
#' @param df data frame as standrd MSD or one from reshape_msd()
#' @param qtr if using standard MSD, need to provide the most recent quarter,
#' ideally using identifypd(df_msd, pd_type = "quarter")
#' @param classic use the original OHA achievement color palette (pre July 2024),
#'  default = FALSE
#'
#' @return data frame with achievement values, labels, and colors
#' @family achievement
#' @export
#'
#' @examples
#' \dontrun{
#' df_msd <- read_msd(path)
#' df_msd_agg <- df_msd %>%
#'  filter(operatingunit == "Jupiter"
#'         indicator %in% c("TX_NEW", "TX_CURR"),
#'         funding_agency != "Dedup",
#'         standardizeddisaggregate == "Total Numerator") %>%
#'  group_by(operatingunit, funding_agency, fiscal_year, indicator) %>%
#'  summarise(across(where(is.double), sum, na.rm = TRUE)) %>%
#'  ungroup()
#'
#' adorn_achievement(df_msd_agg)
#'
#' df_msd_agg %>%
#'  reshape_msd("quarters") %>%
#'  adorn_achievement()
#'
#' df_msd_agg %>%
#'  reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>%
#'  adorn_achievement() }
adorn_achievement <- function(df, qtr = NULL, classic = FALSE){

  #make sure key variables exist
  if(var_missing(df, c("period", "fiscal_year")))
    stop("The data frame provided is missing period or fiscal year, one of which is required.")

  #calculate achievement if it doesn't already exists
  if(var_missing(df, c("achievement", "achievement_qtrly")))
    df <- calc_achievement(df)

  #apply binned color and labels
  df_achv <- color_achievement(df, qtr, classic)

  return(df_achv)
}



#' Calculate Achievement
#'
#' `calc_achievement` creates a target achievement column standard to many PEPFAR
#' analyses. It can calculate achievement from a normal MSD or one reshaped using
#' `reshape_msd`.
#'
#' @param df MSD based dataframe
#'
#' @return one or two additional columns that calculate achievement and/or
#'   quarterly achievement
#' @family achievement
#' @seealso [reshape_msd()]
#' @export
#'
#' @examples
#' \dontrun{
#' df_msd <- read_msd(path)
#' df_msd_agg <- df_msd %>%
#'  filter(operatingunit == "Jupiter"
#'         indicator %in% c("TX_NEW", "TX_CURR"),
#'         funding_agency != "Dedup",
#'         standardizeddisaggregate == "Total Numerator") %>%
#'  group_by(operatingunit, funding_agency, fiscal_year, indicator) %>%
#'  summarise(across(where(is.double), sum, na.rm = TRUE)) %>%
#'  ungroup()
#'
#' calc_achievement(df_msd_agg)
#'
#' df_msd_agg %>%
#'  reshape_msd("quarters") %>%
#'  calc_achievement() }
calc_achievement <- function(df){
  if(!"targets" %in% names(df))
    usethis::ui_stop("No {usethis::ui_field('targets')} in the dataframe provided")

  if(!"cumulative" %in% names(df) && !"results_cumulative" %in% names(df))
    usethis::ui_stop("No {usethis::ui_field('cumulative')} or {usethis::ui_field('results_cumulative')} in the dataframe provided")

  #calculate normal achievement
  if("cumulative" %in% names(df))
    df <- dplyr::mutate(df, achievement = cumulative/targets)

  #calcuate quarterly achievement
  if("results_cumulative" %in% names(df))
    df <- dplyr::mutate(df, achievement_qtrly = results_cumulative/targets)

  #convert Inf and NaN to NA
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("achievement"), ~ dplyr::na_if(., Inf)),
                  dplyr::across(dplyr::starts_with("achievement"), ~ dplyr::na_if(., NaN)))

  #round percent
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("achievement"), ~ round(., 2)))

  return(df)

}



#' Color and label achievement
#'
#' @param df MSD dataframe
#' @param curr_qtr if wide MSD, need to specify the curren period, defaults to Q4
#' @param classic use the original OHA achievement color palette (pre July 2024),
#'  default = FALSE
#'
#' @return dataframe with achievement colors and labels
#' @keywords internal
#'
color_achievement <- function(df, curr_qtr = NULL, classic = FALSE){

  #flag the use of q4 if curr_qtr is not provided
  if(is.null(curr_qtr) && var_missing(df, "period")){
    usethis::ui_info("No quarter provided; assuming all cumulative values are from Q4")
    curr_qtr <- 4
  }

  #stop if not a valid quarter
  if((!is.numeric(curr_qtr) || !curr_qtr %in% c(1:4)) && var_missing(df, "period"))
    stop("The quarter parameter must be a number between 1-4")

  #determine whether to label/color off quarterly or annual achievement
  if(var_exists(df, "achievement_qtrly")){
    df <- dplyr::mutate(df, achv_value = achievement_qtrly)
  } else {
    df <- dplyr::mutate(df, achv_value = achievement)
  }

  #add a quarter column need for the color/labeling
  df <- assign_quarter(df, curr_qtr)

  #apply labels and colors based on quarterly goal
  df <- df %>%
    dplyr::mutate(qtr_goal = ifelse(indicator %in% snapshot_ind, 1, 1*(qtr/4)),
                  achv_desc = dplyr::case_when(is.na(achv_value) ~ NA_character_,
                                               achv_value <= qtr_goal-.25 ~ "Concerned",
                                               achv_value < qtr_goal-.1 ~ "At Risk",
                                               achv_value <= qtr_goal+.1 ~ "On Target",
                                               TRUE ~ "Above Target"),
                  achv_label = dplyr::case_when(is.na(achv_value) ~ NA_character_,
                                                achv_value <= qtr_goal-.25 ~ glue::glue("<{100*(qtr_goal-.25)}%") %>% as.character,
                                                achv_value < qtr_goal-.1 ~ glue::glue("{100*(qtr_goal-.25)}-{100*(qtr_goal-.11)}%") %>% as.character,
                                                achv_value <= qtr_goal+.1 ~ glue::glue("{100*(qtr_goal-.1)}-{100*(qtr_goal+.1)}%") %>% as.character,
                                                TRUE ~ glue::glue("+{100*(qtr_goal+.1)}%") %>% as.character)
    ) %>%
    dplyr::select(-c(achv_value, qtr_goal, qtr))

  #import colors from data to use for achievement
  df_color <- achv_color_map

  #determine what colors to use based on classic TRUE/FALSE
  if(classic == TRUE)
      df_color <- dplyr::mutate(df_color, achv_color = achv_color_classic)

  #join colors to df to apply colors based on quarterly goal
  df <- dplyr::left_join(df,
                         df_color %>%
                           dplyr::select(achv_desc, achv_color),
                         by = "achv_desc")

  #make achv_desc an ordered factor
  df <- df %>%
    dplyr::mutate(achv_desc = factor(achv_desc,
                                     c("Concerned", "At Risk", "On Target",
                                    "Above Target")))


  return(df)
}



#' Assign Quarter
#'
#' @param df MSD dataframe, standard or reshaped via reshape_msd()
#' @param curr_qtr quarter for current fiscal year (if standard MSD)
#'
#' @return data frame with a quarter column
#' @keywords internal
#'
assign_quarter <- function(df, curr_qtr = 4){

  #default to q4 if not provided (if it needs to be used)
  if(is.null(curr_qtr))
    curr_qtr <- 4

  #create a quarter column needed for calculating achievement labels/colors
  if(var_exists(df, "period")){
    df <- df %>%
      dplyr::mutate(fy = stringr::str_sub(period, 3, 4) %>% as.numeric,
                    qtr = dplyr::case_when(stringr::str_detect(period, "Q") ~ stringr::str_sub(period, -1) %>% as.numeric,
                                           fy == max(fy) ~ curr_qtr,
                                           TRUE ~ 4)) %>%
      dplyr::select(-fy)
  } else {
    df <- dplyr::mutate(df, qtr = ifelse(fiscal_year == max(fiscal_year), curr_qtr, 4))
  }

  return(df)
}
