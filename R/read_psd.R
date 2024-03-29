#' Import PEPFAR Structured Datasets .txt into R and covert to .rds
#'
#' Deprecated. Use `read_psd` instead.
#'
#' @export
#' @param file enter the full path to the PEPFAR structured dataset file
#' @param save_rds save the Structured Dataset as an rds file, default = FALSE
#' @param remove_txt should the txt file be removed, default = FALSE
#' @param convert_to_old_names replace FY22Q2 naming convention with old?
#'  default = FALSE
read_msd <-
  function(file,
           save_rds = FALSE,
           remove_txt = FALSE,
           convert_to_old_names = FALSE) {

    #depricate
    lifecycle::deprecate_warn("3.2.0", "read_msd()", "read_psd()")

    read_psd(file, save_rds, remove_txt, convert_to_old_names)
  }

#' Import PEPFAR Structured Datasets .txt into R and covert to .rds
#'
#' `read_psd` imports a stored PEPFAR Structured Datasets and
#' coverts it from a .txt to an .Rds to significantly limit file size. Files can
#' be read directly from a zipped file.
#'
#' The benefit of `read_psd` is that it will read in a MSD, Genie, Financial or
#' HRH PEPFAR dataset, ensuring the column types are correct. The user has the
#' ability to store the txt file as a rds, significantly saving storage space
#' on the computer (and can then remove the txt file after importing).
#'
#' Most of USAID/OHA processes and analyses rely on the use of the MSD file
#' being read in via `read_psd`
#'
#' @export
#' @param file enter the full path to the PEPFAR structured dataset file
#' @param save_rds save the Structured Dataset as an rds file, default = FALSE
#' @param remove_txt should the txt file be removed, default = FALSE
#' @param convert_to_old_names replace FY22Q2 naming convention with old?
#'  default = FALSE
#'
#' @examples
#'
#'\dontrun{
#'#convert Q1 clean PSNU file from txt to Rds
#'#read in file for use
#'  path <- "~/Data/ICPI_MER_Structured_Dataset_PSNU_20180323_v2_1.txt"
#'  df_psnu <- read_msd(path)
#'#convert to RDS and delete the original txt file
#'  read_psd(path, save_rds = TRUE, remove_txt = TRUE) }
#'
read_psd <-
  function(file,
           save_rds = FALSE,
           remove_txt = FALSE,
           convert_to_old_names = FALSE) {

    file_type <- ifelse(tools::file_ext(file) == "rds", "already_rds", "raw_txt")

    switch(file_type,
           "already_rds" = readr::read_rds(file),
           "raw_txt" = process_psd(file,
                                   save_rds = save_rds,
                                   remove_txt = remove_txt,
                                   convert_to_old_names = convert_to_old_names))

  }

#' Processing to handle PSD as a zip/txt file
#'
#' @param file enter the full path to the PEPFAR Structured Dataset file
#' @param save_rds save the Structured Dataset as an rds file, default = FALSE
#' @param remove_txt should the txt file be removed, default = FALSE
#'
#' @keywords internal
#'
process_psd <- function(file,
                        save_rds = FALSE,
                        remove_txt = FALSE,
                        convert_to_old_names = FALSE){
  #import
  df <- vroom::vroom(file, delim = "\t", col_types = c(.default = "c"))

  #drop Genie variables
  vars_genie <- c("dataelementuid", "categoryoptioncombouid",
                  "approvallevel", "approvalleveldescription")
  vars_keep <- setdiff(names(df), vars_genie)
  df <- dplyr::select(df, dplyr::all_of(vars_keep))

  #adjust pipeline issue with tab and space in two rows
  if("cop_budget_pipeline" %in% names(df))
    df <- dplyr::mutate(df, cop_budget_pipeline = dplyr::na_if(cop_budget_pipeline, '\t\"'))

  #replace - with _ for HRH dataset
  if("moh-secondment" %in% names(df))
    df <- dplyr::rename(df, moh_secondment = `moh-secondment`)

  #convert old format (pre-FY19Q1 MSD) to match new if applicable
  df <- convert_oldformat(df)

  #convert new names to old or old to new (changes introduced in FY22Q2)
  df <- convert_names(df, keep_old_names = convert_to_old_names)

  #covert target/results/budgets/ftes/counts to double
  df <- convert_coltype(df)

  #save as rds
  if (save_rds == TRUE){
    newfile <- rename_psd(file)
    saveRDS(df, newfile)
  }

  #remove txt file
  if (remove_txt == TRUE && !grepl(".com", file))
    file.remove(file)

  return(df)
}


#' Rename PSD file when importing
#'
#' @param file enter the full path to the PEPFAR Structured Dataset file,
#' eg "~/ICPI/Data/ICPI_MER_Structured_Dataset_PSNU_20180323_v2_1.txt"
#'
#' @keywords internal

rename_psd <- function(file){

  if(stringr::str_detect(file, "Genie")){
    #classify file type
    headers <- vroom::vroom(file, n_max = 0, col_types = readr::cols(.default = "c")) %>%
      names()
    type <- dplyr::case_when(
      "facility" %in% headers                           ~ "SITE_IM",
      !("mech_code" %in% headers)                       ~ "PSNU",
      !("psnu" %in% headers)                            ~ "OU_IM",
      TRUE                                              ~ "PSNU_IM")
    file <- file.path(dirname(file),
                      paste0("MER_Structured_Dataset_GENIE_FY22-23_", stringr::str_remove_all(Sys.Date(), "-"),".txt"))
  }

  file <- stringr::str_replace(file, "(zip|txt)$", "rds")

  return(file)

}


#' Convert any old PSDs to new format
#'
#' @param df data frame from read_psd()
#' @keywords internal

convert_oldformat <- function(df){

  if(any(stringr::str_detect(names(df), "FY"))){

    #rename all vars to lower & to match new names
      df <- df %>%
        dplyr::rename_all(tolower) %>%
        dplyr::rename(mech_code = mechanismid,
                      mech_name = implementingmechanismname,
                      trendsfine =  agefine,
                      trendssemifine = agesemifine,
                      trendscoarse = agecoarse,
                      statushiv = resultstatus)

    #remove mechanism UID no longer used
      df <- dplyr::select(df, -mechanismuid)

    #reshape full long to convert pd from var to columne
      df <- tidyr::gather(df, period, value, dplyr::starts_with("fy"))

    #separate fy from period and reshape wide to match new format
      df <- df %>%
        dplyr::mutate(period = stringr::str_remove_all(period, "fy|_"),
                      period = stringr::str_replace(period, "q", "qtr")) %>%
        tidyr::separate(period, c("fiscal_year", "period"), sep = 4) %>%
        tidyr::spread(period, value) %>%
        dplyr::rename(cumulative = apr) %>%
        dplyr::select(-cumulative, -qtr1:-qtr4, -targets, dplyr::everything())
  }

  return(df)

}


#' Convert variables back to old names if desired
#'
#' With the new changes to names introduced in FY22Q2, a user may have a desire
#' to keep the old naming convention in order to run old code without errors. To
#' do so, the user would specify `keep_old_names = TRUE`. Alternatively, if a
#' user reads in an older MSD, this function will default switch those names to
#' the current standard.
#'
#' @param df data frame from read_psd()
#' @param keep_old_names replace FY22Q2 naming convention with old? default = F
#'
#' @keywords internal
#'
convert_names <- function(df, keep_old_names = FALSE){

  #MSD
  if(keep_old_names == FALSE){
    #replace old names with new ones
    if(var_exists(df, "countryname"))
      df <- dplyr::rename(df, country = countryname)

    if(var_exists(df, "primepartner"))
      df <- dplyr::rename(df, prime_partner_name = primepartner)

    if(var_exists(df, "fundingagency"))
      df <- dplyr::rename(df, funding_agency = fundingagency)

    if(var_exists(df, "trendsfine"))
      df <- dplyr::rename(df, age_2019 = trendsfine)

    if(var_exists(df, "trendssemifine"))
      df <- dplyr::rename(df, age_2018 = trendssemifine)
  } else {
    #replace new names with old ones
    if(var_exists(df, "country"))
      df <- dplyr::rename(df, countryname = country)

    if(var_exists(df, "prime_partner_name"))
      df <- dplyr::rename(df, primepartner = prime_partner_name)

    if(var_exists(df, "funding_agency"))
      df <- dplyr::rename(df, fundingagency = funding_agency)

    if(var_exists(df, "age_2019"))
      df <- dplyr::rename(df, trendsfine = age_2019)

    if(var_exists(df, "age_2018"))
      df <- dplyr::rename(df, trendssemifine = age_2018)
  }

  #align FSD naming with MSD
  if(var_exists(df, "implementation_year"))
    df <- dplyr::rename(df, fiscal_year = implementation_year)

  return(df)
}

#' Convert to specified column types
#'
#' @param df data frame from read_psd()
#' @keywords internal
#'
convert_coltype <- function(df){

  #covert target/results/budgets to double
  df <- df %>%
    dplyr::mutate(dplyr::across(c(dplyr::matches("targets"), dplyr::starts_with("qtr"),
                                  dplyr::matches("cumulative"), dplyr::matches("cop_budget"),
                                  dplyr::matches("_amt"), dplyr::matches("annual"),
                                  dplyr::matches("ftes"), dplyr::matches("months_of_work")),
                                \(x) as.double(x)))
  #convert year to integer
  df <- dplyr::mutate(df, dplyr::across(c(dplyr::matches("fiscal_year"), dplyr::matches("individual_count")),
                                        \(x) as.integer(x)))

  return(df)
}
