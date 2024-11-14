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
    lifecycle::deprecate_stop(when = "3.2.0", "read_msd()", "read_psd()")
  }

#' Import PEPFAR Structured Datasets to R
#'
#' `read_psd` imports a stored PEPFAR Structured Datasets (.zip, .txt, or
#' .parquet). The function will read in a MSD, Genie, Financial or
#' HRH PEPFAR dataset, ensuring the column types are correct. The user has the
#' ability to store the txt file as a rds or parquet file, significantly saving
#' storage space on the computer (and can then remove the txt file after
#' importing). Most of USAID/OHA processes and analyses rely on the use of
#' the MSD file being read in via `read_psd`. This function can be used in the
#' PDAP space in addition to working locally.
#'
#' @export
#'
#' @param file enter the full path to the PEPFAR structured dataset file
#' @param export_format if desired, save the PSD in another compressed format,
#'   either "rds" or "parquet", default = "none"
#' @param remove_base_file should original base file be removed if exporting in
#'   another compressed format? default = FALSE
#' @param retain_genie_cols should Genie specific columns (`dataelementuid`,
#'   `categoryoptioncombouid`, `approvallevel`, `approvalleveldescription`) be
#'   retained in the output dataset? default = FALSE
#'
#' @examples
#'
#'\dontrun{
#'#convert Q1 clean PSNU file from txt to Rds
#'#read in file for use
#'  path <- "~/Data/MER_Structured_Datasets_OU_IM_FY22-24_20240315_v2_1.zip"
#'  df_psnu <- read_psd(path) }
#'
read_psd <-
  function(file,
           export_format = "none",
           remove_base_file = FALSE,
           retain_genie_cols = FALSE){

    file_type <- ifelse(tools::file_ext(file) == "rds", "already_rds", "raw_txt")

    switch(file_type,
           "already_rds" = readr::read_rds(file),
           "raw_txt" = process_psd(file,
                                   export_format = export_format,
                                   remove_base_file = remove_base_file,
                                   retain_genie_cols = retain_genie_cols))

  }

#' Processing to handle PSD as a zip/txt file
#'
#' @inheritParams read_psd
#'
#' @keywords internal
#'
process_psd <- function(file,
                        export_format = export_format,
                        remove_base_file = FALSE,
                        retain_genie_cols = FALSE){

  #location
  file_location <- get_pdap_loc()

  #identify which bucket to use if on PDAP
  pdap_bucket <- locate_bucket(file)

  #import
  df <- switch(file_location,
               "local" = handle_psd_format(file),
               "pdap" = aws.s3::s3read_using(handle_psd_format,
                                             bucket = pdap_bucket,
                                             object = file))

  #convert new names to old or old to new (changes introduced in FY22Q2)
  df <- convert_names(df, retain_genie_cols)

  #covert target/results/budgets/ftes/counts to double
  df <- convert_coltype(df)

  #save as rds
  if(export_format == "rds"){
    newfile <- rename_psd(file)
    saveRDS(df, newfile)
  }

  #save as parquet
  if(export_format == "parquet"){
    if(!requireNamespace("arrow", quietly = TRUE))
      usethis::ui_stop("Package {usethis::ui_field('arrow')} is required to read a .parquet file. Restart session and install - {usethis::ui_code('install.packages(\\'arrow\\')')}")
    newfile <- rename_psd(file)
    arrow::write_parquet(df, newfile)
  }

  #remove base file
  if (remove_base_file == TRUE && !grepl(".com", file))
    file.remove(file)

  return(df)
}


#' Rename PSD file when importing
#'
#' @inheritParams read_psd
#' @param ext new file extention
#'
#' @keywords internal

rename_psd <- function(file, ext){

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
                      paste0("MER_Structured_Dataset_", type,"_GENIE_FY23-24_", stringr::str_remove_all(Sys.Date(), "-"),".txt"))
  }

  file <- stringr::str_replace(file, "(zip|txt)$", {ext})

  return(file)

}



#' Convert variables for alignment
#'
#'
#' @param df data frame from read_psd()
#' @inheritParams read_psd
#'
#' @keywords internal
#'
convert_names <- function(df, retain_genie_cols){

  #drop Genie variables
  if(retain_genie_cols == TRUE){
    vars_genie <- c("dataelementuid", "categoryoptioncombouid",
                    "approvallevel", "approvalleveldescription")
    vars_keep <- setdiff(names(df), vars_genie)
    df <- dplyr::select(df, dplyr::all_of(vars_keep))
  }

  #replace - with _ for HRH dataset
  if("moh-secondment" %in% names(df))
    df <- dplyr::rename(df, moh_secondment = `moh-secondment`)

  #align FSD naming with MSD
  if(var_exists(df, "implementation_year"))
    df <- dplyr::rename(df, fiscal_year = implementation_year)

  #align FSD naming with MSD
  if(var_exists(df, "fundingagency"))
    df <- dplyr::rename(df, funding_agency = fundingagency)

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

  #adjust pipeline issue with tab and space in two rows [resolved]
  # if("cop_budget_pipeline" %in% names(df))
  #   df <- dplyr::mutate(df, cop_budget_pipeline = dplyr::na_if(cop_budget_pipeline, '\t\"'))

  return(df)
}


#' Read a tsv and parquet file
#'
#' @inheritParams read_psd
#' @keywords internal
#'
handle_psd_format <- function(file){

  #file type
  file_type <- sub(".*\\.(.*)$", "\\1", file)

  #location
  file_location <- get_pdap_loc()

  #txt delimiter
  d <- ifelse(file_type == "txt" & file_location == "pdap", "|", "\t")

  #check that the file will be parsed
  acceptable_types <- c("zip", "txt", "parquet")
  if(!file_type %in% acceptable_types)
    usethis::ui_stop("The {usethis::ui_value('file')} provided, {usethis::ui_path(basename(file))}, does not match any of the accepted file formats - {usethis::ui_field(acceptable_types)}")

  #import tsv format
  if(file_type %in% c("zip","txt")){
    df <- vroom::vroom(file, delim = d, col_types = c(.default = "c"))
    return(df)
  }

  #check that arrow is installed if handling parquet
  if(file_type == "parquet" && !requireNamespace("arrow", quietly = TRUE))
    usethis::ui_stop("Package {usethis::ui_field('arrow')} is required to read a .parquet file. Restart session and install - {usethis::ui_code('install.packages(\\'arrow\\')')}")

  #import parquet and convert all cols to character
  if(file_type == "parquet"){
    df <- arrow::read_parquet(file)
    df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
    return(df)
  }
}
