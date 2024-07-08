#' Extract MSD Source Information
#'
#' This function is used primarily to extract the data from the source file of
#' a MER Structured Dataset, MER NAT_SUBNAT Structured Dataset, Financial
#' Structure Dataset, or DATIM Genie export. It can also be used to extact
#' information from the filename about the fiscal year, quarter, or period.
#'
#' @param path path to the folder containing MSDs or specific MSD file
#' @param type not required unless providing a folder in `path`;
#'   default = "OU_IM_FY21"; other examples include: "PSNU_IM", "NAT_SUBNAT",
#'   "PSNU", "Financial", "HRH"
#' @param return from the info, what should be returned; default = "source"
#' other options are: "period", "fiscal_year", "fiscal_year_label","quarter"
#'
#' @return vector of information related to what is being asked in `return`
#' @export
#' @family metadata
#'
#' @examples
#' \dontrun{
#'  source_info() #works if you have stored path to the MSD folder via glamr::set_paths()
#'  source_info("../Data", type = "PSNUxIM")
#'  source_info("../Data", type = "PSNUxIM", return = "period")
#'  source_info("../Downloads/Genie_PSNU_IM_Jupiter_Daily_c9f5889f-86c9-44e7-ab63-fa86c587d251.zip")
#'  source_info("../Data/MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20210618_v2_1.rds") }
#'
#' \dontrun{
#' library(tidyverse)
#' library(glamr)
#' library(glue)
#'
#' df <- si_path() %>%
#'   return_latest("OU_IM") %>%
#'   read_msd()
#'
#' df_viz <- df %>%
#'   filter(operatingunit == "Saturn",
#'          indicator == "TX_NEW",
#'          standardizeddisaggregate == "Total Numerator") %>%
#'   count(fiscal_year, wt = targets, name = "targets")
#'
#' df_viz %>%
#'   ggplot(aes(fiscal_year, targets)) +
#'   geom_col() +
#'   labs(caption = glue("Source: {source_info()}")) }
source_info <- function(path, type, return = "source"){

  lifecycle::deprecate_soft("4.0.0", "source_info()", "get_metadata()")

  #extra all file metadata
  info <- extract_metadata(path, type)

  #extract key element
  info <- info[[return]]

  return(info)

}

#' Extract MSD Meta Data
#'
#' This function is used to extract meta data as a list from the source file of
#' a MER Structured Dataset, MER NAT_SUBNAT Structured Dataset, Financial
#' Structure Dataset, HRH Structured Dataset, or DATIM Genie export. It creates
#' a list object, metadata, in the global environment containing the source,
#' current fiscal year, current period, current quarter, as well as a caption.
#'
#' @param path path to the folder containing MSDs or specific MSD file (default
#'   relies on glamr::si_path() if available)
#' @param type PSD type: "OU_IM", PSNU_IM", "NAT_SUBNAT", "Financial";
#'   default = "OU_IM_FY2*"
#' @param caption_note additional information to include in a viz caption footer
#'
#' @return list of meta data information about the source dataset
#'
#' @export
#' @family metadata
#' @examples
#' \dontrun{
#'  meta <- get_metadata() #works if you have stored path to the MSD folder via glamr::set_paths()
#'  meta$curr_fy }
#'
#' \dontrun{
#' library(tidyverse)
#' library(glamr)
#' library(gophr)
#' library(glue)
#'
#' ref_id <- "1bdf4c4e"
#'
#' meta <- get_metadata(caption_note = "Created by: The Dream Team")
#'
#' cntry <- "Saturn"
#'
#' df <- si_path() %>%
#'   return_latest("OU_IM") %>%
#'   read_msd()
#'
#' df_viz <- df %>%
#'   filter(operatingunit == cntry,
#'          fiscal_year == meta$curr_fy,
#'          indicator == "TX_NEW",
#'          standardizeddisaggregate == "Total Numerator")
#'
#' df_viz <- df_viz %>%
#'   group_by(fiscal_year, indicator, mech_code) %>%
#'   summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
#'             .groups = "drop")
#'
#' df_viz <- reshape_msd(df_viz, "quarters")
#'
#' df_viz %>%
#'   ggplot(aes(period, results_cumulative)) +
#'   geom_col() +
#'   geom_text(data = . %>% filter(., period == meta$curr_pd),
#'             aes(label = results_cumulative),
#'             vjust = -.5) +
#'   facet_wrap(~fct_reorder2(mech_code, period, targets)) +
#'   labs(title = glue("Upward trend in TX_NEW results thru {meta$curr_qtr} quarters") %>% toupper,
#'        subtitle = glue("{cntry} | {meta$curr_fy_lab} cumulative mechanism results"),
#'        x = NULL, y = NULL,
#'        caption = glue("{meta$caption}")) }
#'
get_metadata <- function(path, type, caption_note){

  #extra all file metadata
  info <- extract_metadata(path, type)

  id <- ifelse(exists('ref_id'),
               glue::glue(' | Ref id: {ref_id}'), "")


  cap <- ifelse(!(missing(caption_note) || is.null(caption_note)),
                glue::glue(' | {caption_note}'), "")

  metadata <- info %>%
    dplyr::mutate(caption = glue::glue("Source: {source}{id}{cap}")) %>%
    dplyr::select(curr_pd = period,
                  curr_fy = fiscal_year,
                  curr_fy_lab = fiscal_year_label,
                  curr_qtr = quarter,
                  source,
                  caption) %>%
    as.list()

  usethis::ui_warn("{usethis::ui_field('metadata')} is NO LONGER (v3.2.3) \\
                    exported by default as a global object.")
  usethis::ui_info("You must store the output as an object to use, e.g. \\
                     {usethis::ui_code('meta <- get_metadata()')}")

  return(metadata)

}

#' Extract MSD Source Information
#'
#' @param path path to the folder containing MSDs or specific MSD file
#' @param type not required unless providing a folder in `path`;
#'  default = "OU_IM_FY2*", other examples include: "PSNU_IM", "NAT_SUBNAT",
#'  "PSNU", "Financial", "HRH"
#'
#' @return dataframe of information related to what is being asked in `return`
#' @family metadata
#' @keywords internal
#'
extract_metadata <- function(path, type){

  r_env <- get_pdap_loc()

  #extract file path
  path <- switch(r_env,
                 "local" = extract_path_local(path, type),
                 "pdap" = extract_path_s3(path, type))

  #strip out full filepath to just keep name
  file_name <- basename(path)

  #identify the type of file
  file_type <- dplyr::case_when(stringr::str_detect(file_name, "NAT_SUBNAT") ~ "NAT_SUBNAT",
                                stringr::str_detect(file_name, "Genie") ~ "DATIM Genie",
                                stringr::str_detect(file_name, "PDAPWave") ~ "PDAP Wave",
                                stringr::str_detect(file_name, "Fin.*_Structured_Dataset") ~ "FSD",
                                stringr::str_detect(file_name, "MER_Structured_Datasets") ~ "MSD",
                                stringr::str_detect(file_name, "MER_Structured_TRAINING_Datasets") ~ "Faux Training MSD",
                                stringr::str_detect(file_name, "HRH_Structured_Datasets") ~ "HRH")
  if(is.na(file_type))
    usethis::ui_stop("Filename does not match PSD structure. Unable to parse.")

  #add date if missing from the file name (!! may be problematic, ie email old file, download, has download date a created date)
  if(!grepl("\\d{8}|\\d{4}-\\d{2}-\\d{2}", file_name)){
    cdate <- file.info(path)$ctime %>% format('%Y-%m-%d') %>% as.character
    file_name <- stringr::str_replace(file_name, glue::glue("\\.({tools::file_ext(file_name)})"), glue::glue("-{cdate}.\\1"))
    usethis::ui_info("ISO date not found in filepath. Assuming the pull date is the same as the file creation date - {usethis::ui_field(cdate)}")
  }

  #capture the dataset date for use in figuring out relevant FY period
  file_date <- dplyr::case_when(stringr::str_detect(file_name, "Genie|Wave|Recent") ~ stringr::str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}"),
                                TRUE ~ stringr::str_extract(file_name, "[:digit:]{8}") %>% as.Date("%Y%m%d") %>% as.character)

  #calendar of DATIM close windows
  info <- glamr::pepfar_data_calendar %>%
    dplyr::select(-msd_release) %>%
    tidyr::pivot_longer(dplyr::starts_with("entry"),
                        names_to = "datim_status",
                        names_prefix = "entry_",
                        values_to = "date") %>%
    dplyr::mutate(type = ifelse(datim_status == "open", "provisional", type),
                  date = as.Date(date))

  #drop provisional window if not using DAILY pull from Genie/PDAP Wave
  if(stringr::str_detect(file_name, "Daily", negate = TRUE))
    info <- dplyr::filter(info, type != "provisional")

  #identify the pull date for the caption if using Genie or PDAP Wave
  pull_date <- ifelse(file_type %in% c("DATIM Genie", "PDAP Wave"),
                          glue::glue(" [{file_date}]"), "")

  #use the file date to identify the PEPFAR reporting period
  info <- info %>%
    dplyr::filter(date <= as.Date(file_date)) %>%
    dplyr::slice_tail() %>%
    dplyr::mutate(fiscal_year_label = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}"),
                  period = glue::glue("{fiscal_year_label}Q{quarter}"),
                  source = glue::glue("{period}{stringr::str_sub(type, end = 1)} {file_type}{pull_date}"))

  #Training dataset
  if(file_type == "Faux Training MSD"){
    info <- info %>%
      dplyr::mutate(fiscal_year = fiscal_year + 37,
                    fiscal_year_label = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}"),
                    period = glue::glue("{fiscal_year_label}Q{quarter}"),
                    source = glue::glue("{period}{stringr::str_sub(type, end = 1)} Faux Training MSD"))
  }

  return(info)
}


#' Extract MSD path locally
#'
#' @inheritParams extract_metadata
#'
#' @keywords internal
#'
extract_path_local <- function(path, type){

  if((missing(path) || is.null(path)) && is.null(getOption("path_msd")))
    stop("No path to a file or folder was provided.")

  if((missing(path) || is.null(path)))
    path <- glamr::si_path()

  if(!file.exists(path))
    stop("File/folder do not exist or path is not correct.")

  if(file.info(path)$isdir && (missing(type) || is.null(type)))
    type <- "OU_IM_FY2"

  if(file.info(path)$isdir)
    path <- glamr::return_latest(path, type)

  return(path)
}


#' Extract MSD path on s3 from Data Mart
#'
#' @inheritParams extract_metadata
#'
#' @keywords internal
#'
extract_path_s3 <- function(path, type){

  if(!requireNamespace("grabr", quietly = TRUE))
    usethis::ui_stop("Package {usethis::ui_field('grabr')} is required, see - https://usaid-oha-si.github.io/grabr/")

  if((missing(path) || is.null(path)) && (missing(type) || is.null(type)))
    type <- "OU_IM_Recent"

  search_key <- ifelse(!(missing(path) || is.null(path)), path, type)

  pdap_bucket <- ifelse(grepl("^usaid/", path), Sys.getenv("S3_WRITE"), Sys.getenv("S3_READ"))
  pdap_prefix <- ifelse(grepl("^usaid/", path), "usaid/", NULL)

  suppressWarnings(
    assets <- grabr::s3_objects(bucket = pdap_bucket,
                                prefix = pdap_prefix,
                                access_key = Sys.getenv("AWS_ACCESS_KEY_ID"),
                                secret_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"))
  )

  path <- assets %>%
    dplyr::filter(stringr::str_detect(key, {{search_key}})) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::mutate(key_date = stringr::str_replace(key, "_Recent(\\.|_)", glue::glue("_Recent-{substr(last_modified, 1, 10)}\\1"))) %>%
    dplyr::pull(key_date)


  return(path)
}
