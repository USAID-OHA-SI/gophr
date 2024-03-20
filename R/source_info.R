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
#' @param path path to the folder containing MSDs or specific MSD file
#' @param type not required unless providing a folder in `path`; default = "OU_IM_FY19"
#' other examples include: "PSNU_IM", "NAT_SUBNAT", "PSNU", "Financial"
#' @param caption_note additional information to include in the footer
#'
#' @return list of meta data information about the source dataset
#'
#' @export
#' @family metadata
#' @examples
#' \dontrun{
#'  get_metadata() #works if you have stored path to the MSD folder via glamr::set_paths()
#'  metadata$curr_fy }
#'
#' \dontrun{
#' library(tidyverse)
#' library(glamr)
#' library(gophr)
#' library(glue)
#'
#' ref_id <- "1bdf4c4e"
#'
#' get_metadata(caption_note = "Created by: The Dream Team")
#'
#' cntry <- "Saturn"
#'
#' df <- si_path() %>%
#'   return_latest("OU_IM") %>%
#'   read_msd()
#'
#' df_viz <- df %>%
#'   filter(operatingunit == cntry,
#'          fiscal_year == metadata$curr_fy,
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
#'   geom_text(data = . %>% filter(., period == metadata$curr_pd),
#'             aes(label = results_cumulative),
#'             vjust = -.5) +
#'   facet_wrap(~fct_reorder2(mech_code, period, targets)) +
#'   labs(title = glue("Upward trend in TX_NEW results thru {metadata$curr_qtr} quarters") %>% toupper,
#'        subtitle = glue("{cntry} | {metadata$curr_fy_lab} cumulative mechanism results"),
#'        x = NULL, y = NULL,
#'        caption = glue("{metadata$caption}")) }
#'
get_metadata <- function(path, type, caption_note){

  #extra all file metadata
  info <- extract_metadata(path, type)

  id <- ifelse(exists('ref_id'),
               glue::glue(' | Ref id: {ref_id}'), "")


  cap <- ifelse(!missing(caption_note),
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

  if(missing(path) && is.null(getOption("path_msd")))
    stop("No path to a file or folder was provided.")

  if(missing(path))
    path <- glamr::si_path()

  if(!file.exists(path))
    stop("File/folder do not exist or path is not correct.")

  if(file.info(path)$isdir && missing(type))
    type <- "OU_IM_FY2"

  if(file.info(path)$isdir)
    path <- glamr::return_latest(path, type)

  #strip out full filepath to just keep name
  file_name <- basename(path)

  #identify the type of file
  file_type <- dplyr::case_when(stringr::str_detect(file_name, "(Genie|NAT_SUBNAT)") ~ stringr::str_extract(path, "(Frozen|Daily|NAT_SUBNAT)"),
                                stringr::str_detect(file_name, "Fin.*_Structured_Dataset") ~ "FSD",
                                stringr::str_detect(file_name, "MER_Structured_Datasets") ~ "MSD",
                                stringr::str_detect(file_name, "MER_Structured_TRAINING_Datasets") ~ "Faux Training MSD",
                                stringr::str_detect(file_name, "HRH_Structured_Datasets") ~ "HRH")

  #capture the dataset date for use in figuring out relvant FY period
  file_date <- ifelse(stringr::str_detect(file_name, "Genie"),
                      file.info(path)$ctime %>% format("%Y-%m-%d"),
                      stringr::str_extract(file_name, "[:digit:]{8}"))

  #depending on the type, create a dataframe with relevant info
  if(file_type == "Frozen") {
    #frozen
    info <- glamr::pepfar_data_calendar %>%
      dplyr::select(-msd_release) %>%
      dplyr::filter(as.Date(entry_close) <= file_date) %>%
      dplyr::slice_tail() %>%
      dplyr::mutate(fiscal_year_label = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}"),
                    period = glue::glue("{fiscal_year_label}Q{quarter}"),
                    source = glue::glue("{period}{stringr::str_sub(type, end = 1)} DATIM Genie [{file_date}]"))
  } else if(file_type == "Daily") {
    #daily
    info <- glamr::pepfar_data_calendar %>%
      dplyr::select(-msd_release) %>%
      tidyr::pivot_longer(dplyr::starts_with("entry"),
                          names_to = "datim_status",
                          names_prefix = "entry_",
                          values_to = "date") %>%
      dplyr::mutate(type = ifelse(datim_status == "open", "provisional", type),
                    date = as.Date(date)) %>%
      dplyr::filter(date <= as.Date(file_date)) %>%
      dplyr::slice_tail() %>%
      dplyr::mutate(fiscal_year_label = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}"),
                    period = glue::glue("{fiscal_year_label}Q{quarter}"),
                    source = glue::glue("{period}{stringr::str_sub(type, end = 1)} DATIM Genie [{file_date}]"))
  } else {
    #MSD/FSD/NAT_SUBNAT
    info <- glamr::pepfar_data_calendar %>%
      dplyr::select(-msd_release) %>%
      dplyr::mutate(entry_close = stringr::str_remove_all(entry_close, "-")) %>%
      dplyr::filter(entry_close <= file_date) %>%
      dplyr::slice_tail() %>%
      dplyr::mutate(fiscal_year_label = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}"),
                    period = glue::glue("{fiscal_year_label}Q{quarter}"),
                    source = glue::glue("{period}{stringr::str_sub(type, end = 1)} {file_type}"))

  }

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
