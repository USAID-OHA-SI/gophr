#' Identify PSD Data Stream and Level
#'
#' @param df PSD dataframe
#'
#' @return character, PSD data stream and the level
#' @export
#'
#' @examples
#' \dontrun{
#'#read in file for use
#'  path <- "~/Data/ICPI_MER_Structured_Dataset_PSNU_20180323_v2_1.txt"
#'  df <- read_psd(path)
#'#convert to RDS and delete the original txt file
#'  identify_psd(df_psnu) }

identify_psd <- function(df){

  headers <- names(df)

  type <- dplyr::case_when(
    "expenditure_amt" %in% headers                    ~ "FSD (PSNU_IM)",
    "cadre" %in% headers                              ~ "HRH SD (SITE_IM)",
    "facility" %in% headers                           ~ "MSD (SITE_IM)",
    "qtr4" %in% headers & !"qtr1" %in% headers        ~ "MSD (NAT_SUBNAT)",
    "indicator" %in% headers & !"psnu" %in% headers   ~ "MSD (OU_IM)",
    "indicator" %in% headers &  "psnu" %in% headers   ~ "MSD (PSNU_IM)",
    TRUE                                              ~ "Unknown")

  if("approvallevel" %in% headers)
    type <- sub("MSD", "Genie", type)

  return(type)
}
