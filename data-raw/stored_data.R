#Snapshot indicator list - MER 2.5
#https://datim.zendesk.com/hc/en-us/articles/360000084446-MER-Indicator-Reference-Guides
snapshot_ind <- c("AGYW_PREV",
                  "AGYW_PREV_D",
                  "OVC_SERV",
                  "PrEP_CURR",
                  "OVC_HIVSTAT",
                  "TX_CURR",
                  "TX_CURR_Lag1",
                  "TX_CURR_Lag2",
                  "TX_ML",
                  "TX_TB_D", #only TX_TB denom, not num
                  "TX_PVLS",
                  "TX_PVLS_D",
                  "SC_CURR")

usethis::use_data(snapshot_ind, overwrite = TRUE)

#Clinical Cascade Indicators
cascade_ind <- c("HTS_TST",
                 "HTS_TST_POS",
                 "TX_NEW",
                 "TX_NET_NEW",
                 "TX_CURR",
                 "TX_PVLS_D",
                 "TX_PVLS",
                 "TX_CURR_Lag2")

usethis::use_data(cascade_ind, overwrite = TRUE)


## PEPFAR country list

library(tidyverse)
library(glamr)

curr_fy <- source_info(return = "fiscal_year")

df_msd <- si_path() %>%
  return_latest("OU_IM") %>%
  read_msd()

lst_ous <- df_msd %>%
  filter(fiscal_year == curr_fy,
         funding_agency %ni% c("Dedup", "Default"),
         indicator != "TX_NET_NEW") %>%
  distinct(operatingunit) %>%
  arrange(operatingunit) %>%
  pull()


usethis::use_data(lst_ous, overwrite = TRUE, internal = TRUE)
