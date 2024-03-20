# test_that("matches PSD naming convention - SD", {
#   # list.files(glamr::si_path(), "zip") %>% clipr::write_clip()
#   # datapasta::vector_paste_vertical()
#
#   names <- c(
#     "Financial_Structured_Datasets_COP17-23_20240215.zip",
#     "MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20240215_v1_1.zip",
#     "MER_Structured_Datasets_NAT_SUBNAT_FY22-24_20240215_v1_1.zip",
#     "MER_Structured_Datasets_OU_IM_FY15-21_20240215_v1_1.zip",
#     "MER_Structured_Datasets_OU_IM_FY22-24_20240215_v1_1.zip",
#     "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1.zip",
#     "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1_Mozambique.zip",
#     "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1_Tanzania.zip",
#     "MER_Structured_TRAINING_Datasets_PSNU_IM_FY58-61_20231114_v1_1.zip",
#     "MER_Structured_TRAINING_Datasets_PSNU_IM_FY59-61_20240215_v1_1.zip",
#     "Genie-SiteByIMs-Western-Hemisphere-Region-Daily-2024-02-22.zip",
#     "Genie-OUByIMs-Tanzania-Frozen-2024-02-16.zip",
#     "MSD_OU_IM_test.zip"
#   )
#
#   expect_setequal(grepl("Structured_.*Datasets",names),
#                   c(rep(TRUE, 10), rep(FALSE, 3))
#                   )
#
# })


test_that("filename has standard fy and date", {
  # list.files(glamr::si_path(), "zip") %>% clipr::write_clip()
  # datapasta::vector_paste_vertical()

  names <- c(
    "Financial_Structured_Datasets_COP17-23_20240215.zip",
    "MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20240215_v1_1.zip",
    "MER_Structured_Datasets_NAT_SUBNAT_FY22-24_20240215_v1_1.zip",
    "MER_Structured_Datasets_OU_IM_FY15-21_20240215_v1_1.zip",
    "MER_Structured_Datasets_OU_IM_FY22-24_20240215_v1_1.zip",
    "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1.zip",
    "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1_Mozambique.zip",
    "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1_Tanzania.zip",
    "MER_Structured_TRAINING_Datasets_PSNU_IM_FY58-61_20231114_v1_1.zip",
    "MER_Structured_TRAINING_Datasets_PSNU_IM_FY59-61_20240215_v1_1.zip",
    "Genie-SiteByIMs-Western-Hemisphere-Region-Daily-2024-02-22.zip",
    "Genie-OUByIMs-Tanzania-Frozen-2024-02-16.zip",
    "MSD_OU_IM_test.zip"
  )

  expect_setequal(grepl("(FY|COP)\\d{2}-\\d{2}_\\d{8}",names),
                  c(rep(TRUE, 12), rep(FALSE, 1))
  )

})


test_that("filename has standard date", {
  # list.files(glamr::si_path(), "zip") %>% clipr::write_clip()
  # datapasta::vector_paste_vertical()

  names <- c(
    "Financial_Structured_Datasets_COP17-23_20240215.zip",
    "MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20240215_v1_1.zip",
    "MER_Structured_Datasets_NAT_SUBNAT_FY22-24_20240215_v1_1.zip",
    "MER_Structured_Datasets_OU_IM_FY15-21_20240215_v1_1.zip",
    "MER_Structured_Datasets_OU_IM_FY22-24_20240215_v1_1.zip",
    "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1.zip",
    "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1_Mozambique.zip",
    "MER_Structured_Datasets_PSNU_IM_FY22-24_20240215_v1_1_Tanzania.zip",
    "MER_Structured_TRAINING_Datasets_PSNU_IM_FY58-61_20231114_v1_1.zip",
    "MER_Structured_TRAINING_Datasets_PSNU_IM_FY59-61_20240215_v1_1.zip",
    "Genie-SiteByIMs-Western-Hemisphere-Region-Daily-2024-02-22.zip",
    "Genie-OUByIMs-Tanzania-Frozen-2024-02-16.zip",
    "MSD_OU_IM_test.zip"
  )

  expect_setequal(grepl("\\d{8}|\\d{4}-\\d{2}-\\d{2}",names),
                  c(rep(TRUE, 12), rep(FALSE, 1))
  )

})




