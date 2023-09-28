test_that("correct psd", {

  #pull headers from each df and convert to vector
  # glamr::si_path() %>%
  #   glamr::return_latest("Genie") %>%
  #   vroom::vroom(n_max = 0, col_types = readr::cols(.default = "c")) %>%
  #   names() %>%
  #   datapasta::vector_paste()

  #dataset names as of FY23Q3c
  genie_siteim_hdr <- c("orgunituid", "sitename", "operatingunit", "operatingunituid", "country", "snu1", "snu1uid", "snu2", "snu2uid", "psnu", "psnuuid", "snuprioritization", "typemilitary", "dreams", "prime_partner_name", "funding_agency", "mech_code", "mech_name", "prime_partner_duns", "prime_partner_uei", "is_indigenous_prime_partner", "award_number", "communityuid", "community", "facilityuid", "facility", "sitetype", "dataelementuid", "indicator", "numeratordenom", "indicatortype", "standardizeddisaggregate", "categoryoptioncombouid", "categoryoptioncomboname", "use_for_age", "ageasentered", "age_2018", "age_2019", "trendscoarse", "sex", "statushiv", "statustb", "statuscx", "hiv_treatment_status", "otherdisaggregate", "otherdisaggregate_sub", "modality", "fiscal_year", "targets", "qtr1", "qtr2", "qtr3", "qtr4", "cumulative", "source_name", "approvallevel", "approvalleveldescription")
  psnuim_hdr <- c("operatingunit", "operatingunituid", "country", "snu1", "snu1uid", "psnu", "psnuuid", "snuprioritization", "typemilitary", "dreams", "prime_partner_name", "funding_agency", "mech_code", "mech_name", "prime_partner_duns", "prime_partner_uei", "is_indigenous_prime_partner", "award_number", "indicator", "numeratordenom", "indicatortype", "standardizeddisaggregate", "categoryoptioncomboname", "use_for_age", "ageasentered", "age_2018", "age_2019", "trendscoarse", "sex", "statushiv", "statustb", "statuscx", "hiv_treatment_status", "otherdisaggregate", "otherdisaggregate_sub", "modality", "fiscal_year", "targets", "qtr1", "qtr2", "qtr3", "qtr4", "cumulative", "source_name")
  ouim_hdr <- c("operatingunit", "operatingunituid", "country", "typemilitary", "prime_partner_name", "funding_agency", "mech_code", "mech_name", "prime_partner_duns", "prime_partner_uei", "is_indigenous_prime_partner", "award_number", "indicator", "numeratordenom", "indicatortype", "standardizeddisaggregate", "categoryoptioncomboname", "use_for_age", "ageasentered", "age_2018", "age_2019", "trendscoarse", "sex", "statushiv", "statustb", "statuscx", "hiv_treatment_status", "otherdisaggregate", "otherdisaggregate_sub", "modality", "fiscal_year", "targets", "qtr1", "qtr2", "qtr3", "qtr4", "cumulative", "source_name")
  subnat_hdr <- c("operatingunit", "operatingunituid", "country", "snu1", "snu1uid", "psnu", "psnuuid", "snuprioritization", "indicator", "numeratordenom", "indicatortype", "standardizeddisaggregate", "categoryoptioncomboname", "use_for_age", "ageasentered", "age_2018", "age_2019", "trendscoarse", "sex", "statushiv", "otherdisaggregate", "fiscal_year", "targets", "qtr4", "source_name")
  hrh_hdr <- c("orgunituid", "sitename", "operatingunituid", "operating_unit", "country", "snu1uid", "snu1", "psnuuid", "psnu", "communityuid", "community", "facilityuid", "facility", "funding_agency", "mech_code", "mech_name", "award_number", "prime_partner_name", "prime_partner_uei", "employment_title", "er_category", "cadre", "site_level", "program", "sub_program", "interaction_type", "beneficiary", "sub_beneficiary", "gender", "prime_or_sub", "subrecipient_name", "subrecipient_uei", "mode_of_hiring", "roving", "work_location", "is_covid_support", "moh-secondment", "is_outside_ou", "is_tech_assist", "is_community_primarily", "comments", "fiscal_year", "avg_fte_per_month", "months_of_work", "individual_count", "annual_fte")
  fsd_hdr <- c("operatingunit", "country", "fundingagency", "prime_partner_name", "prime_partner_duns", "prime_partner_uei", "prime_partner_org_type", "is_indigenous_prime_partner", "procurement_type", "subrecipient_name", "subrecipient_duns", "subrecipient_uei", "award_number", "mech_code", "mech_name", "record_type", "program", "sub_program", "interaction_type", "targeted_beneficiary", "allocated_beneficiary", "cost_category", "sub_cost_category", "funding_account", "planning_cycle", "implementation_year", "cop_budget_new_funding", "cop_budget_pipeline", "cop_budget_total", "workplan_budget_amt", "expenditure_amt")
  bad_hdr <- c("this", "is", "wrong")

  #fcn for converting names to a dummy df to run through `identify_psd`
  conv_df <- function(v){
    tibble::tibble(name = {v},
                   value = rep(1, length({v}))) %>%
      tidyr::pivot_wider()
  }

  expect_equal(fsd_hdr %>% conv_df() %>%  identify_psd(), "FSD (PSNU_IM)")
  expect_equal(hrh_hdr %>% conv_df() %>%  identify_psd(), "HRH SD (SITE_IM)")
  expect_equal(subnat_hdr %>% conv_df() %>%  identify_psd(), "MSD (NAT_SUBNAT)")
  expect_equal(ouim_hdr %>% conv_df() %>%  identify_psd(), "MSD (OU_IM)")
  expect_equal(psnuim_hdr %>% conv_df() %>%  identify_psd(), "MSD (PSNU_IM)")
  expect_equal(genie_siteim_hdr %>% conv_df() %>%  identify_psd(), "Genie (SITE_IM)")
  expect_equal(bad_hdr %>% conv_df() %>%  identify_psd(), "Unknown")
})


