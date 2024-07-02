# gophr 4.1
* Make `achv_desc`, the new output of `adorn_achievement` an ordered factor for ordering purposes [2024-07-02]
* Update `adorn_achievement` to map directly to the new achievement colors and add a parameter to allow users to use the older version, `classic = TRUE` [2024-07-01]
* Adds a new column to the dataframe as a result of `adorn_achievment`, `achv_desc` which is the legend description - 'Concerned', 'At Risk', 'On Target', or 'Above Target' [2024-07-01]

# gophr 4.0
* Add handing to `get_metadata` when working with country files on PEPFAR Posit Workbnech (AWS) [2024-06-03]
* Adjust `get_metadata` to allow it to work with PDAP Wave frozen files [2024-04-30] 
* Add vignette on how to access data in PDAP [2024-04-15]
* Change the delimiter for PDAP txt files as they are separated with a character delimiter (`'|'`) instead of a tab (`'\t'`) [2024-04-15]
* Resolve bug with `get_metadata` when working on PDAP when an explicit path is provided [2024-04-15]
* Resolve bug with `read_psd`, missing second condition about location in an if statement about location [2024-04-09]
* Improve `read_psd` to handle local paths as well as s3 paths to work on PEPFAR Posit Workbench [2024-04-02]
* Align handling of Frozen Genie for `get_metadata` with other file formats [2024-03-29]
* Add handling to `get_metdata` for file names on PEPFAR Posit Workbench that include "Recent" instead of the FYs, DATIM close, and version [2024-03-28] 
* Soft depricate `source_info` as thsi can be pulled from `get_metadata()$source` [2024-03-08]
* Remove outdated parameters from `read_psd` and allow to export as .rds or .parquet [2024-03-08]
* Update `read_psd` to handle .parquet files which are stored on PEPFAR Posit Workbench [2024-03-08]
* Fully depricate `read_msd` [2024-03-28]

# gophr 3.3
* Clean up dependencies list [2024-03-20]
* Add additional check to PSD file parsing with `get_metadata` to ensure the naming is correct, namely an ISO date is included in the file name [2024-03-20]
* Breaking change - change return with `get_metadata` exporting a global object to returning a normal object a user must name [2024-03-20]

# gophr 3.2
* Update `get_metadata` to handle the new fiscal year introduce in FY24Q1i [2024-02-26]
* Change instructions to install from rOpenSci [2024-01-04]
* Resolve issue with NAs being introduced for `targeted_beneficiaries` in FSD [2023-10-03]
* Add new function to return the PSD type based on the dataframe names [2023-09-28]
* Setup proper handling of trainging dataset for `get_metadata` [2023-07-18]
* Adjust FY default searches to reflect new start year of MSDs (FY21) [2023-02-23]
* Add `apply_funding_type` for use with FSD or HRH SD to distiguish between service delivery and non-service delivery
* Replace `read_msd` with `read_psd` to be more inclusive of all of PEFAR's structured datasets [2023-02-07]
* Allow `read_msd` to read in HRH structured dataset [2023-02-07]
* Update `metadata` family functions to get source info for HRH datasets [2023-02-07]

# gophr 3.1
* Add new function to store metadata in a list object (`get_metadata`), rather than returning one item at at time via `source_info` [2022-10-12]
* Add additional `return` type for `source_info` of `fiscal_year_label` which returns `FY[XX]` [2022-10-12]
* Resolve misidentified "provisional" status of Daily Genie files downloaded after DATIM close [2022-08-22]
* Migrate MSD/FSD related function from `glamr` to `gophr`: `apply_partner_type`, `browse_knownissues`, `clean_agency`, `clean_column`, `clean_indicator`, `clean_psnu`, `pluck_totals`, `remove_centralsupport`, `remove_mo`, `remove_sch`, `resolve_knownissues`, `source_info`
 [2022-08-05]

# gophr 3.0.4
* setup GitHub Actions to refresh site automatically. (2022-05-24)
* add a list of cascade indicators, `cascade_ind` that can be used to approximate the 90/95s as well as linkage and VLC. (2022-05-24)
* include `TX_CURR_Lag1` and `TX_CURR_Lag2` as part of `snapshot_ind` (2022-05-24)
* update MSD names to reflect new structure as of FY22Q2 in `read_msd` and `rename_official` (2022-04-08)
* allow user to keep old names when importing MSD in `read_msd` in order to allow old code to run without error (2022-04-08)
* clean up back end of `read_msd` to be more efficient (2022-04-08)

# gophr 3.0.3
* allow user to provide DATIM user name and password to `rename_official`, which previously (v3.0.2) look for stored credentials or prompted every time. (2022-03-15)
* update `rename_offical` to include `fundingagency` to work within [`tameDP`](https://usaid-oha-si.github.io/tameDP/). (2022-03-15)

# gophr 3.0.2
* adjust parameters for `reshape_msd`, retiring `clean` which had more to do with aligning with the old, "wide" versions of the MSDs and replacing it with a param, `include_type` to exclude the `period_type` column from the output dataframe. (2022-02-18)
* adjust `rename_official` to loop over operating units and filter query using a floor date if fiscal year or period are included  in the data frame. (2022-01-26)
* update to `rename_official` to remove timeout (due to large file size) and prompt for password if DATIM credentials are not stored via `glamr::set_datim`. (2022-01-24)

# gophr 3.0.1
* removes the inclusion of future fiscal year results in `reshape_msd` (2021-10-07)
* check mechanism table in DATIM when using `rename_official` to see if credentials
are needed and provides them if stored in `keyring`
* update `read_msd` naming for Genie outputs since the datasets now go through FY22 as of FY21Q3c
* resolve bug causing issues with `results_cumulative` in `reshape_msd` resulting when there were cumulative indicators collected semi-annually

# gophr 3.0.0
* clone ICPIutilities absorb under USAID-OHA-SI org
* remove all depricated functions
* replace README with USAID-OHA-SI standard

# ICPIutilities 2.2.2
* resolve bug with `reshape_msd` causing issues with period naming and dropping of periods
* add `adorn_achievement` to apply achieviement labeling and color; works with normal MSD structure and if reshaping using `reshape_msd`

# ICPIutilities 2.2.1
* add a new function, `calc_achievement` for cumulative and quarterly target achievement calculations
* update naming of `primepartner` from FSD and remove a tab quote from `cop_budget_pipeline` amount that was throwing a warning after convering to a double
* changes the naming convention on the backend for `rename_official` to avoid inadvertently dropping/changing variables due to suffix pattern
* fill Q3 `results_cumulative` for semi-annual indicators when using `reshape_msd(direction = "quarter")`

# ICPIutilities 2.2.0
* updates to `read_msd` to handle two additional reshapes - semi-wide and quarters (for quarterly target achievement)
* clean up bug from converting country in FSD to countryname in `read_msd`, which was causing the variable to be renamed countrynamename 
* update `identify_pd` to provide periods in the same format as `reshape_msd` and decomission the use of targets as a parameter

# ICPIutilities 2.1.8
* require `dplyr v1.0.0` or later to handle some of the code improvements
* update `read_msd` to handle changes to the variable naming with FSD and align with MSD
* change the `reshape_msd` `val` column to be called `value` (warning message added)
* change the default parameter in `reshape_msd` from `clean = TRUE` 
* move from travis.ci to GitHub Actions for CI

# ICPIutilities 2.1.6
* change defaults in `read_msd` to not save as an rds and not delete original txt file
* fix potential bug with `read_msd` rds output filename
* allow `read_msd` to read in rds if its already created
* added backwards comptability to handle old/wide format of MSD prior to FY19Q1

# ICPIutilities 2.1.5
* change default in `reshape_msd` to be long and added a parameter to have a cleaner period output, `clean = TRUE`

# ICPIutilities 2.1.4
* update `read_msd` to not try to delete file if providing a URL
* removed award information from `mech_name` in `rename_official`
* adjust `read_msd` to handle variant of NAT_SUBNAT MSD structure for importing

# ICPIutilities 2.1.3
* faster imports by using `vroom`
* update `read_msd` to handle only lower case variable in the MSD/Genie starting in FY19Q4i
* DEPRECATED: `match_msd` since it is covered by `read_msd`

# ICPIutilities 2.1.2
* allow `read_msd` to import the NAT_SUBNAT dataset

# ICPIutilities 2.1.1
* with the FY19Q3i release, adjusted all functions to work with the new columns and adjusted column names

# ICPIutilities 2.1.0
* add new function, `calc_genpop`, which create a new disaggregate to breakout general population from key populations

# ICPIutilities 2.0.3
* adjust `reshape_msd` to allow to work with naitive camel case variable names in MSD

# ICPIutilities 2.0.2
* fixed bug in `read_msd` that didn't recognize Genie files which didn't convert the targets, quarters or cumulative to numeric columns.

# ICPIutilities 2.0.1

* `identify_pd` updated to work with the new dataset structure
* `reshape_msd` function reshapes the current structure to fully long or to match the previous MSD's wider format
* `read_msd`
  - updated to work for semiwide format
  - `Fiscal_Year` treated as integer
  - Users can now enter a zipped filepath into `file` and `read_msd` will extract the flat file and import it
  - Compatible with the ER dataset
* `add_cumulative`
  - Update semi-annual indicator list to reflect MER 2.3 indicator changes
  - Removed adjustment for FY17 OVC APR
  - DEPRECATED: MSD's new structure includes cumulative natively
*`rename_official` cleaned up code, using `curl` to check internet connection
  
# ICPIutilities 1.0.25

* Fix bug with `read_msd` where columns with now values would be converted to string. Important update for `match_msd` where this may occur.
* When no connection is available, `rename_official` will print out a warning rather than result in an error, halting the rest of the script execution.

# ICPIutilities 1.0.24

* Remove creation of FY17 APR column in `match_msd` as it is now included in the Genie (as of Oct 24, 2018)
* Resolve grouping & duplication bug with `add_cumulative`. Now it aggregates before adding a cumulative value in. 

# ICPIutilities 1.0.23

* Allow user to convert site level Genie output to match MSD via the `match_msd` function
* Fixes bug for `identifypd`, where if `pd_type == "year"` and `prior_pd = TRUE` returned current year

# ICPIutilities 1.0.22

* Adjusted the FY17 APR OVC_SERV values created with `add_cumulative()` to be correct (ie using with Genie) (#36)
* Fixed bug in `add_cumulative` that broke code if variables were upper case

# ICPIutilities 1.0.21

* All RDS files are now saved (and work off of) all lower case file extensions `.rds` instead of `.Rds` (#32)
* Added a `NEWS.md` file to track changes to the package.



