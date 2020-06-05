## 04_create_analytic_data.R ----
## 
## Combine all the different data sets into the columns of interest in
## shapes we want to use later (wide and long).

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "ahrf_variables.R"))

## Constants ----
WORKING_DATA <- "data"
dp02_to_keep <-  c(
    geoid = "GEO_ID",
    "n_households" = "DP02_0001E",
    "avg_hh_size" = "DP02_0015E",
    "n_65_living_alone" = "DP02_0012E",
    "p_65_living_alone" = "DP02_0012PE",
    "n_hh_grandparents_with_under18" = "DP02_0043E",
    "p_hh_grandparents_with_under18" = "DP02_0043PE",
    "n_hh_with_computer" = "DP02_0151E",
    "p_hh_with_computer" = "DP02_0151PE",
    "n_hh_with_broadband" = "DP02_0152E",
    "p_hh_with_broadband" = "DP02_0152PE",
    "n_usa_native" = "DP02_0087E",
    "p_usa_native" = "DP02_0087PE"
)

## Data ----
pop_full <-
    readr::read_csv(here::here(WORKING_DATA, "population_by_age.csv"))
ahrf_full <- readRDS(here::here(WORKING_DATA, "ahrf_2018.RDS"))
empower_full <-
    readr::read_csv(here::here(WORKING_DATA, "empower_data.csv"))
pop_density <-
    readr::read_csv(here::here(WORKING_DATA, "population_density.csv"))
cdc_atlas <-
    read_csv(here(WORKING_DATA, "CDC_atlas_risk_factors.csv"), na = "-1")
acs_vars <- read_csv(
    here(
        "data_raw",
        "ACSDP5Y2018.DP02_2020-03-19T121843",
        "ACSDP5Y2018.DP02_data_with_overlays_2020-03-19T121741.csv"
    ),
)
rwjf_df <-
    read_csv(here(
        "data_raw",
        "RWJF_CHR_2019",
        "RWJF_CHR_analytic_data2019.csv"
    ),
    skip = 1) %>%
    filter(!is.na(county_ranked))
gini_df <- read_csv(
    here(
        "data_raw", 
        "ACSDT5Y2018.B19083_2020-03-20T172559", 
        "ACSDT5Y2018.B19083_data_with_overlays_2020-03-20T172555.csv"
    )
)
renter_df <- read_csv(
    here(
        "data_raw", 
        "ACSST5Y2018.S1101_2020-03-19T180025", 
        "ACSST5Y2018.S1101_data_with_overlays_2020-03-19T180018.csv"
    )
)
ice_df <- read_csv(
    here(
        "data_raw", 
        "USCountyABSM_2018.csv"
    )
)
gq_df <- read_csv(
    here(
        "data_raw", 
        "2018_ACS5_GQ_County.csv"
    )
)
nonwhite_df <- read_csv(
    here(
        "data",
        "percent_nonwhite_pop.csv"
    )
)
ddkids_crowded <-
    read_csv(
        here(
            "data_raw",
            "diversitydatakids",
            "25014_1_P_050_5_crowded_housing_race.csv"
        )
    )
ddkids_highcost <-
    read_csv(
        here(
            "data_raw",
            "diversitydatakids",
            "25106_1_C_050_5_hh_high_housing_costs.csv"
        )
    )

## Subset area health resource file ----
ahrf_subset <- ahrf_full %>%
    dplyr::select(satchits_list) %>%
    dplyr::mutate(fips = paste0(fips_st, fips_ct)) %>%
    dplyr::select(-fips_st, -fips_ct) %>%
    dplyr::group_by(fips, name, census_region_name) %>%
    dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), function(x)
        as.numeric(x)) %>%
    dplyr::ungroup() %>%
    dplyr::select(fips,
                  name,
                  census_region_code,
                  census_region_name,
                  dplyr::everything())

## Add total medsurg/cardiac/other ICU beds ----
ahrf_subset <- ahrf_subset %>%
    dplyr::mutate(
        n_medsurg_card_other_icu_beds_2017 =
            n_medsurg_icu_beds_2017 +
            n_cardiac_icu_beds_2017 +
            n_other_icu_beds_2017
    )

## Add nursing home beds ----
ahrf_subset <- ahrf_subset %>%
    dplyr::mutate(n_nursing_home_beds_2017 =
                      n_hosp_nursing_home_beds_2017 +
                      n_fac_nursing_home_beds_2018)

## ACS variables ----
acs_vars <- acs_vars %>%
    slice(-1) %>%
    select(all_of(dp02_to_keep)) %>%
    mutate(fips = substr(geoid, nchar(geoid) - 4, nchar(geoid))) %>%
    select(-geoid) %>%
    filter(avg_hh_size != "null",
           fips != "000US") %>%
    group_by(fips) %>%
    mutate_at(vars(-group_cols()), as.numeric) %>%
    ungroup() %>%
    mutate(p_hh_grandparents_with_under18 = p_hh_grandparents_with_under18 /
               n_households * 100)

## Gini coefficient ----
gini_df <- gini_df %>%
    slice(-1) %>%
    select(geoid = GEO_ID,
           gini_coef = B19083_001E) %>%
    mutate(fips = substr(geoid, nchar(geoid) - 4, nchar(geoid))) %>%
    select(-geoid) %>%
    filter(gini_coef != "null",
           fips != "000US") %>%
    mutate(gini_coef = as.numeric(gini_coef))

## Renters ----
renter_df <- renter_df %>% 
    slice(-1) %>% 
    select(geoid = GEO_ID, 
           p_hh_renter = S1101_C01_020E) %>%
    mutate(fips = substr(geoid, nchar(geoid) - 4, nchar(geoid))) %>%
    select(-geoid) %>%
    filter(p_hh_renter != "null",
           fips != "000US") %>%
    mutate(p_hh_renter = as.numeric(p_hh_renter))

## ICE ----
ice_df <- ice_df %>% 
    select(fips = GEOID, 
           ice_wb_income = ICEwbinc)

## Group quarters ----
gq_df <- gq_df %>% 
    select(fips = FIPS, 
           p_group_quarters = pctGQ)

## Reshape population ----
pop_wide <- pop_full %>%
    tidyr::spread(age, pop, sep = "")

## Create skeleton using population counts and density ----
data_wide <- pop_wide %>%
    dplyr::left_join(pop_density %>% dplyr::select(-name))

## Join with AHRF subset variables ----
data_wide <- data_wide %>%
    dplyr::left_join(ahrf_subset)

## Create 2018 population count ----
data_wide <- data_wide %>%
    mutate(
        n_pop_2018 = age0 + age5 + age10 + age15 + age20 +
            age25 + age30 + age35 + age40 + age45 + age50 +
            age55 + age60 + age65 + age70 + age75 + age80 +
            age85
    )

## Non-white population (percentage) ----
data_wide <- data_wide %>% 
    left_join(nonwhite_df)

## Join in the CDC Atlas data ----
data_wide <- data_wide %>%
    left_join(cdc_atlas %>%
                  mutate(fips = sprintf("%05d", cnty_fips)) %>%
                  select(-cnty_fips, -display_name))

## Join with ACS ----
data_wide <- data_wide %>%
    left_join(acs_vars)

## Join with Gini coef ----
data_wide <- data_wide %>% 
    left_join(gini_df)

## Join with Renter ----
data_wide <- data_wide %>% 
    left_join(renter_df)

## Join with ICE ----
data_wide <- data_wide %>% 
    left_join(ice_df)

## Join with GQ ----
data_wide <- data_wide %>% 
    left_join(gq_df)

## Join with RWJF ----
data_wide <- data_wide %>% 
    left_join(
        rwjf_df %>%
            select(
                fips = fipscode,
                premature_mort = v001_rawvalue,
                severe_housing_problems = v136_rawvalue,
                severe_housing_cost_burden = v154_rawvalue
            )
    )

## Join with DDKids Crowded Housing ----
data_wide <- data_wide %>% 
    left_join(
       ddkids_crowded %>%
            group_by(geoid) %>%
            filter(year == max(year)) %>%
            ungroup() %>%
            transmute(fips = substr(geoid, nchar(geoid) - 4, nchar(geoid)),
                      p_crowded_housing = total_est) 
    )

## Join with DDKids High Housing Cost ----
data_wide <- data_wide %>% 
    left_join(
        ddkids_highcost %>%
            group_by(geoid) %>%
            filter(year == max(year)) %>%
            ungroup() %>%
            transmute(fips = substr(geoid, nchar(geoid) - 4, nchar(geoid)),
                      high_housing_cost = total_est) 
    )

## Join with emPOWER ----
## NOTE: We should join empower last so the reshape code stays the same
data_wide <- data_wide %>%
    dplyr::left_join(
        empower_full %>%
            dplyr::select(fips,
                          empower_medicare_bene,
                          empower_power_depend,
                          abbrev)
    ) %>%
    dplyr::select(abbrev, name, fips, census_region_name, dplyr::everything())

## Reshape from wide to long ----
data_long <- data_wide %>%
    tidyr::gather(variable, value, age0:empower_power_depend)

## Save ----
readr::write_csv(data_wide,
                 here::here(WORKING_DATA, "analytic_data_wide.csv"))
readr::write_csv(data_long,
                 here::here(WORKING_DATA, "analytic_data_long.csv"))
