## Create a data dictionary ----
##
## We get data from a bunch of difference sources. We want to create a table
## that keeps track of all the sources. Each row should be a variable and
## the columns should be var_original (original variable name), var_label (
## original label on the variable, if applicable), var_notes_source (original
## notes associated with the variable, if applicable), and var_name, the name
## of the variable used in our analytic dataframe.

## Imports ----
library(tidyverse)
library(here)

## AHRF variables ----
ahrf_layout <- readRDS(here::here("data", "ahrf_2018_layout.RDS"))
source(here::here("code", "ahrf_variables.R"))
flipped_list <- names(satchits_list)
names(flipped_list) <- unname(satchits_list)

ahrf_vars <- ahrf_layout %>%
    dplyr::filter(field %in% unname(satchits_list)) %>%
    dplyr::rename(var_original = field) %>%
    dplyr::mutate(
        var_notes_source = sprintf("%s (%s)",
                                   characteristics,
                                   source),
        var_source = "Area Health Resources Files"
    ) %>%
    dplyr::select(dplyr::starts_with("var_"))

ahrf_vars$var_name <- unname(flipped_list[ahrf_vars$var_original])

## Population files ----
population_vars <- dplyr::tibble(
    var_original = NA,
    var_label = sprintf("2018 Population in age group %s-%s",
                        c(seq(0, 80, 5), 85),
                        c(seq(4, 84, 5), Inf)),
    var_notes_source = "NCHS Vintage 2018",
    var_name = sprintf("age%i", seq(0, 85, 5)),
    var_source = "National Center for Health Statistics"
)

## emPOWER data ----
empower_vars <- dplyr::tibble(
    var_original = c("empower_medicare_bene",
                     "empower_power_depend"),
    var_label = c(
        "Medicare Beneficiaries",
        "Power Dependent Medicare Beneficiaries"
    ),
    var_notes_source = "emPOWER February 2020",
    var_name = c("medicare_benes",
                 "power_dependent_devices_dme"),
    var_source = "US Dept. of Health and Human Services emPOWER data"
)

## Population density ----
popdensity_vars <-
    dplyr::tibble(
        var_original = c(
            "density_per_square_mile_of_land_area_population",
            "density_per_square_mile_of_land_area_housing_units"
        ),
        var_label = c(
            "Population per square mile of land area",
            "Housing units per square mile of land area"
        ),
        var_notes_source = c("2010 Census SF1"),
        var_name = c("pop_per_sq_mile",
                     "housing_per_sq_mile"),
        var_source = c("US Census Bureau")
    )

## CDC atlas ----
cdc_atlas_vars <-
    read_csv(here::here("data", "CDC_atlas_risk_factors_dictionary.csv")) %>%
    mutate(var_original = NA) %>%
    select(-link_data) %>%
    filter(!is.na(var_name)) %>%
    mutate(var_source = "Centers for Disease Control and Prevention")

## ACS variables from DP02 table ----
acs_var_labels <-
    read_csv(
        here(
            "data_raw",
            "ACSDP5Y2018.DP02_2020-03-19T121843",
            "ACSDP5Y2018.DP02_metadata_2020-03-19T121741.csv"
        ),
        col_names = c("variable", "label")
    )
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

acs_vars <- tibble(
    var_name = names(dp02_to_keep),
    var_original = unname(dp02_to_keep),
    var_label = acs_var_labels %>%
        filter(variable %in% unname(dp02_to_keep)) %>%
        pull(label),
    var_notes_source = "ACS 5-year 2018 Table DP02",
    var_source = "US Census Bureau"
)

## RWJF variables ----
rwjf_vars <- tibble(
    var_name = c(
        "fips",
        "premature_mort",
        "severe_housing_problems",
        "severe_housing_cost_burden"
    ),
    var_original = c("fipscode",
                     "v001_rawvalue",
                     "v136_rawvalue",
                     "v154_rawvalue"),
    var_label = c(
        "5-digit FIPS Code",
        "Premature death raw value",
        "Severe housing problems raw value",
        "Severe housing cost burden raw value"
    ),
    var_notes_source = c(
        "RWJF County Health Rankings",
        "RWJF County Health Rankings via NCHS 2016-2018 NVSS data",
        "RWJF County Health Rankings via USHUD Comprehensive Housing Affordability Strategy data (2012-2016)",
        "RWJF County Health Rankings via ACS 2014-2018 — Percentage of households that spend more than 50% of their income on housing"
    ),
    var_source = "Robert Wood Johnson Foundation"
)

## Gini coefficient from ACS 2018 ----
gini_var <- tibble(
    var_name = "gini_coef",
    var_original = "B19083_001E",
    var_label = "Estimate!!Gini Index",
    var_notes_source = "ACS 5-year 2018",
    var_source = "US Census Bureau"
)

## Renter data from ACS 2018 ----
renter_var <- tibble(
    var_name = "p_hh_renter",
    var_original = "S1101_C01_020E",
    var_label = "Estimate!!Total!!Total households!!HOUSING TENURE!!Renter-occupied housing units",
    var_notes_source = "ACS 5-year 2018 Table S1101",
    var_source = "US Census Bureau"
)

## Group quarters from ACS 2018 ----
gq_var <- tibble(
    var_name = "p_group_quarters",
    var_original = "pctGQ",
    var_label = "Percent of the population living in group quarters",
    var_notes_source = "ACS 5-year 2018",
    var_source = "US Census Bureau"
)

## Diversity data for kids ----
ddkids_vars <- tibble(
    var_name = c("p_crowded_housing", "high_housing_cost"),
    var_original = c("total_est", "total_est"),
    var_notes_source = c(
        "Diversity Data Kids (25014_1_P_050_5_crowded_housing_race)",
        "Diversity Data Kids (25106_1_C_050_5_hh_high_housing_costs)"
    ),
    var_label = c(
        "The number of occupied housing units with more than one occupant per room divided by the number of occupied housing units, times 100, for the total population and by race/ethnicity.",
        "The share of children living in households where more than 30 percent of the monthly income was spent on rent, mortgage payments, taxes, insurance, and/or related expenses."
    ),
    var_source = "Diversity Data for Kids"
)


## Combine ----
data_dictionary <-
    dplyr::bind_rows(
        population_vars,
        empower_vars,
        popdensity_vars,
        ahrf_vars,
        cdc_atlas_vars,
        acs_vars,
        rwjf_vars,
        gini_var,
        renter_var,
        gq_var,
        ddkids_vars
    ) %>%
    dplyr::select(var_name,
                  var_source,
                  var_label,
                  var_notes_source,
                  var_original)

## Save ----
readr::write_csv(data_dictionary,
                 here::here("data", "data_dictionary.csv"))
