## Create a data dictionary ----
library(tidyverse)
library(here)
source(here::here("code", "ahrf_variables.R"))
flipped_list <- names(satchits_list)
names(flipped_list) <- unname(satchits_list)

ahrf_layout <- readRDS(here::here("data", "ahrf_2018_layout.RDS"))

## CDC atlas
cdc_atlas_vars <-
    read_csv(here::here("data", "CDC_atlas_risk_factors_dictionary.csv")) %>% 
    mutate(var_original = NA) %>% 
    select(-link_data)

## Population file
population_vars <- dplyr::tibble(
    var_original = NA,
    var_label = sprintf("2018 Population in age group %s-%s", 
                        c(seq(0, 80, 5), 85), 
                        c(seq(4, 84, 5), Inf)),
    var_notes_source = "NCHS Vintage 2018",
    var_name = sprintf("age%i", seq(0, 85, 5))
)

## emPOWER file
empower_vars <- dplyr::tibble(
    var_original = c("empower_medicare_bene", 
                     "empower_power_depend"),
    var_label = c(
        "Medicare Beneficiaries",
        "Power Dependent Medicare Beneficiaries"
    ),
    var_notes_source = "emPOWER February 2020",
    var_name = c("medicare_benes", 
                 "power_dependent_devices_dme")
)

## Population density
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
                     "housing_per_sq_mile")
    )

ahrf_vars <- ahrf_layout %>%
    dplyr::filter(field %in% unname(satchits_list)) %>%
    dplyr::rename(var_original = field) %>%
    dplyr::mutate(var_notes_source = sprintf("%s (%s)", 
                                             characteristics, 
                                             source)) %>%
    dplyr::select(dplyr::starts_with("var_"))

ahrf_vars$var_name <-
    unname(flipped_list[ahrf_vars$var_original])

data_dictionary <-
    dplyr::bind_rows(population_vars,
                     empower_vars,
                     popdensity_vars,
                     ahrf_vars,
                     cdc_atlas_vars) %>%
    dplyr::select(var_name, var_label, var_notes_source, var_original)

readr::write_csv(data_dictionary,
                 here::here("data", "data_dictionary.csv"))
