## 01_get_population_data.R ----
## 
## Download and reshape the 2018 NCHS bridged race population file. Will also
## incorporate PR population and calculate non-Hispanic and non-White pop.

## Imports ----
library(tidyverse)
library(here)
library(fs)

## Constants ----
RAW_DATA <- "data_raw"
WORKING_DATA <- "data"
POP_FILE <- paste0("https://www.cdc.gov/nchs/nvss/",
                   "bridged_race/pcen_v2018_y18.txt.zip")

## Make dirs
fs::dir_create(here::here(RAW_DATA))
fs::dir_create(here::here(WORKING_DATA))

## 2018 Vintage of Pop Estimates ----
## https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#vintage2018%20
if (!fs::file_exists(here::here(RAW_DATA, basename(POP_FILE)))) {
    utils::download.file(url = POP_FILE,
                         destfile = here::here(RAW_DATA, basename(POP_FILE)))
}

orig_pop_df <-
    readr::read_fwf(here::here(RAW_DATA, basename(POP_FILE)),
                    readr::fwf_widths(
                        c(4, 4, 1, 5, 2, 1, 1, 8),
                        c(
                            "series",
                            "year",
                            "month",
                            "fips",
                            "age",
                            "racesex",
                            "hispanic",
                            "pop"
                        )
                    ))  %>%
    dplyr::filter(year == 2018, month == 7)

pop_df <- orig_pop_df %>%
    dplyr::select(age, fips, pop) %>%
    dplyr::mutate(age = (cut(
        age,
        c(0, seq(5, 85, 5), Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = FALSE
    ) - 1) * 5) %>%
    dplyr::group_by(fips, age) %>%
    dplyr::summarize(pop = sum(pop)) %>%
    dplyr::ungroup()

## Clean up and reshape PR data ----
pr_pop_df <- read_csv(here(
    RAW_DATA,
    "PR_PEP_2018_PEPAGESEX",
    "PEP_2018_PEPAGESEX_with_ann.csv"
)) %>%
    slice(2:n()) %>%
    janitor::clean_names()

pr_sub <- pr_pop_df %>%
    select(fips = geo_id2,
           one_of(c(
               sprintf("est72018sex0_age%sto%s",
                       seq(0, 80, 5),
                       seq(4, 84, 5)),
               "est72018sex0_age85plus"
           )))

pr_sub <- pr_sub %>%
    gather(age_group,
           estimate,
           est72018sex0_age0to4:est72018sex0_age85plus) %>%
    mutate(age_group = gsub("est72018sex0_age|to[0-9]{1,2}\\>|plus\\>", "", age_group)) %>%
    transmute(fips = fips,
              age = as.numeric(age_group),
              pop = as.numeric(estimate)) %>%
    arrange(fips, age)

pop_df <- bind_rows(pop_df,
                    pr_sub)

readr::write_csv(pop_df, here::here(WORKING_DATA, "population_by_age.csv"))

## Get percentage of non-NHW population ----
nhw_df <- orig_pop_df %>%
    dplyr::select(age, fips, pop, racesex, hispanic) %>%
    dplyr::filter(racesex %in% 1:2, hispanic == 1) %>%
    dplyr::mutate(age = (cut(
        age,
        c(0, seq(5, 85, 5), Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = FALSE
    ) - 1) * 5) %>%
    dplyr::group_by(fips, age) %>%
    dplyr::summarize(nhw_pop = sum(pop)) %>%
    dplyr::ungroup()

non_white_perc <- pop_df %>%
    left_join(nhw_df) %>%
    group_by(fips) %>%
    summarize(pop = sum(pop),
              nhw_pop = sum(nhw_pop)) %>%
    transmute(
        fips = fips,
        p_nonwhite = (pop - nhw_pop) / pop * 100,
        p_white = nhw_pop / pop * 100
    )

readr::write_csv(non_white_perc,
                 here::here(WORKING_DATA, "percent_nonwhite_pop.csv"))
