## 02_import_ahrf_data.R ----
##
## Clean up AHRF files and extract the subset of columns we need.
##
## Note that this code is almost entirely from jjchern's great ahrf repo:
##  https://github.com/mkiang/ahrf/blob/master/data-raw/prep_county.R

## Imports ----
library(tidyverse)
library(here)
library(labelled)
library(fs)

## Constants ----
RAW_DATA <- "data_raw"
WORKING_DATA <- "data"

RAW_SRC <- here::here(RAW_DATA,
                      "AHRF_2018-2019",
                      "DATA",
                      "AHRF2019.asc")
DOC_SRC <- here::here(RAW_DATA,
                      "AHRF_2018-2019",
                      "DOC",
                      "AHRF 2018-2019 Technical Documentation.xlsx")

## Check if file is unzipped ----
if (!fs::file_exists(RAW_SRC)) {
    utils::unzip(here::here(RAW_DATA, "AHRF_2018-2019.zip"),
                 exdir = here::here(RAW_DATA))
}

## Get the FWF layout ----
bgn_line <- readxl::read_excel(DOC_SRC) %>%
    dplyr::pull(`...1`) %>%
    grepl("F00001", .) %>%
    which()

## Import the excel as a df of FWF info ----
ahrf_county_layout <- readxl::read_excel(
    DOC_SRC,
    col_names = c(
        "field",
        "col_col",
        "year_of_data",
        "var_label",
        "characteristics",
        "source",
        "date_on"
    ),
    skip = bgn_line
) %>%
    dplyr::filter(grepl("^F[0-9]", field)) %>%
    tidyr::separate(col_col, c("col_start", "col_end")) %>%
    dplyr::mutate_at(c("col_start", "col_end"), as.integer)

## Import the county-level AHRF file ----
ahrf_county <- readr::read_fwf(
    file = RAW_SRC,
    col_positions = readr::fwf_positions(
        start = ahrf_county_layout$col_start,
        end = ahrf_county_layout$col_end,
        col_names = ahrf_county_layout$field
    )
)

labelled::var_label(ahrf_county) <- ahrf_county_layout %>%
    dplyr::select(field, var_label) %>%
    tibble::deframe() %>%
    as.list()

## Extract scaling factor ----
ahrf_county_layout <- ahrf_county_layout %>%
    dplyr::mutate(scaling_factor = stringr::str_extract(characteristics, "\\(.[0-1]{1,2}\\)")) %>%
    dplyr::mutate(scaling_factor = as.numeric(gsub("\\(|\\)", "", scaling_factor)))

## Rescale columns ----
for (s in unique(ahrf_county_layout$scaling_factor)) {
    if (!is.na(s)) {
        ahrf_county <- ahrf_county %>%
            dplyr::mutate_at(dplyr::vars(
                ahrf_county_layout %>%
                    dplyr::filter(scaling_factor == s) %>%
                    dplyr::pull(field)
            ),
            function(x)
                as.numeric(x) * s)
    }
}

## Save ----
saveRDS(ahrf_county_layout,
        here::here(WORKING_DATA, "ahrf_2018_layout.RDS"))
saveRDS(ahrf_county,
        here::here(WORKING_DATA, "ahrf_2018.RDS"),
        compress = "xz")
