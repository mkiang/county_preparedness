## 03_pull_empower_data.R ----
## 
## Note that the data are public, but we pull this from a private server. We
## share the processed data here but THIS CODE WILL NOT RUN WITHOUT ACCSS TO
## OUR PRIVATE SERVER. We do share the data that results from this code. 

## Imports ----
library(RPostgreSQL)
library(RPostgres)
library(DBI)
library(tidyverse)
library(here)
library(fs)

## Constants ----
POSTGRES_TABLE <- "empower.counties_202003"

## Pull if we don't already have the table ----
if (!fs::file_exists(here::here("data", "empower_data.csv"))) {
    ## This file will not upload to github
    source(here::here("code", "secrets.R"))
    
    ## Connect to the default postgres database
    con <- RPostgres::dbConnect(
        RPostgres::Postgres(),
        dbname = hdsi_database,
        host = hdsi_host,
        user = hdsi_user,
        password = hdsi_password
    )
    
    empower_df <-
        RPostgres::dbGetQuery(
            con,
            sprintf(
                "SELECT medicare_benes, power_dependent_devices_dme, state, county, fips_code
FROM %s;",
                POSTGRES_TABLE
            )
        )
    
    empower_df <- empower_df %>%
        dplyr::rename(
            empower_medicare_bene = medicare_benes,
            empower_power_depend = power_dependent_devices_dme,
            abbrev = state,
            fips = fips_code
        )
    
    readr::write_csv(empower_df,
                     here::here("data", "empower_data.csv"))
    
    RPostgres::dbDisconnect(con)
}
