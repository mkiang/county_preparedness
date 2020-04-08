## Imports ----
library(tidyverse)
source("./code/utils.R")

## Data ----
analytic_df <- read_csv("./data/analytic_data_wide.csv")
plotting_df <- process_to_plotting_df(analytic_df)

## Fig 1 -- % poverty by % over 70 years ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p70older,
    x_high = 15,
    x_low = 10,
    x_label = "Population over\n70 years old (%)",
    y_var = p_poverty,
    y_high = 20,
    y_low = 5,
    y_label = "Households under\npoverty-line (%)"
)
p1$counts %>% filter(color_hex == "#2a1a8a") %>% pull(n)
plotting_df %>% filter(fips %in% (p1$data %>% filter(color_hex == "#2a1a8a") %>% pull(fips))) %>% pull(n_pop_2018) %>% sum(.)
ggsave(
    "./plots/fig01_bivariate_age_poverty.pdf",
    p1$plot,
    device = cairo_pdf,
    width = 10,
    height = 6,
    scale = 1.1
)
ggsave(
    "./plots/fig01_bivariate_age_poverty.jpg",
    p1$plot,
    dpi = 300,
    width = 10,
    height = 6,
    scale = 1.1
)

## Fig 2 -- CHD/HTN hospitalizations vs 18-64 uninsured ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p_18_64_no_insurance,
    x_high = 25,
    x_low = 10,
    x_label = "Population 18-64\nwithout insurance (%)",
    y_var = chd_htn_hosp,
    y_high = 250,
    y_low = 150,
    y_label = "CHD and HTN\nhospitalization rate"
)
p1$counts %>% filter(color_hex == "#2a1a8a") %>% pull(n)
plotting_df %>% filter(fips %in% (p1$data %>% filter(color_hex == "#2a1a8a") %>% pull(fips))) %>% pull(n_pop_2018) %>% sum(.)
ggsave(
    "./plots/fig02_bivariate_uninsured_comorbidity.pdf",
    p1$plot,
    device = cairo_pdf,
    width = 10,
    height = 6,
    scale = 1.1
)
ggsave(
    "./plots/fig02_bivariate_uninsured_comorbidity.jpg",
    p1$plot,
    dpi = 300,
    width = 10,
    height = 6,
    scale = 1.1
)

## Fig 3a -- Group quarters vs ICU beds ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = icu_per_capita,
    x_high = 25,
    x_low = 10,
    x_label = "ICU beds per\n100,000 population",
    rev_x = TRUE,
    y_var = p_group_quarters,
    y_high = 5,
    y_low = 1,
    y_label = "Population living\nin group quarters (%)",
)
p1$counts %>% filter(color_hex == "#2a1a8a") %>% pull(n)
plotting_df %>% filter(fips %in% (p1$data %>% filter(color_hex == "#2a1a8a") %>% pull(fips))) %>% pull(n_pop_2018) %>% sum(.)
ggsave(
    "./plots/fig03a_bivariate_icu_group_quarters.pdf",
    p1$plot,
    device = cairo_pdf,
    width = 10,
    height = 6,
    scale = 1.1
)
ggsave(
    "./plots/fig03a_bivariate_icu_group_quarters.jpg",
    p1$plot,
    dpi = 300,
    width = 10,
    height = 6,
    scale = 1.1
)

## Fig 3b -- Population density vs hospital beds ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = hosp_per_capita,
    x_high = 100,
    x_low = 100,
    x_label = "Hospital beds per\n100,000 population",
    rev_x = TRUE,
    y_var = pop_density,
    y_high = 100,
    y_low = 10,
    y_label = "Population per\nsquare mile",
)
p1$counts %>% filter(color_hex == "#2a1a8a") %>% pull(n)
ggsave(
    "./plots/fig03b_bivariate_hosp_density.pdf",
    p1$plot,
    device = cairo_pdf,
    width = 10,
    height = 6,
    scale = 1.1
)
ggsave(
    "./plots/fig03b_bivariate_hosp_density.jpg",
    p1$plot,
    dpi = 300,
    width = 10,
    height = 6,
    scale = 1.1
)

## Fig 4 -- Premature mortality and % non-white ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p_nonwhite,
    x_high = 50,
    x_low = 25,
    x_label = "% of population\nnon-Hispanic and non-White",
    rev_x = FALSE,
    y_var = premature_mort,
    y_high = 10000,
    y_low = 7500,
    y_label = "Age-adjusted years of\npotential life lost before age 75",
)
p1$counts %>% filter(color_hex == "#2a1a8a") %>% pull(n)
plotting_df %>% filter(fips %in% (p1$data %>% filter(color_hex == "#2a1a8a") %>% pull(fips))) %>% pull(n_pop_2018) %>% sum(.)
ggsave(
    "./plots/fig04_bivariate_nonwhite_premature_mort.pdf",
    p1$plot,
    device = cairo_pdf,
    width = 10,
    height = 6,
    scale = 1.1
)
ggsave(
    "./plots/fig04_bivariate_nonwhite_premature_mort.jpg",
    p1$plot,
    dpi = 300,
    width = 10,
    height = 6,
    scale = 1.1
)
