## 06_supplemental_figures.R ----

## Figures for supplemental materials ----
##
##  Figure S1. Percentage of population 70 years or older, 2018
##  Figure S2. Percentage of households living in poverty, 2016
##  Figure S3. Percentage of population living in group quarters, 2018
##  Figure S4. Percentage of population non-Hispanic and non-White, 2018
##  Figure S5. Hospital beds per 100,000 population, 2017
##  Figure S6. Percentage of population 70 years or older, 2018 and 
##      hospital beds per 100,000 population, 2017.
##  Figure S7. Percentage of households living in poverty, 2016 and hospital 
##      beds per 100,000 population, 2017.
##  Figure S8. Percentage of population living in group quarters, 2018 and 
##      hospital beds per 100,000 population, 2017.
##  Figure S9. Percentage of population that is non-Hispanic and non-White, 
##      2018 and hospital beds per 100,000 population, 2017.
##  Figure S10. Age-adjusted percent of adults over 20 years old who are
##       diagnosed with diabetes, 2016 
##  Figure S11. Coronary heart disease-related hospitalizations per 1,000
##       Medicare beneficiaries (65+), 2014-2016
##  Figure S12. Hypertension hospitalization rate per 1,000 Medicare 
##      beneficiaries (65+), 2014-2016 
##  Figure S13. Average household size, 2018
##  Figure S14. Percentage of households with 65+ year old living alone, 2018
##  Figure S15. Percentage of households with grandparents living with 
##      their own grandchildren (<18 years old), 2018 
##  Figure S16. Percentage of renter-occupied households, 2018 
##  Figure S17. Population estimates (log 10), 2018
##  Figure S18. Population per square mile of land mass (log 10), 2010
##  Figure S19. Percentage of households without broadband internet, 2018
##  Figure S20. Percentage of 18-64 year olds with no health insurance, 2017
##  Figure S21. Intensive care unit (ICU) beds per 100,000 population,2017 
##  Figure S22. Age-adjusted years of potential life lost before age 75 per 
##      100,000 population, 2017

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

## Data ----
analytic_df <- readr::read_csv(here::here("data", "analytic_data_wide.csv"))
plotting_df <- process_to_plotting_df(analytic_df)

## Figure S1. Percentage of population 70 years or older, 2018 ----
p1 <- plot_counties(plotting_df, "p70older") +
    ggplot2::scale_fill_viridis_c(
        "Percent of population over 70",
        trans = "log1p",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS01_p_70yo.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS01_p_70yo.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S2. Percentage of households living in poverty, 2016 ----
p1 <- plot_counties(plotting_df, "p_poverty") +
    ggplot2::scale_fill_viridis_c(
        "Percent of population below poverty level",
        trans = "log1p",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS02_poverty.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS02_poverty.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S3. Percentage of population living in group quarters, 2018 ----
p1 <- plot_counties(plotting_df, "p_group_quarters_trunc") +
    ggplot2::scale_fill_viridis_c(
        "Percent of population housed in group quarters, 2018",
        # trans = "log1p",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        # breaks = c(seq(10, 50, 10)),
        # labels = c("<10", seq(20, 40, 10), ">50")
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS03_group_quarters.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS03_group_quarters.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S4. Percentage of population non-Hispanic and non-White ----
p1 <- plot_counties(plotting_df, "p_nonwhite") +
    ggplot2::scale_fill_viridis_c(
        "Percent of population non-Hispanic and non-White",
        direction = -1,
        # trans = "log1p",
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS04_p_nonwhite_nonhispanic.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS04_p_nonwhite_nonhispanic.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S5. Hospital beds per 100,000 population, 2017 ----
p1 <- plot_counties(plotting_df, "hosp_per_capita_trunc") +
    ggplot2::scale_fill_viridis_c(
        "Hospital beds per 100,000 population (log10)",
        trans = "log10",
        direction = 1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        breaks = c(10, 100, 1000),
        labels = c("<10", 100, ">1000")
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS05_hosp_per_capita.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS05_hosp_per_capita.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Beds vs population over 70 ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = hosp_per_capita,
    x_high = return_rounded_iqr(plotting_df$hosp_per_capita)[2],
    x_low = return_rounded_iqr(plotting_df$hosp_per_capita)[1],
    x_label = "Hospital beds per\n100,000 population",
    rev_x = TRUE, 
    y_var = p70older,
    y_high = return_rounded_iqr(plotting_df$p70older)[2],
    y_low = return_rounded_iqr(plotting_df$p70older)[1],
    y_label = "Population over 70\nyears of age (%)",
)
ggplot2::ggsave(
    here::here("plots", "figS06_bed_capacity_age.pdf"),
    p1$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.2
)
ggplot2::ggsave(
    here::here("plots", "figS06_bed_capacity_age.jpg"),
    p1$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.2
)

## Beds vs poverty ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = hosp_per_capita,
    x_high = return_rounded_iqr(plotting_df$hosp_per_capita)[2],
    x_low = return_rounded_iqr(plotting_df$hosp_per_capita)[1],
    x_label = "Hospital beds per\n100,000 population",
    rev_x = TRUE, 
    y_var = p_poverty,
    y_high = return_rounded_iqr(plotting_df$p_poverty)[2],
    y_low = return_rounded_iqr(plotting_df$p_poverty)[1],
    y_label = "Households under\npoverty-line (%)"
)
ggplot2::ggsave(
    here::here("plots", "figS07_bed_capacity_poverty.pdf"),
    p1$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.2
)
ggplot2::ggsave(
    here::here("plots", "figS07_bed_capacity_poverty.jpg"),
    p1$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.2
)

## Beds vs group quarters ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = hosp_per_capita,
    x_high = return_rounded_iqr(plotting_df$hosp_per_capita)[2],
    x_low = return_rounded_iqr(plotting_df$hosp_per_capita)[1],
    x_label = "Hospital beds per\n100,000 population",
    rev_x = TRUE, 
    y_var = p_group_quarters,
    y_high = return_rounded_iqr(plotting_df$p_group_quarters)[2],
    y_low = return_rounded_iqr(plotting_df$p_group_quarters)[1],
    y_label = "Population living\nin group quarters (%)",
)
ggplot2::ggsave(
    here::here("plots", "figS08_bed_capacity_group_quarters.pdf"),
    p1$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.2
)
ggplot2::ggsave(
    here::here("plots", "figS08_bed_capacity_group_quarters.jpg"),
    p1$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.2
)

## Beds vs nonwhite ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = hosp_per_capita,
    x_high = return_rounded_iqr(plotting_df$hosp_per_capita)[2],
    x_low = return_rounded_iqr(plotting_df$hosp_per_capita)[1],
    x_label = "Hospital beds per\n100,000 population",
    rev_x = TRUE, 
    y_var = p_nonwhite,
    y_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
    y_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
    y_label = "Population non-white (%)",
)
ggplot2::ggsave(
    here::here("plots", "figS09_bed_capacity_nonwhite.pdf"),
    p1$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.2
)
ggplot2::ggsave(
    here::here("plots", "figS09_bed_capacity_nonwhite.jpg"),
    p1$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.2
)

## Figure S10. Age-adjusted percent of adults over 20 years old who are ----
##              diagnosed with diabetes, 2016
p1 <- plot_counties(plotting_df, "p_diabetes") +
    ggplot2::scale_fill_viridis_c(
        "Percent of adults (20+) diagnosed with diabetes",
        trans = "log1p",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS10_diabetes.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS10_diabetes.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S11. Coronary heart disease-related hospitalizations per ----
##              1,000 Medicare beneficiaries (65+), 2014-2016
p1 <- plot_counties(plotting_df, "chd_hosp") +
    ggplot2::scale_fill_viridis_c(
        "Coronary heart disease-related hospitalizations per 1,000 Medicare beneficiaries (65+)",
        trans = "log1p",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS11_chd.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS11_chd.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S12. Hypertension hospitalization rate per 1,000 Medicare ----
##              beneficiaries (65+), 2014-2016
p1 <- plot_counties(plotting_df, "htn_hosp") +
    ggplot2::scale_fill_viridis_c(
        "Hypertension-related hospitalizations per 1,000 Medicare beneficiaries (65+)",
        trans = "log1p",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS12_htn.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS12_htn.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S13. Average household size, 2018 ----
p1 <- plot_counties(plotting_df, "avg_hh_size_trunc") +
    ggplot2::scale_fill_viridis_c(
        "Average household size, 2018",
        # trans = "log",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        breaks = c(2:4),
        labels = c("<2", 3, ">4")
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS13_avg_hh_size.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS13_avg_hh_size.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

# Figure S14. Percentage of households with 65+ year old living alone, 2018 ----
p1 <- plot_counties(plotting_df, "p_65_living_alone") +
    ggplot2::scale_fill_viridis_c(
        "Percent of households with 65+ living alone",
        # trans = "log",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS14_over65alone.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS14_over65alone.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S15. Percentage of households with grandparents living with their ----
##              own grandchildren (<18), 2018
p1 <-
    plot_counties(plotting_df, "p_hh_grandparents_with_under18_trunc") +
    ggplot2::scale_fill_viridis_c(
        "Percent of households with grandparents living with their own grandchildren (<18)",
        # trans = "log",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        # breaks = c(2:4),
        # labels = c("<2", 3, ">4")
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS15_grandparents_with_u18.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS15_grandparents_with_u18.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S16. Percentage of renter-occupied households renting, 2018 ----
p1 <- plot_counties(plotting_df, "p_hh_renter_trunc") +
    ggplot2::scale_fill_viridis_c(
        "Percent of renter-occupied households, 2018",
        # trans = "log",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        breaks = c(seq(10, 50, 10)),
        labels = c("<10", seq(20, 40, 10), ">50")
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS16_hh_renter.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS16_hh_renter.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S17. Population (log 10), 2018 ----
p1 <- plot_counties(plotting_df, "n_pop_2018") +
    ggplot2::scale_fill_viridis_c(
        "Population (log10)",
        trans = "log10",
        direction = 1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        breaks = 10 ^ (2:7),
        labels = c("100", "1,000", "10,000", "100,000", "1,000,000", "10,000,000")
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS17_population.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS17_population.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S18. Population per square mile (log 10), 2010 ----
p1 <- plot_counties(plotting_df, "pop_density") +
    ggplot2::scale_fill_viridis_c(
        "Population per square mile (log10)",
        trans = "log10",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        breaks = c(.1, 10, 1000, 10000, 70000),
        labels = c(.1, 10, "1,000", "10,000", "70,000"),
        limits = c(.1, 70000)
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS18_population_density.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS18_population_density.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)


## Figure S19. Percentage of households without broadband internet, 2018 ----
p1 <- plot_counties(plotting_df, "p_hh_no_broadband") +
    ggplot2::scale_fill_viridis_c(
        "Percent of households without broadband internet, 2018",
        # trans = "log",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        # breaks = c(2:4),
        # labels = c("<2", 3, ">4")
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS19_hh_no_broadband.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS19_hh_no_broadband.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S20. Percentage of 18-64 year olds with no health insurance, 2017 ----
p1 <- plot_counties(plotting_df, "p_18_64_no_insurance") +
    ggplot2::scale_fill_viridis_c(
        "Percent of 18-64 year olds without insurance",
        trans = "log1p",
        direction = -1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS20_p_uninsured.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS20_p_uninsured.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S21. Intensive care unit (ICU) beds per 100,000 population,2017 ----
p1 <- plot_counties(plotting_df, "icu_per_capita_trunc") +
    ggplot2::scale_fill_viridis_c(
        "ICU beds per 100,000 population",
        trans = "log10",
        direction = 1,
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        ),
        breaks = c(5, 10, 25, 50, 100),
        labels = c("<5", 10, 25, 50, ">100")
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS21_icu_per_capita.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS21_icu_per_capita.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)

## Figure S22. Age-adjusted years of potential life lost before age 75, 2017 ----
p1 <- plot_counties(plotting_df, "premature_mort") +
    ggplot2::scale_fill_viridis_c(
        "Years of potential life lost before age 75",
        direction = -1,
        trans = "log10",
        guide = ggplot2::guide_colorbar(
            title.position = "top",
            barheight = ggplot2::unit(.5, "cm"),
            barwidth = ggplot2::unit(12.5, "cm")
        )
    )  +
    ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
    here::here("plots", "figS22_premature_death_ypll.pdf"),
    p1,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 6,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "figS22_premature_death_ypll.jpg"),
    p1,
    dpi = 300,
    width = 7,
    height = 6,
    scale = 1.1
)
