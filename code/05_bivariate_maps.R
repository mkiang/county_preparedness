## 05_bivariate_maps.R ----
##
## Create the primary figures of the paper

## Imports ----
library(tidyverse)
library(patchwork)
library(cowplot)
library(here)
source(here::here("code", "utils.R"))

## Data ----
analytic_df <-
    readr::read_csv(here::here("data", "analytic_data_wide.csv"))
plotting_df <- process_to_plotting_df(analytic_df)

## Covariates we are going to use:
##  1. Percent over 70
##  2. Percent in poverty
##  3. Percent in group quarters
##
##  + 4. how racial/ethnic composition intersects with all of them.
##
## Which will result 6 plots.
##  Figure 1
##  1. Percent over 70 (x) vs percent in poverty (y)
##  2. Percent in poverty (x) vs percent in group quarters (y)
##  3. Percent over 70 (x) vs percent in group quarters (y)
##
##  Figure 2
##  4. Percent non-white (x) vs percent over 70 (y)
##  5. Percent non-white (x) vs percent in poverty (y)
##  6. Percent non-white (x) vs percent in group quarters (y)
##
## Thresholds will be defined by using (rounded) IQR as medium range.

## 1. Percent over 70 vs percent in poverty ----
p1 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p70older,
    x_high = return_rounded_iqr(plotting_df$p70older)[2],
    x_low = return_rounded_iqr(plotting_df$p70older)[1],
    x_label = "Population over 70\nyears of age (%)",
    y_var = p_poverty,
    y_high = return_rounded_iqr(plotting_df$p_poverty)[2],
    y_low = return_rounded_iqr(plotting_df$p_poverty)[1],
    y_label = "Households under\npoverty-line (%)"
)

## 2. Percent in poverty vs percent in group quarters ----
p2 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p_poverty,
    x_high = return_rounded_iqr(plotting_df$p_poverty)[2],
    x_low = return_rounded_iqr(plotting_df$p_poverty)[1],
    x_label = "Households under\npoverty-line (%)",
    y_var = p_group_quarters,
    y_high = return_rounded_iqr(plotting_df$p_group_quarters)[2],
    y_low = return_rounded_iqr(plotting_df$p_group_quarters)[1],
    y_label = "Population living\nin group quarters (%)",
)

## 3. Percent over 70 vs percent in group quarters ----
p3 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p70older,
    x_high = return_rounded_iqr(plotting_df$p70older)[2],
    x_low = return_rounded_iqr(plotting_df$p70older)[1],
    x_label = "Population over 70\nyears of age (%)",
    y_var = p_group_quarters,
    y_high = return_rounded_iqr(plotting_df$p_group_quarters)[2],
    y_low = return_rounded_iqr(plotting_df$p_group_quarters)[1],
    y_label = "Population living\nin group quarters (%)",
)

## 4. Percent non-white vs percent over 70 ----
p4 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p_nonwhite,
    x_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
    x_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
    x_label = "Population non-white (%)",
    y_var = p70older,
    y_high = return_rounded_iqr(plotting_df$p70older)[2],
    y_low = return_rounded_iqr(plotting_df$p70older)[1],
    y_label = "Population over 70\nyears of age (%)"
)

## 5. Percent non-white vs percent in poverty ----
p5 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p_nonwhite,
    x_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
    x_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
    x_label = "Population non-white (%)",
    y_var = p_poverty,
    y_high = return_rounded_iqr(plotting_df$p_poverty)[2],
    y_low = return_rounded_iqr(plotting_df$p_poverty)[1],
    y_label = "Households under\npoverty-line (%)"
)

## 6. Percent non-white vs percent in group quarters ----
p6 <- mega_plot_bivariate(
    plotting_df = plotting_df,
    return_data = TRUE,
    x_var = p_nonwhite,
    x_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
    x_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
    x_label = "Population non-white (%)",
    y_var = p_group_quarters,
    y_high = return_rounded_iqr(plotting_df$p_group_quarters)[2],
    y_low = return_rounded_iqr(plotting_df$p_group_quarters)[1],
    y_label = "Population living\nin group quarters (%)",
)


## Save ----
## Figure 1 all ----
p1_all <- cowplot::plot_grid(
    p1$legend_only,
    p1$map_only,
    p2$legend_only,
    p2$map_only,
    p3$legend_only,
    p3$map_only,
    ncol = 2,
    rel_widths = c(.5, 1),
    labels = c("A", "", "B", "", "C"),
    scale = rep(c(.78, 1), 3)
)
ggplot2::ggsave(
    here::here("plots", "fig01_covariates.pdf"),
    p1_all,
    device = grDevices::cairo_pdf,
    width = 6,
    height = 8,
    scale = 1.2
)
ggplot2::ggsave(
    here::here("plots", "fig01_covariates.jpg"),
    p1_all,
    dpi = 300,
    width = 6,
    height = 8,
    scale = 1.2
)

## Figure 1 all ----
p2_all <- cowplot::plot_grid(
    p4$legend_only,
    p4$map_only,
    p5$legend_only,
    p5$map_only,
    p6$legend_only,
    p6$map_only,
    ncol = 2,
    rel_widths = c(.5, 1),
    labels = c("A", "", "B", "", "C"),
    scale = rep(c(.78, 1), 3)
)
ggplot2::ggsave(
    here::here("plots", "fig02_raceethnicity.pdf"),
    p2_all,
    device = grDevices::cairo_pdf,
    width = 6,
    height = 8,
    scale = 1.2
)
ggplot2::ggsave(
    here::here("plots", "fig02_raceethnicity.jpg"),
    p2_all,
    dpi = 300,
    width = 6,
    height = 8,
    scale = 1.2
)

## Save all the plots as individual plots ----
ggplot2::ggsave(
    here::here("plots", "fig01-1.pdf"),
    p1$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "fig01-1.jpg"),
    p1$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.1
)

ggplot2::ggsave(
    here::here("plots", "fig01-2.pdf"),
    p2$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "fig01-2.jpg"),
    p2$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.1
)

ggplot2::ggsave(
    here::here("plots", "fig01-3.pdf"),
    p3$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "fig01-3.jpg"),
    p3$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.1
)

ggplot2::ggsave(
    here::here("plots", "fig02-1.pdf"),
    p4$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "fig02-1.jpg"),
    p4$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.1
)

ggplot2::ggsave(
    here::here("plots", "fig02-2.pdf"),
    p5$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "fig02-2.jpg"),
    p5$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.1
)

ggplot2::ggsave(
    here::here("plots", "fig02-3.pdf"),
    p6$plot,
    device = grDevices::cairo_pdf,
    width = 7,
    height = 4,
    scale = 1.1
)
ggplot2::ggsave(
    here::here("plots", "fig02-3.jpg"),
    p6$plot,
    dpi = 300,
    width = 7,
    height = 4,
    scale = 1.1
)
