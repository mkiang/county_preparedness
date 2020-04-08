library(usmap)
library(patchwork)
library(glue)
library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))

process_to_plotting_df <- function(analytic_df) {
    analytic_df %>%
        dplyr::transmute(
            abbrev = abbrev,
            name = name,
            fips = fips,
            n_pop_2018,
            n_pop_2017,
            p70older = (age70 + age75 + age80 + age85) / n_pop_2017 * 100,
            icu_per_capita = n_medsurg_card_other_icu_beds_2017 / n_pop_2017 * 100000,
            hosp_per_capita = n_hospital_beds_2017 / n_pop_2017 * 100000,
            p_uninsured_18_64_pov400 = p_18_64_no_insurance_400poverty / 10,
            p_18_64_no_insurance = p_18_64_no_insurance_allincome / 10,
            p_diabetes = prop_diabetes,
            chd_hosp = CHD_hosp,
            htn_hosp = hypertension_hosp,
            p_poverty = prop_poverty,
            avg_hh_size,
            p_65_living_alone,
            p_hh_grandparents_with_under18,
            p_hh_with_broadband,
            p_hh_renter,
            premature_mort,
            severe_housing_problems = severe_housing_problems * 100,
            severe_housing_cost_burden = severe_housing_cost_burden * 100,
            p_crowded_housing,
            high_housing_cost,
            gini_coef,
            ice_wb_income,
            pop_density = pop_per_sq_mile,
            p_nonwhite,
            p_group_quarters = p_group_quarters * 100,
            empower_rate = empower_power_depend / empower_medicare_bene * 1000,
            empower_power_depend,
            empower_medicare_bene
        ) %>%
        dplyr::mutate(
            chd_htn_hosp = chd_hosp + htn_hosp,
            icu_per_capita_trunc = dplyr::case_when(
                icu_per_capita < 5 ~ 5,
                icu_per_capita > 100 ~ 100,
                TRUE ~ icu_per_capita
            ),
            avg_hh_size_trunc = dplyr::case_when(avg_hh_size < 2 ~ 2,
                                                 avg_hh_size > 4 ~ 4,
                                                 TRUE ~ avg_hh_size),
            p_hh_grandparents_with_under18_trunc = dplyr::case_when(
                p_hh_grandparents_with_under18 < 5 ~ 5,
                p_hh_grandparents_with_under18 > 25 ~ 25,
                TRUE ~ p_hh_grandparents_with_under18
            ),
            p_hh_no_broadband = 100 - p_hh_with_broadband,
            p_hh_renter_trunc =  dplyr::case_when(p_hh_renter < 10 ~ 10,
                                                  p_hh_renter > 50 ~ 50,
                                                  TRUE ~ p_hh_renter),
            gini_coef_trunc =  dplyr::case_when(gini_coef < .35 ~ .35,
                                                gini_coef > .55 ~ .55,
                                                TRUE ~ gini_coef),
            p_group_quarters_trunc =  dplyr::case_when(# p_group_quarters < .5 ~ .5,
                p_group_quarters > 15 ~ 15,
                TRUE ~ p_group_quarters)
        ) %>%
        dplyr::filter(abbrev %in% c("DC", datasets::state.abb))
}

discretize_variable <-
    function(plotting_df, d_var, high_val, low_val) {
        d_var <- rlang::enquo(d_var)
        new_var <- paste0(rlang::quo_name(d_var), "_discrete")
        
        plotting_df %>%
            dplyr::transmute(
                fips,
                abbrev,
                name,
                !!new_var := dplyr::case_when(
                    !!d_var > high_val ~ sprintf("high >%0.2f", high_val),
                    !!d_var < low_val ~ sprintf("low <%0.2f", low_val),
                    TRUE ~ sprintf("mid %0.2f to %0.2f", low_val, high_val)
                )
            )
    }

gen_color_legend <- function(rev_x = FALSE, rev_y = FALSE) {
    var_x = c("low_x", "mid_x", "high_x")
    var_y  = c("low_y", "mid_y", "high_y")
    
    if (rev_x) {
        var_x <- rev(var_x)
    }
    if (rev_y) {
        var_y <- rev(var_y)
    }
    
    c_legend <-
        expand.grid(var_x = var_x,
                    var_y = var_y,
                    stringsAsFactors = TRUE) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(
            color_hex = c(
                "#e8e6f2",
                "#b5d3e7",
                "#4fadd0",
                "#e5b4d9",
                "#b8b3d8",
                "#3983bb",
                "#d34fa6",
                "#b03598",
                "#2a1a8a"
            ),
            color_hex_a = c(
                "#e8e8e8",
                "#e4acac",
                "#c85a5a",
                "#b0d5df",
                "#ad9ea5",
                "#985356",
                "#64acbe",
                "#627f8c",
                "#574249"
            )
        )
    
    return(c_legend)
}

create_bivariate_df <- function(plotting_df,
                                x_var,
                                x_high,
                                x_low,
                                y_var,
                                y_high,
                                y_low,
                                rev_x = FALSE,
                                rev_y = FALSE) {
    new_x_var <-
        paste0(rlang::quo_name(rlang::enquo(x_var)), "_discrete")
    new_y_var <-
        paste0(rlang::quo_name(rlang::enquo(y_var)), "_discrete")
    
    result_df <- dplyr::left_join(
        discretize_variable(plotting_df, {
            {
                x_var
            }
        }, x_high, x_low),
        discretize_variable(plotting_df, {
            {
                y_var
            }
        }, y_high, y_low)
    )
    
    result_df$var_x <- NA
    result_df[grepl("mid", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
        "mid_x"
    
    if (rev_x) {
        result_df[grepl("low", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
            "high_x"
        result_df[grepl("high", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
            "low_x"
    } else {
        result_df[grepl("low", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
            "low_x"
        result_df[grepl("high", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
            "high_x"
    }
    
    
    result_df$var_y <- NA
    result_df[grepl("mid", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
        "mid_y"
    
    if (rev_y) {
        result_df[grepl("low", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
            "high_y"
        result_df[grepl("high", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
            "low_y"
    } else {
        result_df[grepl("low", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
            "low_y"
        result_df[grepl("high", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
            "high_y"
    }
    
    result_df %>%
        dplyr::left_join(gen_color_legend())
}

gen_hotspots_legend <- function(rev_x = FALSE, rev_y = FALSE) {
    ## Create the plot
    p_legend <-
        ggplot2::ggplot(
            gen_color_legend(rev_x = rev_x, rev_y = rev_y),
            ggplot2::aes(x = var_x, y = var_y, fill = color_hex)
        ) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_identity(na.value = "grey50") +
        # scale_x_discrete(
        #     "x_label",
        #     expand = c(0.05, 0),
        #     labels = c(
        #         sprintf("Low: 0-%0.1f",
        #                 mort_mid_bin[1]),
        #         sprintf(
        #             "Medium: %0.1f-%0.1f",
        #             mort_mid_bin[1],
        #             mort_mid_bin[2]
        #         ),
    #         sprintf("High: >%0.1f",
    #                 mort_mid_bin[2])
    #     )
    # ) +
    # scale_y_discrete(
    #     "APC (%)",
    #     expand = c(0.05, 0),
    #     labels = c(
    #         sprintf("Slow: 0-%i",
    #                 apc_mid_bin[1]),
    #         sprintf("Moderate: %i-%i",
    #                 apc_mid_bin[1],
    #                 apc_mid_bin[2]),
    #         sprintf("Rapid: >%i",
    #                 apc_mid_bin[2])
    #     )
    # ) +
    mk_nytimes(
        panel.grid.major = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            vjust = 1
        ),
        axis.line = ggplot2::element_line(
            arrow = ggplot2::arrow(
                length = ggplot2::unit(.15, "inches"),
                type = "open",
                angle = 20
            )
        ),
        plot.subtitle = ggplot2::element_text(size = 13, hjust =
                                                  0.5)
    ) +
        ggplot2::coord_equal()
    
    return(p_legend)
}

plot_counties <-
    function(county_df, fill_var = "trunc_weighted_time") {
        usmap::plot_usmap(
            regions = "counties",
            data = county_df,
            values = fill_var,
            color = NA
        ) +
            ggplot2::geom_polygon(
                data = usmap::us_map(region = "states"),
                ggplot2::aes(x = x, y = y, group = group),
                color = "black",
                size = .3,
                fill = NA
            )
    }

plot_bivariate <- function(bivariate_df) {
    usmap::plot_usmap(
        regions = "counties",
        data = bivariate_df,
        values = "color_hex",
        color = NA
    ) +
        ggplot2::geom_polygon(
            data = usmap::us_map(region = "states"),
            ggplot2::aes(x = x, y = y, group = group),
            color = "black",
            size = .3,
            fill = NA
        ) +
        ggplot2::scale_fill_identity()
}

mega_plot_bivariate <-
    function(plotting_df,
             x_var,
             x_high,
             x_low,
             x_label,
             rev_x = FALSE,
             y_var,
             y_high,
             y_low,
             y_label,
             rev_y = FALSE,
             return_data = FALSE,
             use_layout = FALSE,
             as_strings = FALSE) {
        ## We need to recast as symbols because Shiny keeps them as chars
        if (as_strings) {
            x_var <- rlang::sym(x_var)
        }
        if (as_strings) {
            y_var <- rlang::sym(y_var)
        }
        
        x_labels <- c(
            sprintf("Low: <%i", round(x_low)),
            sprintf("Moderate: %i-%i", round(x_low), round(x_high)),
            sprintf("High: >%i", round(x_high))
        )
        y_labels <- c(
            sprintf("Low: <%i", round(y_low)),
            sprintf("Moderate: %i-%i", round(y_low), round(y_high)),
            sprintf("High: >%i", round(y_high))
        )
        
        if (rev_x) {
            x_labels <- rev(x_labels)
        }
        if (rev_y) {
            y_labels <- rev(y_labels)
        }
        
        p1 <- gen_hotspots_legend(rev_x = rev_x, rev_y = rev_y) +
            ggplot2::scale_x_discrete(x_label,
                                      expand = c(0.05, 0),
                                      labels = x_labels) +
            ggplot2::scale_y_discrete(y_label,
                                      expand = c(0.05, 0),
                                      labels = y_labels)
        
        discrete_df <- create_bivariate_df(
            plotting_df,
            {
                {
                    x_var
                }
            },
            x_high,
            x_low,
            {
                {
                    y_var
                }
            },
            y_high,
            y_low,
            rev_x = rev_x,
            rev_y = rev_y
        )
        
        p2 <- plot_bivariate(discrete_df)
        
        if (use_layout) {
            layout <- "
            ##BBBBBB
            ##BBBBBB
            ##BBBBBB
            AABBBBBB
            ##BBBBBB
        "
            p3 <- p1 + p2 + patchwork::plot_layout(design = layout)
        } else {
            p3 <- p1 + p2 + patchwork::plot_layout(widths = c(1, 5), ncol = 2)
        }
        
        if (return_data) {
            x <- list(
                counts = discrete_df %>%
                    dplyr::group_by_at(dplyr::vars(dplyr::one_of(
                        names(discrete_df)[4:8]
                    ))) %>%
                    dplyr::count(),
                data = discrete_df,
                plot = p3
            )
            x
        } else {
            p3
        }
    }

return_dict <- function() {
    list(
        "p70older" = list(
            low = 10,
            high = 15,
            reverse = FALSE,
            label = "% population\nover 70 years old",
            min = 0,
            max = 45,
            step = 1,
            transform = "identity"
        ),
        "icu_per_capita" = list(
            low = 10,
            high = 25,
            reverse = TRUE,
            label = "ICU beds per\n100,000 population",
            min = 0,
            max = 125,
            step = 1,
            transform = "log10"
        ),
        "hosp_per_capita" = list(
            low = 100,
            high = 300,
            reverse = TRUE,
            label = "Hospital beds per\n100,000 population",
            min = 0,
            max = 1500,
            step = 50,
            transform = "log10"
        ),
        "p_18_64_no_insurance" = list(
            low = 10,
            high = 25,
            reverse = FALSE,
            label = "% population 18-64\nwithout health insurance",
            min = 0,
            max = 40,
            step = 1,
            transform = "identity"
        ),
        "p_diabetes" = list(
            low = 5,
            high = 15,
            reverse = FALSE,
            label = "% population 20+\nwith diabetes",
            min = 0,
            max = 25,
            step = 1,
            transform = "identity"
        ),
        "chd_hosp" = list(
            low = 25,
            high = 75,
            reverse = FALSE,
            label = "CHD hospitalizations per\n1,000 Medicare beneficiaries",
            min = 0,
            max = 150,
            step = 5,
            transform = "identity"
        ),
        "htn_hosp" = list(
            low = 25,
            high = 75,
            reverse = FALSE,
            label = "Hypertension hospitalizations per\n1,000 Medicare beneficiaries",
            min = 0,
            max = 250,
            step = 10,
            transform = "identity"
        ),
        "p_poverty" = list(
            low = 10,
            high = 20,
            reverse = FALSE,
            label = "% households\nin poverty",
            min = 0,
            max = 40,
            step = 1,
            transform = "identity"
        ),
        "avg_hh_size" = list(
            low = 1.5,
            high = 2.5,
            reverse = FALSE,
            label = "Average household size",
            min = 0,
            max = 4,
            step = .1,
            transform = "identity"
        ),
        "p_65_living_alone" = list(
            low = 5,
            high = 10,
            reverse = FALSE,
            label = "% households with\n65+ living alone",
            min = 0,
            max = 25,
            step = 1,
            transform = "identity"
        ),
        "p_hh_grandparents_with_under18" = list(
            low = 5,
            high = 10,
            reverse = FALSE,
            label = "% households with grandparents\nliving with grandchildren",
            min = 0,
            max = 20,
            step = .5,
            transform = "identity"
        ),
        "p_hh_renter" = list(
            low = 15,
            high = 30,
            reverse = FALSE,
            label = "% households renting",
            min = 0,
            max = 60,
            step = 1,
            transform = "identity"
        ),
        "pop_density" = list(
            low = 20,
            high = 100,
            reverse = FALSE,
            label = "Population per\nsquare mile",
            min = 0,
            max = 1000,
            step = 25,
            transform = "log1p"
        ),
        "p_group_quarters" = list(
            low = 1,
            high = 5,
            reverse = FALSE,
            label = "% population living\n in group quarters",
            min = 0,
            max = 40,
            step = 1,
            transform = "identity"
        ),
        "chd_htn_hosp" = list(
            low = 150,
            high = 250,
            reverse = FALSE,
            label = "CTN + HTN hospitalizations\nper 1,000 Medicare beneficiaries",
            min = 0,
            max = 400,
            step = 25,
            transform = "identity"
        ),
        "p_hh_no_broadband" = list(
            low = 25,
            high = 50,
            reverse = FALSE,
            label = "% households with\nno broadband internet",
            min = 0,
            max = 50,
            step = 1,
            transform = "identity"
        ),
        
        
        "gini_coef" = list(
            low = .3,
            high = .5,
            reverse = FALSE,
            label = "Gini coefficient",
            min = .25,
            max = .75,
            step = .05,
            transform = "identity"
        ),
        "ice_wb_income" = list(
            low = -.25,
            high = .25,
            reverse = FALSE,
            label = "ICE BW Income",
            min = -.5,
            max = .5,
            step = .05,
            transform = "identity"
        ),
        "premature_mort" = list(
            low = 7500,
            high = 10000,
            reverse = FALSE,
            label = "Age-adjusted years of\npotential life lost before age 75",
            min = 5000,
            max = 15000,
            step = 500,
            transform = "identity"
        ),
        "severe_housing_problems" = list(
            low = 5,
            high = 15,
            reverse = FALSE,
            label = "% households with\nsevere housing problems",
            min = 0,
            max = 30,
            step = 1,
            transform = "identity"
        ),
        "severe_housing_cost_burden" = list(
            low = 5,
            high = 15,
            reverse = FALSE,
            label = "% households with severe\nhousing cost burden",
            min = 0,
            max = 25,
            step = 1,
            transform = "identity"
        ),
        "p_crowded_housing" = list(
            low = 2,
            high = 5,
            reverse = FALSE,
            label = "% crowded housing",
            min = 0,
            max = 15,
            step = .1,
            transform = "identity"
        ),
        # "high_housing_cost" = list(
        #     low = 25,
        #     high = 50,
        #     reverse = FALSE,
        #     label = "% households with\nno broadband internet",
        #     min = 0,
        #     max = 50,
        #     step = 1
        # ),
        "p_nonwhite" = list(
            low = 25,
            high = 50,
            reverse = FALSE,
            label = "% of the population\nnon-Hispanic and non-White",
            min = 0,
            max = 100,
            step = 1,
            transform = "identity"
        ),
        "empower_rate" = list(
            low = 25,
            high = 50,
            reverse = FALSE,
            label = "Medicare beneficiaries with DME\nper 1000 (?) Medicare beneficiaries",
            min = 0,
            max = 200,
            step = 10,
            transform = "identity"
        )
    )
}

return_low <- function(x) {
    return_dict()[[x]]$low
}

return_high <- function(x) {
    return_dict()[[x]]$high
}

return_label <- function(x) {
    return_dict()[[x]]$label
}

return_reverse <- function(x) {
    return_dict()[[x]]$reverse
}

return_min <- function(x) {
    return_dict()[[x]]$min
}

return_max <- function(x) {
    return_dict()[[x]]$max
}

return_step <- function(x) {
    return_dict()[[x]]$step
}

return_transform <- function(x) {
    return_dict()[[x]]$transform
}

return_nonNA <- function(plotting_df, col_x) {
    sum(!is.na(plotting_df[[col_x]]))
}

return_median_val <- function(plotting_df, col_x) {
    sprintf("%0.2f", stats::median(plotting_df[[col_x]], na.rm = TRUE))
}

return_middle_N <- function(plotting_df, col_x, range_x) {
    sum(dplyr::between(plotting_df[[col_x]], range_x[1], range_x[2]), na.rm = TRUE)
}

return_lower_counties <- function(plotting_df, col_x, range_x) {
    sum(plotting_df[[col_x]] < range_x[1], na.rm = TRUE)
}

return_upper_counties <- function(plotting_df, col_x, range_x) {
    sum(plotting_df[[col_x]] > range_x[2], na.rm = TRUE)
}

plot_ranked_covariate <-
    function(plotting_df,
             col_x,
             fips_x = NA,
             rev_x,
             trans_x,
             color_x = "red") {
        sub_df <- plotting_df %>%
            dplyr::select(abbrev, name, fips, col_x) %>%
            dplyr::mutate(name_x = sprintf("%s, %s (%s)", name, abbrev, fips))
        
        if (!rev_x) {
            sub_df$x_rank <-
                dplyr::row_number(dplyr::desc(plotting_df[[col_x]]))
        } else {
            sub_df$x_rank <- dplyr::row_number(plotting_df[[col_x]])
        }
        
        p1 <-
            ggplot2::ggplot(sub_df, ggplot2::aes(x = x_rank, y = !!rlang::sym(col_x))) +
            ggplot2::geom_point(size = .5) +
            ggplot2::scale_x_continuous("Counties by rank (highest to lowest risk)") +
            mk_nytimes(axis.text.x = ggplot2::element_blank()) +
            ggplot2::scale_y_continuous(return_label(col_x),
                                        trans = trans_x)
        
        if (!is.na(fips_x)) {
            p1 <- p1 +
                ggplot2::geom_point(
                    data = sub_df %>% dplyr::filter(fips == fips_x),
                    color = color_x,
                    size = 2.5
                )
        }
        
        p1
    }

generate_county_text <- function(plotting_df, county_x, rf1, rf2) {
    sub_df <- plotting_df %>%
        dplyr::filter(fips == sprintf("%05i", as.integer(county_x)))
    
    COUNTY_NAME <- sprintf("%s, %s", sub_df$name, sub_df$abbrev)
    POP <- sub_df$n_pop_2018
    RISKFACTOR1 <- gsub("\n", " ", return_label(rf1))
    rf1_val <- sub_df[[rf1]]
    HIGHER_COUNTIES1 <-
        sum(plotting_df[[rf1]] < rf1_val, na.rm = TRUE)
    LOWER_COUNTIES1 <-
        sum(plotting_df[[rf1]] > rf1_val, na.rm = TRUE)
    RISKFACTOR2 <- gsub("\n", " ", return_label(rf2))
    rf2_val <- sub_df[[rf2]]
    HIGHER_COUNTIES2 <-
        sum(plotting_df[[rf2]] < rf2_val, na.rm = TRUE)
    LOWER_COUNTIES2 <-
        sum(plotting_df[[rf2]] > rf2_val, na.rm = TRUE)
    
    glue::glue(
        "<b>{COUNTY_NAME}</b> (highlighted in red below) has a population of about <b>{POP}</b> (2018). ",
        "For <b>{RISKFACTOR1}</b>, it has a value of <b>{RF1_VAL}</b>, which is ",
        "higher than <b>{HIGHER_COUNTIES1}</b> counties and lower ",
        "than <b>{LOWER_COUNTIES1}</b> counties. For <b>{RISKFACTOR2}</b>, it has a value ",
        "of <b>{RF2_VAL}</b>, which is higher than <b>{HIGHER_COUNTIES2}</b> counties ",
        "and lower than <b>{LOWER_COUNTIES2}</b> counties.<p><p>",
        COUNTY_NAME = COUNTY_NAME,
        POP = POP,
        RISKFACTOR1 = RISKFACTOR1,
        RF1_VAL =  sprintf("%0.2f", rf1_val),
        HIGHER_COUNTIES1 = HIGHER_COUNTIES1,
        LOWER_COUNTIES1 = LOWER_COUNTIES1,
        RISKFACTOR2 = RISKFACTOR2,
        RF2_VAL = sprintf("%0.2f", rf2_val),
        HIGHER_COUNTIES2 = HIGHER_COUNTIES2,
        LOWER_COUNTIES2 = LOWER_COUNTIES2
    )
}
