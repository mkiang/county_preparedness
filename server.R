library(tidyverse)
library(here)
library(rlang)
library(glue)
library(ggExtra)
source(here("code", "utils.R"))

## Import data ----
analytic_df <- read_csv(here("data", "analytic_data_wide.csv"))
main_df <- process_to_plotting_df(analytic_df)
county_choice <- readRDS(here("data", "county_choice.RDS"))

# Define server
shinyServer(function(input, output, session) {
    observe({
        rf1_val <- input$riskfactor1
        updateSliderInput(
            session,
            "rf1slider",
            value = c(return_low(rf1_val), return_high(rf1_val)),
            min = return_min(rf1_val),
            max = return_max(rf1_val),
            step = return_step(rf1_val)
        )
    })
    
    observe({
        rf2_val <- input$riskfactor2
        updateSliderInput(
            session,
            "rf2slider",
            value = c(return_low(rf2_val), return_high(rf2_val)),
            min = return_min(rf2_val),
            max = return_max(rf2_val),
            step = return_step(rf2_val)
        )
    })
    
    observe({
        rf1_trans_val <- input$rf1transform
        updateSelectInput(
            session,
            "rf1transform",
            selected = return_transform(rf1_trans_val)
        )
    })
    
    observe({
        rf2_trans_val <- input$rf2transform
        updateSelectInput(
            session,
            "rf2transform",
            selected = return_transform(rf2_trans_val)
        )
    })
    
    plotting_df <- reactive({
        county_size <- as.numeric(input$county_size)
        main_df %>% 
            filter(n_pop_2017 >= county_size)
    })
    
    p1 <- reactive({
        mega_plot_bivariate(
        plotting_df = plotting_df(),
        x_var = input$riskfactor1,
        x_high = as.numeric(input$rf1slider[2]),
        x_low = as.numeric(input$rf1slider[1]),
        x_label = return_label(input$riskfactor1),
        rev_x = return_reverse(input$riskfactor1),
        y_var = input$riskfactor2,
        y_high = as.numeric(input$rf2slider[2]),
        y_low = as.numeric(input$rf2slider[1]),
        y_label = return_label(input$riskfactor2),
        rev_y = return_reverse(input$riskfactor2),
        return_data = TRUE, 
        use_layout = TRUE,
        as_strings = TRUE
    )
    })
    
    output$bivariate_map <- renderCachedPlot({
        p1()$plot
    },
    cacheKeyExpr = list(
        input$riskfactor1,
        input$riskfactor2,
        input$rf1slider,
        input$rf2slider,
        input$county_size
    ),
    res = round(72 * 1.5)
    )
    
    output$risk1_label <- renderText(return_label(input$riskfactor1))
    output$risk2_label <- renderText(return_label(input$riskfactor2))
    
    output$scatterplot <- renderCachedPlot({
        ggExtra::ggMarginal(
            ggplot(
                data = left_join(
                    plotting_df(),
                    p1()$data %>%
                        select(fips, color_hex)
                    ),
                aes(
                    x = !!rlang::sym(input$riskfactor1),
                    y = !!rlang::sym(input$riskfactor2),
                    color = color_hex
                )
            ) + geom_point(alpha = .9, size = 2) +
                scale_color_identity() +
                mk_nytimes() +
                scale_x_continuous(return_label(input$riskfactor1),
                                   trans = input$rf1transform) +
                scale_y_continuous(return_label(input$riskfactor2),
                                   trans = input$rf2transform),
            "density"
        )
    },
    cacheKeyExpr = list(
        input$riskfactor1,
        input$riskfactor2,
        input$rf1slider,
        input$rf2slider,
        input$rf1transform,
        input$rf2transform,
        input$county_size
    ),
    res = round(72 * 1.5)
    )
    
    output$risk1_map <- renderCachedPlot({
        plot_counties(plotting_df(), input$riskfactor1) +
            scale_fill_viridis_c(
                return_label(input$riskfactor1),
                trans = input$rf1transform,
                direction = -1,
                guide = guide_colorbar(
                    title.position = "top",
                    barheight = unit(.5, "cm"),
                    barwidth = unit(8.5, "cm")
                )
            )  +
            theme(legend.position = "bottom")
    },
    cacheKeyExpr = list(
        input$riskfactor1,
        input$rf1transform,
        input$county_size
    ),
    res = round(72 * 1.5)
    )
    
    output$risk2_map <- renderCachedPlot({
        plot_counties(plotting_df(), input$riskfactor2) +
            scale_fill_viridis_c(
                return_label(input$riskfactor2),
                trans = input$rf2transform,
                direction = -1,
                guide = guide_colorbar(
                    title.position = "top",
                    barheight = unit(.5, "cm"),
                    barwidth = unit(8.5, "cm")
                )
            )  +
            theme(legend.position = "bottom")
    },
    cacheKeyExpr = list(
        input$riskfactor2,
        input$rf2transform,
        input$county_size
    ),
    res = round(72 * 1.5)
    )
    
    output$risk1_caterpillar <- renderCachedPlot({
        plot_ranked_covariate(
            plotting_df(),
            col_x = input$riskfactor1,
            rev_x = return_reverse(input$riskfactor1),
            trans_x = input$rf1transform,
            fips_x = sprintf("%05i", as.numeric(input$county))
        )
    }, 
    cacheKeyExpr = list(
        input$riskfactor1,
        input$rf1transform,
        input$county_size,
        sprintf("%05i", as.numeric(input$county))
    ),
    res = round(72 * 1.5)
    )
    
    output$risk2_caterpillar <- renderCachedPlot({
        plot_ranked_covariate(
            plotting_df(),
            col_x = input$riskfactor2,
            rev_x = return_reverse(input$riskfactor2),
            trans_x = input$rf2transform,
            fips_x = sprintf("%05i", as.integer(input$county))
        )
    }, 
    cacheKeyExpr = list(
        input$riskfactor2,
        input$rf2transform,
        input$county_size,
        sprintf("%05i", as.integer(input$county))
    ),
    res = round(72 * 1.5)
    )
    
    output$county_text <- renderText({
        generate_county_text(
            plotting_df(),
            county_x = sprintf("%05i", as.numeric(input$county)), 
            rf1 = input$riskfactor1,
            rf2 = input$riskfactor2
        )
    })
    
    output$context_text <- renderText({
        paste0(
            glue(
                "In our data set, the median US county has a value of <b>{MEDIAN}</b> ",
                "for <b>{LABEL}</b> (N={COUNTIES}). The moderate risk range of ",
                "<b>{LOWER} to {UPPER}</b> includes {MIDDLECOUNTIES} counties. There are ",
                "{LOWERCOUNTIES} counties with values below <b>{LOWER}</b> and {UPPERCOUNTIES} ",
                "counties with values above <b>{UPPER}</b>.<p>",
                MEDIAN = return_median_val(plotting_df(), input$riskfactor1),
                LABEL = return_label(input$riskfactor1),
                COUNTIES = return_nonNA(plotting_df(), input$riskfactor1),
                MIDDLECOUNTIES = return_middle_N(plotting_df(), input$riskfactor1, input$rf1slider),
                LOWERCOUNTIES = return_lower_counties(plotting_df(), input$riskfactor1, input$rf1slider),
                LOWER = input$rf1slider[1],
                UPPER = input$rf1slider[2],
                UPPERCOUNTIES = return_upper_counties(plotting_df(), input$riskfactor1, input$rf1slider)
            ),
            
            glue(
                "<p>For <b>{LABEL}</b> (N={COUNTIES}), the median US county has a ",
                "value of <b>{MEDIAN}</b>. ",
                "The moderate risk range of ",
                "<b>{LOWER} to {UPPER}</b> includes {MIDDLECOUNTIES} counties. There are ",
                "{LOWERCOUNTIES} counties with values below <b>{LOWER}</b> and {UPPERCOUNTIES} ",
                "counties with values above <b>{UPPER}</b>.<p>",
                "<p>There are {HIGHRISK} counties with <font color='#2a1a8a'><b>high risk for both factors.</b></font>",
                MEDIAN = return_median_val(plotting_df(), input$riskfactor2),
                LABEL = return_label(input$riskfactor2),
                COUNTIES = return_nonNA(plotting_df(), input$riskfactor2),
                MIDDLECOUNTIES = return_middle_N(plotting_df(), input$riskfactor2, input$rf2slider),
                LOWERCOUNTIES = return_lower_counties(plotting_df(), input$riskfactor2, input$rf2slider),
                LOWER = input$rf2slider[1],
                UPPER = input$rf2slider[2],
                UPPERCOUNTIES = return_upper_counties(plotting_df(), input$riskfactor2, input$rf2slider),
                HIGHRISK = p1()$counts %>% filter(color_hex == "#2a1a8a") %>% pull(n)
            )
        )
        
    })
})
