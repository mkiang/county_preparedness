## Imports ----
library(shiny)

## Textual constants ----
footer_tag  <- HTML(
    "Created in <a href='https://shiny.rstudio.com/'>Shiny</a>.
      Source code is available on
      <a href='https://github.com/mkiang/county_preparedness'>this
      paper's Github repository</a>. See <a href='https://www.medrxiv.org/content/10.1101/2020.04.08.20058248v1'>our pre-print on medRxiv</a>."
)

## More info
more_info <- list(
    h4("About the data"),
    HTML(
        "This app uses publicly available data from a variety of sources 
        including the 
        <a href='https://data.hrsa.gov/topics/health-workforce/ahrf'>Area Health Resources Files</a>, 
        American Community Survey, 
        <a href='https://www.cdc.gov/dhdsp/maps/atlas/index.htm'>Centers for Disease Control and Prevention Atlas file</a>, <a href='https://www.countyhealthrankings.org/'>RWJF County Health Rankings</a>, and 
        <a href='https://www.cdc.gov/nchs/nvss/bridged_race.htm'>National Center for Health Statistics population estimates</a>. <p>
        <p>See <a href='https://www.medrxiv.org/content/10.1101/2020.04.08.20058248v1'>our paper</a> for details. Data and source code are available at the
          <a href='https://github.com/mkiang/county_preparedness'>
          associated Github repository</a>. "
    )
)

page_title <- "U.S. county-level characteristics to inform equitable COVID-19 response"
page_subtitle   <- "Using this app"
page_desc  <- HTML(
    "This is a companion app for <a href='https://www.medrxiv.org/content/10.1101/2020.04.08.20058248v1'>our paper</a>. Select two covariates of interest from the dropdown boxes to the right. For each covariate, select thresholds for the moderate category. Skewed covariates can be transformed using the Axis Transformation option. <p><p>The bivariate risk map shows which counties fall into each category defined by the legend. The scatterplot shows the relationship based on the raw data with the univariate distribution shown on the margins.<p><p>Some measures are sensitive to small population sizes; therefore, we provide a filter to remove counties based on population size below."
)

## Helper functions ----
risk_factor_selector <- function(value_id = "factor1",
                                 label = "Risk Factor 1",
                                 selected_x = "p70older") {
    selectInput(
        value_id,
        label = label,
        choices = list(
            "% population over 70" = "p70older",
            "% 20+ year olds with diabetes" = "p_diabetes",
            "Coronary heart disease (CHD) hospitalizations" = "chd_hosp",
            "Hypertension hospitalizations" = "htn_hosp",
            "CHD and HTN hospitalizations" = "chd_htn_hosp",
            "Average household size" = "avg_hh_size",
            "% households with 65+ living alone" = "p_65_living_alone",
            "% households with grandparent-grandchild" = "p_hh_grandparents_with_under18",
            "% households renting" = "p_hh_renter",
            "Population per square mile" = "pop_density",
            "% population in group quarters" = "p_group_quarters",
            "% households without broadband internet" = "p_hh_no_broadband",
            "% 18-64 year olds without insurance" = "p_18_64_no_insurance",
            "% households under the poverty line" = "p_poverty",
            "Intensive care unit beds per capita" = "icu_per_capita",
            "Hospital beds per capita" = "hosp_per_capita",
            "Years of potential life lost before 75" = "premature_mort",
            "% population non-Hispanic and non-White" = "p_nonwhite",
            "Severe housing problems*" = "severe_housing_problems",
            "Severe housing cost burden*" = "severe_housing_cost_burden",
            "Percent crowded housing*" = "p_crowded_housing",
            "High housing cost*" = "high_housing_cost",
            "Gini coefficient*" = "gini_coef",
            "Index of concentration at the extremes*" = "ice_wb_income"
        ),
        selected = selected_x
    )
}
