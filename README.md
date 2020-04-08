
<!-- README.md is generated from README.Rmd. Please edit that file -->

## U.S. county-level characteristics to inform equitable COVID-19 response

<p align="center">

<img src="./plots/fig04_bivariate_nonwhite_premature_mort.jpg" width="650px" style="display: block; margin: auto;" />

</p>

## Introduction

This is reproducible code for our paper, [*U.S. county-level
characteristics to inform equitable COVID-19 response*](TODO), which
uses public-access county-level data to highlight charateristics of
COVID-19 vulneratbility. We use bivariate maps to show both the
convergent and divergent spatial patterning of these risk factors. The
full citation is:

> Chin T, Kanh R, Li R, Chen JT, Krieger N, Buckee CO, Balsari S, and
> Kiang MV. U.S. county-level characteristics to inform equitable
> COVID-19 response. TODO. doi: TODO.

An interactive companion app for our paper is available at
<https://mkiang.shinyapps.io/county_risks/>.

### Abstract

#### Background

The spread of Coronavirus Disease 2019 (COVID-19) across the United
States confirms that not all Americans are equally at risk of infection,
severe disease, or mortality. A range of intersecting biological,
demographic, and socioeconomic factors are likely to determine an
individual’s susceptibility to COVID-19. These factors vary
significantly across counties in the United States, and often reflect
the structural inequities in our society. Recognizing this vast
inter-county variation in risks will be critical to mounting an adequate
response strategy.

#### Methods and Findings

Using publicly available county-specific data we identified key
biological, demographic, and socioeconomic factors influencing
susceptibility to COVID-19, guided by international experiences and
consideration of epidemiological parameters of importance. We created
bivariate county-level maps to summarize examples of key relationships
across these categories, grouping age and poverty; comorbidities and
lack of health insurance; proximity, density and bed capacity; and race
and ethnicity, and premature death. We have also made available an
interactive online tool that allows public health officials to query
risk factors most relevant to their local context.

Our data demonstrate significant inter-county variation in key
epidemiological risk factors, with a clustering of counties in certain
states, which will result in an increased demand on their public health
system. While the East and West coast cities are particularly vulnerable
owing to their densities (and travel routes), a large number of counties
in the Southeastern states have a high proportion of at-risk
populations, with high levels of poverty, comorbidities, and premature
death at baseline, and low levels of health insurance coverage.

The list of variables we have examined is by no means comprehensive, and
several of them are interrelated and magnify underlying vulnerabilities.
The online tool allows readers to explore additional combinations of
risk factors, set categorical thresholds for each covariate, and filter
counties above different population thresholds.

#### Conclusion

COVID-19 responses and decision making in the United States remain
decentralized. Both the federal and state governments will benefit from
recognizing high intra-state, inter-county variation in population risks
and response capacity. Many of the factors that are likely to exacerbate
the burden of COVID-19 and the demand on healthcare systems are the
compounded result of long-standing structural inequalities in US
society. Strategies to protect those in the most vulnerable counties
will require urgent measures to better support communities’ attempts at
social distancing and to accelerate cooperation across jurisdictions to
supply personnel and equipment to counties that will experience high
demand.

### Issues

Please report issues via email or the [issues
page](https://github.com/mkiang/county_preparedness/issues).

## Project structure

  - `./code/` contains all code needed to reproduce our analyses. This
    code is designed to be run in order. Each file is a discrete step in
    the analytic pipeline and contains a brief description of the file
    objective at the top. I describe the overarching objective of some
    of the files below.
  - `./data/` contains all processed data that results from our
    `./code/` pipeline.
  - `./data_raw/` contains publicly-available data that will be used in
    the analytic pipeline.
  - `./plots/` contains the manuscript-ready plots in both `pdf` and
    `jpg` formats.

## Authors

  - Taylor Chin (![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@taylorchin](https://twitter.com/taylorchin))
  - Rebecca Kahn (![Github](http://i.imgur.com/9I6NRUm.png):
    [rek160](https://github.com/rek160) |
    ![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@rebeccajk13](https://twitter.com/rebeccajk13))
  - [Ruoran Li](https://scholar.harvard.edu/rli/home)
    (![Github](http://i.imgur.com/9I6NRUm.png):
    [ruoranepi](https://github.com/ruoranepi) |
    ![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@ruoranepi](https://twitter.com/ruoranepi))
  - [Jarvis
    Chen](https://www.dfhcc.harvard.edu/insider/member-detail/member/jarvis-t-chen-scd/)
  - [Nancy Krieger](https://www.hsph.harvard.edu/nancy-krieger/)
  - [Caroline
    Buckee](https://www.hsph.harvard.edu/magazine/magazine_article/the-uses-of-outrage/)
    (![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@Caroline\_OF\_B](https://twitter.com/Caroline_OF_B))
  - [Satchit Balsari](https://fxb.harvard.edu/people/satchit-balsari/)
    (![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@Satchit\_Balsari](https://twitter.com/Satchit_Balsari))
  - [Mathew Kiang](https://mathewkiang.com)
    (![Github](http://i.imgur.com/9I6NRUm.png):
    [mkiang](https://github.com/mkiang) |
    ![Twitter](http://i.imgur.com/wWzX9uB.png):
    [@mathewkiang](https://twitter.com/mathewkiang))

## Session info

    > sessioninfo::session_info()
    ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
     setting  value                       
     version  R version 3.6.1 (2019-07-05)
     os       macOS Mojave 10.14.6        
     system   x86_64, darwin15.6.0        
     ui       RStudio                     
     language (EN)                        
     collate  en_US.UTF-8                 
     ctype    en_US.UTF-8                 
     tz       America/Los_Angeles         
     date     2020-04-08                  
    
    ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
     package     * version date       lib source        
     assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.0)
     backports     1.1.5   2019-10-02 [1] CRAN (R 3.6.0)
     broom         0.5.2   2019-04-07 [1] CRAN (R 3.6.0)
     cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.6.0)
     cli           2.0.2   2020-02-28 [1] CRAN (R 3.6.0)
     colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.6.0)
     crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.0)
     dplyr       * 0.8.3   2019-07-04 [1] CRAN (R 3.6.0)
     fansi         0.4.1   2020-01-08 [1] CRAN (R 3.6.0)
     forcats     * 0.4.0   2019-02-17 [1] CRAN (R 3.6.0)
     fs          * 1.3.1   2019-05-06 [1] CRAN (R 3.6.0)
     generics      0.0.2   2018-11-29 [1] CRAN (R 3.6.0)
     ggplot2     * 3.3.0   2020-03-05 [1] CRAN (R 3.6.0)
     glue        * 1.3.2   2020-03-12 [1] CRAN (R 3.6.0)
     gtable        0.3.0   2019-03-25 [1] CRAN (R 3.6.0)
     haven         2.1.1   2019-07-04 [1] CRAN (R 3.6.0)
     here        * 0.1     2017-05-28 [1] CRAN (R 3.6.0)
     hms           0.5.1   2019-08-23 [1] CRAN (R 3.6.0)
     httr          1.4.1   2019-08-05 [1] CRAN (R 3.6.0)
     jsonlite      1.6     2018-12-07 [1] CRAN (R 3.6.0)
     lattice       0.20-38 2018-11-04 [1] CRAN (R 3.6.1)
     lifecycle     0.2.0   2020-03-06 [1] CRAN (R 3.6.0)
     lubridate     1.7.4   2018-04-11 [1] CRAN (R 3.6.0)
     magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.0)
     modelr        0.1.5   2019-08-08 [1] CRAN (R 3.6.0)
     munsell       0.5.0   2018-06-12 [1] CRAN (R 3.6.0)
     nlme          3.1-141 2019-08-01 [1] CRAN (R 3.6.0)
     packrat       0.5.0   2018-11-14 [1] CRAN (R 3.6.0)
     patchwork   * 1.0.0   2019-12-01 [1] CRAN (R 3.6.0)
     pillar        1.4.3   2019-12-20 [1] CRAN (R 3.6.0)
     pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 3.6.0)
     purrr       * 0.3.3   2019-10-18 [1] CRAN (R 3.6.0)
     R6            2.4.1   2019-11-12 [1] CRAN (R 3.6.0)
     Rcpp          1.0.4   2020-03-17 [1] CRAN (R 3.6.0)
     readr       * 1.3.1   2018-12-21 [1] CRAN (R 3.6.0)
     readxl        1.3.1   2019-03-13 [1] CRAN (R 3.6.0)
     rlang         0.4.5   2020-03-01 [1] CRAN (R 3.6.0)
     rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.0)
     rstudioapi    0.11    2020-02-07 [1] CRAN (R 3.6.0)
     rvest         0.3.5   2019-11-08 [1] CRAN (R 3.6.0)
     scales        1.1.0   2019-11-18 [1] CRAN (R 3.6.0)
     sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.0)
     stringi       1.4.3   2019-03-12 [1] CRAN (R 3.6.0)
     stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.6.0)
     tibble      * 2.1.3   2019-06-06 [1] CRAN (R 3.6.0)
     tidyr       * 1.0.0   2019-09-11 [1] CRAN (R 3.6.0)
     tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.6.0)
     tidyverse   * 1.2.1   2017-11-14 [1] CRAN (R 3.6.0)
     usmap       * 0.5.0   2019-09-12 [1] CRAN (R 3.6.0)
     vctrs         0.2.4   2020-03-10 [1] CRAN (R 3.6.0)
     withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.0)
     xml2          1.2.2   2019-08-09 [1] CRAN (R 3.6.0)
    
    [1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library
