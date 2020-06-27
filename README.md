
<!-- README.md is generated from README.Rmd. Please edit that file -->

## US-county level variation in intersecting individual, household, and community characteristics relevant to COVID-19 and planning an equitable response: A cross-sectional analysis

<p align="center">

<img src="./plots/fig01-1.jpg" width="650px" style="display: block; margin: auto;" />

</p>

## Introduction

This is reproducible code for our pre-print, [*US-county level variation
in intersecting individual, household, and community characteristics
relevant to COVID-19 and planning an equitable response: A
cross-sectional analysis*](TODO), which uses public-access county-level
data to highlight characteristics of COVID-19 risk factors across
different levels. We use bivariate maps to show both the intersection
and spatial patterning of these risk factors. The full citation is:

> TODO

An interactive companion app for our paper is available at
<https://ccdd-hsph-harvard.shinyapps.io/county-risk/>.

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

## Note:

An earlier pre-print version of this paper is available on medRxiv:
[*U.S. county-level characteristics to inform equitable COVID-19
response*](https://www.medrxiv.org/content/10.1101/2020.04.08.20058248v1).

Session information is saved in `./session_info.txt`.
