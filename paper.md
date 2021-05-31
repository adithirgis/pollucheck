---
title: 'pollucheck v1.0: A package to explore Open-Source Air-Quality data'
authors:
- affiliation: '1'
  name: Adithi R. Upadhya
  orcid: 0000-0002-1764-1379
- affiliation: '1'
  name: Pratyush Agrawal
- affiliation: '2'
  name: Sreekanth Vakacherla
  orcid: 0000-0003-0400-6584
- affiliation: '1'
  name: Meenakshi Kushwaha
date: "31 May 2021"
bibliography: paper.bib
tags:
- R
- open-source
- air quality
- shiny
affiliations:
- index: '1'
  name: ILK Labs, Bengaluru, India
- index: '2'
  name: Center for Study of Science, Technology & Policy, Bengaluru, India
---

# Summary

R has been an efficient tool to handle air quality data. With ever increasing open source data providers, there is a need for quality check and ease of handling this data. With `pollucheck` we aim to provide a simple workflow to air quality researchers to analyse the available open-source air quality data.

Open-Source air quality data can be downloaded from 3 sources - [CPCB, specific to India](https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing), [OpenAQ](https://openaq.org/#/countries/IN?_k=5ecycz), and
[AirNow](https://www.airnow.gov/international/us-embassies-and-consulates/#India). The data can be uploaded only in .csv or .xlsx format. Data from sources specified here, sometimes contains fill values which can be due to instrument malfunction or unavailability of data. The data will be as it is except that it will remove fill values from the data. This app is currently designed to read the preset format of the input file. Any alterations in the input file may lead to unpredictable results. The title and axis label of the plots generated can be modified as per user input. 

The package `pollucheck` analyses, visualises data downloaded from three different open-source data providers, the app generates: summary statistics, time-series plots and performs simple statistical tests. It also helps check the quality of the data. This app will provide a workflow for air quality researchers interested in working with open source datasets. This app can process all parameters except Wind Direction which when downloaded at 1 hour is processed correctly, any other time-resolution is used then the app will not process wind direction correctly. 

We have implemented two functions from a widely used package in air quality `openair` called `calendarPlot` and `timeVariation`.


## App Display

The output is displayed in five different tabs.

1) `File` displays cleaned data.
2) `Summary` displays summary statistics for all parameters and an option of daily and monthly summary statistics is also available.
3) `Summary Plots` generates data availability plot for all parameters, time series, monthly box-plots, vertical bar plots and diurnal plots for the selected parameter.
4) `Statistical Plots` checks for normality, generates density, Q-Q plot along with checks for trends in the time series for the selected parameter.
5) `Linear Regression` implements linear or multilinear regression using the selected parameters.
6) `Compare` allows users to upload another set of data to compare selected parameters and generate plots.
7) `openair` allows users to use the package's widely used functions for the selected parameters.
8) `FAQ's` helps users to understand this software.
9) `Disclaimer` contains a disclaimer to use this package.


## Limitations

1) Data cannot be pulled from the cloud, it has to be downloaded.
2) Multiple files cannot be uploaded at a single time. 
3) Only data downloaded from the specified sources can be analysed.

## Installation

`pollucheck` can be installed from [GitHub](https://github.com/).

Load and run `pollucheck` as follows:

``` r
devtools::install_github("adithirgis/pollucheck")
library(pollucheck)
pollucheck::pollucheck_run()
```
A pre-loaded dataset appears which is a file downloaded from Central Pollution control Board, Bangalore, India.


# Acknowledgements

We wish to thank Prof. Julian Marshall (University of Washington, Seattle), Prof. Joshua Apte (University of California, Berkeley), Dr. Jai Asundi (CSTEP), Dr. Saumya Singh (University of California, Berkeley) and R Ladies community for their help and support.

# References
