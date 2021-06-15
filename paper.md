---
title: 'pollucheck v1.0: A package to explore open-source air pollution data'
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
date: "12 June 2021"
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

Air pollution is one of the major environmental threats impacting human health, quality of living, climate and economy [@Hystad:2020]. Quantification of the pollution is needed to assess its impact on the above said aspects and device mitigation actions. Measurements are the most accurate way of quantification of air pollution. Many countries conduct regulatory measurements of various air pollutants (e.g. fine and respirable particulate matter, nitrogen dioxide, sulfur dioxide, surface ozone etc.) and make the data available publicly.

Air pollution datasets typically span several seasons, years and the real-time data is recorded typically every hour or a higher frequency. With ever increasing quantum of data and number of data providers, there is a clear need for tools to handle, analyse and visualise large datasets. `Shiny` is a package in R which helps in building interactive applications [@Shiny:2021]. The current shiny app `pollucheck` aims at a simple workflow to generate a suite of statistical plots and summary statistics; users do not need any programming background to be able to analyse time-series data and generate a variety of plots.

`pollucheck` can handle real-time pollution and colocated meteorological (if available) data from three most popular open source air pollution databases. The data sources include [OpenAQ](openaq.org), [Airnow](airnow.gov) and [Indian Central Pollution Control Board (CPCB) dashboard](app.cpcbccr.com). While CPCB data is specific to Indian regulatory monitoring stations, OpenAQ hosts the global open source pollution databases and Airnow hosts the global PM~2.5~ (mass concentration of particulate matter with aerodynamic diameter less than or equal to 2.5 microns) data being collected under the United States Embassy and Consulates' air quality monitoring program.

Pollution data from the above sources is typically in different file formats and templates requiring customised codes/programs for analysis. Also a rigorous quality check of these data is preferred before visualization (plotting) and/or reporting. `pollucheck` offers a single stop solution for

(i) handling the pollution data from the above said open source databases,
(ii) applying a suite of quality check options,
(iii) generating a variety of summary statistics at various averaging intervals,
iv) performing time-series analysis,
(v) generating a bunch of temporal and statistical plots,
(vi) comparing data from two input files.

To our knowledge, currently there is no application that can generate utilisable summary statistics and plots using the data from the above mentioned pollution databases. Though there are a few shiny apps that deal with data cleaning and visualisation for pollution data collected from single/multiple air quality instruments [@Salmon:2017; @Upadhya:2020]. 


# App Display

The output of `pollucheck` is displayed in seven tabs. Different packages used for building `pollucheck` include `tidyverse`, `openair`, `shiny`, `bslib`, `forecast`, `biwavelet`, `readxl`, `DT`, `data.table`, `nortest`, and `zoo`.

i)  The `File` tab is used to upload the input file and to specify the source and time-resolution of the input data. The default time zone is set to *Asia/Kolkata*. For OpenAQ and Airnow datasets, appropriate time zones need to be selected based on the input file. For the CPCB dataset, the time zone option is default and inactive. A set of quality check options, which include (i) removal of negative values, (ii) removal of duplicate consecutive values, (iii) detection of outliers are provided. Data completeness criteria (minimum percent data required) for computing daily mean values can be specified. If the input file contains simultaneous PM~2.5~ and PM~10~ (mass concentration of particulate matter with aerodynamic diameter less than or equal to 10 microns) data, the app computes the PM~2.5~/PM~10~ ratio (a useful metric in the air pollution field). The selected quality check/completeness criteria will be applied to all the parameters of the input file. Hourly or daily mean values of all the parameters can be displayed and downloaded (as `.csv`) from this tab.

ii) `Summary` tab provides various statistics (central tendencies, percentiles, minimum, maximum, standard deviation, interquartile range, etc.) for all the parameters in the input file at three different averaging intervals. The averaging interval can be selected using the drop-down menu. The displayed statistics can be downloaded.

iii) `Summary Plots` tab generates (i) data availability plot for all the parameters (based on daily mean values), (ii) time-series plot (iii) box and whisker plots (iv) vertical bar plot and (v) diurnal variability plots. Except for data availability plot, the parameter of interest to plot needs to be selected from the drop-down menu. Plots can be generated either using hourly or daily mean data. Diurnal variability plot can be plotted either by aggregating the whole data in the input file or month wise. Considering the general log normal nature of the pollution data, an option is provided for the diurnal variability plot to be plotted either using mean and standard deviation or median and interquartile range. The title and Y-axis labels of all the plots are editable. 


iv) `Statistical Plots` tab can be used to conduct normality tests (Anderson-Darling and Shiparo-Wilk), generate density and Quantile-Quantile (QQ) plots, generate autocorrelogram, conduct trends and periodicity analysis on the parameter selected. While the autocorrelogram is generated based on monthly mean values, trend (Mann-Kendall test) and periodicity (wavelet analysis) analysis are conducted on daily mean values of the selected parameter. For trend and periodicity analysis and generating autocorrelogram, the missing daily mean values are imputed using the `forecast` package [@Hyndman:2008].

v)  `Linear Regression` tab offers to perform univariable and multiple linear regression analysis among the parameters of choice. For univariable linear regression, a scatter plot will be generated with least squares linear fit. For multiple linear regression, multiple independent parameters can be selected. A scatter plot between the dependent variable and fitted data (using regression coefficients) will be generated. Relevant statistical coefficients are provided along with the plots. 

vi) `Compare` tab allows users to upload a second data file to compare data between the selected parameters from the two input files. The selected quality check criteria conditions applied on the parameters of the first input file will be automatically applied on the parameters in the second input file. Time series, scatter and diurnal variability plots of the two parameters of interest will be generated.

vii) Some features of the widely used `openair` package [@Carslaw:2021] are integrated in `pollucheck` with permission. Calendar and time variation plots of the selected parameter are generated in this tab. For calendar plots daily data will be used and for time variation plots hourly data will be used.

An extensive list of FAQs (frequently asked questions) is provided as a separate tab for better understanding of the `pollucheck` functioning, detailed features of the plots, analysis and the various packages used to build 'pollucheck'.

# Limitations

1)  `pollucheck` cannot download data itself from the cloud. Downloaded files need to be provided as input.
2)  Multiple files cannot be uploaded to `pollucheck` at a given time.
3)  The current version of `pollucheck` is limited to accepting real time data files from only three data sources.
4)  Few analyses (e.g. periodicity analysis) can be performed using daily mean values only.
5)  A caution needs to be exercised when using the averaged wind direction data.
6)  Any manipulation or alteration to the downloaded file before giving it as input to the app can lead to erroneous results.

## Installation

`pollucheck` can be installed from [GitHub](https://github.com/).

Load and run `pollucheck` as follows:

``` {.r}
devtools::install_github("adithirgis/pollucheck")
library(pollucheck)
pollucheck::pollucheck_run()
```

`pollucheck` is furnished with a pre-loaded dataset for a quick user tour of the analysis, plotting options and functions available. In the `Compare` tab, the pre-loaded data acts as the second input file, if no second file is uploaded.

# Acknowledgements

We wish to thank Prof. Julian D Marshall (University of Washington, Seattle), Prof. Joshua Apte (University of California, Berkeley), Dr. Jai Asundi (Center for Study of Science, Technology & Policy, Bengaluru), Dr. Saumya Singh (University of California, Berkeley) and R Ladies community for their help and support.


# References