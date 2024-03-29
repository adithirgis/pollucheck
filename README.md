# pollucheck: Open-Source Air Quality App!


[![R-CMD-check](https://github.com/adithirgis/pollucheck/workflows/R-CMD-check/badge.svg)](https://github.com/adithirgis/pollucheck/actions) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5128607.svg)](https://doi.org/10.5281/zenodo.5128607)[![DOI](https://joss.theoj.org/papers/10.21105/joss.03435/status.svg)](https://doi.org/10.21105/joss.03435)

pollucheck helps exploring the open-source air quality data.

-   pollucheck allows users to handle open-source air quality data sets available from [OpenAQ](https://openaq.org/#/countries), Central Pollution Control Board [CPCB](https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing), and [AirNow](https://www.airnow.gov/international/us-embassies-and-consulates/).
-   Users can visualize data, analyze data, perform basic statistical operations, and generate a variety of publication-ready plots.
-   We have also included the popular [openair](https://cran.r-project.org/web/packages/openair/index.html) package in this app.
-   We have hosted this app here - https://aruapps.shinyapps.io/OpenSourceAirQualityApp/ 

### We are in this together!

A walk through to use this app for everyone -

#### Download data - CPCB website

-   Example - Where do you live in India?

-   Find the nearest [CPCB station](https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing) to download data from a regulatory air quality monitor.

-   Visit [CPCB website](https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing) to access the Central/State Pollution Control Board Data.

![](inst/shiny/WWW/CPCB_data_down_S.jpeg)

-   Select the Indian state from the "State Name" drop-down.

![](inst/shiny/WWW/CPCB_Station.jpeg)

-   Now select the city for which the data needs to be downloaded using the "City Name" drop-down menu.

![](inst/shiny/WWW/CPCB_Station_city.jpeg)

-   Now from the "Station Name" drop-down select the desired station.

-   Select the Parameters. Note- Multiple parameters can be selected at a time.

![](inst/shiny/WWW/CPCB_Station_parameters.jpeg)

-   Report Format- To use the pollucheck app, Please keep the format as "tabular".

-   Criteria- This drop-down will help you to select between different time averaging of data. Note- pollucheck app only supports 15 min, 30 min, and 60 min average data.

-   Select the Start Date and End date of the data and click on "Submit".

-   Download that data (15, 30, 60 min resolution would be good).

![](inst/shiny/WWW/CPCB_Station_TA.jpeg)

#### Download data - OpenAQ 

- Click on [OpenAQ](https://openaq.org/#/countries/IN?_k=5ecycz) in the app to download the OpenAQ data set.

<img src="inst/shiny/WWW/oaq_1.jpg" width="286"/>

- Now, click on the *Download* option on your browser.

<img src="inst/shiny/WWW/oaq_2.jpg" width="517"/>

- In data download, you can download the data by *Locations* or by *Datasets*.

<img src="inst/shiny/WWW/oaq_3.jpg" width="484"/>

- Now select the desired, *Country*, *City/Region* and *Location* using the drop-down.

<img src="inst/shiny/WWW/oaq_4.jpg" width="306"/><img src="inst/shiny/WWW/oaq_5.jpg" width="103"/><img src="inst/shiny/WWW/oaq_6.jpg" width="215"/>

- To proceed further select the *Start Date* and the *End Date* for the data.

<img src="inst/shiny/WWW/oaq_7.jpg" width="525"/>

- Now select which type of sensor is available at that location. Is it a *Low-cost Sensor* or *Reference Grade* monitor?

![](inst/shiny/WWW/oaq_8.jpg)

- Finally, select the parameters from the list of *Core* and *Additional Parameters* which you wish to download from that particular location and click on *Download Selection*. The file will be saved in your local disk in .csv format.

<img src="inst/shiny/WWW/oaq_9.jpg" width="519"/>

#### Download data - AirNow 

- Click on [AirNow-US Embassies](https://www.airnow.gov/international/us-embassies-and-consulates/) to visit official website to download the data.

<img src="inst/shiny/WWW/a_1.JPG" width="557"/>

- Select the desired city and the parameters.

<img src="inst/shiny/WWW/a_2.JPG" width="554"/>

- Now go to the *Historical* sub-menu on the Homepage and download the desired file.

<img src="inst/shiny/WWW/a_3.JPG" width="557"/>

### App usage

- Select the source from where the data was downloaded.

- Now select the time resolution at which the data was downloaded.

<img src="inst/shiny/WWW/image_1.JPG" width="315"/>

- Select the check box according to your need.

    -   Remove Negative values- Negative values do not represent concentration,they represent missing values, so it is always advised to remove them.This option helps you to remove all the negative values from your entire data set.

    -   Remove duplicate consecutive values- Sometimes when the instrument breaks down, it tends to show exactly same consecutive values, it is advised to remove these as well. This feature removes consecutive repetitive values in your data set.

    -   Specify a multiple (X) to remove outliers based on Mean and SD- If you want to clean your data set based on outliers, not usually necessary, use only if you want to remove outliers based on Mean and Standard Deviation values.

    -   Specify % of data completeness for computing daily mean values- If you are looking for entire/complete data set to be present for analysis and not less, you can use this to select the desired level of completeness in a day using the scroll bar.

    -   Remove PM2.5 and PM10 above- Usually, values above 9999 are incorrect, also because the instruments usually measure only to 999 values in PM instruments. This can be removed using this filter option.

<img src="inst/shiny/WWW/image_2.JPG" width="410"/>

- Output aggregation- The uploaded data can be converted into daily or hourly mean values.

- "Download as csv" or click on "Show Data" to see the data in the app.

![](inst/shiny/WWW/image_3.JPG)

- Look at the time series of pollutant concentrations in the **Plots** tab (time series are plots with x axis representing time). Do you see patterns? Are there times of the month or times of the day where concentrations are particularly higher or lower? Are there particular months in a year that are more polluted than others?
- Think about sources in the particular location: traffic, industries, garbage burning, etc.
- What more do you want to learn? Talk to the Humans of ILK.

### App tabs

##### File tab 

- Displays the data after cleaning process.

![](inst/shiny/WWW/image_4.JPG)

##### Summary tab 

- Displays the summary statistics for daily, monthly or for the entire data set.

![](inst/shiny/WWW/image_5.JPG)

##### Summary Plots tab 

- generates time series, box plot, and diurnal plot of the selected parameter.

- Data availability plot of all the pollutants after the cleaning process can be generated.

![](inst/shiny/WWW/DA_plot.JPG)

- The parameter to plot and the data aggregation options are available.

![](inst/shiny/WWW/image_6.JPG)

- Options to edit the Title and axis labels are available.

- Time-series plot

![](inst/shiny/WWW/image_7.JPG)

![](inst/shiny/WWW/image_8.JPG)

- Month and year box plot

![](inst/shiny/WWW/image_9.JPG)

- Monthly box plot

![](inst/shiny/WWW/image_10.JPG)

- Vertical bar plot

![](inst/shiny/WWW/image_11.JPG)

- Diurnal pot using hourly values - has two types using all data or distributed month wise. There is an option to plot point and bars as Median and IQR respectively or Mean and Standard Deviation. The data used for plotting can be downloaded as csv file.

![](inst/shiny/WWW/image_12.JPG)

![](inst/shiny/WWW/image_13.JPG)

##### Statistical Plots tab 

- Tests for normality, pattern and generates density plot, qq plot of the selected parameter.

- Using a selected parameter and aggregation methods, normality test using the Anderson Darling test (for N \> 500) or Shapiro-Wilk test can be conducted.

- Density plot

![](inst/shiny/WWW/image_14.JPG)

![](inst/shiny/WWW/image_15.JPG)

- Q-Q plot

![](inst/shiny/WWW/image_16.JPG)

![](inst/shiny/WWW/image_17.JPG)

- Trend Analysis is also available for daily values. For trend analysis using Mann-Kendall test we use [mk.test](https://www.rdocumentation.org/packages/trend/versions/1.1.4/topics/mk.test). For imputing values in the discontinuous data set we use [forecast package](https://cran.r-project.org/web/packages/forecast/forecast.pdf). For continuous wavelet transform we use [biwavelet package](https://cran.r-project.org/web/packages/biwavelet/biwavelet.pdf). In periodicity analysis, the contours covered by black lines represent the significant periodicity at 95% significant 519 level.


![](inst/shiny/WWW/image_30.JPG)

<img src="inst/shiny/WWW/image_31.JPG" width="590"/>

<img src="inst/shiny/WWW/image_18.JPG" width="590"/>

##### Linear Regression tab 

- There is an option of plotting linear regression plots between various parameters available.

![](inst/shiny/WWW/image_20.JPG)

![](inst/shiny/WWW/image_21.JPG)

- Also multi linear regression can be performed.

![](inst/shiny/WWW/image_22.JPG)

![](inst/shiny/WWW/image_23.JPG)

##### Compare tab 

- Allows user to upload data from another site for comparison and generate time series and a scatter plot between parameters selected from different sites.

- There are options to generate time series, scatter plot / linear regression and diurnal plots for both the sites.

![](inst/shiny/WWW/image_24.JPG)

<img src="inst/shiny/WWW/image_32.JPG" width="397"/>

![](inst/shiny/WWW/image_26.JPG)

##### `openair` tab 

- allows users use the package's widely used functions for the selected parameter.

- Calendar plot

![](inst/shiny/WWW/image_27.JPG)

![](inst/shiny/WWW/image_28.JPG)

- Time variation plot

![](inst/shiny/WWW/image_29.JPG)

## Installation

`pollucheck` is hosted online on *shinyapps.io* and can be installed to serve locally from [GitHub](https://github.com/).

Load and run `pollucheck` as follows:

``` {.r}
install.packages("devtools")
devtools::install_github("adithirgis/pollucheck")
pollucheck::pollucheck_run()
```

`pollucheck` is furnished with a preloaded data set for a quick user tour of the analysis, plotting options and the functions available. In the `Compare` tab, the preloaded data set acts as the second input file if no second file is uploaded.

## Community guidelines

1. [Contribute to the software](.github/CONTRIBUTING.md)

2. Report issues or problems with the software / Seek Support

- Please open an issue in the [issue tracker of the project.](https://github.com/adithirgis/pollucheck/issues)

3. Contributors must adhere to the [Code of Conduct](.github/CODE_OF_CONDUCT.md).

4. We are happy to incorporate more features based one what users need. Write to us at [contact\@ilklabs.com](mailto:contact@ilklabs.com).

## Author credit statement

Adithi R. Upadhya created the package, Meenakshi Kushwaha supervised and will maintain. Pratyush Agrawal contributed to designing, testing the app and data collection while Sreekanth Vakacherla designed and supervised. All the authors contributed to the manuscript. 
