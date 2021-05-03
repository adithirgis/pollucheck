
# Have a look at the app!

### We are in this together!

A walk through to use this app for everyone -

How to Download Data from the CPCB website?

-   Example - Where do you live in India?

-   Find the nearest [CPCB
    station](https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing)
    to download data from a regulatory air quality monitor.

-   Visit [CPCB
    website](https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing)
    to access the Central/State Pollution Control Board Data.

![](WWW/CPCB_data_down_S.jpeg)

-   Select the Indian state from the “State Name” dropdown.

![](WWW/CPCB_Station.jpeg)

-   Now select the city for which the data needs to be downloaded using
    the “City Name” dropdown menu.

![](WWW/CPCB_Station_city.jpeg)

-   Now from the “Station Name” drop-down select the desired station.

-   Select the Parameters. Note- Multiple parameters can be selected at
    a time.

![](WWW/CPCB_Station_parameters.jpeg)

-   Report Format- To use the PolluCheck app, Please keep the format as
    “tabular”.

-   Criteria- This drop-down will help you to select between different
    time averaging of data. Note- Pollucheck app only supports 15 min,
    30 min, and 60 min average data.

-   Select the Start Date and End date of the data and click on
    “Submit”.

-   Download that data (15, 30, 60 min resolution would be good).

![](WWW/CPCB_Station_TA.jpeg)

### Have a look at the app usage

-   Select the source from where the data was downloaded.
-   Now select the time resolution at which the data was downloaded.

<img src="WWW/image_1.JPG" width="315"/>

-   Select the check box according to your need.

    -   Remove Negative values- Negative values do not represent
        concentration,they represent missing values, so it is always
        advised to remove them.This option helps you to remove all the
        negative values from your entire dataset.

    -   Remove duplicate consecutive values- Sometimes when the
        instrument breaks down, it tends to show exactly same
        consecutive values, it is advised to remove these as well. This
        feature removes consecutive repetitive values in your dataset.

    -   Specify a multiple (X) to remove outliers based on Mean and SD-
        If you want to clean your dataset based on outliers, not usually
        necessary, use only if you want to remove outiliers based on
        Mean and Standard Deviation values.

    -   Specify % of data completeness for computing daily mean values-
        If you are looking for entire/complete dataset to be present for
        analysis and not less, you can use this to select the desired
        level of completeness in a day using the scroll bar.

    -   Remove PM2.5 and PM10 above- Usually, values above 9999 are
        incorrect, also because the instruments usually measure only to
        999 values in PM instruments. This can be removed using this
        filter option.

    <img src="WWW/image_2.JPG" width="410"/>

-   Output aggregation- The uploaded data can be converted into daily or
    hourly mean values.

-   “Download as csv” or click on “Show Data” to see the data in the
    app.

![](WWW/image_3.JPG)

-   Look at the time series of pollutant concentrations in the **Plots**
    tab (time series are plots with x axis representing time). Do you
    see patterns? Are there times of the month or times of the day where
    concentrations are particularly higher or lower? Are there
    particular months in a year that are more polluted than others?
-   Think about sources in the particular location: traffic, industries,
    garbage burning, etc.
-   What more do you want to learn? Talk to the Humans of ILK.
-   What more do you want to learn about the data? We are happy to
    incorporate more features based one what users need. Write to us at
    <contact@ilklabs.com>.
-   If you have feature request - open an issue
    [here](https://github.com/adithirgis/OpenSourceAirQualityApp).

### The application has various tabs - look at each of them and their usage.

##### File tab displays the data after cleaning process.

<img src="WWW/image_4.JPG" style="width:100.0%" />

##### Summary tab displays the summary statistics for daily, monthly or for the entire dataset.

<img src="WWW/image_5.JPG" style="width:100.0%" />

##### Summary Plots tab generates time series, box plot, and diurnal plot of the selected parameter.

-   Data availability plot of all the pollutants after the cleaning
    process can be generated.

    ![](WWW/DA_plot.JPG)

-   The parameter to plot and the data aggregation options are
    available.

    ![](WWW/image_6.JPG)

-   Options to edit the Title and axis labels are available.

-   Time-series plot

    ![](WWW/image_7.JPG)

    <img src="WWW/image_8.JPG" style="width:100.0%" />

-   Month and year box plot

    ![](WWW/image_9.JPG)

-   Monthly box plot

    ![](WWW/image_10.JPG)

-   Vertical bar plot

    ![](WWW/image_11.JPG)

-   Diurnal pot using hourly values - has two types using all data or
    distributed month wise. There is an option to plot point and bars as
    Median and IQR respectively or Mean and Standard Deviation. The data
    used for plotting can be downloaded as csv file.

    ![](WWW/image_12.JPG)

    <img src="WWW/image_13.JPG" style="width:100.0%" />

##### Statistics Plots tab tests for normality, pattern and generates density plot, qq plot of the selected parameter.

-   Using a selected parameter and aggregation methods, normality test
    using the Anderson Darling test (for N &gt; 500) or Shapiro-Wilk
    test can be conducted.

-   Density plot

    ![](WWW/image_14.JPG)

    <img src="WWW/image_15.JPG" style="width:100.0%" />

-   QQ plot

    ![](WWW/image_16.JPG)

    <img src="WWW/image_17.JPG" style="width:100.0%" />

-   Trend Analysis is also available for daily values. For trend
    analysis using Mann-Kendall test we use
    [mk.test](https://www.rdocumentation.org/packages/trend/versions/1.1.4/topics/mk.test).
    For imputing values in the discontinuous data set we use [forecast
    package](https://cran.r-project.org/web/packages/forecast/forecast.pdf).
    For continuous wavelet transform we use [biwavelet
    package](https://cran.r-project.org/web/packages/biwavelet/biwavelet.pdf).
    In periodicity analysis, the contours covered by black lines
    represent the significant periodicity at 95% significant 519 level.

    ![](WWW/image_19.JPG)

    <img src="WWW/image_18.JPG" style="width:100.0%" />

##### Linear Regression tab does univariate and multivariate linear regression for selected parameter(s).

-   There is an option of plotting univariate linear regression plots
    between various parameters available.

    ![](WWW/image_20.JPG)

    <img src="WWW/image_21.JPG" style="width:100.0%" />

-   Also multivariate linear regression can be performed.

    ![](WWW/image_22.JPG)

    <img src="WWW/image_23.JPG" style="width:100.0%" />

##### Compare tab allows users to upload another file for comparision with the loaded data.

-   The Compare tab allows user to upload data from another site for
    comparison and generate time series and a scatter plot between
    parameters selected from different sites.

    ![](WWW/image_24.JPG)

    ![](WWW/image_25.JPG)

    <img src="WWW/image_26.JPG" style="width:100.0%" />

##### `openair` tab allows users use the package’s widely used functions for the selected parameter.

-   Calendar plot

    ![](WWW/image_27.JPG)

    <img src="WWW/image_28.JPG" style="width:100.0%" />

-   Time variation plot

    <img src="WWW/image_29.JPG" style="width:100.0%" />

### Explore yourself!

##### The major pollutants annual standards are plotted below.
