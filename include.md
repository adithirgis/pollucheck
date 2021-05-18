
# Frequently Asked Questions

### File

**What does the app do?**  
*This app helps to analyze and visualize open source air quality data
available. An example dataset is already loaded to help you walk through
all the features of the app. If you need help with downloading your own,
go to the next question.*

**Where can we download the air quality e data from?**  
*Air Quality data from India can be downloaded from 3 sources - [CPCB
website](https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-landing),
[OpenAQ](https://openaq.org/#/countries/IN?_k=5ecycz), and
[AirNow](https://www.airnow.gov/international/us-embassies-and-consulates/#India).*

**Can I use this app to analyze app work for data from countries other
than India?**  
*Yes, two of the sources mentioned above OpenAQ, and AirNow have
datasets from many other countries. CPCB is specific to India. Make sure
you specify the correct timezone in the app when analyzing data from
different countries.*

**How to use the app?**  
*Please check
[here](https://github.com/adithirgis/OpenSourceAirQualityApp) for a walk
through.*

**Will this application read data from a reference monitor which is not
from the sources mentioned in the app?**  
*No, this app can only do post-processing of the data downloaded from
the mentioned sources.*

**What are the different file formats which can be uploaded to this
app?**  
*The data can be uploaded only in .csv or .xlsx format.*

**What if I do not use any quality check options?**  
*Data from sources specified here, sometimes contains fill values which
can be due to instrument malfunction or unavailability of data. The data
will be as it is except that it will remove fill values from the data.*

**What is the naming convention of downloaded files?**  
\*The File tab downloaded data is the hourly or daily average so
\_average.csv, while the summary statistics file has an extension of
\_summary.csv, and the diurnal table download will have a suffix of
\_diurnal.csv.\*

**What are the different packages used in the app?**  
*The different packages we use here are - tidyverse, ggplot2, openair,
lubridate, shiny, bslib, forecast, biwavelet, readxl, DT, data.table,
nortest, janitor, zoo.*

**Can the input file be manipulated/edited?**  
*No. This app is currently designed to read the preset format of the
input file. Any alterations in the input file may lead to unpredictable
results.*

**What is the standard outlier condition?**  
*The standard outlier detection condition is to calculate the daily mean
and standard deviation and then removing values that are lower than Mean
subtracted by x times SD and greater than Mean added to x times SD,
where x is any real number.*

**Why should we remove repeated consecutive measurements?**  
*Sometimes when the instrument breaks down, it tends to show exactly the
same consecutive values (often this value is the last measured value),
it is suggested that such consecutive values be removed from the
dataset*

**Why should we remove values above 9999 from the data?**  
*Usually, values above 9999 are incorrect, also because the instruments
usually measure only up to 999 values in PM instruments. This can be
removed using this filter option, along with this 9999 could also be a
fill value.*

**What are the available time resolutions for plots and tables?**  
*Only hourly and daily average plots can be generated in this
application, although the input data can be of 60, 30, and 15 minutes
time resolution.*

**How to export plots?**  
*To save the image right click on the plot and select save image as. You
can save it on your local computer in .png or .jpeg format.*

**Can we upload datasets of different timezone?**  
*Yes, for data from OpenAQ and AirNow, different time formats are
supported, just select the right timezone.*

**Are the plots customizable?**  
*Only the axis labels and Plot title can be customized.*

**Whom to contact for doubts or suggestions?**  
*We are happy to incorporate more features based on what users need.
Write to us at <contact@ilklabs.com>. For reporting any bug/issue click
[here](https://github.com/adithirgis/OpenSourceAirQualityApp/issues).*

**Can the plots generated using PolluCheck be used for publications?**  
*Depends on the user. If you find the resolution and everything usable,
please go ahead. Remember to cite us!*

**How to cite PolluCheck?**  
*We will update this soon!*

**Is the theme of the application customizable?**  
*No, the theme of the app is not customizable, if you have any
suggestions and resources, please Pull Request in the github repo link
provided above.*

### Summary

**What is the naming convention of the file downloaded?**  
\*The data downloaded in this tab will have a suffix of \_average.csv.\*

### Summary Plots

**What does data availability plot indicate?**  
*The data availability plot shows the available percentage of daily data
points for each parameter.*

**How to read the box plots?**  
*A boxplot is a way to show a [five-number
summary](https://www.statisticshowto.com/statistics-basics/how-to-find-a-five-number-summary-in-statistics/)
in a chart. The main part of the chart (the “box”) shows where the
middle portion of the data is: the interquartile range. At the ends of
the box, you’ll find the first
[quartile](https://www.statisticshowto.com/what-are-quartiles/)(the 25%
mark) and the third quartile (the 75% mark). The whiskers on the far
most of the box plots show the minimum and maximum values in the
datasets. Finally, the median is represented by a vertical bar in the
center of the box.
([Source](https://www.statisticshowto.com/probability-and-statistics/descriptive-statistics/box-plot/))*

**What do the bars in the vertical bar plots indicate?**  
*The vertical bars represent mean + sd and mean - sd.*

**What is the difference between month-yearly plot and monthly plot?**  
*The month-yearly plot displays box plot based on each month and year,
while monthly plot displays the plot for all months in multiple years.*

**What are the bars in the diurnal plot and why are there two
options?**  
*Diurnal plots represent the time of the day variability using mean (of
that hour) and sd or median (of that hour) and IQR (InterQuartile
Range). So when you choose mean and sd the bars represent sd, while when
you choose median and IQR, the bars represent IQR.*

**Can we plot ratios of different pollutants on a time series?**  
*No, this feature is not implemented yet.*

### Statistical Plots

**What is the difference between the Shapiro-Wilk normality test and the
Anderson- Darling normality test?**  
*Shapiro-Wilk test is used to check for normality when the sample size
is below 5000, while Anderson- Darling test is used when sample size is
greater than 5000 if you have data points above 5000, please check
Anderson-Darling test.*

**How is the data imputed for trend analysis?**  
*For imputing values in the discontinuous data set we use [forecast
package](https://cran.r-project.org/web/packages/forecast/forecast.pdf),
please check the resource for more details.*

**What packages were used for Trend Analysis?**  
*Trend Analysis is available for daily values alone. For trend analysis
using Mann-Kendall test we use
[mk.test](https://www.rdocumentation.org/packages/trend/versions/1.1.4/topics/mk.test).
For imputing values in the discontinuous data set we use [forecast
package](https://cran.r-project.org/web/packages/forecast/forecast.pdf).
For continuous wavelet transform we use [biwavelet
package](https://cran.r-project.org/web/packages/biwavelet/biwavelet.pdf).
In periodicity analysis, the contours covered by black lines represent
the significant periodicity at 95% significant 519 level.*

**What are the time resolutions used for Trend Analysis?**  
*Daily mean and monthly mean is used to perform Trend Analysis by
Mann-Kendall test.*

**What are the time resolutions used for Periodicity Analysis?**  
*Daily mean data is used to generate the biwavelet. We have used the
default options in the biwavelet package to generate the plot.*

**How is Q-Q Plot different from a Scatter Plot?**  
*A Q-Q plot is a scatterplot created by plotting two sets of quantiles
against one another. If both sets of quantiles come from the same
distribution, we should see the points forming a line that’s roughly
straight.
([Source](https://data.library.virginia.edu/understanding-q-q-plots/#:~:text=A%20Q%2DQ%20plot%20is%20a,truly%20come%20from%20Normal%20distributions.))*

### Linear Regression

**What are univariable and multivariable?**  
*univariable regression is which consist of single independent variable
while multivariable consist of three or more variables.*

**What is the difference between multiple R- square and adjusted R-
square?**  
*Multiple R squared is simply a measure of Rsquared for models that have
multiple predictor variables. Therefore it measures the amount of
variation in the response variable that can be explained by the
predictor variables. The fundamental point is that when you add
predictors to your model, the multiple Rsquared will always increase, as
a predictor will always explain some portion of the variance. Adjusted
Rsquared controls against this increase, and adds penalties for the
number of predictors in the model. Therefore it shows a balance between
the most parsimonious model, and the best fitting model. Generally, if
you have a large difference between your multiple and your adjusted
Rsquared that indicates you may have overfit your model.
[source](https://stats.stackexchange.com/questions/241283/what-is-the-main-difference-between-multiple-r-squared-and-adjusted-r-squared/241298)*

**How to check if the data is significant or not?**  
*The p-value in the last column tells you the significance of the
regression coefficient for a given parameter. If the p-value is small
enough to claim statistical significance, that means there is strong
evidence that the coefficient is different from 0.
[source](https://stats.stackexchange.com/questions/37912/how-to-determine-which-variables-are-statistically-significant-in-multiple-regre)*

**What are residuals?**  
*The data points usually don’t fall exactly on the regression equation
line; they are scattered around. A residual is the vertical distance
between a data point and the regression line.
[source](https://www.statisticshowto.com/residual/)*

**What are degrees of freedom?**  
*The degrees of freedom indicate the number of independent values that
can vary in an analysis without breaking any constraints.
[source](https://statisticsbyjim.com/hypothesis-testing/degrees-freedom-statistics/)*

### Compare

**Can I compare two datasets of different timezone?**  
*No, right now the app allows for comparison with data from the same
time zone.*

**Can I compare two datasets of different time periods?**  
*Yes, you can compare the time-series of both the datasets, but the
scatter plot will not be generated as they belong to different time
series.*

**Can I compare datasets from different downloading sources (different
websites)?**  
*Yes, you can definitely compare from different available sources.*

**How many datasets can be compared at a time in the app?**  
*As of now, only one single file can be compared with the uploaded
dataset.*

**Is it possible to compare two datasets with different time
resolutions?**  
*Yes! It is possible to compare datasets with different time resolutions
since the application averages to 1 hour or daily average.*

### openair

**What is openair?**  
*openair is an R package developed for the purpose of analysing air
quality data - or more generally atmospheric composition data. We use
functions from openair to plot. The openair package usage and other
details can be found [here](https://github.com/davidcarslaw/openair).*

**What is the difference between “median and quantiles” and “mean and
95% confidence intervals” in Time Variation Plot?**  
*Since the Time Variation plot shows the variation in parameters based
on different time resolutions. It is the same option as time variation
plots to plot mean and sd or median and IQR.*

**How many different plots can be created using openair?**  
*Right now, this app supports only two plots - calendar plot and
temporal variation plot.*

**Where can I find the documentation of openair?**  
*Please click
[here](https://cran.r-project.org/web/packages/openair/openair.pdf).
Also, remember to cite them!*

**What are the ambient levels in India?**  
*Please check the
[link](https://app.cpcbccr.com/ccr_docs/FINAL-REPORT_AQI_.pdf) for
details.*
