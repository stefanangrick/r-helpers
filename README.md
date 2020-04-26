R helper functions
================

This repository contains a set of helper functions to simplify common R
tasks, along with
[examples](https://github.com/stefanangrick/r-helpers/tree/master/examples).

## Plotly chart helpers

`plotly_chart_helpers.R` contains a set of helper functions to quickly
visualise time series data using [plotly](https://plotly.com/). The code
creates interactive charts that allow the user to selectively display
series and switch between transformations (level, quarterly and annual
growth rates). To use these functions, make sure the
[plotly](https://cran.r-project.org/web/packages/plotly/index.html),
[xts](https://cran.r-project.org/web/packages/xts/index.html),
[zoo](https://cran.r-project.org/web/packages/zoo/index.html), and
[lubridate](https://cran.r-project.org/web/packages/lubridate/index.html)
packages are installed on your system, then download the file
`plotly_chart_helpers.R` and run `source("plotly_chart_helpers.R")`.

Line plots are created via:

``` r
plotly_line_growth(xts_a, xts_b = NULL, x_lab = "", y_lab = "", add_q = TRUE,
                   add_y = TRUE, rate = TRUE, colpal = mycolpal,
                   l_orientation = "h")
```

where `xts_a` is an
[xts](https://cran.r-project.org/web/packages/xts/index.html) object
containing the data to be plotted, possibly accompanied by an optional
secondary `xts_b` object of different frequency. `x_lab` and `y_lab`
specify x and y axis labels, respectively. `add_q` and `add_y` toggle
quarterly/yearly transformations. `rate` determines if the growth rate
is to be calculated (`TRUE`), or the difference (`FALSE`). `colpal`
controls the colour palette, `l_orientation` the location of the legend.

Bar plots are created via:

``` r
plotly_line_bar_growth(xts_obj, x_lab = "", y_lab = "", add_q = TRUE,
                       add_y = TRUE, add_shr = TRUE, bar_mode = "relative",
                       rate = TRUE, add_others = TRUE, check_threshold = 0.1,
                       colpal = mycolpal, l_orientation = "h")
```

where `xts_obj` is the
[xts](https://cran.r-project.org/web/packages/xts/index.html) object to
be plotted. This function is very similar to the previous one, so
parameters are nearly identical. One key difference is this function
interprets the first column of the data as the total and calculates
contributions to growth for all subsequent columns. If the components
don’t add up, it automatically creates a new column “others”, which can
be disabled by setting `add_others = FALSE`.

More information is available
[here](https://stefan.angrick.me/create-interactive-time-series-charts-with-plotly-in-r).

## Interactive data dashboard example

`examples/interactive_dashboard.Rmd` contains an example of an
interactive data dashboard which uses the
[oemdlRtools](https://github.com/stefanangrick/oemdlRtools) R package
along with the
[flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/) package
and
[plotly\_chart\_helpers.R](https://github.com/stefanangrick/r-helpers)
helper functions to visualise information from two Oxford Economics
Global Economic Model databases.

To configure the dashboard, open the file `interactive_dashboard.Rmd`
and modify the user parameters towards the top of the file, then
[knit](https://rmarkdown.rstudio.com/flexdashboard/) the dashboard as
usual.

``` r
db_file_new <- "C:/OEF/OEMDLRTOOLS/WPO.db"
db_file_old <- "C:/OEF/OEMDLRTOOLS/Jan20_1.db"
my_sector   <- "JAPAN"
start_yr    <- 2016
end_yr      <- 2024
```

`db_file_new` specifies the path to the newer file (e.g. a database with
updates or simulation results), `db_file_old` specifies the older
baseline for comparison, `my_sector` specifies the sector (country), and
`start_yr` and `end_yr` set start and end years, respectively.
