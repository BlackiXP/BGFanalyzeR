
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BGFanalyzeR

<!-- badges: start -->

<!-- badges: end -->

The goal of BGFanalyzeR is to help standardizing the analysis of biogas
fermentation data, through a novel S3 object class, the BGF. Th package
ships with methods allowing object creation based on external files,
object manipulation, as well as visualizing and exporting BGF’s .

## Installation

You can install the development version of BGFanalyzeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("BlackiXP/BGFanalyzeR")
```

## Example

The package comes with an example data set ‘LabScaleBiogas’

``` r
library(BGFanalyzeR)
myBGF <- LabscaleBiogas
myBGF
#> 'LabscaleBiogas' - a BGF with 15 fermentation(s)
#> 
#> 
#> $ExpParam: 11 experimental paramerters
#> $metaData: 16 meta variables
#> $BioGasData: 735 observations of 7 fermentation variables
#> 
#> 
#>                   yield    sd_yield production sd_production time_production
#> Blank     -2.997602e-14  2.88499567     75.325    5.26794552             1.0
#> Cellulose  2.899017e+01          NA    182.260            NA             3.0
#> S1 ctrl    6.188975e+02  9.87799079    700.350    0.77781746             3.0
#> S1 7d      2.618791e+02  9.49000623    359.790    6.49124025             3.0
#> S1 4d      1.087374e+03 11.21296479    644.655   20.73944189             3.0
#> S2 ctrl    7.781660e+01  0.02079726    119.845    0.04949747             1.0
#> S2 4d      4.011575e+02  2.21973211    110.935    0.21920310             2.0
#> S2 6d      1.335888e+02 42.32196395    128.480   25.54069694             4.5
#>           sd_time_production
#> Blank               0.000000
#> Cellulose                 NA
#> S1 ctrl             0.000000
#> S1 7d               0.000000
#> S1 4d               0.000000
#> S2 ctrl             0.000000
#> S2 4d               0.000000
#> S2 6d               3.535534
```

The data of a BGF can be plotted with simplest methods:

``` r
plot(myBGF)
```

<img src="man/figures/README-plot-1.png" alt="" width="100%" />

This plot is produced by the minimum data needed to set up a BGF, the
accumulated exhaust gas volumes of each fermentation part of the BGF.
Upon further processing, other parameters can be visualized using
advanced plotting methods, such as `plot_curve()` or `bgf_plot()`:

``` r
# plot the production
plot_curve(myBGF,production,reactor)
```

<img src="man/figures/README-advanced-plotting-I-1.png" alt="" width="100%" />

``` r
# plot the yield
bgf_plot(myBGF,type="yield_box")
```

<img src="man/figures/README-advanced-plotting-II-1.png" alt="" width="100%" />
