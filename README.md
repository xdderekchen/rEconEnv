rEconEnv
================
Xiangdong Chen
Jan. 30, 2020

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end --> \# Welcome to the simple R package “rEconEnv” The
Home Price Index and Interest Rate are important factors in analysis
Mortgage data. In this package, we present convenient ways to get these
data from
<https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx>
and <http://www.freddiemac.com/pmms>.

If you have an open access to the internet, the functions of these
packahe will connect to sites, then download the most recent data and
process them. If your running server can’t reach these sites (for
example, being blocked by your corporate firewall), then you can
manually download files and pass the files to the functions for parsing
and processing.

Here is the remote data locations used in this package:

  - HPI - US National Level:
    <https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_us_and_census.csv>

  - HPI - US States Level:
    <https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_state.csv>

  - HPI - US ZIP(3) Level:
    <https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_3zip.xlsx>

  - IR - FRM 15: <http://www.freddiemac.com/pmms/docs/15yr_pmmsmnth.xls>

  - IR - FRM 30: <http://www.freddiemac.com/pmms/docs/30yr_pmmsmnth.xls>

The data retuned from functions are in data.table format.

## Installation

``` r
devtools::install_github("xdderekchen/rEconEnv")
```

## Usage

(for plotting data, read
[here](https://xdderekchen.github.io/rEconEnv/Examples.html) )

#### Use the libraries:

``` r
library(readxl)
library(data.table)
library(magrittr)
library(curl)
library(dygraphs)
library(rEconEnv)
```

#### Get US National HPI

``` r
# 1. get recent National (US) Home Price Index (monthly) since 2010
ushpi_m    <- get_US_hpi(from_year=2010, out.Monthly=TRUE)
#and plotting the data:
#  plot_HP(dt=ushpi_m,  title="Monthly National HPI" )


# 2. get recent National (US) Home Price Index (quarterly) since 2010
ushpi_q    <- get_US_hpi(from_year=2010, out.Monthly=FALSE)
#and plotting the data:
#  plot_HP(dt=ushpi_q,  title="Monthly National HPI" )

print(ushpi_m)
#>      State   Date    Index
#>   1:   USA 201002 323.8700
#>   2:   USA 201003 322.8971
#>   3:   USA 201004 321.9271
#>   4:   USA 201005 320.9600
#>   5:   USA 201006 321.9702
#>  ---                      
#> 112:   USA 201905 443.3500
#> 113:   USA 201906 444.8913
#> 114:   USA 201907 446.4380
#> 115:   USA 201908 447.9900
#> 116:   USA 201909 447.9900
```

#### Get States HPI

``` r
# 3. get recent States Home Price Index (monthly) since 2010
statehpi_m    <- get_State_hpi(from_year=2010, out.Monthly=TRUE)
#plot_HP(dt=statehpi_m,  regionnames = c("MD", "VA", "CA") , title="Monthly HPI for selected states")

# 4. get recent States Home Price Index (quarterly) since 2000
statehpi_q    <- get_State_hpi(from_year=2000, out.Monthly=FALSE)
#plot_HP(dt=statehpi_q,  regionnames = c("MD", "VA", "CA") , title="Monthly HPI for selected states")

print(statehpi_m)
#>       State   Date    Index
#>    1:    AK 201002 275.1200
#>    2:    AK 201003 275.8879
#>    3:    AK 201004 276.6579
#>    4:    AK 201005 277.4300
#>    5:    AK 201006 277.9059
#>   ---                      
#> 5912:    WY 201905 329.2000
#> 5913:    WY 201906 331.3986
#> 5914:    WY 201907 333.6119
#> 5915:    WY 201908 335.8400
#> 5916:    WY 201909 335.8400
```

#### Get Zip Level (ZIP3) HPI

``` r
# 5. get recent ZIP3 Home Price Index (monthly) since 2010
zip3hpi_m    <- get_ZIP3_hpi(from_year=2010, out.Monthly=TRUE)
#plot_HP(dt=zip3hpi_m,  regionnames = c("010", "011", "012") , title="Monthly HPI for selected zips")

# 6. get recent ZIP3 Home Price Index (quarterly) since 2000
zip3hpi_q    <- get_ZIP3_hpi(from_year=2000, out.Monthly=FALSE)
#plot_HP(dt=zip3hpi_q,  regionnames = c("010", "011", "012") , title="Quarterly HPI for selected zips")

print(zip3hpi_m)
#>         ZIP3   Date    Index
#>      1:  010 201002 191.0400
#>      2:  010 201003 190.2837
#>      3:  010 201004 189.5303
#>      4:  010 201005 188.7800
#>      5:  010 201006 189.3947
#>     ---                     
#> 102308:  999 201905 230.1000
#> 102309:  999 201906 227.7970
#> 102310:  999 201907 225.5171
#> 102311:  999 201908 223.2600
#> 102312:  999 201909 223.2600
```

#### Get Intererst Rate

``` r

# 7. get recent 30yr Fixed Mortgage Interest Rate since 2010
ir30        <- get_IR_FRM30(from_year=2010)
#> New names:
#> * `` -> ...1
#> * `` -> ...2
#> * `` -> ...3
#> * `` -> ...4
#> * `` -> ...5
#> * ... and 16 more problems
#plot_IR(ir30, title="IR rate for FRM30yr")

# 8. get recent 15yr Fixed Mortgage Interest Rate since 2010
ir15         <- get_IR_FRM15(from_year=2010)
#> New names:
#> * `` -> ...1
#> * `` -> ...2
#> * `` -> ...3
#> * `` -> ...4
#> * `` -> ...5
#> * ... and 16 more problems
#plot_IR(ir15, title="IR rate for FRM15yr")

# 9. get recent 30yr and 15yr Fixed Mortgage Interest Rate since 2010
irdata       <- get_IR_FRM_15_30()
#> New names:
#> * `` -> ...1
#> * `` -> ...2
#> * `` -> ...3
#> * `` -> ...4
#> * `` -> ...5
#> * ... and 16 more problems
#> New names:
#> * `` -> ...1
#> * `` -> ...2
#> * `` -> ...3
#> * `` -> ...4
#> * `` -> ...5
#> * ... and 16 more problems
#plot_IR(irdata, title="IR rate for FRM30yr and FRM15yr")

print(irdata)
#>        Date    IR15    IR30
#>   1: 200001 8.05000 8.46000
#>   2: 200002 8.18000 8.58000
#>   3: 200003 8.05500 8.49000
#>   4: 200004 8.05000 8.40000
#>   5: 200005 8.43000 8.77000
#>  ---                       
#> 236: 201908 3.20500 3.75500
#> 237: 201909 3.25125 3.74125
#> 238: 201910 3.27000 3.83000
#> 239: 201911 3.27875 3.83750
#> 240: 201912 3.33625 3.88250
```
