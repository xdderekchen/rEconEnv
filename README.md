rEconEnv
================
Xiangdong Chen
Jan. 22, 2020

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

# Welcome to the simple R package “rEconEnv”

For analysing mortgage data, we need some economic data such as the Home
Price Index and Interest Rate. Luckly we can get these data from
<https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx>
and <http://www.freddiemac.com/pmms>.

This package will grab data from these websites and return data in R
data.table formats.

## Usage

(for plotting data, read [here](file:///C:/AWS/rEconEnv/Examples.html) )

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
```

#### Get States HPI

``` r
# 3. get recent States Home Price Index (monthly) since 2010
statehpi_m    <- get_State_hpi(from_year=2010, out.Monthly=TRUE)
#plot_HP(dt=statehpi_m,  regionnames = c("MD", "VA", "CA") , title="Monthly HPI for selected states")

# 4. get recent States Home Price Index (quarterly) since 2000
statehpi_q    <- get_State_hpi(from_year=2000, out.Monthly=FALSE)
#plot_HP(dt=statehpi_q,  regionnames = c("MD", "VA", "CA") , title="Monthly HPI for selected states")
```

#### Get Zip Level (ZIP3) HPI

``` r
# 5. get recent ZIP3 Home Price Index (monthly) since 2010
zip3hpi_m    <- get_ZIP3_hpi(from_year=2010, out.Monthly=TRUE)
#plot_HP(dt=zip3hpi_m,  regionnames = c("010", "011", "012") , title="Monthly HPI for selected zips")

# 6. get recent ZIP3 Home Price Index (quarterly) since 2000
zip3hpi_q    <- get_ZIP3_hpi(from_year=2000, out.Monthly=FALSE)
#plot_HP(dt=zip3hpi_q,  regionnames = c("010", "011", "012") , title="Quarterly HPI for selected zips")
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
```
