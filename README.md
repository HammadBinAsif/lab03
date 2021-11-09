
<!-- README.md is generated from README.Rmd. Please edit that file -->

# advanceLab3g25.R

<!-- badges: start -->

[![Travis build
status](https://app.travis-ci.com/HammadBinAsif/lab03.svg?token=YfBkxpsFM24tj2sZYTgX&branch=main)](https://app.travis-ci.com/github/HammadBinAsif/lab03)
<!-- badges: end -->

The goal of advanceLab3g25.R is understand and learn how to create a
package and publish on cran

## Installation

You can install the released version of advanceLab3g25.R from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("advanceLab3g25.R")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("uzairjan/AdvancedRHT2021_Lab03")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(advanceLab3g25.R)
 wiki_graph <-
 data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
            v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
 dijkstra(wiki_graph, 1)
 dijkstra(wiki_graph, 3)
```
"# lab03" 
