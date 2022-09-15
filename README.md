# chart - Unified interface (with formula) for R plots <a href='https://www.sciviews.org/chart'><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/chart/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/chart/actions/workflows/R-CMD-check.yaml) [![Coverage status](https://img.shields.io/codecov/c/github/SciViews/chart/master.svg)](https://codecov.io/github/SciViews/chart?branch=master) [![CRAN status](https://www.r-pkg.org/badges/version/chart)](https://cran.r-project.org/package=chart) [![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.html) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable) [![R-CMD-check](https://github.com/SciViews/chart/workflows/R-CMD-check/badge.svg)](https://github.com/SciViews/chart/actions)

<!-- badges: end -->

{chart} proposes a formula interface to {ggplot2}, and it also homogenize plot outputs from base R plots, {lattice} and {ggplot}. If labels and/or units attributes are defined for variables in the data, they are used automatically to construct the label (with units) of the axes.

## Installation

You can install the released version of {chart} from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("chart")
```

You can also install the latest development version. Make sure you have the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {chart} package from GitHub (source from **master** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/chart")
```

R should install all required dependencies automatically, and then it should compile and install {chart}.

Latest development version of {chart} (source + Windows binaries for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/chart/build/artifacts).

## Further explore {chart}

You can get further help about this package this way: Make the {chart} package available in your R session:

``` r
library("chart")
```

Get help about this package:

``` r
library(help = "chart")
help("chart-package")
vignette("chart") # Not installed with install_github()
```

For further instructions, please, refer to the help pages at <https://www.sciviews.org/chart/>.

## Code of Conduct

Please note that the {chart} package is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
