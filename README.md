# chart - Unified interface (with formula) for R plots

[![Linux & OSX Build Status](https://travis-ci.org/SciViews/chart.svg )](https://travis-ci.org/SciViews/chart)
[![Win Build Status](https://ci.appveyor.com/api/projects/status/github/SciViews/chart?branch=master&svg=true)](http://ci.appveyor.com/project/phgrosjean/chart)
[![Coverage Status](https://img.shields.io/codecov/c/github/SciViews/chart/master.svg)
](https://codecov.io/github/SciViews/chart?branch=master)
[![CRAN Status](http://www.r-pkg.org/badges/version/chart)](http://cran.r-project.org/package=chart)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](http://www.gnu.org/licenses/gpl-2.0.html)


## Installation

### Latest stable version

The latest stable version of **chart** can simply be installed from [CRAN](http://cran.r-project.org):

```r
install.packages("chart")
```


### Development version

Make sure you have the **devtools** R package installed:

```r
install.packages("devtools")
```

Use `install_github()` to install the **chart** package from Github (source from **master** branch will be recompiled on your machine):

```r
devtools::install_github("SciViews/chart", build_vignettes = TRUE)
```

R should install all required dependencies automatically, and then it should compile and install **chart**.

Latest devel version of **chart** (source + Windows binaires for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/chart/build/artifacts).


## Usage

Make the **chart** package available in your R session:

```r
library("chart")
```

Get help about this package:

```r
library(help = "chart")
help("chart-package")
```

For further instructions, please, refer to these help pages.


## Note to developers

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
