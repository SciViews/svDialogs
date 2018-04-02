# svDialogs - Standard Dialog Boxes for R

[![Linux & OSX Build Status](https://travis-ci.org/SciViews/svDialogs.svg )](https://travis-ci.org/SciViews/svDialogs)
[![Win Build Status](https://ci.appveyor.com/api/projects/status/github/SciViews/svDialogs?branch=master&svg=true)](http://ci.appveyor.com/project/phgrosjean/svDialogs)
[![Coverage Status](https://img.shields.io/codecov/c/github/SciViews/svDialogs/master.svg)
](https://codecov.io/github/SciViews/svDialogs?branch=master)
[![CRAN Status](http://www.r-pkg.org/badges/version/svDialogs)](http://cran.r-project.org/package=svDialogs)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](http://www.gnu.org/licenses/gpl-2.0.html)


## Installation

### Latest stable version

The latest stable version of **svDialogs** can simply be installed from [CRAN](http://cran.r-project.org):

```r
install.packages(c("svGUI", svDialogs"))
```


### Development version

Make sure you have the **devtools** R package installed:

```r
install.packages("devtools")
```

Use `install_github()` to install the **svDialogs** package from Github (source from **master** branch will be recompiled on your machine):

```r
devtools::install_github("SciViews/svGUI")
devtools::install_github("SciViews/svDialogs")
```

R should install all required dependencies automatically, and then it should compile and install **svDialogs**.

Latest devel version of **svDialogs** (source + Windows binaires for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/svDialogs/build/artifacts).


## Usage

Make the **svDialogs** package available in your R session:

```r
library("svDialogs")
```

Get help about this package:

```r
library(help = "svDialogs")
help("svDialogs-package")
```

For further instructions, please, refer to these help pages.


## Note to developers

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

This package used to be developed on R-Forge in the past. However, the latest [R-Forge version](https://r-forge.r-project.org/projects/sciviews/) was moved to this Github repository on 2018-04-02 (SVN version 569). **Please, do not use R-Forge anymore for SciViews development, use this Github repository instead.**
