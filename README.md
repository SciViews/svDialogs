
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ‘SciViews::R’ - Standard Dialog Boxes for Windows, MacOS and Linuxes <a href="https://www.sciviews.org/svDialogs"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/svDialogs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/svDialogs/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/SciViews/svDialogs/graph/badge.svg?token=0d0w6SgDxq)](https://codecov.io/gh/SciViews/svDialogs)
[![CRAN
status](https://www.r-pkg.org/badges/version/svDialogs)](https://cran.r-project.org/package=svDialogs)
[![r-universe
status](https://sciviews.r-universe.dev/badges/svDialogs)](https://sciviews.r-universe.dev/svDialogs)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.html)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

With {svDialogs}, you can rapidly construct standard dialog boxes for
your GUI, including message boxes, input boxes, list, file or directory
selection, … In case R cannot display GUI dialog boxes, a simpler
command line version of these interactive elements is provided as
fallback solution (e.g., in a terminal session).

## Installation

You can install the released version of {svDialogs} from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("svDialogs")
```

You can also install the latest development version. Make sure you have
the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {svDialogs} package from GitHub
(source from **master** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/svDialogs")
```

### Latest stable version

The latest stable version of {svDialogs} can simply be installed from
[CRAN](http://cran.r-project.org):

``` r
install.packages(c("svGUI", svDialogs"))
```

R should install all required dependencies automatically, and then it
should compile and install {svDialogs}.

Latest devel version of {svDialogs} (source + Windows binaries for the
latest stable version of R at the time of compilation) is also available
from
[appveyor](https://ci.appveyor.com/project/phgrosjean/svDialogs/build/artifacts).

## Usage

You can get further help about this package this way: Make the
{svDialogs} package available in your R session:

``` r
library("svDialogs")
```

Get help about this package:

``` r
library(help = "svDialogs")
help("svDialogs-package")
vignette("svDialogs") # None is installed with install_github()
```

For further instructions, please, refer to the help pages at
<https://www.sciviews.org/svDialogs/>.

## Code of Conduct

Please note that the svDialogs project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Note to developers

This package used to be developed on R-Forge in the past. However, the
latest [R-Forge
version](https://r-forge.r-project.org/projects/sciviews/) was moved to
this GitHub repository on 2018-04-02 (SVN version 569). **Please, do not
use R-Forge anymore for SciViews development, use this GitHub repository
instead.**
