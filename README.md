# mccca

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/mccca)](https://CRAN.R-project.org/package=mccca)
<!-- badges: end -->

The R package **mccca** implements multiple-class cluster correspondence
analysis (MCCCA; Takagishi and van de Velden, 2022). MCCCA is a statistical
method that identifies and visualizes heterogeneous tendencies specific to
certain "classes", specified by the researcher, in a low dimensional space.
In MCCCA, two kinds of variables, external and active variables, are
distinguished. External variables directly define the classes whereas the
active variables are used to derive, for each class, multiple clusters of
observations exhibiting similar tendencies. The class-specific clusters are
displayed together with the categories of the active variables in a single
biplot.

## Installation

Install the released version from CRAN:

``` r
install.packages("mccca")
```

Or install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("mtakagishi0/mccca")
```

## Example

``` r
library(mccca)
data(mealDrink)

# Create the classes from the external variables (nationality and gender)
mcccadata <- create.MCCCAdata(mealDrink[, 1:2], mealDrink[, 3:4])

# Apply MCCCA with the numbers of class-specific clusters
set.seed(1)
res <- MCCCA(mcccadata, K.vec = c(2, 1, 2, 2))

# Show the MCCCA biplot and a numerical summary
plot(res, classlabel = c("AF", "AM", "JF", "JM"))
summary(res)
```

For real data, the numbers of class-specific clusters can be chosen with
`decideK()`. See `vignette("mccca")` for a full example.

## Reference

Takagishi, M. and van de Velden, M. (2022). Visualizing class specific
heterogeneous tendencies in categorical data. *Journal of Computational and
Graphical Statistics*, 31(3), 790–801.
<https://doi.org/10.1080/10618600.2022.2035737>
