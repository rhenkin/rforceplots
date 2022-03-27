
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rforceplots

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`rforceplots` is as a stand-alone wrapper for the
[shapjs](https://www.npmjs.com/package/shapjs) node package to
facilitate rendering additive force plots in R. It does not require
Python or the reticulate package and is compatible with objects exported
by the `fastshap::explain()`, `shapr::explain()` and
`shapper::individual_variable_effect()` functions. It includes Shiny
bindings and also gives access to the three base visualization functions
from the shapjs node package for custom SHAP explanation formats.

## Installation

You can install the development version of rforceplots with:

``` r
remotes::install_github("rhenkin/rforceplots")
```

## Usage

The main method of using the package is combining it with the three SHAP
packages available in R,:
[fastshap](https://cran.r-project.org/web/packages/fastshap/index.html),
[shapr](https://cran.r-project.org/web/packages/shapr/index.html) and
[shapper](https://cran.r-project.org/web/packages/shapper/index.html).
With all three, using `rforceplots` comes after using an `explain`
function or equivalent.

### fastshap

The `fastshap` package already includes most of the plots from the
original Python SHAP package. However, the force plots from `fastshap`
require Python to be installed. `rforceplots` provides an alternative
without the Python dependency. The example below is based on the
[vignette](https://bgreenwell.github.io/fastshap/articles/forceplot.html)
from the `fastshap` package.

``` r
# Load required packages
library(fastshap)
library(xgboost)
library(rforceplots)

# Load the Boston housing data
# install.packages("pdp)
data(boston, package = "pdp")
X <- data.matrix(subset(boston, select = -cmedv))  # matrix of feature values

# Fit a gradient boosted regression tree ensemble; hyperparameters were tuned 
# using `autoxgb::autoxgb()`
set.seed(859)  # for reproducibility
bst <- xgboost(X, label = boston$cmedv, nrounds = 338, max_depth = 3, eta = 0.1,
               verbose = 0)

# Compute exact explanations for all rows
ex <- explain(bst, exact = TRUE, X = X)

# For combined force plot
ForcePlot(ex, X)

# For individual additive force plot
ForcePlot(ex[1,], X[1,])
```

### shapr

Using `ForcePlot` with shapjs is also straightforward. If no extra
argument is passed, a combined force plot is displayed. With the use of
the additional `i` argument, a single additive force plot is showed,
where i corresponds to the observation row index. Most of the code below
is part of the example for the shapr package and is run when you call
`example("explain", package = "shapr")`.

``` r
library(shapr)
library(rforceplots)

# Load example data
data("Boston", package = "MASS")
# Split data into test- and training data
x_train <- head(Boston, -3)
x_test <- tail(Boston, 3)
# Fit a linear model
model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
 explainer <- shapr(x_train, model)
p <- mean(x_train$mpg)
explanation <- explain(x_test, explainer, approach = "empirical",
prediction_zero = p, n_samples = 1e2)

# Visualize one explanation
ForcePlot(explanation, 1)
```

### shapper

After running the shapper pipeline, you can call `ForcePlot()` on the
resulting `individual_variable_effect` object. Calling the function on
the IVE object will show a force plot for the whole set of new
observations to be explained, whereas using the additional `id` argument
will show a single additive force plot. The `id` here corresponds to the
row of the object in the original training data.

``` r
library(shapper)
library(rforceplots)
library(DALEX)
library(randomForest)

Y_train <- HR$status
x_train <- HR[ , -6]

set.seed(123)
model_rf <- randomForest(x = x_train, y = Y_train)

p_function <- function(model, data) predict(model, newdata = data, type = "prob")

ive_rf <-
  individual_variable_effect(
    model_rf,
    data = x_train,
    predict_function = p_function,
    new_observation = x_train[1:2, ],
    nsamples = 50
  )

# Show force plot for two observations (see new_observation argument in previous function)
ForcePlot(ive_rf)

# Show force plot for single observation
ForcePlot(ive_rf, id = 1, outName = "fired")
```

## Saving the plots

The plots included in this package are based on the `htmlwidgets`
package. To save a plot as HTML (including the interactions), you can
use:

``` r
# [... previous code ...]
forceplot_to_save <- ForcePlot(...)
htmlwidgets::saveWidget(forceplot_to_save, "saved_plot.html")
```

## Development

If you you want to work on the `rforceplots` package, you will need
[node](https://nodejs.org) and [yarn](https://yarnpkg.com) installed to
set up the JavaScript dependencies, e.g. new node packages or updated
versions. Once they are installed and the package.json is updated, you
can run the following and proceed to change the R code as usual.

``` bash
yarn install
yarn run webpack
```
