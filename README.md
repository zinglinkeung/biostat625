
# linreg

<!-- badges: start -->
[![R-CMD-check](https://github.com/zinglinkeung/biostat625/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zinglinkeung/biostat625/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/zinglinkeung/linreg/branch/main/graph/badge.svg)](https://app.codecov.io/gh/zinglinkeung/linreg?branch=main)
<!-- badges: end -->

The goal of `linreg` is to build up simple linear regression and multiple linear regression models with or without intercept. Linear regression can be done by three calculation methods. QR decomposition is the default, singular value decomposition(SVD) and least square estimation method(LSE) also applied. The outcome includes estimation results, partial T test and overall F test.

## Installation

You can install the development version of linreg from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zinglinkeung/linreg")
```

## Usage

To learn more about `linreg`,please refer to vignettes for tutorial and correctness and efficiency tests.

## Example

The following are basic examples which shows you how to build linear regression models:

``` r
library(linreg)
```
*(1) Fit a simple linear regression model with intercept:*

```r
x <- rnorm(100)
y <- rnorm(100)
fit <- linreg(y,x)
```

*(2) Fit a simple linear regression model without intercept:*

```r
x <- rnorm(100)
y <- rnorm(100)
fit <- linreg(y,x,intercept = FALSE)
```

*(3) Fit a multiple linear regression model with intercept:*

```r
x <- rnorm(100)
y <- matrix(rnorm(600),nrow=100,ncol=6)
fit <- linreg(y,x)
```

*(4) Fit a multiple linear regression model without intercept:*

```r
x <- rnorm(100)
y <- matrix(rnorm(600),nrow=100,ncol=6)
fit <- linreg(y,x,intercept=FALSE)
```

*(4) Fit a multiple linear regression model with intercept and $\alpha=0.9$:*

```r
x <- rnorm(100)
y <- matrix(rnorm(600),nrow=100,ncol=6)
fit <- linreg(y,x,CI_level = 0.9)
```

we can check regression outcomes as follow:

a. check the exact fitted model

```r
fit$Call #function of the regression model
```

b. check estimated coefficients and the partial t test

```r
knitr::kable(fit$coefficients) 

#coefficients table include estimators of coefficients, standard error, confidence interval, T statistics and corresponding p-value.
```

c. check the fitted value of the model

```r
head(fit$fitted.values) #the first 6 fitted values
```

d. check the residuals

```r
fit$residuals # the first 6 residuals
```

e. check the model's mean square error

```r
fit$MSE
```

f. check the confidence interval of the estimator

```r
data.frame(fit$lower_ci,fit$upper_ci)
```

g. check the R^2 and adjusted R^2

```r
data.frame(fit$R_squared)
```

h. check the overall F test, results including f-statistics, corresponding p value and the degree of freedom.

```r
data.frame(fit$F_test)
```
