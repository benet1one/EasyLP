# Installing

Use `devtools` to install this package from GitHub. Make sure to build vignettes!

``` R
# install.packages("devtools")
devtools::install_github("benet1one/EasyLP", build_vignettes = TRUE)
```

# Getting Started

First, create the problem by using `easylp$new()`. Then define variables with `$var()`, set the objective function with `$min()` or `$max()`, and constraint the problem with `$con()`. Then, solve the problem and print the results.

``` R
library(easylp)
lp <- easylp$new()
lp$var("x")
lp$var("y")
lp$max(x + y)
lp$con(
    x + 2*y <= 3,
    y >= 3*x - 2
)

lp$solve()
lp

## Easy Linear Problem 
## Status: optimal
## Objective Value = 2
## 
## Solution:
##
## $x
## [1] 1
##
## $y
## [1] 1
```

# Learn more with vignettes

Make sure to `build_vignettes = TRUE` and discover everything you can do using the powerful and intuitive syntax.

``` R
vignette("easylp")
vignette("objective")
vignette("constraints")
```
