# Installing

Use `devtools` to install this package from GitHub.

    # install.packages("devtools")
    devtools::install_github("benet1one/EasyLP", build_vignettes = TRUE)

    ## Downloading GitHub repo benet1one/EasyLP@HEAD

    ## rlang (1.1.3 -> 1.1.4) [CRAN]

    ## Installing 1 packages: rlang

    ## Installing package into 'C:/Users/Benet/AppData/Local/R/win-library/4.2'
    ## (as 'lib' is unspecified)

    ## 
    ##   There is a binary version available but the source version is later:
    ##       binary source needs_compilation
    ## rlang  1.1.3  1.1.4              TRUE

    ## installing the source package 'rlang'

    ## Warning in i.p(...): installation of package 'rlang' had non-zero exit status

    ## ── R CMD build ─────────────────────────────────────────────────────────────────
    ##          checking for file 'C:\Users\Benet\AppData\Local\Temp\Rtmp69BTS3\remotes176456e7d75\benet1one-EasyLP-a231253/DESCRIPTION' ...  ✔  checking for file 'C:\Users\Benet\AppData\Local\Temp\Rtmp69BTS3\remotes176456e7d75\benet1one-EasyLP-a231253/DESCRIPTION'
    ##       ─  preparing 'easylp':
    ##    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
    ##       ─  installing the package to build vignettes
    ##          creating vignettes ...     creating vignettes ...   ✔  creating vignettes (7.6s)
    ##       ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ##      Omitted 'LazyData' from DESCRIPTION
    ##       ─  building 'easylp_0.2.1.tar.gz'
    ##      
    ## 

    ## Installing package into 'C:/Users/Benet/AppData/Local/R/win-library/4.2'
    ## (as 'lib' is unspecified)

# Getting Started

First, create the problem by using `easylp$new()`. Then define variables
with `$var()`, set the objective function with `$min()` or `$max()`, and
constraint the problem with `$con()`. Then, solve the problem and print
the results.

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

# Learn more with vignettes

Make sure to `build_vignettes = TRUE` and discover everything you can do
using the powerful and intuitive syntax.

    vignette("easylp")

    ## starting httpd help server ... done

    vignette("constraints")
