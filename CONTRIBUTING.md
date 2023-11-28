# Contributing to Rarr

This document outlines how to propose a change to Rarr. 

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface.  
You can do this by clicking on the pen icon, which will then fork the project to your account and allow you to propose the changes.
Please note we use [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) to generate the manual paages, meaning you'll need to edit the `.R`, not the `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.
Part of the vignette is used in multiple places, and can be found in the non-standard `/inst/rmd` location, rather than the `/vignettes` folder.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the Rarr team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal reproducible example.  You can use the
[reprex](https://www.tidyverse.org/help/#reprex) package to achieve this.

### Pull request process

*  Fork the package from this repository and then clone the fork onto your computer.

*  Install all development dependences with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
   If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.
    
*  Create a Git branch for your pull request (PR). Choose a name for the new bracnh that describeds the change you are introducing.

*  Make your changes, commit to git, and then create a pull request on GitHub.
   The title of your PR should briefly describe the change.
   The body of your PR should reference the relevant issue by mentioning somewhere in the description `#issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Try to follow the style described in <https://style.tidyverse.org/news.html>.

### Code style

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [tiny](https://cran.r-project.org/package=tinytest) for unit tests. 
   Contributions with test cases included are easier to accept.
   Test files can be found in `/inst/extdata/zarr_examples`.
   If you do not find a file with the features you need for your test look in the folder `/inst/scripts/` to find the Dockerfile and Python script used to create the test files.

## Code of Conduct

Please note that the Rarr project adhears to the
[Bioconductor Code of Conduct](https://bioconductor.github.io/bioc_coc_multilingual/). By contributing to this
project you agree to abide by its terms.
