
# tidybayes.rethinking: Extend tidybayes to work with the rethinking package <img id="tidybayes_logo" src="man/figures/logo.svg" align="right" />

[![R build
status](https://github.com/mjskay/tidybayes.rethinking/workflows/R-CMD-check/badge.svg)](https://github.com/mjskay/tidybayes.rethinking/actions)

*Matthew Kay, Northwestern University, <mjskay@northwestern.edu>*

This package extends the
[tidybayes](https://mjskay.github.io/tidybayes/) R package to support
models from Richard McElreath’s
[rethinking](https://github.com/rmcelreath/rethinking) package.

## Installation

`tidybayes.rethinking` is separate from the core `tidybayes` package
because `rethinking` is not currently available on CRAN. Thus,
`tidybayes.rethinking` is also not available on CRAN.

You can install `tidybayes.rethinking` from Github:

``` r
install.packages("devtools")     # only necessary if you don't have devtools already
devtools::install_github("mjskay/tidybayes.rethinking")
```

## Examples

See the
[tidy-rethinking](https://mjskay.github.io/tidybayes.rethinking/articles/tidy-rethinking.html)
vignette for `rethinking`-specific examples, or check out the
[tidybayes](https://mjskay.github.io/tidybayes/) documentation for more
general examples of `tidybayes` usage.

## Feedback and issues

I welcome feedback, suggestions, and issues! If you have found a bug in
the rethinking-specific version of functions, please file it on [the
tidybayes.rethinking
github](https://github.com/mjskay/tidybayes.rethinking/issues/new) with
minimal code to reproduce the issue. For general `tidybayes` issues,
please file bugs on [the tidybayes
github](https://github.com/mjskay/tidybayes/issues/new). If you’re not
sure which it falls under, file it on one or the other and I will
redirect the issue if necessary.

## Citing `tidybayes`

Matthew Kay (2021). *tidybayes: Tidy Data and Geoms for Bayesian
Models*. R package version 3.0.0, <https://mjskay.github.io/tidybayes/>.
DOI: [10.5281/zenodo.1308151](https://doi.org/10.5281/zenodo.1308151).
