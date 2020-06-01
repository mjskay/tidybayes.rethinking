# Deprecated functions
#
# Author: mjskay
###############################################################################


#' Deprecated functions, arguments, and column names in tidybayes
#'
#' Deprecated functions, arguments, and column names and their alternatives are listed below.
#'
#' @section Deprecated Functions:
#'
#' When `tidybayes.rethinking` was originally created, `tidybayes` did not have good
#' analogs for `tidy_link()` or `tidy_sim()`. However, `tidybayes` now has a mature API
#' analogous to these functions, so they have been deprecated:
#'
#' - `tidy_link()` has been replaced with `add_fitted_draws()` / `fitted_draws()`. See
#'   `fitted_draws.ulam()` for information on rethinking-specific arguments, or
#'   `tidybayes::fitted_draws()` for information on the general function.
#'
#' - `tidy_sim()` has been replaced with `add_predicted_draws()` / `predicted_draws()`. See
#'   `predicted_draws.ulam()` for information on rethinking-specific arguments, or
#'   `tidybayes::predicted_draws()` for information on the general function.
#'
#' @format NULL
#' @usage NULL
#' @author Matthew Kay
#' @name tidybayes.rethinking-deprecated
NULL
