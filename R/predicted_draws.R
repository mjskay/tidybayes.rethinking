# predicted_draws
#
# Author: mjskay
###############################################################################


# deprecated names for predicted_draws -------------------------------

#' @rdname tidybayes.rethinking-deprecated
#' @format NULL
#' @usage NULL
#' @importFrom tidybayes predicted_draws
#' @export
tidy_sim = function(data, fit, ...) {
  .Deprecated("add_predicted_draws", "tidybayes.rethinking")

  fitted_draws(object = fit, newdata = data, ...)
}


# predicted_draws ------------------------------------------------------------

#' Add draws from the posterior predictive of a rethinking model to a data frame
#'
#' Adds draws from the posterior predictive distribution of a rethinking model to a data frame.
#' Provides support for [tidybayes::predicted_draws()] / [tidybayes::add_predicted_draws()] for
#' models from the `rethinking` package.
#'
#' @inheritParams tidybayes::predicted_draws
#' @param object A model fit using `rethinking::quap()`, `rethinking::ulam()`,
#' `rethinking::map()`, or `rethinking::map2stan()`.
#' @param ... Optional parameters passed on to `rethinking::sim()`. The most pertinent are:
#'   - `post`: Optional samples from posterior. If missing, simulates samples using `ndraws`.
#' @param ndraws The number of draws per prediction to return. When `NULL` (the default), `rethinking::ulam()` and
#' `rethinking::map2stan()` models return all draws; `rethinking::quap()` and `rethinking::map()` models
#' return 5000 draws.
#' @param re_formula,category Not used with this model type.
#' @importFrom rlang is_true is_false is_empty
#' @importFrom tidybayes predicted_draws add_draws sample_draws
#' @export
predicted_draws.ulam = function(object, newdata, value = ".prediction", ..., ndraws = NULL, seed = NULL,
  re_formula = NULL, category = ".category"
) {
  if (!is.null(re_formula)) {
    warning("The re_formula parameter is not supported by rethinking models; ignored.")
  }
  if (category != ".category") {
    warning("The category parameter is not supported by rethinking models; ignored.")
  }

  # map and quap models need to specify the number of draws (since they are generated)
  if ((inherits(object, "map") || inherits(object, "quap")) && is.null(ndraws)) {
    ndraws = 5000
  }

  # get the draws from the posterior predictive
  set.seed(seed)
  sims = rethinking::sim(object, newdata, n = ndraws, ...)
  draws = add_draws(newdata, sims, value = value)

  # ulam and map2stan models seem to ignore n
  if ((inherits(object, "map2stan") || inherits(object, "ulam")) && !is.null(ndraws)) {
    draws = sample_draws(draws, ndraws)
  }

  draws
}

#' @rdname predicted_draws.ulam
#' @export
predicted_draws.quap = predicted_draws.ulam

#' @rdname predicted_draws.ulam
#' @export
predicted_draws.map = predicted_draws.ulam

#' @rdname predicted_draws.ulam
#' @export
predicted_draws.map2stan = predicted_draws.ulam
