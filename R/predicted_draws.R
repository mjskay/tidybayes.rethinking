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

  fitted_draws(model = fit, newdata = data, ...)
}


# predicted_draws ------------------------------------------------------------

#' Add draws from the posterior predictive of a rethinking model to a data frame
#'
#' Adds draws from the posterior predictive distribution of a rethinking model to a data frame.
#' Provides support for [tidybayes::predicted_draws()] / [tidybayes::add_predicted_draws()] for
#' models from the `rethinking` package.
#'
#' @inheritParams tidybayes::predicted_draws
#' @param model A model fit using `rethinking::quap()`, `rethinking::ulam()`,
#' `rethinking::map()`, or `rethinking::map2stan()`.
#' @param ... Optional parameters passed on to `rethinking::sim()`. The most pertinent are:
#'   - `post`: Optional samples from posterior. If missing, simulates samples using `n`.
#' @param n The number of draws per prediction to return. When `NULL` (the default), `rethinking::ulam()` and
#' `rethinking::map2stan()` models return all draws; `rethinking::quap()` and `rethinking::map()` models
#' return 5000 draws.
#' @param re_formula,category Not used with this model type.
#' @importFrom rlang is_true is_false is_empty
#' @importFrom tidybayes predicted_draws add_draws sample_draws
#' @export
predicted_draws.ulam = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL,
  re_formula = NULL, category = ".category"
) {
  if (!is.null(re_formula)) {
    warning("The re_formula parameter is not supported by rethinking models; ignored.")
  }
  if (category != ".category") {
    warning("The category parameter is not supported by rethinking models; ignored.")
  }

  # map and quap models need to specify the number of draws (since they are generated)
  if ((inherits(model, "map") || inherits(model, "quap")) && is.null(n)) {
    n = 5000
  }

  # get the draws from the posterior predictive
  set.seed(seed)
  sims = rethinking::sim(model, newdata, n = n, ...)
  draws = add_draws(newdata, sims, value = prediction)

  # ulam and map2stan models seem to ignore n
  if ((inherits(model, "map2stan") || inherits(model, "ulam")) && !is.null(n)) {
    draws = sample_draws(draws, n)
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
