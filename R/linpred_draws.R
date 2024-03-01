# linpred_draws
#
# Author: mjskay
###############################################################################


# deprecated names for linpred_draws -------------------------------

#' @rdname tidybayes.rethinking-deprecated
#' @format NULL
#' @usage NULL
#' @importFrom tidybayes linpred_draws
#' @export
tidy_link = function(data, fit, ...) {
  .Deprecated("add_linpred_draws", "tidybayes.rethinking")

  linpred_draws(object = fit, newdata = data, ...)
}

#' @importFrom tidybayes fitted_draws
#' @export
fitted_draws.ulam = function(model, newdata, ..., value = ".value", n = NULL) {
  .Deprecated("linpred_draws", "tidybayes", paste0(
    "`fitted_draws` and `add_fitted_draws` are deprecated as their names were confusing.\n",
    "Use [add_]linpred_draws() to get the distribution of the linear predictor."
  ))

  linpred_draws(object = model, newdata = newdata, ..., value = value, ndraws = n)
}
#' @export
linpred_draws.quap = fitted_draws.ulam
#' @export
linpred_draws.map = fitted_draws.ulam
#' @export
linpred_draws.map2stan = fitted_draws.ulam


# linpred_draws ------------------------------------------------------------

#' Add draws from the posterior link-level predictor of a rethinking model to a data frame
#'
#' Adds draws from the posterior link-level predictor of a rethinking model to a data frame.
#' Provides support for [tidybayes::linpred_draws()] / [tidybayes::add_linpred_draws()] /
#' for models from the `rethinking` package.
#'
#' @inheritParams tidybayes::linpred_draws
#' @param object A model fit using `rethinking::quap()`, `rethinking::ulam()`,
#' `rethinking::map()`, or `rethinking::map2stan()`.
#' @param ... Optional parameters passed on to `rethinking::link()`. The most pertinent are:
#'   - `replace`: Optional named list of samples to replace inside posterior samples. See examples in `rethinking::link()`.
#' @param post Optional samples from posterior. When missing, `linpred_draws()` extracts these in advance,
#'   bypassing `rethinking::link()`'s normal process (`rethinking::link()` uses `rstan::extract()`, which
#'   unfortunately permutes samples, breaking the ability of the `.draw` column to be meaningfully joined
#'   with output from other methods, like `spread_draws()`).
#' @param ndraws The number of draws per fit to return. When `NULL` (the default), `rethinking::ulam()` and
#' `rethinking::map2stan()` models return all draws; `rethinking::quap()` and `rethinking::map()` models
#' return 5000 draws.
#' @param dpar Should distributional regression
#' parameters be included in the output? In rethinking models, these correspond to the linear submodels
#' returned by `rethinking::link()`. If `TRUE`, distributional regression
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. `c(sigma.hat = "sigma")`
#' would output the `"sigma"` parameter from a model as a column named `"sigma.hat"`).
#' If `FALSE` (the default), distributional regression parameters are not included; instead,
#' just the first linear submodel returned by `rethinking::link()` is used.
#' @param re_formula,category Not used with this model type.
#' @importFrom rlang is_true is_false is_empty
#' @importFrom tidybayes linpred_draws add_draws sample_draws
#' @importFrom rethinking extract.samples
#' @export
linpred_draws.ulam = function(object, newdata, value = ".value", ..., post = NULL, ndraws = NULL, seed = NULL,
  dpar = FALSE, re_formula = NULL, category = ".category"
) {
  if (!is.null(re_formula)) {
    warning("The re_formula parameter is not supported by rethinking models; ignored.")
  }
  if (category != ".category") {
    warning("The category parameter is not supported by rethinking models; ignored.")
  }

  set.seed(seed)

  # map and quap models need to specify the number of draws (since they are generated)
  unpermute_samples = FALSE
  if (inherits(object, "map") || inherits(object, "quap")){
    if (is.null(ndraws)) {
      ndraws = 5000
    }
    if (is.null(post)) {
      post = extract.samples(object, n = ndraws)
    }
  } else if (hasName(attributes(object), "stanfit") && is.null(post)) {
    # have to do this manually because rethinking::link with rstan under the hood
    # uses permuted samples (!!) and can't use unpermuted samples because when
    # permuted = FALSE the format of the returned samples changes (!!!!)
    # see the statement guarded by unpermute_samples below
    unpermute_samples = TRUE
    post = rethinking::extract.samples(object, permuted = TRUE)
  }

  # get the draws from the link-level predictors
  draws_list = if (is.null(post)) {
    rethinking::link(object, newdata, n = ndraws, flatten = FALSE, ...)
  } else {
    rethinking::link(object, newdata, n = ndraws, post = post, flatten = FALSE, ...)
  }
  draws = add_draws(newdata, draws_list[[1]], value = value)

  # get the names of distributional regression parameters to include
  dpars = if (is_true(dpar)) {
    names(draws_list)
  } else if (is_false(dpar)) {
    NULL
  } else {
    dpar
  }
  if (is_empty(dpars)) {
    # the above conditions might return an empty vector, which does not play well with the code below
    # (if there are no dpars, it is expected that dpars is NULL)
    dpars = NULL
  }

  # missing names default to the same name used for the parameter in the model
  if (is.null(names(dpars))) {
    names(dpars) = dpars
  } else {
    missing_names = is.na(names(dpars)) | names(dpars) == ""
    names(dpars)[missing_names] = dpars[missing_names]
  }

  for (i in seq_along(dpars)) {
    varname = names(dpars)[[i]]
    dpar_linpred_draws = add_draws(newdata, draws_list[[dpars[[i]]]], value = ".value")
    draws[[varname]] = dpar_linpred_draws[[".value"]]
  }

  if (unpermute_samples) {
    # unpermute the samples
    # TODO: this is an awful hack!
    perm = stanfit_permutation(object@stanfit)
    draws$.draw = perm[draws$.draw]
  }

  # ulam and map2stan models seem to ignore n
  if ((inherits(object, "map2stan") || inherits(object, "ulam")) && !is.null(ndraws)) {
    draws = sample_draws(draws, ndraws)
  }

  draws
}

#' @rdname linpred_draws.ulam
#' @export
linpred_draws.quap = linpred_draws.ulam

#' @rdname linpred_draws.ulam
#' @export
linpred_draws.map = linpred_draws.ulam

#' @rdname linpred_draws.ulam
#' @export
linpred_draws.map2stan = linpred_draws.ulam


# helpers -----------------------------------------------------------------

stanfit_permutation = function(stanfit) {
  # the draw order permutation in a stanfit used with rstan::extract()
  chain_perm = stanfit@sim$permutation
  n_per_chain = length(chain_perm[[1]])
  unlist(lapply(seq_along(chain_perm), function(chain) {
    perm = chain_perm[[chain]]
    (chain - 1)*n_per_chain + perm
  }))
}
