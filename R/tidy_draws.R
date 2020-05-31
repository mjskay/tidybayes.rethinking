# tidy_draws
#
# Author: mjskay
###############################################################################


#' Get a sample of posterior draws from quap and map models as a tibble
#'
#' Implementation of [tidybayes::tidy_draws()] for [rethinking::map()] and [rethinking::quap()]
#' models. Extract draws from a Bayesian fit into a wide-format data frame with a
#' `.chain`, `.iteration`, and `.draw` column, as well as all variables as columns.
#' While this function can be useful for quick glances at models (especially
#' combined with [gather_variables()] and [median_qi()]), it is
#' generally speaking not as useful as [spread_draws()] or
#' [gather_draws()] for most applications, and is mainly used internally.
#'
#' @details
#'
#' The main additional functionality compared to [tidybayes::tidy_draws()] when used on
#' other models is that since draws must be generated on-the-fly,
#' an argument (`n`) is provided to indicate how many draws to take. The `.chain` and
#' `.iteration` columns are also always `NA`, since they have no meaning for these model
#' types (use the `.draw` column if you need to index draws). Otherwise, the result of
#' this function follows the same format as [tidybayes::tidy_draws()]; see that
#' documentation for more information.
#'
#' @examples
#'
#' library(rethinking)
#' library(tidybayes)
#' library(magrittr)
#'
#' m = quap(alist(
#'     mpg ~ dlnorm(mu, sigma),
#'     mu <- a + b*wt,
#'     c(a,b) ~ dnorm(0, 10),
#'     sigma ~ dexp(1)
#'   ),
#'   data = mtcars,
#'   start = list(a = 4, b = -1, sigma = 1)
#' )
#'
#' m %>%
#'   tidy_draws() %>%
#'   gather_variables() %>%
#'   median_qi()
#'
#' @importFrom tidybayes tidy_draws
#' @importFrom MASS mvrnorm
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @export
tidy_draws.map = function(model, n = 10000, ...) {
  mu = rethinking::coef(model)
  draws = as_tibble(mvrnorm(n = n, mu = mu, Sigma = rethinking::vcov(model)))

  #map models have no chains
  draws = bind_cols(tibble(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = 1:nrow(draws)
    ),
    draws
  )

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @rdname tidy_draws.map
#' @export
tidy_draws.quap = tidy_draws.map

#' @export
tidy_draws.map2stan = function(model, ...) {
  draws = tidy_draws(model@stanfit, ...)

  attr(draws, "tidybayes_constructors") = attr(model, "tidybayes_constructors")
  draws
}

#' @export
tidy_draws.ulam = tidy_draws.map2stan
