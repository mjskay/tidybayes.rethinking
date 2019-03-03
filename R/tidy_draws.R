# tidy_draws
#
# Author: mjskay
###############################################################################


#' @importFrom tidybayes tidy_draws
#' @importFrom MASS mvrnorm
#' @importFrom stats coef vcov
#' @export
tidy_draws.map = function(model) {
  mu = coef(model)
  samples = as_tibble(mvrnorm(n = 10000, mu = mu, Sigma = vcov(model)))
  #map models have no chains
  bind_cols(data_frame(.chain = 1, .iteration = 1:nrow(samples)), samples)
}

#' @export
tidy_draws.map2stan = function(model) {
  tidy_draws(model@stanfit)
}
