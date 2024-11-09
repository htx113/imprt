#' Imputation with Normal Distribution
#'
#' This function performs imputation for missing observations based on observed
#' data.
#' The observed data are modeled with a normal distribution, and
#' the missing observations are imputed with the posterior predictive
#' distribution
#' with non-informative prior.
#'
#' @param null A vector of two values representing two exposure groups specified
#' in the null hypothesis.
#' It should specify the two levels of the 'exposure' variable that are being
#' compared.
#' @param exposure A vector indicating the exposure status for each observation.
#' The values in this vector should contain the levels provided in the 'null'
#' vector.
#' @param observation A numeric vector of observed values.
#'
#' @return A vector of imputed values sampled from the observed data.
#' @export
#' @importFrom stats rt
#' @importFrom stats var
Imputation_normal <- function(null,exposure,observation){
  n <- length(exposure)
  n_obs <- sum(exposure %in% null)
  n_mis <- n - n_obs
  data_obs <- observation[which(exposure %in% null)]
  A <- mean(data_obs)
  B <- var(data_obs)
  imp <- A + sqrt((1 + 1/n_obs)*B) * rt(n_mis,df=n_obs-1)
  return(imp)
}
