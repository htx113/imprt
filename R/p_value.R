#' P-value Calculation with Multiple Imputation
#'
#' @param null A vector of two values representing two exposure groups specified
#' in the null hypothesis.
#' @param exposure_matrix A matrix of dimension (number of units x number of
#' randomizations, i.e. assignments),
#' where each column represents different exposure assignments for
#' randomizations.
#' @param observation A numeric vector of observed values.
#' @param obs_index An integer indicating which column of the exposure_matrix is
#' correspondent to the observed exposure.
#' @param impute_function A function used for imputing missing values; defaults
#' to 'Imputation_sample'.
#' @param statistic A function used to calculate the test statistic; defaults to
#' 'test_statistic', which is the absolute value of the difference-in-means test
#' statistic.
#'
#' @return A numeric value representing the mean of p-values computed via
#' multiple imputation.
#'
#' @examples
#' # This example illustrates the use of the imputation-based randomization test
#' # (IRT) to calculate p-value in the setting of clustered interference.
#'
#' library(imprt)
#' N <- 300
#' K <- 150
#' size <- N/K # size of clusters
#' K_treat <- floor(K/2) # number of treated clusters
#'
#' # set of randomly selected treatment assignments
#' set.seed(113)
#' num_randomizations <- 1000
#' housestruct <- rep(N/K,K)
#'
#' Z <- matrix(0, nrow=N, ncol=num_randomizations)
#' for (id_rand in 1:num_randomizations){
#'   W <- matrix(rep(0,N),nc=K)
#'   treat_cluster <- sort(sample(c(1:K),K_treat))
#'   treat_unit <- apply(as.matrix(housestruct[treat_cluster]),1,FUN = function(x){
#'     sample(c(1:x),1)
#'   })
#'   for (i in 1:K_treat){
#'     W[treat_unit[i],treat_cluster[i]] <- 1
#'   }
#'   Z[,id_rand] <- c(W)
#' }
#'
#' exposure_matrix <- sapply(c(1:num_randomizations), function(i){
#'   Z_temp = Z[,i]
#'   f_temp = exposure(Z_temp,K)
#'   return(f_temp)
#' })
#'
#' null <- c(0,1)
#'
#' # generate potential outcomes
#' set.seed(113)
#' Ypot <- matrix(0, N, 2)
#' tau <- 0.5
#' Ypot[,1] <- rnorm(N, 0, 1)
#' Ypot[,2] <- Ypot[,1] + tau
#' Ypot <- data.frame(Ypot)
#' colnames(Ypot) <- c('exp0','exp1')
#
#' obs_index <- 1
#' exposure_obs <- exposure_matrix[,obs_index]
#' observation <- rep(NA,N)
#' observation[exposure_obs == 0] <-Ypot$exp0[exposure_obs == 0]
#' observation[exposure_obs == 1] <-Ypot$exp1[exposure_obs == 1]
#
#' set.seed(113)
#' pvalue_MI(null,exposure_matrix,observation,
#'           obs_index,impute_function=Imputation_sample,
#'           statistic=test_statistic)
#'
#' @export
pvalue_MI <- function(null=c(0,1),exposure_matrix,observation,
                      obs_index=1,impute_function=Imputation_sample,
                      statistic=test_statistic){
  num_randomizations <- ncol(exposure_matrix)
  exposure_obs <- exposure_matrix[,obs_index]
  T_obs <- test_statistic(null,exposure_obs,observation)
  p_temp <- c()
  for (j in 1:num_randomizations) {
    # pseudo data
    Y_pseu <- observation
    Y_pseu[!exposure_obs %in% null] <- impute_function(null,exposure_obs,observation)
    exposure_temp <- exposure_matrix[,j]
    T_temp <- test_statistic(null,exposure_temp,Y_pseu)
    p_temp[j] <- ifelse(T_temp >= T_obs,1,0)
  }
  return(mean(p_temp))
}
