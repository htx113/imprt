
<!-- README.md is generated from README.Rmd. Please edit that file -->

# imprt

<!-- badges: start -->
<!-- badges: end -->

The goal of imprt is to conduct imputation-based randomization test when
interference exists between units.

## Installation

You can install the development version of imprt from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("htx113/imprt")
```

## Example

This is a basic example illustrates the use of the imputation-based
randomization test (IRT) for testing spill-over effect in clustered
interference.

``` r
library(imprt)

### basic setting
N <- 300 # number of units
K <- 150 # number of clusters
size <- N/K # size of clusters
K_treat <- floor(K/2) # number of treated clusters

### set of randomly selected treatment assignments
set.seed(113)
num_randomizations <- 1000 # number of randomizations
housestruct <- rep(N/K,K)

Z <- matrix(0, nrow=N, ncol=num_randomizations)
for (id_rand in 1:num_randomizations){
  W <- matrix(rep(0,N),nc=K)
  treat_cluster <- sort(sample(c(1:K),K_treat))
  treat_unit <- apply(as.matrix(housestruct[treat_cluster]),1,FUN = function(x){
    sample(c(1:x),1)
  })
  for (i in 1:K_treat){
    W[treat_unit[i],treat_cluster[i]] <- 1
  }
  Z[,id_rand] <- c(W)
}

### matrix of exposure
exposure_matrix <- sapply(c(1:num_randomizations), function(i){
  Z_temp <- Z[,i]
  f_temp <- exposure(Z_temp,K)
  return(f_temp)
})

### exposures included in the null hypothesis
null <- c(0,1)

### generate potential outcomes for exposure 0 and 1
set.seed(113)
Ypot <- matrix(0, N, 2)
tau <- 0.5
Ypot[,1] <- rnorm(N, 0, 1)
Ypot[,2] <- Ypot[,1] + tau
Ypot <- data.frame(Ypot)
colnames(Ypot) <- c('exp0','exp1')

### p-value when the observed assignment is the first one
obs_index <- 1
exposure_obs <- exposure_matrix[,obs_index]
observation <- rep(NA,N)
observation[exposure_obs == 0] <- Ypot$exp0[exposure_obs == 0]
observation[exposure_obs == 1] <- Ypot$exp1[exposure_obs == 1]

set.seed(113)
### IRT with empirical distribution
pvalue_MI(null,exposure_matrix,observation,
          obs_index,impute_function=Imputation_sample,
          statistic=test_statistic)
#> [1] 0.001
### IRT with normal distribution
pvalue_MI(null,exposure_matrix,observation,
          obs_index,impute_function=Imputation_normal,
          statistic=test_statistic)
#> [1] 0.001

### power of IRT with empirical distribution
set.seed(113)
result <- power_MI(null,Ypot,exposure_matrix,impute_function=Imputation_sample,
                   statistic=test_statistic)
#> Process: 100 / 1000 
#> Process: 200 / 1000 
#> Process: 300 / 1000 
#> Process: 400 / 1000 
#> Process: 500 / 1000 
#> Process: 600 / 1000 
#> Process: 700 / 1000 
#> Process: 800 / 1000 
#> Process: 900 / 1000 
#> Process: 1000 / 1000
## histogram of p-value
# hist(result$p)
## power
result$power
#> [1] 0.953

# ### power of IRT with normal distribution
# set.seed(113)
# result <- power_MI(null,Ypot,exposure_matrix,impute_function=Imputation_normal,
#                    statistic=test_statistic)
# ## histogram of p-value
# # hist(result$p)
# ## power
# result$power
```

## Reference

Tingxuan Han, Ke Zhu, Ke Deng and Hanzhong Liu. (2024) “Imputation-based
randomization tests for randomized experiments with interference.”
