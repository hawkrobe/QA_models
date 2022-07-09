library(rwebppl) # devtools::install_github("mhtess/rwebppl")
library(tidyverse)
library(mvtnorm)
library(aida)    # remotes::install_github("michael-franke/aida-package")



run_model_tso <- function (params, utils) {
  
  webPPL_data <- tibble('task' = "TSO") %>% 
    cbind(params) %>% 
    cbind(utils)
  
  webppl(
    program_file = "qa-models-current.webppl",
    data = webPPL_data,
    data_var = "RInput"
  ) -> output
  
  return(output)
}

params <- tibble(
  'R0Alpha'          = 0.0001,
  'policyAlpha'      = 2.5,
  'questionerAlpha'  = 4,
  'R1Alpha'          = 3,
  'relevanceBetaR0'  = 0,
  'relevanceBetaR1'  = 0.95,
  'costWeight'       = 0.45,
  'questionCost'     = 0.25
)

utils <- tibble(
  'utilTarget'       = c(7, 7.1),
  'utilCompetitor'   = c(6, 5.9),
  'utilSameCat'      = c(4 , 4.05),
  'utilOtherCat'     = c(1, 0.8)
)

priorSampleParams <- function() {
  params <- tibble(
    'R0Alpha'          = runif(1,min = 0.00005, max = 0.0001),
    'policyAlpha'      = runif(1,min = 2, max = 3),
    'questionerAlpha'  = runif(1,min = 3.5, max = 4.5),
    'R1Alpha'          = runif(1,min = 2.5, max = 3.5),
    'relevanceBetaR0'  = 0,
    'relevanceBetaR1'  = runif(1,min = 0.95, max = 0.975),
    'costWeight'       = runif(1,min = 0.4, max = 0.5),
    'questionCost'     = runif(1,min = 0.2, max = 0.3)
  )
  return(params)
}

priorSampleUtils <- function() {
  # covariance matrix for MV-Gaussian
  sigma = matrix(c( 1.0,  0.9,  0.8, -0.5,
                    0.9,  1.0,  0.8, -0.5,
                    0.8,  0.8,  1.0, -0.5,
                   -0.5, -0.5, -0.5,  1.0), byrow = T, nrow = 4)
  # sample from MV-Guassian
  pSample <- rmvnorm(n = 1, mean = c(7,6,4,1), sigma = sigma)
  utils <- tibble(
    'utilTarget'       = pSample[1],
    'utilCompetitor'   = pSample[2],
    'utilSameCat'      = pSample[3],
    'utilOtherCat'     = pSample[4]
  )
  return(utils)
}

n_samples = 1000

priorPred <- map_df(1:3, function(i) {
  message('run ', i)
  params <- priorSampleParams()
  utils  <- priorSampleUtils()
  out    <- tibble('run' = i) %>% 
    cbind(params) %>% 
    cbind(utils) %>% 
    cbind(run_model_tso(params, utils))
  return (out)
})

priorPred %>% 
  group_by(support) %>% 
  tidyboot::tidyboot_mean(prob)

