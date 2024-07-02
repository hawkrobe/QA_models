library(rwebppl) # devtools::install_github("mhtess/rwebppl")
library(tidyverse)
library(aida)    # remotes::install_github("michael-franke/aida-package")
library(readr)
library(furrr)
library(feather)

##################################################

# these options help Stan run faster
options(mc.cores = parallel::detectCores())

# use the aida-theme for plotting
theme_set(theme_aida())

# global color scheme / non-optimized
project_colors = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# setting theme colors globally
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = project_colors)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = project_colors)
} 

##################################################

# get empirical data + prior preferences

urlfile = "https://raw.githubusercontent.com/magpie-ea/magpie3-qa-overinfo-free-production/main/data%2Banalysis/data/results_e2_human_category_cleaned4modeling.csv"
empirical_responses <- read_csv(url(urlfile))

urlfile = "https://raw.githubusercontent.com/magpie-ea/magpie3-qa-overinfo-free-production/main/data%2Banalysis/data/PragmaticQA-E2-priorElicitation-sliderRating-full_120.csv"
priors <- read_csv(url(urlfile))
scenarios <- unique(empirical_responses$itemName)

#######################################

policyAlpha = c(1,3,5,7,9)
questionerAlpha = c(1,3,5,7,9)
R1Alpha = c(1,3,5,7,9)
relevanceBetaR0 = c(0)
relevanceBetaR1 = c(0.1, 0.3, 0.5, 0.7, 0.9)
costWeight = c(0.1, 0.3, 0.5, 0.7, 0.9)
n_sample <- c(1,2,3,4,5)
questionCost <- c(0)

param_space <- expand_grid(policyAlpha, questionerAlpha, R1Alpha, relevanceBetaR0, relevanceBetaR1, costWeight, questionCost, scenarios, n_sample)

##############################################

run_model_tsos <- function (params, utils) {
  webPPL_data <- tibble('task' = "TSOS") %>% 
    cbind(params) %>% 
    cbind(utils)
  
  webppl(
    program_file = "./03-current-models-webppl/06-run-multicontext-prior-predictive-R.webppl",
    packages = c("./03-current-models-webppl/pragmaticQAModel"),
    data = webPPL_data,
    data_var = "RInput"
  ) -> output
  
  return(output)
}

priorSampleParams <- function() {
  params <- tibble(
    'policyAlpha'      = runif(1,min = 8, max = 10), # searched 0-10
    'questionerAlpha'  = runif(1,min = 8, max = 10), # searched 0-10
    'R1Alpha'          = runif(1,min = 0, max = 2), # searched 0-10
    'relevanceBetaR0'  = runif(1,min = 0, max = 0), # fixed at 0
    'relevanceBetaR1'  = runif(1,min = 0.6, max = 0.8), # searched 0-1
    'costWeight'       = runif(1,min = 1, max = 2), # searched 0-5
    'questionCost'     = runif(1,min = 0, max = 0) # fixed at 0
  )
  return(params)
}


empiricalPrior <- function(scenario) {
  these_priors <- priors %>% 
    filter(itemName == scenario) %>%
    sample_n(1)
    
  utils <- tibble(
    'utilTarget'       = these_priors$itemQuestion,
    'utilCompetitor'   = these_priors$competitor,
    'utilSameCat'      = these_priors$sameCategory,
    'utilOtherCat'     = these_priors$otherCategory,
    'utilMostSimilar'     = these_priors$mostSimilar
  )
  return(utils)
}

# run samples in parallel 
samples_each = 200 # 1000 for param search
scenarios_rep = rep(scenarios, samples_each)
n_samples = length(scenarios_rep)


param_search = TRUE

plan(multisession, workers = 100)

if (param_search == TRUE) {
  n_samples = nrow(param_space)
}
priorPred <- furrr::future_map_dfr(1:n_samples, function(i) {
  message('run ', i)
  if (param_search == TRUE) {
    scenario <- param_space[i,]['scenarios'] %>% pull()
    params <- param_space[i,] %>% select(-scenarios, -n_sample) %>% tibble()
  } else {
    scenario = scenarios_rep[i]
    params <- priorSampleParams()
  }
  utils  <- empiricalPrior(scenario)
  out    <- tibble('run' = i) %>%
    cbind(params) %>%
    cbind(scenario) %>%
    cbind(run_model_tsos(params, utils))
    return (out)
}, .progress = TRUE, .options = furrr_options(seed = 123))


write_feather(priorPred, './03-current-models-webppl/data/case_study_3_parameter_search.feather')
write_csv(priorPred, './03-current-models-webppl/data/case_study_3_parameter_search.csv')
#write_csv(priorPred, './03-current-models-webppl/data/case_study_3_RSA_preds.csv')

