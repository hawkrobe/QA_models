library(rwebppl) # devtools::install_github("mhtess/rwebppl")
library(tidyverse)
library(aida)    # remotes::install_github("michael-franke/aida-package")
library(readr)
library(furrr)

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
scenarios <- unique(priors$itemName)

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
    'policyAlpha'      = runif(1,min = 0, max = 10), # searched 0-10
    'questionerAlpha'  = runif(1,min = 0, max = 10), # searched 0-10
    'R1Alpha'          = runif(1,min = 0, max = 10), # searched 0-10
    'relevanceBetaR0'  = runif(1,min = 0, max = 0), # fixed at 0
    'relevanceBetaR1'  = runif(1,min = 0, max = 1), # searched 0-1
    'costWeight'       = runif(1,min = 0, max = 5), # searched 0-5
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
samples_each = 1000 # 1000 for param search
scenarios_rep = rep(scenarios, samples_each)
n_samples = length(scenarios_rep)

plan(multisession, workers = 100)
priorPred <- furrr::future_map_dfr(1:n_samples, function(i) {
  message('run ', i)
  scenario = scenarios_rep[i]
  params <- priorSampleParams()
  utils  <- empiricalPrior(scenario)
  out    <- tibble('run' = i) %>%
    cbind(params) %>%
    cbind(scenario) %>%
    cbind(run_model_tsos(params, utils))
  return (out)
}, .progress = TRUE, .options = furrr_options(seed = 123))

write_csv(priorPred, './03-current-models-webppl/data/case_study_3_parameter_search.csv')
#write_csv(priorPred, './03-current-models-webppl/data/case_study_3_RSA_preds.csv')

