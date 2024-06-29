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


# get empirical priors
urlfile="https://raw.githubusercontent.com/magpie-ea/magpie3-qa-overinfo-free-production/main/data%2Banalysis/data/PragmaticQA-E1-priorElicitation-sliderRating-full_450_byItem_means.csv"
priors <- read_csv(url(urlfile))
scenarios <- unique(priors$itemName)

urlfile="https://raw.githubusercontent.com/magpie-ea/magpie3-qa-overinfo-free-production/main/data%2Banalysis/data/results_QA-overinfo-freeTyping-cogsci_full_anonymized_categorized.csv"
empirical_responses <- read_csv(url(urlfile))

urlfile="https://raw.githubusercontent.com/magpie-ea/magpie3-qa-overinfo-free-production/main/data%2Banalysis/data/PragmaticQA-E1-priorElicitation-sliderRating-full_450_anonymized.csv"
full_matrix_data <- read_csv(url(urlfile))

##################################################

# reformat full matrix data

full_matrix <- full_matrix_data %>%
  filter(trial_type == "main") %>%
  select(submission_id, itemName, targetOption,
         itemQuestion, competitor, otherCategory, sameCategory) 

full_matrix %>%
  count(itemName, targetOption) 

#################################################

run_model_tso <- function (params, utils) {
  webPPL_data <- tibble('task' = "TSO") %>% 
    cbind(params) %>% 
    cbind(utils)
  
  webppl(
    program_file = "./03-current-models-webppl/04-run-TSO-prior-predictive-R.webppl",
    packages = c("./03-current-models-webppl/pragmaticQAModel"),
    data = webPPL_data,
    data_var = "RInput"
  ) -> output
  
  return(output)
}

priorSampleParams <- function() {
  params <- tibble(
    'policyAlpha'      = runif(1,min = 0, max = 10), #0-10
    'questionerAlpha'  = runif(1,min = 0, max = 10), #0-10
    'R1Alpha'          = runif(1,min = 0, max = 10), #0-10
    'relevanceBetaR0'  = runif(1,min = 0, max = 1), #0-1
    'relevanceBetaR1'  = runif(1,min = 0, max = 1), #0-1
    'costWeight'       = runif(1,min = 0, max = 5), #0-5
    'questionCost'     = runif(1,min = 0, max = 0) #0
  )
  return(params)
}

empiricalPrior <- function(scenario) {
  these_priors <- full_matrix %>% 
    mutate(targetOption = fct_relevel(targetOption, 'itemQuestion', 'competitor', 'sameCategory', 'otherCategory')) %>%
    group_by(targetOption) %>%
    filter(itemName == scenario) %>%
    sample_n(1)
    
  utils <- tibble(
    'utilTarget'       = these_priors$itemQuestion,
    'utilCompetitor'   = these_priors$competitor,
    'utilSameCat'      = these_priors$sameCategory,
    'utilOtherCat'     = these_priors$otherCategory
  )
  return(utils)
}

# run samples in parallel 
samples_each = 500
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
    cbind(run_model_tso(params, utils))
  return (out)
}, .progress = TRUE, .options = furrr_options(seed = 123))

write_csv(priorPred, './03-current-models-webppl/data/case_study_2_parameter_search.csv')

