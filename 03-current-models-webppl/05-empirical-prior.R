library(rwebppl) # devtools::install_github("mhtess/rwebppl")
library(tidyverse)
library(mvtnorm)
library(aida)    # remotes::install_github("michael-franke/aida-package")
library(readr)

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
  count(itemName, targetOption) %>% View()

#################################################

run_model_tso <- function (params, utils) {
  webPPL_data <- tibble('task' = "TSO") %>% 
    cbind(params) %>% 
    cbind(utils)
  
  webppl(
    program_file = "04-run-TSO-prior-predictive-R.webppl",
    packages = c("./pragmaticQAModel"),
    data = webPPL_data,
    data_var = "RInput"
  ) -> output
  
  return(output)
}

priorSampleParams <- function() {
  params <- tibble(
    'policyAlpha'      = runif(1,min = 2.25, max = 2.75),
    'questionerAlpha'  = runif(1,min = 3.75, max = 4.25),
    'R1Alpha'          = runif(1,min = 2.75, max = 3.25),
    'relevanceBetaR0'  = 0,
    'relevanceBetaR1'  = runif(1,min = 0.95, max = 0.97),
    'costWeight'       = runif(1,min = 0.5, max = 3),
    'questionCost'     = runif(1,min = 0.2, max = 0.3)
  )
  return(params)
}
# 
# empiricalPriorGaussian <- function(scenario) {
#   these_priors <- priors %>% filter(itemName == scenario, 
#                                     requested_option == "target")
#   target_val = pull(filter(these_priors, received_option == "target"), mean_response)
#   competitor_val = pull(filter(these_priors, received_option == "competitor"), mean_response)
#   same_val = pull(filter(these_priors, received_option == "sameCategory"), mean_response)
#   other_val = pull(filter(these_priors, received_option == "otherCategory"), mean_response)
#   print(these_priors)
#   # covariance matrix for MV-Gaussian
#   sigma = matrix(c( 1.0,  0.9,  0.8, -0.5,
#                     0.9,  1.0,  0.8, -0.5,
#                     0.8,  0.8,  1.0, -0.5,
#                     -0.5, -0.5, -0.5,  1.0), byrow = T, nrow = 4)
#   # sample from MV-Guassian
#   pSample <- rmvnorm(n = 1, mean = c(target_val,competitor_val,same_val,other_val), sigma = sigma)
#   utils <- tibble(
#     'utilTarget'       = pSample[1],
#     'utilCompetitor'   = pSample[2],
#     'utilSameCat'      = pSample[3],
#     'utilOtherCat'     = pSample[4]
#   )
#   return(utils)
# }

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

samples_each = 10
scenarios_rep = rep(scenarios, samples_each)
n_samples = length(scenarios_rep)

priorPred <- furrr::future_map_dfr(1:n_samples, function(i) {
  message('run ', i)
  scenario = scenarios_rep[i]
  params <- priorSampleParams()
  ## show(params)
  utils  <- empiricalPrior(scenario)
  ## show(utils)
  out    <- tibble('run' = i) %>%
    cbind(params) %>%
    cbind(scenario) %>%
    cbind(run_model_tso(params, utils))
  return (out)
}, .progress = TRUE)

write_csv(priorPred, 'priorPredbyScenarioxSubj.csv')
## priorPred <- read_csv('priorPredbyScenario.csv')

priorPredSummary <- priorPred %>% 
  dplyr::group_by(scenario, support) %>% 
  dplyr::do(aida::summarize_sample_vector(.$prob)) %>% 
  dplyr::select(-Parameter)

answerOrder <- c('taciturn', 'competitor', 'same category', 'other category', 'exhaustive', 'unclassified')

empirical_recoded <- empirical_responses %>%
  filter(trial_type != "filler") %>%
  dplyr::mutate(
    answerType = dplyr::case_when(
      category == "taciturn" ~ 'taciturn',
      category == "competitor" ~ 'competitor',
      category == "sameCategory" ~ 'same category',
      category == "fullList" ~ 'exhaustive',
      category == "otherCategory" ~ 'other category',
      category == NA_character_ ~ 'other response',
      .default = "other response"
    ) %>% factor(levels = answerOrder)
  ) %>% 
  rename(scenario = itemName) %>%
  count(scenario, answerType) %>%
  ungroup() %>%
  group_by(scenario) %>%
  mutate(mean = n/sum(n)) 

empirical_recoded %>%
  ggplot(aes(x = answerType, fill = answerType, y = mean)) +
  facet_wrap(~scenario) +
  geom_col() +
  theme(legend.position = 'none') +
  xlab('answer type') +
  ylab('mean prior predictive') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

priorpred_recoded <- priorPredSummary %>% 
  dplyr::mutate(
    answerType = dplyr::case_when(
      support == "no.---" ~ 'taciturn',
      support == "no.competitor" ~ 'competitor',
      support == "no.competitor+sameCat" ~ 'same category',
      support == "no.competitor+sameCat+otherCat" ~ 'exhaustive',
      support == "no.otherCat" ~ 'other category'
    ) %>% factor(levels = answerOrder)
  ) 

priorpred_recoded %>%
  ggplot(aes(x = answerType, fill = answerType, y = mean)) +
  facet_wrap(~scenario) +
  geom_col() +
  #geom_errorbar(aes(ymin = `|95%`, ymax = `95%|`), alpha = 0.3, width =0.2) +
  theme(legend.position = 'none') +
  xlab('answer type') +
  ylab('mean prior predictive') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

priorpred_recoded %>%
  group_by(scenario, answerType) %>%
  summarise(all_prop = sum(mean)) %>%
  ungroup() %>%
  group_by(scenario) %>%
  mutate(mean = all_prop/sum(all_prop)) %>%
  select(-all_prop) %>%
  left_join(empirical_recoded %>% rename(human_mean = mean) %>% select(-n), 
            by = c("scenario", "answerType")) %>%
  ggplot(aes(x = human_mean, y = mean)) +
  geom_point()

priorpred_recoded %>%
  group_by(scenario, answerType) %>%
  summarise(all_prop = sum(mean)) %>%
  ungroup() %>%
  group_by(scenario) %>%
  mutate(mean = all_prop/sum(all_prop)) %>%
  select(-all_prop) %>%
  left_join(empirical_recoded %>% rename(human_mean = mean) %>% select(-n), 
            by = c("scenario", "answerType")) %>%
  ggplot(aes(x = answerType, fill = answerType, y = human_mean)) +
  geom_crossbar(aes(ymin=mean,ymax=mean,y = mean),
                position = position_dodge(.5), width = .5,
                alpha = .5, size = .5) + 
  facet_wrap(~scenario) +
  geom_col() +
  #geom_errorbar(aes(ymin = `|95%`, ymax = `95%|`), alpha = 0.3, width =0.2) +
  theme(legend.position = 'none') +
  xlab('answer type') +
  ylab('mean prior predictive') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

