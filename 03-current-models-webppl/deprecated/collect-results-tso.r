library(rwebppl) # devtools::install_github("mhtess/rwebppl")
library(tidyverse)
library(mvtnorm)
library(aida)    # remotes::install_github("michael-franke/aida-package")


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

priorSampleUtils <- function() {
  # covariance matrix for MV-Gaussian
  sigma = matrix(c( 1.0,  0.9,  0.8, -0.5,
                    0.9,  1.0,  0.8, -0.5,
                    0.8,  0.8,  1.0, -0.5,
                   -0.5, -0.5, -0.5,  1.0), byrow = T, nrow = 4)
  # sample from MV-Guassian
  pSample <- rmvnorm(n = 1, mean = c(7,5,4,2), sigma = sigma)
  utils <- tibble(
    'utilTarget'       = pSample[1],
    'utilCompetitor'   = pSample[2],
    'utilSameCat'      = pSample[3],
    'utilOtherCat'     = pSample[4]
  )
  return(utils)
}

priorSampleParamsFixed <- function() {
  params <- tibble(
    'policyAlpha'      = 2.5,
    'questionerAlpha'  = 4,
    'R1Alpha'          = 3,
    'relevanceBetaR0'  = 0,
    'relevanceBetaR1'  = 0.96,
    'costWeight'       = 0.45,
    'questionCost'     = 0.25
  )
  return(params)
}

priorSampleUtilsFixed <- function() {
  utils <- tibble(
    'utilTarget'       = 15,
    'utilCompetitor'   = 10,
    'utilSameCat'      = 10,
    'utilOtherCat'     = 5
  )
  return(utils)
}

n_samples = 100

priorPred <- map_df(1:n_samples, function(i) {
  message('run ', i)
  params <- priorSampleParams()
  ## params <- priorSampleParamsFixed()
  ## show(params)
  utils  <- priorSampleUtils()
  ## utils  <- priorSampleUtilsFixed()
  ## show(utils)
  out    <- tibble('run' = i) %>%
    cbind(params) %>%
    cbind(utils) %>%
    cbind(run_model_tso(params, utils))
  return (out)
})

write_csv(priorPred, 'priorPred.csv')
## priorPred <- read_csv('priorPred.csv')

priorPredSummary <- priorPred %>% 
  group_by(support) %>% 
  do(aida::summarize_sample_vector(.$prob)) %>% 
  select(-Parameter)


answerOrder <- c('taciturn', 'competitor', 'same category', 'other category', 'exhaustive', 'unclassified')

priorPredSummary %>% 
  mutate(
    answerType = case_when(
      support == "no.---" ~ 'taciturn',
      support == "no.competitor" ~ 'competitor',
      support == "no.competitor+sameCat" ~ 'same category',
      support == "no.competitor+sameCat+otherCat" ~ 'exhaustive',
      support == "no.otherCat" ~ 'other category'
    ) %>% factor(levels = answerOrder)
  ) %>% 
  ggplot(aes(x = answerType, fill = answerType, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = `|95%`, ymax = `95%|`), alpha = 0.3, width =0.2) +
  theme(legend.position = 'none') +
  xlab('answer type') +
  ylab('mean prior predictive')


