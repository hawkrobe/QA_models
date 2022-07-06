library(tidyverse)
library(tidyboot)
library(aida)

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

answerOrder <- c('taciturn', 'competitor', 'same category', 'other category', 'exhaustive')

GPT3Pred <- read_csv('GPT3-predictions-overinfo.csv') %>% 
  pivot_longer(cols = -itemName, names_to = "answerType", values_to = "prediction") %>% 
  mutate(
    answerType = case_when(
      answerType == 'sameCategory' ~ 'same category', 
      answerType == 'otherCategory' ~ 'other category', 
      answerType == 'fullList' ~ 'exhaustive', 
      TRUE ~ answerType
    ),
    answerType = factor(answerType, levels = answerOrder),
    model = "GPT3"
  )  

GPT3PredOneShot <- read_csv('GPT3-predictions-overinfo-oneShotLearner.csv') %>% 
  pivot_longer(cols = -itemName, names_to = "answerType", values_to = "prediction") %>% 
  mutate(
    answerType = case_when(
      answerType == 'sameCategory' ~ 'same category', 
      answerType == 'otherCategory' ~ 'other category', 
      answerType == 'fullList' ~ 'exhaustive', 
      TRUE ~ answerType
    ),
    answerType = factor(answerType, levels = answerOrder),
    model = "GPT3-OneShot"
  ) 
 
GPT3Pred <- rbind(GPT3Pred, GPT3PredOneShot)
 
##################################################

# get means and bootstrapped CIs, averaging across items
sumStatsGPT3Pred <- GPT3Pred %>% 
  group_by(answerType, model) %>% 
  tidyboot_mean(column = prediction) %>% 
  arrange(answerType)

# plot of average predictions over items
sumStatsGPT3Pred %>% 
  ggplot(aes(x = answerType, fill = answerType, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.3, width =0.2) +
  facet_grid(~ model) +
  theme(legend.position = 'none') +
  xlab('answer type') +
  ylab('prediction')

# plot of predictions per item
GPT3Pred %>%
  ggplot(aes(x = answerType, fill = answerType, y = prediction)) +
  geom_col() +
  facet_grid(itemName ~ model) +
  theme(legend.position = 'none') +
  xlab('answer type') +
  ylab('prediction') + 
  theme(strip.text.y = element_text(angle = 0))

