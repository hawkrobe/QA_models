library(tidyverse)
library(here)
library(tidyboot)
library(ggthemes)

# remove experimenter id & irrelevant trials
d.raw <- read_csv(here('data/preferenceData.csv')) %>%
  filter(!is.na(wID) & aID != 'UNSAVED-STUDY') %>%
  filter(stimulus_type == 'question') %>%
  separate(scenario, into = c('_', 'scenario'), sep = "<b>") %>%
  separate(scenario, into = c('scenario', '__'), sep = "</b>") %>%
  select(-`_`, -`__`, -gameid, -stimulus_type,-iterationName) %>%
  mutate(wID = as.numeric(as.factor(wID)))
  
# bootstrap responses in each condition
d.boot <- d.raw %>% 
  group_by(itemType, scenario) %>%
  tidyboot_mean(response) %>%
  ungroup() %>%
  mutate(itemType = fct_relevel(itemType, 'close competitor', 'same category')) 

d.boot %>%
  ggplot(aes(x = itemType, y = empirical_stat, color = itemType)) +
    geom_point() +
    geom_jitter(aes(y = response), width = 0.2, alpha = 0.075, data = d.raw) +
    geom_hline(yintercept = 50, linetype = 'dotted') +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0) +
    labs(y = 'change in belief', x ='') +
    theme_few() +
    facet_grid(~ scenario) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
          legend.position = 'top', aspect.ratio = 1)

ggsave('./preferences-by-scenario.pdf', width = 8, height =)  
