library(rwebppl) # devtools::install_github("mhtess/rwebppl")
library(tidyverse)
library(aida)    # remotes::install_github("michael-franke/aida-package")


run_plot_model <- function (model_file_name = "qa-models-current.webppl", task_name = "R1Posterior_BinaryPrefs", file_name_addition = "", highlight1 = c(), highlight2 = c(), highlight3 = c(), highlight4 = c()) {
  webPPL_data = tibble('task' = task_name)
  webppl(
    program_file = model_file_name,
    data = webPPL_data,
    data_var = "RInput"
  ) -> output

  nr_highlight_levels = c(length(highlight1) >0, length(highlight2) >0, length(highlight3) >0, length(highlight4) >0) %>% sum()
  print(nr_highlight_levels)
  if (nr_highlight_levels == 0) {
    highlight_values = "#505B55"
  } else {
    highlight_values = c(c("#B04035", "#FFA811", "#6494ED", "#006600")[1:nr_highlight_levels], "#505B55")
  }

  output %>%
    as_tibble() %>%
    mutate(support = fct_reorder(support, round(prob,3))) %>%
    mutate(
      highlight_level = case_when(
        support %in% highlight1 ~ "highlight_1",
        support %in% highlight2 ~ "highlight_2",
        support %in% highlight3 ~ "highlight_3",
        support %in% highlight4 ~ "highlight_4",
        TRUE ~ "no_highlight"
      )
    ) %>%
    filter(prob >= 1e-20) %>%
    ggplot(aes(
      x = support,
      y = prob,
      fill = highlight_level
      )) +
    geom_col() + coord_flip() +
    scale_fill_manual(name = "area", values=highlight_values) +
    xlab("") +
    ylab("") +
    theme_aida() +
    theme(legend.position="none")

  factor = 3
  ggsave(filename = paste0("pics/results-", task_name, file_name_addition, ".pdf"), width = 16/factor, height = 9/factor)

}

run_plot_model(task_name = "safeAnswererPositive")
run_plot_model(task_name = "safeAnswererNegative")

run_plot_model(task_name = "pieCakeContextMinimal")
run_plot_model(task_name = "pieCakeContextMinimalWithPreferences",
               highlight1 = "RP?",
               highlight2 = "LC?",
               highlight3 = c("AS?", "SC?"))
run_plot_model(task_name = "pieCakeContext")
run_plot_model(task_name = "pieCakeContextAdditivePreferences", highlight1 = "Anything w/ raspberry?", highlight2 = "Anything w/ lemon?", highlight3 = "Pie?", highlight4 = "Cake?")
run_plot_model(task_name = "pieCakeContextBiasedPessimist", highlight1 = "Which goods?", highlight2= "Anything?")
run_plot_model(task_name = "pieCakeContextUnbiasedNoPref", file_name_addition = "-against-pessimist", highlight1 = "Which goods?", highlight2= "Anything?")
run_plot_model(task_name = "pieCakeContextUnbiasedNoPref", file_name_addition = "no-highlight")
run_plot_model(task_name = "pieCakeContextBiasedNoPref", highlight1 = c("Which goods?"), highlight2 = c("Raspberry pie?"))
run_plot_model(task_name = "pieCakeContextUnbiasedNoPref", file_name_addition = "-against-opinionated", highlight1 = c("Which goods?"), highlight2 = c("Raspberry pie?", "Lemon cake?"))


run_plot_model(task_name = "R1Responses_BinaryPrefs")
run_plot_model(task_name = "R1Posterior_BinaryPrefs")


## get data for continuous inference

webPPL_data = tibble('task' = "continuousInference")
contInf_data <- webppl(
    program_file = "qa-models-current.webppl",
    data = webPPL_data,
    data_var = "RInput"
  ) %>% 
  pivot_wider(id_cols = Iteration, names_from = Parameter, values_from = value)
  
contInf_data %>%   mutate(RP_smaller = RP<LC) %>% 
    ggplot(aes(x=RP, y=LC, color = RP_smaller)) +
  geom_point(size=0.8, alpha = 0.3) + theme_aida() +
  theme(legend.position = "none") +
  xlim(-5,10) + ylim(-5,10) +
  scale_color_manual(values=highlight_values[c(3,5)])

factor = 3
ggsave(filename = paste0("pics/results-", "continuousInference", ".pdf"), width = 9/factor, height = 6/factor)

message("Posterior of RP > 0: ", mean(contInf_data$RP > 0))
message("Posterior of LC > 0: ", mean(contInf_data$LC > 0))
message("Posterior of RP > LP: ", mean(contInf_data$RP > contInf_data$LC))


        
highlight_values = c("#B04035", "#FFA811", "#6494ED", "#006600", "#505B55")
## prior plot
prior_samples <- tibble(RP = rnorm(4000,2,2),
       LC = RP + rnorm(4000,0,1),
       RP_smaller = RP < LC) 

message("Prior of RP > 0: " , mean(prior_samples$RP > 0))
message("Prior of LC > 0: " , mean(prior_samples$LC > 0))
message("Prior of RP > LP: ", mean(prior_samples$RP > prior_samples$LC))


prior_samples %>% 
  ggplot(aes(x=RP, y=LC, color = RP_smaller)) +
  geom_point(size=0.8, alpha = 0.3) + theme_aida() +
  theme(legend.position = "none") +
  xlim(-5,10) + ylim(-5,10) +
  scale_color_manual(values=highlight_values[c(3,5)])
  
factor = 3
ggsave(filename = paste0("pics/results-", "continuousInference-prior", ".pdf"), width = 9/factor, height = 6/factor)
  