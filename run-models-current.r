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
    highlight_values = c(c("#B04035","#FFA811", "#6494ED", "#006600")[1:nr_highlight_levels], "#505B55")
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
    filter(prob != 0) %>%
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

run_plot_model(task_name = "pieCakeContextMinimal")
run_plot_model(task_name = "pieCakeContextMinimalWithPreferences",
               highlight1 = "RP?",
               highlight2 = "LC?",
               highlight3 = c("AS?", "SC?"))
run_plot_model(task_name = "pieCakeContext")
run_plot_model(task_name = "pieCakeContextAdditivePreferences", highlight1 = "Anything w/ raspberry?", highlight2 = "Anything w/ lemon?", highlight3 = "Pie?", highlight4 = "Cake?")
run_plot_model(task_name = "pieCakeContextBiasedPessimist", highlight1 = "Which pies?", highlight2= "Anything?")
run_plot_model(task_name = "pieCakeContextUnbiasedNoPref", file_name_addition = "-against-pessimist", highlight1 = "Which pies?", highlight2= "Anything?")
run_plot_model(task_name = "pieCakeContextUnbiasedNoPref", file_name_addition = "no-highlight")
run_plot_model(task_name = "pieCakeContextBiasedNoPref", highlight1 = c("Which goods?"), highlight2 = c("Raspberry pie?", "Lemon cake?"))
run_plot_model(task_name = "pieCakeContextUnbiasedNoPref", file_name_addition = "-against-opinionated", highlight1 = c("Which goods?"), highlight2 = c("Raspberry pie?", "Lemon cake?"))


run_plot_model(task_name = "R1Responses_BinaryPrefs")
run_plot_model(task_name = "R1Posterior_BinaryPrefs")

