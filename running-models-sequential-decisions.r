library(rwebppl)
library(tidyverse)
library(aida) # remotes::install_github("michael-franke/aida-package")


run_plot_model <- function (context_name = "pieCakeContext", questionerUtilFct = "KL") {
  webPPL_data = tibble('context' = context_name, 'questionerUtilFct' = questionerUtilFct)
  webppl(
    program_file = "qa-models-sequential-decisions.webppl",
    data = webPPL_data,
    data_var = "myDF"
  ) -> output

  output %>%
    as_tibble() %>%
    mutate(support = fct_reorder(support, prob)) %>%
    ggplot(aes(x = support, y = prob)) +
    geom_col(fill = "#505B55") + coord_flip() +
    xlab("") +
    ylab("") +
    theme_aida()

  factor = 3

  ggsave(filename = str_c("pics/results-", context_name, ".pdf"), width = 16/factor, height = 9/factor)

}

run_plot_model("pieCakeContextMinimal")
run_plot_model("pieCakeContextMinimalWithPreferences")
run_plot_model("pieCakeContext")
run_plot_model("pieCakeContextBiasedNoPref")
run_plot_model("pieCakeContextUnbiasedNoPref")
