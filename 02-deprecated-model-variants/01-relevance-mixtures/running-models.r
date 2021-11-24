library(rwebppl)
library(tidyverse)
library(aida)


run_plot_model <- function (context_name = "pieCakeContext", questionerUtilFct = "KL") {
  webPPL_data = tibble('context' = context_name, 'questionerUtilFct' = questionerUtilFct)
  webppl(
    program_file = "qa-models.webppl",
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

  ggsave(filename = str_c("results-", context_name, ".pdf"), width = 16/factor, height = 9/factor)

}

run_plot_model("neutralContext")
run_plot_model("pieCakeContext")
run_plot_model("pieCakeContextBiasedNoPref")
run_plot_model("pieCakeContextUnbiasedNoPref")
