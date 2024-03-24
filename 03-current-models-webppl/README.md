 # Scripts to dev/test pragmatic QA model
 
 `pragmaticQAModel` is a webppl package containing the basic model functionality, as well as different contexts to be tested.
 Numbered scripts include running / testing functionality, in the sequence of development (older to newer).
 
 - `01-runPieCake-examples.webppl` contains code to run several variants of the "pie-cake" context examples
 - `02-run-preferences-inferece.webppl` provides code to run inference of continuous preferences; the code is currently dysfunctional, as it requires visualization in the browser, which requires copy-pasting large chunks of code from the `pragmaticQAModel` into the browser
 - `03-run-TSO-dev.webppl` is code to run the "target-same-other" type of example (Experiment 1) for dev-purposes from the terminal; it contains an example where the pragmatic respondent is uncertain about the context
 - `04-run-TSO-prior-predicitive*` are files to run the 'TSO' model from R so as to collect predictions for various model parameters and numerical preferences (which are supplied from the R script)
 
 
