# Bootstrap samples exploration
[This Shiny app](https://martes.shinyapps.io/bootstrap_samples/) lets you play with bootstrap samples and compare them to regular samples and the original population.

## How it works
You select the population parameters and the sample size.

The first sample is bootstraped _n_ times to create the bootstrap sample.

Additionally, another _n-1_ regular samples are generated and merged with the first sample to create a regular sample as large as the bootstrap sample.

You can compare both the bootstraped sample and the large regular sample with visualizations.
