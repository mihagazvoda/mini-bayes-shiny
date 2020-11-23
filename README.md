# mini-shiny-bayes

The goal of this project is to intuitively present Bayesian updating for Bernoulli trials (trials with 2 outcomes - success or failure.). See dashboard [here](https://mihagazvoda.shinyapps.io/bayesian-updating/).

The dashboard enables you to input outcomes of trials and then observe posteriors for a probability of success. It allows you to use different prior distributions: uniform, step, and normal.

## Usage

Clone the project. Run `make app` in your terminal (inside project folder). Bam!

## Further reading

This project was inspired by an earth tossing example (chapter 2) from [Statistical Rethinking](https://xcelab.net/rm/statistical-rethinking/) by Richard McElreath. I find the book to be a great resource to dive into Bayesian statistics. 

## Possible improvements

* Add plot with 95% highest posterior density (HPDI) and percentile (PI) intervals over trials.
* Add prediction plot based on all data.
* Refactor code into Shiny modules.


