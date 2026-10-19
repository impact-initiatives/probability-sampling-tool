# Probability Sampling Tool

[![CI](https://github.com/impact-initiatives/probability-sampling-tool/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/impact-initiatives/probability-sampling-tool/actions/workflows/ci.yml)

A Shiny application that draws a probability sample from a sampling frame for survey work, and tells the field teams how many surveys to run where. It is maintained by IMPACT Initiatives.

**Use it at https://impact-initiatives.shinyapps.io/probability-sampling-tool/**

## What it does

You load your sampling frame (one CSV file), choose a sampling type (simple random sampling, PPS sampling with replacement, or two-stage PPS cluster sampling with replacement) and its parameters, and apply: the tool checks the file and the settings and says what to fix. It then draws the sample and gives you **one Excel file** with the sample, its summary and the inputs of the draw (version of the app, file, settings and seed), so that the draw can be reproduced and checked.

The **Guide** tab of the application explains the sampling types, what the file must contain, the parameters, the checks, the results and the formulas. What changed in each version is in the Guide under *What's new*, and in [NEWS.md](NEWS.md).

## Your data

The file you load is read during your session to draw the sample. The tool does not save it, and does not send it anywhere else: it is processed by the application, which runs on shinyapps.io (Posit). Follow the data protection rules of your organisation, and do not load personal data: a sampling frame lists areas, camps or population groups, not individuals.

The file must be a CSV of at most 100 MB, with one row per unit of the frame (see the Guide, *Your data*). An example is in [data/data_example.csv](data/data_example.csv) and in the application (*Use test data*).

## Help and feedback

To report a problem, suggest a change or ask a question, [open an issue](https://github.com/impact-initiatives/probability-sampling-tool/issues/new/choose): there is a form for each case.

## Maintaining the tool

To draw a sample, use the production application above, not a copy on a computer: it runs a released version, on the packages that were tested. How to work on the code, the workflow of the contributions, the continuous integration and the releases are in [CONTRIBUTING.md](CONTRIBUTING.md).

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
