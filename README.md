# Probability Sampling Tool

A Shiny application for calculating probability-based sampling designs and producing sample selection outputs for survey work.

## Project status

This repository is the IMPACT-maintained version of the existing sampling tool. The project history was preserved during migration into the IMPACT Initiatives GitHub organization and serves as the reference baseline for future maintenance and review.

The current production application is available here:

https://impact-initiatives.shinyapps.io/probability-sampling-tool/

## Purpose

The tool supports common sampling approaches used in field data collection, including:

- simple random sampling,
- cluster sampling,
- stratified sampling.

## Local development

This project uses `renv` to manage the R package environment and keep local setup reproducible.

1. Open the project in RStudio or your preferred R environment.
2. Restore the project dependencies:

```r
renv::restore()
```

3. Launch the Shiny app locally:

```r
shiny::runApp()
```

The project is configured to use the project-local library when opened in the project directory.

## Contribution workflow

Contributions follow a simple issue-driven workflow:

1. Create an issue describing the request, bug, or task.
2. Create a dedicated branch linked to that issue.
3. Implement the change on the branch.
4. Open a pull request for review.
5. Merge only after review and approval.

This process is intended to keep changes traceable, reviewable, and aligned with project priorities.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Maintenance note

This README reflects the project as it exists at the start of official maintenance. More detailed project documentation, standards, and technical guidance will be added as the codebase is reviewed and updated.
