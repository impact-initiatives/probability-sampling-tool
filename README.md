# Probability Sampling Tool

A Shiny application that draws a probability sample from a sampling frame for survey work, and tells the field teams how many surveys to run where. It is maintained by IMPACT Initiatives.

The production application is at https://impact-initiatives.shinyapps.io/probability-sampling-tool/. Its **Guide** tab explains the sampling types, the parameters, the checks and the results; the changes of each version are in [NEWS.md](NEWS.md).

## What it does

The tool draws a probability sample from your sampling frame: it returns the units to survey and the number of surveys to run in each. The **Guide** tab of the app explains everything (Start, Sampling types, Your data, Parameters, Results, Methods); this is a short summary.

### How it works

1. **Prepare the frame.** One CSV file, up to 100 MB (`MAX_UPLOAD_MB` in [global.R](global.R)), with one row per unit of your frame: the whole population of interest, not a sample already drawn. An example is in [data/data_example.csv](data/data_example.csv), and in the app (*Use test data*). *Guide: Your data.*
2. **Set up**, in the *Sampling frame* tab: the mode, the type of sampling, the columns and the parameters. *Guide: Parameters.*
3. **Apply.** The tool computes the target of each stratum and checks the file, the columns and the parameters; if something is wrong, it says what to fix.
4. **Sample and export**, in the *Sampling* tab: the tool draws the sample, and you download one Excel file with the inputs of the draw, the *Sample summary* and the *Sample*. *Guide: Results.*

### Three sampling types

| | Simple random sampling | PPS sampling with replacement | Two-stage PPS cluster sampling with replacement |
|---|---|---|---|
| **One row is** | a unit to survey (household, person) | a PSU (an area) with its population | a PSU with its population |
| **Needs** | no population column | a population column | a cluster column and a population column |

A strata column is optional in the three types. *Guide: Sampling types.*

### Size of the sample

- **Sample size based on population:** computed for each stratum from the confidence level, the error margin and a buffer for non-response (the proportion is fixed at 0.5); the cluster type also takes the cluster size and the ICC into account, through the design effect.
- **Enter sample size:** the number you type, applied to each stratum.

The formulas are in the Guide, under *Methods*. The inputs of the draw (version of the app, file, settings and seed) are in the `inputs` sheet of the Excel file, and the seed is in the *Sample summary*, so a draw can be reproduced and checked later.

## Local development

You need R 4.4 or later (the lockfile was made with R 4.6.1) and, to run the browser tests, Chrome. The project uses `renv` to manage the R packages, so that every developer works with the same versions.

1. Clone the repository and open the project (`Probability-sampling-tool.Rproj`) in RStudio, or start R in its folder: `renv` is activated automatically.
2. Install the packages. The first time it can take a few minutes:

```r
renv::restore()
```

3. Launch the app:

```r
shiny::runApp()
```

4. Run the tests from the terminal, in the folder of the project:

```sh
NOT_CRAN=true Rscript -e 'shinytest2::test_app()'
```

The tests check the calculation, the checks and the messages, the tables and the downloaded files, and the behaviour of the page in a browser.

## Code organisation

| Path | Content |
|---|---|
| `global.R`, `ui.R`, `server.R` | the packages and options, the page (it assembles the three tabs) and the reactive flow of the app |
| `R/` | the functions, loaded by Shiny |
| `guide/` | the texts of the Guide tab (Markdown) and the checks of the app (`checks.yml`) |
| `data/`, `www/` | the example sampling frame and the style sheet |
| `tests/` | unit tests and browser tests |

The files of `R/` are grouped by their prefix:

- `sampling_*`: the calculation (frame, targets, draw, sample and summary), with no Shiny code;
- `checks_*`: the checks of the file, the columns, the parameters and the draw;
- `ui_*` and `guide.R`: the interface and the guide (a Shiny module);
- `results_tables.R`, `sampling_status.R` and `server_helpers.R`: helpers of the server, and the messages of the Sampling tab;
- `draw_record.R` and `output_workbook.R`: the settings kept with each draw, and the Excel file built from them;
- `constants.R`, `params.R`, `upload.R`, `app_info.R` and `utils.R`: shared values (the version of the app included), the settings, the reading of the file and small helpers.

Conventions:

- The calculation and the checks take a plain list `params` (`read_params(input)`), not the Shiny `input`.
- Each piece of information has one source (this README repeats the names of the sampling types and the modes: update it when they change): the names of the sampling types, the modes and the labels of the columns and of the fields are in `R/constants.R`, the messages of the checks in `guide/checks.yml`. The guide fills the `{{...}}` of its texts from them.
- The version of the app is `APP_VERSION` in `R/constants.R`: a release tag `vX.Y.Z` must be equal to it. A `BUILD_INFO` file (`Commit: abc1234`), written by the deployment, adds the commit to the version shown in the Excel file.
- Names in `snake_case`, constants in `UPPER_CASE`, a roxygen block with `@noRd` on every function, code formatted with [Air](https://posit-dev.github.io/air/).

To add a check, write its message in `guide/checks.yml`, then make a function return `check_message("its_id")`: the guide shows it without further change.

## Contribution workflow

Contributions follow a simple issue-driven workflow:

1. Create an issue describing the request, bug, or task (the issue templates are in `.github/`).
2. Create a dedicated branch linked to that issue.
3. Implement the change on the branch.
4. Open a pull request to the current development branch (`dev<year>.<n>`); the checks run on it.
5. Merge only after review and approval.

This process is intended to keep changes traceable, reviewable, and aligned with project priorities.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
