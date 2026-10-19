# Contributing

This file is for the people who maintain the tool or contribute to it. To use the tool, see the [README](README.md).

- [Working on the code](#working-on-the-code)
- [Workflow](#workflow)
- [Conventions](#conventions)
- [Code organisation](#code-organisation)
- [Continuous integration](#continuous-integration)
- [Versions and releases](#versions-and-releases)
- [Deployment](#deployment)

## Working on the code

To draw a sample, use the production application, not a copy on a computer: a copy can contain changes that are not released, and it would still write the version of the last release in the Excel file. A copy is for developing and testing a change.

You need R 4.4 or later (the lockfile was made with R 4.6.1) and, to run the browser tests, Chrome. The project uses [`renv`](https://rstudio.github.io/renv/) to manage the R packages, so that everybody works with the same versions.

1. Clone the repository and open the project (`Probability-sampling-tool.Rproj`) in RStudio, or start R in its folder: `renv` is activated automatically.
2. Install the packages. The first time it can take a few minutes:

   ```r
   renv::restore()
   ```

3. Launch the app:

   ```r
   shiny::runApp()
   ```

4. Run the tests, from the terminal, in the folder of the project:

   ```sh
   NOT_CRAN=true Rscript -e 'shinytest2::test_app()'
   ```

   They check the calculation, the checks and the messages, the tables and the Excel file, and the behaviour of the page in a browser.

5. Format the code with [Air](https://posit-dev.github.io/air/), in the version pinned in [`ci.yml`](.github/workflows/ci.yml):

   ```sh
   air format .
   ```

## Workflow

`master` only holds released versions. Each release has a development branch, `dev<year>.<n>` (for example `dev2026.1.0`), where the work of the release is merged. Contributions follow an issue-driven workflow:

1. **Open an issue** describing the request, the bug or the task (the forms are in `.github/ISSUE_TEMPLATE`).
2. **Create a branch** linked to the issue, from the current development branch. Keep its name short, the number of the issue and two or three words (`56-ci-workflow`): Windows limits a path to 260 characters, and a long branch name can make the folder of the repository too deep. With the GitHub CLI: `gh issue develop 56 --base dev2026.1.0 --checkout --name 56-ci-workflow`.
3. **Make the change** on the branch, with its tests. A change that users can see also updates the Guide (`guide/`) and [NEWS.md](NEWS.md).
4. **Open a pull request** to the current development branch, with the template of the repository. `format` and `test` run on it.
5. **Merge** only after a review and an approval, once `format` and `test` pass.

Commit messages are short, in the imperative, with a prefix that says what the commit is: `feat:`, `fix:`, `docs:`, `test:`, `refactor:`, `ci:`.

## Conventions

- The calculation and the checks take a plain list `params` (`read_params(input)`), not the Shiny `input`.
- Each piece of information has **one source**: the names of the sampling types and of the modes, the labels of the columns and of the fields, the names of the sheets of the Excel file and the phrases that say what one row of the frame is are in `R/constants.R`; the messages of the checks are in `guide/checks.yml`. The Guide fills the `{{...}}` of its texts from them, so do not repeat them in a text.
- Names in `snake_case`, constants in `UPPER_CASE`, a roxygen block with `@noRd` on every function (a test checks it), code formatted with Air.

## Code organisation

| Path | Content |
|---|---|
| `global.R`, `ui.R`, `server.R` | the packages and options, the page (it assembles the three tabs) and the reactive flow of the app |
| `R/` | the functions, loaded by Shiny |
| `guide/` | the texts of the Guide tab (Markdown) and the checks of the app (`checks.yml`) |
| `data/`, `www/` | the example sampling frame and the style sheet |
| `tests/` | unit tests and browser tests |

The files of `R/` are grouped by their name:

- `sampling_*`: the calculation (frame, targets, draw, sample and summary), with no Shiny code, except `sampling_status.R`, the messages of the Sampling tab;
- `checks_*`: the checks of the file, the columns, the parameters, the draw and the output file;
- `ui_*` and `guide.R`: the interface and the Guide (a Shiny module);
- `draw_record.R` and `output_workbook.R`: the settings kept with each draw, and the Excel file built from them;
- `constants.R`, `params.R`, `upload.R`, `app_info.R`, `utils.R`, `results_tables.R` and `server_helpers.R`: shared values, the settings, the reading of the file and helpers.

To add a check, write its message in `guide/checks.yml`, then make a function return `check_message("its_id")`: the Guide shows it without further change.

## Continuous integration

The workflow [`ci.yml`](.github/workflows/ci.yml) runs on every pull request, and on every push to `master` and to the `dev<year>.<n>` branches:

| Job | What it checks |
|---|---|
| `format` | the code is formatted with Air (`air format . --check`) |
| `test` | the whole test suite, browser tests included, on the packages of `renv.lock` and the R version of the lockfile, on Ubuntu 24.04 |
| `tag` | only when a release is marked: see *Versions and releases* |

Because the packages come from `renv.lock`, a new package or a new version is tested on the packages that will be deployed. The version of Air and the actions are pinned (the actions to a commit), and [Dependabot](.github/dependabot.yml) proposes the updates of the actions.

## Versions and releases

**The version of the app** is `APP_VERSION` in `R/constants.R`, in calendar versioning (`YYYY.N.N`, for example `2026.1.0`). It is set to the version of the release when its development branch starts (`dev2026.1.0` carries `2026.1.0`). It is written in the `inputs` sheet of the Excel file of every draw, with the short commit when the deployment provides one (a `BUILD_INFO` file with the line `Commit: abc1234`). A sample therefore says which version of the app drew it: the same file, settings, seed and version of the app give the same sample.

**A release** is made when the development branch is merged into `master` and the merge is tagged:

1. All the issues of the release are merged in the development branch, [NEWS.md](NEWS.md) describes them (it is shown in the Guide, under *What's new*) and the CI is green.
2. The development branch is merged into `master`, by pull request.
3. The merge commit of `master` is tagged with the version of the app, `v` followed by `APP_VERSION` (`v2026.1.0`).
4. The tag is deployed to production (see *Deployment*).

When a tag is pushed, the `tag` job checks that it is the version of the app (so that the Excel files of the release say the right version) and that its commit is on `master`; otherwise the release is refused.

**After a release.** The next release starts with a new development branch, `dev<year>.<n+1>`, created from `master`, whose first change is `APP_VERSION`. A fix to a released version is a short branch from `master`, merged into `master` by pull request, with the last number of `APP_VERSION` increased (`2026.1.1`) and tagged the same way; `master` is then merged into the current development branch, so that the fix is not lost.

## Deployment

The application is deployed to shinyapps.io (account `impact-initiatives`) by the workflow [`deploy.yml`](.github/workflows/deploy.yml), started by hand.

| | dev | production |
|---|---|---|
| Application | `probability-sampling-tool-dev`, shared by link for the tests | `probability-sampling-tool` |
| Deployed from | a branch or a tag, usually `dev<year>.<n>` | a version tag only (`vYYYY.N.N`) |

**To deploy**: in the *Actions* tab, choose *Deploy*, *Run workflow*, pick the branch or the tag under *Use workflow from*, choose where to deploy and run it. The workflow runs the CI on that commit and goes on only if it passes; for production it also checks that the tag is `APP_VERSION` and that its commit is on `master`. It writes `BUILD_INFO` (the commit, shown in the Excel file), and sends the files that `.rscignore` does not leave out; the packages come from `renv.lock`. A workflow that is started by hand has to be on `master` to appear in the *Actions* tab.

**Before a release**: deploy the development branch to dev and check the application there. Then merge it into `master`, tag the merge, and deploy that tag to production.

**Settings** (*Settings → Secrets and variables → Actions*): the secrets `SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET`, the token and the secret of a token of the shinyapps.io account (*Account → Tokens*); the variables `SHINYAPPS_ACCOUNT`, `SHINYAPPS_APP_NAME_DEV` and `SHINYAPPS_APP_NAME_PROD`.

**Who can deploy.** Whoever can run a workflow in the repository, so the people with write access: GitHub Environments, which would ask for an approval, are not available on the plan of the organisation. Keep the write access to people you trust, and when someone with access leaves, or in case of doubt, create a new token on shinyapps.io and replace the two secrets. Every deployment is in the history of the *Actions* tab, with who started it.

> *N.B. shinyapps.io is moving to Posit Connect Cloud, with an automatic migration planned early 2027: this section will have to be updated then.*
