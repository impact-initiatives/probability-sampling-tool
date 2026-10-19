Draws units **one by one with the same probability, without replacement**: a unit is drawn at most once. Use it when your CSV lists the units you will actually survey.

| | |
|---|---|
| **One row of your CSV is** | {{one_row_detail:simple}} |
| **Required columns** | none (an optional stratification column) |
| **Draw** | equal probability, without replacement |
| **Surveys per unit drawn** | 1 |
| **Design effect** | none (DEFF = 1) |
| **The sample size cannot exceed** | the number of rows of the stratum (the whole frame if not stratified) |

**What you get.** The list of the units drawn, each with `PST_survey = 1`. The frame has to cover the whole population of interest, not a sample already drawn.

**Good to know.**

- There is no population column: the population of a stratum is its number of rows.
- If you ask for more units than the stratum has, the draw is capped to the stratum and a warning tells you which strata.
