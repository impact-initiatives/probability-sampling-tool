## Names of the sampling types

The three types are named after what they do (the word *allocation* is not used any more).

| Before | Now |
|---|---|
| Simple random | **Simple random sampling** |
| Simple random - allocation | **PPS sampling with replacement** |
| Cluster sampling | **Two-stage PPS cluster sampling with replacement** |

## Cluster sampling: what changed in the method

The two-stage PPS cluster sampling is the only type whose method changed. Simple random sampling and PPS sampling with replacement use the same formulas and draws as before (only their names changed).

| Topic | Before | Now |
|---|---|---|
| **Number of draws** | PSUs were added one by one until a target, recomputed at each draw from the draws already made, was reached. The number of draws was random: the same inputs could give different totals. | Fixed before the draw: $m = \lceil \text{requested target} / b \rceil$, with $b$ the cluster size. The same file, settings and seed give the same sample. |
| **Design effect** | $\mathrm{DEFF} = 1 + (\bar{k} \cdot b - 1)\,\mathrm{ICC}$, with $\bar{k}$ the mean number of times a PSU was drawn: a PSU drawn $k$ times counted as one cluster of $k \cdot b$. | $\mathrm{DEFF} = 1 + (b - 1)\,\mathrm{ICC}$ with the planned $b$: a PSU drawn $k$ times is $k$ independent clusters of $b$ surveys. With "Sample size based on population" it multiplies the computed size, $\lceil n \times \mathrm{DEFF} \rceil$, then the buffer is added. |
| **Cluster size and DEFF in the summary** | A "planned" and a "realized" value, equal except after a fallback to simple random sampling. | One value of each, and the effective sample size $= \text{Surveys} / \mathrm{DEFF}$. |
| **Fallback to simple random sampling** | When every PSU had been used, the stratum silently switched to simple random sampling, with 10% more buffer. | Removed: the problem is reported before the draw, as below. |
| **PSUs smaller than the cluster size** | Left out of the draw without a message. | Blocked at Apply, with the list of the PSUs: lower the cluster size or remove them from the frame. |
| **Stratum that cannot reach its target** | Fallback to simple random sampling. | Blocked at Apply, with the list: a stratum can host $\sum_i \lfloor N_i / b \rfloor$ clusters. |
| **PSU drawn too many times** | Not checked: the surveys of a PSU ($k \times b$) could exceed its population. | Blocked at Sample, with the list: a PSU can be drawn at most $\lfloor N_i / b \rfloor$ times. |
| **Number of surveys** | – | $m \times b$: it can be a little above the requested target, because clusters are whole. |
| **"Enter sample size"** | The number entered was not inflated (no buffer, no ICC); the ICC, the design effect and the effective sample size were blank. | The same number is drawn, per stratum; the design effect and the effective sample size are shown, as information. |

The formulas are in [Methods](#guide:methods) and the checks in [What the tool checks](#guide:data/checks).

## Results

- **One Excel file replaces the two CSV downloads**: `PST_Sampling_Output_<date>-<time>.xlsx` (time of the draw, in UTC), with three sheets, `inputs`, `sample_summary` and `sample`. The `inputs` sheet gives what is needed to reproduce the sample: the version of the app and of R, the date and time of the draw, the file, the settings of the draw and the seed. A sample too large for an Excel sheet is blocked with a message.
- **Column names of the Sample summary**:

  | Before | Now |
  |---|---|
  | `Stratification` | **Stratum** |
  | `# surveys` | **Surveys** |
  | `# units to assess` | **Clusters drawn** |
  | `Mean Cluster size`, `Cluster size set` | **Cluster size** |
  | `DESS` | **DEFF** |
  | `Effective sample` | **Effective sample size** |
  | `% buffer` | **Buffer** |

  Three columns are new: **Distinct PSUs**, **Requested Target (with buffer)** and **Seed**.

- **Columns of the Sample**: the columns added by the tool start with `PST_` and come first, and the unnamed column of row numbers of the CSV file is gone.

  | Before | Now |
  |---|---|
  | `id_sampl` | **PST_id_sample** |
  | `Survey` | **PST_survey** |
  | `strata_id` | **PST_stratum_id** |
  | `psu_id` | **PST_psu_id** |
  | `pop_numbers` | **PST_pop_size** |
  | `SumDist` | **PST_pop_size_stratum** |
  | `proba` | **PST_prob** |

  A frame with a column named like one of them was silently altered or made the draw fail: it is now blocked, with a message.

- **The Sample has one row per PSU**, with the total number of surveys to run there (`PST_survey`). Before, a PSU drawn several times appeared on several rows.
- **Requested Target**: the target columns show what was asked for, which is not always what is drawn. In simple random and PPS sampling a stratum cannot give more than its population: when the request is larger, the draw stops at the population and a warning says so, after Apply and after Sample. The *Surveys* column of the summary is what was drawn.
- Changing a parameter, like a new Apply, **hides the previous sample** and its downloads until Apply and then Sample are clicked again. The Sampling tab says why Sample, the tables and the downloads are not available.

## Checks

The tool now checks the sampling frame and the settings before the draw. Every check is listed in [What the tool checks](#guide:data/checks), in the order they run: the file, then the columns, then the parameters. The first problem is reported with what to do, and when it concerns the data, a list of the rows to fix can be downloaded.

## Parameters

- The **proportion** is fixed at 0.5: the field is shown but not editable.
- A **seed** makes the draw reproducible: the same seed and settings give the same sample.
- With "Enter sample size", the number applies to each stratum, and no buffer is added. For the cluster type the summary still shows the ICC, the design effect and the effective sample size.
- The **buffer** is a share of the target between 0 and 2 (200%): a larger value is reported before the draw.

## Interface

- The Sampling frame tab groups the setup in one panel (type of sampling, parameters, data source and columns), and the Sampling tab holds the draw and the export. The panels can be folded to give the tables the whole width.
- The Introduction page is replaced by this Guide.
- The four tables have a filter on each column and shorter numbers (display only: the Excel file keeps every digit).
- The *New seed* and *Refresh* buttons are gone: the seed is a field you edit.
