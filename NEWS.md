# 2026.2.0

## Results

- **The Excel file has a fourth sheet, `sampling_frame`**: the sampling frame as the tool read it (the file that was sampled, without the columns the tool adds). With the `inputs` sheet it is enough to draw the same sample again: save the sheet as a CSV file, load it in the tool and use the same settings, seed and version of the app. The file now contains the whole sampling frame: share it with the care you give to the frame itself.

# 2026.1.0

## Names of the sampling types

| Before | Now |
|---|---|
| Simple random | **Simple random sampling** |
| Simple random - allocation | **PPS sampling with replacement** |
| Cluster sampling | **Two-stage PPS cluster sampling with replacement** |

## Cluster sampling: what changed in the method

The two-stage PPS cluster sampling is the only type whose method changed. Simple random sampling and PPS sampling with replacement use the same formulas and draws as before (only their names changed).

| Topic | Before | Now |
|---|---|---|
| **Number of draws** | The number of clusters was not decided before the draw. The tool kept adding PSUs until the number of surveys reached a target that was recomputed after each draw, from the PSUs already drawn. The total therefore changed from one run to the next, even with the same inputs. | The number of clusters is decided before the draw: the requested target divided by the cluster size $b$, rounded up. The total is the same at each run, and the same file, settings and seed give the same sample. |
| **Design effect** | $\mathrm{DEFF} = 1 + (\bar{k} \cdot b - 1)\,\mathrm{ICC}$, with $\bar{k}$ the mean number of times a PSU was drawn: a PSU drawn $k$ times counted as one cluster of $k \cdot b$. | $\mathrm{DEFF} = 1 + (b - 1)\,\mathrm{ICC}$ with the planned $b$: a PSU drawn $k$ times is $k$ independent clusters of $b$ surveys. With "Sample size based on population" it multiplies the computed size, $\lceil n \times \mathrm{DEFF} \rceil$, then the buffer is added. |
| **Fallback to simple random sampling** | When every PSU of a stratum had been drawn at least once and the target was still not reached, the tool switched the stratum to simple random sampling, with 10% more buffer. Only the `Sampling type` column of the summary said so. | Removed: the type of sampling never changes. |
| **PSUs smaller than the cluster size** | Left out of the draw without a message. | Blocked at Apply, with the list of the PSUs: lower the cluster size or remove them from the frame. |
| **Stratum that cannot reach its target** | Not checked. | Blocked at Apply, with the list: a stratum can host $\sum_i \lfloor N_i / b \rfloor$ clusters. |
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

- **Columns of the Sample**: the columns added by the tool start with `PST_` and come first.

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

- **Requested Target**: the target columns show what was asked for, which is not always what is drawn. In simple random and PPS sampling a stratum cannot give more than its population: when the request is larger, the draw stops at the population and a warning says so, after Apply and after Sample. The *Surveys* column of the summary is what was drawn.
- Changing a parameter, like a new Apply, **hides the previous sample** and its downloads until Apply and then Sample are clicked again. The Sampling tab says why Sample, the tables and the downloads are not available.

## Checks

The tool now checks the sampling frame and the settings before the draw. Every check is listed in [What the tool checks](#guide:data/checks), in the order they run: the file, then the columns, then the parameters. The first problem is reported with what to do, and when it concerns the data, a list of the rows to fix can be downloaded.

## Parameters

- The **proportion** is fixed at 0.5: the field is shown but not editable.
- **New: a seed field.** The same file, settings and seed give the same sample, so a draw can be reproduced and checked. The seed is in the Sample summary and in the Excel file.
- With "Enter sample size" and the cluster type, the summary now shows the ICC, the design effect and the effective sample size, as information: the number of surveys is the one you entered.
- The **buffer** is a share of the target between 0 and 2 (200%): a larger value is reported before the draw.

## Interface

- The Sampling frame and Sampling tabs are redesigned: the setup is in a side panel that can be folded to give the tables the whole width.
- The Introduction page is replaced by this Guide.
- The four tables have a filter on each column and shorter numbers (display only: the Excel file keeps every digit).
