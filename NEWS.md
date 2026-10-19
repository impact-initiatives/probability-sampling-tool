## Names of the sampling types

The three types are named after what they do (the word *allocation* is not used any more).

| Before | Now |
|---|---|
| Simple random | **Simple random sampling** |
| Simple random - allocation | **PPS sampling with replacement** |
| Cluster sampling | **Two-stage PPS cluster sampling with replacement** |

The `Sampling type` column of the summary shows the new names.

## Results

- **New column names** in the Sample summary:

  | Before | Now |
  |---|---|
  | Stratification | **Stratum** |
  | `# surveys` | **Surveys** |
  | `# PSUs to assess` | **Clusters drawn** |
  | `# Unique PSUs` | **Distinct PSUs** |
  | `Effective sample size (SRS-equivalent)` | **Effective sample size** |
  | `% buffer` | **Buffer** |
  | `Cluster size set (planned)`, `Mean Cluster size (realized)` | **Cluster size** |
  | `DEFF (planned)`, `DEFF (realized)` | **DEFF** |

- **Columns added by the tool** start with `PST_` and come first: `PST_id_sample`, `PST_survey`, `PST_stratum_id`, `PST_psu_id`, `PST_pop_size`, `PST_pop_size_stratum`, `PST_prob`. A frame with a column named like one of them was silently altered or made the draw fail: it is now blocked, with a message.
- **Target columns** are labelled *Requested Target*: the draw is capped to the population in the random types, and a warning is shown after Apply and after Sample.
- Changing a parameter, like a new Apply, **hides the previous sample** and its downloads until Apply and then Sample are clicked again. The Sampling tab says why Sample, the tables and the downloads are not available.

## Checks

- **No more silent switch to simple random sampling** in cluster sampling. A PSU smaller than the cluster size, a stratum that cannot reach its target, or a PSU drawn more often than its population allows, now stop the run with a clear message and a list to download.
- Missing or negative populations, the same column used twice, a file that is not a CSV, is empty or too large, a wrong seed or wrong parameters are reported before the draw.

## Parameters

- The **proportion** is fixed at 0.5: the field is shown but not editable.
- A **seed** makes the draw reproducible: the same seed and settings give the same sample.
- With "Enter sample size", the number applies to each stratum, and no buffer is added. For the cluster type the summary still shows the ICC, the design effect and the effective sample size.
