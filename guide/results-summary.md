| Column | Meaning | Shown for |
|---|---|---|
| **Stratum**, **Population**, **Surveys** | the stratum, its population, the total number of surveys drawn | all |
| **Requested Target (with buffer)** | what was asked: the draw can be capped to the population, or rounded up to whole clusters | all |
| **Clusters drawn** | number of draws, repeats included | cluster type |
| **Distinct PSUs** | different PSUs among the draws: the sites to visit | PPS and cluster types |
| **Cluster size** | surveys per cluster, noted $b$ | cluster type |
| **DEFF** | design effect $1 + (b-1)\,\mathrm{ICC}$, from the ICC you entered, not measured | cluster type, mode "based on population" |
| **Effective sample size** | $\text{Surveys} / \mathrm{DEFF}$: the size of the simple random sample with the same precision | cluster type, mode "based on population" |
| **ICC** | the intracluster correlation you entered | cluster type, mode "based on population" |
| **Buffer**, **Confidence level**, **Error margin** | the parameters used | mode "based on population" |
| **Seed**, **Sampling type** | the seed and the type of the draw | all |

With "Enter sample size" the size is the number you entered, so the columns that come from the computation (design effect, effective sample size, ICC, buffer, confidence level, error margin) are not shown.
