| Column | Meaning | Shown for |
|---|---|---|
| **{{label:PST_stratum_id}}**, **{{label:population}}**, **{{label:surveys}}** | the stratum, its population, the total number of surveys drawn | all |
| **{{label:target_with_buffer}}** | what was asked: the draw can be capped to the population, or rounded up to whole clusters | all |
| **{{label:clusters_drawn}}** | number of draws, repeats included | cluster type |
| **{{label:distinct_psus}}** | different PSUs among the draws: the sites to visit | PPS and cluster types |
| **{{label:cluster_size}}** | surveys per cluster, noted $b$ | cluster type |
| **{{label:deff}}** | design effect $1 + (b-1)\,\mathrm{ICC}$, from the ICC you entered, not measured | cluster type |
| **{{label:effective_sample_size}}** | $\text{Surveys} / \mathrm{DEFF}$: the size of the simple random sample with the same precision | cluster type |
| **{{label:icc}}** | the intracluster correlation you entered | cluster type |
| **{{label:buffer}}**, **{{label:confidence_level}}**, **{{label:error_margin}}** | the parameters used | mode "{{SAMPLING_MODE_FROM_POPULATION}}" |
| **{{label:seed}}**, **{{label:sampling_type}}** | the seed and the type of the draw | all |

With "{{SAMPLING_MODE_ENTER_SIZE}}" the size is the number you entered, so the parameters of the computation (buffer, confidence level, error margin) are not shown. The design effect and the effective sample size still are for the cluster type: they tell what the surveys are worth.
