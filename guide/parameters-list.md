| Parameter | Meaning | Value |
|---|---|---|
| **Confidence level** | how often the interval around an estimate contains the true value (0.95 = 95%). A higher level needs a larger sample | 0.95 by default; strictly between 0 and 1 |
| **Error margin** | the largest gap accepted between the estimate and the true value (0.05 = ±5 percentage points). A smaller margin needs a larger sample | 0.05 by default; strictly between 0 and 1 |
| **Proportion** | **fixed at 0.5**, the most cautious value: it gives the largest sample size. The field is shown for information and cannot be edited | 0.5 |
| **Buffer** | share added to the computed sample size to cover non-response (0.05 = 5%) | 0.05 by default; 0 or more |
| **Target** | the number of surveys wanted for each stratum | whole number above 0 |
| **Seed** | starting point of the random draw. The same seed and settings give the same sample, so a draw can be **reproduced and checked later**; it is recorded in the Sample summary. Change it for another sample | a random number at each session; whole number |
| **Cluster size** | surveys taken in each cluster, noted $b$ in the formulas | 5 by default; whole number, 2 or more |
| **ICC** | intracluster correlation: how alike the units of a cluster are (0 = not at all, 1 = identical). Take it from a similar past survey | 0.06 by default; between 0 and 1 |
