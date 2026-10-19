Draws **PSUs** one survey at a time, **with probability proportional to their population and with replacement**: a large PSU can be drawn several times and then gets several surveys. Use it when your frame lists areas (regions, camps, settlements) or population groups (host, IDP, returnee) with their population, not the individuals.

| | |
|---|---|
| **One row of your CSV is** | {{one_row_detail:pps}} |
| **Required columns** | a **population** column (numeric, no missing or negative value) |
| **Draw** | probability proportional to the population, with replacement |
| **Surveys per draw** | 1; the surveys of a PSU are the number of times it is drawn |
| **Design effect** | none (DEFF = 1) |
| **The sample size cannot exceed** | the total population of the stratum (sum of the population column) |

**What you get.** One row per PSU drawn, with `PST_survey` = the number of surveys to run there. The households themselves are chosen by the teams inside each PSU.

**Good to know.**

- The number of surveys of a PSU is **random**: it is proportional to its population only on average. With another seed a PSU can get more or fewer surveys; with the same seed and settings the result is the same.
- This is the first stage of the cluster type below with a cluster size of 1. It is kept apart because the frame and the use are different, and no design effect applies.
