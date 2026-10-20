**Two-stage cluster sampling.** The tool draws the PSUs (first stage) **with probability proportional to their population and with replacement**, and sets a **fixed number of surveys per draw**, the cluster size. The households of a cluster are then selected inside the PSU at data collection.

| | |
|---|---|
| **One row of your CSV is** | {{one_row_detail:cluster}} |
| **Required columns** | a **cluster** column (one unique value per row) and a **population** column |
| **Draw** | PSUs proportional to the population, with replacement |
| **Surveys per draw** | the **cluster size, noted $b$** (at least 2), the same for every draw |
| **Design effect** | applied to the computed sample size (with "Enter sample size" it is only shown in the summary): $\mathrm{DEFF} = 1 + (b - 1)\,\mathrm{ICC}$, where $b$ is the **cluster size** (surveys per cluster) and ICC the intracluster correlation: how alike the units of a cluster are (0 = not at all, 1 = identical) |
| **Number of clusters** | the target divided by the cluster size $b$, rounded up |

**Reading the results.** A **cluster** is one draw: a PSU and the surveys taken in it. A PSU drawn several times gives several independent clusters.

| Example, cluster size 5 | Times drawn | Clusters | Surveys |
|---|---|---|---|
| PSU A | 3 | 3 | $3 \times 5 = 15$ |
| PSU B | 1 | 1 | $1 \times 5 = 5$ |

*{{label:clusters_drawn}}* counts the draws (here 4), *{{label:distinct_psus}}* the different PSUs, that is the sites to visit (here 2).

**Good to know.**

- The tool does **not** select the households. They have to be selected at random, with equal probability, inside each PSU for the sample to be self-weighting.
- Every PSU needs a population of at least the cluster size $b$, and a PSU can host $\lfloor N_i / b \rfloor$ whole clusters. The tool blocks **Apply** when a PSU is smaller than the cluster size or a stratum does not have room for all its clusters, and blocks **Sample** when a PSU is drawn more often than it can host (see [What the tool checks](#guide:data/checks), in [Your sampling frame](#guide:data)). It never switches to another type of sampling.
