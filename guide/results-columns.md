They start with `PST_` (Probability Sampling Tool) and come first, in this order. `PST_stratum_id` is also the first column of the Target sampling table.

| Column | Meaning | Sampling frame | Sample |
|---|---|---|---|
| `PST_id_sample` | identifier of the row (`id_` and its position in your file) | yes | yes |
| `PST_survey` | surveys to run in this unit | – | yes |
| `PST_stratum_id` | the stratum of the unit (`all` if not stratified) | yes | yes |
| `PST_psu_id` | the PSU (the row itself in simple random sampling) | yes | yes |
| `PST_pop_size` | population of the unit (1 in simple random sampling) | yes | yes |
| `PST_pop_size_stratum` | population of the stratum | yes | yes |
| `PST_prob` | probability of drawing the unit at each draw: its share of the stratum population | yes | yes |
