| Mode | What you enter | Sample size |
|---|---|---|
| **{{SAMPLING_MODE_FROM_POPULATION}}** | confidence level, error margin, buffer (the proportion is fixed) | computed for each stratum from its population, see *Methods* |
| **{{SAMPLING_MODE_ENTER_SIZE}}** | the target | the number you enter, **applied to each stratum**; no buffer and no design effect |

With **{{field:stratified}}**, every stratum gets its own target. In the mode "{{SAMPLING_MODE_FROM_POPULATION}}" it depends on the population of the stratum; in "{{SAMPLING_MODE_ENTER_SIZE}}" it is the same number for every stratum.

Changing a parameter disables **Sample** and hides the sample on screen and its downloads until the next Apply; the Sampling tab says so. The target and frame tables of the Sampling frame tab keep showing what was last applied.
