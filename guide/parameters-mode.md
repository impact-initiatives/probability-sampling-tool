| Mode | What you enter | Sample size |
|---|---|---|
| **{{SAMPLING_MODE_FROM_POPULATION}}** | confidence level, error margin, buffer (the proportion is fixed) | computed for each stratum from its population, see *Methods* |
| **{{SAMPLING_MODE_ENTER_SIZE}}** | the target | the number you enter, **applied to each stratum**; no buffer, and the design effect does not change the size (it is only shown in the summary for the cluster type) |

With **{{STRATIFIED}}**, every stratum gets its own target. In the mode "{{SAMPLING_MODE_FROM_POPULATION}}" it depends on the population of the stratum; in "{{SAMPLING_MODE_ENTER_SIZE}}" it is the same number for every stratum.

Changing a parameter does not erase the tables, but **Sample** is disabled until the next Apply, and a new Apply hides the previous sample.
