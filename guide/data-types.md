| | {{SAMPLING_TYPE_SIMPLE}} | {{SAMPLING_TYPE_PPS}} | {{SAMPLING_TYPE_CLUSTER}} |
|---|---|---|---|
| **One row is** | a unit to survey | a PSU (an area) with its population | a PSU with its population |
| **Population column** | not used | numeric, $\geq 0$, no missing | numeric, $\geq 0$, no missing, $\geq$ cluster size |
| **Cluster column** | not used | not used | one unique value per row |

The **strata column is optional** in the three types. The cluster, population and strata columns have to be different columns.
