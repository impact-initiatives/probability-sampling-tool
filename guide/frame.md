**File format**

- A **CSV** file of at most {{MAX_UPLOAD_MB}} MB (an Excel workbook has to be saved as CSV).
- **One row per unit of the frame**, covering the whole population of interest, not a sample already drawn.
- **No column name starting with `PST_`**: the prefix is reserved for the columns the tool adds. A previous sample reloaded as a frame has them: rename or remove them.

**What each type of sampling expects**

| Your sampling frame | {{SAMPLING_TYPE_SIMPLE}} | {{SAMPLING_TYPE_PPS}} | {{SAMPLING_TYPE_CLUSTER}} |
|---|---|---|---|
| **One row is** | {{one_row:simple}} | {{one_row:pps}} | {{one_row:cluster}} |
| **Population column** | not used | numeric, $\geq 0$, no missing | numeric, $\geq 0$, no missing, $\geq$ cluster size |
| **Cluster column** | not used | not used | one unique value per row |

The **stratum column is optional** in the three types. The cluster, population and stratum columns have to be different columns.
