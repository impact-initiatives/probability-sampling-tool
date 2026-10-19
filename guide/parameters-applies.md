| Parameter | Mode: based on population | Mode: enter sample size | Simple random | PPS | Cluster |
|---|---|---|---|---|---|
| **{{field:confidence_level}}** | ✓ | – | ✓ | ✓ | ✓ |
| **{{field:error_margin}}** | ✓ | – | ✓ | ✓ | ✓ |
| **{{field:proportion}}** (fixed) | ✓ | – | ✓ | ✓ | ✓ |
| **{{field:buffer}}** | ✓ | – | ✓ | ✓ | ✓ |
| **{{field:target_size}}** | – | ✓ | ✓ | ✓ | ✓ |
| **{{field:seed}}** | ✓ | ✓ | ✓ | ✓ | ✓ |
| **{{field:cluster_size}}** | ✓ | ✓ | – | – | ✓ |
| **{{field:icc}}** | ✓ | – | – | – | ✓ |

✓ the parameter is used, – it is not. A parameter has to be used in **both** its mode and its type: the ICC only matters for the cluster type in the "based on population" mode, where it sets the design effect. The form hides the fields that the type or the mode do not use, except the **ICC**, which stays visible in the cluster type and has no effect with "Enter sample size".
