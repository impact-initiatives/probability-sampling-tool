| Parameter | Mode: based on population | Mode: enter sample size | Simple random | PPS | Cluster |
|---|---|---|---|---|---|
| **Confidence level** | ✓ | – | ✓ | ✓ | ✓ |
| **Error margin** | ✓ | – | ✓ | ✓ | ✓ |
| **Proportion** (fixed) | ✓ | – | ✓ | ✓ | ✓ |
| **Buffer** | ✓ | – | ✓ | ✓ | ✓ |
| **Target** | – | ✓ | ✓ | ✓ | ✓ |
| **Seed** | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Cluster size** | ✓ | ✓ | – | – | ✓ |
| **ICC** | ✓ | ✓ | – | – | ✓ |

✓ the parameter is used, – it is not. A parameter has to be used in **both** its mode and its type: the ICC only matters for the cluster type, where it sets the design effect. With "based on population" the design effect enlarges the computed sample size; with "Enter sample size" the size is the number you enter, and the design effect is only shown in the summary, with the effective sample size. The form hides the fields that the type or the mode do not use.
