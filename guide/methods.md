This part is for readers who want the statistical detail. $N$ is the population of the stratum (its number of rows in simple random sampling, the sum of the population column otherwise) and $N_i$ the population of the PSU $i$.

## Sample size (mode "based on population")

For a proportion, per stratum, with the finite population correction (Cochran, 1977):

$$n_0 = \frac{z^2\,p\,(1-p)}{e^2}, \qquad n = \frac{n_0}{1 + \dfrac{n_0 - 1}{N}}$$

where $z$ is the two-sided normal quantile of the confidence level, $e$ the error margin and $p$ the expected proportion.

**Proportion.** $p$ is fixed at 0.5: it maximises $p(1-p)$, so it gives the largest sample, the most cautious choice, used for every indicator. The formula is exact for a proportion; for means it is an approximation. See the [discussion #9](https://github.com/impact-initiatives/probability-sampling-tool/issues/9).

**Target.** The size is multiplied by the design effect (1 outside the cluster type, see below) and rounded up. This is the *Requested Target*:

$$n_{\text{target}} = \lceil n \times \mathrm{DEFF} \rceil$$

**Buffer.** The target is then increased to cover non-response. This is the *Requested Target (with buffer)*, the number the draw aims at:

$$n_{\text{requested}} = \lceil n_{\text{target}} \times (1 + \text{buffer}) \rceil$$

## Design effect

Clustering makes the surveys of a cluster alike, so a clustered sample is worth less than a simple random sample of the same size. The **design effect** measures this loss (Kish, 1965):

$$\mathrm{DEFF} = 1 + (b - 1)\,\mathrm{ICC}$$

where $b$ is the **cluster size** (surveys per cluster) and ICC the intracluster correlation: 0 if the units of a cluster are not alike, 1 if they are identical. In the two other types $\mathrm{DEFF} = 1$.

The **effective sample size** shown in the summary is $\text{Surveys} / \mathrm{DEFF}$.

The ICC is the value you enter, not a measure on the draw. $b$ and the DEFF are set by the design: a PSU drawn $k$ times is $k$ independent clusters of $b$ surveys, so they do not depend on the PSUs drawn.

## Draws

At each draw, the PSU $i$ is drawn with probability $p_i = N_i / N$, its share of the population of the stratum. Draws are independent and **with replacement**: a PSU can be drawn again.

| Type | Number of draws | Probability at each draw |
|---|---|---|
| Simple random sampling | $\min(n_{\text{requested}}, N)$ units, **without replacement** | the same for every unit not yet drawn |
| PPS sampling with replacement | $\min(n_{\text{requested}}, N)$ independent draws, **with replacement**; the surveys of a PSU are the number of times it is drawn | $p_i = N_i / N$ |
| Two-stage PPS cluster sampling with replacement | $m = \lceil n_{\text{requested}} / b \rceil$ independent draws, **with replacement**; each draw is a cluster of $b$ surveys | $p_i = N_i / N$ |

In the cluster type the number of surveys is $m \times b$, which can be a little above $n_{\text{requested}}$ because clusters are whole. If the households are selected at random with equal probability in each PSU, every household has the same probability of inclusion.

**Limits, counted in clusters.** A PSU can host $\lfloor N_i / b \rfloor$ whole clusters, so:

- a PSU cannot be drawn more than $\lfloor N_i / b \rfloor$ times;
- a stratum can host $\sum_i \lfloor N_i / b \rfloor$ clusters, and the number of draws $m$ cannot exceed it.

## References

- Cochran, W. G. (1977), *Sampling Techniques*, 3rd ed., Wiley.
- Kish, L. (1965), *Survey Sampling*, Wiley.
