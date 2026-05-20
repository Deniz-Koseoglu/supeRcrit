## Design of Experiments (DOE) — Design Generation

This module generates experimental designs for systematic investigation of process parameters. The generated design matrix specifies the experimental conditions for each run, which can then be executed in the laboratory and analyzed in the DOE Analysis module.

### Available Design Types

**Box-Behnken Design (BBD)** is a response surface design that requires 3–4 factors. It uses three levels per factor and does not include corner points, making it efficient when extreme factor combinations should be avoided.

**Central Composite Design (CCD)** is a response surface design supporting 2–4 factors. Two variants are available: Circumscribed (CCC) extends star points beyond the factor range (±1.414, creating 5 levels), while Face-Centered (CCF) places star points at the factor faces (±1.0, creating 3 levels).

**Full Factorial Design (FFD)** explores all possible combinations of factor levels. Supports 2–5 factors at 2 levels or 2–3 factors at 3 levels. Three-level designs include 3 center points by default.

**Fractional Factorial Design (FrFD)** is a subset of the full factorial that requires fewer runs while maintaining the ability to estimate main effects. Supports 3–5 factors with configurable fractionation and aliasing.

**Taguchi Method (TM)** uses orthogonal arrays for robust parameter design with 3–5 factors at 2–4 levels. Focuses on minimizing variation rather than optimizing the mean response.

### Center Points

Additional center points can be added to any design to help detect curvature (non-linear effects) and estimate pure experimental error. These are runs performed at the midpoint of all factor ranges.

### References

**NIST/SEMATECH** (2012), *e-Handbook of Statistical Methods*. [DOI](https://doi.org/10.18434/M32189)

**Weese, M.L., Ramsey, P.J., Montgomery, D.C.** (2025), 'Response Surface Methodology: Past, Present, and Future Perspectives', *Applied System Innovation* **8** (4), article 99. [DOI](https://doi.org/10.3390/asi8040099)
