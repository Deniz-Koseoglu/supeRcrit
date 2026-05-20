## DOE Analysis

This module builds, trims, and analyzes polynomial regression models from experimental design data. It supports first-order (screening), interaction, and second-order (response surface) models, with automatic or manual model simplification and canonical analysis for response optimization.

### Workflow

**1. Data Loading:** Import results from a saved DOE design or upload a CSV file with coded factors (capital letters A, B, C...), uncoded factors, a response column, and optionally a time/run order column.

**2. Model Building:** Fit a polynomial model of the selected order (linear, linear + interactions, or quadratic) to the response data. The initial model includes all possible terms for the chosen order.

**3. Model Trimming:** Remove insignificant terms using stepwise backward elimination (AIC-based), p-value cutoff, both methods sequentially, or keep the full model. The trimmed model retains only statistically significant effects.

**4. Model Diagnostics:** Examine ANOVA tables, R² and adjusted R², lack-of-fit tests, residual plots, and influence diagnostics to assess model adequacy.

**5. Optimization:** The fitted response surface is analyzed via canonical analysis to identify the stationary point (optimum) and characterize the surface shape (maximum, minimum, or saddle point). Direct numerical optimization is also performed to find factor settings that minimize or maximize the response.

### References

**Kiratu, J., Raynie, D.E.** (2015), 'Aiding the Development of Extraction Procedures with Response Surface Methodology', *LCGC North America* **33** (7), pp. 104-111.

**NIST/SEMATECH** (2013), *Engineering Statistics Handbook*. [Link](https://www.itl.nist.gov/div898/handbook/index.htm)

**Sharif, K.M., Rahman, M.M., Azmir, J., Mohamed, A., Jahurul, M.H.A., Sahena, F., Zaidul, I.S.M.** (2014), 'Experimental design of supercritical fluid extraction – A review', *Journal of Food Engineering* **124**, pp. 105-116. [DOI](https://doi.org/10.1016/j.jfoodeng.2013.10.003)

**Yolmeh, M., Jafari, S.M.** (2017), 'Applications of Response Surface Methodology in the Food Industry Processes', *Food and Bioprocess Technology* **10**, pp. 413-433. [DOI](https://www.doi.org/10.1007/s11947-016-1855-2)
